# ts_tna / ts_ftna / ts_cna / ts_atna — Nestimate bridge.

.make_stna_series <- function() {
  set.seed(1)
  list(
    a = cumsum(rnorm(60L)),
    b = cumsum(rnorm(60L)),
    c = cumsum(rnorm(45L))
  )
}

test_that("ts_tna builds a Nestimate netobject that keeps its data", {
  skip_if_not_installed("Nestimate")
  series <- .make_stna_series()
  network <- ts_tna(series, n_states = 3, labels = c("low", "mid", "high"))

  expect_s3_class(network, "netobject")
  expect_s3_class(network, "cograph_network")
  expect_setequal(network$nodes$label, c("low", "mid", "high"))
  # Row-normalized transition probabilities.
  expect_equal(unname(rowSums(network$weights)), rep(1, 3), tolerance = 1e-8)
  # Data kept: wide sequences (padded to the longest series) + tidy source.
  expect_equal(dim(network$data), c(3L, 60L))
  expect_equal(nrow(network$ts_source), 60L + 60L + 45L)
  expect_named(network$ts_source, c("id", "time", "value", "state"))
  expect_equal(network$meta$tsn$discretization, "quantile")
  expect_equal(network$meta$tsn$n_states, 3L)
  expect_setequal(network$meta$tsn$series, c("a", "b", "c"))
})

test_that("ts_ftna counts every observed transition", {
  skip_if_not_installed("Nestimate")
  series <- .make_stna_series()
  counts <- ts_ftna(series, n_states = 3)
  expect_true(all(counts$weights == round(counts$weights)))
  # One transition per consecutive pair within each series.
  expect_equal(sum(counts$weights), (60L - 1L) + (60L - 1L) + (45L - 1L))
})

test_that("all four builders return netobjects", {
  skip_if_not_installed("Nestimate")
  series <- .make_stna_series()
  invisible(lapply(list(ts_tna, ts_ftna, ts_cna, ts_atna), function(builder) {
    network <- builder(series, n_states = 3)
    expect_s3_class(network, "netobject")
    expect_true(is.matrix(network$weights))
    expect_equal(nrow(network$ts_source), 165L)
  }))
})

test_that("a tsn_states input skips discretization and keeps its settings", {
  skip_if_not_installed("Nestimate")
  series <- .make_stna_series()
  states <- discretize(series, method = "kmeans", n_states = 3, seed = 42)
  network <- ts_tna(states)
  expect_s3_class(network, "netobject")
  expect_equal(network$meta$tsn$discretization, "kmeans")
  # ts_source keeps the states as a factor preserving the level order.
  expect_s3_class(network$ts_source$state, "factor")
  expect_identical(levels(network$ts_source$state), levels(states$state))
  expect_identical(
    as.character(network$ts_source$state),
    as.character(states$state)
  )
})

test_that("tsn_states inputs honour series selection", {
  skip_if_not_installed("Nestimate")
  states <- discretize(
    .make_stna_series(),
    method = "quantile",
    n_states = 3
  )
  network <- ts_tna(states, series = "b")

  expect_identical(unique(network$ts_source$id), "b")
  expect_identical(network$meta$tsn$series, "b")
  expect_error(ts_tna(states, series = "missing"), "Unknown series")
})

test_that("ordinal transition models exclude trailing display fills", {
  skip_if_not_installed("Nestimate")
  values <- c(4, 1, 3, 2, 5, 0, 6, 2)
  states <- discretize(values, method = "ordinal", m = 3, tau = 1)
  counts <- ts_ftna(states)

  expect_equal(nrow(states), 8L)
  expect_equal(nrow(counts$ts_source), 6L)
  expect_identical(counts$ts_source$time, seq_len(6L))
  expect_equal(sum(counts$weights), 5)
  expect_equal(
    counts$weights["1", "1"],
    0,
    info = "the two forward-filled tail values must not add self-transitions"
  )
})

test_that("single series and long data inputs work", {
  skip_if_not_installed("Nestimate")
  set.seed(3)
  single <- ts_tna(cumsum(rnorm(50)), n_states = 3)
  expect_equal(nrow(single$data), 1L)
  long <- data.frame(
    person = rep(c("x", "y"), each = 40L),
    day = rep(seq_len(40L), times = 2L),
    score = c(cumsum(rnorm(40L)), cumsum(rnorm(40L)))
  )
  network <- ts_tna(long, value = "score", id = "person", time = "day",
                    n_states = 3)
  expect_equal(nrow(network$data), 2L)
  expect_setequal(network$meta$tsn$series, c("x", "y"))
})

test_that("Nestimate verbs run on the bridged objects", {
  skip_if_not_installed("Nestimate")
  series <- .make_stna_series()
  network <- ts_tna(series, n_states = 3)
  centrality <- Nestimate::net_centrality(network)
  expect_s3_class(centrality, "data.frame")
  expect_equal(nrow(centrality), 3L)
  boot <- Nestimate::bootstrap_network(network, iter = 50, seed = 7)
  expect_s3_class(boot, "net_bootstrap")
})

test_that("bridged networks render through cograph", {
  skip_if_not_installed("Nestimate")
  skip_if_not_installed("cograph")
  series <- .make_stna_series()
  network <- ts_tna(series, n_states = 3)
  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output)
  on.exit({
    grDevices::dev.off()
    unlink(output)
  }, add = TRUE)
  expect_no_error(cograph::splot(network))
})

test_that("sequence reshaping pads unequal series in order", {
  source <- data.frame(
    id = c("b", "b", "b", "a", "a"),
    state = c("1", "2", "1", "3", "3"),
    stringsAsFactors = FALSE
  )
  sequences <- .tsn_state_sequences(source)
  expect_equal(dim(sequences), c(2L, 3L))
  expect_equal(unname(unlist(sequences[1L, ])), c("1", "2", "1"))
  expect_equal(unname(unlist(sequences[2L, ])), c("3", "3", NA))
})

test_that("series_networks splits a pooled model per series", {
  skip_if_not_installed("Nestimate")
  series <- .make_stna_series()
  pooled <- ts_tna(series, n_states = 3, labels = c("low", "mid", "high"))
  networks <- series_networks(pooled)

  expect_s3_class(networks, "tsn_series_networks")
  expect_named(networks, c("a", "b", "c"))
  # Shared alphabet: every split model has the SAME node set, in the same
  # order, matching the pooled model.
  expect_identical(networks$a$nodes$label, pooled$nodes$label)
  expect_identical(networks$b$nodes$label, networks$c$nodes$label)
  # Each element is a full ts_tna netobject with its own data.
  invisible(lapply(networks, function(network) {
    expect_s3_class(network, "ts_tna")
    expect_s3_class(network, "netobject")
  }))
  expect_equal(nrow(networks$c$ts_source), 45L)
  expect_equal(networks$a$meta$tsn$series, "a")
  # Nestimate verbs run on split models.
  centrality <- Nestimate::net_centrality(networks$a)
  expect_equal(nrow(centrality), 3L)
  # Subset + validation.
  expect_named(series_networks(pooled, series = "b"), "b")
  expect_error(series_networks(pooled, series = "zz"), "Unknown series")
})

test_that("series network collections have tidy accessors and safe plotting", {
  skip_if_not_installed("Nestimate")
  skip_if_not_installed("cograph")
  pooled <- ts_tna(
    .make_stna_series(),
    n_states = 3,
    labels = c("low", "mid", "high")
  )
  networks <- series_networks(pooled)
  index <- summary(networks)

  expect_s3_class(index, "data.frame")
  expect_identical(
    names(index),
    c("series", "type", "observations", "states", "edges")
  )
  expect_identical(index$series, c("a", "b", "c"))
  expect_identical(index$observations, c(60L, 60L, 45L))
  expect_identical(as.data.frame(networks), index)
  expect_output(print(networks), "series")

  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output)
  on.exit({
    grDevices::dev.off()
    unlink(output)
  }, add = TRUE)
  expect_invisible(plot(networks, series = "a", type = "network"))
  expect_error(plot(networks), "Select exactly one")
  expect_error(plot(networks, series = "zz"), "Unknown series")

  one <- series_networks(pooled, series = "a")
  expect_invisible(plot(one, type = "network"))
})

test_that("per-series ftna counts partition the pooled counts", {
  skip_if_not_installed("Nestimate")
  series <- .make_stna_series()
  pooled <- ts_ftna(series, n_states = 3)
  networks <- series_networks(pooled)
  split_total <- sum(vapply(networks, function(network) {
    sum(network$weights)
  }, numeric(1L)))
  expect_equal(split_total, sum(pooled$weights))
})

test_that("series_networks preserves every builder's Nestimate arguments", {
  skip_if_not_installed("Nestimate")
  builders <- list(
    tna = ts_tna,
    ftna = ts_ftna,
    cna = ts_cna,
    atna = ts_atna
  )
  invisible(lapply(builders, function(builder) {
    pooled <- builder(
      .make_stna_series(),
      n_states = 3,
      labels = c("low", "mid", "high"),
      start = TRUE,
      end = TRUE,
      threshold = 0.05,
      scaling = "max",
      params = list(alphabet = c("low", "mid", "high"))
    )
    networks <- series_networks(pooled)

    expect_true(isTRUE(pooled$meta$tsn$builder_args$start))
    expect_true(isTRUE(pooled$meta$tsn$builder_args$end))
    expect_equal(pooled$meta$tsn$builder_args$threshold, 0.05)
    expect_identical(pooled$meta$tsn$builder_args$scaling, "max")
    expect_identical(
      pooled$meta$tsn$builder_args$params$alphabet,
      c("low", "mid", "high")
    )
    expect_identical(networks$a$nodes$label, pooled$nodes$label)
    expect_true(all(c("Start", "End") %in% networks$a$nodes$label))
  }))
})

test_that("plot.ts_tna draws every combination", {
  skip_if_not_installed("Nestimate")
  skip_if_not_installed("cograph")
  series <- .make_stna_series()
  network <- ts_tna(series, n_states = 3, labels = c("low", "mid", "high"))
  expect_s3_class(network, "ts_tna")
  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output)
  on.exit({
    grDevices::dev.off()
    unlink(output)
  }, add = TRUE)

  expect_invisible(plot(network))
  expect_invisible(plot(network, overlay = "vertical"))
  expect_invisible(plot(network, network = "summary"))
  expect_invisible(plot(network, overlay = "none"))
  expect_invisible(plot(network, ribbon = TRUE))
  expect_invisible(plot(network, overlay = "none", ribbon = TRUE,
                        points = TRUE, network = "summary"))
  expect_invisible(plot(network, "network"))
  expect_invisible(plot(network, "network", network = "summary"))
  expect_invisible(plot(network, "network", node_size = "outstrength",
                        show_weights = FALSE))
  expect_invisible(plot(network, "series", series = "a", legend = FALSE))
  expect_invisible(plot(network, series = "a", network = "per_series"))
  expect_invisible(plot(network, network_width = 0.6, legend = FALSE))
  expect_error(plot(network, series = "zz"), "Unknown series")
  expect_error(plot(network, node_size = "pagerank"))
  expect_error(plot(network, network = "grouped"))
  expect_error(plot(network, network_width = 0))
})

test_that("TNA plots default to horizontal value bands", {
  expect_identical(
    formals(plot.ts_tna)$overlay,
    quote(c("horizontal", "vertical", "none"))
  )
})

test_that("ts_tna class does not break Nestimate dispatch", {
  skip_if_not_installed("Nestimate")
  series <- .make_stna_series()
  network <- ts_tna(series, n_states = 3)
  expect_identical(
    class(network),
    c("ts_tna", "netobject", "cograph_network")
  )
  centrality <- Nestimate::net_centrality(network)
  expect_equal(nrow(centrality), 3L)
})
