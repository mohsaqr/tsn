test_that("all discretizers use packaged srl through direct tsn calls", {
  data("srl", package = "tsn")
  methods <- c(
    "threshold", "width", "quantile", "kde", "kmeans", "gaussian",
    "hclust"
  )
  networks <- setNames(lapply(methods, function(discretization_method) {
    arguments <- list(
      srl,
      value = "effort",
      id = "name",
      time = "day",
      series = "Erik",
      unit = "state",
      visibility = "horizontal",
      discretization = discretization_method
    )
    # `breaks` is only consumed by "threshold" and `seed` only by the
    # stochastic discretizers; supplying them elsewhere is an error.
    if (discretization_method == "threshold") {
      arguments$breaks <- c(40, 70)
    }
    if (discretization_method %in% c("kmeans", "gaussian")) {
      arguments$seed <- 1707L
    }
    do.call(tsn, arguments)
  }), methods)

  expect_true(all(vapply(networks, inherits, logical(1L), what = "tsn")))
  expect_true(all(vapply(networks, function(network) {
    source <- as.data.frame(network, what = "series")
    nrow(source) == 156L &&
      identical(unique(source[["id"]]), "Erik") &&
      is.numeric(source[["time"]]) &&
      identical(sort(unique(as.character(source[["state"]]))),
                as.character(1:3)) &&
      !anyNA(source[["state"]])
  }, logical(1L))))
  expect_true(all(vapply(networks, function(network) {
    all(is.finite(as.data.frame(network)$distance)) && all(is.finite(as.data.frame(network)$weight))
  }, logical(1L))))
})

test_that("series selects long IDs and wide columns inside tsn", {
  long <- data.frame(
    id = rep(c("a", "b", "c"), each = 4L),
    time = rep(seq_len(4L), times = 3L),
    value = seq_len(12L)
  )
  selected_long <- tsn(
    long,
    value = "value",
    id = "id",
    time = "time",
    series = c("a", "c"),
    method = "distance"
  )
  selected_wide <- tsn(
    data.frame(a = 1:4, b = 2:5, label = letters[1:4]),
    series = c("a", "b"),
    method = "distance"
  )
  selected_list <- tsn(
    list(a = 1:4, b = 2:5, c = 3:6),
    series = c("b", "c"),
    method = "distance"
  )

  expect_identical(selected_long$nodes$label, c("a", "c"))
  expect_identical(selected_wide$nodes$label, c("a", "b"))
  expect_identical(selected_list$nodes$label, c("b", "c"))
  expect_error(tsn(long, value = "value", series = "a"), "id.*required")
  expect_error(tsn(long, value = "value", id = "id", series = "z"),
               "Unknown series")
  expect_error(tsn(1:5, series = "a"), "selection requires")
})

test_that("esm_srl supports direct one-line wide selection", {
  data("esm_srl", package = "tsn")
  hana <- subset(esm_srl, name == "Hana")
  anxiety <- tsn(
    hana,
    series = "anxiety",
    unit = "state",
    visibility = "horizontal",
    discretization = "gaussian",
    seed = 1707L
  )
  variables <- tsn(
    hana,
    series = c("planning", "monitoring", "effort", "anxiety"),
    method = "distance",
    distance = "correlation"
  )

  expect_s3_class(anxiety, "tsn")
  expect_equal(nrow(as.data.frame(anxiety, what = "series")), nrow(hana))
  expect_false(anyNA(as.data.frame(anxiety, what = "series")[["state"]]))
  expect_identical(variables$nodes$label,
                   c("planning", "monitoring", "effort", "anxiety"))
})
