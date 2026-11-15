# Audit of R/stna.R: the ts_*na constructors, grouping, segmentation,
# series_networks() and the transition-network plot methods. Every count below
# is computed by hand from the toy sequence, not read back from the package.

audit_states <- function(values, labels = c("L", "M", "H")) {
  discretize(
    values, method = "threshold", breaks = c(3, 7), n_states = 3L,
    labels = labels
  )
}

audit_panel <- function() {
  # Series a: states 1 2 3 1 2 3; groups x x y y x x.
  # Series b: states 3 3 1 1 2 2; groups y y y x x x.
  data.frame(
    id = rep(c("a", "b"), each = 6L),
    t = rep(seq_len(6L), 2L),
    v = c(1, 5, 9, 1, 5, 9, 9, 9, 1, 1, 5, 5),
    g = c("x", "x", "y", "y", "x", "x", "y", "y", "y", "x", "x", "x"),
    stringsAsFactors = FALSE
  )
}

audit_grouped <- function(data = audit_panel(), group = "g", ...) {
  ts_ftna(
    data, value = "v", id = "id", time = "t", group = group,
    discretization = "threshold", breaks = c(3, 7), n_states = 3L, ...
  )
}

hand_counts <- function(states, alphabet) {
  unclass(table(
    factor(utils::head(states, -1L), levels = alphabet),
    factor(states[-1L], levels = alphabet)
  ))
}

test_that("ts_ftna and ts_tna match a hand-computed transition count matrix", {
  skip_if_not_installed("Nestimate")
  states <- audit_states(c(1, 1, 5, 5, 9, 1, 5, 9, 9, 1))
  sequence <- c("L", "L", "M", "M", "H", "L", "M", "H", "H", "L")
  expected <- hand_counts(sequence, c("L", "M", "H"))
  counts <- unname(as.matrix(ts_ftna(states)))
  expect_equal(counts, unname(matrix(expected, 3L)), ignore_attr = TRUE)
  expect_identical(rownames(as.matrix(ts_ftna(states))), c("L", "M", "H"))
  probabilities <- as.matrix(ts_tna(states))
  expect_equal(unname(probabilities), unname(expected / rowSums(expected)),
               ignore_attr = TRUE)
  expect_equal(unname(rowSums(probabilities)), rep(1, 3L))
})

test_that("a state with no outgoing transition keeps an all-zero row", {
  skip_if_not_installed("Nestimate")
  # H appears only as the last observation, so its row has nothing to count.
  states <- audit_states(c(1, 5, 1, 5, 1, 9))
  probabilities <- as.matrix(ts_tna(states))
  expect_true(all(is.finite(probabilities)))
  expect_equal(unname(probabilities["H", ]), c(0, 0, 0))
  expect_equal(unname(probabilities["L", ]), c(0, 2 / 3, 1 / 3))
  expect_equal(unname(as.matrix(ts_ftna(states))["L", ]), c(0, 2, 1))
})

test_that("ts_cna and ts_atna match their published definitions", {
  skip_if_not_installed("Nestimate")
  sequence <- c("L", "L", "M", "M", "H", "L", "M", "H", "H", "L")
  states <- audit_states(c(1, 1, 5, 5, 9, 1, 5, 9, 9, 1))
  alphabet <- c("L", "M", "H")
  # Co-occurrence over one sequence: n_i * n_j off the diagonal, choose(n, 2)
  # on it.
  n <- as.vector(table(factor(sequence, levels = alphabet)))
  expected_cna <- outer(n, n)
  diag(expected_cna) <- choose(n, 2)
  expect_equal(unname(as.matrix(ts_cna(states))), expected_cna,
               ignore_attr = TRUE)
  # Attention: every ordered forward pair i < j adds exp(-(j - i)).
  position <- seq_along(sequence)
  weight <- exp(-abs(outer(position, position, "-")))
  weight[lower.tri(weight, diag = TRUE)] <- 0
  code <- match(sequence, alphabet)
  expected_atna <- matrix(0, 3L, 3L)
  invisible(lapply(seq_len(3L), function(from) {
    lapply(seq_len(3L), function(to) {
      expected_atna[from, to] <<- sum(weight[code == from, code == to])
    })
  }))
  expect_equal(unname(as.matrix(ts_atna(states))), expected_atna,
               ignore_attr = TRUE, tolerance = 1e-12)
})

test_that("transitions are never counted across a series boundary", {
  skip_if_not_installed("Nestimate")
  panel <- data.frame(
    id = rep(c("a", "b"), each = 3L), t = rep(seq_len(3L), 2L),
    v = c(1, 1, 1, 9, 9, 9)
  )
  counts <- as.matrix(ts_ftna(
    panel, value = "v", id = "id", time = "t",
    discretization = "threshold", breaks = 5, n_states = 2L
  ))
  # a: 1 -> 1 twice; b: 2 -> 2 twice; the a|b seam would add a 1 -> 2.
  expect_equal(unname(counts), matrix(c(2, 0, 0, 2), 2L), ignore_attr = TRUE)
})

test_that("row order of the input does not change any count", {
  skip_if_not_installed("Nestimate")
  panel <- audit_panel()
  shuffled <- panel[c(12, 3, 7, 1, 9, 5, 2, 11, 4, 10, 6, 8), ]
  pooled <- function(data) {
    as.matrix(ts_ftna(
      data, value = "v", id = "id", time = "t",
      discretization = "threshold", breaks = c(3, 7), n_states = 3L
    ))
  }
  expect_identical(pooled(shuffled), pooled(panel))
  expect_identical(
    as.data.frame(audit_grouped(shuffled)), as.data.frame(audit_grouped(panel))
  )
})

test_that("grouped counts match a hand count with runs cut at every group change", {
  skip_if_not_installed("Nestimate")
  grouped <- audit_grouped()
  expect_s3_class(grouped, "ts_tna_group")
  expect_identical(class(grouped), c("ts_tna_group", "netobject_group"))
  expect_identical(attr(grouped, "group_col"), "g")
  expect_identical(names(grouped), c("x", "y"))
  expect_true(all(vapply(
    grouped,
    function(network) identical(
      class(network), c("ts_tna", "netobject", "cograph_network")
    ),
    logical(1L)
  )))
  # x runs: a(1 2), a(2 3), b(1 2 2); y runs: a(3 1), b(3 3 1).
  expected_x <- matrix(0, 3L, 3L)
  expected_x[1L, 2L] <- 2
  expected_x[2L, 2L] <- 1
  expected_x[2L, 3L] <- 1
  expected_y <- matrix(0, 3L, 3L)
  expected_y[3L, 1L] <- 2
  expected_y[3L, 3L] <- 1
  expect_equal(unname(as.matrix(grouped$x)), expected_x, ignore_attr = TRUE)
  expect_equal(unname(as.matrix(grouped$y)), expected_y, ignore_attr = TRUE)
  # Every group carries the full pooled alphabet as its node set.
  expect_identical(
    vapply(grouped, function(network) nrow(network$nodes), integer(1L)),
    c(x = 3L, y = 3L)
  )
})

test_that("a factor group with unused levels keeps its level order", {
  skip_if_not_installed("Nestimate")
  panel <- audit_panel()
  panel$g <- factor(panel$g, levels = c("z", "y", "x"))
  expect_no_warning(grouped <- audit_grouped(panel))
  expect_identical(names(grouped), c("y", "x"))
})

test_that("a single-observation group warns and keeps the full node set", {
  skip_if_not_installed("Nestimate")
  panel <- audit_panel()
  panel$g <- c(rep("x", 11L), "y")
  expect_warning(grouped <- audit_grouped(panel), "no two observations")
  expect_identical(names(grouped), c("x", "y"))
  expect_equal(sum(as.matrix(grouped$y)), 0)
  expect_identical(dim(as.matrix(grouped$y)), c(3L, 3L))
})

test_that("NA, empty, and non-scalar group arguments fail early and clearly", {
  skip_if_not_installed("Nestimate")
  panel <- audit_panel()
  panel$g[3L] <- NA
  expect_error(audit_grouped(panel), "contains NA")
  panel$g[3L] <- ""
  expect_error(audit_grouped(panel), "empty label")
  panel$g <- factor(panel$g)
  expect_error(audit_grouped(panel), "empty label")
  expect_error(audit_grouped(group = c("g", "t")), "single column name")
  expect_error(audit_grouped(group = 1), "single column name")
  expect_error(audit_grouped(group = NA_character_), "single column name")
})

test_that("numeric group values that print identically are not merged", {
  skip_if_not_installed("Nestimate")
  panel <- audit_panel()
  panel$g <- ifelse(panel$g == "x", 0.3, 0.1 + 0.2)
  # Two distinct groups both render as "0.3"; before the fix they were
  # silently merged into a single network named "0.3".
  expect_error(audit_grouped(panel), "print identically")
  panel$g <- ifelse(panel$g > 0.3, 2, 1)
  expect_identical(names(audit_grouped(panel)), c("1", "2"))
})

test_that("group alignment is exact on times that print identically", {
  skip_if_not_installed("Nestimate")
  panel <- data.frame(
    id = "a", v = c(1, 1, 9, 9, 1, 1),
    t = c(0.1, 0.1 + 0.2, 0.3, 0.4, 0.5, 0.6),
    g = c("x", "x", "y", "y", "x", "x")
  )
  # Sorted by time: 0.1(x) 0.3(y) 0.1+0.2(x) 0.4(y) 0.5(x) 0.6(x), so the
  # groups alternate and only the final x run holds a transition.
  expect_warning(grouped <- ts_ftna(
    panel, value = "v", id = "id", time = "t", group = "g",
    discretization = "threshold", breaks = 5, n_states = 2L
  ), "no two observations")
  expect_equal(sum(as.matrix(grouped$x)), 1)
  expect_equal(sum(as.matrix(grouped$y)), 0)
})

test_that("generated sequence IDs never weld a caller's own series", {
  skip_if_not_installed("Nestimate")
  panel <- data.frame(
    id = rep(c("a", "a.1"), each = 4L), t = rep(seq_len(4L), 2L),
    v = c(1, 9, 1, 9, 5, 5, 5, 5),
    g = c("x", "x", "y", "y", "x", "x", "x", "x")
  )
  grouped <- audit_grouped(panel)
  ids <- unique(grouped$x$ts_source$id)
  expect_length(ids, 2L)
  expect_true("a.1" %in% ids)
  # a's first run: 1 -> 3; series a.1: 2 -> 2 three times. No 3 -> 2 seam.
  counts <- as.matrix(grouped$x)
  expect_equal(sum(counts), 4)
  expect_equal(unname(counts[3L, 2L]), 0)
  expect_error(
    tsn:::.tsn_indexed_ids(
      c("a", "a.1", "a..1", "a...1"), rep(1L, 4L),
      suffix = c(TRUE, FALSE, FALSE, FALSE)
    ),
    "unique sequence IDs"
  )
})

test_that("the lost-group warning fires only for a group discretization removed", {
  skip_if_not_installed("Nestimate")
  set.seed(11)
  panel <- data.frame(
    id = rep(c("a", "b"), each = 10L), t = rep(seq_len(10L), 2L),
    v = stats::rnorm(20L),
    g = c(rep("x", 8L), "y", "y", rep("x", 10L)),
    g2 = rep(c("x", "z"), each = 10L)
  )
  # Ordinal m = 3 trims the last two rows of each series, which hold all of y.
  expect_warning(
    ts_ftna(panel, value = "v", id = "id", time = "t", group = "g",
            discretization = "ordinal", m = 3, tau = 1),
    "\"y\" had observations"
  )
  # z lives only in series b, which `series` excluded: not "lost".
  expect_no_warning(selected <- ts_ftna(
    panel, value = "v", id = "id", time = "t", group = "g2", series = "a"
  ))
  expect_identical(names(selected), "x")
})

test_that("ordinal fills are trimmed when a tsn_states input is series-selected", {
  skip_if_not_installed("Nestimate")
  set.seed(12)
  panel <- data.frame(
    id = rep(c("a", "b"), each = 10L), t = rep(seq_len(10L), 2L),
    v = stats::rnorm(20L)
  )
  states <- discretize(panel, value = "v", id = "id", time = "t",
                       method = "ordinal", m = 3, tau = 1)
  selected <- ts_ftna(states, series = "a")
  # 10 observations, 2 trailing fills dropped: 8 patterns, 7 transitions.
  expect_equal(sum(as.matrix(selected)), 7)
  expect_identical(nrow(as.data.frame(selected, what = "series")), 8L)
})

test_that("overlap without segment is rejected rather than ignored", {
  skip_if_not_installed("Nestimate")
  values <- c(1, 5, 9, 1, 5, 9, 1)
  expect_error(ts_tna(values, overlap = TRUE), "requires `segment`")
  expect_error(ts_tna(values, overlap = NA), "single TRUE or FALSE")
  expect_error(ts_tna(values, segment = 3, overlap = "yes"),
               "single TRUE or FALSE")
  expect_no_error(ts_tna(values, overlap = FALSE))
})

test_that("malformed segment widths fail with one clear error and no warning", {
  skip_if_not_installed("Nestimate")
  values <- c(1, 5, 9, 1, 5, 9, 1)
  invisible(lapply(list(1, 2.5, Inf, NA_real_, "3", c(2, 3), -2), function(w) {
    expect_no_warning(
      expect_error(ts_tna(values, segment = w), "whole number of at least 2")
    )
  }))
  partitioned <- ts_ftna(values, segment = 3, discretization = "threshold",
                         breaks = c(3, 7), n_states = 3L)
  # Blocks (1 5 9) (1 5 9) and a dropped single-observation tail.
  expect_equal(unname(as.matrix(partitioned)),
               matrix(c(0, 0, 0, 2, 0, 0, 0, 2, 0), 3L), ignore_attr = TRUE)
})

test_that("series_networks splits into per-series models that sum to the pool", {
  skip_if_not_installed("Nestimate")
  series <- list(a = c(1, 5, 9, 1, 5), b = c(9, 9, 1, 5, 5))
  pooled <- ts_ftna(series, discretization = "threshold", breaks = c(3, 7),
                    n_states = 3L)
  networks <- series_networks(pooled)
  expect_s3_class(networks, "tsn_series_networks")
  expect_identical(names(networks), c("a", "b"))
  total <- Reduce(`+`, lapply(networks, as.matrix))
  expect_identical(total, as.matrix(pooled))
  alone <- ts_ftna(series, series = "b", discretization = "threshold",
                   breaks = c(3, 7), n_states = 3L)
  expect_identical(as.matrix(networks$b), as.matrix(alone))
  index <- summary(networks)
  expect_identical(
    names(index), c("series", "type", "observations", "states", "edges")
  )
  expect_identical(index$series, c("a", "b"))
  expect_identical(index$observations, c(5L, 5L))
  expect_identical(index$states, c(3L, 3L))
  # a: 1->2, 2->3, 3->1 (2x 1->2 counted once as an edge); b: 3->3, 3->1,
  # 1->2, 2->2.
  expect_identical(index$edges, c(3L, 4L))
  expect_identical(as.data.frame(networks), index)
  expect_error(series_networks(pooled, series = "zz"), "Unknown series")
})

audit_restores_par <- function(code) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(1, 2, 3, 4), cex = 0.9)
  before <- graphics::par(no.readonly = TRUE)
  code()
  identical(graphics::par(no.readonly = TRUE), before)
}

test_that("plot methods restore every graphics parameter, cex included", {
  skip_if_not_installed("Nestimate")
  skip_if_not_installed("cograph")
  set.seed(5)
  panel <- data.frame(
    id = rep(c("a", "b", "c", "d"), each = 20L), t = rep(seq_len(20L), 4L),
    v = stats::rnorm(80L), g4 = rep(c("p", "q", "r", "s"), each = 20L),
    g1 = "only"
  )
  one <- ts_tna(panel, value = "v", id = "id", time = "t", group = "g1")
  four <- ts_tna(panel, value = "v", id = "id", time = "t", group = "g4")
  pooled <- ts_tna(panel, value = "v", id = "id", time = "t")
  expect_true(audit_restores_par(function() plot(one)))
  expect_true(audit_restores_par(function() plot(four, group = "r")))
  expect_true(audit_restores_par(
    function() plot(four, group = "p", type = "network")
  ))
  expect_true(audit_restores_par(
    function() plot(four, group = "q", network = "summary")
  ))
  expect_true(audit_restores_par(function() plot(pooled, "series")))
  expect_true(audit_restores_par(
    function() plot(pooled, ribbon = TRUE, points = TRUE, legend = FALSE)
  ))
  expect_true(audit_restores_par(
    function() plot(series_networks(pooled), series = "b")
  ))
  expect_error(plot(four), "exactly one")
})

test_that("network plots fail cleanly without cograph while series plots work", {
  skip_if_not_installed("Nestimate")
  set.seed(6)
  panel <- data.frame(id = rep(c("a", "b"), each = 15L),
                      t = rep(seq_len(15L), 2L), v = stats::rnorm(30L),
                      g = "only")
  pooled <- ts_tna(panel, value = "v", id = "id", time = "t")
  grouped <- ts_tna(panel, value = "v", id = "id", time = "t", group = "g")
  local_mocked_bindings(.tsn_require_cograph = function() {
    stop("Network plotting requires the cograph package.", call. = FALSE)
  })
  expect_true(audit_restores_par(function() {
    expect_error(plot(pooled), "requires the cograph package")
    expect_error(plot(grouped), "requires the cograph package")
  }))
  expect_true(audit_restores_par(function() plot(pooled, type = "series")))
  expect_true(audit_restores_par(function() plot(grouped, type = "series")))
})

test_that("a state keeps one colour across groups that do not all visit it", {
  skip_if_not_installed("Nestimate")
  skip_if_not_installed("cograph")
  panel <- data.frame(
    id = rep(c("a", "b"), each = 6L), t = rep(seq_len(6L), 2L),
    v = c(1, 1, 5, 5, 1, 1, 5, 9, 9, 5, 9, 5),
    g = rep(c("p", "q"), each = 6L)
  )
  grouped <- ts_tna(panel, value = "v", id = "id", time = "t", group = "g",
                    discretization = "threshold", breaks = c(3, 7),
                    n_states = 3L, labels = c("L", "M", "H"))
  # Group q never visits L. Before the fix its M took L's colour and its H
  # points were drawn with an NA (invisible) fill.
  palettes <- list()
  fills <- list()
  local_mocked_bindings(
    .tsn_splot_network = function(model, state_colors, ...) {
      palettes[[length(palettes) + 1L]] <<- state_colors
      invisible(NULL)
    }
  )
  local_mocked_bindings(
    points = function(x, y = NULL, ..., bg) {
      fills[[length(fills) + 1L]] <<- bg
      invisible(NULL)
    },
    .package = "graphics"
  )
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  plot(grouped, group = "p", points = TRUE)
  plot(grouped, group = "q", points = TRUE)
  expect_length(palettes, 2L)
  expect_identical(palettes[[1L]], palettes[[2L]])
  expect_identical(names(palettes[[1L]]), c("L", "M", "H"))
  q_states <- as.character(
    as.data.frame(grouped$q, what = "series")$state
  )
  expect_false(anyNA(fills[[2L]]))
  expect_identical(fills[[2L]], unname(palettes[[2L]][q_states]))
})

test_that("plot rejects a non-finite max_series without a coercion warning", {
  skip_if_not_installed("Nestimate")
  pooled <- ts_tna(list(a = c(1, 5, 9, 1, 5), b = c(9, 9, 1, 5, 5)))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_warning(expect_error(plot(pooled, "series", max_series = Inf)))
  expect_error(plot(pooled, "series", max_series = 0))
  expect_error(plot(pooled, "series", node_scale = Inf))
})
