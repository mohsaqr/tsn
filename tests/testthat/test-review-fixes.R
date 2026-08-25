# Regression tests for the 2026-08-24 package review findings.

test_that("state-network density counts loops in the denominator regardless of observation", {
  # One observed A-B edge, no loops: 3 possible undirected state pairs
  # (A-A, A-B, B-B), so density is 1/3 — not 1.
  no_loops <- tsn(
    1:4,
    method = "visibility",
    unit = "state",
    state = c("A", "B", "A", "B"),
    visibility = "horizontal"
  )
  expect_equal(summary(no_loops)$density, 1 / 3)

  # A-A, A-B, and B-B all observed: all 3 possible pairs, density 1.
  with_loops <- tsn(
    c(1, 2, 3, 4),
    method = "visibility",
    unit = "state",
    state = c("A", "A", "B", "B"),
    visibility = "horizontal"
  )
  expect_equal(summary(with_loops)$density, 1)

  # Directed with loops possible: n * (n - 1) + n = n^2 possible edges.
  directed <- tsn(
    c(1, 2, 3, 4),
    method = "visibility",
    unit = "state",
    state = c("A", "A", "B", "B"),
    visibility = "horizontal",
    directed = TRUE
  )
  directed_summary <- summary(directed)
  expect_equal(
    directed_summary$density,
    directed_summary$edges / (directed_summary$nodes^2)
  )

  # Non-state networks never include loops in the denominator.
  series_network <- tsn(
    list(a = 1:4, b = 2:5, c = 4:1),
    method = "distance",
    unit = "series"
  )
  expect_equal(summary(series_network)$density, 1)
})

test_that("explicit factor state order and unused levels are preserved", {
  states <- factor(
    c("high", "low", "mid", "high"),
    levels = c("low", "mid", "high"),
    ordered = TRUE
  )
  network <- tsn(1:4, method = "visibility", unit = "state", state = states)

  expect_identical(network$nodes$label, c("low", "mid", "high"))
  expect_identical(rownames(as.matrix(network)), c("low", "mid", "high"))
  expect_s3_class(network$source$state, "factor")
  expect_identical(levels(network$source$state), c("low", "mid", "high"))

  # An unused declared level stays a zero-degree node.
  partial <- factor(c("low", "high", "low", "high"),
                    levels = c("low", "mid", "high"))
  with_unused <- tsn(1:4, method = "visibility", unit = "state",
                     state = partial)
  expect_identical(with_unused$nodes$label, c("low", "mid", "high"))
  expect_true(all(as.matrix(with_unused)["mid", ] == 0))
  expect_true(all(as.matrix(with_unused)[, "mid"] == 0))

  # Plain character states keep first-appearance order.
  plain <- tsn(1:4, method = "visibility", unit = "state",
               state = c("b", "a", "b", "a"))
  expect_identical(plain$nodes$label, c("b", "a"))
})

test_that("discretized states order nodes numerically, not by first appearance", {
  # First observation lands in the top state, so first-appearance order would
  # start at "3"; the numeric state order must win.
  values <- c(9, 1, 5, 9, 1, 5, 9, 1)
  network <- tsn(values, method = "visibility", unit = "state",
                 discretization = "quantile", n_states = 3L)
  expect_identical(network$nodes$label, c("1", "2", "3"))
})

test_that("point-distance networks accept ids and times whose display strings collide", {
  colliding <- data.frame(
    id = c("a:b", "a"),
    time = c("c", "b:c"),
    value = c(1, 2)
  )
  network <- tsn(
    colliding,
    value = "value", id = "id", time = "time",
    method = "distance", unit = "time"
  )
  expect_identical(network$n_nodes, 2L)
  expect_identical(anyDuplicated(network$nodes$label), 0L)
  # The same collision-safe labels as the visibility family.
  expect_true(all(c("3:a:b|1:c", "1:a|3:b:c") %in% network$nodes$label))
})

test_that("arguments a visibility network cannot consume are rejected", {
  expect_error(tsn(1:5, method = "visibility", chain = TRUE), "`chain`")
  expect_error(tsn(1:5, method = "visibility", normalize = "max"),
               "`normalize`")
  expect_error(tsn(1:5, method = "visibility", connect = "full"),
               "`connect`")
  expect_error(tsn(1:5, method = "visibility", distance = "euclidean"),
               "`distance`")
  # State-only options need `unit = "state"`.
  expect_error(tsn(1:5, method = "visibility", n_states = 3L), "`n_states`")
  expect_error(tsn(1:5, method = "visibility", aggregation = "count"),
               "`aggregation`")
  expect_error(tsn(1:5, method = "visibility", seed = 1L), "`seed`")
})

test_that("arguments a distance network cannot consume are rejected", {
  series <- list(a = 1:4, b = 2:5)
  expect_error(tsn(series, method = "distance", unit = "series", limit = 0.01),
               "`limit`")
  expect_error(tsn(series, method = "distance", unit = "series", decay = 100),
               "`decay`")
  expect_error(
    tsn(series, method = "distance", unit = "series", penetrable = 3),
    "`penetrable`"
  )
  expect_error(
    tsn(series, method = "distance", unit = "series", aggregation = "count"),
    "`aggregation`"
  )
  expect_error(tsn(series, method = "distance", unit = "series", step = 2L),
               "`step`")
  expect_error(tsn(series, method = "distance", unit = "series", p = 3),
               "`p`")
  expect_error(
    tsn(series, method = "distance", unit = "series", visibility = "natural"),
    "`visibility`"
  )
  # The consuming configurations still work.
  expect_s3_class(
    tsn(series, method = "distance", unit = "series",
        distance = "minkowski", p = 3),
    "tsn"
  )
  expect_s3_class(
    tsn(series, method = "distance", unit = "window", window = 2L, step = 2L),
    "tsn"
  )
})

test_that("discretizer options are rejected outside the modes that consume them", {
  values <- c(3, 1, 4, 2, 5, 3, 6, 2, 7)
  # breaks only with the threshold discretizer.
  expect_error(
    tsn(values, unit = "state", discretization = "quantile", breaks = c(3, 5)),
    "`breaks`"
  )
  expect_error(discretize(values, method = "quantile", breaks = c(3, 5)),
               "`breaks`")
  # n_states is not consumed by ordinal.
  expect_error(
    tsn(values, unit = "state", discretization = "ordinal", n_states = 4L),
    "`n_states`"
  )
  expect_error(discretize(values, method = "ordinal", n_states = 4L),
               "`n_states`")
  # seed only with the stochastic discretizers.
  expect_error(
    tsn(values, unit = "state", discretization = "quantile", seed = 1L),
    "`seed`"
  )
  expect_error(discretize(values, method = "quantile", seed = 1L), "`seed`")
  # Caller-supplied states bypass the discretizer entirely.
  expect_error(
    tsn(values, unit = "state", state = rep(c("a", "b", "c"), 3L),
        n_states = 3L),
    "`n_states`"
  )
  # Explicit NULL means the default and passes.
  expect_s3_class(
    tsn(values, unit = "state", discretization = "quantile", breaks = NULL,
        seed = NULL),
    "tsn"
  )
})

test_that("shortcut methods reject contradicting granular arguments", {
  values <- c(3, 1, 4, 2, 5)
  expect_error(tsn(values, "hvg", visibility = "natural"), "hvg")
  expect_error(tsn(values, "nvg", visibility = "horizontal"), "nvg")
  expect_error(
    tsn(c(3, 1, 4, 2, 5, 3, 6, 2, 7), "quantile",
        discretization = "kmeans"),
    "quantile"
  )
  expect_error(
    tsn(c(3, 1, 4, 2, 5, 3, 6, 2, 7), "quantile", unit = "time"),
    "state"
  )
  # Redundant but consistent values remain valid.
  expect_s3_class(tsn(values, "hvg", visibility = "horizontal"), "tsn")
})

test_that("ts_tna rejects options its input cannot consume", {
  skip_if_not_installed("Nestimate")
  set.seed(1)
  values <- cumsum(stats::rnorm(60))
  expect_error(ts_tna(values, breaks = c(-1, 1)), "`breaks`")
  expect_error(ts_tna(values, seed = 9L), "`seed`")
  expect_error(ts_tna(values, discretization = "ordinal", n_states = 4L),
               "`n_states`")
  states <- discretize(values, n_states = 3L)
  expect_error(ts_tna(states, discretization = "kmeans"), "tsn_states")
  expect_error(ts_tna(states, n_states = 4L), "tsn_states")
  expect_s3_class(ts_tna(states), "ts_tna")
  expect_s3_class(ts_tna(values, discretization = "ordinal"), "ts_tna")
})

test_that("long-data series selection follows the requested order like list input", {
  long <- data.frame(
    id = rep(c("a", "b", "c"), each = 3),
    time = rep(1:3, 3),
    value = c(1:3, 4:6, 7:9)
  )
  from_long <- tsn(
    long,
    value = "value", id = "id", time = "time",
    series = c("c", "a"), method = "distance",
    unit = "series", chain = TRUE, directed = TRUE
  )
  from_list <- tsn(
    list(a = 1:3, b = 4:6, c = 7:9),
    series = c("c", "a"), method = "distance",
    unit = "series", chain = TRUE, directed = TRUE
  )
  expect_identical(unique(from_long$source$id), c("c", "a"))
  expect_identical(as.data.frame(from_long)$from, "c")
  expect_identical(as.data.frame(from_long)$to, "a")
  expect_identical(
    as.data.frame(from_long)[c("from", "to", "distance")],
    as.data.frame(from_list)[c("from", "to", "distance")]
  )
})

test_that("the okabe palette covers 1 through 9 states and rejects 10", {
  one <- tsn:::.tsn_palette_colors("okabe", 1L)
  nine <- tsn:::.tsn_palette_colors("okabe", 9L)
  expect_length(one, 1L)
  expect_length(nine, 9L)
  expect_false(anyNA(nine))
  expect_error(tsn:::.tsn_palette_colors("okabe", 10L), "9 colour")
})

test_that("series selection does not trigger the lost-to-discretization warning", {
  skip_if_not_installed("Nestimate")
  long <- data.frame(
    id = rep(c("a", "b"), each = 12),
    time = rep(1:12, 2),
    value = c(1:12, 12:1),
    grp = rep(c("A", "B"), each = 12)
  )
  expect_no_warning(
    selected <- ts_tna(
      long,
      value = "value", id = "id", time = "time",
      series = "a", group = "grp"
    )
  )
  expect_identical(names(selected), "A")
  # A group genuinely removed by discretization still warns: ordinal
  # embedding trims the tail rows that are the only members of group "tail".
  trimmed <- data.frame(
    id = "a",
    time = 1:12,
    value = c(5, 1, 4, 2, 6, 3, 7, 2, 8, 1, 9, 4),
    grp = c(rep("body", 10), rep("tail", 2))
  )
  expect_warning(
    ts_tna(
      trimmed,
      value = "value", id = "id", time = "time",
      discretization = "ordinal", group = "grp"
    ),
    "survived discretization"
  )
})

test_that("natural visibility scales quadratically, not cubically", {
  skip_on_cran()
  set.seed(7)
  elapsed <- system.time(
    tsn:::.tsn_visibility(stats::rnorm(2000), method = "natural")
  )[["elapsed"]]
  # The pre-review cubic implementation needed ~1.5 s for n = 800 and tens of
  # seconds at n = 2000; the quadratic scan runs in well under a second.
  expect_lt(elapsed, 5)
})

test_that("the rewritten visibility engine matches exhaustive evaluation", {
  brute_force <- function(values, times, method, penetrable) {
    labels <- as.character(seq_along(values))
    candidates <- utils::combn(seq_along(values), 2L)
    blocked <- vapply(
      seq_len(ncol(candidates)),
      function(candidate) {
        left <- candidates[1L, candidate]
        right <- candidates[2L, candidate]
        if (right - left == 1L) {
          return(0L)
        }
        intermediate <- seq.int(left + 1L, right - 1L)
        boundary <- if (identical(method, "horizontal")) {
          rep.int(min(values[left], values[right]), length(intermediate))
        } else {
          values[left] +
            (values[right] - values[left]) *
              (times[intermediate] - times[left]) /
                (times[right] - times[left])
        }
        sum(values[intermediate] >= boundary)
      },
      integer(1L)
    )
    visible <- blocked <= penetrable
    data.frame(
      from = labels[candidates[1L, visible]],
      to = labels[candidates[2L, visible]],
      stringsAsFactors = FALSE
    )
  }
  set.seed(23)
  fixtures <- lapply(seq_len(60L), function(index) {
    n <- sample(2:40, 1L)
    list(
      values = if (index %% 2L == 0L) sample(-3:3, n, replace = TRUE) else
        stats::rnorm(n),
      times = if (index %% 3L == 0L) cumsum(stats::runif(n, 0.1, 3)) else
        as.numeric(seq_len(n))
    )
  })
  configurations <- expand.grid(
    method = c("natural", "horizontal"),
    penetrable = c(0L, 2L),
    stringsAsFactors = FALSE
  )
  equivalent <- vapply(
    fixtures,
    function(fixture) {
      all(vapply(
        seq_len(nrow(configurations)),
        function(row) {
          method <- configurations$method[row]
          penetrable <- configurations$penetrable[row]
          engine <- tsn:::.tsn_visibility(
            fixture$values, times = fixture$times,
            method = method, penetrable = penetrable
          )
          reference <- brute_force(
            fixture$values, times = fixture$times,
            method = method, penetrable = penetrable
          )
          identical(engine[c("from", "to")], reference)
        },
        logical(1L)
      ))
    },
    logical(1L)
  )
  expect_true(all(equivalent))
})
