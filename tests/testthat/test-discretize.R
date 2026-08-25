test_that("every discretizer is reachable through the public verb", {
  set.seed(3)
  values <- cumsum(stats::rnorm(120)) + 50
  methods <- c(
    "width", "quantile", "kde", "kmeans", "gaussian", "hclust", "symbolic",
    "change_points", "entropy", "magnitude", "adaptive_magnitude",
    "percentile_magnitude", "dtw", "ordinal"
  )
  results <- lapply(methods, function(method) {
    arguments <- list(values, method = method)
    # `n_states` is not consumed by ordinal and `seed` only by the stochastic
    # discretizers; supplying either elsewhere is now an error by contract.
    if (!identical(method, "ordinal")) {
      arguments$n_states <- 3L
    }
    if (method %in% c("kmeans", "gaussian")) {
      arguments$seed <- 1L
    }
    do.call(discretize, arguments)
  })
  expect_true(all(vapply(results, inherits, logical(1L), what = "tsn_states")))

  thresholded <- discretize(values, method = "threshold", n_states = 3L,
                            breaks = stats::quantile(values, c(1 / 3, 2 / 3)))
  expect_s3_class(thresholded, "tsn_states")
})

test_that("discretize and tsn(unit = state) assign identical states", {
  set.seed(5)
  values <- cumsum(stats::rnorm(90)) + 30
  methods <- c(
    "width", "quantile", "kmeans", "gaussian", "hclust", "symbolic",
    "change_points", "entropy", "magnitude", "percentile_magnitude", "dtw"
  )
  invisible(lapply(methods, function(method) {
    seeded <- method %in% c("kmeans", "gaussian")
    direct_arguments <- list(values, method = method, n_states = 3L)
    network_arguments <- list(values, method = "visibility", unit = "state",
                              discretization = method, n_states = 3L)
    if (seeded) {
      direct_arguments$seed <- 1L
      network_arguments$seed <- 1L
    }
    direct <- do.call(discretize, direct_arguments)
    network <- do.call(tsn, network_arguments)
    network_states <- as.data.frame(network, what = "series")$state
    expect_identical(as.character(direct$state), as.character(network_states))
  }))

  ordinal_direct <- discretize(values, method = "ordinal")
  ordinal_network <- as.data.frame(tsn(values, "ordinal"), what = "series")
  expect_identical(as.character(ordinal_direct$state),
                   as.character(ordinal_network$state))
})

test_that("temporal discretizers never cross series boundaries", {
  series <- list(
    a = 1:4,
    b = c(100, 90, 80, 70)
  )
  ordinal <- discretize(series, method = "ordinal", m = 3)
  ordinal_model <- attr(ordinal, "model")

  expect_identical(
    as.character(ordinal$state[ordinal$id == "a"]),
    rep("1", 4L)
  )
  expect_identical(
    as.character(ordinal$state[ordinal$id == "b"]),
    rep("2", 4L)
  )
  expect_identical(ordinal_model$patterns, c("1-2-3", "3-2-1"))
  expect_identical(ordinal_model$n_windows, 4L)

  adaptive <- discretize(
    series,
    method = "adaptive_magnitude",
    n_states = 2
  )
  adaptive_model <- attr(adaptive, "model")
  first_by_series <- match(unique(adaptive$id), adaptive$id)
  expect_equal(adaptive_model$z_scores[first_by_series], c(0, 0))

  dtw_series <- list(a = 1:10, b = seq(20, 2, by = -2))
  dtw <- discretize(dtw_series, method = "dtw", n_states = 2)
  dtw_model <- attr(dtw, "model")
  expect_identical(dtw_model$n_windows, 18L)
  expect_identical(as.integer(table(dtw_model$window_series)), c(9L, 9L))
})

test_that("plot guides are expressed on the raw signed axis", {
  values <- c(-100, -20, -4, -1, 1, 4, 20, 100)
  magnitude <- discretize(values, method = "magnitude", n_states = 3)
  magnitude_breaks <- attr(magnitude, "breaks")
  finite_magnitude <- magnitude_breaks[is.finite(magnitude_breaks)]
  expect_identical(
    .tsn_plot_breaks(magnitude),
    sort(c(-finite_magnitude, finite_magnitude))
  )

  standardized <- discretize(
    values,
    method = "quantile",
    n_states = 3,
    transform = "zscore"
  )
  standardized_breaks <- attr(standardized, "breaks")
  expected <- mean(values) +
    standardized_breaks[is.finite(standardized_breaks)] * stats::sd(values)
  expect_equal(.tsn_plot_breaks(standardized), expected)

  adaptive <- discretize(
    rep(c(-2, -1, 0, 1, 2), 4),
    method = "adaptive_magnitude",
    n_states = 3
  )
  expect_length(.tsn_plot_breaks(adaptive), 0L)
})

test_that("tsn state aggregation uses the same group-safe temporal states", {
  series <- list(
    a = 1:4,
    b = c(100, 90, 80, 70)
  )
  states <- discretize(series, method = "ordinal", m = 3)
  network <- tsn(series, "ordinal", m = 3)

  expect_s3_class(network$source$state, "factor")
  expect_identical(as.character(network$source$state),
                   as.character(states$state))
  expect_setequal(network$nodes$label, c("1", "2"))
})

test_that("custom labels and transforms are honoured", {
  values <- c(1, 5, 2, 9, 3, 7, 4, 8, 6, 2)
  labelled <- discretize(values, method = "quantile", n_states = 3L,
                         labels = c("low", "mid", "high"))
  expect_setequal(levels(labelled$state), c("low", "mid", "high"))
  expect_error(
    discretize(values, method = "quantile", n_states = 3L,
               labels = c("a", "b")),
    "labels"
  )

  logged <- discretize(c(1, 10, 100, 1000, 5, 50), method = "width",
                       n_states = 2L, transform = "log")
  standard <- discretize(c(1, 10, 100, 1000, 5, 50), method = "width",
                         n_states = 2L, transform = "none")
  expect_false(identical(as.character(logged$state),
                         as.character(standard$state)))

  zscored <- discretize(values, method = "quantile", transform = "zscore")
  expect_s3_class(zscored, "tsn_states")
  expect_error(discretize(rep(1, 5), method = "quantile", transform = "zscore"),
               "non-constant")
})

test_that("discretize returns the documented tidy contract", {
  result <- discretize(c(1, 5, 2, 9, 3, 7, 4, 8), method = "quantile",
                       n_states = 3L)

  expect_s3_class(result, "tsn_states")
  expect_identical(names(result),
                   c("id", "time", "value", "state", "probability"))
  overview <- summary(result)
  expect_identical(names(overview),
                   c("state", "count", "proportion", "mean_value"))
})

test_that("discretize accepts a tsn object and its own plot renders", {
  network <- tsn(c(3, 1, 4, 2, 5, 3, 6, 2, 7, 1, 8), "hvg")
  result <- discretize(network, method = "quantile", n_states = 3L)
  expect_s3_class(result, "tsn_states")

  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output)
  on.exit({
    grDevices::dev.off()
    unlink(output)
  }, add = TRUE)
  expect_invisible(plot(result, overlay = "vertical"))
})

test_that("discretize honours series selection for tsn inputs", {
  network <- tsn(
    list(a = 1:12, b = 21:32),
    method = "distance", unit = "series"
  )
  selected <- discretize(
    network,
    series = "b",
    method = "quantile",
    n_states = 2
  )

  expect_identical(unique(selected$id), "b")
  expect_error(
    discretize(network, series = "missing", n_states = 2),
    "Unknown series"
  )
})
