# Audit of R/trend.R: formulas checked against independent base-R
# computations, rolling-window placement, classification boundaries, missing
# values, irregular time, and the print/summary/plot contracts.

audit_trend_series <- list(
  increasing = c(1, 3, 4, 7, 8, 12),
  decreasing = c(10, 8, 7, 3, 2, 0),
  constant = rep(5, 6),
  noisy = c(0.3, -1.2, 0.8, 0.1, -0.4, 0.9)
)
audit_trend_time <- c(0, 1, 3, 4, 8, 9)

audit_theil_sen <- function(values, time) {
  slopes <- outer(values, values, "-") / outer(time, time, "-")
  stats::median(slopes[upper.tri(slopes)])
}

test_that("OLS slope equals coef(lm()) on irregular time", {
  observed <- vapply(audit_trend_series, .tsn_metric_ols,
                     numeric(1L), time = audit_trend_time)
  expected <- vapply(audit_trend_series, function(values) {
    unname(stats::coef(stats::lm(values ~ audit_trend_time))[2L])
  }, numeric(1L))
  expect_equal(observed, expected, tolerance = 1e-10)
})

test_that("Theil-Sen slope is the median of all pairwise slopes", {
  observed <- vapply(audit_trend_series, .tsn_metric_theil_sen,
                     numeric(1L), time = audit_trend_time)
  expected <- vapply(audit_trend_series, audit_theil_sen,
                     numeric(1L), time = audit_trend_time)
  expect_equal(observed, expected, tolerance = sqrt(.Machine$double.eps))
})

test_that("rank slopes rescale cor() by sd(value) / sd(time)", {
  non_constant <- audit_trend_series[c("increasing", "decreasing", "noisy")]
  reference <- function(method) {
    vapply(non_constant, function(values) {
      stats::cor(audit_trend_time, values, method = method) *
        stats::sd(values) / stats::sd(audit_trend_time)
    }, numeric(1L))
  }
  expect_equal(
    vapply(non_constant, .tsn_metric_spearman, numeric(1L),
           time = audit_trend_time),
    reference("spearman"), tolerance = sqrt(.Machine$double.eps)
  )
  expect_equal(
    vapply(non_constant, .tsn_metric_kendall, numeric(1L),
           time = audit_trend_time),
    reference("kendall"), tolerance = sqrt(.Machine$double.eps)
  )
  # Ties in the values: Kendall uses tau-b, 12 / sqrt(15 * 12).
  tied <- c(1, 1, 2, 2, 3, 3)
  expect_equal(
    .tsn_metric_kendall(tied, seq_along(tied)) * stats::sd(seq_along(tied)) /
      stats::sd(tied),
    12 / sqrt(15 * 12), tolerance = sqrt(.Machine$double.eps)
  )
})

test_that("growth factor is last over first non-missing value", {
  expect_equal(.tsn_metric_growth(c(2, 3, 8), 1:3), 4)
  expect_equal(.tsn_metric_growth(c(NA, 2, 5, NA), 1:4), 2.5)
  expect_true(is.na(.tsn_metric_growth(c(0, 1, 2), 1:3)))
  expect_true(is.na(.tsn_metric_growth(c(NA, NA, 3), 1:3)))
})

test_that("a constant window has slope 0 under every estimator", {
  constant <- rep(5, 6)
  metrics <- list(ols = .tsn_metric_ols, robust = .tsn_metric_theil_sen,
                  spearman = .tsn_metric_spearman,
                  kendall = .tsn_metric_kendall)
  observed <- vapply(metrics, function(metric) {
    metric(constant, audit_trend_time)
  }, numeric(1L))
  expect_equal(unname(observed), rep(0, 4L), tolerance = 1e-12)
  expect_no_warning(.tsn_metric_spearman(constant, audit_trend_time))
  expect_no_warning(.tsn_metric_kendall(constant, audit_trend_time))
})

test_that("a constant run is Flat, not Initial, for the rank slopes", {
  values <- c(1, 2, 3, 4, 5, rep(5, 6), 6, 7, 8, 9, 10)
  invisible(lapply(c("spearman", "kendall", "ols", "robust"), function(s) {
    expect_no_warning(
      result <- trend(values, window = 3, slope = s)
    )
    states <- as.character(result$state)
    expect_identical(states[6:10], rep("Flat", 5L), label = s)
    expect_identical(states[c(1L, 16L)], rep("Initial", 2L), label = s)
  }))
})

test_that("all estimators use the non-missing values of a window", {
  with_gap <- c(1, 2, NA, 4, 5, 6, 7, 8)
  invisible(lapply(c("ols", "robust", "spearman", "kendall"), function(s) {
    result <- trend(with_gap, window = 3, slope = s)
    expect_equal(result$metric[c(2L, 4L)], c(1, 1),
                 tolerance = sqrt(.Machine$double.eps), label = s)
    expect_identical(
      as.character(result$state),
      c("Initial", "Ascending", "Missing Data", rep("Ascending", 4L),
        "Initial"),
      label = s
    )
  }))
  # A window with fewer than two observed values has no metric.
  expect_true(is.na(.tsn_metric_ols(c(NA, 2, NA), 1:3)))
  expect_true(is.na(.tsn_metric_spearman(c(NA, 2, NA), 1:3)))
})

test_that("rolling windows are placed as documented", {
  centre_time <- function(values, time) mean(time)
  times <- seq_len(10L)
  expect_identical(
    .tsn_roll(centre_time, times, times, 3L, "center"),
    c(NA, 2:9, NA) + 0
  )
  # Even window: the centre is the earlier of the two middle points.
  expect_identical(
    .tsn_roll(centre_time, times, times, 4L, "center"),
    c(NA, seq(2.5, 8.5, by = 1), NA, NA)
  )
  expect_identical(
    .tsn_roll(centre_time, times, times, 4L, "right"),
    c(NA, NA, NA, seq(2.5, 8.5, by = 1))
  )
  expect_identical(
    .tsn_roll(centre_time, times, times, 4L, "left"),
    c(seq(2.5, 8.5, by = 1), NA, NA, NA)
  )
  # window = n gives one value; window > n gives none.
  expect_identical(
    .tsn_roll(centre_time, 1:5, 1:5, 5L, "center"),
    c(NA, NA, 3, NA, NA)
  )
  expect_true(all(is.na(.tsn_roll(centre_time, 1:5, 1:5, 6L, "center"))))
})

test_that("window bounds are enforced with informative errors", {
  expect_error(trend(1:10, window = 10), "between 3 and 9")
  expect_error(trend(1:10, window = 2), "between 3 and 9")
  expect_error(trend(1:10, window = 2.5), "whole number")
  expect_error(trend(1:10, window = NA), "whole number")
  expect_error(trend(1:3), "at least 4 observations")
  expect_identical(attr(trend(1:10, window = 9), "parameters")$window, 9L)
  expect_error(trend(1:10, window = 3, epsilon = -1), "epsilon")
  expect_error(trend(1:10, window = 3, epsilon = c(0.1, 0.2)), "epsilon")
})

test_that("epsilon thresholds are strict: a slope of exactly epsilon is Flat", {
  # Integer data give an exactly representable OLS slope of 1.
  values <- c(0, 1, 2, 3, 4, 5)
  expect_identical(.tsn_metric_ols(values[1:3], 1:3), 1)
  at_boundary <- trend(values, window = 3, slope = "ols", epsilon = 1)
  above <- trend(values, window = 3, slope = "ols", epsilon = 0.99)
  below <- trend(-values, window = 3, slope = "ols", epsilon = 1)
  expect_identical(unique(as.character(at_boundary$state[2:5])), "Flat")
  expect_identical(unique(as.character(above$state[2:5])), "Ascending")
  expect_identical(unique(as.character(below$state[2:5])), "Flat")
})

test_that("metrics use real time, not the row index", {
  long <- data.frame(
    id = "a", time = c(0, 1, 3, 4, 8, 9, 10, 15),
    value = 2 * c(0, 1, 3, 4, 8, 9, 10, 15)
  )
  result <- trend(long, value = "value", id = "id", time = "time",
                  window = 3, slope = "ols")
  expect_equal(result$metric[2:7], rep(2, 6L),
               tolerance = sqrt(.Machine$double.eps))
  dated <- data.frame(id = "a", time = as.Date("2024-01-01") + c(0, 1, 3, 4),
                      value = c(0, 1, 3, 4))
  dated_result <- trend(dated, value = "value", id = "id", time = "time",
                        window = 3, slope = "ols")
  expect_equal(dated_result$metric[2:3], c(1, 1),
               tolerance = sqrt(.Machine$double.eps))
})

test_that("non-positive values warn for growth factors", {
  expect_warning(
    trend(c(-10, -8, -6, -4, -2, -1), window = 3, method = "growth_factor"),
    class = "tsn_trend_nonpositive_growth"
  )
  expect_warning(
    trend(c(0, 1, 2, 3, 4, 5), window = 3, method = "growth_factor"),
    class = "tsn_trend_nonpositive_growth"
  )
  expect_no_warning(
    positive <- trend(c(1, 2, 4, 8, 16, 32), window = 3,
                      method = "growth_factor")
  )
  expect_equal(positive$metric[2:5], rep(4, 4L),
               tolerance = sqrt(.Machine$double.eps))
  expect_no_warning(trend(c(-1, 2, 3, 4, 5), window = 3, slope = "ols"))
})

test_that("summary is a tidy count table over the observed states", {
  result <- trend(c(1, 2, NA, 4, 5, 6, 7, 8), window = 3, slope = "ols")
  overview <- summary(result)
  expect_s3_class(overview, "data.frame")
  expect_identical(names(overview), c("state", "count", "proportion"))
  expect_identical(overview$state, c("Ascending", "Missing Data", "Initial"))
  expect_identical(overview$count, c(5L, 1L, 2L))
  expect_equal(sum(overview$proportion), 1)
})

test_that("print survives subset() and never points at `$`", {
  result <- trend(cumsum(c(0.4, -1, 2, 0.3, 0.1, -0.5, 1, 1.2, -0.3, 0.8)),
                  window = 3)
  printed <- utils::capture.output(print(result))
  expect_match(printed[1L], "slope \\(robust\\), window 3")
  expect_false(any(grepl("$", printed, fixed = TRUE)))

  subsetted <- subset(result, state != "Initial")
  expect_null(attr(subsetted, "parameters"))
  expect_no_error(subset_printed <- utils::capture.output(print(subsetted)))
  expect_match(subset_printed[1L], "8 observations across 1 series")
  utils::capture.output(expect_invisible(print(subsetted)))
})

test_that("plot rejects palettes it would otherwise ignore", {
  result <- trend(cumsum(c(0.4, -1, 2, 0.3, 0.1, -0.5, 1, 1.2, -0.3, 0.8)),
                  window = 3)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_error(plot(result, palette = c("red", "blue")), "named by trend")
  expect_error(plot(result, palette = 1:3), "named by trend")
  expect_error(plot(result, palette = c(ascending = "red")),
               "Unknown trend state")
  expect_invisible(plot(result, palette = c(Ascending = "forestgreen")))
  expect_identical(
    .tsn_trend_colors(c(Flat = "#000000"))[["Flat"]], "#000000"
  )
})

test_that("every plot type restores par(), including cex", {
  one <- trend(cumsum(sin(seq_len(50L)) + 0.1), window = 5)
  many <- trend(
    lapply(stats::setNames(seq_len(6L), paste0("s", seq_len(6L))),
           function(shift) cumsum(sin(seq_len(40L) + shift))),
    window = 5
  )
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(1, 2, 3, 4), cex = 0.9)
  fields <- c("mar", "cex", "mfrow", "mfcol", "mgp", "oma")
  before <- graphics::par(fields)
  # Only the views drawn in R/trend.R are checked for cex; the ribbon and
  # heatmap views are drawn by R/plot.R helpers.
  invisible(lapply(c("points", "panels"), function(type) {
    expect_invisible(plot(one, type))
    expect_identical(graphics::par(fields), before, label = type)
    expect_invisible(plot(many, type))
    expect_identical(graphics::par(fields), before, label = type)
  }))
  invisible(lapply(c("ribbon", "heatmap"), function(type) {
    expect_invisible(plot(one, type))
    expect_invisible(plot(many, type))
  }))
})
