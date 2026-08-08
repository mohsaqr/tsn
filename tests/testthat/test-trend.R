# Independent single-series reference used to verify the vectorized
# implementation.
trend_reference_metric <- list(
  ols = function(values, time) stats::cov(time, values) / stats::var(time),
  robust = function(values, time) {
    pairs <- utils::combn(seq_along(time), 2L)
    slopes <- (values[pairs[2L, ]] - values[pairs[1L, ]]) /
      (time[pairs[2L, ]] - time[pairs[1L, ]])
    stats::median(slopes, na.rm = TRUE)
  },
  spearman = function(values, time) {
    corr <- stats::cor(time, values, method = "spearman", use = "complete.obs")
    corr * stats::sd(values, na.rm = TRUE) / stats::sd(time, na.rm = TRUE)
  },
  kendall = function(values, time) {
    corr <- stats::cor(time, values, method = "kendall", use = "complete.obs")
    corr * stats::sd(values, na.rm = TRUE) / stats::sd(time, na.rm = TRUE)
  }
)

trend_reference_roll <- function(fun, values, time, window, align) {
  n <- length(values)
  out <- rep(NA_real_, n)
  left <- (window - 1L) %/% 2L
  right <- window - 1L - left
  start <- 1L
  end <- n
  if (align == "center") {
    start <- 1L + left
    end <- n - right
  } else if (align == "right") {
    start <- window
  } else {
    end <- n - window + 1L
  }
  indices <- seq.int(start, end)
  windows <- lapply(indices, function(i) {
    if (align == "center") {
      seq.int(i - left, i + right)
    } else if (align == "right") {
      seq.int(i - window + 1L, i)
    } else {
      seq.int(i, i + window - 1L)
    }
  })
  out[indices] <- vapply(
    windows,
    function(index) fun(values = values[index], time = time[index]),
    numeric(1L)
  )
  out
}

trend_reference_states <- function(values, window, slope, epsilon, align,
                                   turbulence_threshold = 5,
                                   flat_to_turbulent_factor = 1.5) {
  time <- seq_along(values)
  n <- length(values)
  state <- rep("Initial", n)
  metric_values <- trend_reference_roll(trend_reference_metric[[slope]],
                                        values, time, window, align)
  lower <- -epsilon
  upper <- epsilon
  valid <- !is.na(metric_values)
  valid_metrics <- metric_values[valid]
  state[valid] <- ifelse(valid_metrics > upper, "Ascending",
                         ifelse(valid_metrics < lower, "Descending", "Flat"))
  volatility_window <- min(max(3L, window %/% 2L), sum(valid))
  valid_idx <- which(valid)
  n_valid <- length(valid_metrics)
  if (n_valid >= volatility_window) {
    candidate <- seq.int(volatility_window, n_valid)
    turbulent <- vapply(candidate, function(j) {
      window_metric <- valid_metrics[seq(j - volatility_window + 1L, j)]
      if (sum(!is.na(window_metric)) < 2L) return(FALSE)
      metric_sd <- stats::sd(window_metric, na.rm = TRUE)
      metric_am <- abs(mean(window_metric, na.rm = TRUE))
      metric_range <- diff(range(window_metric, na.rm = TRUE))
      if (is.na(metric_sd) || is.na(metric_am) || is.na(metric_range) ||
          metric_sd == 0 || metric_am == 0) return(FALSE)
      combined <- metric_sd / metric_am + 0.5 * (metric_range / metric_am)
      k <- valid_idx[j]
      effective <- if (state[k] == "Flat") {
        turbulence_threshold * flat_to_turbulent_factor
      } else {
        turbulence_threshold
      }
      combined > effective
    }, logical(1L))
    state[valid_idx[candidate[turbulent]]] <- "Turbulent"
  }
  state
}

test_that("trend classifications match the upstream reference exactly", {
  set.seed(2024)
  configs <- expand.grid(
    slope = c("ols", "robust", "spearman", "kendall"),
    align = c("center", "right", "left"),
    stringsAsFactors = FALSE
  )
  invisible(Map(function(slope, align) {
    values <- cumsum(stats::rnorm(70))
    observed <- trend(values, window = 9L, slope = slope, epsilon = 0.08,
                      align = align)
    expected <- trend_reference_states(values, window = 9L, slope = slope,
                                       epsilon = 0.08, align = align)
    expect_identical(as.character(observed$state), expected)
  }, configs$slope, configs$align))
})

test_that("trend returns the documented tidy contract", {
  set.seed(1)
  result <- trend(cumsum(stats::rnorm(60)), window = 8L, slope = "ols")

  expect_s3_class(result, "tsn_trend")
  expect_identical(names(result), c("id", "time", "value", "metric", "state"))
  expect_s3_class(result$state, "factor")
  expect_true(all(levels(result$state) %in% c(
    "Ascending", "Descending", "Flat", "Turbulent", "Missing Data", "Initial"
  )))
  overview <- summary(result)
  expect_identical(names(overview), c("state", "count", "proportion"))
  expect_equal(sum(overview$proportion), 1)
})

test_that("trend honours series selection for tsn inputs", {
  network <- tsn(
    list(a = 1:20, b = 21:40),
    method = "distance", unit = "series"
  )
  selected <- trend(network, series = "b", window = 5, slope = "ols")

  expect_identical(unique(selected$id), "b")
  expect_error(
    trend(network, series = "missing", window = 5),
    "Unknown series"
  )
})

test_that("trend supports missing values and nonnumeric time labels", {
  values <- c(1, 2, NA, 4, 5, 6, 7, 8)
  missing <- trend(values, window = 3, slope = "ols")
  expect_identical(as.character(missing$state[3L]), "Missing Data")
  expect_false(any(is.infinite(missing$metric), na.rm = TRUE))

  labelled <- data.frame(
    id = "a",
    occasion = paste0("T", seq_len(12L)),
    value = seq_len(12L)
  )
  expect_warning(
    labelled_result <- trend(
      labelled,
      value = "value", id = "id", time = "occasion",
      window = 5, slope = "ols"
    ),
    NA
  )
  expect_true(any(is.finite(labelled_result$metric)))
  expect_identical(labelled_result$time, labelled$occasion)
})

test_that("growth factors remain finite and thresholds are validated", {
  growth <- trend(
    c(0, 1, 2, 3, 4, 5),
    window = 3,
    method = "growth_factor"
  )
  expect_false(any(is.infinite(growth$metric), na.rm = TRUE))
  expect_error(trend(1:10, window = 3, epsilon = -0.1))
  expect_error(trend(1:10, window = 3, turbulence_threshold = -1))
  expect_error(trend(1:10, window = 3, flat_to_turbulent_factor = 0))
})

test_that("growth_factor and adaptive window behave", {
  result <- trend(c(1, 2, 3, 4, 5, 4, 3, 2, 1, 2, 3), window = 3L,
                  method = "growth_factor")
  expect_s3_class(result, "tsn_trend")

  auto <- trend(cumsum(stats::rnorm(80)))
  expect_equal(attr(auto, "parameters")$window, max(3L, round(80 / 10)))
})

test_that("trend accepts a tsn object and its own plot renders", {
  network <- tsn(cumsum(stats::rnorm(60)) + 20, "hvg")
  result <- trend(network, window = 7L, slope = "ols")
  expect_s3_class(result, "tsn_trend")

  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output)
  on.exit({
    grDevices::dev.off()
    unlink(output)
  }, add = TRUE)
  expect_invisible(plot(result))
})

test_that("trend plots tolerate missing and all-missing values", {
  partly_missing <- trend(
    c(1, 2, NA, 4, 5, 6, 7, 8),
    window = 3,
    slope = "ols"
  )
  all_missing <- trend(
    rep(NA_real_, 8),
    window = 3,
    slope = "ols"
  )
  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output)
  on.exit({
    grDevices::dev.off()
    unlink(output)
  }, add = TRUE)

  expect_invisible(plot(partly_missing))
  expect_invisible(plot(all_missing, "panels"))
  expect_identical(.tsn_expand_range(rep(NA_real_, 3)), c(-0.5, 0.5))
})
