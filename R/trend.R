#' Classify Rolling Trends in a Time Series
#'
#' `trend()` computes a rolling trend metric for each observation and classifies
#' every point as `"Ascending"`, `"Descending"`, `"Flat"`, `"Turbulent"`,
#' `"Missing Data"`, or `"Initial"`. It is a faithful base-R port of the
#' upstream `tsn::trend()` behaviour: rolling slope (OLS, Theil-Sen, Spearman,
#' or Kendall) or growth-factor metrics, an `epsilon` flat band, and a
#' volatility override that reclassifies noisy segments as turbulent.
#'
#' The metric is first thresholded with `epsilon`: values above `+epsilon`
#' (or above `1 + epsilon` for growth factors) are ascending, values below
#' `-epsilon` (or `1 - epsilon`) are descending, and the remainder are flat.
#' A rolling volatility measure (coefficient of variation plus half the range
#' factor of the metric) then overrides these labels with `"Turbulent"` when it
#' exceeds `turbulence_threshold`. Segments already labelled `"Flat"` use the
#' higher threshold `turbulence_threshold * flat_to_turbulent_factor`, making
#' them more resistant to noise-driven reclassification.
#'
#' The thresholds are strict: a metric exactly equal to `+epsilon` or
#' `-epsilon` is flat. Metrics are computed against the real time coordinates,
#' so irregular spacing is respected and the slope is in value units per time
#' unit: per day for `Date` time and per second for `POSIXct` time (choose
#' `epsilon` accordingly). Character or factor time labels are replaced by
#' their position `1, 2, ...`. Each window uses its non-missing values; a
#' window whose values are all identical has a slope of `0` under every
#' estimator.
#'
#' `"Missing Data"` marks observations whose value is `NA`. `"Initial"` marks
#' observations without a defined metric: the edge positions that lack a full
#' window (both ends for `align = "center"`), and windows whose metric is
#' undefined, such as a growth factor whose first value is `0`.
#'
#' @param data A numeric vector, `ts`, matrix, named list of numeric vectors,
#'   data frame, or a `tsn` object (its source series are used).
#' @param value Optional value-column name for long data.
#' @param id Optional series-ID column name for long data.
#' @param time Optional time-column name for long data.
#' @param series Optional series IDs or wide-data column names to select.
#' @param window Rolling-window width. When `NULL`, the adaptive default
#'   `max(3, min(n, round(n / 10)))` is used, where `n` is the shortest series
#'   length. Must satisfy `2 < window < n`.
#' @param method Trend metric: `"slope"` (default) or `"growth_factor"`. The
#'   growth factor is the last non-missing value in the window divided by the
#'   first; it is only meaningful for strictly positive values, and a warning
#'   of class `tsn_trend_nonpositive_growth` is raised when the data contain a
#'   zero or negative value.
#' @param slope Slope estimator when `method = "slope"`: `"robust"` (Theil-Sen,
#'   the median of all pairwise slopes; the default), `"ols"` (least-squares
#'   slope), `"spearman"`, or `"kendall"`. The two rank options rescale the
#'   rank correlation (Kendall's tau-b under ties) by
#'   `sd(value) / sd(time)`, so they share the units of a slope.
#' @param epsilon Flat-band half-width, in metric units (value units per time
#'   unit for slopes). Default `0.05`.
#' @param turbulence_threshold Baseline volatility threshold for the turbulent
#'   override. Default `5`.
#' @param flat_to_turbulent_factor Multiplier applied to `turbulence_threshold`
#'   for points already classified as flat. Default `1.5`.
#' @param align Window alignment: `"center"` (default), `"right"`, or `"left"`.
#'   The metric is assigned to the centre, rightmost, or leftmost point of the
#'   window respectively. For an even `window` the centre is the earlier of
#'   the two middle points.
#' @return A tidy data frame of class `tsn_trend` with one row per observation
#'   and columns `id`, `time`, `value`, `metric`, and `state` (a factor over the
#'   six trend classes). Print, summary, and plot methods are provided.
#' @examples
#' set.seed(123)
#' walk <- cumsum(rnorm(120))
#' trend(walk, window = 15, slope = "ols", epsilon = 0.1)
#'
#' trend(c(1, 2, 3, 4, 5, 4, 3, 2, 1), window = 3, method = "growth_factor")
#' @export
trend <- function(data, value = NULL, id = NULL, time = NULL, series = NULL,
                  window = NULL, method = "slope", slope = "robust",
                  epsilon = 0.05, turbulence_threshold = 5,
                  flat_to_turbulent_factor = 1.5, align = "center") {
  method <- match.arg(method, c("slope", "growth_factor"))
  slope <- match.arg(slope, c("robust", "ols", "spearman", "kendall"))
  align <- match.arg(align, c("center", "right", "left"))
  stopifnot(
    "`epsilon` must be a single finite number >= 0" =
      is.numeric(epsilon) && length(epsilon) == 1L && is.finite(epsilon) &&
      epsilon >= 0,
    "`turbulence_threshold` must be a single finite number >= 0" =
      is.numeric(turbulence_threshold) &&
      length(turbulence_threshold) == 1L &&
      is.finite(turbulence_threshold) && turbulence_threshold >= 0,
    "`flat_to_turbulent_factor` must be a single finite number > 0" =
      is.numeric(flat_to_turbulent_factor) &&
      length(flat_to_turbulent_factor) == 1L &&
      is.finite(flat_to_turbulent_factor) && flat_to_turbulent_factor > 0
  )

  source <- if (inherits(data, "tsn")) {
    .tsn_select_canonical(
      data$source[c("id", "time", "value")],
      series = series
    )
  } else {
    .tsn_normalize_input(
      .tsn_select_data(data, series = series, value = value, id = id),
      value = value, id = id, time = time, allow_missing = TRUE
    )
  }

  n_min <- min(vapply(split(source$value, source$id), length, integer(1L)))
  if (n_min < 4L) {
    stop(sprintf(paste0(
      "Every series needs at least 4 observations for a rolling trend; ",
      "the shortest has %d."
    ), n_min), call. = FALSE)
  }
  window <- if (is.null(window)) {
    max(3L, min(n_min, round(n_min / 10)))
  } else {
    stopifnot(
      "`window` must be a single whole number" =
        is.numeric(window) && length(window) == 1L && is.finite(window) &&
        abs(window - round(window)) < sqrt(.Machine$double.eps)
    )
    as.integer(round(window))
  }
  if (method == "growth_factor") {
    present_values <- source$value[!is.na(source$value)]
    non_positive <- sum(present_values <= 0)
    if (non_positive > 0L) {
      warning(warningCondition(sprintf(paste0(
        "Growth factors (last / first value in each window) are only ",
        "meaningful for strictly positive values; %d value(s) are zero or ",
        "negative. Windows starting at 0 get no metric, and a rising series ",
        "of negative values reads as descending."
      ), non_positive), class = "tsn_trend_nonpositive_growth"))
    }
  }
  if (!(window > 2L && window < n_min)) {
    stop(sprintf("`window` must be between 3 and %d (the shortest series).",
                 n_min - 1L), call. = FALSE)
  }
  metric_fun <- if (method == "growth_factor") .tsn_metric_growth else
    switch(slope,
           ols = .tsn_metric_ols,
           robust = .tsn_metric_theil_sen,
           spearman = .tsn_metric_spearman,
           kendall = .tsn_metric_kendall)

  groups <- split(
    source,
    factor(source$id, levels = unique(source$id)),
    drop = TRUE
  )
  per_group <- lapply(groups, function(group) {
    classified <- .tsn_trend_series(
      values = group$value,
      time = .tsn_time_coordinates(group$time),
      window = window,
      method = method,
      metric_fun = metric_fun,
      epsilon = epsilon,
      turbulence_threshold = turbulence_threshold,
      flat_to_turbulent_factor = flat_to_turbulent_factor,
      align = align
    )
    data.frame(
      id = group$id,
      time = group$time,
      value = group$value,
      metric = classified$metric,
      state = classified$state,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, per_group)
  rownames(out) <- NULL
  out$state <- factor(
    out$state,
    levels = c("Ascending", "Descending", "Flat", "Turbulent",
               "Missing Data", "Initial")
  )
  structure(
    out,
    class = c("tsn_trend", "data.frame"),
    parameters = list(
      window = window, method = method, slope = slope, epsilon = epsilon,
      turbulence_threshold = turbulence_threshold,
      flat_to_turbulent_factor = flat_to_turbulent_factor, align = align
    )
  )
}

#' Classify a single series into trend states
#'
#' @param values Numeric values (may contain `NA`).
#' @param time Numeric time values.
#' @param window Rolling-window width.
#' @param method Trend method.
#' @param metric_fun Metric function of `(values, time)`.
#' @param epsilon Flat-band half-width.
#' @param turbulence_threshold Baseline volatility threshold.
#' @param flat_to_turbulent_factor Flat resistance multiplier.
#' @param align Window alignment.
#' @return A list with numeric `metric` and character `state` vectors.
#' @noRd
.tsn_trend_series <- function(values, time, window, method, metric_fun,
                              epsilon, turbulence_threshold,
                              flat_to_turbulent_factor, align) {
  n <- length(values)
  state <- rep("Initial", n)
  metric_values <- .tsn_roll(metric_fun, values, time, window, align)
  values_na <- is.na(values)
  state[values_na] <- "Missing Data"
  neutral <- if (method == "growth_factor") 1 else 0
  lower <- neutral - epsilon
  upper <- neutral + epsilon
  valid <- !is.na(metric_values) & !values_na
  valid_metrics <- metric_values[valid]
  state[valid] <- ifelse(
    valid_metrics > upper, "Ascending",
    ifelse(valid_metrics < lower, "Descending", "Flat")
  )

  volatility_window <- min(max(3L, window %/% 2L), sum(valid))
  valid_idx <- which(valid)
  n_valid <- length(valid_metrics)
  if (n_valid >= volatility_window && volatility_window >= 2L) {
    js <- seq.int(volatility_window, n_valid)
    combined_vol <- vapply(js, function(j) {
      window_metric <- valid_metrics[seq.int(j - volatility_window + 1L, j)]
      if (sum(!is.na(window_metric)) < 2L) {
        return(NA_real_)
      }
      metric_sd <- stats::sd(window_metric, na.rm = TRUE)
      metric_am <- abs(mean(window_metric, na.rm = TRUE))
      metric_range <- diff(range(window_metric, na.rm = TRUE))
      if (!is.finite(metric_sd) || !is.finite(metric_am) ||
          !is.finite(metric_range) || metric_sd == 0 || metric_am == 0) {
        return(NA_real_)
      }
      metric_sd / metric_am + 0.5 * (metric_range / metric_am)
    }, numeric(1L))
    k <- valid_idx[js]
    base_trend <- state[k]
    effective <- ifelse(base_trend == "Flat",
                        turbulence_threshold * flat_to_turbulent_factor,
                        turbulence_threshold)
    turbulent <- !is.na(combined_vol) & base_trend != "Missing Data" &
      combined_vol > effective
    state[k[turbulent]] <- "Turbulent"
  }
  list(metric = metric_values, state = state)
}

#' Apply a metric over rolling windows without a loop
#'
#' @param fun Metric function of `(values, time)`.
#' @param values Numeric values.
#' @param time Numeric time values.
#' @param window Window width.
#' @param align Window alignment.
#' @return A numeric vector aligned with `values`; positions without a full
#'   window are `NA`.
#' @noRd
.tsn_roll <- function(fun, values, time, window, align) {
  n <- length(values)
  out <- rep(NA_real_, n)
  left <- (window - 1L) %/% 2L
  right <- window - 1L - left
  bounds <- switch(
    align,
    center = list(start = 1L + left, end = n - right, offset = -left),
    right = list(start = window, end = n, offset = -(window - 1L)),
    left = list(start = 1L, end = n - window + 1L, offset = 0L)
  )
  if (bounds$end < bounds$start) {
    return(out)
  }
  positions <- seq.int(bounds$start, bounds$end)
  out[positions] <- vapply(
    positions,
    function(i) {
      window_index <- seq.int(i + bounds$offset, i + bounds$offset + window - 1L)
      fun(values[window_index], time[window_index])
    },
    numeric(1L)
  )
  out
}

#' Ordinary-least-squares slope metric over the non-missing values
#' @noRd
.tsn_metric_ols <- function(values, time) {
  complete <- !is.na(values) & !is.na(time)
  if (sum(complete) < 2L) {
    return(NA_real_)
  }
  values <- values[complete]
  time <- time[complete]
  time_variance <- stats::var(time)
  if (!(time_variance > 0)) {
    return(NA_real_)
  }
  stats::cov(time, values) / time_variance
}

#' Theil-Sen robust slope metric (vectorized pairwise slopes)
#' @noRd
.tsn_metric_theil_sen <- function(values, time) {
  n <- length(time)
  if (n < 2L) {
    return(NA_real_)
  }
  pairs <- utils::combn(n, 2L)
  slopes <- (values[pairs[2L, ]] - values[pairs[1L, ]]) /
    (time[pairs[2L, ]] - time[pairs[1L, ]])
  stats::median(slopes, na.rm = TRUE)
}

#' Rank-correlation slope metric over the non-missing values
#'
#' The rank correlation rescaled by `sd(values) / sd(time)`, with both
#' standard deviations taken over the same complete pairs as the correlation.
#' A window of identical values has slope `0`: its correlation is undefined
#' but the spread factor is zero.
#'
#' @param method `"spearman"` or `"kendall"` (R computes tau-b under ties).
#' @noRd
.tsn_metric_rank_slope <- function(values, time, method) {
  complete <- !is.na(values) & !is.na(time)
  if (sum(complete) < 2L) {
    return(NA_real_)
  }
  values <- values[complete]
  time <- time[complete]
  time_spread <- stats::sd(time)
  if (!(time_spread > 0)) {
    return(NA_real_)
  }
  value_spread <- stats::sd(values)
  if (!is.finite(value_spread)) {
    return(NA_real_)
  }
  if (!(value_spread > 0)) {
    return(0)
  }
  stats::cor(time, values, method = method) * value_spread / time_spread
}

#' Spearman-based slope metric
#' @noRd
.tsn_metric_spearman <- function(values, time) {
  .tsn_metric_rank_slope(values, time, method = "spearman")
}

#' Kendall-based slope metric
#' @noRd
.tsn_metric_kendall <- function(values, time) {
  .tsn_metric_rank_slope(values, time, method = "kendall")
}

#' Growth-factor metric (last over first non-missing value)
#' @noRd
.tsn_metric_growth <- function(values, time) {
  present <- values[!is.na(values)]
  n <- length(present)
  if (n < 2L) {
    return(NA_real_)
  }
  if (present[1L] == 0) {
    return(NA_real_)
  }
  present[n] / present[1L]
}

#' Print a trend classification
#'
#' @param x A `tsn_trend` result.
#' @param ... Passed to `print.data.frame()`.
#' @return `x`, invisibly.
#' @export
print.tsn_trend <- function(x, ...) {
  parameters <- attr(x, "parameters")
  # subset() and `[` drop the parameters attribute but keep the class, so the
  # header falls back to the counts alone rather than failing.
  settings <- if (is.null(parameters)) {
    ""
  } else {
    sprintf(
      " %s (%s), window %d",
      parameters$method,
      if (identical(parameters$method, "slope")) parameters$slope else "growth",
      parameters$window
    )
  }
  cat(sprintf(
    "<tsn_trend>%s: %d observations across %d series\n",
    settings,
    nrow(x),
    length(unique(x$id))
  ))
  print.data.frame(utils::head(as.data.frame(x), 10L), row.names = FALSE, ...)
  invisible(x)
}

#' Summarize a trend classification
#'
#' @param object A `tsn_trend` result.
#' @param ... Ignored.
#' @return A tidy data frame with one row per observed trend state, giving the
#'   `count` and `proportion` of observations in that state.
#' @export
summary.tsn_trend <- function(object, ...) {
  stopifnot(inherits(object, "tsn_trend"))
  counts <- table(object$state)
  present <- counts[counts > 0]
  data.frame(
    state = names(present),
    count = as.integer(present),
    proportion = as.numeric(present) / sum(present),
    stringsAsFactors = FALSE
  )
}

#' Plot a trend classification
#'
#' Four views of a trend classification, selected with `type`. `"points"`
#' (default) draws each series with observations coloured by trend state;
#' `"ribbon"` runs the trend classification as a strip underneath the clean
#' series line; `"heatmap"` draws one row per series with one coloured tile
#' per observation, comparing trend timing across many series; `"panels"`
#' stacks each series above its rolling trend metric (the slope or
#' correlation `trend()` classified on) so a state change can be read
#' against the metric that produced it.
#'
#' @param x A `tsn_trend` result.
#' @param type The view: `"points"`, `"ribbon"`, `"heatmap"`, or `"panels"`.
#' @param series Optional character vector of series IDs to display.
#' @param max_series Maximum number of series to draw (`25` for the heatmap,
#'   `4` for the panel view, `10` otherwise).
#' @param columns Number of panel columns.
#' @param palette Optional named colours overriding individual trend states
#'   (e.g. `c(Ascending = "forestgreen")`).
#' @param line_color Series line colour.
#' @param line_width Series line width.
#' @param point_size Observation point size.
#' @param strip_height Ribbon strip height as a fraction of the panel.
#' @param sort Heatmap row order: `"none"`, `"mean"`, or `"state"`.
#' @param border Whether heatmap tiles carry a thin separator border.
#' @param flat_band For `type = "panels"`, shade the `epsilon` flat band on
#'   the metric panel so the classification threshold is visible.
#' @param legend Whether to draw the legend row.
#' @param xlab,ylab Axis titles.
#' @param cex Global text size multiplier.
#' @param grid Whether to draw the background grid.
#' @param background Panel background colour.
#' @param ... Reserved for future options.
#' @return `x`, invisibly.
#' @examples
#' data(srl)
#' complete <- subset(srl, !is.na(effort))
#' classified <- trend(
#'   complete,
#'   value = "effort", id = "name", time = "day", window = 7
#' )
#' plot(classified, series = "Erik")
#' plot(classified, "ribbon", series = "Erik")
#' plot(classified, "panels", series = "Erik")
#' plot(classified, "heatmap", max_series = 12)
#' @export
plot.tsn_trend <- function(x, type = c("points", "ribbon", "heatmap", "panels"),
                           series = NULL, max_series = NULL, columns = NULL,
                           palette = NULL, line_color = "#9AA3AD",
                           line_width = 1.3, point_size = 1.05,
                           strip_height = 0.1, sort = "none", border = FALSE,
                           flat_band = TRUE, legend = TRUE,
                           xlab = "Time", ylab = "Value", cex = 1,
                           grid = TRUE, background = "#FFFFFF", ...) {
  stopifnot(inherits(x, "tsn_trend"))
  type <- match.arg(type)
  if (is.null(max_series)) {
    max_series <- switch(type, heatmap = 25L, panels = 4L, 10L)
  }
  source <- data.frame(
    id = as.character(x$id), time = x$time, value = x$value,
    metric = x$metric, state = as.character(x$state),
    stringsAsFactors = FALSE
  )
  trend_palette <- .tsn_trend_colors(palette)
  switch(
    type,
    points = .tsn_plot_trend_points(
      source = source, series = series, max_series = max_series,
      columns = columns, palette = trend_palette, line_color = line_color,
      line_width = line_width, point_size = point_size, legend = legend,
      xlab = xlab, ylab = ylab, cex = cex, grid = grid,
      background = background, ...
    ),
    ribbon = .tsn_plot_series_frame(
      source = source, series = series, overlay = "none", points = FALSE,
      trend = FALSE, columns = columns, max_series = max_series,
      scales = "free", palette = trend_palette, strip = TRUE,
      legend_title = "Trend", line_color = "#3B4252",
      line_width = line_width + 0.3, point_size = point_size,
      strip_height = strip_height, legend = legend, xlab = xlab,
      ylab = ylab, cex = cex, grid = grid, background = background, ...
    ),
    heatmap = .tsn_plot_state_heatmap(
      source = source, series = series, max_series = max_series,
      palette = trend_palette, legend_title = "Trend", sort = sort,
      border = border, legend = legend, xlab = xlab, cex = cex,
      grid = grid, background = background, ...
    ),
    panels = .tsn_plot_trend_panels(
      source = source, series = series, max_series = max_series,
      palette = trend_palette, line_color = line_color,
      line_width = line_width, point_size = point_size,
      flat_band = flat_band, epsilon = attr(x, "parameters")$epsilon,
      legend = legend, xlab = xlab, ylab = ylab, cex = cex, grid = grid,
      background = background, ...
    )
  )
  invisible(x)
}

#' Draw the state-coloured point view of a trend classification
#'
#' @param source Data frame with `id`, `time`, `value`, `state`.
#' @param palette Named trend-state colours.
#' @return `NULL`, invisibly.
#' @noRd
.tsn_plot_trend_points <- function(source, series = NULL, max_series = 10L,
                                   columns = NULL, palette,
                                   line_color = "#9AA3AD", line_width = 1.3,
                                   point_size = 1.05, legend = TRUE,
                                   xlab = "Time", ylab = "Value", cex = 1,
                                   grid = TRUE, background = "#FFFFFF", ...) {
  style <- .tsn_style(cex = cex, grid = grid, background = background)
  available <- unique(source$id)
  selected <- if (is.null(series)) available else unique(as.character(series))
  unknown <- setdiff(selected, available)
  if (length(unknown) > 0L) {
    stop(sprintf("Unknown series: %s.", paste(unknown, collapse = ", ")),
         call. = FALSE)
  }
  selected <- utils::head(selected, as.integer(max_series))
  columns <- if (is.null(columns)) ceiling(sqrt(length(selected))) else
    as.integer(columns)
  rows <- ceiling(length(selected) / columns)
  previous <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::layout(1L)
    graphics::par(previous)
    # Restoring `mfrow`/`mfcol` inside par(previous) resets `cex` to 1, so
    # put the caller's `cex` back last.
    graphics::par(cex = previous$cex)
  }, add = TRUE)
  panel_count <- rows * columns
  if (legend) {
    panel_layout <- matrix(seq_len(panel_count), nrow = rows, byrow = TRUE)
    graphics::layout(
      rbind(panel_layout, rep.int(panel_count + 1L, columns)),
      heights = c(rep.int(1, rows), 0.15)
    )
  } else {
    graphics::layout(matrix(seq_len(panel_count), nrow = rows, byrow = TRUE))
  }
  graphics::par(mar = c(3.1, 3.9, 2.2, 0.9), mgp = c(2.1, 0.7, 0))
  invisible(lapply(selected, function(series_id) {
    series_data <- source[source$id == series_id, , drop = FALSE]
    .tsn_panel(
      x = series_data$time, y = series_data$value,
      xlim = range(series_data$time),
      ylim = .tsn_expand_range(series_data$value),
      main = as.character(series_id), xlab = xlab, ylab = ylab,
      style = style
    )
    graphics::lines(series_data$time, series_data$value,
                    col = line_color, lwd = line_width)
    graphics::points(
      series_data$time, series_data$value,
      pch = 21, cex = point_size,
      bg = unname(palette[as.character(series_data$state)]),
      col = background, lwd = 0.7
    )
  }))
  unused <- panel_count - length(selected)
  if (unused > 0L) {
    invisible(lapply(seq_len(unused), function(index) graphics::plot.new()))
  }
  if (legend) {
    present <- sort(unique(as.character(
      source$state[source$id %in% selected]
    )))
    .tsn_legend_row(
      labels = present, colors = unname(palette[present]),
      title = "Trend", style = style, kind = "point"
    )
  }
  invisible(NULL)
}

#' Deterministic trend-state colours
#' @noRd
.tsn_trend_colors <- function(palette = NULL) {
  default <- c(
    Ascending = "#2E9E6F", Descending = "#D1495B", Flat = "#8D99AE",
    Turbulent = "#9656C9", `Missing Data` = "#D6DBDF", Initial = "#5C6B73"
  )
  if (is.null(palette)) {
    return(default)
  }
  if (!is.character(palette) || is.null(names(palette)) ||
        anyNA(names(palette)) || any(!nzchar(names(palette)))) {
    stop(paste0(
      "`palette` must be a character vector named by trend state, e.g. ",
      "c(Ascending = \"forestgreen\")."
    ), call. = FALSE)
  }
  unknown <- setdiff(names(palette), names(default))
  if (length(unknown) > 0L) {
    stop(sprintf(
      "Unknown trend state(s) in `palette`: %s. Valid names: %s.",
      paste(unknown, collapse = ", "),
      paste(names(default), collapse = ", ")
    ), call. = FALSE)
  }
  default[names(palette)] <- palette
  default
}

#' Draw series-plus-metric trend panels
#'
#' For each selected series, stacks the value series (points coloured by trend
#' state) above the rolling trend metric that produced the classification,
#' with a zero reference line. Reading the two together shows *why* each
#' window was classified as it was.
#'
#' @param source Data frame with `id`, `time`, `value`, `metric`, `state`.
#' @param series Optional series selection.
#' @param max_series Maximum number of series to draw.
#' @param palette Named trend-state colours.
#' @return `NULL`, invisibly.
#' @noRd
.tsn_plot_trend_panels <- function(source, series = NULL, max_series = 4L,
                                   palette, line_color = "#9AA3AD",
                                   line_width = 1.3, point_size = 1.05,
                                   flat_band = TRUE, epsilon = NULL,
                                   legend = TRUE, xlab = "Time",
                                   ylab = "Value", cex = 1, grid = TRUE,
                                   background = "#FFFFFF", ...) {
  stopifnot(is.logical(flat_band), length(flat_band) == 1L, !is.na(flat_band))
  style <- .tsn_style(cex = cex, grid = grid, background = background)
  available <- unique(source$id)
  selected <- if (is.null(series)) available else unique(as.character(series))
  unknown <- setdiff(selected, available)
  if (length(unknown) > 0L) {
    stop(sprintf("Unknown series: %s.", paste(unknown, collapse = ", ")),
         call. = FALSE)
  }
  selected <- utils::head(selected, as.integer(max_series))
  previous <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::layout(1L)
    graphics::par(previous)
    # Restoring `mfrow`/`mfcol` inside par(previous) resets `cex` to 1, so
    # put the caller's `cex` back last.
    graphics::par(cex = previous$cex)
  }, add = TRUE)
  panel_rows <- 2L * length(selected)
  layout_rows <- if (legend) panel_rows + 1L else panel_rows
  graphics::layout(
    matrix(seq_len(layout_rows), ncol = 1L),
    heights = c(rep.int(c(1, 0.55), length(selected)),
                if (legend) 0.16 else NULL)
  )
  graphics::par(mar = c(1.6, 3.9, 2.0, 0.9), mgp = c(2.1, 0.7, 0))
  invisible(lapply(selected, function(series_id) {
    series_data <- source[source$id == series_id, , drop = FALSE]
    colors <- unname(palette[as.character(series_data$state)])
    .tsn_panel(
      x = series_data$time, y = series_data$value,
      xlim = range(series_data$time),
      ylim = .tsn_expand_range(series_data$value),
      main = as.character(series_id), xlab = "", ylab = ylab,
      style = style
    )
    graphics::lines(series_data$time, series_data$value, col = line_color,
                    lwd = line_width)
    graphics::points(series_data$time, series_data$value, pch = 21,
                     cex = 0.85 * point_size, bg = colors, col = background,
                     lwd = 0.6)
    finite_metric <- series_data$metric[is.finite(series_data$metric)]
    metric_limits <- if (length(finite_metric) > 0L) {
      range(c(finite_metric, 0))
    } else {
      c(-1, 1)
    }
    graphics::par(mar = c(3.2, 3.9, 0.6, 0.9))
    .tsn_panel(
      x = series_data$time, y = series_data$metric,
      xlim = range(series_data$time),
      ylim = .tsn_expand_range(metric_limits),
      main = "", xlab = xlab, ylab = "Metric", style = style
    )
    if (flat_band && is.numeric(epsilon) && length(epsilon) == 1L &&
          is.finite(epsilon) && epsilon > 0) {
      limits <- graphics::par("usr")
      graphics::rect(
        limits[1L], -epsilon, limits[2L], epsilon,
        col = grDevices::adjustcolor("#8D99AE", alpha.f = 0.16),
        border = NA
      )
    }
    graphics::abline(h = 0, col = style$axis_color, lty = 2L, lwd = 0.9)
    graphics::lines(series_data$time, series_data$metric, col = line_color,
                    lwd = line_width)
    graphics::points(series_data$time, series_data$metric, pch = 21,
                     cex = 0.7 * point_size, bg = colors, col = background,
                     lwd = 0.6)
    graphics::par(mar = c(1.6, 3.9, 2.0, 0.9))
  }))
  if (legend) {
    present <- sort(unique(as.character(
      source$state[source$id %in% selected]
    )))
    .tsn_legend_row(
      labels = present, colors = unname(palette[present]),
      title = "Trend", style = style, kind = "point"
    )
  }
  invisible(NULL)
}
