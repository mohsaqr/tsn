#' Discretize a Time Series into States
#'
#' `discretize()` exposes every internal state discretizer as a first-class
#' verb. It accepts the same inputs as [tsn()] (plus a `tsn` object) and returns
#' a tidy one-row-per-observation table of discretized states. `tsn()` routes
#' its own state aggregation through the same engine, so
#' `tsn(x, "<method>")` and `discretize(x, method = "<method>")` produce
#' identical state assignments. Scalar methods learn one shared state space
#' from all selected values. Temporal methods (`"ordinal"`,
#' `"adaptive_magnitude"`, and `"dtw"`) first compute patterns or windows
#' within each series, never across an ID boundary, and then use a shared
#' alphabet or clustering model.
#'
#' @param data A numeric vector, `ts`, matrix, named list of numeric vectors,
#'   data frame, or a `tsn` object (its source series are used).
#' @param value Optional value-column name for long data.
#' @param id Optional series-ID column name for long data.
#' @param time Optional time-column name for long data.
#' @param series Optional series IDs or wide-data column names to select.
#' @param method Discretization method. One of `"threshold"`, `"width"`,
#'   `"quantile"`, `"kde"`, `"kmeans"`, `"gaussian"`, `"hclust"`, `"ordinal"`,
#'   `"symbolic"`, `"change_points"`, `"entropy"`, `"magnitude"`,
#'   `"adaptive_magnitude"`, `"percentile_magnitude"`, or `"dtw"`.
#' @param n_states Number of states. Not consumed by `"ordinal"`, whose state
#'   count follows the embedding arguments `m` and `tau`; supplying both is an
#'   error.
#' @param breaks Optional interior thresholds for `method = "threshold"`. An
#'   error with any other method, which computes its own boundaries.
#' @param labels Optional custom state labels. Length must equal the number of
#'   states produced. When `NULL`, states are numbered consecutively.
#' @param transform Pre-discretization transform: `"none"` (default), `"log"`
#'   (uses `log1p(abs(x))`), or `"zscore"` (standardized values).
#' @param m Embedding dimension for `method = "ordinal"` (default `3`).
#' @param tau Embedding lag for `method = "ordinal"` (default `1`).
#' @param seed Optional seed for the stochastic discretizers (`"kmeans"` and
#'   `"gaussian"`). An error with the deterministic methods.
#' @return A tidy data frame of class `tsn_states` with columns `id`, `time`,
#'   `value`, `state` (a factor), and `probability`. The discretization model
#'   and any bin boundaries are stored in the `model` and `breaks` attributes.
#' @references
#' Bandt, C., & Pompe, B. (2002). Permutation entropy: A natural complexity
#' measure for time series. *Physical Review Letters*, 88, 174102.
#' \doi{10.1103/PhysRevLett.88.174102}
#' @examples
#' discretize(c(1, 5, 2, 9, 3, 7, 4, 8), method = "quantile", n_states = 3)
#'
#' discretize(
#'   c(1, 5, 2, 9, 3, 7, 4, 8),
#'   method = "quantile",
#'   n_states = 3,
#'   labels = c("low", "mid", "high")
#' )
#'
#' discretize(c(3, 1, 4, 1, 5, 9, 2, 6), method = "ordinal", m = 3)
#'
#' discretize(c(10, 12, 8, 40, 44, 9), method = "width", transform = "log")
#' @export
discretize <- function(data, value = NULL, id = NULL, time = NULL,
                       series = NULL, method = "quantile", n_states = 3L,
                       breaks = NULL, labels = NULL, transform = "none",
                       m = NULL, tau = NULL, seed = NULL) {
  method <- match.arg(method, c(
    "threshold", "width", "quantile", "kde", "kmeans", "gaussian", "hclust",
    "ordinal", "symbolic", "change_points", "entropy", "magnitude",
    "adaptive_magnitude", "percentile_magnitude", "dtw"
  ))
  transform <- match.arg(transform, c("none", "log", "zscore"))
  # Options a method cannot consume are rejected rather than silently
  # discarded: a typo or misunderstood configuration must not produce a
  # plausible but unintended discretization.
  if (!is.null(breaks) && !identical(method, "threshold")) {
    stop(sprintf(
      paste0(
        "`breaks` cannot be used with `method = \"%s\"`; only the ",
        "\"threshold\" discretizer takes explicit boundaries."
      ), method
    ), call. = FALSE)
  }
  if (!missing(n_states) && identical(method, "ordinal")) {
    stop(
      "`n_states` cannot be used with `method = \"ordinal\"`, whose state ",
      "count follows `m` and `tau`.",
      call. = FALSE
    )
  }
  if (!is.null(seed) && !method %in% c("kmeans", "gaussian")) {
    stop(sprintf(
      paste0(
        "`seed` cannot be used with `method = \"%s\"`; only the stochastic ",
        "\"kmeans\" and \"gaussian\" discretizers are seeded."
      ), method
    ), call. = FALSE)
  }

  source <- if (inherits(data, "tsn")) {
    .tsn_select_canonical(
      data$source[c("id", "time", "value")],
      series = series
    )
  } else {
    .tsn_normalize_input(
      .tsn_select_data(data, series = series, value = value, id = id),
      value = value, id = id, time = time
    )
  }

  fit <- .tsn_discretize_values(
    values = source$value, method = method, n_states = n_states,
    breaks = breaks, transform = transform, m = m, tau = tau, seed = seed,
    groups = source$id
  )
  observed_states <- max(fit$state)
  state_labels <- if (is.null(labels)) {
    as.character(seq_len(observed_states))
  } else {
    labels <- as.character(labels)
    if (length(labels) != observed_states) {
      stop(sprintf(
        "`labels` must provide exactly %d value(s) (the number of states).",
        observed_states
      ), call. = FALSE)
    }
    labels
  }
  out <- data.frame(
    id = source$id,
    time = source$time,
    value = source$value,
    state = factor(state_labels[fit$state], levels = state_labels),
    probability = fit$probability,
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  structure(
    out,
    class = c("tsn_states", "data.frame"),
    model = fit$model,
    breaks = fit$breaks,
    parameters = list(method = method, transform = transform,
                      n_states = observed_states)
  )
}

#' Shared discretization path for `tsn()` and `discretize()`
#'
#' Applies the optional transform and delegates to the internal engine so both
#' public entry points produce identical integer state assignments.
#'
#' @param values Finite numeric vector (untransformed).
#' @param method Discretization method.
#' @param n_states Number of states.
#' @param breaks Optional thresholds for `method = "threshold"`.
#' @param transform Pre-discretization transform.
#' @param m,tau Ordinal embedding arguments.
#' @param seed Optional stochastic seed.
#' @param groups Optional series identifiers. Temporal discretizers are fitted
#'   without allowing ordinal, rolling, or DTW windows to cross group
#'   boundaries, while retaining a shared state alphabet.
#' @return A list with integer `state`, numeric `probability`, and the `model`
#'   and `breaks` attributes copied from the engine.
#' @noRd
.tsn_discretize_values <- function(values, method, n_states, breaks,
                                   transform = "none", m = NULL, tau = NULL,
                                   seed = NULL, groups = NULL) {
  stopifnot(
    is.numeric(values), length(values) > 0L, all(is.finite(values))
  )
  if (!is.null(groups)) {
    stopifnot(
      is.atomic(groups), is.null(dim(groups)),
      length(groups) == length(values), !anyNA(groups)
    )
  }
  transformed <- switch(
    transform,
    none = values,
    log = log1p(abs(values)),
    zscore = {
      spread <- stats::sd(values)
      if (!is.finite(spread) || spread <= 0) {
        stop("`transform = \"zscore\"` requires a non-constant series.",
             call. = FALSE)
      }
      (values - mean(values)) / spread
    }
  )
  temporal_methods <- c("ordinal", "adaptive_magnitude", "dtw")
  fit <- if (!is.null(groups) && length(unique(groups)) > 1L &&
               method %in% temporal_methods) {
    .tsn_discretize_grouped(
      transformed,
      groups = groups,
      method = method,
      n_states = n_states,
      thresholds = breaks,
      m = m,
      tau = tau,
      seed = seed
    )
  } else {
    .tsn_discretize(
      transformed, method = method, n_states = n_states, thresholds = breaks,
      m = m, tau = tau, seed = seed
    )
  }
  list(
    state = as.integer(fit$state),
    probability = as.numeric(fit$probability),
    model = attr(fit, "model"),
    breaks = attr(fit, "breaks")
  )
}

#' Discretize Multiple Series Without Crossing Their Boundaries
#'
#' Temporal features are computed within each series. Ordinal patterns, local
#' z-scores, and DTW shape clusters are then reconciled into one shared state
#' alphabet so series remain comparable.
#'
#' @param values Finite numeric values in canonical row order.
#' @param groups Series identifier for each value.
#' @param method One of `"ordinal"`, `"adaptive_magnitude"`, or `"dtw"`.
#' @param n_states Requested number of states.
#' @param thresholds Optional thresholds (unused by temporal methods).
#' @param m,tau Ordinal embedding arguments.
#' @param seed Optional stochastic seed.
#' @return A discretization data frame with `model` and optional `breaks`
#'   attributes, matching `.tsn_discretize()`.
#' @noRd
.tsn_discretize_grouped <- function(values, groups, method, n_states,
                                    thresholds = NULL, m = NULL, tau = NULL,
                                    seed = NULL) {
  stopifnot(
    is.numeric(values), length(values) > 0L, all(is.finite(values)),
    is.atomic(groups), is.null(dim(groups)),
    length(groups) == length(values), !anyNA(groups),
    is.character(method), length(method) == 1L, !is.na(method),
    method %in% c("ordinal", "adaptive_magnitude", "dtw"),
    is.null(thresholds)
  )
  group_text <- as.character(groups)
  group_levels <- unique(group_text)
  indices <- split(
    seq_along(values),
    factor(group_text, levels = group_levels),
    drop = TRUE
  )
  switch(
    method,
    ordinal = .tsn_discretize_ordinal_grouped(
      values, indices = indices, m = m, tau = tau
    ),
    adaptive_magnitude = .tsn_discretize_adaptive_grouped(
      values, indices = indices, n_states = n_states
    ),
    dtw = .tsn_discretize_dtw_grouped(
      values, indices = indices, n_states = n_states
    )
  )
}

#' Group-Safe Ordinal Discretization
#' @noRd
.tsn_discretize_ordinal_grouped <- function(values, indices, m, tau) {
  stopifnot(
    is.numeric(values), all(is.finite(values)),
    is.list(indices), length(indices) > 1L
  )
  fits <- lapply(
    indices,
    function(index) .tsn_discretize_ordinal(values[index], m = m, tau = tau)
  )
  per_group_patterns <- lapply(
    fits,
    function(fit) {
      model <- fit$model
      model$patterns[fit$state]
    }
  )
  pattern_at_point <- character(length(values))
  pattern_at_point[unlist(indices, use.names = FALSE)] <-
    unlist(per_group_patterns, use.names = FALSE)
  pattern_levels <- sort(unique(pattern_at_point))
  state <- match(pattern_at_point, pattern_levels)
  state_means <- as.numeric(tapply(values, state, mean))
  models <- lapply(fits, `[[`, "model")
  result <- data.frame(
    index = seq_along(values),
    state = as.integer(state),
    probability = rep.int(1, length(values)),
    stringsAsFactors = FALSE
  )
  attr(result, "model") <- list(
    method = "ordinal",
    n_states = length(pattern_levels),
    state_means = state_means,
    m = models[[1L]]$m,
    tau = models[[1L]]$tau,
    patterns = pattern_levels,
    n_patterns = length(pattern_levels),
    n_windows = sum(vapply(models, `[[`, integer(1L), "n_windows")),
    trailing_filled = sum(vapply(
      models, `[[`, integer(1L), "trailing_filled"
    )),
    per_series = models
  )
  result
}

#' Group-Safe Adaptive-Magnitude Discretization
#' @noRd
.tsn_discretize_adaptive_grouped <- function(values, indices, n_states) {
  stopifnot(
    is.numeric(values), all(is.finite(values)),
    is.list(indices), length(indices) > 1L
  )
  .tsn_check_count(n_states, "n_states", lower = 2L)
  n_states <- as.integer(n_states)
  windows <- vapply(
    indices,
    function(index) max(2L, min(10L, length(index))),
    integer(1L)
  )
  z_by_group <- Map(
    function(index, window) {
      .tsn_rolling_zscore(values[index], window = window)
    },
    indices,
    windows
  )
  z_scores <- numeric(length(values))
  z_scores[unlist(indices, use.names = FALSE)] <-
    unlist(z_by_group, use.names = FALSE)
  fit <- .tsn_cut_fit(
    z_scores,
    .tsn_quantile_breaks(z_scores, n_states),
    "adaptive_magnitude"
  )
  ordered <- .tsn_order_states(values, fit$state)
  result <- data.frame(
    index = seq_along(values),
    state = ordered$state,
    probability = as.numeric(fit$probability),
    stringsAsFactors = FALSE
  )
  attr(result, "model") <- c(
    list(
      method = "adaptive_magnitude",
      n_states = length(ordered$means),
      state_means = ordered$means
    ),
    fit$model,
    list(z_scores = z_scores, window = windows)
  )
  attr(result, "breaks") <- fit$breaks
  result
}

#' Group-Safe DTW Shape Discretization
#' @noRd
.tsn_discretize_dtw_grouped <- function(values, indices, n_states) {
  stopifnot(
    is.numeric(values), all(is.finite(values)),
    is.list(indices), length(indices) > 1L
  )
  .tsn_check_count(n_states, "n_states", lower = 2L)
  n_states <- as.integer(n_states)
  lengths <- vapply(indices, length, integer(1L))
  if (any(lengths < 2L)) {
    stop("`dtw` discretization needs at least two observations per series.",
         call. = FALSE)
  }
  window <- max(2L, min(lengths) %/% 10L)
  window_counts <- lengths - window + 1L
  if (sum(window_counts) < n_states) {
    stop(sprintf(
      "`dtw` discretization needs at least %d within-series windows.",
      n_states
    ), call. = FALSE)
  }
  windows_by_group <- lapply(
    indices,
    function(index) {
      starts <- seq_len(length(index) - window + 1L)
      lapply(
        starts,
        function(start) values[index[seq.int(start, start + window - 1L)]]
      )
    }
  )
  windows <- unlist(windows_by_group, recursive = FALSE, use.names = FALSE)
  clustering <- .tsn_pam(.tsn_dtw_matrix(windows), n_states)
  cluster_by_group <- split(
    clustering$cluster,
    factor(
      rep(names(indices), times = window_counts),
      levels = names(indices)
    ),
    drop = TRUE
  )
  votes <- Map(
    function(cluster, n) .tsn_window_vote(cluster, window = window, n = n),
    cluster_by_group,
    lengths
  )
  raw_state <- integer(length(values))
  probability <- numeric(length(values))
  raw_state[unlist(indices, use.names = FALSE)] <-
    unlist(lapply(votes, `[[`, "state"), use.names = FALSE)
  probability[unlist(indices, use.names = FALSE)] <-
    unlist(lapply(votes, `[[`, "confidence"), use.names = FALSE)
  ordered <- .tsn_order_states(values, raw_state)
  cluster_order <- match(seq_len(n_states), unique(raw_state[order(ordered$state)]))
  result <- data.frame(
    index = seq_along(values),
    state = ordered$state,
    probability = probability,
    stringsAsFactors = FALSE
  )
  attr(result, "model") <- list(
    method = "dtw",
    n_states = length(ordered$means),
    state_means = ordered$means,
    window = window,
    n_windows = length(windows),
    medoids = clustering$medoids,
    window_cluster = clustering$cluster,
    window_series = rep(names(indices), times = window_counts),
    cluster_order = cluster_order
  )
  result
}

#' Print a state discretization
#'
#' @param x A `tsn_states` result.
#' @param ... Passed to `print.data.frame()`.
#' @return `x`, invisibly.
#' @export
print.tsn_states <- function(x, ...) {
  parameters <- attr(x, "parameters")
  cat(sprintf(
    "<tsn_states> %s discretization (transform: %s): %d observations, %d states\n",
    parameters$method, parameters$transform, nrow(x), parameters$n_states
  ))
  print.data.frame(utils::head(as.data.frame(x), 10L), row.names = FALSE, ...)
  invisible(x)
}

#' Summarize a state discretization
#'
#' @param object A `tsn_states` result.
#' @param ... Ignored.
#' @return A tidy data frame with one row per state giving the `count`,
#'   `proportion`, and mean `value` of observations in that state.
#' @export
summary.tsn_states <- function(object, ...) {
  stopifnot(inherits(object, "tsn_states"))
  states <- factor(object$state, levels = levels(object$state))
  counts <- table(states)
  present <- names(counts)[counts > 0]
  means <- tapply(object$value, states, mean)
  data.frame(
    state = present,
    count = as.integer(counts[present]),
    proportion = as.numeric(counts[present]) / sum(counts),
    mean_value = as.numeric(means[present]),
    stringsAsFactors = FALSE
  )
}

#' Plot a state discretization
#'
#' Four views of a discretized series, selected with `type`. `"overlay"`
#' (default) draws each series with translucent state shading behind it;
#' `"ribbon"` keeps the series clean and runs a state-classification strip
#' underneath it instead; `"heatmap"` draws one row per series with one
#' coloured tile per observation, which scales to many series where the
#' faceted views do not; `"stack"` places an aligned companion series above
#' the discretized series.
#'
#' @param x A `tsn_states` result.
#' @param type The view: `"overlay"`, `"ribbon"`, `"heatmap"`, or `"stack"`.
#'   The stack view draws a companion series (`with`) above the discretized
#'   series with BOTH panels vertically shaded by the discretized states —
#'   e.g. the original time series read against the states of its rolling
#'   complexity.
#' @param series Optional character vector of series IDs to display.
#' @param overlay Colour shading for the overlay view: `"horizontal"` (default;
#'   value bands), `"vertical"` (time runs), or `"none"`.
#' @param lines Dashed guide lines complementing the shading:
#'   `"horizontal"` draws grey lines at the state boundaries (the
#'   discretization breaks when available, empirical boundaries otherwise).
#'   Transformed and magnitude-space breaks are converted back to the raw
#'   signed value axis; adaptive local boundaries are omitted.
#'   `"vertical"` draws a line at each state transition (filtered by
#'   `min_run`) **in the colour of the state that starts there** — a green
#'   dashed line means the green state begins at that point and runs until
#'   the next line; `"both"`, or `"none"` (default). Combine one shading
#'   direction with dashed lines in the other, e.g.
#'   `overlay = "horizontal", lines = "vertical"`.
#' @param min_run For `lines = "vertical"`: only transitions into a state
#'   persisting at least `min_run` observations get a line.
#' @param points Whether to draw observations over each line.
#' @param with For `type = "stack"`: a finite numeric vector aligned with the
#'   rows of `x` giving the companion series (e.g. the original values when
#'   `x` discretizes a derived series).
#' @param with_label Y-axis title of the companion panel.
#' @param shade For `type = "stack"`: whether to draw the vertical state
#'   shading on both panels.
#' @param color_line For `type = "stack"`: colour each line segment by the
#'   state at its left endpoint instead of drawing a neutral line.
#' @param ribbons For `type = "ribbon"`: an optional NAMED list of additional
#'   classifications to stack below the series — each element a
#'   `tsn_states`, a [trend()] result, or a character/factor vector aligned
#'   with the selected series. The object's own states form the first strip
#'   (labelled `ribbon_label`); one series at a time. This is the
#'   resilience-style multi-ribbon view: the series read against several
#'   classifications at once, each strip labelled on the axis with its own
#'   legend row.
#' @param ribbon_label Strip label for the object's own states in the
#'   multi-ribbon view.
#' @param max_series Maximum number of series to draw (`25` for the heatmap,
#'   `10` otherwise).
#' @param columns Number of panel columns.
#' @param palette State colours: `NULL` for the default palette, a preset name
#'   (`"default"`, `"okabe"`, `"viridis"`, `"cool"`, `"warm"`, `"pastel"`,
#'   `"dark"`), a named colour vector keyed by state, or a plain colour
#'   vector.
#' @param alpha Shading opacity for the overlay view.
#' @param line_color Series line colour.
#' @param line_width Series line width.
#' @param point_size Observation point size.
#' @param strip_height Height of the ribbon strip as a fraction of the panel.
#' @param sort Heatmap row order: `"none"`, `"mean"` (by series mean value),
#'   or `"state"` (group rows by modal state).
#' @param border Whether heatmap tiles carry a thin separator border.
#' @param legend Whether to draw the legend row.
#' @param xlab,ylab Axis titles.
#' @param cex Global text size multiplier.
#' @param grid Whether to draw the background grid.
#' @param background Panel background colour.
#' @param ... Reserved for future options.
#' @return `x`, invisibly.
#' @examples
#' data(steps)
#' complete <- subset(steps, !is.na(steps))
#' states <- discretize(
#'   complete,
#'   value = "steps", id = "id", time = "day",
#'   method = "quantile", n_states = 3
#' )
#' plot(states, series = "536")
#' plot(states, series = "536", overlay = "vertical", lines = "horizontal")
#' plot(states, "ribbon", series = "536", points = FALSE)
#' plot(states, "heatmap", max_series = 12, sort = "mean")
#' plot(states, "heatmap", palette = "viridis", max_series = 12)
#'
#' # Stack: original series read against the states of its rolling volatility.
#' one <- subset(complete, id == 536)
#' volatility <- abs(c(0, diff(one$steps)))
#' vol_states <- discretize(
#'   data.frame(id = one$id, day = one$day, vol = volatility),
#'   value = "vol", id = "id", time = "day",
#'   method = "quantile", n_states = 3
#' )
#' plot(vol_states, "stack", with = one$steps, with_label = "Steps",
#'      ylab = "Volatility")
#' @export
plot.tsn_states <- function(x, type = c("overlay", "ribbon", "heatmap",
                                        "stack"),
                            series = NULL,
                            overlay = c("horizontal", "vertical", "none"),
                            lines = "none", min_run = 1L,
                            points = TRUE, max_series = NULL, columns = NULL,
                            palette = NULL, alpha = NULL,
                            line_color = "#3B4252", line_width = 1.6,
                            point_size = 1, strip_height = 0.1,
                            sort = "none", border = FALSE,
                            with = NULL, with_label = "Series",
                            shade = TRUE, color_line = FALSE,
                            ribbons = NULL, ribbon_label = "States",
                            legend = TRUE, xlab = "Time", ylab = "Value",
                            cex = 1, grid = TRUE, background = "#FFFFFF",
                            ...) {
  stopifnot(inherits(x, "tsn_states"))
  type <- match.arg(type)
  overlay <- match.arg(overlay)
  if (type == "stack" && is.null(with)) {
    stop("`type = \"stack\"` requires `with`: a numeric companion series ",
         "aligned with the rows of `x`.", call. = FALSE)
  }
  if (type != "stack" && !is.null(with)) {
    stop("`with` is only valid with `type = \"stack\"`.", call. = FALSE)
  }
  if (!is.null(ribbons) && type != "ribbon") {
    stop("`ribbons` is only valid with `type = \"ribbon\"`.", call. = FALSE)
  }
  source <- data.frame(
    id = as.character(x$id), time = x$time, value = x$value,
    state = factor(as.character(x$state), levels = levels(x$state)),
    stringsAsFactors = FALSE
  )
  if (is.null(max_series)) {
    max_series <- switch(type, heatmap = 25L, stack = 3L, 10L)
  }
  if (type == "ribbon" && !is.null(ribbons)) {
    stopifnot(is.list(ribbons), !is.null(names(ribbons)),
              all(nzchar(names(ribbons))))
    available <- unique(source$id)
    selected <- if (is.null(series)) available else
      unique(as.character(series))
    unknown <- setdiff(selected, available)
    if (length(unknown) > 0L) {
      stop(sprintf("Unknown series: %s.", paste(unknown, collapse = ", ")),
           call. = FALSE)
    }
    if (length(selected) != 1L) {
      stop("The multi-ribbon view draws one series at a time; select one ",
           "with `series`.", call. = FALSE)
    }
    one <- source[source$id == selected, , drop = FALSE]
    resolved <- c(
      stats::setNames(
        list(list(
          states = one$state,
          colors = .tsn_state_colors(one$state, palette = palette)
        )),
        ribbon_label
      ),
      stats::setNames(
        lapply(seq_along(ribbons), function(index) {
          .tsn_resolve_ribbon(
            ribbons[[index]], series_id = selected,
            n_expected = nrow(one), name = names(ribbons)[index]
          )
        }),
        names(ribbons)
      )
    )
    .tsn_plot_ribbon_stack(
      source = one, ribbons = resolved,
      strip_height = min(strip_height, 0.08), points = points,
      line_color = line_color, line_width = line_width,
      point_size = point_size, legend = legend,
      main = selected, xlab = xlab, ylab = ylab,
      cex = cex, grid = grid, background = background, ...
    )
    return(invisible(x))
  }
  switch(
    type,
    overlay = .tsn_plot_series_frame(
      source = source, series = series, overlay = overlay, points = points,
      trend = FALSE, columns = columns, max_series = max_series,
      scales = "free", palette = palette,
      alpha = if (is.null(alpha)) 0.28 else alpha,
      line_color = line_color, line_width = line_width,
      point_size = point_size, legend = legend,
      lines = lines, min_run = min_run, breaks = .tsn_plot_breaks(x),
      xlab = xlab, ylab = ylab,
      cex = cex, grid = grid, background = background, ...
    ),
    ribbon = .tsn_plot_series_frame(
      source = source, series = series, overlay = "none", points = points,
      trend = FALSE, columns = columns, max_series = max_series,
      scales = "free", palette = palette, strip = TRUE,
      line_color = line_color, line_width = line_width,
      point_size = point_size, strip_height = strip_height,
      legend = legend, xlab = xlab, ylab = ylab, cex = cex, grid = grid,
      background = background, ...
    ),
    heatmap = .tsn_plot_state_heatmap(
      source = source, series = series, max_series = max_series,
      palette = palette, sort = sort, border = border, legend = legend,
      xlab = xlab, cex = cex, background = background, ...
    ),
    stack = .tsn_plot_state_stack(
      source = source, with = with, series = series,
      max_series = max_series, palette = palette, shade = shade,
      alpha = if (is.null(alpha)) 0.28 else alpha, points = points,
      color_line = color_line, line_color = line_color,
      line_width = line_width, point_size = point_size, legend = legend,
      with_label = with_label, ylab = ylab, xlab = xlab,
      cex = cex, grid = grid, background = background, ...
    )
  )
  invisible(x)
}

#' Convert Discretization Breaks to the Raw Plot Axis
#'
#' Stored breaks live on the transformed scale used by the discretizer.
#' Convert z-score boundaries back to the raw scale and mirror any
#' absolute-magnitude boundaries around zero (or around the z-score mean).
#' Adaptive-magnitude states have time-varying local boundaries and therefore
#' have no valid global raw-axis guides.
#'
#' @param x A `tsn_states` result.
#' @return Numeric raw-axis guide positions.
#' @noRd
.tsn_plot_breaks <- function(x) {
  stopifnot(inherits(x, "tsn_states"))
  parameters <- attr(x, "parameters")
  method <- parameters$method
  transform <- parameters$transform
  if (identical(method, "adaptive_magnitude")) {
    return(numeric())
  }
  breaks <- attr(x, "breaks")
  finite <- if (is.numeric(breaks)) breaks[is.finite(breaks)] else numeric()
  if (length(finite) == 0L) {
    return(numeric())
  }
  magnitude_space <- method %in% c("magnitude", "percentile_magnitude") ||
    identical(transform, "log")
  if (identical(transform, "zscore")) {
    center <- mean(x$value)
    spread <- stats::sd(x$value)
    stopifnot(is.finite(center), is.finite(spread), spread > 0)
    guides <- if (magnitude_space) {
      center + c(-finite * spread, finite * spread)
    } else {
      center + finite * spread
    }
  } else if (identical(transform, "log")) {
    raw_magnitude <- expm1(finite)
    guides <- c(-raw_magnitude, raw_magnitude)
  } else if (magnitude_space) {
    guides <- c(-finite, finite)
  } else {
    guides <- finite
  }
  sort(unique(as.numeric(guides)))
}
