#' Build a Time-Series Network
#'
#' `tsn()` is the core entry point for constructing networks from time-series
#' geometry. It builds distance networks between complete series or sliding
#' windows, and natural or horizontal visibility networks between time points
#' or discretized states.
#'
#' @param data A numeric vector, `ts`, numeric matrix, named list of numeric
#'   vectors, or data frame.
#' @param value Optional value-column name for long data.
#' @param id Optional series-ID column name for long data.
#' @param time Optional time-column name for long data.
#' @param series Optional series IDs for long data or numeric column names for
#'   wide data. Selection happens inside `tsn()`.
#' @param method Network selector. The base families are `"distance"` and
#'   `"visibility"`. Convenience shortcuts resolve to a family with sensible
#'   defaults: `"nvg"`/`"natural"` (natural visibility graph), `"hvg"`/
#'   `"horizontal"` (horizontal visibility graph), and any discretizer name
#'   (`"ordinal"`, `"quantile"`, `"symbolic"`, ...) which builds a visibility
#'   network on states from that discretizer. Shortcuts only set defaults; the
#'   granular arguments (`unit`, `visibility`, `discretization`) still apply.
#' @param unit Node unit. Distance networks use `"series"`, `"window"`, or
#'   `"time"` (one node per time point); visibility networks use `"time"` or
#'   `"state"`. When `NULL`, the natural unit for the selected method is
#'   inferred.
#' @param distance Distance measure for distance networks. One of
#'   `"euclidean"`, `"manhattan"`, `"maximum"`, `"canberra"`, `"minkowski"`,
#'   `"binary"`, `"cosine"`, `"correlation"`, `"spearman"`, `"dtw"`, `"ccf"`
#'   (one minus the maximum absolute cross-correlation across lags), `"nmi"`
#'   (one minus normalized mutual information after separately quantile-binning
#'   each series), `"voi"` (variation of information after the same marginal
#'   binning),
#'   `"event_sync"` (one minus the Quiroga event-synchronization index; units
#'   are interpreted as event times), or `"van_rossum"` (exact van Rossum
#'   spike-train distance; units are interpreted as event times).
#' @param connect Distance-to-network rule.
#' @param window Sliding-window width when `unit = "window"`.
#' @param step Sliding-window step.
#' @param neighbors Number of neighbours when `connect = "nearest"`.
#' @param threshold Maximum distance when `connect = "threshold"`.
#' @param percentile Proportion of shortest distances retained when
#'   `connect = "percentile"`.
#' @param bandwidth Positive kernel scale. Used by `connect = "gaussian"` and
#'   by the `"negative_exp"` and `"gaussian"` similarity kernels; defaults to
#'   the median positive distance.
#' @param p Minkowski power.
#' @param bins Number of marginal quantile bins for `distance = "nmi"` and
#'   `distance = "voi"`.
#' @param lag Maximum cross-correlation lag for `distance = "ccf"`. `NULL`
#'   uses the `stats::ccf()` default.
#' @param tolerance Event-time scale for the event-based distances: an optional
#'   upper bound on the adaptive coincidence window for
#'   `distance = "event_sync"`, or the kernel time constant for
#'   `distance = "van_rossum"`. For event synchronization, `NULL` uses the
#'   uncapped adaptive local window. For van Rossum, `NULL` uses the median
#'   positive inter-event interval of each pooled pair.
#' @param similarity Optional similarity kernel mapping edge distances to
#'   weights: `"inverse"` (the default weight rule, `1 / (1 + d)`),
#'   `"normalized_inverse"` (`1 - d / max(d)`), `"negative_exp"`
#'   (`exp(-d / bandwidth)`), or `"gaussian"`
#'   (`exp(-d^2 / (2 * bandwidth^2))`).
#' @param visibility Visibility rule: `"natural"` or `"horizontal"`.
#' @param state Optional state-column name or state vector.
#' @param discretization Internal state-discretization method. One of
#'   `"threshold"`, `"width"`, `"quantile"`, `"kde"`, `"kmeans"`, `"gaussian"`,
#'   `"hclust"`, `"ordinal"`, `"symbolic"`, `"change_points"`, `"entropy"`,
#'   `"magnitude"`, `"adaptive_magnitude"`, `"percentile_magnitude"`, or
#'   `"dtw"`.
#' @param n_states Number of states. Ignored by `discretization = "ordinal"`,
#'   whose state count follows the embedding arguments `m` and `tau`.
#' @param breaks Optional internal thresholds when `discretization = "threshold"`.
#' @param m Embedding dimension for `discretization = "ordinal"` (default `3`).
#'   Only valid with the ordinal discretizer.
#' @param tau Embedding lag for `discretization = "ordinal"` (default `1`). Only
#'   valid with the ordinal discretizer.
#' @param directed Whether edges follow their ordered direction. Visibility
#'   edges then run forward in time; distance edges follow the evaluated unit
#'   order. With `FALSE`, reciprocal distance relations are represented by one
#'   undirected edge.
#' @param limit Optional maximum visibility distance in the units of `time`
#'   (or observation steps when no numeric/date-time axis is supplied).
#' @param penetrable Number of intermediate points allowed to block visibility.
#' @param decay Non-negative exponential edge-decay rate per unit of `time`
#'   (or per observation step when no numeric/date-time axis is supplied).
#' @param aggregation State-edge aggregation rule.
#' @param chain When `TRUE`, distance networks connect only consecutive series
#'   or windows (a transition chain) instead of all pairs.
#' @param normalize Distance rescaling applied before the connection rule.
#'   `FALSE` (default) leaves distances unchanged; `TRUE` or `"max"` divides by
#'   the maximum distance; `"minmax"` rescales to `[0, 1]`; `"quantile"`
#'   rescales by the 5th-95th percentile range, clamped to `[0, 1]`.
#' @param seed Optional seed used by stochastic discretizers.
#' @return A list-backed network object of class
#'   `c("tsn", "netobject", "cograph_network")`. Its `$table` component is the
#'   tidy dyad table; use `as.data.frame()` for that table,
#'   `as.data.frame(x, what = "series")` for the canonical source observations,
#'   and `as.matrix()` for the weighted adjacency matrix.
#' @references
#' Lacasa, L., Luque, B., Ballesteros, F., Luque, J., & Nuño, J. C. (2008).
#' From time series to complex networks: The visibility graph. *Proceedings of
#' the National Academy of Sciences*, 105(13), 4972-4975.
#' \doi{10.1073/pnas.0709247105}
#'
#' Luque, B., Lacasa, L., Ballesteros, F., & Luque, J. (2009). Horizontal
#' visibility graphs: Exact results for random time series. *Physical Review
#' E*, 80, 046103. \doi{10.1103/PhysRevE.80.046103}
#'
#' Quian Quiroga, R., Kreuz, T., & Grassberger, P. (2002). Event
#' synchronization: A simple and fast method to measure synchronicity and time
#' delay patterns. *Physical Review E*, 66, 041904.
#' \doi{10.1103/PhysRevE.66.041904}
#' @examples
#' series <- list(
#'   first = c(1, 2, 3, 2, 1),
#'   second = c(1, 1, 2, 3, 5),
#'   third = c(5, 4, 3, 2, 1)
#' )
#'
#' tsn(
#'   data = series,
#'   method = "distance",
#'   unit = "series",
#'   distance = "euclidean",
#'   connect = "full"
#' )
#'
#' # One data argument, one method string.
#' tsn(c(3, 1, 4, 2, 5), "hvg")
#' tsn(c(3, 1, 4, 2, 5, 3, 6, 2, 7), "ordinal")
#' tsn(c(3, 1, 4, 2, 5, 3, 6, 2, 7), "distance")
#'
#' data(steps)
#' tsn(
#'   steps,
#'   value = "steps",
#'   id = "id",
#'   time = "day",
#'   series = 536,
#'   unit = "state",
#'   discretization = "gaussian"
#' )
#' @export
tsn <- function(data, method = "visibility", value = NULL, id = NULL,
                time = NULL, series = NULL, unit = NULL,
                distance = "euclidean", connect = "full",
                window = NULL, step = 1L, neighbors = NULL,
                threshold = NULL, percentile = NULL, bandwidth = NULL,
                p = 2, bins = NULL, lag = NULL, tolerance = NULL,
                similarity = NULL, visibility = "natural", state = NULL,
                discretization = "gaussian", n_states = 3L,
                breaks = NULL, m = NULL, tau = NULL,
                directed = FALSE, limit = NULL, penetrable = 0L,
                decay = 0, aggregation = "sum", chain = FALSE,
                normalize = FALSE, seed = NULL) {
  resolved <- .tsn_resolve_method(method, unit = unit, visibility = visibility,
                                  discretization = discretization)
  method <- resolved$method
  unit <- resolved$unit
  visibility <- resolved$visibility
  discretization <- resolved$discretization
  stopifnot(is.logical(chain), length(chain) == 1L, !is.na(chain))
  normalize <- .tsn_resolve_normalize(normalize)
  .tsn_validate_distance_options(
    method = method, distance = distance, bins = bins, lag = lag,
    tolerance = tolerance, similarity = similarity
  )
  selected_data <- .tsn_select_data(
    data,
    series = series,
    value = value,
    id = id
  )
  state_values <- .tsn_normalize_states(
    selected_data,
    state = state,
    id = id,
    time = time
  )
  source <- .tsn_normalize_input(
    selected_data,
    value = value,
    id = id,
    time = time
  )
  unit <- .tsn_resolve_unit(unit, method, source)
  if (method == "distance" && unit == "window" && is.null(window)) {
    window <- .tsn_default_window(source)
  }
  .tsn_validate_public_arguments(
    method = method, unit = unit, window = window, neighbors = neighbors,
    threshold = threshold, percentile = percentile, bandwidth = bandwidth,
    state = state_values, directed = directed, limit = limit,
    penetrable = penetrable, decay = decay,
    discretization = discretization, m = m, tau = tau
  )

  result <- if (method == "distance") {
    constructed <- switch(
      unit,
      series = .tsn_series_units(source),
      time = .tsn_point_units(source),
      window = .tsn_window_units(source, window = window, step = step)
    )
    if (length(constructed$units) < 2L) {
      stop("Distance networks require at least two series, windows, or points.",
           call. = FALSE)
    }
    dyads <- if (chain) {
      .tsn_chain_distance(
        constructed$units,
        method = distance, p = p,
        bins = if (is.null(bins)) 10L else bins,
        lag = lag, tolerance = tolerance,
        groups = if (unit == "window") constructed$metadata$series else NULL
      )
    } else {
      .tsn_pairwise_distance(
        constructed$units,
        method = distance, p = p,
        bins = if (is.null(bins)) 10L else bins,
        lag = lag, tolerance = tolerance
      )
    }
    dyads$distance <- .tsn_normalize_distances(dyads$distance, normalize)
    dyads <- .tsn_connect(
      dyads,
      method = connect,
      neighbors = neighbors,
      threshold = threshold,
      percentile = percentile,
      bandwidth = bandwidth,
      directed = directed,
      similarity = similarity
    )
    list(dyads = dyads, metadata = constructed$metadata,
         nodes = names(constructed$units), distance_method = distance,
         connection_method = connect)
  } else {
    .tsn_build_visibility(
      source = source,
      unit = unit,
      visibility = visibility,
      state = state_values,
      discretization = discretization,
      n_states = n_states,
      breaks = breaks,
      m = m,
      tau = tau,
      directed = directed,
      limit = limit,
      penetrable = penetrable,
      decay = decay,
      aggregation = aggregation,
      seed = seed
    )
  }

  edges <- .tsn_finalize_edges(
    result$dyads,
    metadata = result$metadata,
    method = method,
    unit = unit,
    distance_method = result$distance_method,
    connection_method = result$connection_method,
    directed = directed
  )
  parameters <- list(
    method = method,
    unit = unit,
    distance = result$distance_method,
    connection = result$connection_method,
    directed = directed,
    call = match.call()
  )
  output_source <- source
  if (!is.null(result$states)) {
    output_source$state <- result$states
  }
  .new_tsn(edges, source = output_source, nodes = result$nodes,
           parameters = parameters)
}

#' Resolve the `method` selector into a family and its defaults
#'
#' Shortcut method values (`"nvg"`, `"hvg"`, and every discretizer name) are
#' mapped to a base family (`"distance"` or `"visibility"`) plus the `unit`,
#' `visibility`, and `discretization` defaults they imply. Explicit granular
#' arguments are preserved.
#'
#' @param method The public `method` value.
#' @param unit,visibility,discretization Current granular arguments.
#' @return A list with resolved `method`, `unit`, `visibility`, and
#'   `discretization`.
#' @noRd
.tsn_resolve_method <- function(method, unit, visibility, discretization) {
  discretizers <- c(
    "threshold", "width", "quantile", "kde", "kmeans", "gaussian", "hclust",
    "ordinal", "symbolic", "change_points", "entropy", "magnitude",
    "adaptive_magnitude", "percentile_magnitude", "dtw"
  )
  method <- match.arg(method, c(
    "distance", "visibility", "nvg", "natural", "hvg", "horizontal",
    discretizers
  ))
  if (method %in% discretizers) {
    return(list(
      method = "visibility",
      unit = if (is.null(unit)) "state" else unit,
      visibility = visibility,
      discretization = method
    ))
  }
  switch(
    method,
    distance = list(method = "distance", unit = unit, visibility = visibility,
                    discretization = discretization),
    visibility = list(method = "visibility", unit = unit,
                      visibility = visibility, discretization = discretization),
    nvg = ,
    natural = list(method = "visibility", unit = unit, visibility = "natural",
                   discretization = discretization),
    hvg = ,
    horizontal = list(method = "visibility", unit = unit,
                      visibility = "horizontal",
                      discretization = discretization)
  )
}

#' Adaptive default sliding-window width for distance networks
#'
#' Uses `max(2, n %/% 10)` where `n` is the shortest series length, capped so
#' at least two windows fit.
#'
#' @param source Canonical long input.
#' @return A positive integer window width.
#' @noRd
.tsn_default_window <- function(source) {
  lengths <- vapply(split(source$value, source$id), length, integer(1L))
  n_min <- min(lengths)
  max(2L, min(n_min - 1L, n_min %/% 10L))
}

#' @noRd
.tsn_resolve_unit <- function(unit, method, source) {
  choices <- if (method == "distance") c("series", "window", "time") else
    c("time", "state")
  if (is.null(unit)) {
    return(if (method == "visibility") "time" else
      if (length(unique(source$id)) > 1L) "series" else "window")
  }
  match.arg(unit, choices)
}

#' @noRd
.tsn_validate_public_arguments <- function(method, unit, window, neighbors,
                                           threshold, percentile, bandwidth,
                                           state, directed, limit, penetrable,
                                           decay, discretization = "gaussian",
                                           m = NULL, tau = NULL) {
  if (!identical(discretization, "ordinal") &&
      (!is.null(m) || !is.null(tau))) {
    stop("`m` and `tau` are only valid with `discretization = \"ordinal\"`.",
         call. = FALSE)
  }
  stopifnot(is.logical(directed), length(directed) == 1L, !is.na(directed))
  stopifnot(is.numeric(penetrable), length(penetrable) == 1L,
            is.finite(penetrable), penetrable >= 0, penetrable == as.integer(penetrable))
  stopifnot(is.numeric(decay), length(decay) == 1L, is.finite(decay), decay >= 0)
  if (!is.null(limit)) {
    stopifnot(is.numeric(limit), length(limit) == 1L, is.finite(limit),
              limit > 0)
  }
  if (method == "distance" && unit == "window" && is.null(window)) {
    stop("`window` is required when `unit = \"window\"`.", call. = FALSE)
  }
  if (method == "distance" && unit %in% c("series", "time") &&
      !is.null(window)) {
    stop("`window` is only valid when `unit = \"window\"`.", call. = FALSE)
  }
  if (method == "visibility" && any(vapply(
    list(window, neighbors, threshold, percentile, bandwidth),
    Negate(is.null),
    logical(1L)
  ))) {
    stop("Distance-network arguments cannot be used with visibility networks.",
         call. = FALSE)
  }
  if (unit != "state" && !is.null(state)) {
    stop("`state` is only valid when `unit = \"state\"`.", call. = FALSE)
  }
  numeric_options <- list(
    neighbors = neighbors,
    threshold = threshold,
    percentile = percentile,
    bandwidth = bandwidth
  )
  invalid <- vapply(
    numeric_options,
    function(option) !is.null(option) &&
      (!is.numeric(option) || length(option) != 1L || !is.finite(option)),
    logical(1L)
  )
  if (any(invalid)) {
    stop(sprintf("Invalid numeric argument: %s.", names(invalid)[which(invalid)[1L]]),
         call. = FALSE)
  }
  invisible(TRUE)
}

#' @noRd
.tsn_build_visibility <- function(source, unit, visibility, state,
                                  discretization, n_states, breaks, m, tau,
                                  directed, limit,
                                  penetrable, decay, aggregation, seed) {
  labels <- .tsn_visibility_labels(source$id, source$time)
  source$.tsn_node_label <- labels
  groups <- split(
    source,
    factor(source$id, levels = unique(source$id)),
    drop = TRUE
  )
  per_group <- Map(
    function(group, series_id) {
      group_labels <- group$.tsn_node_label
      dyads <- .tsn_visibility(
        values = group$value,
        labels = group_labels,
        times = .tsn_time_coordinates(group$time),
        method = visibility,
        directed = directed,
        limit = limit,
        penetrable = penetrable,
        decay = decay
      )
      metadata <- data.frame(
        node = group_labels,
        start = seq_len(nrow(group)),
        end = seq_len(nrow(group)),
        stringsAsFactors = FALSE
      )
      list(dyads = dyads, metadata = metadata, labels = group_labels)
    },
    groups,
    names(groups)
  )
  dyads <- do.call(rbind, lapply(per_group, `[[`, "dyads"))
  metadata <- do.call(rbind, lapply(per_group, `[[`, "metadata"))
  labels <- unlist(lapply(per_group, `[[`, "labels"), use.names = FALSE)

  if (unit == "state") {
    states <- .tsn_resolve_states(
      source = source,
      state = state,
      discretization = discretization,
      n_states = n_states,
      breaks = breaks,
      m = m,
      tau = tau,
      seed = seed
    )
    dyads <- .tsn_aggregate_visibility(
      dyads = dyads,
      states = states,
      labels = labels,
      aggregation = aggregation,
      directed = directed
    )
    nodes <- unique(as.character(states))
    metadata <- data.frame(node = nodes, start = NA_integer_, end = NA_integer_,
                           stringsAsFactors = FALSE)
  } else {
    nodes <- labels
    states <- NULL
  }
  list(
    dyads = dyads,
    metadata = metadata,
    nodes = nodes,
    states = states,
    distance_method = NA_character_,
    connection_method = visibility
  )
}

#' Create Collision-Safe Visibility Node Labels
#'
#' Uses the familiar `id:time` form unless separate `(id, time)` pairs would
#' collapse to the same text. In that rare case, length-prefixed components
#' preserve both readability and uniqueness.
#'
#' @param id Series identifiers.
#' @param time Observation times.
#' @return A unique character vector.
#' @noRd
.tsn_visibility_labels <- function(id, time) {
  stopifnot(
    is.atomic(id), (is.atomic(time) || inherits(time, "POSIXt")),
    length(id) == length(time),
    length(id) > 0L, !anyNA(id), !anyNA(time)
  )
  id_text <- as.character(id)
  time_text <- as.character(time)
  labels <- paste(id_text, time_text, sep = ":")
  if (anyDuplicated(labels)) {
    labels <- paste0(
      nchar(id_text, type = "bytes"), ":", id_text, "|",
      nchar(time_text, type = "bytes"), ":", time_text
    )
  }
  stopifnot(!anyDuplicated(labels))
  labels
}

#' @noRd
.tsn_resolve_states <- function(source, state, discretization, n_states,
                                breaks, m = NULL, tau = NULL, seed) {
  if (is.null(state)) {
    fit <- .tsn_discretize_values(
      source$value,
      method = discretization,
      n_states = n_states,
      breaks = breaks,
      m = m,
      tau = tau,
      seed = seed,
      groups = source$id
    )
    return(as.character(fit$state))
  }
  states <- if (is.character(state) && length(state) == 1L &&
                state %in% names(source)) source[[state]] else state
  if (length(states) != nrow(source) || anyNA(states)) {
    stop("`state` must provide one non-missing value per observation.",
         call. = FALSE)
  }
  as.character(states)
}

#' @noRd
.tsn_finalize_edges <- function(dyads, metadata, method, unit,
                                distance_method, connection_method, directed) {
  rownames(dyads) <- NULL
  from_position <- match(dyads$from, metadata$node)
  to_position <- match(dyads$to, metadata$node)
  if (anyNA(from_position) || anyNA(to_position)) {
    stop("Internal node metadata does not match the computed dyads.",
         call. = FALSE)
  }
  data.frame(
    from = as.character(dyads$from),
    to = as.character(dyads$to),
    distance = as.numeric(dyads$distance),
    weight = as.numeric(dyads$weight),
    connected = as.logical(dyads$connected),
    method = rep(method, nrow(dyads)),
    unit = rep(unit, nrow(dyads)),
    distance_method = rep(distance_method, nrow(dyads)),
    connection_method = rep(connection_method, nrow(dyads)),
    directed = rep(directed, nrow(dyads)),
    from_start = as.integer(metadata$start[from_position]),
    from_end = as.integer(metadata$end[from_position]),
    to_start = as.integer(metadata$start[to_position]),
    to_end = as.integer(metadata$end[to_position]),
    stringsAsFactors = FALSE
  )
}

#' Resolve the `normalize` Argument
#'
#' Accepts the historical logical form (`TRUE` means `"max"`) or a mode name.
#'
#' @param normalize Logical flag or one of `"max"`, `"minmax"`, `"quantile"`.
#' @return `NULL` for no rescaling, otherwise the mode name.
#' @noRd
.tsn_resolve_normalize <- function(normalize) {
  if (is.logical(normalize)) {
    stopifnot(length(normalize) == 1L, !is.na(normalize))
    return(if (normalize) "max" else NULL)
  }
  stopifnot(is.character(normalize), length(normalize) == 1L, !is.na(normalize))
  match.arg(normalize, c("max", "minmax", "quantile"))
}

#' Rescale a Distance Vector
#'
#' All modes keep distances non-negative so the connection rules stay valid:
#' `"max"` divides by the maximum, `"minmax"` maps the observed range onto
#' `[0, 1]`, and `"quantile"` rescales by the 5th-95th percentile range with
#' clamping to `[0, 1]` (winsorized scaling).
#'
#' @param distances Non-negative finite numeric vector.
#' @param mode `NULL` or a mode name from `.tsn_resolve_normalize()`.
#' @return The rescaled distance vector.
#' @noRd
.tsn_normalize_distances <- function(distances, mode) {
  if (is.null(mode) || length(distances) == 0L) {
    return(distances)
  }
  switch(
    mode,
    max = {
      maximum <- max(distances)
      if (is.finite(maximum) && maximum > 0) distances / maximum else distances
    },
    minmax = {
      minimum <- min(distances)
      spread <- max(distances) - minimum
      if (spread > 0) (distances - minimum) / spread else rep(0, length(distances))
    },
    quantile = {
      bounds <- stats::quantile(
        distances,
        probs = c(0.05, 0.95),
        names = FALSE,
        type = 7
      )
      spread <- bounds[2L] - bounds[1L]
      if (spread > 0) {
        pmin(1, pmax(0, (distances - bounds[1L]) / spread))
      } else {
        rep(0, length(distances))
      }
    }
  )
}

#' Validate the Method-Specific Distance Options
#'
#' `bins`, `lag`, `tolerance`, and `similarity` only apply to distance
#' networks, and the first three only to their specific distance measures.
#'
#' @param method Resolved base method (`"distance"` or `"visibility"`).
#' @param distance Selected distance measure.
#' @param bins,lag,tolerance,similarity Public arguments as supplied.
#' @return `NULL`, invisibly.
#' @noRd
.tsn_validate_distance_options <- function(method, distance, bins, lag,
                                           tolerance, similarity) {
  if (method != "distance") {
    if (!is.null(bins) || !is.null(lag) || !is.null(tolerance) ||
          !is.null(similarity)) {
      stop(
        "`bins`, `lag`, `tolerance`, and `similarity` are only valid for ",
        "distance networks.",
        call. = FALSE
      )
    }
    return(invisible(NULL))
  }
  if (!is.null(bins)) {
    if (!distance %in% c("nmi", "voi")) {
      stop("`bins` is only valid with `distance = \"nmi\"` or `\"voi\"`.",
           call. = FALSE)
    }
    stopifnot(
      is.numeric(bins), length(bins) == 1L, is.finite(bins),
      bins == as.integer(bins), bins >= 2
    )
  }
  if (!is.null(lag)) {
    if (distance != "ccf") {
      stop("`lag` is only valid with `distance = \"ccf\"`.", call. = FALSE)
    }
    stopifnot(
      is.numeric(lag), length(lag) == 1L, is.finite(lag),
      lag == as.integer(lag), lag >= 1
    )
  }
  if (!is.null(tolerance)) {
    if (!distance %in% c("event_sync", "van_rossum")) {
      stop(
        "`tolerance` is only valid with `distance = \"event_sync\"` or ",
        "`\"van_rossum\"`.",
        call. = FALSE
      )
    }
    stopifnot(
      is.numeric(tolerance), length(tolerance) == 1L,
      is.finite(tolerance), tolerance > 0
    )
  }
  if (!is.null(similarity)) {
    stopifnot(is.character(similarity), length(similarity) == 1L)
    match.arg(
      similarity,
      c("inverse", "normalized_inverse", "negative_exp", "gaussian")
    )
  }
  invisible(NULL)
}
