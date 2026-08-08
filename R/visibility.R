#' Construct visibility dyads for a time series
#'
#' Internal base-R engine for natural and horizontal visibility graphs. Candidate
#' dyads always follow temporal order. Undirected output stores each dyad once.
#'
#' @param values Finite numeric time-series values.
#' @param labels Unique character labels, one for each value.
#' @param times Strictly increasing numeric time coordinates, one for each
#'   value. Natural-visibility geometry, edge distance, `limit`, and `decay`
#'   are evaluated on this axis.
#' @param method Visibility definition: `"natural"` or `"horizontal"`.
#' @param directed Whether the temporally ordered dyads are directed.
#' @param limit Optional maximum elapsed-time lag.
#' @param penetrable Number of intermediate blockers allowed.
#' @param decay Non-negative exponential weight-decay rate.
#'
#' @return A data frame with `from`, `to`, `distance`, `weight`, and
#'   `connected` columns. Only visible dyads are returned.
#' @keywords internal
#' @noRd
.tsn_visibility <- function(
    values,
    labels = as.character(seq_along(values)),
    times = seq_along(values),
    method = c("natural", "horizontal"),
    directed = FALSE,
    limit = NULL,
    penetrable = 0L,
    decay = 0) {
  stopifnot(
    is.numeric(values),
    is.atomic(values),
    is.null(dim(values)),
    all(is.finite(values)),
    is.character(labels),
    length(labels) == length(values),
    !anyNA(labels),
    all(nzchar(labels)),
    anyDuplicated(labels) == 0L,
    is.numeric(times),
    is.atomic(times),
    is.null(dim(times)),
    length(times) == length(values),
    all(is.finite(times)),
    !is.unsorted(times, strictly = TRUE),
    is.logical(directed),
    length(directed) == 1L,
    !is.na(directed),
    is.numeric(penetrable),
    length(penetrable) == 1L,
    is.finite(penetrable),
    penetrable >= 0,
    penetrable == as.integer(penetrable),
    is.numeric(decay),
    length(decay) == 1L,
    is.finite(decay),
    decay >= 0
  )
  method <- match.arg(method)
  if (!is.null(limit)) {
    stopifnot(
      is.numeric(limit),
      length(limit) == 1L,
      is.finite(limit),
      limit > 0
    )
    limit <- as.numeric(limit)
  }
  penetrable <- as.integer(penetrable)

  if (identical(method, "horizontal") && penetrable == 0L && is.null(limit)) {
    return(.tsn_horizontal_visibility_fast(
      values,
      labels,
      times = times,
      decay = decay
    ))
  }

  empty_dyads <- data.frame(
    from = character(),
    to = character(),
    distance = numeric(),
    weight = numeric(),
    connected = logical(),
    stringsAsFactors = FALSE
  )
  if (length(values) < 2L) {
    return(empty_dyads)
  }

  candidates <- utils::combn(seq_along(values), 2L)
  temporal_distance <- times[candidates[2L, ]] - times[candidates[1L, ]]
  within_limit <- if (is.null(limit)) {
    rep.int(TRUE, length(temporal_distance))
  } else {
    temporal_distance <= limit
  }
  candidates <- candidates[, within_limit, drop = FALSE]
  temporal_distance <- temporal_distance[within_limit]
  if (ncol(candidates) == 0L) {
    return(empty_dyads)
  }

  blocker_count <- vapply(
    seq_len(ncol(candidates)),
    function(candidate_index) {
      left <- candidates[1L, candidate_index]
      right <- candidates[2L, candidate_index]
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
  visible <- blocker_count <= penetrable
  if (!any(visible)) {
    return(empty_dyads)
  }

  visible_candidates <- candidates[, visible, drop = FALSE]
  visible_distance <- temporal_distance[visible]
  data.frame(
    from = labels[visible_candidates[1L, ]],
    to = labels[visible_candidates[2L, ]],
    distance = as.numeric(visible_distance),
    weight = exp(-decay * visible_distance),
    connected = rep.int(TRUE, sum(visible)),
    stringsAsFactors = FALSE
  )
}

#' Construct a standard horizontal visibility graph in linearithmic time
#'
#' A monotone stack limits candidate comparisons. Binary search identifies the
#' suffix of lower points visible from each new observation, and a preallocated
#' environment avoids incremental edge-table growth.
#'
#' @param values Finite numeric time-series values.
#' @param labels Unique character node labels.
#' @param times Strictly increasing numeric time coordinates.
#' @param decay Non-negative temporal decay.
#' @return A tidy visibility dyad data frame.
#' @noRd
.tsn_horizontal_visibility_fast <- function(values, labels, times, decay) {
  if (length(values) < 2L) {
    return(data.frame(
      from = character(), to = character(), distance = numeric(),
      weight = numeric(), connected = logical(), stringsAsFactors = FALSE
    ))
  }
  capacity <- max(1L, 2L * length(values))
  state <- new.env(parent = emptyenv())
  state$stack <- integer(length(values))
  state$top <- 0L
  state$from <- integer(capacity)
  state$to <- integer(capacity)
  state$count <- 0L

  invisible(Reduce(
    function(unused, current) {
      top <- state$top
      if (top > 0L) {
        first_lower <- .tsn_first_lower_stack(
          stack = state$stack,
          values = values,
          target = values[current],
          left = 1L,
          right = top,
          top = top
        )
        popped <- if (first_lower <= top) {
          state$stack[seq.int(first_lower, top)]
        } else {
          integer()
        }
        boundary_position <- first_lower - 1L
        boundary <- if (boundary_position >= 1L) {
          state$stack[boundary_position]
        } else {
          integer()
        }
        visible <- c(popped, boundary)
        if (length(visible) > 0L) {
          positions <- seq.int(state$count + 1L, state$count + length(visible))
          state$from[positions] <- visible
          state$to[positions] <- current
          state$count <- state$count + length(visible)
        }
        # An equal-height boundary is visible now, but it blocks every earlier
        # point of the same or lower height from future observations. Retaining
        # it would create false edges across a horizontal blocker.
        equal_boundary <- length(boundary) == 1L &&
          values[boundary] == values[current]
        state$top <- boundary_position - as.integer(equal_boundary)
      }
      state$top <- state$top + 1L
      state$stack[state$top] <- current
      NULL
    },
    seq_along(values),
    init = NULL
  ))

  from_index <- state$from[seq_len(state$count)]
  to_index <- state$to[seq_len(state$count)]
  ordering <- order(from_index, to_index, method = "radix")
  from_index <- from_index[ordering]
  to_index <- to_index[ordering]
  temporal_distance <- times[to_index] - times[from_index]
  data.frame(
    from = labels[from_index],
    to = labels[to_index],
    distance = as.numeric(temporal_distance),
    weight = exp(-decay * temporal_distance),
    connected = rep.int(TRUE, length(from_index)),
    stringsAsFactors = FALSE
  )
}

#' Find the first monotone-stack value strictly below a target
#'
#' @param stack Integer stack of source indices.
#' @param values Numeric source values.
#' @param target Current value.
#' @param left,right Current binary-search bounds.
#' @param top Active stack size.
#' @return First matching stack position, or `top + 1` when absent.
#' @noRd
.tsn_first_lower_stack <- function(stack, values, target, left, right, top) {
  if (left > right) {
    return(top + 1L)
  }
  if (left == right) {
    return(if (values[stack[left]] < target) left else top + 1L)
  }
  middle <- (left + right) %/% 2L
  if (values[stack[middle]] < target) {
    .tsn_first_lower_stack(stack, values, target, left, middle, top)
  } else {
    .tsn_first_lower_stack(stack, values, target, middle + 1L, right, top)
  }
}

#' Aggregate visibility dyads over states
#'
#' Maps time-point labels to states and combines repeated state dyads. For an
#' undirected graph, state pairs are canonicalized before aggregation, so each
#' source visibility dyad contributes exactly once. `distance` is the mean
#' temporal lag represented by the state dyad; `weight` follows `aggregation`.
#'
#' @param dyads Output from the internal visibility engine.
#' @param states State labels corresponding to `labels`.
#' @param labels Unique time-point labels corresponding to `states`.
#' @param aggregation Edge-weight aggregation method.
#' @param directed Whether state-dyad direction should be retained.
#'
#' @return A state-level data frame with the same basic schema as `dyads`.
#' @keywords internal
#' @noRd
.tsn_aggregate_visibility <- function(
    dyads,
    states,
    labels,
    aggregation = c("sum", "mean", "max", "count"),
    directed = FALSE) {
  required_columns <- c("from", "to", "distance", "weight", "connected")
  stopifnot(
    is.data.frame(dyads),
    all(required_columns %in% names(dyads)),
    is.atomic(states),
    is.null(dim(states)),
    length(states) == length(labels),
    !anyNA(states),
    is.character(labels),
    !anyNA(labels),
    all(nzchar(labels)),
    anyDuplicated(labels) == 0L,
    is.character(dyads$from),
    is.character(dyads$to),
    is.numeric(dyads$distance),
    all(is.finite(dyads$distance)),
    all(dyads$distance >= 0),
    is.numeric(dyads$weight),
    all(is.finite(dyads$weight)),
    is.logical(dyads$connected),
    !anyNA(dyads$connected),
    all(dyads$connected),
    is.logical(directed),
    length(directed) == 1L,
    !is.na(directed)
  )
  aggregation <- match.arg(aggregation)
  states <- as.character(states)
  stopifnot(all(nzchar(states)))

  empty_dyads <- data.frame(
    from = character(),
    to = character(),
    distance = numeric(),
    weight = numeric(),
    connected = logical(),
    stringsAsFactors = FALSE
  )
  if (nrow(dyads) == 0L) {
    return(empty_dyads)
  }

  from_index <- match(dyads$from, labels)
  to_index <- match(dyads$to, labels)
  stopifnot(!anyNA(from_index), !anyNA(to_index))
  from_state <- states[from_index]
  to_state <- states[to_index]

  state_levels <- unique(states)
  from_code <- match(from_state, state_levels)
  to_code <- match(to_state, state_levels)
  canonical_from <- if (directed) from_code else pmin(from_code, to_code)
  canonical_to <- if (directed) to_code else pmax(from_code, to_code)
  pair_key <- paste(canonical_from, canonical_to, sep = "\r")
  groups <- split(
    seq_len(nrow(dyads)),
    factor(pair_key, levels = unique(pair_key))
  )

  aggregate_weight <- switch(
    aggregation,
    sum = function(index) sum(dyads$weight[index]),
    mean = function(index) mean(dyads$weight[index]),
    max = function(index) max(dyads$weight[index]),
    count = function(index) as.numeric(length(index))
  )
  first_index <- vapply(groups, function(index) index[1L], integer(1L))
  data.frame(
    from = state_levels[canonical_from[first_index]],
    to = state_levels[canonical_to[first_index]],
    distance = vapply(groups, function(index) mean(dyads$distance[index]), numeric(1L)),
    weight = vapply(groups, aggregate_weight, numeric(1L)),
    connected = rep.int(TRUE, length(groups)),
    stringsAsFactors = FALSE
  )
}
