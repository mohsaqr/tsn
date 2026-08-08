#' Supported Distance Methods
#'
#' `dtw`, `event_sync`, and `van_rossum` permit unequal lengths; all other
#' methods require equal lengths. `event_sync` and `van_rossum` interpret each
#' vector as event times rather than an aligned sequence of values.
#'
#' @noRd
.tsn_distance_methods <- c(
  "euclidean", "manhattan", "maximum", "canberra", "minkowski",
  "binary", "cosine", "correlation", "spearman", "dtw",
  "ccf", "nmi", "voi", "event_sync", "van_rossum"
)

#' Distance Methods That Permit Unequal-Length Vectors
#' @noRd
.tsn_unequal_length_methods <- c("dtw", "event_sync", "van_rossum")

#' Calculate a Distance Between Two Time Series
#'
#' Internal distance engine for numeric time-series vectors.
#'
#' @param x A non-empty finite numeric vector.
#' @param y A non-empty finite numeric vector.
#' @param method Distance method. Dynamic time warping and the event-based
#'   methods permit unequal lengths; all other methods require equal lengths.
#' @param p Minkowski power, at least one.
#' @param bins Number of quantile bins for the information-theoretic methods.
#' @param lag Maximum cross-correlation lag, or `NULL` for the `stats::ccf()`
#'   default.
#' @param tolerance Maximum event-synchronization coincidence window or van
#'   Rossum kernel time constant. For event synchronization, `NULL` uses the
#'   adaptive local window of Quian Quiroga et al. (2002). For van Rossum,
#'   `NULL` uses the median positive inter-event interval of the pooled pair.
#'
#' @return A finite non-negative numeric scalar.
#' @noRd
.tsn_distance <- function(
    x,
    y,
    method = .tsn_distance_methods,
    p = 2,
    bins = 10L,
    lag = NULL,
    tolerance = NULL) {
  stopifnot(
    is.numeric(x), length(x) > 0L, all(is.finite(x)),
    is.numeric(y), length(y) > 0L, all(is.finite(y)),
    is.character(method), length(method) >= 1L, !anyNA(method),
    is.numeric(p), length(p) == 1L, is.finite(p), p >= 1
  )
  method <- match.arg(method, .tsn_distance_methods)
  stopifnot(
    method %in% .tsn_unequal_length_methods || length(x) == length(y)
  )

  distance <- switch(
    method,
    euclidean = sqrt(sum((x - y)^2)),
    manhattan = sum(abs(x - y)),
    maximum = max(abs(x - y)),
    canberra = .tsn_canberra_distance(x, y),
    minkowski = sum(abs(x - y)^p)^(1 / p),
    binary = .tsn_binary_distance(x, y),
    cosine = .tsn_cosine_distance(x, y),
    correlation = .tsn_correlation_distance(x, y, method = "pearson"),
    spearman = .tsn_correlation_distance(x, y, method = "spearman"),
    dtw = .tsn_dtw_distance(x, y),
    ccf = .tsn_ccf_distance(x, y, lag = lag),
    nmi = .tsn_information_distance(x, y, bins = bins, type = "nmi"),
    voi = .tsn_information_distance(x, y, bins = bins, type = "voi"),
    event_sync = .tsn_event_sync_distance(x, y, tolerance = tolerance),
    van_rossum = .tsn_van_rossum_distance(x, y, tolerance = tolerance)
  )
  stopifnot(length(distance) == 1L, is.finite(distance), distance >= 0)
  as.numeric(distance)
}

#' Calculate All Unique Pairwise Time-Series Distances
#'
#' @param units A named list of non-empty finite numeric vectors.
#' @param method Distance method accepted by the internal scalar engine.
#' @param p Minkowski power, at least one.
#' @param bins,lag,tolerance Method-specific options passed to the scalar
#'   engine.
#' @param groups Optional unit-group identifiers. When supplied, consecutive
#'   links are created only within a group.
#'
#' @return A data frame with one unique unordered dyad per row and columns
#'   `from`, `to`, and `distance`.
#' @noRd
.tsn_pairwise_distance <- function(
    units,
    method = .tsn_distance_methods,
    p = 2,
    bins = 10L,
    lag = NULL,
    tolerance = NULL) {
  stopifnot(
    is.list(units), length(units) > 0L,
    !is.null(names(units)), !anyNA(names(units)), all(nzchar(names(units))),
    !anyDuplicated(names(units)),
    is.numeric(p), length(p) == 1L, is.finite(p), p >= 1
  )
  valid_units <- vapply(
    units,
    function(unit) {
      is.numeric(unit) && length(unit) > 0L && all(is.finite(unit))
    },
    logical(1)
  )
  stopifnot(all(valid_units))
  method <- match.arg(method, .tsn_distance_methods)
  stopifnot(
    method %in% .tsn_unequal_length_methods ||
      length(unique(vapply(units, length, integer(1)))) == 1L
  )

  if (length(units) == 1L) {
    return(data.frame(
      from = character(),
      to = character(),
      distance = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  pairs <- utils::combn(seq_along(units), 2L)
  matrix_methods <- c(
    "euclidean", "manhattan", "maximum", "canberra", "minkowski",
    "binary", "cosine", "correlation", "spearman"
  )
  distances <- if (method %in% matrix_methods) {
    .tsn_pairwise_equal_length(units, pairs, method = method, p = p)
  } else {
    vapply(
      seq_len(ncol(pairs)),
      function(index) {
        .tsn_distance(
          units[[pairs[1L, index]]],
          units[[pairs[2L, index]]],
          method = method,
          p = p,
          bins = bins,
          lag = lag,
          tolerance = tolerance
        )
      },
      numeric(1)
    )
  }
  data.frame(
    from = names(units)[pairs[1L, ]],
    to = names(units)[pairs[2L, ]],
    distance = distances,
    stringsAsFactors = FALSE
  )
}

#' Calculate equal-length pairwise distances with matrix primitives
#'
#' @param units Named equal-length numeric vectors.
#' @param pairs Two-row integer matrix of unique dyads.
#' @param method Distance method.
#' @param p Minkowski power.
#' @return Numeric vector aligned with `pairs`.
#' @noRd
.tsn_pairwise_equal_length <- function(units, pairs, method, p) {
  values <- do.call(rbind, units)
  if (method %in% c(
    "euclidean", "manhattan", "maximum", "canberra", "minkowski", "binary"
  )) {
    arguments <- list(x = values, method = method)
    if (method == "minkowski") {
      arguments$p <- p
    }
    return(as.numeric(do.call(stats::dist, arguments)))
  }
  similarity <- if (method == "cosine") {
    norms <- sqrt(rowSums(values^2))
    if (any(norms == 0)) {
      return(vapply(
        seq_len(ncol(pairs)),
        function(index) .tsn_cosine_distance(
          units[[pairs[1L, index]]],
          units[[pairs[2L, index]]]
        ),
        numeric(1L)
      ))
    }
    tcrossprod(values) / outer(norms, norms)
  } else {
    correlation_method <- if (method == "spearman") "spearman" else "pearson"
    if (any(apply(values, 1L, stats::sd) == 0)) {
      stop("Correlation distance requires non-constant series.", call. = FALSE)
    }
    stats::cor(t(values), method = correlation_method)
  }
  selected <- 1 - similarity[cbind(pairs[1L, ], pairs[2L, ])]
  if (any(!is.finite(selected))) {
    stop("Correlation distance requires non-constant series.", call. = FALSE)
  }
  pmin(2, pmax(0, as.numeric(selected)))
}

#' Calculate Consecutive (Chain) Time-Series Distances
#'
#' Connects only consecutive units, producing a transition chain rather than a
#' fully connected distance network.
#'
#' @param units A named list of numeric vectors in order.
#' @param method Distance method accepted by the internal scalar engine.
#' @param p Minkowski power, at least one.
#' @param bins,lag,tolerance Method-specific options passed to the scalar
#'   engine.
#' @return A data frame with columns `from`, `to`, and `distance`, one row per
#'   consecutive unit pair.
#' @noRd
.tsn_chain_distance <- function(
    units,
    method = .tsn_distance_methods,
    p = 2,
    bins = 10L,
    lag = NULL,
    tolerance = NULL,
    groups = NULL) {
  stopifnot(
    is.list(units), length(units) >= 2L, !is.null(names(units)),
    !anyDuplicated(names(units)),
    is.numeric(p), length(p) == 1L, is.finite(p), p >= 1
  )
  if (!is.null(groups)) {
    stopifnot(
      is.atomic(groups), length(groups) == length(units), !anyNA(groups)
    )
  }
  method <- match.arg(method, .tsn_distance_methods)
  k <- length(units)
  indices <- seq_len(k - 1L)
  if (!is.null(groups)) {
    indices <- indices[groups[indices] == groups[indices + 1L]]
  }
  distances <- vapply(
    indices,
    function(index) {
      .tsn_distance(
        units[[index]],
        units[[index + 1L]],
        method = method,
        p = p,
        bins = bins,
        lag = lag,
        tolerance = tolerance
      )
    },
    numeric(1L)
  )
  data.frame(
    from = names(units)[indices],
    to = names(units)[indices + 1L],
    distance = distances,
    stringsAsFactors = FALSE
  )
}

#' Calculate Canberra Distance
#'
#' @param x,y Equal-length numeric vectors.
#'
#' @return A numeric scalar.
#' @noRd
.tsn_canberra_distance <- function(x, y) {
  denominators <- abs(x) + abs(y)
  contributions <- ifelse(denominators == 0, 0, abs(x - y) / denominators)
  sum(contributions)
}

#' Calculate Binary Distance
#'
#' @param x,y Equal-length numeric vectors interpreted as absent or present.
#'
#' @return The proportion of discordant presences among positions where at
#'   least one vector is present.
#' @noRd
.tsn_binary_distance <- function(x, y) {
  x_present <- x != 0
  y_present <- y != 0
  union_size <- sum(x_present | y_present)
  if (union_size == 0L) {
    return(0)
  }
  sum(x_present != y_present) / union_size
}

#' Calculate Cosine Distance
#'
#' @param x,y Equal-length numeric vectors.
#'
#' @return One minus cosine similarity. Two zero vectors have distance zero;
#'   a single zero vector has distance one from a non-zero vector.
#' @noRd
.tsn_cosine_distance <- function(x, y) {
  x_norm <- sqrt(sum(x^2))
  y_norm <- sqrt(sum(y^2))
  if (x_norm == 0 && y_norm == 0) {
    return(0)
  }
  if (x_norm == 0 || y_norm == 0) {
    return(1)
  }
  similarity <- sum(x * y) / (x_norm * y_norm)
  1 - min(1, max(-1, similarity))
}

#' Calculate Correlation Distance
#'
#' @param x,y Equal-length non-constant numeric vectors.
#' @param method Either `"pearson"` or `"spearman"`.
#'
#' @return One minus the requested correlation.
#' @noRd
.tsn_correlation_distance <- function(x, y, method) {
  stopifnot(length(x) >= 2L, stats::sd(x) > 0, stats::sd(y) > 0)
  correlation <- stats::cor(x, y, method = method)
  stopifnot(length(correlation) == 1L, is.finite(correlation))
  1 - min(1, max(-1, correlation))
}

#' Calculate Cross-Correlation Distance
#'
#' One minus the maximum absolute cross-correlation across lags, following
#' the ts2net convention. Requires non-constant series.
#'
#' @param x,y Equal-length numeric vectors.
#' @param lag Maximum lag, or `NULL` for the `stats::ccf()` default.
#'
#' @return A numeric scalar in `[0, 1]`.
#' @noRd
.tsn_ccf_distance <- function(x, y, lag = NULL) {
  stopifnot(length(x) >= 2L, stats::sd(x) > 0, stats::sd(y) > 0)
  correlations <- stats::ccf(
    x, y,
    lag.max = lag,
    plot = FALSE,
    demean = TRUE
  )$acf
  stopifnot(length(correlations) > 0L, all(is.finite(correlations)))
  1 - min(1, max(abs(correlations)))
}

#' Calculate an Information-Theoretic Distance
#'
#' Bins each series by its own empirical quantiles, then computes either one
#' minus the normalized mutual information (`nmi`, normalized by the larger
#' marginal entropy) or the variation of information (`voi`, in nats).
#' Marginal binning preserves the expected invariance to monotone changes of
#' measurement scale.
#'
#' @param x,y Equal-length numeric vectors.
#' @param bins Number of quantile bins.
#' @param type Either `"nmi"` or `"voi"`.
#'
#' @return A non-negative numeric scalar; `nmi` lies in `[0, 1]`.
#' @noRd
.tsn_information_distance <- function(x, y, bins = 10L, type = c("nmi", "voi")) {
  type <- match.arg(type)
  stopifnot(is.numeric(bins), length(bins) == 1L, is.finite(bins), bins >= 2L)
  labels <- list(
    x = .tsn_marginal_quantile_bins(x, bins = as.integer(bins)),
    y = .tsn_marginal_quantile_bins(y, bins = as.integer(bins))
  )
  joint <- table(labels$x, labels$y) / length(x)
  x_marginal <- rowSums(joint)
  y_marginal <- colSums(joint)
  entropy_x <- -sum(x_marginal[x_marginal > 0] * log(x_marginal[x_marginal > 0]))
  entropy_y <- -sum(y_marginal[y_marginal > 0] * log(y_marginal[y_marginal > 0]))
  expected <- outer(x_marginal, y_marginal)
  positive <- joint > 0
  mutual_information <- sum(joint[positive] * log(joint[positive] / expected[positive]))
  mutual_information <- max(0, mutual_information)
  if (type == "voi") {
    return(max(0, entropy_x + entropy_y - 2 * mutual_information))
  }
  maximum_entropy <- max(entropy_x, entropy_y)
  if (maximum_entropy == 0) {
    return(0)
  }
  1 - min(1, mutual_information / maximum_entropy)
}

#' Bin One Series by Marginal Quantiles
#'
#' @param values Numeric vector.
#' @param bins Number of bins.
#'
#' @return An integer label vector.
#' @noRd
.tsn_marginal_quantile_bins <- function(values, bins) {
  stopifnot(
    is.numeric(values), length(values) > 0L, all(is.finite(values)),
    is.numeric(bins), length(bins) == 1L, is.finite(bins), bins >= 2L
  )
  observed_bins <- min(as.integer(bins), length(unique(values)))
  if (observed_bins < 2L) {
    return(rep.int(1L, length(values)))
  }
  as.integer(
    cut(
      values,
      breaks = .tsn_quantile_breaks(values, observed_bins),
      include.lowest = TRUE,
      labels = FALSE
    )
  )
}

#' Calculate Event-Synchronization Distance
#'
#' Interprets both vectors as strictly increasing event times and implements
#' the adaptive Quian Quiroga, Kreuz, and Grassberger (2002) event-
#' synchronization index. Each event-pair window is half the minimum of the
#' four adjacent inter-event intervals. `tolerance`, when supplied, caps that
#' adaptive window. Simultaneous events contribute one half to each temporal
#' direction. The distance is one minus the symmetric synchronization index,
#' normalized by the geometric mean of the event counts.
#'
#' @param x,y Strictly increasing event-time vectors (any positive lengths).
#' @param tolerance Optional positive upper bound on the adaptive coincidence
#'   windows.
#'
#' @return A numeric scalar in `[0, 1]`.
#' @noRd
.tsn_event_sync_distance <- function(x, y, tolerance = NULL) {
  .tsn_validate_event_times(x, "x")
  .tsn_validate_event_times(y, "y")
  if (!is.null(tolerance)) {
    stopifnot(
      is.numeric(tolerance), length(tolerance) == 1L,
      is.finite(tolerance), tolerance > 0
    )
  }

  local_window <- function(events) {
    previous_gap <- c(Inf, diff(events))
    next_gap <- c(diff(events), Inf)
    pmin(previous_gap, next_gap) / 2
  }
  coincidence_window <- outer(
    local_window(x),
    local_window(y),
    FUN = pmin
  )
  if (!is.null(tolerance)) {
    coincidence_window <- pmin(coincidence_window, tolerance)
  }

  signed_gap <- outer(x, y, "-")
  simultaneous <- signed_gap == 0
  x_after_y <- signed_gap > 0 & signed_gap <= coincidence_window
  y_after_x <- signed_gap < 0 & -signed_gap <= coincidence_window
  count_x_after_y <- sum(x_after_y) + 0.5 * sum(simultaneous)
  count_y_after_x <- sum(y_after_x) + 0.5 * sum(simultaneous)
  synchronization <- (count_x_after_y + count_y_after_x) /
    sqrt(length(x) * length(y))
  1 - min(1, max(0, synchronization))
}

#' Calculate the van Rossum Spike-Train Distance
#'
#' Exact continuous-time van Rossum distance with a causal exponential kernel
#' of time constant `tolerance`, computed from the closed-form kernel sums
#' rather than a discretized grid.
#'
#' @param x,y Event-time vectors (any lengths).
#' @param tolerance Kernel time constant, or `NULL` for the median inter-event
#'   interval of the pooled train.
#'
#' @return A non-negative numeric scalar.
#' @noRd
.tsn_van_rossum_distance <- function(x, y, tolerance = NULL) {
  .tsn_validate_event_times(x, "x")
  .tsn_validate_event_times(y, "y")
  tau <- .tsn_event_tolerance(x, y, tolerance)
  kernel_sum <- function(a, b) sum(exp(-abs(outer(a, b, "-")) / tau))
  squared <- (kernel_sum(x, x) + kernel_sum(y, y) - 2 * kernel_sum(x, y)) / 2
  sqrt(max(0, squared))
}

#' Validate an Event-Time Vector
#'
#' @param events Numeric event times.
#' @param name Argument label used in an informative error.
#'
#' @return `NULL`, invisibly.
#' @noRd
.tsn_validate_event_times <- function(events, name) {
  stopifnot(
    is.numeric(events), length(events) > 0L, all(is.finite(events)),
    is.character(name), length(name) == 1L, !is.na(name), nzchar(name)
  )
  if (is.unsorted(events, strictly = TRUE)) {
    stop(sprintf("Event times in `%s` must be strictly increasing.", name),
         call. = FALSE)
  }
  invisible(NULL)
}

#' Resolve the Van Rossum Time Constant
#'
#' @param x,y Strictly increasing event-time vectors.
#' @param tolerance User-supplied positive scalar or `NULL`.
#'
#' @return A positive numeric scalar.
#' @noRd
.tsn_event_tolerance <- function(x, y, tolerance) {
  if (!is.null(tolerance)) {
    stopifnot(
      is.numeric(tolerance), length(tolerance) == 1L,
      is.finite(tolerance), tolerance > 0
    )
    return(as.numeric(tolerance))
  }
  pooled_gaps <- diff(sort(c(x, y)))
  positive_gaps <- pooled_gaps[pooled_gaps > 0]
  if (length(positive_gaps) == 0L) {
    return(1)
  }
  stats::median(positive_gaps)
}

#' Calculate Dynamic Time-Warping Distance
#'
#' Uses absolute local cost and an unconstrained symmetric warping path.
#'
#' @param x,y Non-empty finite numeric vectors.
#'
#' @return The cumulative path cost.
#' @noRd
.tsn_dtw_distance <- function(x, y) {
  n_x <- length(x)
  n_y <- length(y)
  initial_costs <- matrix(Inf, nrow = n_x + 1L, ncol = n_y + 1L)
  initial_costs[1L, 1L] <- 0
  costs <- Reduce(
    function(cost_matrix, diagonal) {
      x_indices <- seq.int(max(1L, diagonal - n_y), min(n_x, diagonal - 1L))
      y_indices <- diagonal - x_indices
      predecessors <- pmin(
        cost_matrix[cbind(x_indices, y_indices + 1L)],
        cost_matrix[cbind(x_indices + 1L, y_indices)],
        cost_matrix[cbind(x_indices, y_indices)]
      )
      cost_matrix[cbind(x_indices + 1L, y_indices + 1L)] <-
        abs(x[x_indices] - y[y_indices]) + predecessors
      cost_matrix
    },
    seq.int(2L, n_x + n_y),
    init = initial_costs
  )
  costs[n_x + 1L, n_y + 1L]
}
