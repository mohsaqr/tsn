#' Internal time-series discretization engine
#'
#' Discretizes a finite numeric vector while preserving observation order.
#' State identifiers are always ordered by increasing within-state mean. This
#' engine uses only base and recommended R packages and is intentionally kept
#' separate from the public API.
#'
#' @param values A finite numeric vector.
#' @param method One of `"threshold"`, `"width"`, `"quantile"`, `"kde"`,
#'   `"kmeans"`, `"gaussian"`, `"hclust"`, `"ordinal"`, `"symbolic"`,
#'   `"change_points"`, `"entropy"`, `"magnitude"`, `"adaptive_magnitude"`,
#'   `"percentile_magnitude"`, or `"dtw"`.
#' @param n_states Integer number of requested states. Ignored by
#'   `method = "ordinal"`, whose state count is governed by `m` and `tau`.
#' @param thresholds Numeric thresholds used by `method = "threshold"`.
#' @param max_iterations Positive integer iteration limit for iterative methods.
#' @param tolerance Positive finite convergence tolerance.
#' @param seed Optional single integer seed. Random-number state is restored on
#'   exit.
#' @param m Embedding dimension for `method = "ordinal"`. `NULL` uses `3`.
#' @param tau Embedding lag for `method = "ordinal"`. `NULL` uses `1`.
#'
#' @return A data frame with integer `index` and `state` columns and numeric
#'   `probability`. Model details are stored in the `model` attribute; binning
#'   methods also store their boundaries in the `breaks` attribute. Integer
#'   state labels increase with the within-state mean for every method except
#'   `"ordinal"`, whose labels follow lexicographic permutation-pattern order.
#' @keywords internal
#' @noRd
.tsn_discretize <- function(
    values,
    method = c(
      "threshold", "width", "quantile", "kde", "kmeans", "gaussian",
      "hclust", "ordinal", "symbolic", "change_points", "entropy",
      "magnitude", "adaptive_magnitude", "percentile_magnitude", "dtw"
    ),
    n_states = 3L,
    thresholds = NULL,
    max_iterations = 200L,
    tolerance = 1e-8,
    seed = NULL,
    m = NULL,
    tau = NULL
) {
  stopifnot(is.numeric(values), is.atomic(values), is.null(dim(values)))
  if (!length(values) || anyNA(values) || any(!is.finite(values))) {
    stop("`values` must be a non-empty, finite numeric vector.", call. = FALSE)
  }

  method <- match.arg(method)
  if (method != "ordinal" && (!is.null(m) || !is.null(tau))) {
    stop(
      "`m` and `tau` are only used when `discretization = \"ordinal\"`.",
      call. = FALSE
    )
  }
  if (method != "ordinal") {
    .tsn_check_count(n_states, "n_states", lower = 2L)
  }
  .tsn_check_count(max_iterations, "max_iterations", lower = 1L)
  if (!is.numeric(tolerance) || length(tolerance) != 1L ||
      is.na(tolerance) || !is.finite(tolerance) || tolerance <= 0) {
    stop("`tolerance` must be one positive finite number.", call. = FALSE)
  }
  if (!is.null(seed)) {
    .tsn_check_count(seed, "seed", lower = 0L)
  }

  if (method != "ordinal") {
    n_states <- as.integer(n_states)
  }
  max_iterations <- as.integer(max_iterations)
  unique_values <- sort(unique(values))
  if (method != "ordinal" && length(unique_values) < n_states) {
    stop(
      sprintf(
        "`values` has %d unique value(s), but `n_states = %d`; at least one unique value per state is required.",
        length(unique_values), n_states
      ),
      call. = FALSE
    )
  }

  fit <- switch(
    method,
    threshold = .tsn_discretize_threshold(values, n_states, thresholds),
    width = .tsn_discretize_width(values, n_states),
    quantile = .tsn_discretize_quantile(values, n_states),
    kde = .tsn_discretize_kde(values, n_states),
    kmeans = .tsn_discretize_kmeans(
      values, n_states, max_iterations, seed
    ),
    gaussian = .tsn_discretize_gaussian(
      values, n_states, max_iterations, tolerance, seed
    ),
    hclust = .tsn_discretize_hclust(values, n_states),
    ordinal = .tsn_discretize_ordinal(values, m, tau),
    symbolic = .tsn_discretize_symbolic(values, n_states),
    change_points = .tsn_discretize_change_points(values, n_states),
    entropy = .tsn_discretize_entropy(values, n_states),
    magnitude = .tsn_discretize_magnitude(values, n_states),
    adaptive_magnitude = .tsn_discretize_adaptive_magnitude(values, n_states),
    percentile_magnitude = .tsn_discretize_percentile_magnitude(
      values, n_states
    ),
    dtw = .tsn_discretize_dtw(values, n_states)
  )

  if (isTRUE(fit$pre_ordered)) {
    state <- as.integer(fit$state)
    state_means <- as.numeric(tapply(values, state, mean))
  } else {
    ordered <- .tsn_order_states(values, fit$state)
    state <- ordered$state
    state_means <- ordered$means
  }
  result <- data.frame(
    index = seq_along(values),
    state = state,
    probability = as.numeric(fit$probability),
    stringsAsFactors = FALSE
  )
  attr(result, "model") <- c(
    list(
      method = method,
      n_states = length(state_means),
      state_means = state_means
    ),
    fit$model
  )
  if (!is.null(fit$breaks)) {
    attr(result, "breaks") <- fit$breaks
  }
  result
}

#' @keywords internal
#' @noRd
.tsn_check_count <- function(value, name, lower) {
  valid <- is.numeric(value) && length(value) == 1L && !is.na(value) &&
    is.finite(value) && value == as.integer(value) && value >= lower
  if (!valid) {
    stop(
      sprintf("`%s` must be a single integer greater than or equal to %d.",
              name, lower),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' @keywords internal
#' @noRd
.tsn_order_states <- function(values, states) {
  if (length(states) != length(values) || anyNA(states)) {
    stop("Internal discretization produced invalid state assignments.",
         call. = FALSE)
  }
  state_means <- tapply(values, states, mean)
  ordered_labels <- names(sort(state_means, na.last = NA))
  remapped <- match(as.character(states), ordered_labels)
  list(
    state = as.integer(remapped),
    means = as.numeric(sort(state_means, na.last = NA))
  )
}

#' @keywords internal
#' @noRd
.tsn_cut_fit <- function(values, breaks, method, fallback = FALSE) {
  states <- cut(
    values,
    breaks = breaks,
    labels = FALSE,
    include.lowest = TRUE,
    right = TRUE
  )
  list(
    state = as.integer(states),
    probability = rep(1, length(values)),
    model = list(fallback = fallback, boundaries = breaks),
    breaks = breaks
  )
}

#' @keywords internal
#' @noRd
.tsn_discretize_threshold <- function(values, n_states, thresholds) {
  if (!is.numeric(thresholds) || length(thresholds) != n_states - 1L ||
      anyNA(thresholds) || any(!is.finite(thresholds))) {
    stop(
      sprintf("`thresholds` must contain exactly %d finite numeric value(s).",
              n_states - 1L),
      call. = FALSE
    )
  }
  thresholds <- sort(as.numeric(thresholds))
  if (anyDuplicated(thresholds) ||
      any(thresholds <= min(values)) || any(thresholds >= max(values))) {
    stop("`thresholds` must be distinct and strictly inside the data range.",
         call. = FALSE)
  }
  .tsn_cut_fit(values, c(-Inf, thresholds, Inf), "threshold")
}

#' @keywords internal
#' @noRd
.tsn_discretize_width <- function(values, n_states) {
  internal <- seq(min(values), max(values), length.out = n_states + 1L)
  .tsn_cut_fit(values, c(-Inf, internal[-c(1L, n_states + 1L)], Inf), "width")
}

#' @keywords internal
#' @noRd
.tsn_quantile_breaks <- function(values, n_states) {
  proposed <- unname(stats::quantile(
    values,
    probs = seq(0, 1, length.out = n_states + 1L),
    names = FALSE,
    type = 8
  ))
  internal <- proposed[-c(1L, n_states + 1L)]
  if (length(unique(internal)) == length(internal) &&
      all(internal > min(values)) && all(internal < max(values))) {
    return(c(-Inf, internal, Inf))
  }

  unique_values <- sort(unique(values))
  gap_indices <- unique(round(seq(
    1,
    length(unique_values) - 1L,
    length.out = n_states - 1L
  )))
  if (length(gap_indices) != n_states - 1L) {
    stop("Unable to construct distinct quantile boundaries.", call. = FALSE)
  }
  midpoints <- (
    unique_values[gap_indices] + unique_values[gap_indices + 1L]
  ) / 2
  c(-Inf, midpoints, Inf)
}

#' @keywords internal
#' @noRd
.tsn_discretize_quantile <- function(values, n_states) {
  .tsn_cut_fit(values, .tsn_quantile_breaks(values, n_states), "quantile")
}

#' @keywords internal
#' @noRd
.tsn_discretize_kde <- function(values, n_states) {
  estimate <- stats::density(values, n = 512L)
  candidate <- which(
    estimate$y[2:(length(estimate$y) - 1L)] <=
      estimate$y[1:(length(estimate$y) - 2L)] &
      estimate$y[2:(length(estimate$y) - 1L)] <=
      estimate$y[3:length(estimate$y)] &
      (
        estimate$y[2:(length(estimate$y) - 1L)] <
          estimate$y[1:(length(estimate$y) - 2L)] |
          estimate$y[2:(length(estimate$y) - 1L)] <
          estimate$y[3:length(estimate$y)]
      )
  ) + 1L
  candidate <- candidate[
    estimate$x[candidate] > min(values) & estimate$x[candidate] < max(values)
  ]
  selected <- utils::head(candidate[order(estimate$y[candidate])], n_states - 1L)
  use_fallback <- length(selected) < n_states - 1L
  breaks <- if (use_fallback) {
    .tsn_quantile_breaks(values, n_states)
  } else {
    c(-Inf, sort(estimate$x[selected]), Inf)
  }
  fit <- .tsn_cut_fit(values, breaks, "kde", fallback = use_fallback)
  if (length(unique(fit$state)) < n_states && !use_fallback) {
    fit <- .tsn_cut_fit(
      values,
      .tsn_quantile_breaks(values, n_states),
      "kde",
      fallback = TRUE
    )
  }
  fit$model$density <- estimate
  fit
}

#' @keywords internal
#' @noRd
.tsn_initial_centers <- function(values, n_states) {
  unique_values <- sort(unique(values))
  indices <- unique(round(seq(
    1,
    length(unique_values),
    length.out = n_states
  )))
  if (length(indices) != n_states) {
    stop("Unable to initialize distinct cluster centers.", call. = FALSE)
  }
  unique_values[indices]
}

#' @keywords internal
#' @noRd
.tsn_discretize_kmeans <- function(values, n_states, max_iterations, seed) {
  centers <- .tsn_initial_centers(values, n_states)
  fit <- .tsn_with_seed(seed, stats::kmeans(
    x = values,
    centers = centers,
    iter.max = max_iterations,
    algorithm = "Lloyd"
  ))
  list(
    state = fit$cluster,
    probability = rep(1, length(values)),
    model = list(
      centers = as.numeric(fit$centers),
      withinss = fit$withinss,
      total_withinss = fit$tot.withinss,
      iterations = fit$iter,
      converged = fit$ifault == 0L
    ),
    breaks = NULL
  )
}

#' @keywords internal
#' @noRd
.tsn_discretize_hclust <- function(values, n_states) {
  tree <- stats::hclust(stats::dist(values), method = "complete")
  list(
    state = stats::cutree(tree, k = n_states),
    probability = rep(1, length(values)),
    model = list(tree = tree, linkage = "complete"),
    breaks = NULL
  )
}

#' @keywords internal
#' @noRd
.tsn_with_seed <- function(seed, expression) {
  effective_seed <- if (is.null(seed)) 1L else as.integer(seed)
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  previous_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit(
    if (had_seed) {
      assign(".Random.seed", previous_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    },
    add = TRUE
  )
  set.seed(effective_seed)
  force(expression)
}

#' @keywords internal
#' @noRd
.tsn_log_sum_exp_rows <- function(log_values) {
  row_maximum <- apply(log_values, 1L, max)
  row_maximum + log(rowSums(exp(log_values - row_maximum)))
}

#' @keywords internal
#' @noRd
.tsn_gmm_expectation <- function(values, weights, means, variances) {
  log_components <- vapply(
    seq_along(weights),
    function(component) {
      log(weights[component]) + stats::dnorm(
        values,
        mean = means[component],
        sd = sqrt(variances[component]),
        log = TRUE
      )
    },
    numeric(length(values))
  )
  if (is.null(dim(log_components))) {
    log_components <- matrix(log_components, ncol = length(weights))
  }
  log_normalizer <- .tsn_log_sum_exp_rows(log_components)
  posterior <- exp(log_components - log_normalizer)
  list(posterior = posterior, log_likelihood = sum(log_normalizer))
}

#' @keywords internal
#' @noRd
.tsn_gmm_iterate <- function(
    values,
    weights,
    means,
    variances,
    variance_floor,
    recovery_means,
    max_iterations,
    tolerance,
    iteration = 1L,
    previous_log_likelihood = -Inf
) {
  expectation <- .tsn_gmm_expectation(values, weights, means, variances)
  log_likelihood <- expectation$log_likelihood
  converged <- is.finite(previous_log_likelihood) &&
    abs(log_likelihood - previous_log_likelihood) <=
      tolerance * (1 + abs(previous_log_likelihood))
  if (converged || iteration >= max_iterations) {
    return(list(
      weights = weights,
      means = means,
      variances = variances,
      posterior = expectation$posterior,
      log_likelihood = log_likelihood,
      iterations = iteration,
      converged = converged
    ))
  }

  posterior <- expectation$posterior
  effective_counts <- colSums(posterior)
  collapsed <- !is.finite(effective_counts) |
    effective_counts <= max(1e-8, length(values) * 1e-10)
  safe_counts <- pmax(effective_counts, 1e-8)
  updated_means <- colSums(posterior * values) / safe_counts
  centered <- sweep(
    matrix(values, nrow = length(values), ncol = length(weights)),
    2L,
    updated_means,
    "-"
  )
  updated_variances <- colSums(posterior * centered^2) / safe_counts
  updated_weights <- effective_counts / length(values)
  updated_means[collapsed] <- recovery_means[collapsed]
  updated_variances[collapsed] <- max(stats::var(values), variance_floor)
  updated_weights[collapsed] <- 1 / length(values)
  updated_variances <- pmax(updated_variances, variance_floor)
  updated_weights <- pmax(updated_weights, .Machine$double.eps)
  updated_weights <- updated_weights / sum(updated_weights)
  component_order <- order(updated_means)

  .tsn_gmm_iterate(
    values = values,
    weights = updated_weights[component_order],
    means = updated_means[component_order],
    variances = updated_variances[component_order],
    variance_floor = variance_floor,
    recovery_means = recovery_means,
    max_iterations = max_iterations,
    tolerance = tolerance,
    iteration = iteration + 1L,
    previous_log_likelihood = log_likelihood
  )
}

#' @keywords internal
#' @noRd
.tsn_gmm_start <- function(
    values,
    means,
    variance_floor,
    max_iterations,
    tolerance,
    recovery_means
) {
  initial_state <- max.col(-abs(outer(values, means, "-")), ties.method = "first")
  global_variance <- max(stats::var(values), variance_floor)
  initial_variances <- vapply(
    seq_along(means),
    function(component) {
      component_values <- values[initial_state == component]
      component_variance <- if (length(component_values) > 1L) {
        stats::var(component_values)
      } else {
        global_variance
      }
      max(component_variance, variance_floor)
    },
    numeric(1L)
  )
  initial_weights <- tabulate(initial_state, nbins = length(means)) / length(values)
  initial_weights <- pmax(initial_weights, .Machine$double.eps)
  initial_weights <- initial_weights / sum(initial_weights)
  component_order <- order(means)

  .tsn_gmm_iterate(
    values = values,
    weights = initial_weights[component_order],
    means = means[component_order],
    variances = initial_variances[component_order],
    variance_floor = variance_floor,
    recovery_means = recovery_means,
    max_iterations = max_iterations,
    tolerance = tolerance
  )
}

#' @keywords internal
#' @noRd
.tsn_discretize_gaussian <- function(
    values,
    n_states,
    max_iterations,
    tolerance,
    seed
) {
  recovery_means <- .tsn_initial_centers(values, n_states)
  variance_floor <- max(
    stats::var(values) * 1e-8,
    (.Machine$double.eps^0.5) * max(1, stats::var(values))
  )
  starts <- .tsn_with_seed(seed, lapply(seq_len(5L), function(start) {
    if (start == 1L) {
      recovery_means
    } else {
      sort(sample(unique(values), n_states, replace = FALSE))
    }
  }))
  fits <- lapply(
    starts,
    .tsn_gmm_start,
    values = values,
    variance_floor = variance_floor,
    max_iterations = max_iterations,
    tolerance = tolerance,
    recovery_means = recovery_means
  )
  likelihoods <- vapply(fits, `[[`, numeric(1L), "log_likelihood")
  fit <- fits[[which.max(likelihoods)]]
  state <- max.col(fit$posterior, ties.method = "first")
  probability <- fit$posterior[cbind(seq_along(values), state)]
  parameter_count <- 3L * n_states - 1L
  entropy <- -sum(fit$posterior * log(pmax(
    fit$posterior,
    .Machine$double.xmin
  )))
  model <- list(
    weights = fit$weights,
    means = fit$means,
    variances = fit$variances,
    posterior = fit$posterior,
    log_likelihood = fit$log_likelihood,
    aic = -2 * fit$log_likelihood + 2 * parameter_count,
    bic = -2 * fit$log_likelihood + log(length(values)) * parameter_count,
    icl = -2 * fit$log_likelihood + log(length(values)) * parameter_count +
      2 * entropy,
    converged = fit$converged,
    iterations = fit$iterations,
    variance_floor = variance_floor,
    starts = length(starts),
    start_log_likelihoods = likelihoods
  )
  list(
    state = state,
    probability = probability,
    model = model,
    breaks = NULL
  )
}

#' Discretize by Bandt-Pompe ordinal permutation patterns
#'
#' Symbolizes the series by the ordinal (permutation) pattern of each embedding
#' window of dimension `m` and lag `tau`. Ties are broken by first-occurrence
#' rank, so equal values keep their temporal order. Distinct observed patterns
#' become states ordered lexicographically by their rank vector; this ordering
#' is final (pre-ordered), unlike the mean-based ordering used by magnitude
#' methods. Trailing points that cannot open a full window inherit the pattern
#' of the last complete window, keeping every observation labelled.
#'
#' @param values Finite numeric vector.
#' @param m Embedding dimension (`NULL` uses 3).
#' @param tau Embedding lag (`NULL` uses 1).
#' @return A discretization fit list with a `pre_ordered` flag.
#' @keywords internal
#' @noRd
.tsn_discretize_ordinal <- function(values, m = NULL, tau = NULL) {
  m <- if (is.null(m)) 3L else m
  tau <- if (is.null(tau)) 1L else tau
  .tsn_check_count(m, "m", lower = 2L)
  .tsn_check_count(tau, "tau", lower = 1L)
  m <- as.integer(m)
  tau <- as.integer(tau)
  n <- length(values)
  span <- (m - 1L) * tau
  if (n <= span) {
    stop(
      sprintf(
        "`ordinal` discretization needs more than %d observation(s) for m = %d and tau = %d.",
        span, m, tau
      ),
      call. = FALSE
    )
  }
  n_windows <- n - span
  offsets <- seq.int(0L, span, by = tau)
  window_keys <- vapply(
    seq_len(n_windows),
    function(start) {
      window <- values[start + offsets]
      paste(rank(window, ties.method = "first"), collapse = "-")
    },
    character(1L)
  )
  pattern_levels <- sort(unique(window_keys))
  window_state <- match(window_keys, pattern_levels)
  trailing <- n - n_windows
  state <- c(window_state, rep.int(window_state[n_windows], trailing))
  list(
    state = as.integer(state),
    probability = rep.int(1, n),
    model = list(
      m = m,
      tau = tau,
      patterns = pattern_levels,
      n_patterns = length(pattern_levels),
      n_windows = n_windows,
      trailing_filled = trailing
    ),
    breaks = NULL,
    pre_ordered = TRUE
  )
}

#' Discretize by SAX-style Gaussian breakpoints
#'
#' Z-normalizes the series and places `n_states - 1` interior breakpoints at the
#' Gaussian quantiles `qnorm(seq_len(n_states - 1) / n_states)`. Breakpoints are
#' expressed back in the original value scale so the stored boundaries stay
#' consistent with the other binning engines.
#'
#' @param values Finite numeric vector.
#' @param n_states Requested number of states.
#' @return A discretization fit list.
#' @keywords internal
#' @noRd
.tsn_discretize_symbolic <- function(values, n_states) {
  mean_value <- mean(values)
  sd_value <- stats::sd(values)
  if (!is.finite(sd_value) || sd_value <= 0) {
    stop("`symbolic` discretization requires a non-constant series.",
         call. = FALSE)
  }
  breakpoints_z <- stats::qnorm(seq_len(n_states - 1L) / n_states)
  internal <- breakpoints_z * sd_value + mean_value
  fit <- .tsn_cut_fit(values, c(-Inf, internal, Inf), "symbolic")
  fit$model$mean <- mean_value
  fit$model$sd <- sd_value
  fit$model$breakpoints <- breakpoints_z
  fit
}

#' Discretize by largest-jump value segmentation
#'
#' Sorts the distinct values, finds the `n_states - 1` largest consecutive gaps,
#' and places breaks at their midpoints. This isolates natural clusters in the
#' value distribution rather than imposing equal widths or frequencies.
#'
#' @param values Finite numeric vector.
#' @param n_states Requested number of states.
#' @return A discretization fit list.
#' @keywords internal
#' @noRd
.tsn_discretize_change_points <- function(values, n_states) {
  sorted <- sort(unique(values))
  gaps <- diff(sorted)
  chosen <- sort(order(gaps, decreasing = TRUE)[seq_len(n_states - 1L)])
  midpoints <- (sorted[chosen] + sorted[chosen + 1L]) / 2
  fit <- .tsn_cut_fit(values, c(-Inf, midpoints, Inf), "change_points")
  fit$model$change_points <- midpoints
  fit$model$gaps <- gaps[chosen]
  fit
}

#' Discretize by entropy-maximizing bin placement
#'
#' Searches a grid of anchored bin widths, each yielding exactly `n_states` bins
#' over the value range, and selects the binning whose state distribution has
#' the largest Shannon entropy (the most balanced allocation). Falls back to
#' robust quantile breaks when no candidate fills every bin.
#'
#' @param values Finite numeric vector.
#' @param n_states Requested number of states.
#' @return A discretization fit list.
#' @keywords internal
#' @noRd
.tsn_discretize_entropy <- function(values, n_states) {
  lo <- min(values)
  hi <- max(values)
  span <- hi - lo
  width_grid <- seq(span / (2 * n_states), span / n_states, length.out = 20L)
  evaluate <- function(width) {
    internal <- lo + width * seq_len(n_states - 1L)
    if (any(internal <= lo) || any(internal >= hi) || anyDuplicated(internal)) {
      return(list(entropy = -Inf, breaks = NULL))
    }
    breaks <- c(-Inf, internal, Inf)
    states <- cut(values, breaks = breaks, labels = FALSE,
                  include.lowest = TRUE, right = TRUE)
    counts <- tabulate(states, nbins = n_states)
    if (any(counts == 0L)) {
      return(list(entropy = -Inf, breaks = breaks))
    }
    probabilities <- counts / sum(counts)
    list(entropy = -sum(probabilities * log(probabilities)), breaks = breaks)
  }
  evaluations <- lapply(width_grid, evaluate)
  entropies <- vapply(evaluations, `[[`, numeric(1L), "entropy")
  best <- which.max(entropies)
  feasible <- is.finite(entropies[best])
  breaks <- if (feasible) {
    evaluations[[best]]$breaks
  } else {
    .tsn_quantile_breaks(values, n_states)
  }
  fit <- .tsn_cut_fit(values, breaks, "entropy", fallback = !feasible)
  fit$model$entropy <- if (feasible) entropies[best] else NA_real_
  fit$model$width_grid <- width_grid
  fit
}

#' Discretize by geometric magnitude breaks
#'
#' Bins absolute magnitudes with geometric (log-spaced) breakpoints, so each
#' state spans a multiplicative rather than additive band of magnitude. Useful
#' when small and large excursions matter on different scales.
#'
#' @param values Finite numeric vector.
#' @param n_states Requested number of states.
#' @return A discretization fit list; boundaries are in magnitude space.
#' @keywords internal
#' @noRd
.tsn_discretize_magnitude <- function(values, n_states) {
  magnitudes <- abs(values)
  positive <- magnitudes[magnitudes > 0]
  if (length(positive) < 1L) {
    stop("`magnitude` discretization requires at least one non-zero value.",
         call. = FALSE)
  }
  lo <- min(positive)
  hi <- max(magnitudes)
  if (hi <= lo) {
    stop("`magnitude` discretization requires a range of magnitudes.",
         call. = FALSE)
  }
  internal <- exp(seq(log(lo), log(hi), length.out = n_states + 1L))[
    seq.int(2L, n_states)
  ]
  fit <- .tsn_cut_fit(magnitudes, c(-Inf, internal, Inf), "magnitude")
  fit$model$magnitude_scale <- internal
  fit
}

#' Discretize by adaptive (rolling-normalized) magnitude
#'
#' Normalizes each observation against a trailing rolling mean and standard
#' deviation (partial windows at the start, undefined spread mapped to zero),
#' then bins the resulting local z-scores with robust quantile breaks. States
#' capture how extreme a point is relative to its recent context.
#'
#' @param values Finite numeric vector.
#' @param n_states Requested number of states.
#' @param window Trailing window width (default 10).
#' @return A discretization fit list.
#' @keywords internal
#' @noRd
.tsn_discretize_adaptive_magnitude <- function(values, n_states, window = 10L) {
  effective_window <- max(2L, min(as.integer(window), length(values)))
  z_scores <- .tsn_rolling_zscore(values, effective_window)
  fit <- .tsn_cut_fit(
    z_scores, .tsn_quantile_breaks(z_scores, n_states), "adaptive_magnitude"
  )
  fit$model$z_scores <- z_scores
  fit$model$window <- effective_window
  fit
}

#' Trailing rolling z-scores with partial start windows
#' @noRd
.tsn_rolling_zscore <- function(values, window) {
  vapply(
    seq_along(values),
    function(i) {
      trailing <- values[seq.int(max(1L, i - window + 1L), i)]
      spread <- if (length(trailing) > 1L) stats::sd(trailing) else 0
      if (!is.finite(spread) || spread <= 0) {
        0
      } else {
        (values[i] - mean(trailing)) / spread
      }
    },
    numeric(1L)
  )
}

#' Discretize by percentile magnitude
#'
#' Bins absolute magnitudes at their own quantiles, giving roughly equal-count
#' magnitude bands. On non-negative series this coincides with the `quantile`
#' method; on signed series it folds sign and ranks by magnitude.
#'
#' @param values Finite numeric vector.
#' @param n_states Requested number of states.
#' @return A discretization fit list; boundaries are in magnitude space.
#' @keywords internal
#' @noRd
.tsn_discretize_percentile_magnitude <- function(values, n_states) {
  magnitudes <- abs(values)
  if (length(unique(magnitudes)) < n_states) {
    stop(
      sprintf("`percentile_magnitude` needs at least %d distinct magnitudes.",
              n_states),
      call. = FALSE
    )
  }
  fit <- .tsn_cut_fit(
    magnitudes, .tsn_quantile_breaks(magnitudes, n_states),
    "percentile_magnitude"
  )
  fit$model$percentiles <- seq(0, 1, length.out = n_states + 1L)
  fit
}

#' Discretize by DTW shape clustering of sliding windows
#'
#' Slides a window across the series, measures dynamic-time-warping distances
#' between windows with the package's own DTW engine, groups the windows into
#' `n_states` shape clusters with a deterministic PAM (partitioning around
#' medoids), and assigns each time point the majority cluster of the windows
#' covering it. The majority fraction is reported as the state probability.
#'
#' @param values Finite numeric vector.
#' @param n_states Requested number of states.
#' @param window Optional sliding-window width; a data-driven default is used
#'   when `NULL`.
#' @return A discretization fit list.
#' @keywords internal
#' @noRd
.tsn_discretize_dtw <- function(values, n_states, window = NULL) {
  n <- length(values)
  effective_window <- if (is.null(window)) max(2L, n %/% 10L) else {
    as.integer(window)
  }
  effective_window <- min(effective_window, n - n_states + 1L)
  if (effective_window < 2L) {
    stop(
      sprintf("`dtw` discretization needs a longer series for %d states.",
              n_states),
      call. = FALSE
    )
  }
  starts <- seq_len(n - effective_window + 1L)
  windows <- lapply(
    starts, function(start) values[seq.int(start, start + effective_window - 1L)]
  )
  distance_matrix <- .tsn_dtw_matrix(windows)
  clustering <- .tsn_pam(distance_matrix, n_states)
  vote <- .tsn_window_vote(clustering$cluster, effective_window, n)
  list(
    state = vote$state,
    probability = vote$confidence,
    model = list(
      window = effective_window,
      n_windows = length(windows),
      medoids = clustering$medoids,
      window_cluster = clustering$cluster
    ),
    breaks = NULL
  )
}

#' Pairwise DTW distance matrix over a list of windows
#' @noRd
.tsn_dtw_matrix <- function(windows) {
  k <- length(windows)
  distance_matrix <- matrix(0, nrow = k, ncol = k)
  if (k < 2L) {
    return(distance_matrix)
  }
  pairs <- utils::combn(k, 2L)
  distances <- vapply(
    seq_len(ncol(pairs)),
    function(index) {
      .tsn_dtw_distance(windows[[pairs[1L, index]]], windows[[pairs[2L, index]]])
    },
    numeric(1L)
  )
  distance_matrix[cbind(pairs[1L, ], pairs[2L, ])] <- distances
  distance_matrix[cbind(pairs[2L, ], pairs[1L, ])] <- distances
  distance_matrix
}

#' Deterministic partitioning around medoids on a distance matrix
#'
#' Standard PAM build then swap phase. Ties are resolved to the smallest index,
#' so the result is fully reproducible without any random start.
#'
#' @param distance_matrix Symmetric non-negative distance matrix.
#' @param k Number of medoids.
#' @return A list with integer `cluster` assignments and `medoids` indices.
#' @keywords internal
#' @noRd
.tsn_pam <- function(distance_matrix, k) {
  n <- nrow(distance_matrix)
  medoids <- which.min(colSums(distance_matrix))
  while (length(medoids) < k) {
    nearest <- apply(distance_matrix[, medoids, drop = FALSE], 1L, min)
    candidates <- setdiff(seq_len(n), medoids)
    gains <- vapply(
      candidates,
      function(candidate) sum(pmax(0, nearest - distance_matrix[, candidate])),
      numeric(1L)
    )
    medoids <- c(medoids, candidates[which.max(gains)])
  }
  cluster_cost <- function(current) {
    sum(apply(distance_matrix[, current, drop = FALSE], 1L, min))
  }
  cost <- cluster_cost(medoids)
  swapping <- TRUE
  while (swapping) {
    combos <- expand.grid(
      slot = seq_len(k),
      candidate = setdiff(seq_len(n), medoids)
    )
    costs <- vapply(
      seq_len(nrow(combos)),
      function(row) {
        trial <- medoids
        trial[combos$slot[row]] <- combos$candidate[row]
        cluster_cost(trial)
      },
      numeric(1L)
    )
    best <- which.min(costs)
    if (length(costs) > 0L && costs[best] < cost - 1e-9) {
      medoids[combos$slot[best]] <- combos$candidate[best]
      cost <- costs[best]
    } else {
      swapping <- FALSE
    }
  }
  cluster <- max.col(-distance_matrix[, medoids, drop = FALSE],
                     ties.method = "first")
  list(cluster = as.integer(cluster), medoids = as.integer(medoids))
}

#' Assign per-point states by majority vote of covering windows
#'
#' @param window_cluster Integer cluster of each sliding window.
#' @param window Window width.
#' @param n Series length.
#' @return A list with integer `state` and numeric `confidence` per point.
#' @noRd
.tsn_window_vote <- function(window_cluster, window, n) {
  n_windows <- length(window_cluster)
  k <- max(window_cluster)
  votes <- lapply(
    seq_len(n),
    function(point) {
      window_cluster[seq.int(max(1L, point - window + 1L), min(point, n_windows))]
    }
  )
  state <- vapply(
    votes, function(vote) which.max(tabulate(vote, nbins = k)), integer(1L)
  )
  confidence <- vapply(
    votes, function(vote) max(tabulate(vote, nbins = k)) / length(vote),
    numeric(1L)
  )
  list(state = as.integer(state), confidence = confidence)
}
