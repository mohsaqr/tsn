#' Connect Time-Series Distance Dyads
#'
#' Converts distances to similarities and marks connected dyads without
#' changing the number or order of rows.
#'
#' @param dyads A data frame containing character columns `from` and `to` and
#'   a finite non-negative numeric column `distance`.
#' @param method Connection method.
#' @param neighbors Number of neighbors for `method = "nearest"`. Required for
#'   that method and smaller than the number of nodes.
#' @param threshold Maximum distance for `method = "threshold"`.
#' @param percentile Distance quantile in `[0, 1]` for
#'   `method = "percentile"`.
#' @param bandwidth Positive Gaussian bandwidth. By default, uses the median
#'   positive distance.
#' @param directed Whether nearest-neighbor selection respects the stored
#'   `from` to `to` orientation. Undirected selection uses the union of each
#'   endpoint's nearest neighbors. Ties at the k-th distance are broken by the
#'   neighbor label, then row order, so exactly `neighbors` candidates are kept
#'   per node.
#' @param similarity Optional kernel for `.tsn_similarity_weights()`; an
#'   error with `method = "gaussian"`, which sets its own weights.
#'
#' @return `dyads` with numeric `weight` and logical `connected` columns added.
#' @noRd
.tsn_connect <- function(
    dyads,
    method = c("full", "nearest", "threshold", "percentile", "gaussian"),
    neighbors = NULL,
    threshold = NULL,
    percentile = NULL,
    bandwidth = NULL,
    directed = FALSE,
    similarity = NULL) {
  .tsn_validate_dyads(dyads)
  stopifnot(is.logical(directed), length(directed) == 1L, !is.na(directed))
  method <- match.arg(method)
  if (!is.null(similarity)) {
    similarity <- match.arg(
      similarity,
      c("inverse", "normalized_inverse", "negative_exp", "gaussian")
    )
  }
  .tsn_validate_connection_arguments(
    method = method,
    neighbors = neighbors,
    threshold = threshold,
    percentile = percentile,
    bandwidth = bandwidth,
    node_count = length(unique(c(dyads$from, dyads$to))),
    similarity = similarity
  )

  connected <- switch(
    method,
    full = rep(TRUE, nrow(dyads)),
    nearest = .tsn_nearest_connections(
      dyads,
      neighbors = as.integer(neighbors),
      directed = directed
    ),
    threshold = dyads$distance <= threshold,
    percentile = dyads$distance <= .tsn_percentile_cutoff(
      dyads$distance,
      percentile = percentile
    ),
    gaussian = rep(TRUE, nrow(dyads))
  )
  weights <- if (method == "gaussian" && is.null(similarity)) {
    selected_bandwidth <- if (is.null(bandwidth)) {
      positive <- dyads$distance[dyads$distance > 0]
      if (length(positive) > 0L) stats::median(positive) else 1
    } else {
      bandwidth
    }
    # Scaling before squaring keeps a tiny bandwidth from underflowing
    # `bandwidth^2` to zero, which would turn a zero distance into 0 / 0.
    exp(-0.5 * (dyads$distance / selected_bandwidth)^2)
  } else {
    kernel <- if (is.null(similarity)) "inverse" else similarity
    ifelse(
      connected,
      .tsn_similarity_weights(
        dyads$distance,
        kernel = kernel,
        sigma = bandwidth
      ),
      0
    )
  }
  # The exponential kernels are strictly positive in exact arithmetic, so a
  # connected weight of exactly zero is an underflow: the edge would be
  # indistinguishable from an absent one in the adjacency matrix. The dyad
  # stays connected (the rule decides connection, the kernel only weights it),
  # but the caller is told. `normalized_inverse` reaches zero by definition at
  # the maximum distance and is not an underflow.
  exponential <- (method == "gaussian" && is.null(similarity)) ||
    isTRUE(similarity %in% c("negative_exp", "gaussian"))
  underflow <- exponential & connected & !(weights > 0)
  if (any(underflow)) {
    warning(warningCondition(
      sprintf(
        paste0(
          "%d connected dyad(s) have a kernel weight that underflows to 0; ",
          "the bandwidth is too small for these distances."
        ),
        sum(underflow)
      ),
      class = "tsn_weight_underflow",
      call = NULL
    ))
  }
  output <- dyads
  output$weight <- as.numeric(weights)
  output$connected <- as.logical(connected)
  stopifnot(
    nrow(output) == nrow(dyads),
    all(is.finite(output$weight)),
    all(output$weight >= 0 & output$weight <= 1),
    !anyNA(output$connected)
  )
  output
}

#' Validate Distance Dyads
#'
#' @param dyads A prospective tidy distance table.
#'
#' @return `NULL`, invisibly.
#' @noRd
.tsn_validate_dyads <- function(dyads) {
  stopifnot(
    "Dyads must be a data frame with `from`, `to`, and `distance` columns." =
      is.data.frame(dyads) &&
      all(c("from", "to", "distance") %in% names(dyads)),
    "Dyad endpoints must be non-missing, non-empty character labels." =
      is.character(dyads$from) && is.character(dyads$to) &&
      !anyNA(dyads$from) && !anyNA(dyads$to) &&
      all(nzchar(dyads$from)) && all(nzchar(dyads$to)),
    "Dyad distances must be finite and non-negative." =
      is.numeric(dyads$distance) && all(is.finite(dyads$distance)) &&
      all(dyads$distance >= 0),
    "Dyads must not contain self-pairs." = all(dyads$from != dyads$to),
    "Each unordered pair of nodes must appear in at most one dyad." =
      !anyDuplicated(paste(
        pmin(dyads$from, dyads$to),
        pmax(dyads$from, dyads$to),
        sep = "\r"
      ))
  )
  invisible(NULL)
}

#' Validate Connection Arguments
#'
#' Every conditional argument must belong to the selected connection rule or
#' similarity kernel; a supplied argument the configuration cannot use is an
#' error rather than being ignored.
#'
#' @param method Selected connection method.
#' @param neighbors,threshold,percentile,bandwidth Conditional arguments.
#' @param node_count Number of unique nodes in the dyads.
#' @param similarity Optional similarity kernel name.
#'
#' @return `NULL`, invisibly.
#' @noRd
.tsn_validate_connection_arguments <- function(
    method,
    neighbors,
    threshold,
    percentile,
    bandwidth,
    node_count,
    similarity = NULL) {
  scaled_similarity <- !is.null(similarity) &&
    similarity %in% c("negative_exp", "gaussian")
  # `connect = "gaussian"` is itself a weighting rule; a kernel would replace
  # it and make the request indistinguishable from `connect = "full"`. This
  # matches the public check in `.tsn_validate_argument_use()`.
  if (identical(method, "gaussian") && !is.null(similarity)) {
    stop(sprintf(
      paste0(
        "`connect = \"gaussian\"` already sets Gaussian weights; use ",
        "`connect = \"full\"` with `similarity = \"%s\"` instead."
      ),
      similarity
    ), call. = FALSE)
  }
  supplied <- c(
    neighbors = !is.null(neighbors),
    threshold = !is.null(threshold),
    percentile = !is.null(percentile),
    bandwidth = !is.null(bandwidth)
  )
  usable <- c(
    neighbors = method == "nearest",
    threshold = method == "threshold",
    percentile = method == "percentile",
    bandwidth = method == "gaussian" || scaled_similarity
  )
  unused <- names(supplied)[supplied & !usable]
  if (length(unused) > 0L) {
    stop(sprintf(
      paste0(
        "`%s` cannot be used with `connect = \"%s\"`; `neighbors` belongs ",
        "to \"nearest\", `threshold` to \"threshold\", `percentile` to ",
        "\"percentile\", and `bandwidth` to \"gaussian\" or the ",
        "\"negative_exp\"/\"gaussian\" similarity kernels."
      ),
      unused[1L], method
    ), call. = FALSE)
  }
  if (!is.null(bandwidth)) {
    stopifnot(
      "`bandwidth` must be a single positive finite number." =
        is.numeric(bandwidth) && length(bandwidth) == 1L &&
        is.finite(bandwidth) && bandwidth > 0
    )
  }
  if (method == "nearest") {
    stopifnot(
      "`neighbors` is required when `connect = \"nearest\"`." =
        !is.null(neighbors),
      "`neighbors` must be a single positive whole number." =
        is.numeric(neighbors) && length(neighbors) == 1L &&
        is.finite(neighbors) && neighbors >= 1 &&
        neighbors == round(neighbors)
    )
    if (neighbors >= node_count) {
      stop(sprintf(
        "`neighbors` must be smaller than the number of nodes (%d).",
        node_count
      ), call. = FALSE)
    }
  }
  if (method == "threshold") {
    stopifnot(
      "`threshold` must be a single non-negative finite number when `connect = \"threshold\"`." =
        is.numeric(threshold) && length(threshold) == 1L &&
        is.finite(threshold) && threshold >= 0
    )
  }
  if (method == "percentile") {
    stopifnot(
      "`percentile` must be a single proportion in [0, 1] when `connect = \"percentile\"`." =
        is.numeric(percentile) && length(percentile) == 1L &&
        is.finite(percentile) && percentile >= 0 && percentile <= 1
    )
  }
  invisible(NULL)
}

#' Select Nearest-Neighbor Connections
#'
#' @param dyads A validated tidy distance table.
#' @param neighbors Positive integer number of neighbors.
#' @param directed Whether to use only stored `from` to `to` orientations.
#'
#' @return A logical vector aligned with `dyads`.
#' @noRd
.tsn_nearest_connections <- function(dyads, neighbors, directed) {
  row_ids <- seq_len(nrow(dyads))
  candidates <- if (directed) {
    data.frame(
      row_id = row_ids,
      node = dyads$from,
      neighbor = dyads$to,
      distance = dyads$distance,
      stringsAsFactors = FALSE
    )
  } else {
    rbind(
      data.frame(
        row_id = row_ids,
        node = dyads$from,
        neighbor = dyads$to,
        distance = dyads$distance,
        stringsAsFactors = FALSE
      ),
      data.frame(
        row_id = row_ids,
        node = dyads$to,
        neighbor = dyads$from,
        distance = dyads$distance,
        stringsAsFactors = FALSE
      )
    )
  }
  by_node <- split(candidates, candidates$node, drop = TRUE)
  selected_rows <- unique(unlist(
    lapply(
      by_node,
      function(candidate) {
        ordered <- order(
          candidate$distance,
          candidate$neighbor,
          candidate$row_id,
          method = "radix"
        )
        candidate$row_id[utils::head(ordered, neighbors)]
      }
    ),
    use.names = FALSE
  ))
  row_ids %in% selected_rows
}

#' Convert Distances to Similarity Weights
#'
#' Maps a non-negative distance vector into `[0, 1]` similarity weights with
#' the selected kernel. `normalized_inverse` rescales by the maximum distance
#' (the legacy `max_minus` kernel is algebraically identical and therefore not
#' offered separately); `negative_exp` and `gaussian` use `sigma`, defaulting
#' to the median positive distance.
#'
#' @param distances Non-negative finite numeric vector.
#' @param kernel One of `"inverse"`, `"normalized_inverse"`, `"negative_exp"`,
#'   or `"gaussian"`.
#' @param sigma Positive kernel scale, or `NULL` for the data-driven default.
#'
#' @return A numeric vector of weights in `[0, 1]`.
#' @noRd
.tsn_similarity_weights <- function(
    distances,
    kernel = c("inverse", "normalized_inverse", "negative_exp", "gaussian"),
    sigma = NULL) {
  kernel <- match.arg(kernel)
  stopifnot(is.numeric(distances), all(is.finite(distances)), all(distances >= 0))
  if (kernel %in% c("negative_exp", "gaussian") && is.null(sigma)) {
    positive <- distances[distances > 0]
    sigma <- if (length(positive) > 0L) stats::median(positive) else 1
  }
  switch(
    kernel,
    inverse = 1 / (1 + distances),
    normalized_inverse = {
      maximum <- max(distances)
      if (maximum == 0) rep(1, length(distances)) else 1 - distances / maximum
    },
    negative_exp = exp(-distances / sigma),
    gaussian = exp(-0.5 * (distances / sigma)^2)
  )
}

#' Percentile Distance Cutoff
#'
#' The type-7 sample quantile of `distances`. When `1 + (n - 1) * percentile`
#' is an order-statistic index up to floating-point fuzz, that order statistic
#' is returned exactly: `stats::quantile()` applies no fuzz for type 7, so for
#' example `percentile = 0.58` over 101 distances computes the index
#' 58.99999999999999 and interpolates to just below the 59th smallest
#' distance, dropping it from a `distance <= cutoff` rule.
#'
#' @param distances Non-negative finite numeric vector.
#' @param percentile Single proportion in `[0, 1]`.
#' @return A single numeric cutoff (`NA` for no distances).
#' @noRd
.tsn_percentile_cutoff <- function(distances, percentile) {
  stopifnot(
    is.numeric(distances), all(is.finite(distances)),
    is.numeric(percentile), length(percentile) == 1L,
    percentile >= 0, percentile <= 1
  )
  count <- length(distances)
  if (count == 0L) {
    return(NA_real_)
  }
  index <- 1 + (count - 1) * percentile
  nearest <- round(index)
  # The same relative fuzz `stats::quantile()` uses for types 4-9.
  if (abs(index - nearest) <= 4 * .Machine$double.eps * index) {
    return(sort(distances, method = "radix")[nearest])
  }
  stats::quantile(distances, probs = percentile, names = FALSE, type = 7)
}
