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
#'   endpoint's nearest neighbors.
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
  scaled_kernels <- c("negative_exp", "gaussian")
  .tsn_validate_connection_arguments(
    method = method,
    neighbors = neighbors,
    threshold = threshold,
    percentile = percentile,
    bandwidth = bandwidth,
    node_count = length(unique(c(dyads$from, dyads$to))),
    scaled_similarity = !is.null(similarity) && similarity %in% scaled_kernels
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
    percentile = dyads$distance <= stats::quantile(
      dyads$distance,
      probs = percentile,
      names = FALSE,
      type = 7
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
    exp(-(dyads$distance^2) / (2 * selected_bandwidth^2))
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
    is.data.frame(dyads),
    all(c("from", "to", "distance") %in% names(dyads)),
    is.character(dyads$from), is.character(dyads$to),
    !anyNA(dyads$from), !anyNA(dyads$to),
    all(nzchar(dyads$from)), all(nzchar(dyads$to)),
    is.numeric(dyads$distance),
    !anyNA(dyads$distance), all(is.finite(dyads$distance)),
    all(dyads$distance >= 0),
    all(dyads$from != dyads$to),
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
#' @param method Selected connection method.
#' @param neighbors,threshold,percentile,bandwidth Conditional arguments.
#' @param node_count Number of unique nodes in the dyads.
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
    scaled_similarity = FALSE) {
  supplied <- c(
    nearest = !is.null(neighbors),
    threshold = !is.null(threshold),
    percentile = !is.null(percentile),
    gaussian = !is.null(bandwidth) && !scaled_similarity
  )
  stopifnot(!any(supplied[names(supplied) != method]))
  if (scaled_similarity && !is.null(bandwidth)) {
    stopifnot(
      is.numeric(bandwidth), length(bandwidth) == 1L,
      is.finite(bandwidth), bandwidth > 0
    )
  }
  if (method == "nearest") {
    stopifnot(!is.null(neighbors))
    selected_neighbors <- neighbors
    stopifnot(
      is.numeric(selected_neighbors), length(selected_neighbors) == 1L,
      is.finite(selected_neighbors), selected_neighbors == as.integer(selected_neighbors),
      selected_neighbors >= 1L, selected_neighbors < node_count
    )
  }
  if (method == "threshold") {
    stopifnot(
      is.numeric(threshold), length(threshold) == 1L,
      is.finite(threshold), threshold >= 0
    )
  }
  if (method == "percentile") {
    stopifnot(
      is.numeric(percentile), length(percentile) == 1L,
      is.finite(percentile), percentile >= 0, percentile <= 1
    )
  }
  if (method == "gaussian" && !is.null(bandwidth)) {
    stopifnot(
      is.numeric(bandwidth), length(bandwidth) == 1L,
      is.finite(bandwidth), bandwidth > 0
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
    gaussian = exp(-(distances^2) / (2 * sigma^2))
  )
}
