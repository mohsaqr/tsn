#' Create a validated TSN network object
#'
#' Builds the dual-class `c("tsn", "netobject", "cograph_network")` result. The
#' object is a list that carries the fields the cograph renderer expects
#' (`nodes`, `edges`, `directed`, `weights`, `meta`, `node_groups`) alongside
#' the rich tsn-specific fields (`table`, the tidy 14-column dyad table;
#' `source`, the source series; and `parameters`). `cograph::splot()` reads the
#' cograph fields directly, while [as.data.frame()], [as.matrix()], print, and
#' summary read the tsn fields.
#'
#' @param table The tidy 14-column dyad data frame from `.tsn_finalize_edges()`.
#' @param source The canonical source series data frame.
#' @param nodes Character vector of node labels.
#' @param parameters The parameter list describing the construction.
#' @return A `tsn` network object.
#' @noRd
.new_tsn <- function(table, source, nodes, parameters) {
  required <- c(
    "from", "to", "distance", "weight", "connected", "method", "unit",
    "distance_method", "connection_method", "directed", "from_start",
    "from_end", "to_start", "to_end"
  )
  stopifnot(is.data.frame(table), identical(names(table), required))
  stopifnot(is.character(table$from), is.character(table$to))
  stopifnot(is.numeric(table$distance), is.numeric(table$weight))
  stopifnot(is.logical(table$connected), is.logical(table$directed))
  stopifnot(!anyNA(table$from), !anyNA(table$to))
  stopifnot(all(is.finite(table$distance)), all(is.finite(table$weight)))
  stopifnot(all(table$distance >= 0), all(table$weight >= 0))
  stopifnot(is.data.frame(source), is.character(nodes), !anyDuplicated(nodes))

  directed <- isTRUE(parameters$directed)
  weights <- .tsn_weight_matrix(table, nodes, directed = directed)
  edges <- .tsn_cograph_edges(table, nodes)
  nodes_df <- data.frame(
    id = seq_along(nodes),
    label = nodes,
    name = nodes,
    x = NA_real_,
    y = NA_real_,
    stringsAsFactors = FALSE
  )
  rownames(source) <- NULL

  structure(
    list(
      nodes = nodes_df,
      edges = edges,
      directed = directed,
      weights = weights,
      data = NULL,
      meta = list(source = "tsn", layout = NULL, tna = NULL),
      node_groups = NULL,
      table = table,
      source = source,
      parameters = parameters,
      n_nodes = length(nodes),
      n_edges = nrow(edges)
    ),
    class = c("tsn", "netobject", "cograph_network")
  )
}

#' Build the weighted adjacency matrix from the tidy dyad table
#' @noRd
.tsn_weight_matrix <- function(table, nodes, directed) {
  out <- matrix(0, nrow = length(nodes), ncol = length(nodes),
                dimnames = list(nodes, nodes))
  connected <- table$connected
  if (any(connected)) {
    positions <- cbind(
      match(table$from[connected], nodes),
      match(table$to[connected], nodes)
    )
    out[positions] <- table$weight[connected]
    if (!directed) {
      out[cbind(positions[, 2L], positions[, 1L])] <- table$weight[connected]
    }
  }
  out
}

#' Build the cograph integer edge list from the tidy dyad table
#' @noRd
.tsn_cograph_edges <- function(table, nodes) {
  connected <- table[table$connected, , drop = FALSE]
  data.frame(
    from = match(connected$from, nodes),
    to = match(connected$to, nodes),
    weight = as.numeric(connected$weight),
    stringsAsFactors = FALSE
  )
}

#' Print a TSN network
#'
#' @param x A `tsn` result.
#' @param ... Additional arguments passed to `print.data.frame()`.
#' @return `x`, invisibly.
#' @export
print.tsn <- function(x, ...) {
  parameters <- x$parameters
  cat(sprintf(
    "<tsn> %s %s network: %d nodes, %d connected dyads\n",
    parameters$method,
    parameters$unit,
    x$n_nodes,
    sum(x$table$connected)
  ))
  print.data.frame(utils::head(as.data.frame(x), 10L), row.names = FALSE, ...)
  cat("Use plot(x) for the network or plot(x, \"series\") for the source series.\n")
  cat("With cograph installed, splot(x) renders a publication-quality network.\n")
  invisible(x)
}

#' Summarize a TSN network
#'
#' @param object A `tsn` result.
#' @param ... Ignored.
#' @return A one-row data frame.
#' @export
summary.tsn <- function(object, ...) {
  stopifnot(inherits(object, "tsn"))
  parameters <- object$parameters
  table <- object$table
  connected_weights <- table$weight[table$connected]
  self_loops <- any(table$connected & table$from == table$to)
  n_nodes <- object$n_nodes
  possible <- if (parameters$directed) {
    n_nodes * (n_nodes - 1L + as.integer(self_loops))
  } else {
    choose(n_nodes, 2L) + as.integer(self_loops) * n_nodes
  }
  data.frame(
    method = parameters$method,
    unit = parameters$unit,
    nodes = n_nodes,
    dyads = nrow(table),
    edges = sum(table$connected),
    density = if (possible == 0) 0 else sum(table$connected) / possible,
    minimum_weight = if (length(connected_weights)) min(connected_weights) else NA_real_,
    maximum_weight = if (length(connected_weights)) max(connected_weights) else NA_real_,
    directed = parameters$directed,
    stringsAsFactors = FALSE
  )
}

#' Coerce a TSN result to a data frame
#'
#' @param x A `tsn` result.
#' @param row.names Optional row names.
#' @param optional Ignored.
#' @param what Which table to return: `"edges"` (the stable dyad schema, the
#'   default) or `"series"` (the tidy source time series, including the
#'   discretized `state` column when the network uses states).
#' @param ... Ignored.
#' @return A base data frame: the stable TSN dyad schema when
#'   `what = "edges"`, or the source time series when `what = "series"`.
#' @export
as.data.frame.tsn <- function(x, row.names = NULL, optional = FALSE,
                              what = c("edges", "series"), ...) {
  stopifnot(inherits(x, "tsn"))
  what <- match.arg(what)
  if (what == "series") {
    series <- x$source
    rownames(series) <- NULL
    return(series)
  }
  out <- x$table
  rownames(out) <- NULL
  out
}

#' Coerce a TSN result to a weighted adjacency matrix
#'
#' @param x A `tsn` result.
#' @param rownames.force Ignored.
#' @param ... Ignored.
#' @return A named numeric matrix.
#' @export
as.matrix.tsn <- function(x, rownames.force = NA, ...) {
  x$weights
}
