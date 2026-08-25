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
#' @return A one-row data frame with columns `method`, `unit`, `nodes`,
#'   `dyads`, `edges`, `density`, `minimum_weight`, `maximum_weight`, and
#'   `directed`. `density` divides the connected edges by the number of
#'   possible edges for the network family: state networks
#'   (`unit = "state"`) always count self-loops among the possible edges,
#'   because a state can follow itself, while series, window, and time-point
#'   networks never do, because their units cannot pair with themselves. The
#'   denominator therefore depends only on the network type, never on which
#'   edges happen to be observed, so densities are comparable across models
#'   of the same family.
#' @export
summary.tsn <- function(object, ...) {
  stopifnot(inherits(object, "tsn"))
  parameters <- object$parameters
  table <- object$table
  connected_weights <- table$weight[table$connected]
  # Loops are part of the possible-edge universe exactly when the network
  # family permits them (a state can follow itself); whether any loop was
  # observed must not change the denominator, or densities stop being
  # comparable between models of the same family.
  loops_possible <- identical(parameters$unit, "state")
  n_nodes <- object$n_nodes
  possible <- if (parameters$directed) {
    n_nodes * (n_nodes - 1L) + if (loops_possible) n_nodes else 0L
  } else {
    choose(n_nodes, 2L) + if (loops_possible) n_nodes else 0L
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
#' @param connected Return only the retained edges? The edge table lists every
#'   evaluated dyad; `TRUE` keeps the rows whose `connected` flag is set, i.e.
#'   the edges that survived the `connect` rule. Ignored when
#'   `what = "series"`.
#' @param ... Ignored.
#' @return A base data frame: the stable TSN dyad schema when
#'   `what = "edges"`, or the source time series when `what = "series"`.
#' @export
as.data.frame.tsn <- function(x, row.names = NULL, optional = FALSE,
                              what = c("edges", "series"), connected = FALSE,
                              ...) {
  stopifnot(
    inherits(x, "tsn"),
    is.logical(connected),
    length(connected) == 1L,
    !is.na(connected)
  )
  what <- match.arg(what)
  if (what == "series") {
    series <- x$source
    rownames(series) <- NULL
    return(series)
  }
  out <- x$table
  if (connected) {
    out <- out[out$connected, , drop = FALSE]
  }
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

#' Coerce a transition network to a weighted adjacency matrix
#'
#' The state-to-state weight matrix behind a [ts_tna()]-family model: transition
#' probabilities for `ts_tna()`, counts for `ts_ftna()`, and so on. Without this
#' method `as.matrix()` falls through to the default, which coerces the object's
#' component list rather than its network.
#'
#' @param x A `ts_tna` result.
#' @param rownames.force Ignored.
#' @param ... Ignored.
#' @return A named numeric matrix, states in row and column order.
#' @examplesIf requireNamespace("Nestimate", quietly = TRUE)
#' set.seed(1)
#' network <- ts_tna(cumsum(rnorm(60)), labels = c("low", "mid", "high"))
#' as.matrix(network)
#' @export
as.matrix.ts_tna <- function(x, rownames.force = NA, ...) {
  stopifnot(inherits(x, "ts_tna"))
  x$weights
}

#' Coerce a transition network to a tidy edge table
#'
#' Returns one row per state pair, so a transition network can be read, sorted,
#' or joined as data rather than reached into. `as.data.frame()` on the
#' underlying Nestimate object errors, so this method supplies the tidy view.
#'
#' @param x A `ts_tna` result.
#' @param row.names Optional row names.
#' @param optional Ignored.
#' @param what Which table to return: `"edges"` (one row per state pair, the
#'   default) or `"series"` (the tidy per-observation source table with `id`,
#'   `time`, `value`, and `state`).
#' @param ... Ignored.
#' @return A base data frame.
#' @examplesIf requireNamespace("Nestimate", quietly = TRUE)
#' set.seed(1)
#' network <- ts_tna(cumsum(rnorm(60)), labels = c("low", "mid", "high"))
#' as.data.frame(network)
#' head(as.data.frame(network, what = "series"))
#' @export
as.data.frame.ts_tna <- function(x, row.names = NULL, optional = FALSE,
                                 what = c("edges", "series"), ...) {
  stopifnot(inherits(x, "ts_tna"))
  what <- match.arg(what)
  if (what == "series") {
    series <- x$ts_source
    rownames(series) <- NULL
    return(series)
  }
  weights <- x$weights
  states <- rownames(weights)
  out <- data.frame(
    from = factor(rep(states, times = ncol(weights)), levels = states),
    to = factor(rep(colnames(weights), each = nrow(weights)), levels = states),
    weight = as.vector(weights),
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

#' Coerce a grouped transition network to a tidy data frame
#'
#' A grouped model is a collection of networks, one per group. This method is
#' the tidy view of it: every table it returns carries a `group` column, so a
#' comparison across groups is a data frame you can read, sort, or join rather
#' than a set of objects you have to reach into one at a time.
#'
#' @param x A `ts_tna_group` result from [ts_tna()] and friends, built with
#'   their `group` argument.
#' @param row.names Optional row names.
#' @param optional Ignored.
#' @param what Which table to return: `"edges"` (one row per group and state
#'   pair, the default), `"series"` (the per-observation source table for every
#'   group), or `"groups"` (a one-row-per-group index of how much data backs
#'   each network).
#' @param ... Ignored.
#' @return A base data frame whose first column is `group`.
#' @seealso [ts_tna()] for the `group` argument that builds these models.
#' @examplesIf requireNamespace("Nestimate", quietly = TRUE)
#' data(motivation)
#' networks <- ts_tna(
#'   motivation,
#'   series = "pleasure", group = "task_context_type",
#'   labels = c("low", "mid", "high")
#' )
#' as.data.frame(networks, what = "groups")
#' head(as.data.frame(networks))
#' @export
as.data.frame.ts_tna_group <- function(x, row.names = NULL, optional = FALSE,
                                       what = c("edges", "series", "groups"),
                                       ...) {
  stopifnot(inherits(x, "ts_tna_group"), !is.null(names(x)))
  what <- match.arg(what)
  rows <- Map(
    function(network, key) {
      part <- switch(
        what,
        edges = as.data.frame(network),
        series = as.data.frame(network, what = "series"),
        groups = data.frame(
          type = network$meta$tsn$type,
          sequences = nrow(network$data),
          observations = nrow(network$ts_source),
          states = nrow(network$nodes),
          edges = nrow(network$edges),
          stringsAsFactors = FALSE
        )
      )
      cbind(
        group = factor(key, levels = names(x)),
        part,
        stringsAsFactors = FALSE
      )
    },
    x,
    names(x)
  )
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
