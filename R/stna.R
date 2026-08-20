#' Transition Network Analysis of Time Series
#'
#' `ts_tna()`, `ts_ftna()`, `ts_cna()`, and `ts_atna()` bridge tsn's
#' discretization engine to the Nestimate package: a numeric time series (or
#' several) is discretized into states with any of tsn's discretizers, each
#' series becomes one state sequence, and Nestimate builds the transition
#' network — row-normalized probabilities (`ts_tna`), raw transition counts
#' (`ts_ftna`), co-occurrence counts (`ts_cna`), or attention-weighted
#' transitions (`ts_atna`).
#'
#' The result is a full Nestimate `netobject` (also a `cograph_network`), so
#' compatible Nestimate descriptive and inferential verbs apply and
#' `cograph::splot()` renders it directly. Inference still requires the sample
#' its method assumes: for example, sequence bootstrap is degenerate for a
#' single sequence. The object keeps its data: `$data` holds the wide state
#' sequences Nestimate built from, `$ts_source` the tidy per-observation table
#' (`id`, `time`, `value`, `state`), and `$meta$tsn` the discretization and
#' builder settings, so the network remains traceable back to the raw series.
#'
#' A single series supports every descriptive verb but no sequence-based test,
#' because a bootstrap resamples sequences and one series is one sequence. The
#' `segment` argument cuts a long series into blocks so that those tests have
#' units to work with; see its documentation for the trade-off between
#' partitioned and sliding blocks.
#'
#' Multiple series are supported through every tsn input form (named list,
#' matrix, wide or long data frame). Scalar discretizers learn from pooled
#' values. Temporal discretizers compute patterns or windows separately within
#' each series before assigning one shared state alphabet. Each series
#' contributes one sequence. Passing an existing [discretize()] result skips
#' discretization and uses its states as-is.
#'
#' @param data A numeric vector, `ts`, matrix, named list of numeric vectors,
#'   data frame, or a `tsn_states` result from [discretize()].
#' @param value Optional value-column name for long data.
#' @param id Optional series-ID column name for long data.
#' @param time Optional time-column name for long data.
#' @param series Optional series IDs or wide-data column names to select.
#' @param discretization Discretization method passed to [discretize()]
#'   (default `"quantile"`). Ignored when `data` is already a `tsn_states`.
#' @param n_states Number of states (default `3`).
#' @param breaks Optional interior thresholds for
#'   `discretization = "threshold"`.
#' @param labels Optional state labels; these become the network's node
#'   names (e.g. `c("low", "mid", "high")`).
#' @param transform Pre-discretization transform: `"none"`, `"log"`, or
#'   `"zscore"`.
#' @param m,tau Embedding arguments for `discretization = "ordinal"`.
#' @param segment Optional block width, in observations, used to cut each series
#'   into several shorter sequences. Sequence-based inference resamples whole
#'   sequences, so a single long series offers nothing to resample; segmenting
#'   supplies the units. Blocks never span an ID boundary, and segmentation is
#'   applied *after* discretization, so the state alphabet is still learned from
#'   the whole series. At least `2`.
#' @param overlap When segmenting, slide the block one observation at a time
#'   instead of partitioning (default `FALSE`). Partitioning loses the
#'   transition at every cut, roughly one per block. Sliding keeps every
#'   transition — at `segment = 2` the blocks are the consecutive lag-1 pairs
#'   and the network is identical to the unsegmented one — but the blocks share
#'   observations, so they are not independent and intervals computed from them
#'   run narrow. Partition for conservative intervals that tolerate dependence
#'   beyond one lag; slide to preserve the estimate exactly.
#' @param seed Optional seed used by stochastic discretizers.
#' @param ... Passed on to the corresponding Nestimate builder
#'   (`Nestimate::build_tna()` and friends), e.g. `start`, `end`,
#'   `scaling`, `threshold`.
#' @return A Nestimate `netobject` of class
#'   `c("ts_tna", "netobject", "cograph_network")` with the additional fields
#'   `$ts_source` (tidy `id`/`time`/`value`/`state` table) and `$meta$tsn`
#'   (discretization settings).
#' @examplesIf requireNamespace("Nestimate", quietly = TRUE)
#' set.seed(1)
#' series <- list(
#'   a = cumsum(rnorm(60)),
#'   b = cumsum(rnorm(60)),
#'   c = cumsum(rnorm(60))
#' )
#' network <- ts_tna(series, n_states = 3, labels = c("low", "mid", "high"))
#' network$weights
#'
#' # Frequency counts instead of probabilities:
#' counts <- ts_ftna(series, n_states = 3)
#'
#' # Reuse an existing discretization:
#' states <- discretize(series, method = "kmeans", n_states = 3)
#' ts_tna(states)
#'
#' # Cut one long series into blocks so sequence-based tests have units:
#' long <- cumsum(rnorm(300))
#' ts_tna(long, segment = 10, labels = c("low", "mid", "high"))
#'
#' # Sliding lag-1 pairs keep every transition and the exact estimate:
#' ts_tna(long, segment = 2, overlap = TRUE, labels = c("low", "mid", "high"))
#' @export
ts_tna <- function(data, value = NULL, id = NULL, time = NULL, series = NULL,
                   discretization = "quantile", n_states = 3L, breaks = NULL,
                   labels = NULL, transform = "none", m = NULL, tau = NULL,
                   segment = NULL, overlap = FALSE, seed = NULL, ...) {
  .tsn_build_stna(
    data, type = "tna", value = value, id = id, time = time, series = series,
    discretization = discretization, n_states = n_states, breaks = breaks,
    labels = labels, transform = transform, m = m, tau = tau,
    segment = segment, overlap = overlap, seed = seed, ...
  )
}

#' @rdname ts_tna
#' @export
ts_ftna <- function(data, value = NULL, id = NULL, time = NULL, series = NULL,
                    discretization = "quantile", n_states = 3L, breaks = NULL,
                    labels = NULL, transform = "none", m = NULL, tau = NULL,
                    segment = NULL, overlap = FALSE, seed = NULL, ...) {
  .tsn_build_stna(
    data, type = "ftna", value = value, id = id, time = time, series = series,
    discretization = discretization, n_states = n_states, breaks = breaks,
    labels = labels, transform = transform, m = m, tau = tau,
    segment = segment, overlap = overlap, seed = seed, ...
  )
}

#' @rdname ts_tna
#' @export
ts_cna <- function(data, value = NULL, id = NULL, time = NULL, series = NULL,
                   discretization = "quantile", n_states = 3L, breaks = NULL,
                   labels = NULL, transform = "none", m = NULL, tau = NULL,
                   segment = NULL, overlap = FALSE, seed = NULL, ...) {
  .tsn_build_stna(
    data, type = "cna", value = value, id = id, time = time, series = series,
    discretization = discretization, n_states = n_states, breaks = breaks,
    labels = labels, transform = transform, m = m, tau = tau,
    segment = segment, overlap = overlap, seed = seed, ...
  )
}

#' @rdname ts_tna
#' @export
ts_atna <- function(data, value = NULL, id = NULL, time = NULL, series = NULL,
                    discretization = "quantile", n_states = 3L, breaks = NULL,
                    labels = NULL, transform = "none", m = NULL, tau = NULL,
                    segment = NULL, overlap = FALSE, seed = NULL, ...) {
  .tsn_build_stna(
    data, type = "atna", value = value, id = id, time = time, series = series,
    discretization = discretization, n_states = n_states, breaks = breaks,
    labels = labels, transform = transform, m = m, tau = tau,
    segment = segment, overlap = overlap, seed = seed, ...
  )
}

#' Shared Engine for the ts_*na Verbs
#'
#' Discretizes (unless given a `tsn_states`), reshapes each series into one
#' wide state sequence, delegates to the requested Nestimate builder, and
#' attaches the tsn provenance fields.
#'
#' @param data Public input.
#' @param type One of `"tna"`, `"ftna"`, `"cna"`, `"atna"`.
#' @inheritParams ts_tna
#' @return A Nestimate `netobject`.
#' @noRd
.tsn_build_stna <- function(data, type, value, id, time, series,
                            discretization, n_states, breaks, labels,
                            transform, m, tau, segment = NULL,
                            overlap = FALSE, seed, ...) {
  if (!requireNamespace("Nestimate", quietly = TRUE)) {
    stop(
      sprintf("`ts_%s()` requires the Nestimate package. ", type),
      "Install it with install.packages(\"Nestimate\").",
      call. = FALSE
    )
  }
  states <- if (inherits(data, "tsn_states")) {
    .tsn_select_canonical(data, series = series)
  } else {
    discretize(
      data, value = value, id = id, time = time, series = series,
      method = discretization, n_states = n_states, breaks = breaks,
      labels = labels, transform = transform, m = m, tau = tau, seed = seed
    )
  }
  source <- .tsn_transition_source(states)
  # Segmentation happens after discretization on purpose: the state alphabet is
  # learned from the whole series, so cutting it into blocks changes the unit of
  # resampling without moving the cut points underneath it.
  if (!is.null(segment)) {
    source <- .tsn_segment_source(source, segment = segment, overlap = overlap)
  }
  sequences <- .tsn_state_sequences(source)
  builder <- switch(
    type,
    tna = Nestimate::build_tna,
    ftna = Nestimate::build_ftna,
    cna = Nestimate::build_cna,
    atna = Nestimate::build_atna
  )
  # Fix the node set to the state alphabet in level order, so nodes come
  # out low/mid/high (not alphabetical) and match series_networks() splits.
  dots <- list(...)
  params <- if (is.null(dots$params)) list() else dots$params
  if (is.null(params$alphabet)) {
    params$alphabet <- levels(source$state)
  }
  dots$params <- params
  network <- do.call(builder, c(list(sequences), dots))
  network$ts_source <- source
  network$meta$tsn <- list(
    type = type,
    discretization = attr(states, "parameters")$method,
    transform = attr(states, "parameters")$transform,
    n_states = attr(states, "parameters")$n_states,
    breaks = attr(states, "breaks"),
    segment = segment,
    overlap = overlap,
    series = unique(source$id),
    builder_args = dots
  )
  class(network) <- c("ts_tna", class(network))
  network
}

#' Cut Each Series into Shorter Sequences
#'
#' Sequence-based inference (bootstrap, permutation, stability) resamples whole
#' sequences, so a single long series gives it nothing to resample. Splitting
#' that series into blocks supplies the missing units. Blocks are cut within
#' each series and never across an ID boundary.
#'
#' Two schemes, with different costs:
#'
#' * Non-overlapping (`overlap = FALSE`) partitions the series into consecutive
#'   blocks. Each cut destroys the transition straddling it, so `n` observations
#'   in blocks of `segment` yield about `n / segment` sequences and lose about
#'   the same number of transitions.
#' * Overlapping (`overlap = TRUE`) slides a window of width `segment` one
#'   observation at a time, giving `n - segment + 1` blocks and keeping every
#'   transition. At `segment = 2` the blocks are the consecutive lag-1 pairs and
#'   the estimated network is identical to the unsegmented one. The blocks share
#'   observations, so they are not independent and intervals derived from them
#'   run narrow.
#'
#' @param source A tidy `id`/`time`/`value`/`state` table.
#' @param segment Block width in observations; at least 2, since a block of one
#'   observation contains no transition.
#' @param overlap Slide the block by one observation instead of partitioning.
#' @return The same table with `id` replaced by `<id>.<block>`.
#' @noRd
.tsn_segment_source <- function(source, segment, overlap = FALSE) {
  stopifnot(
    is.numeric(segment), length(segment) == 1L, !is.na(segment),
    segment >= 2, segment == as.integer(segment),
    is.logical(overlap), length(overlap) == 1L, !is.na(overlap)
  )
  segment <- as.integer(segment)
  groups <- split(source, factor(source$id, levels = unique(source$id)))
  blocks <- lapply(names(groups), function(key) {
    group <- groups[[key]]
    n <- nrow(group)
    if (n < 2L) {
      return(NULL)
    }
    starts <- if (overlap) {
      seq_len(max(n - segment + 1L, 1L))
    } else {
      seq.int(1L, n, by = segment)
    }
    pieces <- lapply(seq_along(starts), function(index) {
      rows <- seq.int(starts[index], min(starts[index] + segment - 1L, n))
      # A trailing partial block of one observation carries no transition.
      if (length(rows) < 2L) {
        return(NULL)
      }
      piece <- group[rows, , drop = FALSE]
      piece$id <- paste0(key, ".", index)
      piece
    })
    do.call(rbind, pieces[!vapply(pieces, is.null, logical(1L))])
  })
  blocks <- blocks[!vapply(blocks, is.null, logical(1L))]
  if (!length(blocks)) {
    stop(
      "`segment` left no sequence with at least two observations.",
      call. = FALSE
    )
  }
  segmented <- do.call(rbind, blocks)
  row.names(segmented) <- NULL
  segmented
}

#' Prepare Discretized States for Transition Counting
#'
#' Ordinal discretization labels trailing observations for aligned plotting,
#' but those fills do not start complete embedding windows. Remove them before
#' building a transition sequence so only observed ordinal patterns contribute
#' transitions.
#'
#' @param states A `tsn_states` result.
#' @return A tidy state data frame suitable for Nestimate.
#' @noRd
.tsn_transition_source <- function(states) {
  stopifnot(inherits(states, "tsn_states"))
  source <- data.frame(
    id = as.character(states$id),
    time = states$time,
    value = states$value,
    state = factor(as.character(states$state), levels = levels(states$state)),
    stringsAsFactors = FALSE
  )
  parameters <- attr(states, "parameters")
  if (!identical(parameters$method, "ordinal")) {
    return(source)
  }
  model <- attr(states, "model")
  stopifnot(
    is.list(model), is.numeric(model$m), length(model$m) == 1L,
    is.numeric(model$tau), length(model$tau) == 1L
  )
  trailing <- (as.integer(model$m) - 1L) * as.integer(model$tau)
  groups <- split(
    source,
    factor(source$id, levels = unique(source$id)),
    drop = TRUE
  )
  trimmed <- lapply(groups, function(group) {
    group[seq_len(nrow(group) - trailing), , drop = FALSE]
  })
  source <- do.call(rbind, trimmed)
  rownames(source) <- NULL
  source
}

#' Reshape Tidy States into Wide Sequence Data
#'
#' One row per series in first-appearance order, one column per time step,
#' shorter series padded with `NA`.
#'
#' @param source Tidy data frame with `id` and `state` in temporal order.
#' @return A character data frame of sequences.
#' @noRd
.tsn_state_sequences <- function(source) {
  ids <- unique(source$id)
  by_id <- split(as.character(source$state), source$id)[ids]
  max_length <- max(lengths(by_id))
  padded <- lapply(by_id, function(sequence) {
    c(sequence, rep(NA_character_, max_length - length(sequence)))
  })
  sequences <- as.data.frame(
    do.call(rbind, padded),
    stringsAsFactors = FALSE
  )
  names(sequences) <- paste0("T", seq_len(max_length))
  rownames(sequences) <- NULL
  sequences
}

#' Per-Series Transition Networks
#'
#' Split a pooled `ts_tna()`-family model into one full model per source
#' series. Each element is rebuilt with the same network type and the
#' shared state alphabet (`params = list(alphabet = ...)`), so every
#' network has the same node set in the same order — directly comparable
#' with each other and with the pooled summary model. Each element is a
#' complete `ts_tna` netobject: it keeps its own series data, plots with
#' [plot.ts_tna()], and works with compatible Nestimate verbs. Statistical
#' procedures still require enough independent sequences for their sampling
#' design; a one-series split is descriptive rather than bootstrap-ready.
#'
#' @param x A `ts_tna` result from [ts_tna()], [ts_ftna()], [ts_cna()], or
#'   [ts_atna()].
#' @param series Optional series IDs to keep (default: all).
#' @return A named `tsn_series_networks` collection containing one `ts_tna`
#'   object per series. Its print, summary, and data-frame methods return a tidy
#'   one-row-per-series index; `plot()` draws the sole model or a model selected
#'   with its `series` argument.
#' @examplesIf requireNamespace("Nestimate", quietly = TRUE) && requireNamespace("cograph", quietly = TRUE)
#' set.seed(1)
#' series <- list(a = cumsum(rnorm(80)), b = cumsum(rnorm(80)))
#' pooled <- ts_tna(series, n_states = 3, labels = c("low", "mid", "high"))
#' networks <- series_networks(pooled)
#' plot(networks, series = "a")
#' @export
series_networks <- function(x, series = NULL) {
  stopifnot(inherits(x, "ts_tna"))
  if (!requireNamespace("Nestimate", quietly = TRUE)) {
    stop("series_networks() requires the Nestimate package.", call. = FALSE)
  }
  source <- x$ts_source
  available <- unique(source$id)
  selected <- if (is.null(series)) available else unique(as.character(series))
  unknown <- setdiff(selected, available)
  if (length(unknown) > 0L) {
    stop(sprintf("Unknown series: %s.", paste(unknown, collapse = ", ")),
         call. = FALSE)
  }
  alphabet <- levels(source$state)
  builder <- switch(
    x$meta$tsn$type,
    tna = Nestimate::build_tna,
    ftna = Nestimate::build_ftna,
    cna = Nestimate::build_cna,
    atna = Nestimate::build_atna
  )
  networks <- lapply(selected, function(series_id) {
    one <- source[source$id == series_id, , drop = FALSE]
    builder_args <- x$meta$tsn$builder_args
    if (is.null(builder_args)) {
      builder_args <- list()
    }
    params <- if (is.null(builder_args$params)) list() else builder_args$params
    params$alphabet <- alphabet
    builder_args$params <- params
    network <- do.call(
      builder,
      c(list(data = .tsn_state_sequences(one)), builder_args)
    )
    network$ts_source <- one
    network$meta$tsn <- x$meta$tsn
    network$meta$tsn$series <- series_id
    class(network) <- c("ts_tna", class(network))
    network
  })
  names(networks) <- selected
  structure(networks, class = c("tsn_series_networks", "list"))
}

#' Summarize Per-Series Transition Networks
#'
#' @param object A `tsn_series_networks` result from [series_networks()].
#' @param ... Ignored.
#' @return A tidy data frame with one row per series.
#' @export
summary.tsn_series_networks <- function(object, ...) {
  stopifnot(
    inherits(object, "tsn_series_networks"),
    is.list(object),
    !is.null(names(object)),
    !anyDuplicated(names(object))
  )
  rows <- Map(
    function(network, series_id) {
      data.frame(
        series = series_id,
        type = network$meta$tsn$type,
        observations = nrow(network$ts_source),
        states = nrow(network$nodes),
        edges = nrow(network$edges),
        stringsAsFactors = FALSE
      )
    },
    object,
    names(object)
  )
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' @rdname summary.tsn_series_networks
#' @param x A `tsn_series_networks` result.
#' @param row.names,optional Ignored.
#' @export
as.data.frame.tsn_series_networks <- function(x, row.names = NULL,
                                              optional = FALSE, ...) {
  summary(x)
}

#' @rdname summary.tsn_series_networks
#' @export
print.tsn_series_networks <- function(x, ...) {
  print.data.frame(summary(x), row.names = FALSE)
  invisible(x)
}

#' Plot One Per-Series Transition Network
#'
#' @param x A `tsn_series_networks` result from [series_networks()].
#' @param y Ignored.
#' @param series Series ID to draw. It may be omitted when the collection holds
#'   exactly one model.
#' @param ... Passed to [plot.ts_tna()].
#' @return `x`, invisibly.
#' @examplesIf requireNamespace("Nestimate", quietly = TRUE) && requireNamespace("cograph", quietly = TRUE)
#' states <- discretize(c(3, 1, 4, 2, 5, 3, 6, 2, 7))
#' networks <- series_networks(ts_tna(states))
#' plot(networks, type = "network")
#' @export
plot.tsn_series_networks <- function(x, y = NULL, series = NULL, ...) {
  stopifnot(inherits(x, "tsn_series_networks"), is.null(y))
  available <- names(x)
  selected <- if (is.null(series)) available else unique(as.character(series))
  unknown <- setdiff(selected, available)
  if (length(unknown) > 0L) {
    stop(sprintf("Unknown series: %s.", paste(unknown, collapse = ", ")),
         call. = FALSE)
  }
  if (length(selected) != 1L) {
    stop("Select exactly one model with `series` before plotting a collection.",
         call. = FALSE)
  }
  plot(x[[selected]], ...)
  invisible(x)
}

#' Plot a Time-Series Transition Network
#'
#' Combined views of a `ts_tna()`-family result. `type = "combined"`
#' (default) places each source series — with optional state shading
#' and/or a classification ribbon — beside a transition network on the
#' same row. With `network = "per_series"` (default) every row pairs a
#' series with **its own** network (rebuilt via [series_networks()], so
#' all networks share the same node set and are directly comparable);
#' `network = "summary"` draws the pooled model instead, spanning all
#' series rows and titled "Summary". One state colour mapping ties the
#' shading, ribbons, and network nodes together. `type = "network"` draws
#' only the network(s) — a row of per-series networks or the single
#' summary; `type = "series"` only the series panels. Every combination
#' of `overlay` (shaded or not) and `ribbon` is available.
#'
#' Network panels are rendered exclusively by
#' `cograph::splot()` — the full TNA style with self-loops and probability
#' labels — with node fills matching the state shading and nodes sized by
#' cograph's native centrality scaling (`scale_nodes_by = "instrength"` by
#' default, with `loops = FALSE`), so heavily-entered states read as larger
#' and self-transitions do not drown the between-state structure; pass
#' `scale_nodes_by = list("instrength", loops = TRUE)` through `...` to
#' include self-transitions. `"outstrength"` and `"strength"`
#' (total) are alternatives, and `...` is forwarded to `cograph::splot()`
#' for full control (anything you set there overrides these defaults).
#' `show_weights` controls cograph's edge labels.
#'
#' @param x A `ts_tna` result from [ts_tna()], [ts_ftna()], [ts_cna()], or
#'   [ts_atna()].
#' @param type `"combined"`, `"network"`, or `"series"`.
#' @param series Optional series IDs to draw (default: up to `max_series`).
#' @param max_series Maximum number of series panels (default `3`).
#' @param network Which network(s) to draw: `"per_series"` (default) —
#'   one network per series, each built from that series' own transitions
#'   — or `"summary"` — the single pooled model across all series.
#' @param overlay Series shading: `"horizontal"` (default), `"vertical"`, or
#'   `"none"`.
#' @param ribbon Whether to run the state strip under each series panel.
#' @param points Whether to draw state-coloured observation points.
#' @param node_size Node sizing rule: `"instrength"` (default),
#'   `"outstrength"`, or `"strength"` — passed to cograph as
#'   `scale_nodes_by` (self-loops excluded from the centrality).
#' @param node_scale Multiplier for cograph's node size range
#'   (`node_size_range = c(7, 14) * node_scale`).
#' @param network_width Width of the network column relative to the series
#'   column (default `0.85`); combined view only.
#' @param show_weights Whether to print edge weights on the network.
#' @param alpha Shading opacity.
#' @param line_color,line_width,point_size Series styling.
#' @param strip_height Ribbon strip height as a fraction of each panel.
#' @param palette State colours (preset name, named vector, or vector).
#' @param legend Whether to draw the state legend row.
#' @param xlab,ylab Series axis titles.
#' @param cex Global text size multiplier.
#' @param grid Whether to draw the background grid.
#' @param background Panel background colour.
#' @param ... Forwarded to `cograph::splot()` for the network panel
#'   (e.g. `scale_nodes_by`, `node_size_range`, `scale_nodes_scale`,
#'   `edge_color`); user values override the defaults set here.
#' @return `x`, invisibly.
#' @examplesIf requireNamespace("Nestimate", quietly = TRUE) && requireNamespace("cograph", quietly = TRUE)
#' set.seed(1)
#' series <- list(a = cumsum(rnorm(80)), b = cumsum(rnorm(80)))
#' network <- ts_tna(series, n_states = 3, labels = c("low", "mid", "high"))
#' plot(network)
#' plot(network, network = "summary")
#' plot(network, ribbon = TRUE, overlay = "none")
#' plot(network, "network")
#' @export
plot.ts_tna <- function(x, type = c("combined", "network", "series"),
                        series = NULL, max_series = 3L,
                        network = c("per_series", "summary"),
                        overlay = c("horizontal", "vertical", "none"),
                        ribbon = FALSE,
                        points = FALSE,
                        node_size = c("instrength", "outstrength", "strength"),
                        node_scale = 1, network_width = 0.85,
                        show_weights = TRUE,
                        alpha = 0.28, line_color = "#3B4252",
                        line_width = 1.5, point_size = 0.9,
                        strip_height = 0.09, palette = NULL,
                        legend = TRUE, xlab = "Time", ylab = "Value",
                        cex = 1, grid = TRUE, background = "#FFFFFF", ...) {
  stopifnot(inherits(x, "ts_tna"))
  type <- match.arg(type)
  network <- match.arg(network)
  overlay <- match.arg(overlay)
  node_size <- match.arg(node_size)
  stopifnot(
    is.logical(ribbon), length(ribbon) == 1L, !is.na(ribbon),
    is.logical(points), length(points) == 1L, !is.na(points),
    is.logical(show_weights), length(show_weights) == 1L, !is.na(show_weights),
    is.logical(legend), length(legend) == 1L, !is.na(legend),
    is.numeric(node_scale), length(node_scale) == 1L, node_scale > 0,
    is.numeric(network_width), length(network_width) == 1L,
    network_width > 0,
    is.numeric(max_series), length(max_series) == 1L, max_series >= 1L,
    max_series == as.integer(max_series)
  )
  style <- .tsn_style(cex = cex, grid = grid, background = background)
  source <- x$ts_source
  state_colors <- .tsn_state_colors(source$state, palette = palette)
  available <- unique(source$id)
  selected <- if (is.null(series)) available else unique(as.character(series))
  unknown <- setdiff(selected, available)
  if (length(unknown) > 0L) {
    stop(sprintf("Unknown series: %s.", paste(unknown, collapse = ", ")),
         call. = FALSE)
  }
  selected <- utils::head(selected, as.integer(max_series))

  draw_series <- type %in% c("combined", "series")
  draw_network <- type %in% c("combined", "network")
  if (draw_network) {
    .tsn_require_cograph()
  }
  per_series <- draw_network && network == "per_series"
  networks <- if (per_series) series_networks(x, series = selected) else NULL
  series_panels <- if (draw_series) length(selected) else 0L
  dots <- list(...)
  previous <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::layout(1L)
    graphics::par(previous)
  }, add = TRUE)

  last_series <- selected[length(selected)]
  draw_one_series <- function(series_id) {
    graphics::par(mar = c(3.0, 3.9, 2.0, 0.9), mgp = c(2.1, 0.7, 0))
    one <- source[source$id == series_id, , drop = FALSE]
    y_limits <- .tsn_expand_range(one$value)
    if (ribbon) {
      y_limits[1L] <- y_limits[1L] -
        (strip_height + 0.05) / (1 - strip_height) *
          (y_limits[2L] - y_limits[1L])
    }
    .tsn_panel(
      x = one$time, y = one$value,
      xlim = range(one$time), ylim = y_limits,
      main = as.character(series_id),
      xlab = if (identical(series_id, last_series)) xlab else "",
      ylab = ylab,
      style = style
    )
    if (overlay == "vertical") {
      .tsn_draw_vertical_overlay(one, state_colors, alpha = alpha)
    } else if (overlay == "horizontal") {
      .tsn_draw_horizontal_overlay(one, state_colors, alpha = alpha)
    }
    if (ribbon) {
      .tsn_draw_state_strip(one, state_colors,
                            strip_height = strip_height, style = style)
    }
    graphics::lines(one$time, one$value, col = line_color,
                    lwd = line_width)
    if (points) {
      graphics::points(
        one$time, one$value, pch = 21, cex = point_size,
        bg = unname(state_colors[one$state]), col = background, lwd = 0.6
      )
    }
  }
  draw_one_network <- function(model, title = NULL) {
    .tsn_splot_network(
      model, state_colors = state_colors, node_size = node_size,
      node_scale = node_scale, show_weights = show_weights,
      dots = dots, title = title
    )
  }

  if (draw_series && draw_network && per_series) {
    # Combined per-series: each row pairs a series panel with its OWN
    # network; legend across the bottom.
    cells <- cbind(
      seq(1L, by = 2L, length.out = series_panels),
      seq(2L, by = 2L, length.out = series_panels)
    )
    if (legend) {
      cells <- rbind(cells, rep.int(2L * series_panels + 1L, 2L))
    }
    graphics::layout(
      cells,
      widths = c(1, network_width),
      heights = c(rep.int(1, series_panels), if (legend) 0.18 else NULL)
    )
    invisible(lapply(selected, function(series_id) {
      draw_one_series(series_id)
      draw_one_network(networks[[series_id]])
    }))
  } else if (draw_series && draw_network) {
    # Combined summary: series panels stacked in the left column, the
    # pooled network in a right-hand column spanning the same rows.
    network_id <- series_panels + 1L
    cells <- cbind(seq_len(series_panels), rep.int(network_id, series_panels))
    if (legend) {
      cells <- rbind(cells, rep.int(network_id + 1L, 2L))
    }
    graphics::layout(
      cells,
      widths = c(1, network_width),
      heights = c(rep.int(1, series_panels), if (legend) 0.18 else NULL)
    )
    invisible(lapply(selected, draw_one_series))
    draw_one_network(
      x, title = if (length(selected) > 1L) "Summary" else NULL
    )
  } else if (draw_network && per_series && length(selected) > 1L) {
    # Networks only, one panel per series in a row; legend underneath.
    cells <- matrix(seq_along(selected), nrow = 1L)
    if (legend) {
      cells <- rbind(cells, rep.int(length(selected) + 1L, length(selected)))
    }
    graphics::layout(cells, heights = c(1, if (legend) 0.18 else NULL))
    invisible(lapply(selected, function(series_id) {
      draw_one_network(networks[[series_id]], title = series_id)
    }))
  } else {
    heights <- c(
      rep.int(1, series_panels),
      if (draw_network) 1.7 else NULL,
      if (legend) 0.16 else NULL
    )
    panel_total <- series_panels + as.integer(draw_network) +
      as.integer(legend)
    graphics::layout(matrix(seq_len(panel_total), ncol = 1L),
                     heights = heights)
    if (draw_series) {
      invisible(lapply(selected, draw_one_series))
    }
    if (draw_network) {
      model <- if (per_series) networks[[selected[1L]]] else x
      draw_one_network(
        model,
        title = if (!per_series && length(selected) > 1L) "Summary" else NULL
      )
    }
  }
  if (legend) {
    .tsn_legend_row(
      labels = names(state_colors), colors = state_colors,
      title = "State", style = style, kind = "fill"
    )
  }
  invisible(x)
}

#' Render One Network Panel
#'
#' Uses `cograph::splot()` with state-coloured node fills and cograph's
#' native centrality sizing (`scale_nodes_by`, self-loops excluded so
#' self-transitions do not drown the between-state structure — matching
#' `Nestimate::net_centrality`). Caller-supplied `dots` win over these
#' defaults. Network rendering belongs exclusively to cograph.
#'
#' @noRd
.tsn_splot_network <- function(model, state_colors, node_size, node_scale,
                               show_weights, dots, title = NULL) {
  .tsn_require_cograph()
  nodes <- model$nodes$label
  node_fills <- unname(state_colors[nodes])
  node_fills[is.na(node_fills)] <- "#9CA3AF"
  if (!"scale_nodes_by" %in% names(dots)) {
    dots$scale_nodes_by <- list(node_size, loops = FALSE)
    if (!"node_size_range" %in% names(dots)) {
      dots$node_size_range <- c(7, 14) * node_scale
    }
  }
  if (!show_weights && !"edge_label_style" %in% names(dots)) {
    dots$edge_label_style <- "none"
  }
  if (!is.null(title) && !"title" %in% names(dots)) {
    dots$title <- title
  }
  do.call(cograph::splot, c(list(model, node_fill = node_fills), dots))
}
