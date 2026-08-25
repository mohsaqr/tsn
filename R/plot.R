#' Plot a TSN Result
#'
#' By default, `plot()` renders the constructed network through
#' [cograph::splot()]. The diagnostic source-series view (with optional state
#' shading) remains available via `type = "series"` and does not require
#' cograph.
#'
#' @param x A `tsn` result.
#' @param type What to draw: `"network"` (default) or `"series"`.
#' @param ... Arguments forwarded to the network or series renderer.
#'   See details.
#' @details
#' Network rendering belongs exclusively to cograph. tsn supplies restrained
#' defaults that keep edges visible and avoid meaningless per-node rainbow
#' fills; named arguments in `...` override any of them. tsn results already
#' implement the `cograph_network` contract. Source-series views remain
#' tsn-specific because they visualize the original observations rather than a
#' generic network.
#'
#' For `type = "network"`, use cograph arguments such as `layout`, `labels`,
#' `scale_nodes_by`, `node_size_range`, and `edge_label_style`. For
#' `type = "series"`, `...` accepts `series`,
#' `overlay`, `points`, `trend`, `columns`, `max_series`, `scales`, and
#' `palette`.
#' @return `x`, invisibly.
#' @examples
#' network <- tsn(c(3, 1, 4, 2, 5, 3, 6, 2, 7), "hvg")
#' if (requireNamespace("cograph", quietly = TRUE)) {
#'   plot(network)
#'   plot(network, layout = "spring")
#' }
#'
#' data(srl)
#' states <- tsn(
#'   srl,
#'   value = "effort",
#'   id = "name",
#'   time = "day",
#'   series = "Erik",
#'   unit = "state",
#'   discretization = "quantile"
#' )
#' plot(states, "series", overlay = "vertical")
#' @export
plot.tsn <- function(x, type = c("network", "series"), ...) {
  stopifnot(inherits(x, "tsn"))
  type <- match.arg(type)
  if (type == "network") {
    return(.tsn_plot_network(x, ...))
  }
  .tsn_plot_series(x, ...)
}

#' Draw the source time series of a TSN result
#'
#' @inheritParams plot.tsn
#' @param series Optional character vector of series IDs to display.
#' @param overlay State shading: `"horizontal"` (default), `"vertical"`, or
#'   `"none"`.
#' @param points Whether to draw observations over each line.
#' @param trend Whether to add a least-squares trend line.
#' @param columns Number of panel columns.
#' @param max_series Maximum number of series to draw.
#' @param scales Panel scales.
#' @param palette Optional state colors.
#' @return `x`, invisibly.
#' @noRd
.tsn_plot_series <- function(x, series = NULL,
                             overlay = c("horizontal", "vertical", "none"),
                             points = FALSE, trend = FALSE, columns = NULL,
                             max_series = 10L,
                             scales = c("free", "fixed", "free_x", "free_y"),
                             palette = NULL, ...) {
  overlay <- match.arg(overlay)
  scales <- match.arg(scales)
  source <- x$source
  .tsn_plot_series_frame(
    source = source, series = series, overlay = overlay, points = points,
    trend = trend, columns = columns, max_series = max_series,
    scales = scales, palette = palette, ...
  )
  invisible(x)
}

#' Draw a tidy source frame with optional state shading
#'
#' @param source A data frame with `id`, `time`, `value`, and optional `state`.
#' @inheritParams .tsn_plot_series
#' @return `NULL`, invisibly.
#' @noRd
.tsn_plot_series_frame <- function(source, series = NULL,
                                   overlay = "horizontal", points = FALSE,
                                   trend = FALSE, columns = NULL,
                                   max_series = 10L, scales = "free",
                                   palette = NULL, strip = FALSE,
                                   legend_title = "State",
                                   alpha = 0.28, line_color = "#3B4252",
                                   line_width = 1.6, point_size = 1,
                                   strip_height = 0.1, legend = TRUE,
                                   lines = "none", min_run = 1L,
                                   breaks = NULL,
                                   xlab = "Time", ylab = "Value",
                                   cex = 1, grid = TRUE,
                                   background = "#FFFFFF", ...) {
  stopifnot(is.logical(points), length(points) == 1L, !is.na(points))
  stopifnot(is.logical(trend), length(trend) == 1L, !is.na(trend))
  stopifnot(is.logical(strip), length(strip) == 1L, !is.na(strip))
  stopifnot(is.logical(legend), length(legend) == 1L, !is.na(legend))
  stopifnot(is.numeric(alpha), length(alpha) == 1L, alpha >= 0, alpha <= 1)
  stopifnot(is.numeric(line_width), length(line_width) == 1L, line_width > 0)
  stopifnot(is.numeric(point_size), length(point_size) == 1L, point_size > 0)
  stopifnot(is.numeric(strip_height), length(strip_height) == 1L,
            strip_height > 0, strip_height <= 0.4)
  stopifnot(is.numeric(max_series), length(max_series) == 1L,
            is.finite(max_series), max_series >= 1L,
            max_series == as.integer(max_series))
  stopifnot(is.numeric(min_run), length(min_run) == 1L, is.finite(min_run),
            min_run >= 1L, min_run == as.integer(min_run))
  lines <- match.arg(lines, c("none", "horizontal", "vertical", "both"))
  style <- .tsn_style(cex = cex, grid = grid, background = background)
  if (!"state" %in% names(source)) {
    overlay <- "none"
    strip <- FALSE
    lines <- "none"
  }
  available <- unique(source$id)
  selected <- if (is.null(series)) available else unique(as.character(series))
  unknown <- setdiff(selected, available)
  if (length(unknown) > 0L) {
    stop(sprintf("Unknown series: %s.", paste(unknown, collapse = ", ")),
         call. = FALSE)
  }
  if (length(selected) == 0L) {
    stop("At least one series must be selected.", call. = FALSE)
  }
  selected <- utils::head(selected, as.integer(max_series))
  columns <- if (is.null(columns)) ceiling(sqrt(length(selected))) else as.integer(columns)
  stopifnot(length(columns) == 1L, !is.na(columns), columns >= 1L)
  rows <- ceiling(length(selected) / columns)
  plot_source <- source[source$id %in% selected, , drop = FALSE]
  state_colors <- if (overlay == "none" && !strip && lines == "none") NULL else
    .tsn_state_colors(plot_source$state, palette = palette)
  guide_boundaries <- if (lines %in% c("horizontal", "both")) {
    .tsn_state_boundaries(plot_source, breaks)
  } else {
    numeric()
  }
  previous <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::layout(1L)
    graphics::par(previous)
  }, add = TRUE)
  panel_count <- rows * columns
  show_legend <- legend && !is.null(state_colors)
  if (!show_legend) {
    graphics::layout(matrix(seq_len(panel_count), nrow = rows, byrow = TRUE))
  } else {
    panel_layout <- matrix(seq_len(panel_count), nrow = rows, byrow = TRUE)
    legend_panel <- panel_count + 1L
    graphics::layout(
      rbind(panel_layout, rep.int(legend_panel, columns)),
      heights = c(rep.int(1, rows), 0.15)
    )
  }
  graphics::par(mar = c(3.1, 3.9, 2.2, 0.9), mgp = c(2.1, 0.7, 0))
  global_x <- range(plot_source$time)
  global_y <- .tsn_expand_range(plot_source$value)
  invisible(lapply(selected, function(series_id) {
    series_data <- source[source$id == series_id, , drop = FALSE]
    x_limits <- if (scales %in% c("fixed", "free_y")) global_x else
      range(series_data$time)
    y_limits <- if (scales %in% c("fixed", "free_x")) global_y else
      .tsn_expand_range(series_data$value)
    if (strip) {
      y_limits[1L] <- y_limits[1L] -
        (strip_height + 0.05) / (1 - strip_height) *
          (y_limits[2L] - y_limits[1L])
    }
    .tsn_panel(
      x = series_data$time, y = series_data$value,
      xlim = x_limits, ylim = y_limits,
      main = as.character(series_id), xlab = xlab, ylab = ylab,
      style = style
    )
    if (overlay == "vertical") {
      .tsn_draw_vertical_overlay(series_data, state_colors, alpha = alpha)
    } else if (overlay == "horizontal") {
      .tsn_draw_horizontal_overlay(series_data, state_colors, alpha = alpha)
    }
    if (strip) {
      .tsn_draw_state_strip(series_data, state_colors,
                            strip_height = strip_height, style = style)
    }
    if (length(guide_boundaries) > 0L) {
      graphics::abline(h = guide_boundaries, col = style$axis_color,
                       lty = 2L, lwd = 1)
    }
    if (lines %in% c("vertical", "both")) {
      transitions <- .tsn_transitions(series_data, min_run)
      if (nrow(transitions) > 0L) {
        graphics::abline(
          v = transitions$time,
          col = unname(state_colors[transitions$state]),
          lty = 2L, lwd = 1.3
        )
      }
    }
    graphics::lines(series_data$time, series_data$value, col = line_color,
                    lwd = line_width)
    if (points) {
      point_colors <- if (is.null(state_colors)) line_color else
        unname(state_colors[as.character(series_data$state)])
      graphics::points(
        series_data$time,
        series_data$value,
        pch = 21,
        bg = point_colors,
        col = background,
        lwd = 0.7,
        cex = point_size
      )
    }
    if (trend && nrow(series_data) >= 2L) {
      graphics::abline(
        stats::lm(series_data$value ~ as.numeric(series_data$time)),
        col = grDevices::adjustcolor(style$text_color, alpha.f = 0.55),
        lty = 2L,
        lwd = 1.4
      )
    }
  }))
  unused_panels <- panel_count - length(selected)
  if (unused_panels > 0L) {
    invisible(lapply(seq_len(unused_panels), function(index) graphics::plot.new()))
  }
  if (show_legend) {
    .tsn_legend_row(
      labels = names(state_colors), colors = state_colors,
      title = legend_title, style = style, kind = "fill"
    )
  }
  invisible(NULL)
}

#' Create deterministic state colors
#'
#' `palette` accepts `NULL` (default preset), a preset name understood by
#' `.tsn_palette_colors()`, a named colour vector covering the state levels,
#' or a plain colour vector. Factor input keeps its LEVEL ORDER (so ordered
#' states like low/mid/high map to colours in that order); character input
#' falls back to sorted unique values.
#'
#' @noRd
.tsn_state_colors <- function(states, palette = NULL) {
  levels <- if (is.factor(states)) {
    observed <- levels(states)[levels(states) %in% unique(as.character(states))]
    observed
  } else {
    sort(unique(as.character(states)))
  }
  named_palette <- is.character(palette) && !is.null(names(palette)) &&
    all(levels %in% names(palette))
  if (named_palette) {
    return(palette[levels])
  }
  colors <- .tsn_palette_colors(palette, length(levels))
  names(colors) <- levels
  colors
}

#' Draw contiguous vertical state runs
#' @noRd
.tsn_draw_vertical_overlay <- function(series_data, colors, alpha = 0.28) {
  segments <- .tsn_overlay_segments(
    position = series_data$time,
    states = series_data$state,
    sort_position = FALSE
  )
  limits <- graphics::par("usr")
  graphics::rect(
    xleft = segments$start,
    ybottom = limits[3L],
    xright = segments$end,
    ytop = limits[4L],
    col = grDevices::adjustcolor(
      unname(colors[segments$state]),
      alpha.f = alpha
    ),
    border = NA
  )
}

#' Draw horizontal value bands by ordered state means
#' @noRd
.tsn_draw_horizontal_overlay <- function(series_data, colors, alpha = 0.28) {
  segments <- .tsn_overlay_segments(
    position = series_data$value,
    states = series_data$state,
    sort_position = TRUE
  )
  limits <- graphics::par("usr")
  graphics::rect(
    xleft = limits[1L],
    ybottom = pmax(limits[3L], segments$start),
    xright = limits[2L],
    ytop = pmin(limits[4L], segments$end),
    col = grDevices::adjustcolor(
      unname(colors[segments$state]),
      alpha.f = alpha
    ),
    border = NA
  )
}

#' Calculate midpoint-bounded state segments
#' @noRd
.tsn_overlay_segments <- function(position, states, sort_position) {
  stopifnot(length(position) == length(states), length(position) > 0L)
  ordering <- if (sort_position) order(position, method = "radix") else
    seq_along(position)
  ordered_position <- as.numeric(position[ordering])
  ordered_states <- as.character(states[ordering])
  runs <- rle(ordered_states)
  run_end <- cumsum(runs$lengths)
  run_start <- c(1L, utils::head(run_end, -1L) + 1L)
  boundaries <- if (length(ordered_position) == 1L) {
    c(ordered_position - 0.5, ordered_position + 0.5)
  } else {
    middle <- (utils::head(ordered_position, -1L) +
      utils::tail(ordered_position, -1L)) / 2
    c(
      ordered_position[1L] - (middle[1L] - ordered_position[1L]),
      middle,
      ordered_position[length(ordered_position)] +
        (ordered_position[length(ordered_position)] - middle[length(middle)])
    )
  }
  data.frame(
    start = boundaries[run_start],
    end = boundaries[run_end + 1L],
    state = runs$values,
    stringsAsFactors = FALSE
  )
}

#' Expand a zero-width numeric plotting range
#' @noRd
.tsn_expand_range <- function(values) {
  finite <- values[is.finite(values)]
  if (length(finite) == 0L) {
    return(c(-0.5, 0.5))
  }
  limits <- range(finite)
  if (diff(limits) == 0) {
    padding <- if (limits[1L] == 0) 0.5 else abs(limits[1L]) * 0.04
    limits <- limits + c(-padding, padding)
  }
  limits
}

#' Render a TSN Network with cograph
#'
#' Network rendering belongs exclusively to cograph. Time-series panels remain
#' implemented locally because they visualize source observations rather than
#' a generic graph.
#'
#' @param x A `tsn` result.
#' @param ... Passed to [cograph::splot()], overriding tsn's network defaults.
#' @return `x`, invisibly.
#' @noRd
.tsn_plot_network <- function(x, ...) {
  .tsn_require_cograph()
  arguments <- .tsn_network_plot_arguments(...)
  do.call(cograph::splot, c(list(x), arguments))
  invisible(x)
}

#' Resolve Presentation Defaults for Generic Network Plots
#'
#' Keeps the public `plot(network)` call readable while preserving cograph's
#' complete customization surface. Caller-supplied named arguments always win.
#'
#' @param ... Named cograph plotting arguments.
#' @return A named argument list for `cograph::splot()`.
#' @noRd
.tsn_network_plot_arguments <- function(...) {
  supplied <- list(...)
  defaults <- list(
    layout = "spring",
    labels = FALSE,
    scale_nodes_by = "degree",
    node_size_range = c(2, 9),
    node_fill = "#4E79A7",
    edge_label_style = "none",
    edge_color = "#B9C2CC"
  )
  defaults[names(supplied)] <- NULL
  c(defaults, supplied)
}

#' Require cograph for Generic Network Rendering
#'
#' @return `TRUE`, invisibly.
#' @noRd
.tsn_require_cograph <- function() {
  if (!requireNamespace("cograph", quietly = TRUE)) {
    stop(
      "Network plotting requires the cograph package. ",
      "Install it with install.packages(\"cograph\"), or use a time-series ",
      "plot type that does not draw a network.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Draw a state-classification strip along the panel bottom
#'
#' Fills the band reserved by the `strip` option of
#' `.tsn_plot_series_frame()` with contiguous state tiles, giving the
#' ribbon view: the series stays readable while its classification runs
#' underneath it. Segment boundaries carry a thin background-coloured
#' separator so runs read as discrete tiles.
#'
#' @noRd
.tsn_draw_state_strip <- function(series_data, colors, strip_height = 0.1,
                                  style = .tsn_style()) {
  segments <- .tsn_overlay_segments(
    position = series_data$time,
    states = series_data$state,
    sort_position = FALSE
  )
  limits <- graphics::par("usr")
  strip_top <- limits[3L] + strip_height * (limits[4L] - limits[3L])
  graphics::rect(
    xleft = segments$start,
    ybottom = limits[3L],
    xright = segments$end,
    ytop = strip_top,
    col = unname(colors[segments$state]),
    border = style$background,
    lwd = 0.6
  )
  graphics::abline(h = strip_top, col = style$frame_color, lwd = 0.9)
}

#' Draw a series-by-time state heatmap
#'
#' One row per series, one tile per observation, coloured by state: the
#' sequence-index view for comparing state timing across many series at
#' once. Rows can be reordered by mean value or by modal state so similar
#' series sit together.
#'
#' @noRd
.tsn_plot_state_heatmap <- function(source, series = NULL, max_series = 25L,
                                    palette = NULL, legend_title = "State",
                                    sort = c("none", "mean", "state"),
                                    border = FALSE, legend = TRUE,
                                    main = "", xlab = "Time",
                                    tile_gap = 0.06, cex = 1, grid = FALSE,
                                    background = "#FFFFFF", ...) {
  stopifnot(is.numeric(max_series), length(max_series) == 1L,
            is.finite(max_series), max_series >= 1L,
            max_series == as.integer(max_series))
  stopifnot(is.logical(border), length(border) == 1L, !is.na(border))
  stopifnot(is.logical(legend), length(legend) == 1L, !is.na(legend))
  stopifnot(is.numeric(tile_gap), length(tile_gap) == 1L,
            tile_gap >= 0, tile_gap < 0.5)
  sort <- match.arg(sort)
  style <- .tsn_style(cex = cex, grid = grid, background = background)
  available <- unique(source$id)
  selected <- if (is.null(series)) available else unique(as.character(series))
  unknown <- setdiff(selected, available)
  if (length(unknown) > 0L) {
    stop(sprintf("Unknown series: %s.", paste(unknown, collapse = ", ")),
         call. = FALSE)
  }
  selected <- utils::head(selected, as.integer(max_series))
  plot_source <- source[source$id %in% selected, , drop = FALSE]
  if (sort == "mean") {
    means <- tapply(plot_source$value, plot_source$id, mean)
    selected <- names(base::sort(means[selected]))
  } else if (sort == "state") {
    modal <- vapply(
      split(as.character(plot_source$state), plot_source$id),
      function(states) names(base::sort(table(states), decreasing = TRUE))[1L],
      character(1L)
    )
    selected <- selected[order(modal[selected], selected, method = "radix")]
  }
  state_colors <- .tsn_state_colors(plot_source$state, palette = palette)
  previous <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::layout(1L)
    graphics::par(previous)
  }, add = TRUE)
  if (legend) {
    graphics::layout(matrix(c(1L, 2L), nrow = 2L), heights = c(1, 0.13))
  }
  graphics::par(mar = c(3.1, 5.7, if (nzchar(main)) 2.2 else 1.1, 0.9),
                mgp = c(2.1, 0.7, 0))
  row_count <- length(selected)
  half <- 0.5 - tile_gap / 2
  .tsn_panel(
    x = plot_source$time,
    y = c(0.5, row_count + 0.5),
    xlim = range(plot_source$time),
    ylim = c(0.5, row_count + 0.5),
    main = main, xlab = xlab, ylab = "",
    style = style, y_axis = FALSE
  )
  invisible(lapply(seq_along(selected), function(row_index) {
    series_data <- plot_source[plot_source$id == selected[row_index], ,
                               drop = FALSE]
    segments <- .tsn_overlay_segments(
      position = as.numeric(series_data$time),
      states = series_data$state,
      sort_position = FALSE
    )
    graphics::rect(
      xleft = segments$start,
      ybottom = row_index - half,
      xright = segments$end,
      ytop = row_index + half,
      col = unname(state_colors[segments$state]),
      border = if (border) style$background else NA,
      lwd = 0.5
    )
  }))
  graphics::axis(
    2L, at = seq_len(row_count), labels = selected, las = 1L,
    col = NA, col.axis = style$axis_color, cex.axis = 0.75 * style$cex,
    tcl = 0
  )
  if (legend) {
    .tsn_legend_row(
      labels = names(state_colors), colors = state_colors,
      title = legend_title, style = style, kind = "fill"
    )
  }
  invisible(NULL)
}
#' Draw a Companion Series Stacked Above the Discretized Series
#'
#' The cross-shading view: per series, the companion values (e.g. the
#' original time series) sit in the top panel and the discretized series
#' (e.g. its rolling complexity) in the bottom panel, on a shared time
#' axis. Both panels carry the SAME state shading — the states of the
#' discretized series — so the companion series is read directly against
#' the classification derived from the other. Points on both panels are
#' coloured by state; a segment-coloured line (`color_line = TRUE`) colours
#' each line segment by the state at its left endpoint instead.
#'
#' @noRd
.tsn_plot_state_stack <- function(source, with, series = NULL,
                                  max_series = 3L, palette = NULL,
                                  shade = TRUE, alpha = 0.28,
                                  points = TRUE, color_line = FALSE,
                                  line_color = "#3B4252", line_width = 1.6,
                                  point_size = 0.95, legend = TRUE,
                                  with_label = "Series",
                                  ylab = "Value", xlab = "Time",
                                  legend_title = "State", cex = 1,
                                  grid = TRUE, background = "#FFFFFF", ...) {
  stopifnot(
    is.numeric(with), length(with) == nrow(source), all(is.finite(with)),
    is.logical(shade), length(shade) == 1L, !is.na(shade),
    is.logical(points), length(points) == 1L, !is.na(points),
    is.logical(color_line), length(color_line) == 1L, !is.na(color_line),
    is.logical(legend), length(legend) == 1L, !is.na(legend),
    is.numeric(alpha), length(alpha) == 1L, alpha >= 0, alpha <= 1,
    is.numeric(max_series), length(max_series) == 1L,
    is.finite(max_series), max_series >= 1L,
    max_series == as.integer(max_series)
  )
  style <- .tsn_style(cex = cex, grid = grid, background = background)
  source$companion <- as.numeric(with)
  available <- unique(source$id)
  selected <- if (is.null(series)) available else unique(as.character(series))
  unknown <- setdiff(selected, available)
  if (length(unknown) > 0L) {
    stop(sprintf("Unknown series: %s.", paste(unknown, collapse = ", ")),
         call. = FALSE)
  }
  selected <- utils::head(selected, as.integer(max_series))
  plot_source <- source[source$id %in% selected, , drop = FALSE]
  state_colors <- .tsn_state_colors(plot_source$state, palette = palette)
  previous <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::layout(1L)
    graphics::par(previous)
  }, add = TRUE)
  panel_rows <- 2L * length(selected)
  layout_rows <- if (legend) panel_rows + 1L else panel_rows
  graphics::layout(
    matrix(seq_len(layout_rows), ncol = 1L),
    heights = c(rep.int(c(1, 0.8), length(selected)),
                if (legend) 0.16 else NULL)
  )
  draw_panel <- function(series_data, values, label, title, x_label,
                         top_margin, bottom_margin) {
    graphics::par(mar = c(bottom_margin, 3.9, top_margin, 0.9),
                  mgp = c(2.1, 0.7, 0))
    .tsn_panel(
      x = series_data$time, y = values,
      xlim = range(series_data$time),
      ylim = .tsn_expand_range(values),
      main = title, xlab = x_label, ylab = label, style = style
    )
    if (shade) {
      .tsn_draw_vertical_overlay(series_data, state_colors, alpha = alpha)
    }
    if (color_line && nrow(series_data) >= 2L) {
      run_colors <- unname(state_colors[as.character(
        utils::head(series_data$state, -1L)
      )])
      graphics::segments(
        x0 = as.numeric(utils::head(series_data$time, -1L)),
        y0 = utils::head(values, -1L),
        x1 = as.numeric(utils::tail(series_data$time, -1L)),
        y1 = utils::tail(values, -1L),
        col = run_colors, lwd = line_width
      )
    } else {
      graphics::lines(series_data$time, values, col = line_color,
                      lwd = line_width)
    }
    if (points) {
      graphics::points(
        series_data$time, values, pch = 21, cex = point_size,
        bg = unname(state_colors[as.character(series_data$state)]),
        col = background, lwd = 0.6
      )
    }
  }
  invisible(lapply(selected, function(series_id) {
    series_data <- plot_source[plot_source$id == series_id, , drop = FALSE]
    draw_panel(
      series_data, series_data$companion, label = with_label,
      title = as.character(series_id), x_label = "",
      top_margin = 2.0, bottom_margin = 1.4
    )
    draw_panel(
      series_data, series_data$value, label = ylab,
      title = "", x_label = xlab,
      top_margin = 0.6, bottom_margin = 3.2
    )
  }))
  if (legend) {
    .tsn_legend_row(
      labels = names(state_colors), colors = state_colors,
      title = legend_title, style = style, kind = "fill"
    )
  }
  invisible(NULL)
}

#' Resolve Horizontal State Boundaries
#'
#' Uses the finite discretization breaks when supplied; otherwise derives
#' empirical boundaries as the midpoint between adjacent state value ranges
#' (states ordered by mean value).
#'
#' @noRd
.tsn_state_boundaries <- function(plot_source, breaks) {
  if (is.numeric(breaks)) {
    finite <- breaks[is.finite(breaks)]
    if (length(finite) > 0L) {
      return(as.numeric(finite))
    }
  }
  if (!"state" %in% names(plot_source)) {
    return(numeric())
  }
  by_state <- split(plot_source$value, plot_source$state)
  means <- vapply(by_state, mean, numeric(1L))
  ordered <- by_state[order(means)]
  if (length(ordered) < 2L) {
    return(numeric())
  }
  vapply(
    seq_len(length(ordered) - 1L),
    function(index) {
      (max(ordered[[index]]) + min(ordered[[index + 1L]])) / 2
    },
    numeric(1L)
  )
}

#' Calculate State Transitions for Dashed Guides
#'
#' Returns one row per transition into a state that persists for at least
#' `min_run` observations: the midpoint `time` of the transition and the
#' `state` being entered, so guide lines can carry the colour of the state
#' that starts there.
#'
#' @noRd
.tsn_transitions <- function(series_data, min_run = 1L) {
  empty <- data.frame(time = numeric(), state = character(),
                      stringsAsFactors = FALSE)
  if (nrow(series_data) < 2L) {
    return(empty)
  }
  runs <- rle(as.character(series_data$state))
  run_ends <- cumsum(runs$lengths)
  entering <- which(utils::tail(runs$lengths, -1L) >= min_run)
  changed <- run_ends[entering]
  valid <- changed < nrow(series_data)
  changed <- changed[valid]
  if (length(changed) == 0L) {
    return(empty)
  }
  data.frame(
    time = (as.numeric(series_data$time[changed]) +
              as.numeric(series_data$time[changed + 1L])) / 2,
    state = runs$values[entering[valid] + 1L],
    stringsAsFactors = FALSE
  )
}

#' Draw One Series Above a Stack of Classification Ribbons
#'
#' The resilience-style ribbon stack: a single series on top and one
#' coloured strip per classification below it, all sharing the time axis.
#' Each ribbon is an independent state classification of the same series
#' (its own discretization, a trend classification, a discretized derived
#' metric, ...), so the series can be read against several perspectives at
#' once. Ribbon names label the strips on the y axis; each ribbon gets a
#' legend row.
#'
#' @param source Tidy single-series data (`id`, `time`, `value`, `state`).
#' @param ribbons Named list of resolved ribbons, each
#'   `list(states = <character>, colors = <named colours>)`, drawn top to
#'   bottom in list order.
#' @noRd
.tsn_plot_ribbon_stack <- function(source, ribbons,
                                   strip_height = 0.055, points = FALSE,
                                   line_color = "#3B4252", line_width = 1.6,
                                   point_size = 0.9, legend = TRUE,
                                   main = "", xlab = "Time", ylab = "Value",
                                   cex = 1, grid = TRUE,
                                   background = "#FFFFFF", ...) {
  stopifnot(
    is.list(ribbons), length(ribbons) >= 1L, !is.null(names(ribbons)),
    all(nzchar(names(ribbons))),
    is.numeric(strip_height), length(strip_height) == 1L,
    strip_height > 0, strip_height <= 0.2,
    is.logical(points), length(points) == 1L, !is.na(points),
    is.logical(legend), length(legend) == 1L, !is.na(legend)
  )
  style <- .tsn_style(cex = cex, grid = grid, background = background)
  ribbon_count <- length(ribbons)
  gap <- 0.014
  reserved <- ribbon_count * strip_height + (ribbon_count + 1L) * gap
  stopifnot(reserved < 0.6)
  previous <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::layout(1L)
    graphics::par(previous)
  }, add = TRUE)
  if (legend) {
    legend_height <- 0.05 + 0.05 * ribbon_count
    graphics::layout(matrix(c(1L, 2L), nrow = 2L),
                     heights = c(1, legend_height))
  }
  graphics::par(mar = c(3.1, 6.3, if (nzchar(main)) 2.2 else 1.1, 0.9),
                mgp = c(2.1, 0.7, 0))
  data_limits <- .tsn_expand_range(source$value)
  data_span <- data_limits[2L] - data_limits[1L]
  total_limits <- c(
    data_limits[1L] - reserved / (1 - reserved) * data_span,
    data_limits[2L]
  )
  panel_style <- style
  panel_style$grid <- FALSE
  .tsn_panel(
    x = source$time, y = source$value,
    xlim = range(source$time), ylim = total_limits,
    main = main, xlab = xlab, ylab = "", style = panel_style,
    y_axis = FALSE
  )
  value_ticks <- pretty(data_limits)
  value_ticks <- value_ticks[value_ticks >= data_limits[1L] &
                               value_ticks <= data_limits[2L]]
  if (style$grid) {
    graphics::abline(v = graphics::axTicks(1L), col = style$grid_color,
                     lwd = 0.9)
    graphics::abline(h = value_ticks, col = style$grid_color, lwd = 0.9)
  }
  graphics::axis(
    2L, at = value_ticks, las = 1L, col = NA,
    col.ticks = style$axis_color, col.axis = style$axis_color,
    cex.axis = 0.8 * style$cex, tcl = -0.25, lwd.ticks = 0.8, hadj = 0.85
  )
  limits <- graphics::par("usr")
  full_span <- limits[4L] - limits[3L]
  strip_units <- strip_height * full_span
  gap_units <- gap * full_span
  centers <- vapply(seq_len(ribbon_count), function(index) {
    bottom <- limits[3L] + gap_units +
      (ribbon_count - index) * (strip_units + gap_units)
    ribbon <- ribbons[[index]]
    segments <- .tsn_overlay_segments(
      position = source$time,
      states = ribbon$states,
      sort_position = FALSE
    )
    graphics::rect(
      xleft = segments$start,
      ybottom = bottom,
      xright = segments$end,
      ytop = bottom + strip_units,
      col = unname(ribbon$colors[segments$state]),
      border = style$background,
      lwd = 0.5
    )
    bottom + strip_units / 2
  }, numeric(1L))
  graphics::abline(
    h = limits[3L] + gap_units +
      ribbon_count * (strip_units + gap_units) - gap_units / 2,
    col = style$frame_color, lwd = 0.9
  )
  graphics::axis(
    2L, at = centers, labels = names(ribbons), las = 1L, tick = FALSE,
    col.axis = style$axis_color, cex.axis = 0.72 * style$cex, line = -0.4
  )
  graphics::mtext(ylab, side = 2L, line = 2.6, cex = 0.85 * style$cex,
                  col = style$axis_color,
                  at = mean(data_limits))
  graphics::lines(source$time, source$value, col = line_color,
                  lwd = line_width)
  if (points) {
    primary <- ribbons[[1L]]
    graphics::points(
      source$time, source$value, pch = 21, cex = point_size,
      bg = unname(primary$colors[primary$states]),
      col = background, lwd = 0.7
    )
  }
  if (legend) {
    graphics::par(mar = c(0, 6.3, 0, 0.9))
    graphics::plot.new()
    graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
    rows <- seq(0.8, 0.2, length.out = max(ribbon_count, 2L))
    label_cex <- 0.78 * style$cex
    invisible(lapply(seq_len(ribbon_count), function(index) {
      ribbon <- ribbons[[index]]
      row_y <- rows[index]
      graphics::text(
        0, row_y, labels = paste0(names(ribbons)[index], ":"),
        adj = c(0, 0.5), font = 2L, cex = label_cex,
        col = style$text_color
      )
      labels <- names(ribbon$colors)
      title_width <- graphics::strwidth(
        paste0(names(ribbons)[index], ":  "),
        cex = label_cex, font = 2L
      )
      label_widths <- graphics::strwidth(labels, cex = label_cex)
      swatch_pad <- 0.012
      item_gap <- 0.035
      item_widths <- swatch_pad + label_widths + item_gap
      x_swatches <- 0.02 + max(title_width, 0.1) +
        c(0, cumsum(utils::head(item_widths, -1L)))
      graphics::points(x_swatches, rep(row_y, length(labels)), pch = 15L,
                       col = unname(ribbon$colors), cex = 1.15)
      graphics::text(x_swatches + swatch_pad, rep(row_y, length(labels)),
                     labels = labels, adj = c(0, 0.5), cex = label_cex,
                     col = style$text_color)
    }))
  }
  invisible(NULL)
}

#' Resolve a Ribbon Specification to States and Colours
#'
#' Accepts a `tsn_states`, a `tsn_trend`, or a plain character/factor vector
#' and returns the state sequence for one series with its colour mapping.
#'
#' @noRd
.tsn_resolve_ribbon <- function(ribbon, series_id, n_expected, name) {
  states <- if (inherits(ribbon, c("tsn_states", "tsn_trend"))) {
    ribbon$state[as.character(ribbon$id) == series_id]
  } else if (is.character(ribbon) || is.factor(ribbon)) {
    ribbon
  } else {
    stop(sprintf(
      "Ribbon `%s` must be a tsn_states, tsn_trend, or character vector.",
      name
    ), call. = FALSE)
  }
  if (length(states) != n_expected) {
    stop(sprintf(
      "Ribbon `%s` provides %d states for series \"%s\" but %d are needed.",
      name, length(states), series_id, n_expected
    ), call. = FALSE)
  }
  colors <- if (inherits(ribbon, "tsn_trend")) {
    trend_colors <- .tsn_trend_colors()
    present <- levels(droplevels(factor(states)))
    trend_colors[intersect(names(trend_colors), present)]
  } else {
    .tsn_state_colors(states)
  }
  list(states = as.character(states), colors = colors)
}
