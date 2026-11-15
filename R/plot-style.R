#' Resolve the Shared Plot Style
#'
#' Central style contract for every tsn plot: panel background, grid,
#' axis, frame, and text styling plus global size scaling. Every public
#' plot method exposes these as arguments and forwards them here.
#'
#' @param cex Global size multiplier for text.
#' @param grid Whether to draw the background grid.
#' @param background Panel background colour.
#' @param grid_color Grid line colour.
#' @param axis_color Axis tick and label colour.
#' @param text_color Title and emphasis colour.
#' @param frame_color Panel frame colour, or `NA` for no frame.
#' @return A named list of style constants.
#' @noRd
.tsn_style <- function(cex = 1, grid = TRUE, background = "#FFFFFF",
                       grid_color = "#ECEEF0", axis_color = "#6B7280",
                       text_color = "#1F2937", frame_color = "#D8DCE0") {
  stopifnot(
    "`cex` must be a single positive finite number" =
      is.numeric(cex) && length(cex) == 1L && is.finite(cex) && cex > 0,
    "`grid` must be TRUE or FALSE" =
      is.logical(grid) && length(grid) == 1L && !is.na(grid)
  )
  list(
    cex = cex, grid = grid, background = background,
    grid_color = grid_color, axis_color = axis_color,
    text_color = text_color, frame_color = frame_color
  )
}

#' Open a Styled Panel
#'
#' Draws the empty panel every renderer builds on: background fill, soft
#' grid, minimal tick-only axes (horizontal y labels), a light frame, and
#' a left-aligned title. Date axes keep date labels.
#'
#' @param x,y Data vectors (used for class-aware axes and limits).
#' @param xlim,ylim Axis limits.
#' @param main,xlab,ylab Panel labels.
#' @param style A style list from `.tsn_style()`.
#' @param y_axis Whether to draw the y axis.
#' @param time_labels Optional character labels for an ordinal time axis:
#'   `x` then holds integer positions into `time_labels` (see
#'   `.tsn_plot_time()`), and the axis prints the labels instead of the
#'   positions.
#' @return The x-axis tick positions, invisibly (so a caller that draws its
#'   own grid can align it with the ticks).
#' @noRd
.tsn_panel <- function(x, y, xlim = NULL, ylim = NULL, main = "",
                       xlab = "Time", ylab = "Value", style = .tsn_style(),
                       y_axis = TRUE, time_labels = NULL) {
  if (is.null(xlim)) {
    xlim <- range(x, na.rm = TRUE)
  }
  if (is.null(ylim)) {
    ylim <- .tsn_expand_range(y)
  }
  graphics::plot(
    x = x, y = seq_along(x) * 0 + ylim[1L],
    type = "n", xlim = xlim, ylim = ylim, axes = FALSE, ann = FALSE
  )
  limits <- graphics::par("usr")
  graphics::rect(limits[1L], limits[3L], limits[2L], limits[4L],
                 col = style$background, border = NA)
  # Draw the time axis first so the vertical grid uses the tick positions
  # the axis actually chose: axis.Date()/axis.POSIXct() pick calendar ticks
  # that need not coincide with the numeric axTicks(1).
  x_axis <- list(
    side = 1L, col = NA, col.ticks = style$axis_color,
    col.axis = style$axis_color, cex.axis = 0.8 * style$cex,
    tcl = -0.25, lwd.ticks = 0.8, padj = -0.6
  )
  x_ticks <- if (!is.null(time_labels)) {
    ticks <- graphics::axTicks(1L)
    ticks <- ticks[ticks >= 1 & ticks <= length(time_labels) &
                     abs(ticks - round(ticks)) < sqrt(.Machine$double.eps)]
    ticks <- round(ticks)
    do.call(graphics::axis, c(x_axis, list(at = ticks,
                                           labels = time_labels[ticks])))
  } else if (inherits(x, "Date")) {
    do.call(graphics::axis.Date, c(x_axis, list(x = x)))
  } else if (inherits(x, "POSIXt")) {
    do.call(graphics::axis.POSIXct, c(x_axis, list(x = x)))
  } else {
    do.call(graphics::axis, x_axis)
  }
  if (style$grid) {
    graphics::abline(v = x_ticks, col = style$grid_color, lwd = 0.9)
    graphics::abline(h = graphics::axTicks(2L), col = style$grid_color,
                     lwd = 0.9)
  }
  if (y_axis) {
    graphics::axis(
      2L, las = 1L, col = NA, col.ticks = style$axis_color,
      col.axis = style$axis_color, cex.axis = 0.8 * style$cex,
      tcl = -0.25, lwd.ticks = 0.8, hadj = 0.85
    )
  }
  if (!is.na(style$frame_color)) {
    graphics::box(col = style$frame_color, lwd = 0.9)
  }
  if (nzchar(main)) {
    graphics::title(
      main = main, adj = 0, line = 0.7, font.main = 2,
      cex.main = 0.95 * style$cex, col.main = style$text_color
    )
  }
  if (nzchar(xlab)) {
    graphics::title(xlab = xlab, line = 1.9, cex.lab = 0.85 * style$cex,
                    col.lab = style$axis_color)
  }
  if (nzchar(ylab)) {
    graphics::title(ylab = ylab, line = 2.6, cex.lab = 0.85 * style$cex,
                    col.lab = style$axis_color)
  }
  invisible(x_ticks)
}

#' Named Palette Presets
#'
#' Resolves a palette specification to `n` colours. Accepts a preset name
#' (`"default"`, `"okabe"`, `"viridis"`, `"cool"`, `"warm"`, `"pastel"`,
#' `"dark"`), a vector of colours (recycled/truncated to `n`, names kept),
#' or `NULL` for the default preset. `"okabe"` provides at most its nine
#' colour-blind-safe colours and rejects larger requests.
#'
#' @param palette Palette specification.
#' @param n Number of colours needed.
#' @return A character vector of `n` colours.
#' @noRd
.tsn_palette_colors <- function(palette, n) {
  tableau <- c(
    "#4E79A7", "#F28E2B", "#59A14F", "#E15759", "#B07AA1",
    "#76B7B2", "#EDC948", "#FF9DA7", "#9C755F", "#BAB0AC"
  )
  if (is.null(palette)) {
    palette <- "default"
  }
  if (is.character(palette) && length(palette) == 1L && is.null(names(palette)) &&
        palette %in% c("default", "okabe", "viridis", "cool", "warm",
                       "pastel", "dark")) {
    colors <- switch(
      palette,
      default = if (n <= length(tableau)) tableau[seq_len(n)] else
        .tsn_spectral_colors(n),
      okabe = {
        # The Okabe-Ito palette has exactly nine colour-blind-safe colours;
        # recycling or padding would silently break the safety guarantee.
        if (n > 9L) {
          stop(sprintf(
            paste0(
              "The \"okabe\" palette provides 9 colour-blind-safe colours, ",
              "but %d are needed. Reduce the number of states or choose ",
              "another palette."
            ), n
          ), call. = FALSE)
        }
        grDevices::palette.colors(max(2L, n), palette = "Okabe-Ito")[seq_len(n)]
      },
      viridis = grDevices::hcl.colors(n, palette = "Viridis"),
      cool = grDevices::hcl.colors(n, palette = "Teal"),
      warm = grDevices::hcl.colors(n, palette = "Peach", rev = TRUE),
      pastel = grDevices::hcl.colors(n, palette = "Pastel 1"),
      dark = grDevices::hcl.colors(n, palette = "Dark 3")
    )
    return(unname(colors))
  }
  if (!is.character(palette) || length(palette) == 0L) {
    stop(paste0(
      "`palette` must be NULL, a preset name (\"default\", \"okabe\", ",
      "\"viridis\", \"cool\", \"warm\", \"pastel\", \"dark\"), or a ",
      "character vector of colours."
    ), call. = FALSE)
  }
  invalid <- palette[!.tsn_is_color(palette)]
  if (length(invalid) > 0L) {
    stop(sprintf(
      paste0(
        "`palette` contains value(s) that are neither a preset name nor a ",
        "colour: %s. Presets are \"default\", \"okabe\", \"viridis\", ",
        "\"cool\", \"warm\", \"pastel\", and \"dark\"."
      ),
      paste0("\"", unique(invalid), "\"", collapse = ", ")
    ), call. = FALSE)
  }
  if (length(palette) < n) {
    stop(sprintf(
      "`palette` supplies %d colour(s) but %d are needed.",
      length(palette), n
    ), call. = FALSE)
  }
  palette[seq_len(n)]
}

#' Validate a Single Flag, Number, or Count
#'
#' Scalar predicates shared by the renderers' named `stopifnot()` contracts.
#'
#' @param x Value to test.
#' @return `TRUE` or `FALSE`.
#' @noRd
.tsn_is_flag <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}

#' @noRd
.tsn_is_number <- function(x) {
  is.numeric(x) && length(x) == 1L && is.finite(x)
}

#' @noRd
.tsn_is_count <- function(x) {
  .tsn_is_number(x) && x >= 1 &&
    abs(x - round(x)) < sqrt(.Machine$double.eps)
}

#' Restore the Graphics State Saved Before a Multi-Panel Plot
#'
#' A plain `par(previous)` is not enough: setting `mfrow`/`mfcol` (which
#' `layout()` changed) resets `cex` to 1 as a side effect, so a caller's
#' `par(cex = 0.9)` would be lost and line-based margins would be converted
#' to inches at the wrong `cex`. The grid parameters are therefore restored
#' first and every other parameter after them. (A caller who set margins in
#' inches, `mai`, gets them back via `mar` at the restored `cex`.)
#'
#' @param previous The result of `par(no.readonly = TRUE)` taken before the
#'   plot changed anything.
#' @return `NULL`, invisibly.
#' @noRd
.tsn_restore_par <- function(previous) {
  graphics::layout(1L)
  grid_parameters <- c("mfrow", "mfcol")
  graphics::par(previous[grid_parameters])
  graphics::par(previous[setdiff(names(previous), grid_parameters)])
  invisible(NULL)
}

#' Map Time Values to Plot Positions
#'
#' Numeric, `Date`, and `POSIXct` times are plotted as they are. Character
#' (or factor) times are ordered labels -- tsn keeps non-ISO character times
#' in caller order -- so they are drawn at integer positions `1..k` (the
#' order of first appearance, or the factor levels) and the axis prints the
#' labels.
#'
#' @param time A time vector.
#' @return A list with `position` (plottable) and `labels` (`NULL`, or the
#'   character labels indexed by `position`).
#' @noRd
.tsn_plot_time <- function(time) {
  if (is.factor(time)) {
    labels <- levels(droplevels(time))
    return(list(position = match(as.character(time), labels),
                labels = labels))
  }
  if (is.character(time)) {
    labels <- unique(time)
    return(list(position = match(time, labels), labels = labels))
  }
  list(position = time, labels = NULL)
}

#' Spectral Colours Without Missing Entries
#'
#' `grDevices::hcl.colors(n, "Spectral")` returns `NA` for its neutral
#' midpoint at some odd `n` (11, 21, 23, 27, ... in R 4.5.2): a rounding
#' edge in the diverging-x trajectory. An `NA` fill is invisible, so a state
#' would silently vanish from the plot and the legend. The midpoint of the
#' palette is the same colour for every odd `n`, so it is taken from the
#' three-colour palette.
#'
#' @param n Number of colours (`n >= 1`).
#' @return A character vector of `n` colours without `NA`.
#' @noRd
.tsn_spectral_colors <- function(n) {
  colors <- grDevices::hcl.colors(n, palette = "Spectral")
  missing <- is.na(colors)
  if (any(missing)) {
    colors[missing] <- grDevices::hcl.colors(3L, palette = "Spectral")[2L]
  }
  if (anyNA(colors)) {
    stop("Could not resolve the default palette to valid colours.",
         call. = FALSE)
  }
  colors
}

#' Test Whether Values Are Valid Colours
#'
#' @param colors A character vector.
#' @return A logical vector, `TRUE` where the value is a colour name or hex
#'   code that `grDevices::col2rgb()` accepts. `NA` is not a colour here: it
#'   draws nothing.
#' @noRd
.tsn_is_color <- function(colors) {
  vapply(colors, function(color) {
    if (is.na(color)) {
      return(FALSE)
    }
    tryCatch({
      grDevices::col2rgb(color)
      TRUE
    }, error = function(condition) FALSE)
  }, logical(1L), USE.NAMES = FALSE)
}

#' Reject Unrecognised Plotting Arguments
#'
#' The internal renderers keep a trailing `...` so callers can forward their
#' own `...`; without this check a misspelled option (`overlays =`,
#' `pallete =`) would be dropped without a trace.
#'
#' @param ... The renderer's unconsumed arguments.
#' @return `NULL`, invisibly; errors when `...` is not empty.
#' @noRd
.tsn_reject_dots <- function(...) {
  if (...length() == 0L) {
    return(invisible(NULL))
  }
  extra <- names(list(...))
  extra <- if (is.null(extra)) {
    rep("<unnamed>", ...length())
  } else {
    ifelse(nzchar(extra), extra, "<unnamed>")
  }
  stop(sprintf(
    "Unknown plotting argument(s): %s.",
    paste0("`", extra, "`", collapse = ", ")
  ), call. = FALSE)
}

#' Draw the Styled Legend Row
#'
#' @param labels Legend labels.
#' @param colors Fill or line colours aligned with `labels`.
#' @param title Legend title.
#' @param style Style list.
#' @param kind `"fill"` for swatches, `"line"` for line keys,
#'   `"point"` for point keys.
#' @param pch Point symbol(s) for `kind = "point"`: one value, or one per
#'   label so a state can be keyed by shape as well as colour.
#' @return `NULL`, invisibly.
#' @noRd
.tsn_legend_row <- function(labels, colors, title, style,
                            kind = c("fill", "line", "point"), pch = 19L) {
  kind <- match.arg(kind)
  stopifnot(
    "`colors` must align with `labels`" = length(colors) == length(labels),
    "`pch` must have length 1 or one value per label" =
      length(pch) %in% c(1L, length(labels))
  )
  graphics::par(mar = rep.int(0, 4L))
  graphics::plot.new()
  arguments <- list(
    x = "center", legend = labels, bty = "n",
    horiz = length(labels) <= 6L,
    ncol = if (length(labels) <= 6L) 1L else ceiling(length(labels) / 2L),
    cex = 0.82 * style$cex, title = title, title.font = 2L,
    title.cex = 0.85 * style$cex, text.col = style$text_color,
    x.intersp = 0.7, seg.len = 1.2
  )
  if (kind == "fill") {
    arguments$fill <- colors
    arguments$border <- NA
  } else if (kind == "line") {
    arguments$col <- colors
    arguments$lwd <- 2.4
  } else {
    arguments$col <- colors
    arguments$pch <- pch
    arguments$pt.cex <- 1.1
  }
  do.call(graphics::legend, arguments)
  invisible(NULL)
}
