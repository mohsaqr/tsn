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
    is.numeric(cex), length(cex) == 1L, is.finite(cex), cex > 0,
    is.logical(grid), length(grid) == 1L, !is.na(grid)
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
#' @return `NULL`, invisibly.
#' @noRd
.tsn_panel <- function(x, y, xlim = NULL, ylim = NULL, main = "",
                       xlab = "Time", ylab = "Value", style = .tsn_style(),
                       y_axis = TRUE) {
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
  if (style$grid) {
    graphics::abline(v = graphics::axTicks(1L), col = style$grid_color,
                     lwd = 0.9)
    graphics::abline(h = graphics::axTicks(2L), col = style$grid_color,
                     lwd = 0.9)
  }
  if (inherits(x, "Date")) {
    graphics::axis.Date(
      1L, x = x, col = NA, col.ticks = style$axis_color,
      col.axis = style$axis_color, cex.axis = 0.8 * style$cex,
      tcl = -0.25, lwd.ticks = 0.8, padj = -0.6
    )
  } else {
    graphics::axis(
      1L, col = NA, col.ticks = style$axis_color,
      col.axis = style$axis_color, cex.axis = 0.8 * style$cex,
      tcl = -0.25, lwd.ticks = 0.8, padj = -0.6
    )
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
  invisible(NULL)
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
        grDevices::hcl.colors(n, palette = "Spectral"),
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
  stopifnot(is.character(palette), length(palette) >= 1L)
  if (length(palette) < n) {
    stop(sprintf(
      "`palette` supplies %d colour(s) but %d are needed.",
      length(palette), n
    ), call. = FALSE)
  }
  palette[seq_len(n)]
}

#' Draw the Styled Legend Row
#'
#' @param labels Legend labels.
#' @param colors Fill or line colours aligned with `labels`.
#' @param title Legend title.
#' @param style Style list.
#' @param kind `"fill"` for swatches, `"line"` for line keys,
#'   `"point"` for point keys.
#' @return `NULL`, invisibly.
#' @noRd
.tsn_legend_row <- function(labels, colors, title, style, kind = "fill") {
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
    arguments$pch <- 19L
    arguments$pt.cex <- 1.1
  }
  do.call(graphics::legend, arguments)
  invisible(NULL)
}
