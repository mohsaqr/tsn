# Audit of R/plot.R and R/plot-style.R: graphics-state hygiene, palette
# contracts, argument strictness, and the geometry helpers.

# Draw `expr` on a null PDF device and return the observed warnings, after
# asserting the graphics state is unchanged (cex included: restoring mfrow
# resets it).
.audit_draw <- function(expr, ignore = character()) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::plot.new()
  graphics::par(cex = 0.9, mar = c(4, 4, 3, 1))
  before <- graphics::par(no.readonly = TRUE)
  warnings <- character()
  withCallingHandlers(
    force(expr),
    warning = function(condition) {
      warnings <<- c(warnings, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )
  after <- graphics::par(no.readonly = TRUE)
  keep <- setdiff(names(before), ignore)
  expect_identical(after[keep], before[keep])
  warnings
}

.audit_source <- function(n_series = 1L, n = 20L, n_states = 3L,
                          time = NULL) {
  do.call(rbind, lapply(seq_len(n_series), function(index) {
    data.frame(
      id = sprintf("s%02d", index),
      time = if (is.null(time)) seq_len(n) else time,
      value = sin(seq_len(n) * 0.7 + index),
      state = factor(
        rep_len(letters[seq_len(n_states)], n),
        levels = letters[seq_len(n_states)]
      ),
      stringsAsFactors = FALSE
    )
  }))
}

test_that("every series view of every network family restores par", {
  wide <- data.frame(a = c(3, 1, 4, 2, 5, 3, 6, 2, 7),
                     b = c(2, 7, 1, 8, 2, 8, 1, 8, 2),
                     c = c(1, 4, 1, 5, 9, 2, 6, 5, 3))
  networks <- list(
    distance_series = tsn(wide, method = "distance", unit = "series"),
    distance_time = tsn(wide$a, method = "distance", unit = "time"),
    distance_window = tsn(wide$a, method = "distance", unit = "window",
                          window = 3L),
    visibility_time = tsn(wide$a, "hvg"),
    visibility_state = tsn(wide$a, method = "visibility", unit = "state",
                           discretization = "quantile")
  )
  warnings <- unlist(lapply(networks, function(network) {
    c(
      .audit_draw(plot(network, "series")),
      .audit_draw(plot(network, "series", overlay = "vertical",
                       points = TRUE, trend = TRUE)),
      .audit_draw(plot(network, "series", overlay = "none",
                       scales = "fixed"))
    )
  }))
  expect_length(warnings, 0L)
})

test_that("network plots leave par untouched apart from plot coordinates", {
  skip_if_not_installed("cograph")
  network <- tsn(c(3, 1, 4, 2, 5, 3, 6, 2, 7), "hvg")
  # usr/xaxp/yaxp describe the coordinate system of the plot just drawn;
  # leaving them is what lets a caller annotate it, as after any base plot.
  warnings <- .audit_draw(plot(network), ignore = c("usr", "xaxp", "yaxp"))
  expect_length(warnings, 0L)
})

test_that("every tsn_states view restores par, including cex", {
  states <- discretize(
    data.frame(id = rep(c("a", "b"), each = 12L), t = rep(seq_len(12L), 2L),
               v = c(sin(seq_len(12L)), cos(seq_len(12L)))),
    value = "v", id = "id", time = "t", method = "quantile", n_states = 3L
  )
  warnings <- c(
    .audit_draw(plot(states, overlay = "vertical", lines = "both")),
    .audit_draw(plot(states, "ribbon")),
    .audit_draw(plot(states, "heatmap", sort = "state", border = TRUE)),
    .audit_draw(plot(states, "stack", with = seq_len(24L))),
    .audit_draw(plot(states, "ribbon", series = "a",
                     ribbons = list(Again = states)))
  )
  expect_length(warnings, 0L)
})

test_that("par and cex are restored when a renderer errors midway", {
  source <- .audit_source(n_series = 2L)
  expect_error(.audit_draw(
    tsn:::.tsn_plot_series_frame(source, line_color = "not-a-colour")
  ), "invalid color")
  expect_error(.audit_draw(
    tsn:::.tsn_plot_state_heatmap(source, main = NULL)
  ))
  expect_error(.audit_draw(
    tsn:::.tsn_plot_state_stack(source, with = seq_len(40L), xlab = NULL)
  ))
  one <- .audit_source()
  ribbons <- list(R = list(states = as.character(one$state),
                           colors = tsn:::.tsn_state_colors(one$state)))
  expect_error(.audit_draw(
    tsn:::.tsn_plot_ribbon_stack(one, ribbons, xlab = NULL)
  ))
  # .audit_draw's par assertion runs only when expr returns, so check the
  # error path explicitly too.
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::plot.new()
  # Set cex and margins together: after par(cex = ) alone, par("mai") keeps
  # reporting the inches of the old cex, a value no restore can reproduce.
  graphics::par(cex = 0.9, mar = c(4, 4, 3, 1))
  before <- graphics::par(no.readonly = TRUE)
  expect_error(tsn:::.tsn_plot_series_frame(source, xlab = NULL))
  expect_identical(graphics::par(no.readonly = TRUE), before)
  expect_error(tsn:::.tsn_plot_state_heatmap(source, main = NULL))
  expect_identical(graphics::par(no.readonly = TRUE), before)
})

test_that("1 vs 40 series and 1, 9, 12 states draw without warnings", {
  warnings <- c(
    .audit_draw(tsn:::.tsn_plot_series_frame(.audit_source(1L),
                                             overlay = "vertical")),
    .audit_draw(tsn:::.tsn_plot_series_frame(.audit_source(40L),
                                             max_series = 40L)),
    .audit_draw(tsn:::.tsn_plot_state_heatmap(.audit_source(40L),
                                              max_series = 40L)),
    .audit_draw(tsn:::.tsn_plot_series_frame(
      .audit_source(n_states = 1L), lines = "both", strip = TRUE
    )),
    .audit_draw(tsn:::.tsn_plot_series_frame(
      .audit_source(n = 60L, n_states = 9L), palette = "okabe"
    )),
    .audit_draw(tsn:::.tsn_plot_series_frame(
      .audit_source(n = 60L, n_states = 12L), overlay = "vertical"
    ))
  )
  expect_length(warnings, 0L)
})

test_that("short, constant, and calendar-time series draw cleanly", {
  dates <- as.Date("2024-01-01") + seq_len(20L)
  stamps <- as.POSIXct("2024-01-01", tz = "UTC") + 3600 * seq_len(20L)
  constant <- .audit_source()
  constant$value <- 5
  warnings <- c(
    .audit_draw(tsn:::.tsn_plot_series_frame(
      .audit_source(n = 1L), overlay = "vertical", strip = TRUE,
      lines = "both", trend = TRUE, points = TRUE
    )),
    .audit_draw(tsn:::.tsn_plot_series_frame(
      .audit_source(n = 2L), overlay = "horizontal", strip = TRUE,
      lines = "both", trend = TRUE, points = TRUE
    )),
    .audit_draw(tsn:::.tsn_plot_state_heatmap(.audit_source(n = 1L))),
    .audit_draw(tsn:::.tsn_plot_state_stack(.audit_source(n = 1L),
                                            with = 1)),
    .audit_draw(tsn:::.tsn_plot_series_frame(constant, lines = "both",
                                             trend = TRUE)),
    .audit_draw(tsn:::.tsn_plot_series_frame(
      .audit_source(2L, time = dates), overlay = "vertical", strip = TRUE,
      lines = "both", trend = TRUE
    )),
    .audit_draw(tsn:::.tsn_plot_series_frame(
      .audit_source(2L, time = stamps), overlay = "vertical", strip = TRUE,
      lines = "both", trend = TRUE
    )),
    .audit_draw(tsn:::.tsn_plot_state_heatmap(.audit_source(3L,
                                                            time = stamps))),
    .audit_draw(tsn:::.tsn_plot_state_stack(.audit_source(time = dates),
                                            with = seq_len(20L),
                                            color_line = TRUE))
  )
  expect_length(warnings, 0L)
})

test_that("POSIXct and Date panels get calendar axes, not raw numbers", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  stamps <- as.POSIXct("2024-01-01", tz = "UTC") + 3600 * seq_len(10L)
  dates <- as.Date("2024-01-01") + seq_len(30L)
  # The returned ticks are those the axis drew; the old code drew POSIXct
  # times through axis(), labelling them as epoch seconds (1704070000).
  expect_s3_class(tsn:::.tsn_panel(stamps, seq_len(10L)), "POSIXct")
  expect_s3_class(tsn:::.tsn_panel(dates, seq_len(30L)), "Date")
  expect_type(tsn:::.tsn_panel(seq_len(5L), seq_len(5L)), "double")
})

test_that("character (label) time is drawn at ordinal positions", {
  data <- data.frame(id = "a", t = c("b", "a", "c", "e", "d"),
                     v = c(3, 1, 4, 2, 5))
  mapped <- tsn:::.tsn_plot_time(data$t)
  expect_identical(mapped$position, seq_len(5L))
  expect_identical(mapped$labels, c("b", "a", "c", "e", "d"))
  expect_identical(tsn:::.tsn_plot_time(factor(c("y", "x"),
                                               levels = c("y", "x", "z"))),
                   list(position = c(1L, 2L), labels = c("y", "x")))
  expect_identical(tsn:::.tsn_plot_time(c(2, 5))$labels, NULL)

  network <- tsn(data, value = "v", id = "id", time = "t",
                 method = "visibility", unit = "state",
                 discretization = "quantile")
  states <- discretize(data, value = "v", id = "id", time = "t",
                       method = "quantile")
  # Before the fix each call warned "NAs introduced by coercion" and drew
  # nothing on the time axis.
  warnings <- c(
    .audit_draw(plot(network, "series", overlay = "vertical",
                     lines = "both", trend = TRUE, points = TRUE)),
    .audit_draw(plot(states)),
    .audit_draw(plot(states, "ribbon")),
    .audit_draw(plot(states, "heatmap")),
    .audit_draw(plot(states, "stack", with = seq_len(5L))),
    .audit_draw(plot(states, "ribbon", ribbons = list(Again = states)))
  )
  expect_length(warnings, 0L)
})

test_that("the default palette never returns a missing colour", {
  # hcl.colors(n, "Spectral") returns NA at its midpoint for these n.
  colours <- lapply(c(11L, 21L, 23L, 27L), function(n) {
    tsn:::.tsn_palette_colors("default", n)
  })
  expect_false(any(vapply(colours, anyNA, logical(1L))))
  expect_identical(lengths(colours), c(11L, 21L, 23L, 27L))
  expect_identical(colours[[1L]][6L],
                   grDevices::hcl.colors(3L, palette = "Spectral")[2L])
  expect_identical(tsn:::.tsn_palette_colors("default", 12L),
                   grDevices::hcl.colors(12L, palette = "Spectral"))
  expect_identical(tsn:::.tsn_palette_colors(NULL, 3L),
                   c("#4E79A7", "#F28E2B", "#59A14F"))
  states <- tsn:::.tsn_state_colors(factor(letters[seq_len(11L)]))
  expect_false(anyNA(states))
})

test_that("okabe stops at nine colours with the documented message", {
  expect_length(tsn:::.tsn_palette_colors("okabe", 9L), 9L)
  expect_error(
    tsn:::.tsn_palette_colors("okabe", 10L),
    "The \"okabe\" palette provides 9 colour-blind-safe colours, but 10"
  )
})

test_that("custom palettes are validated as colours", {
  expect_error(tsn:::.tsn_palette_colors("virdis", 1L),
               "neither a preset name nor a colour")
  expect_error(tsn:::.tsn_palette_colors(c("red", NA), 2L),
               "neither a preset name nor a colour")
  expect_error(tsn:::.tsn_palette_colors(1:3, 2L), "character vector")
  expect_error(tsn:::.tsn_palette_colors("red", 2L), "supplies 1 colour")
  expect_identical(tsn:::.tsn_palette_colors(c("red", "#0000FF", "green"),
                                             2L), c("red", "#0000FF"))
})

test_that("a named palette must cover every state it colours", {
  states <- c("b", "a")
  expect_identical(
    tsn:::.tsn_state_colors(states, palette = c(b = "blue", a = "red",
                                                z = "green")),
    c(a = "red", b = "blue")
  )
  # Before the fix, `c(b = "red", c = "blue")` silently coloured a red.
  expect_error(
    tsn:::.tsn_state_colors(states, palette = c(b = "red", c = "blue")),
    "no colour for state\\(s\\): \"a\""
  )
  expect_error(
    tsn:::.tsn_state_colors(states, palette = c(a = "red", b = "nope")),
    "not colours"
  )
  levels_kept <- tsn:::.tsn_state_colors(
    factor(c("hi", "lo"), levels = c("lo", "mid", "hi"))
  )
  expect_identical(names(levels_kept), c("lo", "hi"))
})

test_that("unrecognised plotting arguments are rejected, not dropped", {
  network <- tsn(c(3, 1, 4, 2, 5, 3, 6, 2, 7), method = "visibility",
                 unit = "state", discretization = "quantile")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_error(plot(network, "series", overlays = "vertical"),
               "Unknown plotting argument\\(s\\): `overlays`")
  source <- .audit_source()
  expect_error(tsn:::.tsn_plot_state_heatmap(source, bogus = 1),
               "`bogus`")
  expect_error(tsn:::.tsn_plot_state_stack(source, with = seq_len(20L),
                                           bogus = 1), "`bogus`")
  ribbons <- list(R = list(states = as.character(source$state),
                           colors = tsn:::.tsn_state_colors(source$state)))
  expect_error(tsn:::.tsn_plot_ribbon_stack(source, ribbons, bogus = 1),
               "`bogus`")
  expect_error(tsn:::.tsn_reject_dots(1, named = 2),
               "`<unnamed>`, `named`")
  expect_null(tsn:::.tsn_reject_dots())
})

test_that("empty-level states give finite horizontal guides, no warnings", {
  data <- data.frame(id = rep(c("a", "b"), each = 6L),
                     t = rep(seq_len(6L), 2L),
                     v = c(1, 2, 1, 2, 1, 2, 5, 9, 5, 9, 1, 9))
  network <- tsn(data, value = "v", id = "id", time = "t",
                 method = "visibility", unit = "state",
                 discretization = "quantile", n_states = 3L)
  one <- subset(as.data.frame(network, what = "series"), id == "a")
  boundaries <- tsn:::.tsn_state_boundaries(one, NULL)
  # Before the fix: c(1.5, Inf) with a min() warning.
  expect_identical(boundaries, 1.5)
  expect_length(.audit_draw(plot(network, "series", series = "a",
                                 lines = "both")), 0L)
  expect_identical(tsn:::.tsn_state_boundaries(one, c(-Inf, 1.2, Inf)), 1.2)
})

test_that("network options pass through to cograph exactly once", {
  defaults <- tsn:::.tsn_network_plot_arguments()
  merged <- tsn:::.tsn_network_plot_arguments(layout = "circle",
                                              title = "T",
                                              node_fill = NULL)
  expect_false(anyDuplicated(names(merged)) > 0L)
  expect_setequal(names(merged), c(names(defaults), "title"))
  expect_identical(merged$layout, "circle")
  expect_identical(merged$title, "T")
  expect_true("node_fill" %in% names(merged))
  expect_null(merged$node_fill)
  expect_error(tsn:::.tsn_network_plot_arguments("circle"), "must be named")
  expect_error(tsn:::.tsn_network_plot_arguments(layout = "a", layout = "b"),
               "more than once: `layout`")

  skip_if_not_installed("cograph")
  network <- tsn(c(3, 1, 4, 2, 5, 3, 6, 2, 7), "hvg")
  received <- NULL
  local_mocked_bindings(
    splot = function(...) {
      received <<- list(...)
      invisible(NULL)
    },
    .package = "cograph"
  )
  expect_invisible(plot(network, layout = "circle", edge_color = "red",
                        title = "T"))
  expect_s3_class(received[[1L]], "tsn")
  passed <- received[-1L]
  expect_false(anyDuplicated(names(passed)) > 0L)
  expect_identical(passed$layout, "circle")
  expect_identical(passed$edge_color, "red")
  expect_identical(passed$title, "T")
  expect_identical(passed$scale_nodes_by, "degree")
  expect_error(plot(network, "network", "circle"), "must be named")
})

test_that("a missing cograph gives an actionable message", {
  local_mocked_bindings(requireNamespace = function(...) FALSE,
                        .package = "base")
  expect_error(tsn:::.tsn_require_cograph(),
               "Network plotting requires the cograph package")
  network <- tsn(c(3, 1, 4, 2, 5), "hvg")
  expect_error(plot(network), "install.packages\\(\"cograph\"\\)")
})

test_that("empty series selections fail early in every view", {
  source <- .audit_source(3L)
  expect_error(tsn:::.tsn_plot_state_heatmap(source, series = character()),
               "At least one series")
  expect_error(tsn:::.tsn_plot_state_stack(source, with = seq_len(60L),
                                           series = character()),
               "At least one series")
  expect_error(tsn:::.tsn_plot_series_frame(source, series = character()),
               "At least one series")
  expect_error(tsn:::.tsn_plot_series_frame(source, columns = 0),
               "`columns` must be NULL or a single positive whole number")
  expect_error(tsn:::.tsn_plot_series_frame(source, columns = 2.5),
               "whole number")
  expect_error(tsn:::.tsn_plot_state_stack(source, with = c(NA, 1:59)),
               "must not contain NA")
})

test_that("too many ribbons is explained, not a bare assertion", {
  one <- .audit_source()
  ribbon <- list(states = as.character(one$state),
                 colors = tsn:::.tsn_state_colors(one$state))
  six <- stats::setNames(rep(list(ribbon), 6L), paste0("R", seq_len(6L)))
  seven <- stats::setNames(rep(list(ribbon), 7L), paste0("R", seq_len(7L)))
  expect_length(.audit_draw(tsn:::.tsn_plot_ribbon_stack(
    one, six, strip_height = 0.08
  )), 0L)
  expect_error(
    tsn:::.tsn_plot_ribbon_stack(one, seven, strip_height = 0.08),
    "7 ribbons do not fit .* at most 6 fit"
  )
  expect_error(tsn:::.tsn_plot_ribbon_stack(one, unname(six)),
               "fully named list")
})

test_that("ribbon resolution validates states and times", {
  states <- discretize(
    data.frame(id = "a", t = seq_len(8L), v = c(1, 5, 2, 6, 3, 7, 4, 8)),
    value = "v", id = "id", time = "t", method = "quantile", n_states = 2L
  )
  resolved <- tsn:::.tsn_resolve_ribbon(states, "a", 8L, "S",
                                        time = seq_len(8L))
  expect_identical(resolved$states, as.character(states$state))
  expect_identical(names(resolved$colors), levels(states$state))
  plain <- tsn:::.tsn_resolve_ribbon(rep(c("x", "y"), 4L), "a", 8L, "P")
  expect_identical(names(plain$colors), c("x", "y"))
  expect_error(tsn:::.tsn_resolve_ribbon(1:8, "a", 8L, "N"),
               "must be a tsn_states")
  expect_error(tsn:::.tsn_resolve_ribbon(states, "a", 7L, "S"),
               "provides 8 states")
  expect_error(tsn:::.tsn_resolve_ribbon(c(NA, rep("x", 7L)), "a", 8L, "M"),
               "missing states")
  expect_error(tsn:::.tsn_resolve_ribbon(states, "a", 8L, "S",
                                         time = seq_len(8L) + 10L),
               "classifies different times")
})

test_that(".tsn_transitions matches a hand count", {
  series <- data.frame(time = seq_len(10L),
                       state = c("a", "a", "b", "b", "b", "a", "c", "c",
                                 "a", "a"))
  # Runs: a(2) b(3) a(1) c(2) a(2); entries at 2.5, 5.5, 6.5, 8.5.
  expect_identical(tsn:::.tsn_transitions(series),
                   data.frame(time = c(2.5, 5.5, 6.5, 8.5),
                              state = c("b", "a", "c", "a")))
  expect_identical(tsn:::.tsn_transitions(series, min_run = 2L)$time,
                   c(2.5, 6.5, 8.5))
  expect_identical(tsn:::.tsn_transitions(series, min_run = 3L)$state, "b")
  expect_identical(nrow(tsn:::.tsn_transitions(series[1L, ])), 0L)
  one_change <- tsn:::.tsn_transitions(
    data.frame(time = as.Date("2024-01-01") + c(0, 2), state = c("x", "y"))
  )
  expect_equal(one_change$time, as.numeric(as.Date("2024-01-02")))
})

test_that(".tsn_expand_range never returns a zero-width range", {
  widths <- vapply(list(rep(5, 4L), rep(0, 3L), rep(-3, 2L), 7,
                        c(NA, Inf, -Inf), numeric(), c(5, NA, 5),
                        c(1e-300, 1e-300)),
                   function(values) diff(tsn:::.tsn_expand_range(values)),
                   numeric(1L))
  expect_true(all(widths > 0))
  expect_identical(tsn:::.tsn_expand_range(rep(0, 3L)), c(-0.5, 0.5))
  expect_equal(tsn:::.tsn_expand_range(rep(5, 3L)), c(4.8, 5.2))
  expect_identical(tsn:::.tsn_expand_range(c(1, 3, NA)), c(1, 3))
})

test_that("overlay segments tile the axis from the first to the last index", {
  segments <- tsn:::.tsn_overlay_segments(c(1, 2, 3, 4, 5),
                                          c("a", "b", "b", "b", "c"),
                                          sort_position = FALSE)
  expect_identical(segments$start, c(0.5, 1.5, 4.5))
  expect_identical(segments$end, c(1.5, 4.5, 5.5))
  expect_identical(segments$state, c("a", "b", "c"))
  # Contiguous: each segment ends where the next starts.
  expect_identical(utils::tail(segments$start, -1L),
                   utils::head(segments$end, -1L))
  single <- tsn:::.tsn_overlay_segments(3, "a", sort_position = FALSE)
  expect_identical(c(single$start, single$end), c(2.5, 3.5))
  expect_error(tsn:::.tsn_overlay_segments(numeric(), character(), FALSE))
})

test_that("heatmap tiles keep full width at the first and last index", {
  source <- .audit_source(n = 2L)
  captured <- NULL
  panel <- tsn:::.tsn_panel
  local_mocked_bindings(.tsn_panel = function(..., xlim) {
    captured <<- xlim
    panel(..., xlim = xlim)
  })
  expect_length(.audit_draw(tsn:::.tsn_plot_state_heatmap(source)), 0L)
  # Times 1 and 2: each tile is one unit wide, so the axis spans 0.5..2.5.
  expect_identical(captured, c(0.5, 2.5))
})

test_that(".tsn_legend_row validates kind and accepts per-label symbols", {
  style <- tsn:::.tsn_style()
  labels <- paste("a very long state label", seq_len(8L))
  colours <- tsn:::.tsn_palette_colors(NULL, 8L)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_invisible(tsn:::.tsn_legend_row(labels, colours, "T", style))
  expect_invisible(tsn:::.tsn_legend_row(labels[1:3], colours[1:3], "T",
                                         style, kind = "line"))
  expect_invisible(tsn:::.tsn_legend_row(labels[1:3], colours[1:3], "T",
                                         style, kind = "point",
                                         pch = c(15L, 17L, 19L)))
  expect_error(tsn:::.tsn_legend_row(labels[1:3], colours[1:3], "T", style,
                                     kind = "lines"), "should be one of")
  expect_error(tsn:::.tsn_legend_row(labels[1:3], colours[1:2], "T", style),
               "must align")
  expect_error(tsn:::.tsn_legend_row(labels[1:3], colours[1:3], "T", style,
                                     kind = "point", pch = 1:2), "`pch`")
})

test_that(".tsn_style overrides individual fields and validates scalars", {
  base <- tsn:::.tsn_style()
  changed <- tsn:::.tsn_style(cex = 1.5, grid_color = "red", frame_color = NA)
  expect_identical(changed$cex, 1.5)
  expect_identical(changed$grid_color, "red")
  expect_identical(changed$frame_color, NA)
  untouched <- setdiff(names(base), c("cex", "grid_color", "frame_color"))
  expect_identical(changed[untouched], base[untouched])
  expect_error(tsn:::.tsn_style(cex = 0), "`cex` must be")
  expect_error(tsn:::.tsn_style(grid = NA), "`grid` must be TRUE or FALSE")
})
