test_that("state visibility retains aligned states for plotting", {
  values <- c(3, 1, 4, 2, 5, 3, 6, 2, 7)
  result <- tsn(
    values,
    method = "visibility",
    unit = "state",
    discretization = "gaussian",
    n_states = 3L,
    seed = 12L
  )
  source <- as.data.frame(result, what = "series")

  expect_identical(nrow(source), length(values))
  expect_true("state" %in% names(source))
  expect_false(anyNA(source$state))
})

test_that("base plot supports vertical and horizontal state shading", {
  result <- tsn(
    c(3, 1, 4, 2, 5, 3, 6, 2, 7),
    method = "visibility",
    unit = "state",
    discretization = "quantile",
    n_states = 3L
  )
  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output)
  on.exit({
    grDevices::dev.off()
    unlink(output)
  }, add = TRUE)

  expect_invisible(plot(result, "series", overlay = "vertical", trend = TRUE))
  expect_invisible(plot(result, "series", overlay = "horizontal", points = FALSE))
})

test_that("raw-series plot remains available without state shading", {
  result <- tsn(c(3, 1, 4, 2, 5))
  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output)
  on.exit({
    grDevices::dev.off()
    unlink(output)
  }, add = TRUE)

  expect_invisible(plot(result, "series", overlay = "vertical"))
})

test_that("generic network plot defaults are readable and caller-overridable", {
  defaults <- tsn:::.tsn_network_plot_arguments()
  customized <- tsn:::.tsn_network_plot_arguments(
    layout = "circle",
    labels = TRUE,
    node_fill = "#F28E2B"
  )

  expect_identical(defaults$layout, "spring")
  expect_false(defaults$labels)
  expect_identical(defaults$scale_nodes_by, "degree")
  expect_identical(defaults$node_size_range, c(2, 9))
  expect_identical(defaults$edge_label_style, "none")
  expect_identical(customized$layout, "circle")
  expect_true(customized$labels)
  expect_identical(customized$node_fill, "#F28E2B")
  expect_identical(customized$edge_color, "#B9C2CC")
})

test_that("state segments follow midpoint boundaries in both orientations", {
  vertical <- tsn:::.tsn_overlay_segments(
    position = c(1, 2, 4, 7),
    states = c("a", "a", "b", "b"),
    sort_position = FALSE
  )
  horizontal <- tsn:::.tsn_overlay_segments(
    position = c(4, 1, 3, 2),
    states = c("high", "low", "high", "low"),
    sort_position = TRUE
  )

  expect_equal(vertical$start, c(0.5, 3))
  expect_equal(vertical$end, c(3, 8.5))
  expect_identical(vertical$state, c("a", "b"))
  expect_equal(horizontal$start, c(0.5, 2.5))
  expect_equal(horizontal$end, c(2.5, 4.5))
  expect_identical(horizontal$state, c("low", "high"))
  expect_equal(tsn:::.tsn_expand_range(c(2, 2)), c(1.92, 2.08))
  expect_equal(tsn:::.tsn_expand_range(c(0, 0)), c(-0.5, 0.5))
})

test_that("multi-series plots support predictable scale and selection controls", {
  data <- data.frame(
    id = rep(c("a", "b", "c"), each = 5L),
    time = rep(seq_len(5L), times = 3L),
    value = c(1:5, 10 * (1:5), c(3, 1, 4, 2, 5)),
    state = rep(c("low", "low", "middle", "high", "high"), times = 3L)
  )
  result <- tsn(
    data,
    value = "value",
    id = "id",
    time = "time",
    method = "visibility",
    unit = "state",
    state = "state"
  )
  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output)
  on.exit({
    grDevices::dev.off()
    unlink(output)
  }, add = TRUE)

  invisible(lapply(
    c("fixed", "free", "free_x", "free_y"),
    function(scale) expect_invisible(plot(
      result,
      "series",
      overlay = "vertical",
      scales = scale,
      columns = 2L,
      max_series = 2L
    ))
  ))
  expect_error(plot(result, "series", scales = "unknown"))
  expect_error(plot(result, "series", max_series = 0L))
  expect_error(plot(result, "series", series = character()), "At least one")
})
