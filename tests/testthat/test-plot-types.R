# New plot types: ribbon, heatmap, joint (states) and ribbon, heatmap,
# panels (trend).

.make_state_fixture <- function() {
  set.seed(5)
  long <- data.frame(
    id = rep(c("a", "b", "c"), each = 40L),
    time = rep(seq_len(40L), times = 3L),
    value = as.numeric(vapply(
      seq_len(3L),
      function(index) cumsum(rnorm(40L)) + index,
      numeric(40L)
    ))
  )
  discretize(long, value = "value", id = "id", time = "time",
             method = "quantile", n_states = 3L)
}

.make_trend_fixture <- function() {
  set.seed(6)
  long <- data.frame(
    id = rep(c("a", "b"), each = 50L),
    time = rep(seq_len(50L), times = 2L),
    value = c(
      cumsum(rnorm(50L, mean = 0.3)),
      sin(seq_len(50L) / 4) + rnorm(50L, sd = 0.2)
    )
  )
  trend(long, value = "value", id = "id", time = "time", window = 7L)
}

test_that("state plot types all render", {
  states <- .make_state_fixture()
  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output)
  on.exit({
    grDevices::dev.off()
    unlink(output)
  }, add = TRUE)

  expect_invisible(plot(states))
  expect_invisible(plot(states, "ribbon"))
  expect_invisible(plot(states, "ribbon", points = FALSE, series = "a"))
  expect_invisible(plot(states, "heatmap"))
  expect_invisible(plot(states, overlay = "horizontal", series = c("a", "b")))
})

test_that("trend plot types all render", {
  classified <- .make_trend_fixture()
  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output)
  on.exit({
    grDevices::dev.off()
    unlink(output)
  }, add = TRUE)

  expect_invisible(plot(classified))
  expect_invisible(plot(classified, "ribbon"))
  expect_invisible(plot(classified, "heatmap"))
  expect_invisible(plot(classified, "panels"))
  expect_invisible(plot(classified, "panels", series = "a"))
})

test_that("plot types validate their inputs", {
  states <- .make_state_fixture()
  classified <- .make_trend_fixture()
  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output)
  on.exit({
    grDevices::dev.off()
    unlink(output)
  }, add = TRUE)

  expect_error(plot(states, "mosaic"))
  expect_error(plot(states, "heatmap", series = "zz"), "Unknown series")
  expect_error(plot(classified, "panels", series = "zz"), "Unknown series")
})

test_that("heatmap and panels respect max_series caps", {
  states <- .make_state_fixture()
  classified <- .make_trend_fixture()
  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output)
  on.exit({
    grDevices::dev.off()
    unlink(output)
  }, add = TRUE)

  expect_invisible(plot(states, "heatmap", max_series = 2L))
  expect_invisible(plot(classified, "panels", max_series = 1L))
})

test_that("device state is restored after the new plot types", {
  states <- .make_state_fixture()
  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output)
  on.exit({
    grDevices::dev.off()
    unlink(output)
  }, add = TRUE)
  before <- graphics::par(c("mar", "mfrow"))
  plot(states, "heatmap")
  plot(states, "ribbon")
  after <- graphics::par(c("mar", "mfrow"))
  expect_identical(before, after)
})

test_that("stack view and both-overlay render and validate", {
  states <- .make_state_fixture()
  companion <- seq_len(nrow(states)) + rnorm(nrow(states))
  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output)
  on.exit({
    grDevices::dev.off()
    unlink(output)
  }, add = TRUE)

  expect_invisible(plot(states, "stack", with = companion))
  expect_invisible(plot(states, "stack", with = companion,
                        color_line = TRUE, points = FALSE, shade = FALSE))
  expect_invisible(plot(states, "stack", with = companion, series = "a",
                        with_label = "Original"))
  expect_error(plot(states, "stack"), "requires `with`")
  expect_error(plot(states, "ribbon", with = companion), "only valid")
  expect_error(plot(states, "stack", with = companion[-1]))
  expect_error(plot(states, "stack", with = companion, series = "zz"),
               "Unknown series")
})

test_that("shading combines with dashed guide lines", {
  states <- .make_state_fixture()
  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output)
  on.exit({
    grDevices::dev.off()
    unlink(output)
  }, add = TRUE)

  expect_invisible(plot(states, series = "a", overlay = "vertical",
                        lines = "horizontal"))
  expect_invisible(plot(states, series = "a", overlay = "horizontal",
                        lines = "vertical", min_run = 3))
  expect_invisible(plot(states, overlay = "none", lines = "both"))
  expect_invisible(plot(states, series = "a", lines = "horizontal",
                        points = FALSE))
  expect_error(plot(states, lines = "diagonal"))
  expect_error(plot(states, lines = "vertical", min_run = 0))
  expect_error(plot(states, overlay = "both"))
})

test_that("state boundaries fall back to empirical midpoints", {
  states <- .make_state_fixture()
  source <- data.frame(
    id = as.character(states$id), time = states$time, value = states$value,
    state = as.character(states$state), stringsAsFactors = FALSE
  )
  from_breaks <- .tsn_state_boundaries(source, attr(states, "breaks"))
  expect_true(length(from_breaks) >= 1L)
  expect_true(all(is.finite(from_breaks)))
  empirical <- .tsn_state_boundaries(source, NULL)
  expect_true(length(empirical) >= 1L)
  expect_true(all(is.finite(empirical)))
})

test_that("multi-ribbon view stacks classifications for one series", {
  states <- .make_state_fixture()
  # Trend on the SAME observations as the state fixture so rows align.
  classified <- trend(
    data.frame(id = states$id, time = states$time, value = states$value),
    value = "value", id = "id", time = "time", window = 7L
  )
  n_a <- sum(as.character(states$id) == "a")
  extra <- rep(c("x", "y"), length.out = n_a)
  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output)
  on.exit({
    grDevices::dev.off()
    unlink(output)
  }, add = TRUE)

  expect_invisible(plot(states, "ribbon", series = "a",
                        ribbons = list(Trend = classified)))
  expect_invisible(plot(states, "ribbon", series = "a",
                        ribbons = list(Trend = classified, Extra = extra),
                        points = FALSE, ribbon_label = "Quantile"))
  expect_error(plot(states, "ribbon", ribbons = list(Trend = classified)),
               "one series")
  expect_error(plot(states, "overlay", ribbons = list(Trend = classified)),
               "only valid")
  expect_error(plot(states, "ribbon", series = "a",
                    ribbons = list(Bad = extra[-1])),
               "states for series")
  expect_error(plot(states, "ribbon", series = "a",
                    ribbons = list(Bad = 1:5)))
})
