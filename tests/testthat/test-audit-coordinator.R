# Cross-file fixes applied by the audit coordinator after the per-file
# audits: arguments that one plot method forwarded to a helper but another
# silently dropped.

test_that("heatmap views of states and trends forward `grid`", {
  states <- discretize(c(1, 3, 2, 8, 9, 7, 1, 2, 9, 8, 3, 2), n_states = 3)
  trends <- trend(c(1, 2, 3, 4, 5, 4, 3, 2, 1, 2, 3, 4), window = 3)
  seen <- new.env()
  testthat::local_mocked_bindings(
    .tsn_plot_state_heatmap = function(source, ..., grid = FALSE) {
      seen$grid <- c(seen$grid, grid)
      invisible(NULL)
    },
    .package = "tsn"
  )
  plot(states, type = "heatmap", grid = TRUE)
  plot(trends, type = "heatmap", grid = TRUE)
  expect_identical(seen$grid, c(TRUE, TRUE))
})

test_that("a ribbon classified at different times is rejected", {
  values <- c(1, 3, 2, 8, 9, 7, 1, 2, 9, 8, 3, 2)
  states <- discretize(
    data.frame(id = "a", time = 1:12, value = values),
    value = "value", id = "id", time = "time", n_states = 3
  )
  shifted <- discretize(
    data.frame(id = "a", time = 101:112, value = values),
    value = "value", id = "id", time = "time", n_states = 3
  )
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_error(
    plot(states, type = "ribbon", ribbons = list(other = shifted)),
    "classifies different times"
  )
  expect_no_error(plot(states, type = "ribbon", ribbons = list(same = states)))
})
