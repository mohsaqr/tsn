test_that("cograph::splot renders every tsn network family", {
  skip_if_not_installed("cograph")
  values <- c(3, 1, 4, 2, 5, 3, 6, 2, 7, 1, 8, 4, 2, 6, 3)
  cases <- list(
    hvg = tsn(values, "hvg"),
    ordinal = tsn(values, "ordinal"),
    windowed_distance = tsn(values, method = "distance", unit = "window",
                            window = 4L),
    directed = tsn(values, "hvg", directed = TRUE)
  )
  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output)
  on.exit({
    grDevices::dev.off()
    unlink(output)
  }, add = TRUE)

  invisible(lapply(cases, function(network) {
    expect_no_error(cograph::splot(network))
  }))
})

test_that("plot.tsn delegates network rendering exclusively to cograph", {
  skip_if_not_installed("cograph")
  network <- tsn(c(3, 1, 4, 2, 5, 3, 6), "nvg")
  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output)
  on.exit({
    grDevices::dev.off()
    unlink(output)
  }, add = TRUE)

  expect_invisible(plot(network, layout = "spring"))
  renderer <- paste(deparse(body(.tsn_plot_network)), collapse = "\n")
  expect_match(renderer, "cograph::splot", fixed = TRUE)
  expect_false(grepl("graphics::", renderer, fixed = TRUE))
  expect_false(exists(".tsn_draw_state_network", envir = asNamespace("tsn")))
})

test_that("tsn results expose the cograph_network contract", {
  network <- tsn(c(3, 1, 4, 2, 5, 3, 6), "nvg")

  expect_s3_class(network, "cograph_network")
  expect_true(is.data.frame(network$nodes))
  expect_true(all(c("id", "label", "name", "x", "y") %in% names(network$nodes)))
  expect_true(is.data.frame(network$edges))
  expect_true(all(c("from", "to", "weight") %in% names(network$edges)))
  expect_true(is.integer(network$edges$from))
  expect_true(is.integer(network$edges$to))
  expect_true(is.matrix(network$weights))
  expect_identical(dim(network$weights), c(network$n_nodes, network$n_nodes))
})
