test_that("vg is identical to the equivalent tsn visibility call", {
  values <- c(3, 1, 4, 2, 5, 3, 6, 2, 7, 1, 8)

  expect_identical(
    as.data.frame(vg(values)),
    as.data.frame(tsn(values, "nvg"))
  )
  expect_identical(
    as.data.frame(vg(values, "horizontal")),
    as.data.frame(tsn(values, "hvg"))
  )
  expect_identical(
    as.data.frame(vg(values, "natural")),
    as.data.frame(tsn(values, method = "visibility", visibility = "natural"))
  )
})

test_that("vg forwards every visibility argument to tsn", {
  values <- c(3, 1, 4, 2, 5, 3, 6, 2, 7, 1, 8)

  expect_identical(
    as.matrix(vg(values, "horizontal", penetrable = 1L)),
    as.matrix(tsn(values, method = "visibility", visibility = "horizontal",
                  penetrable = 1L))
  )
  expect_identical(
    as.data.frame(vg(values, "natural", directed = TRUE, limit = 3L)),
    as.data.frame(tsn(values, method = "visibility", visibility = "natural",
                      directed = TRUE, limit = 3L))
  )
  expect_identical(
    as.data.frame(vg(values, "horizontal", unit = "state",
                     discretization = "quantile", n_states = 3L)),
    as.data.frame(tsn(values, method = "visibility", visibility = "horizontal",
                      unit = "state", discretization = "quantile",
                      n_states = 3L))
  )
})

test_that("vg returns the dual-class netobject", {
  network <- vg(c(3, 1, 4, 2, 5, 3, 6))
  expect_s3_class(network, "tsn")
  expect_s3_class(network, "netobject")
  expect_s3_class(network, "cograph_network")
})
