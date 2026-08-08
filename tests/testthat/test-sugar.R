test_that("method shortcuts equal their verbose equivalents", {
  values <- c(3, 1, 4, 2, 5, 3, 6, 2, 7, 1, 8, 4, 2)
  edges <- function(x) as.data.frame(x)

  expect_identical(
    edges(tsn(values, "nvg")),
    edges(tsn(values, method = "visibility", visibility = "natural"))
  )
  expect_identical(edges(tsn(values, "natural")), edges(tsn(values, "nvg")))
  expect_identical(
    edges(tsn(values, "hvg")),
    edges(tsn(values, method = "visibility", visibility = "horizontal"))
  )
  expect_identical(edges(tsn(values, "horizontal")), edges(tsn(values, "hvg")))
  expect_identical(
    edges(tsn(values, "distance")),
    edges(tsn(values, method = "distance"))
  )
})

test_that("every discretizer name is a direct method on a bare vector", {
  values <- c(3, 1, 4, 2, 5, 3, 6, 2, 7, 1, 8, 4, 2, 6, 3, 9, 5, 2)
  discretizers <- c(
    "width", "quantile", "kde", "kmeans", "gaussian", "hclust", "symbolic",
    "change_points", "entropy", "magnitude", "adaptive_magnitude",
    "percentile_magnitude", "dtw", "ordinal"
  )
  networks <- lapply(discretizers, function(method) tsn(values, method))
  expect_true(all(vapply(networks, inherits, logical(1L), what = "tsn")))
})

test_that("discretizer shortcuts equal the verbose state-network call", {
  values <- c(3, 1, 4, 2, 5, 3, 6, 2, 7, 1, 8, 4, 2)

  expect_identical(
    as.data.frame(tsn(values, "quantile")),
    as.data.frame(tsn(values, method = "visibility", unit = "state",
                      discretization = "quantile"))
  )
  expect_identical(
    as.data.frame(tsn(values, "ordinal")),
    as.data.frame(tsn(values, method = "visibility", unit = "state",
                      discretization = "ordinal"))
  )
})
