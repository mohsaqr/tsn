test_that("scalar distance methods return exact expected values", {
  x <- c(0, 1, 2)
  y <- c(0, 3, 6)

  expect_equal(tsn:::.tsn_distance(x, y, "euclidean"), sqrt(20))
  expect_equal(tsn:::.tsn_distance(x, y, "manhattan"), 6)
  expect_equal(tsn:::.tsn_distance(x, y, "maximum"), 4)
  expect_equal(tsn:::.tsn_distance(x, y, "canberra"), 1)
  expect_equal(tsn:::.tsn_distance(x, y, "minkowski", p = 3), 72^(1 / 3))
  expect_equal(tsn:::.tsn_distance(c(0, 1, 1), c(1, 1, 0), "binary"), 2 / 3)
  expect_equal(tsn:::.tsn_distance(x, 2 * x, "cosine"), 0)
  expect_equal(tsn:::.tsn_distance(x, 2 * x, "correlation"), 0)
  expect_equal(tsn:::.tsn_distance(x, rev(x), "spearman"), 2)
})

test_that("zero-vector cosine and binary distances are defined", {
  zero <- c(0, 0, 0)

  expect_equal(tsn:::.tsn_distance(zero, zero, "cosine"), 0)
  expect_equal(tsn:::.tsn_distance(zero, c(0, 1, 0), "cosine"), 1)
  expect_equal(tsn:::.tsn_distance(zero, zero, "binary"), 0)
})

test_that("DTW supports unequal lengths and is symmetric", {
  x <- c(1, 2, 3)
  y <- c(1, 1, 2, 3)

  expect_equal(tsn:::.tsn_distance(x, y, "dtw"), 0)
  expect_equal(
    tsn:::.tsn_distance(x, y, "dtw"),
    tsn:::.tsn_distance(y, x, "dtw")
  )
  expect_equal(tsn:::.tsn_distance(x, x, "dtw"), 0)
})

test_that("pairwise distances are tidy, unique, named, and ordered", {
  units <- list(alpha = c(0, 0), beta = c(3, 4), gamma = c(0, 4))
  result <- tsn:::.tsn_pairwise_distance(units, method = "euclidean")

  expect_s3_class(result, "data.frame")
  expect_identical(names(result), c("from", "to", "distance"))
  expect_identical(result$from, c("alpha", "alpha", "beta"))
  expect_identical(result$to, c("beta", "gamma", "gamma"))
  expect_equal(result$distance, c(5, 4, 3))
  expect_false(anyDuplicated(paste(result$from, result$to)) > 0L)
})

test_that("pairwise DTW accepts unequal unit lengths", {
  units <- list(first = c(1, 2, 3), second = c(1, 1, 2, 3))
  result <- tsn:::.tsn_pairwise_distance(units, method = "dtw")

  expect_equal(result$distance, 0)
})

test_that("one unit returns an empty typed dyad table", {
  result <- tsn:::.tsn_pairwise_distance(list(only = 1:3), "euclidean")

  expect_identical(dim(result), c(0L, 3L))
  expect_type(result$from, "character")
  expect_type(result$distance, "double")
})

test_that("distance validation rejects invalid inputs", {
  expect_error(tsn:::.tsn_distance(c(1, NA), 1:2, "euclidean"))
  expect_error(tsn:::.tsn_distance(1:2, 1:3, "euclidean"))
  expect_error(tsn:::.tsn_distance(1:2, 1:2, "minkowski", p = 0.5))
  expect_error(tsn:::.tsn_distance(c(1, 1), c(2, 2), "correlation"))
  expect_error(tsn:::.tsn_pairwise_distance(list(1:2, 3:4), "euclidean"))
  expect_error(
    tsn:::.tsn_pairwise_distance(
      list(short = 1:2, long = 1:3),
      "euclidean"
    )
  )
})
