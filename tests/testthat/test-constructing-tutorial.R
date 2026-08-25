# Pins the central claims of vignette("constructing-time-series-networks").
# If a data or code change moves these numbers, the vignette prose must be
# recomputed and rewritten, not just re-rendered.

data("esm_srl", package = "tsn", envir = environment())

ct_segment <- utils::head(subset(esm_srl, name == "Jamal"), 60L)
ct_states <- function() {
  discretize(ct_segment, series = "anxiety", labels = c("Low", "Middle", "High"))
}

test_that("the data section describes the segment correctly", {
  expect_identical(nrow(ct_segment), 60L)
  expect_false(anyNA(ct_segment$anxiety))
  expect_equal(mean(ct_segment$anxiety), 47.0, tolerance = 1e-2)
  expect_equal(sd(ct_segment$anxiety), 27.1, tolerance = 1e-1)
  # No drift: the first and last twenty reports average nearly the same.
  expect_lt(
    abs(mean(utils::head(ct_segment$anxiety, 20)) -
          mean(utils::tail(ct_segment$anxiety, 20))),
    2
  )
})

test_that("the quantile discretization splits into exactly equal states", {
  states <- ct_states()
  composition <- summary(states)
  expect_identical(composition$count, c(20L, 20L, 20L))
  expect_equal(composition$mean_value, c(17.04301, 45.69892, 78.38710),
               tolerance = 1e-5)
})

test_that("the four transition constructors match the reported numbers", {
  skip_if_not_installed("Nestimate")
  states <- ct_states()

  probabilities <- as.matrix(ts_tna(states))
  expect_equal(unname(diag(probabilities)), c(0.316, 0.200, 0.400),
               tolerance = 1e-3)
  expect_equal(unname(probabilities["Low", "Middle"]), 0.526,
               tolerance = 1e-3)

  counts <- as.matrix(ts_ftna(states))
  expect_identical(sum(counts), 59L)
  expect_identical(unname(counts["Low", "Middle"]), 10L)
  expect_identical(unname(counts["Middle", "Middle"]), 4L)

  cooccurrence <- as.matrix(ts_cna(states))
  expect_true(all(cooccurrence[upper.tri(cooccurrence)] == 400))
  expect_true(all(diag(cooccurrence) == 190))

  attention <- as.matrix(ts_atna(states))
  expect_equal(range(attention), c(2.462, 5.000), tolerance = 1e-3)
  expect_identical(
    which(attention == max(attention), arr.ind = TRUE)[1, ],
    c(row = 2L, col = 3L)
  )
})

test_that("the trend labels match the reported counts, with no Flat label", {
  directions <- trend(ct_segment, series = "anxiety")
  composition <- summary(directions)
  expect_identical(
    subset(composition, state == "Ascending")$count, 24L
  )
  expect_identical(
    subset(composition, state == "Descending")$count, 21L
  )
  expect_identical(
    subset(composition, state == "Turbulent")$count, 10L
  )
  expect_false("Flat" %in% composition$state)
})

test_that("the visibility and window networks match the reported counts", {
  natural <- vg(ct_segment, series = "anxiety")
  horizontal <- vg(ct_segment, type = "horizontal", series = "anxiety")
  expect_identical(natural$n_edges, 144L)
  expect_identical(horizontal$n_edges, 106L)

  sparse <- tsn(ct_segment, method = "distance", series = "anxiety",
                step = 2, connect = "nearest", neighbors = 2)
  expect_identical(sparse$n_nodes, 28L)
  expect_identical(sparse$n_edges, 41L)
  edges <- subset(as.data.frame(sparse), connected)
  gap <- abs(as.integer(sub(".*:W", "", edges$from)) -
               as.integer(sub(".*:W", "", edges$to)))
  # Only two retained edges join adjacent windows; the rest span 2-19.
  expect_identical(sum(gap == 1L), 2L)
  expect_identical(max(gap), 19L)

  warped <- tsn(ct_segment, method = "distance", series = "anxiety",
                step = 2, distance = "dtw", connect = "nearest",
                neighbors = 2)
  expect_identical(warped$n_edges, 37L)
  shared <- length(intersect(
    paste(edges$from, edges$to),
    with(subset(as.data.frame(warped), connected), paste(from, to))
  ))
  expect_identical(shared, 12L)
})
