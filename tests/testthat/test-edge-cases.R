test_that("normalization rejects every invalid input boundary", {
  expect_error(tsn(NULL), "must not be NULL")
  expect_error(tsn("invalid"), "must be numeric")
  expect_error(tsn(1), "at least two")
  expect_error(tsn(list(a = numeric())), "non-empty")
  expect_s3_class(tsn(list(1:3, 2:4), method = "distance"), "tsn")
  expect_error(tsn(data.frame(label = letters[1:3])), "only numeric")

  bad_id <- data.frame(id = c("", "a"), time = 1:2, value = 1:2)
  expect_error(tsn(bad_id, value = "value", id = "id", time = "time"),
               "non-missing")
  bad_time <- data.frame(id = "a", time = c(1, NA), value = 1:2)
  expect_error(tsn(bad_time, value = "value", id = "id", time = "time"),
               "must not be missing")
  expect_error(tsn(data.frame(value = 1:3), value = NA_character_),
               "Column arguments")
  expect_error(tsn(data.frame(value = 1:3), value = "unknown"),
               "Unknown column")
})

test_that("state normalization validates names lengths and placement", {
  data <- data.frame(id = rep(c("a", "b"), each = 3), time = 1:6,
                     value = 1:6, state = letters[1:6])
  expect_error(
    tsn(data, value = "value", id = "id", time = "time",
        method = "visibility", unit = "state", state = "unknown"),
    "Unknown state"
  )
  expect_error(
    tsn(data, value = "value", id = "id", time = "time",
        method = "visibility", unit = "state", state = letters[1:2]),
    "one value"
  )
  expect_error(tsn(1:5, state = letters[1:5]), "only valid")
  expect_error(
    tsn(1:5, method = "visibility", unit = "state", state = letters[1:2]),
    "one non-missing"
  )
})

test_that("conditional public arguments are strict", {
  series <- list(a = 1:5, b = 2:6)
  expect_error(tsn(series, method = "distance", unit = "series", window = 3),
               "only valid")
  expect_error(tsn(1:5, method = "visibility", neighbors = 2),
               "cannot be used")
  expect_error(tsn(1:5, method = "visibility", limit = 0), "limit")
  expect_error(tsn(1:5, method = "visibility", penetrable = -1), "penetrable")
  expect_error(tsn(1:5, method = "visibility", decay = -1), "decay")
  expect_error(tsn(series, method = "distance", threshold = "bad"),
               "Invalid numeric")
  expect_error(tsn(series, method = "distance", connect = "threshold"),
               "threshold")
  expect_error(tsn(series, method = "distance", connect = "percentile",
                   percentile = 2), "percentile")
})

test_that("window boundaries reject invalid and short configurations", {
  expect_error(tsn(1:5, method = "distance", unit = "window",
                   window = 1), "at least 2")
  expect_error(tsn(1:5, method = "distance", unit = "window",
                   window = 2, step = 0), "positive")
  expect_error(tsn(1:5, method = "distance", unit = "window",
                   window = 6), "exceeds")
  expect_error(tsn(1:5, method = "distance", unit = "series"),
               "at least two")
})

test_that("objects cover print directed summary and empty connections", {
  result <- tsn(list(a = 1:3, b = 4:6), method = "distance",
                connect = "threshold", threshold = 0)
  expect_output(print(result), "<tsn>")
  expect_true(all(as.matrix(result) == 0))
  overview <- summary(result)
  expect_true(is.na(overview$minimum_weight))

  directed <- tsn(c(3, 1, 4, 2), method = "visibility", directed = TRUE)
  expect_true(summary(directed)$directed)
  expect_false(isSymmetric(as.matrix(directed)))

  directed_chain <- tsn(list(a = 1:5, b = 2:6, c = 5:1),
                        method = "distance", directed = TRUE, chain = TRUE)
  expect_true(summary(directed_chain)$directed)
  expect_true(all(as.data.frame(directed_chain)$directed))
})

test_that("plot validation palettes and single-point panels are covered", {
  data <- data.frame(
    id = c("a", "b"),
    time = c(1, 1),
    value = c(1, 2),
    state = c("low", "high")
  )
  result <- tsn(data, value = "value", id = "id", time = "time",
                method = "visibility", unit = "state", state = "state")
  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output)
  on.exit({
    grDevices::dev.off()
    unlink(output)
  }, add = TRUE)

  expect_invisible(plot(result, "series", overlay = "vertical",
                        palette = c(low = "blue", high = "red")))
  expect_invisible(plot(result, "series", overlay = "horizontal",
                        palette = c("blue", "red"), points = FALSE))
  expect_error(plot(result, "series", series = "unknown"), "Unknown series")
  expect_error(plot(result, "series", palette = "blue"))
})

test_that("rare numerical branches remain deterministic", {
  zero_units <- list(zero = c(0, 0, 0), signal = c(1, 2, 3),
                     other = c(3, 2, 1))
  cosine <- tsn:::.tsn_pairwise_distance(zero_units, method = "cosine")
  expect_true(all(is.finite(cosine$distance)))
  expect_error(
    tsn:::.tsn_pairwise_distance(
      list(constant = rep(1, 4), varying = 1:4),
      method = "correlation"
    ),
    "non-constant"
  )
  expect_error(tsn:::.tsn_order_states(1:3, c(1L, NA, 2L)), "invalid")
  expect_s3_class(
    tsn:::.tsn_visibility(1, method = "natural"),
    "data.frame"
  )
  expect_s3_class(
    tsn:::.tsn_visibility(1, method = "horizontal"),
    "data.frame"
  )
  empty <- data.frame(from = character(), to = character(),
                      distance = numeric(), weight = numeric(),
                      connected = logical())
  expect_equal(
    tsn:::.tsn_aggregate_visibility(empty, "state", "node"),
    empty
  )
})
