test_that("numeric, ts, matrix, list, and long inputs normalize uniformly", {
  numeric_data <- tsn(c(3, 1, 4, 2), method = "visibility")
  ts_data <- tsn(stats::ts(c(3, 1, 4, 2), start = 2000), method = "visibility")
  matrix_data <- tsn(
    cbind(first = 1:4, second = 2:5),
    method = "distance",
    unit = "series"
  )
  list_data <- tsn(
    list(first = 1:4, second = 2:5),
    method = "distance",
    unit = "series"
  )
  long <- data.frame(
    person = rep(c("second", "first"), each = 4),
    occasion = rep(1:4, 2),
    score = c(2:5, 1:4)
  )
  long_data <- tsn(
    long,
    value = "score",
    id = "person",
    time = "occasion",
    method = "distance",
    unit = "series"
  )

  expect_s3_class(numeric_data, "tsn")
  expect_s3_class(ts_data, "tsn")
  expect_equal(as.data.frame(matrix_data), as.data.frame(list_data))
  node_order <- sort(matrix_data$nodes$label)
  expect_equal(
    as.matrix(matrix_data)[node_order, node_order],
    as.matrix(long_data)[node_order, node_order]
  )
  expect_identical(unique(long_data$source$id), c("second", "first"))
})

test_that("interleaved IDs are split by identity rather than row ranges", {
  long <- data.frame(
    person = rep(c("first", "second"), 4),
    occasion = rep(1:4, each = 2),
    score = c(rbind(1:4, 2:5))
  )
  result <- tsn(
    long,
    value = "score",
    id = "person",
    time = "occasion",
    method = "distance",
    unit = "series"
  )

  edges <- as.data.frame(result)
  expect_equal(edges$distance, 2)
  expect_identical(edges$from_start, 1L)
  expect_identical(edges$from_end, 4L)
})

test_that("nonnumeric time labels preserve caller order", {
  long <- data.frame(
    id = "a",
    occasion = c("T1", "T2", "T10", "T3"),
    value = c(1, 2, 10, 3)
  )
  normalized <- tsn:::.tsn_normalize_input(
    long,
    value = "value", id = "id", time = "occasion"
  )

  expect_identical(normalized$time, long$occasion)
  expect_identical(normalized$value, long$value)
})

test_that("input validation rejects ambiguous and invalid data", {
  expect_error(tsn(data.frame(group = letters[1:3])), "numeric")
  expect_error(tsn(c(1, NA, 2)), "finite")
  expect_error(tsn(list(a = 1:3, a = 2:4), method = "distance"), "unique")
  duplicate_time <- data.frame(id = c("a", "a"), time = c(1, 1), value = 1:2)
  expect_error(
    tsn(duplicate_time, value = "value", id = "id", time = "time"),
    "unique"
  )
})

test_that("window construction retains every input series", {
  result <- tsn(
    list(first = 1:6, second = 2:7),
    method = "distance",
    unit = "window",
    window = 3,
    step = 3
  )
  info <- summary(result)
  edges <- as.data.frame(result)

  expect_equal(info$nodes, 4)
  expect_true(all(grepl(":W", unique(c(edges$from, edges$to)), fixed = TRUE)))
})
