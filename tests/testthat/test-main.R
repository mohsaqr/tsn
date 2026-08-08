test_that("default call builds a natural visibility network", {
  result <- tsn(c(3, 1, 4, 2, 5))
  edges <- as.data.frame(result)

  expect_s3_class(result, "tsn")
  expect_s3_class(result, "cograph_network")
  expect_true(all(edges$method == "visibility"))
  expect_true(all(edges$connection_method == "natural"))
  expect_true(all(edges$connected))
})

test_that("state visibility accepts a named state column", {
  data <- data.frame(
    person = "one",
    occasion = 1:6,
    score = c(1, 3, 2, 4, 2, 5),
    phase = rep(c("low", "high"), 3)
  )
  result <- tsn(
    data,
    value = "score",
    id = "person",
    time = "occasion",
    method = "visibility",
    unit = "state",
    state = "phase",
    aggregation = "count"
  )

  expect_setequal(result$nodes$label, c("low", "high"))
  expect_true(all(as.data.frame(result)$unit == "state"))
})

test_that("irrelevant and missing conditional arguments fail early", {
  expect_error(
    tsn(list(a = 1:3, b = 2:4), method = "distance",
        connect = "nearest"),
    "neighbors"
  )
})

test_that("weights always decrease as distance increases", {
  result <- tsn(
    list(a = c(0, 0), b = c(1, 1), c = c(3, 3)),
    method = "distance",
    unit = "series",
    connect = "full"
  )
  edges <- as.data.frame(result)
  ranked <- edges[order(edges$distance), , drop = FALSE]
  expect_true(all(diff(ranked$weight) <= 0))
})
