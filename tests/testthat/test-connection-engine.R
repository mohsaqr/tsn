connection_dyads <- data.frame(
  from = c("a", "a", "a", "b", "b", "c"),
  to = c("b", "c", "d", "c", "d", "d"),
  distance = c(1, 1, 4, 2, 3, 5),
  stringsAsFactors = FALSE
)

test_that("full connections use decreasing similarity", {
  result <- tsn:::.tsn_connect(connection_dyads, method = "full")

  expect_equal(result$weight, 1 / (1 + connection_dyads$distance))
  expect_true(all(result$connected))
  expect_identical(result$from, connection_dyads$from)
  expect_identical(result$to, connection_dyads$to)
  expect_identical(nrow(result), nrow(connection_dyads))
})

test_that("undirected nearest uses endpoint union and deterministic ties", {
  result <- tsn:::.tsn_connect(
    connection_dyads,
    method = "nearest",
    neighbors = 1,
    directed = FALSE
  )

  expect_identical(result$connected, c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE))
  expect_equal(
    result$weight,
    ifelse(result$connected, 1 / (1 + connection_dyads$distance), 0)
  )
})

test_that("directed nearest respects stored orientation", {
  result <- tsn:::.tsn_connect(
    connection_dyads,
    method = "nearest",
    neighbors = 1,
    directed = TRUE
  )

  expect_identical(result$connected, c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE))
})

test_that("threshold and percentile preserve all dyads", {
  threshold_result <- tsn:::.tsn_connect(
    connection_dyads,
    method = "threshold",
    threshold = 2
  )
  percentile_result <- tsn:::.tsn_connect(
    connection_dyads,
    method = "percentile",
    percentile = 0.5
  )

  expect_identical(threshold_result$connected, connection_dyads$distance <= 2)
  expect_identical(
    percentile_result$connected,
    connection_dyads$distance <= stats::median(connection_dyads$distance)
  )
  expect_identical(nrow(threshold_result), nrow(connection_dyads))
  expect_identical(nrow(percentile_result), nrow(connection_dyads))
})

test_that("gaussian uses supplied and default positive bandwidth", {
  supplied <- tsn:::.tsn_connect(
    connection_dyads,
    method = "gaussian",
    bandwidth = 2
  )
  automatic <- tsn:::.tsn_connect(connection_dyads, method = "gaussian")
  default_bandwidth <- stats::median(connection_dyads$distance)

  expect_equal(
    supplied$weight,
    exp(-(connection_dyads$distance^2) / (2 * 2^2))
  )
  expect_equal(
    automatic$weight,
    exp(-(connection_dyads$distance^2) / (2 * default_bandwidth^2))
  )
  expect_true(all(supplied$connected))
  expect_true(all(automatic$connected))
})

test_that("gaussian assigns unit weight to identical series", {
  result <- tsn(
    list(a = 1:5, b = 1:5, c = 1:5),
    method = "distance", unit = "series", connect = "gaussian"
  )

  expect_true(all(result$table$distance == 0))
  expect_true(all(result$table$weight == 1))
  expect_true(all(result$table$connected))
})

test_that("connection output satisfies schema and numeric invariants", {
  methods <- c("full", "nearest", "threshold", "percentile", "gaussian")
  arguments <- list(
    full = list(),
    nearest = list(neighbors = 2),
    threshold = list(threshold = 3),
    percentile = list(percentile = 0.75),
    gaussian = list(bandwidth = 2)
  )
  results <- lapply(
    methods,
    function(method) {
      do.call(
        tsn:::.tsn_connect,
        c(list(dyads = connection_dyads, method = method), arguments[[method]])
      )
    }
  )

  expect_true(all(vapply(results, is.data.frame, logical(1))))
  expect_true(all(vapply(results, nrow, integer(1)) == nrow(connection_dyads)))
  expect_true(all(vapply(
    results,
    function(result) all(is.finite(result$weight)),
    logical(1)
  )))
  expect_true(all(vapply(
    results,
    function(result) all(result$weight >= 0 & result$weight <= 1),
    logical(1)
  )))
})

test_that("connection validation rejects malformed dyads and arguments", {
  bad_distance <- transform(
    connection_dyads,
    distance = c(NA_real_, 1, 4, 2, 3, 5)
  )
  duplicate <- rbind(
    connection_dyads,
    data.frame(from = "a", to = "b", distance = 1)
  )

  expect_error(tsn:::.tsn_connect(bad_distance, "full"))
  expect_error(tsn:::.tsn_connect(duplicate, "full"))
  expect_error(tsn:::.tsn_connect(connection_dyads, "nearest"))
  expect_error(tsn:::.tsn_connect(connection_dyads, "nearest", neighbors = 0))
  expect_error(tsn:::.tsn_connect(connection_dyads, "nearest", neighbors = 4))
  expect_error(tsn:::.tsn_connect(connection_dyads, "threshold"))
  expect_error(tsn:::.tsn_connect(connection_dyads, "threshold", threshold = -1))
  expect_error(tsn:::.tsn_connect(connection_dyads, "percentile", percentile = 2))
  expect_error(tsn:::.tsn_connect(connection_dyads, "gaussian", bandwidth = 0))
  expect_error(tsn:::.tsn_connect(connection_dyads, "full", threshold = 1))
})

test_that("automatic gaussian bandwidth handles all-zero distances", {
  zero_dyads <- data.frame(
    from = "a",
    to = "b",
    distance = 0,
    stringsAsFactors = FALSE
  )

  expect_equal(
    tsn:::.tsn_connect(zero_dyads, method = "gaussian")$weight,
    1
  )
  expect_equal(
    tsn:::.tsn_connect(
      zero_dyads,
      method = "gaussian",
      bandwidth = 1
    )$weight,
    1
  )
})
