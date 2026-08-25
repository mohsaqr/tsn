test_that("horizontal visibility matches a hand-calculated graph", {
  result <- tsn:::.tsn_visibility(c(3, 1, 2), method = "horizontal")

  expect_equal(result$from, c("1", "1", "2"))
  expect_equal(result$to, c("2", "3", "3"))
  expect_equal(result$distance, c(1L, 2L, 1L))
  expect_equal(result$weight, rep(1, 3))
  expect_true(all(result$connected))
})

test_that("natural and horizontal visibility use their distinct boundaries", {
  values <- c(3, 1.5, 1)
  natural <- tsn:::.tsn_visibility(values, method = "natural")
  horizontal <- tsn:::.tsn_visibility(values, method = "horizontal")

  expect_true(any(natural$from == "1" & natural$to == "3"))
  expect_false(any(horizontal$from == "1" & horizontal$to == "3"))
})

test_that("penetrable visibility permits the requested number of blockers", {
  standard <- tsn:::.tsn_visibility(c(3, 4, 3), method = "horizontal")
  penetrable <- tsn:::.tsn_visibility(
    c(3, 4, 3),
    method = "horizontal",
    penetrable = 1L
  )

  expect_false(any(standard$from == "1" & standard$to == "3"))
  expect_true(any(penetrable$from == "1" & penetrable$to == "3"))
})

test_that("temporal limits and decay are deterministic", {
  result <- tsn:::.tsn_visibility(
    c(3, 1, 2),
    method = "horizontal",
    limit = 2L,
    decay = 0.5
  )

  expect_equal(result$weight, exp(-0.5 * result$distance))
  expect_true(all(result$distance <= 2L))
})

test_that("irregular time controls NVG geometry, distance, limit, and decay", {
  irregular <- data.frame(
    id = "a",
    time = c(0, 1, 10),
    value = c(0, 2, 10)
  )
  natural <- tsn(
    irregular,
    value = "value", id = "id", time = "time",
    method = "nvg"
  )
  expect_false(any(
    natural$table$from == "a:0" & natural$table$to == "a:10"
  ))

  horizontal_data <- within(irregular, value <- c(3, 1, 2))
  horizontal <- tsn(
    horizontal_data,
    value = "value", id = "id", time = "time",
    method = "hvg", decay = 0.1
  )
  long_edge <- horizontal$table$from == "a:0" &
    horizontal$table$to == "a:10"
  expect_equal(horizontal$table$distance[long_edge], 10)
  expect_equal(horizontal$table$weight[long_edge], exp(-1))

  limited <- tsn(
    horizontal_data,
    value = "value", id = "id", time = "time",
    method = "hvg", limit = 3
  )
  expect_false(any(limited$table$to == "a:10"))
})

test_that("visibility node labels cannot collide across id-time pairs", {
  colliding <- data.frame(
    id = c("a", "a", "a:b", "a:b"),
    time = c("b:c", "d", "c", "d"),
    value = 1:4
  )
  result <- tsn(
    colliding,
    value = "value", id = "id", time = "time",
    method = "hvg"
  )

  expect_equal(anyDuplicated(result$nodes$label), 0L)
  expect_equal(result$n_nodes, 4L)
  expect_true(all(c("1:a|3:b:c", "3:a:b|1:c") %in% result$nodes$label))

  timestamps <- as.POSIXlt(
    c("2026-01-01 00:00:00", "2026-01-01 00:01:00"),
    tz = "UTC"
  )
  expect_identical(
    .tsn_node_labels(c("a", "a"), timestamps),
    paste("a", as.character(timestamps), sep = ":")
  )
})

test_that("undirected visibility contains unique dyads without reciprocals", {
  result <- tsn:::.tsn_visibility(
    c(4, 1, 3, 2),
    labels = c("a", "b", "c", "d"),
    method = "horizontal"
  )
  keys <- paste(pmin(result$from, result$to), pmax(result$from, result$to))
  ordered_keys <- paste(result$from, result$to, sep = "\r")
  reciprocal_keys <- paste(result$to, result$from, sep = "\r")

  expect_equal(anyDuplicated(keys), 0L)
  expect_false(any(reciprocal_keys %in% ordered_keys))
})

test_that("directed visibility retains forward temporal orientation", {
  labels <- c("t3", "t1", "t2")
  result <- tsn:::.tsn_visibility(
    c(3, 1, 2),
    labels = labels,
    method = "horizontal",
    directed = TRUE
  )

  expect_true(all(match(result$from, labels) < match(result$to, labels)))
})

test_that("state aggregation does not double count undirected dyads", {
  dyads <- tsn:::.tsn_visibility(
    c(3, 1, 2),
    labels = c("a", "b", "c"),
    method = "horizontal"
  )
  result <- tsn:::.tsn_aggregate_visibility(
    dyads,
    states = c("high", "low", "low"),
    labels = c("a", "b", "c"),
    aggregation = "count"
  )

  high_low <- result$from == "high" & result$to == "low"
  low_low <- result$from == "low" & result$to == "low"
  expect_equal(result$weight[high_low], 2)
  expect_equal(result$weight[low_low], 1)
  expect_equal(nrow(result), 2L)
})

test_that("state aggregation supports sum mean max and directed pairs", {
  dyads <- data.frame(
    from = c("a", "b", "c"),
    to = c("b", "c", "a"),
    distance = c(1, 2, 3),
    weight = c(1, 0.5, 0.25),
    connected = TRUE,
    stringsAsFactors = FALSE
  )
  states <- c("x", "y", "x")
  labels <- c("a", "b", "c")
  summed <- tsn:::.tsn_aggregate_visibility(dyads, states, labels, "sum")
  averaged <- tsn:::.tsn_aggregate_visibility(dyads, states, labels, "mean")
  maximized <- tsn:::.tsn_aggregate_visibility(dyads, states, labels, "max")
  directed <- tsn:::.tsn_aggregate_visibility(
    dyads, states, labels, "count", directed = TRUE
  )

  xy_sum <- summed$weight[summed$from == "x" & summed$to == "y"]
  xy_mean <- averaged$weight[averaged$from == "x" & averaged$to == "y"]
  xy_max <- maximized$weight[maximized$from == "x" & maximized$to == "y"]
  expect_equal(xy_sum, 1.5)
  expect_equal(xy_mean, 0.75)
  expect_equal(xy_max, 1)
  expect_true(any(directed$from == "x" & directed$to == "y"))
  expect_true(any(directed$from == "y" & directed$to == "x"))
})

test_that("visibility validation rejects non-finite values and duplicate labels", {
  expect_error(tsn:::.tsn_visibility(c(1, NA_real_)))
  expect_error(tsn:::.tsn_visibility(c(1, Inf)))
  expect_error(tsn:::.tsn_visibility(c(1, 2), labels = c("a", "a")))
})

test_that("optimized horizontal visibility matches exhaustive evaluation", {
  set.seed(91)
  fixtures <- c(
    lapply(
      seq_len(100L),
      function(index) sample(-3:3, sample(2:40, 1L), replace = TRUE)
    ),
    lapply(
      seq_len(100L),
      function(index) stats::rnorm(sample(2:40, 1L))
    )
  )
  equivalent <- vapply(
    fixtures,
    function(values) {
      optimized <- tsn:::.tsn_visibility(values, method = "horizontal")
      exhaustive <- tsn:::.tsn_visibility(
        values,
        method = "horizontal",
        limit = length(values)
      )
      isTRUE(all.equal(optimized, exhaustive, check.attributes = FALSE))
    },
    logical(1L)
  )

  expect_true(all(equivalent))
})
