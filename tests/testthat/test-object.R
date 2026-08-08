test_that("every method returns the stable tidy schema", {
  distance_result <- tsn(
    list(a = 1:5, b = 2:6, c = 5:1),
    method = "distance",
    unit = "series"
  )
  visibility_result <- tsn(c(3, 1, 4, 2, 5), method = "visibility")
  expected_names <- c(
    "from", "to", "distance", "weight", "connected", "method", "unit",
    "distance_method", "connection_method", "directed", "from_start",
    "from_end", "to_start", "to_end"
  )
  distance_edges <- as.data.frame(distance_result)
  visibility_edges <- as.data.frame(visibility_result)

  expect_identical(names(distance_edges), expected_names)
  expect_identical(names(visibility_edges), expected_names)
  expect_identical(vapply(distance_edges, class, character(1L)),
                   vapply(visibility_edges, class, character(1L)))
})

test_that("summary and matrix coercion are predictable", {
  result <- tsn(
    list(a = 1:4, b = 2:5, c = 4:1),
    method = "distance",
    unit = "series",
    connect = "full"
  )
  overview <- summary(result)
  matrix_result <- as.matrix(result)

  expect_s3_class(overview, "data.frame")
  expect_identical(
    names(overview),
    c("method", "unit", "nodes", "dyads", "edges", "density",
      "minimum_weight", "maximum_weight", "directed")
  )
  expect_equal(dim(matrix_result), c(3, 3))
  expect_true(isSymmetric(matrix_result))
  expect_true(all(diag(matrix_result) == 0))
})

test_that("as.data.frame exposes edges and the source series", {
  network <- tsn(
    data.frame(id = rep("x", 6L), day = 1:6, steps = c(1, 5, 2, 9, 3, 7)),
    value = "steps",
    id = "id",
    time = "day",
    unit = "state",
    discretization = "quantile",
    n_states = 2L
  )
  edges <- as.data.frame(network)
  series <- as.data.frame(network, what = "series")

  expect_identical(edges, as.data.frame(network, what = "edges"))
  expect_false("state" %in% names(edges))
  expect_identical(names(series), c("id", "time", "value", "state"))
  expect_equal(nrow(series), 6L)
  expect_false(anyNA(series$state))
  expect_error(as.data.frame(network, what = "nodes"))
})

test_that("the package exports its construction and analysis verbs", {
  exports <- getNamespaceExports("tsn")
  expect_setequal(exports, c(
    "tsn", "vg", "trend", "discretize",
    "ts_tna", "ts_ftna", "ts_cna", "ts_atna", "series_networks"
  ))
})

test_that("tsn results are cograph-compatible netobjects", {
  network <- tsn(c(3, 1, 4, 2, 5, 3, 6, 2, 7), "hvg")

  expect_s3_class(network, "tsn")
  expect_s3_class(network, "netobject")
  expect_s3_class(network, "cograph_network")
  expect_true(is.list(network))
  expect_identical(names(network$nodes), c("id", "label", "name", "x", "y"))
  expect_identical(names(network$edges), c("from", "to", "weight"))
  expect_true(is.matrix(network$weights))
  expect_true(all(c("source", "layout", "tna") %in% names(network$meta)))
  expect_identical(network$n_nodes, nrow(network$nodes))
})
