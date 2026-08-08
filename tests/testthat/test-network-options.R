test_that("chain connects only consecutive units", {
  series <- list(a = 1:5, b = 2:6, c = 5:1, d = 3:7)
  full <- tsn(series, method = "distance", unit = "series")
  chain <- tsn(series, method = "distance", unit = "series", chain = TRUE)
  chain_edges <- as.data.frame(chain)

  expect_equal(nrow(as.data.frame(full)), choose(length(series), 2L))
  expect_equal(nrow(chain_edges), length(series) - 1L)
  expect_identical(chain_edges$from, c("a", "b", "c"))
  expect_identical(chain_edges$to, c("b", "c", "d"))
})

test_that("series chains preserve caller order", {
  series <- list(z = 1:5, a = 2:6, m = 3:7)
  chain <- tsn(
    series,
    method = "distance", unit = "series", chain = TRUE
  )
  edges <- as.data.frame(chain)

  expect_identical(edges$from, c("z", "a"))
  expect_identical(edges$to, c("a", "m"))
  expect_identical(unique(chain$source$id), c("z", "a", "m"))
})

test_that("window chains never cross series boundaries", {
  chain <- tsn(
    list(a = 1:6, b = 11:16),
    method = "distance", unit = "window",
    window = 3, step = 2, chain = TRUE
  )
  edges <- as.data.frame(chain)

  expect_equal(nrow(edges), 2L)
  expect_true(all(sub(":W.*$", "", edges$from) == sub(":W.*$", "", edges$to)))
  expect_false(any(edges$from == "a:W2" & edges$to == "b:W1"))
})

test_that("directed distance networks are directed and asymmetric", {
  series <- list(a = 1:5, b = 2:6, c = 5:1, d = 3:7)
  directed <- tsn(series, method = "distance", unit = "series",
                  directed = TRUE, chain = TRUE)

  expect_true(summary(directed)$directed)
  expect_true(all(as.data.frame(directed)$directed))
  expect_false(isSymmetric(as.matrix(directed)))
})

test_that("normalize rescales distances to a unit maximum", {
  series <- list(a = c(0, 0), b = c(1, 1), c = c(6, 6))
  normalized <- tsn(series, method = "distance", unit = "series",
                    normalize = TRUE)
  raw <- tsn(series, method = "distance", unit = "series")

  expect_equal(max(as.data.frame(normalized)$distance), 1)
  expect_gt(max(as.data.frame(raw)$distance), 1)
})

test_that("chain and normalize compose", {
  series <- list(a = 1:6, b = 3:8, c = 6:1)
  result <- tsn(series, method = "distance", unit = "series",
                chain = TRUE, normalize = TRUE)
  edges <- as.data.frame(result)

  expect_equal(nrow(edges), length(series) - 1L)
  expect_lte(max(edges$distance), 1)
})
