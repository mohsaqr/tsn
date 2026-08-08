test_that("all discretizers use packaged steps through direct tsn calls", {
  data("steps", package = "tsn")
  methods <- c(
    "threshold", "width", "quantile", "kde", "kmeans", "gaussian",
    "hclust"
  )
  networks <- setNames(lapply(methods, function(discretization_method) {
    tsn(
      steps,
      value = "steps",
      id = "id",
      time = "day",
      series = 536,
      unit = "state",
      visibility = "horizontal",
      discretization = discretization_method,
      breaks = if (discretization_method == "threshold") c(10000, 16500) else NULL,
      seed = 1707L
    )
  }), methods)

  expect_true(all(vapply(networks, inherits, logical(1L), what = "tsn")))
  expect_true(all(vapply(networks, function(network) {
    source <- as.data.frame(network, what = "series")
    nrow(source) == 265L &&
      identical(unique(source[["id"]]), "536") &&
      inherits(source[["time"]], "Date") &&
      identical(sort(unique(source[["state"]])), as.character(1:3)) &&
      !anyNA(source[["state"]])
  }, logical(1L))))
  expect_true(all(vapply(networks, function(network) {
    all(is.finite(as.data.frame(network)$distance)) && all(is.finite(as.data.frame(network)$weight))
  }, logical(1L))))
})

test_that("series selects long IDs and wide columns inside tsn", {
  long <- data.frame(
    id = rep(c("a", "b", "c"), each = 4L),
    time = rep(seq_len(4L), times = 3L),
    value = seq_len(12L)
  )
  selected_long <- tsn(
    long,
    value = "value",
    id = "id",
    time = "time",
    series = c("a", "c"),
    method = "distance"
  )
  selected_wide <- tsn(
    data.frame(a = 1:4, b = 2:5, label = letters[1:4]),
    series = c("a", "b"),
    method = "distance"
  )
  selected_list <- tsn(
    list(a = 1:4, b = 2:5, c = 3:6),
    series = c("b", "c"),
    method = "distance"
  )

  expect_identical(selected_long$nodes$label, c("a", "c"))
  expect_identical(selected_wide$nodes$label, c("a", "b"))
  expect_identical(selected_list$nodes$label, c("b", "c"))
  expect_error(tsn(long, value = "value", series = "a"), "id.*required")
  expect_error(tsn(long, value = "value", id = "id", series = "z"),
               "Unknown series")
  expect_error(tsn(1:5, series = "a"), "selection requires")
})

test_that("motivation supports direct one-line wide selection", {
  data("motivation", package = "tsn")
  mood <- tsn(
    motivation,
    series = "mood",
    unit = "state",
    visibility = "horizontal",
    discretization = "gaussian",
    seed = 1707L
  )
  variables <- tsn(
    motivation,
    series = c("autonomy", "competence", "relatedness", "mood"),
    method = "distance",
    distance = "correlation"
  )

  expect_s3_class(mood, "tsn")
  expect_equal(nrow(as.data.frame(mood, what = "series")), nrow(motivation))
  expect_false(anyNA(as.data.frame(mood, what = "series")[["state"]]))
  expect_identical(variables$nodes$label,
                   c("autonomy", "competence", "relatedness", "mood"))
})
