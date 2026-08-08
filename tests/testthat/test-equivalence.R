test_that("standard distance engines equal stats dist", {
  x <- c(-2, 0, 1, 4, 7)
  y <- c(-1, 2, 1, 3, 9)
  methods <- c("euclidean", "manhattan", "maximum", "canberra", "binary")

  invisible(lapply(methods, function(method) {
    observed <- tsn:::.tsn_distance(x, y, method = method)
    expected <- as.numeric(stats::dist(rbind(x, y), method = method))
    expect_equal(observed, expected, tolerance = 1e-14)
  }))
  expect_equal(
    tsn:::.tsn_distance(x, y, method = "minkowski", p = 3),
    as.numeric(stats::dist(rbind(x, y), method = "minkowski", p = 3)),
    tolerance = 1e-14
  )
})

test_that("shape distance engines equal direct mathematical definitions", {
  x <- c(-2, 0, 1, 4, 7)
  y <- c(-1, 2, 1, 3, 9)
  cosine <- 1 - sum(x * y) / sqrt(sum(x^2) * sum(y^2))

  expect_equal(tsn:::.tsn_distance(x, y, "cosine"), cosine,
               tolerance = 1e-14)
  expect_equal(tsn:::.tsn_distance(x, y, "correlation"),
               1 - stats::cor(x, y), tolerance = 1e-14)
  expect_equal(tsn:::.tsn_distance(x, y, "spearman"),
               1 - stats::cor(x, y, method = "spearman"), tolerance = 1e-14)
  expect_equal(tsn:::.tsn_distance(1:4, 4:1, "correlation"), 2,
               tolerance = 1e-14)
})

test_that("optimized pairwise calculations equal scalar calculations", {
  set.seed(11)
  units <- stats::setNames(
    lapply(seq_len(6L), function(index) stats::rnorm(25L, mean = index)),
    paste0("series_", seq_len(6L))
  )
  methods <- c(
    "euclidean", "manhattan", "maximum", "canberra", "minkowski",
    "binary", "cosine", "correlation", "spearman"
  )
  invisible(lapply(methods, function(method) {
    observed <- tsn:::.tsn_pairwise_distance(units, method = method, p = 3)
    expected <- vapply(seq_len(nrow(observed)), function(index) {
      tsn:::.tsn_distance(
        units[[observed$from[index]]],
        units[[observed$to[index]]],
        method = method,
        p = 3
      )
    }, numeric(1L))
    expect_equal(observed$distance, expected, tolerance = 1e-12)
  }))
})

test_that("DTW matches hand-calculated reference paths", {
  expect_equal(tsn:::.tsn_distance(c(1, 2, 3), c(1, 2, 3), "dtw"), 0)
  expect_equal(tsn:::.tsn_distance(c(1, 2, 3), c(2, 3), "dtw"), 1)
  expect_equal(tsn:::.tsn_distance(c(1, 1, 2), c(1, 2, 2, 3), "dtw"), 1)
})

test_that("visibility satisfies defining inequalities", {
  values <- c(4, 1, 3, 2, 5, 1)
  labels <- paste0("t", seq_along(values))
  pairs <- utils::combn(seq_along(values), 2L)
  expected_horizontal <- vapply(seq_len(ncol(pairs)), function(index) {
    left <- pairs[1L, index]
    right <- pairs[2L, index]
    between <- if (right - left == 1L) numeric() else
      values[seq.int(left + 1L, right - 1L)]
    !length(between) || all(between < min(values[left], values[right]))
  }, logical(1L))
  observed <- tsn:::.tsn_visibility(values, labels, method = "horizontal")
  expected_keys <- paste(labels[pairs[1L, expected_horizontal]],
                         labels[pairs[2L, expected_horizontal]])

  expect_setequal(paste(observed$from, observed$to), expected_keys)
})

visibility_reference <- function(values, times, method, penetrable = 0L,
                                 limit = NULL, decay = 0) {
  labels <- paste0("t", seq_along(values))
  pairs <- utils::combn(seq_along(values), 2L)
  elapsed <- times[pairs[2L, ]] - times[pairs[1L, ]]
  keep <- if (is.null(limit)) rep.int(TRUE, ncol(pairs)) else elapsed <= limit
  pairs <- pairs[, keep, drop = FALSE]
  elapsed <- elapsed[keep]
  if (ncol(pairs) == 0L) {
    return(data.frame(
      from = character(), to = character(), distance = numeric(),
      weight = numeric(), connected = logical(), stringsAsFactors = FALSE
    ))
  }
  blockers <- vapply(seq_len(ncol(pairs)), function(pair_index) {
    left <- pairs[1L, pair_index]
    right <- pairs[2L, pair_index]
    if (right - left == 1L) {
      return(0L)
    }
    between <- seq.int(left + 1L, right - 1L)
    boundary <- if (method == "horizontal") {
      rep.int(min(values[left], values[right]), length(between))
    } else {
      values[left] + (values[right] - values[left]) *
        (times[between] - times[left]) / (times[right] - times[left])
    }
    sum(values[between] >= boundary)
  }, integer(1L))
  visible <- blockers <= penetrable
  data.frame(
    from = labels[pairs[1L, visible]],
    to = labels[pairs[2L, visible]],
    distance = as.numeric(elapsed[visible]),
    weight = exp(-decay * elapsed[visible]),
    connected = rep.int(TRUE, sum(visible)),
    stringsAsFactors = FALSE
  )
}

test_that("visibility matches an independent irregular-time reference", {
  set.seed(912)
  fixtures <- lapply(seq_len(40L), function(index) {
    n <- sample(3:15, 1L)
    list(
      values = sample(-3:3, n, replace = TRUE),
      times = cumsum(stats::runif(n, min = 0.2, max = 2))
    )
  })
  settings <- expand.grid(
    method = c("natural", "horizontal"),
    penetrable = 0:2,
    limited = c(FALSE, TRUE),
    decay = c(0, 0.2),
    stringsAsFactors = FALSE
  )
  equivalent <- vapply(fixtures, function(fixture) {
    all(vapply(seq_len(nrow(settings)), function(setting_index) {
      setting <- settings[setting_index, , drop = FALSE]
      limit <- if (setting$limited) 4 else NULL
      observed <- .tsn_visibility(
        fixture$values,
        labels = paste0("t", seq_along(fixture$values)),
        times = fixture$times,
        method = setting$method,
        penetrable = setting$penetrable,
        limit = limit,
        decay = setting$decay
      )
      expected <- visibility_reference(
        fixture$values,
        times = fixture$times,
        method = setting$method,
        penetrable = setting$penetrable,
        limit = limit,
        decay = setting$decay
      )
      isTRUE(all.equal(observed, expected, tolerance = 1e-14))
    }, logical(1L)))
  }, logical(1L))

  expect_true(all(equivalent))
})

test_that("connection transformations equal their definitions", {
  dyads <- data.frame(
    from = c("a", "a", "b"),
    to = c("b", "c", "c"),
    distance = c(1, 2, 4),
    stringsAsFactors = FALSE
  )
  full <- tsn:::.tsn_connect(dyads, method = "full")
  gaussian <- tsn:::.tsn_connect(dyads, method = "gaussian", bandwidth = 2)
  percentile <- tsn:::.tsn_connect(dyads, method = "percentile", percentile = 0.5)

  expect_equal(full$weight, 1 / (1 + dyads$distance))
  expect_equal(gaussian$weight, exp(-(dyads$distance^2) / 8))
  expect_identical(percentile$connected, c(TRUE, TRUE, FALSE))
})

test_that("all public input forms are numerically equivalent", {
  wide <- data.frame(first = 1:8, second = 2:9, third = 8:1)
  series <- unname(as.list(wide))
  names(series) <- names(wide)
  long <- data.frame(
    id = rep(names(wide), each = nrow(wide)),
    time = rep(seq_len(nrow(wide)), times = ncol(wide)),
    value = unlist(wide, use.names = FALSE)
  )
  from_wide <- tsn(wide, method = "distance", unit = "series")
  from_list <- tsn(series, method = "distance", unit = "series")
  from_long <- tsn(
    long,
    value = "value",
    id = "id",
    time = "time",
    method = "distance",
    unit = "series"
  )

  expect_equal(as.data.frame(from_wide), as.data.frame(from_list))
  expect_equal(as.data.frame(from_wide), as.data.frame(from_long))
})
