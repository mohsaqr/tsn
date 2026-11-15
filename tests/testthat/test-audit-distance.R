# Audit of R/distance.R: every metric is checked against an independent
# base-R or textbook implementation, plus the edge cases that the engine
# must either define or reject with an informative error.

audit_entropy <- function(probabilities) {
  positive <- probabilities[probabilities > 0]
  -sum(positive * log(positive))
}

audit_dtw <- function(x, y) {
  # Textbook O(nm) recursion, written independently of the engine's
  # anti-diagonal sweep.
  costs <- matrix(Inf, length(x) + 1L, length(y) + 1L)
  costs[1L, 1L] <- 0
  cells <- expand.grid(i = seq_along(x), j = seq_along(y))
  cells <- cells[order(cells$i, cells$j), ]
  costs <- Reduce(function(matrix_so_far, row) {
    i <- cells$i[row]
    j <- cells$j[row]
    matrix_so_far[i + 1L, j + 1L] <- abs(x[i] - y[j]) + min(
      matrix_so_far[i, j + 1L], matrix_so_far[i + 1L, j],
      matrix_so_far[i, j]
    )
    matrix_so_far
  }, seq_len(nrow(cells)), costs)
  costs[length(x) + 1L, length(y) + 1L]
}

audit_event_sync <- function(x, y, cap = Inf) {
  # Hand count of Quian Quiroga et al. (2002), event pair by event pair.
  gaps <- function(events, index) {
    c(
      if (index > 1L) events[index] - events[index - 1L] else Inf,
      if (index < length(events)) events[index + 1L] - events[index] else Inf
    )
  }
  grid <- expand.grid(i = seq_along(x), j = seq_along(y))
  contributions <- vapply(seq_len(nrow(grid)), function(row) {
    i <- grid$i[row]
    j <- grid$j[row]
    window <- min(min(c(gaps(x, i), gaps(y, j))) / 2, cap)
    if (is.infinite(window)) window <- 0
    gap <- x[i] - y[j]
    if (gap == 0) return(1)
    if (abs(gap) <= window) 1 else 0
  }, numeric(1L))
  1 - min(1, sum(contributions) / sqrt(length(x) * length(y)))
}

test_that("audit: lp and binary engines equal stats::dist on random data", {
  set.seed(101)
  x <- stats::rnorm(12L)
  y <- stats::rnorm(12L)
  methods <- c("euclidean", "manhattan", "maximum", "canberra", "binary")
  observed <- vapply(methods, function(method) {
    tsn:::.tsn_distance(x, y, method = method)
  }, numeric(1L))
  expected <- vapply(methods, function(method) {
    as.numeric(stats::dist(rbind(x, y), method = method))
  }, numeric(1L))
  expect_equal(observed, expected, tolerance = 1e-14)
  invisible(lapply(c(1, 1.5, 3, 7), function(power) {
    expect_equal(
      tsn:::.tsn_distance(x, y, "minkowski", p = power),
      as.numeric(stats::dist(rbind(x, y), "minkowski", p = power)),
      tolerance = 1e-13
    )
  }))
  expect_equal(tsn:::.tsn_distance(x, y, "minkowski", p = 1),
               tsn:::.tsn_distance(x, y, "manhattan"), tolerance = 1e-14)
  expect_equal(tsn:::.tsn_distance(x, y, "minkowski", p = 2),
               tsn:::.tsn_distance(x, y, "euclidean"), tolerance = 1e-14)
})

test_that("audit: canberra with joint zeros agrees between full and chain paths", {
  a <- c(0, 1, 2, 0)
  b <- c(0, 3, 0, 1)
  # Lance-Williams hand value: 0 + 2/4 + 2/2 + 1/1 (a 0/0 term counts zero).
  expected <- 0.5 + 1 + 1
  expect_equal(tsn:::.tsn_distance(a, b, "canberra"), expected)
  # stats::dist omits the 0/0 term and rescales by 4/3; the engine does not.
  expect_equal(as.numeric(stats::dist(rbind(a, b), "canberra")),
               expected * 4 / 3)
  units <- list(a = a, b = b, c = c(1, 1, 0, 0))
  full <- tsn:::.tsn_pairwise_distance(units, "canberra")
  chain <- tsn:::.tsn_chain_distance(units, "canberra")
  # Previously the full network gave 3.333 for a-b and 3.333 for b-c while
  # the chain gave 2.5 for both.
  expect_equal(full$distance, c(2.5, 2, 2.5), tolerance = 1e-14)
  expect_equal(chain$distance, full$distance[c(1L, 3L)], tolerance = 1e-14)
  set.seed(109)
  sparse <- stats::setNames(lapply(seq_len(6L), function(i) {
    stats::rpois(12L, 0.7) * sample(c(-1, 1), 12L, replace = TRUE)
  }), letters[seq_len(6L)])
  fast <- tsn:::.tsn_pairwise_distance(sparse, "canberra")
  scalar <- vapply(seq_len(nrow(fast)), function(index) {
    tsn:::.tsn_distance(sparse[[fast$from[index]]], sparse[[fast$to[index]]],
                        "canberra")
  }, numeric(1L))
  expect_equal(fast$distance, scalar, tolerance = 1e-13)
})

test_that("audit: two all-zero canberra vectors have distance zero, not NA", {
  zero <- c(0, 0, 0)
  expect_identical(tsn:::.tsn_distance(zero, zero, "canberra"), 0)
  pairwise <- tsn:::.tsn_pairwise_distance(
    list(a = zero, b = zero, c = c(1, 0, 0)), "canberra"
  )
  expect_true(all(is.finite(pairwise$distance)))
  expect_equal(pairwise$distance, c(0, 1, 1))
})

test_that("audit: minkowski with a large power stays finite in both paths", {
  x <- c(0, 0)
  y <- c(3, 4)
  # Limit p -> Inf is the maximum norm; 4 * (1 + 0.75^1000)^(1/1000) == 4.
  expect_equal(tsn:::.tsn_distance(x, y, "minkowski", p = 1000), 4)
  pairwise <- tsn:::.tsn_pairwise_distance(list(a = x, b = y), "minkowski",
                                           p = 1000)
  expect_equal(pairwise$distance, 4)
  expect_identical(tsn:::.tsn_distance(x, x, "minkowski", p = 1000), 0)
})

test_that("audit: cosine and correlation equal their definitions", {
  set.seed(102)
  x <- stats::rnorm(10L)
  y <- stats::rnorm(10L)
  expect_equal(tsn:::.tsn_distance(x, y, "cosine"),
               1 - sum(x * y) / sqrt(sum(x^2) * sum(y^2)), tolerance = 1e-14)
  expect_equal(tsn:::.tsn_distance(x, y, "correlation"),
               1 - stats::cor(x, y), tolerance = 1e-14)
  ties <- c(1, 2, 2, 3, 5, 5, 5, 8, 9, 9)
  expect_equal(tsn:::.tsn_distance(ties, y, "spearman"),
               1 - stats::cor(rank(ties), rank(y)), tolerance = 1e-14)
  # Zero vectors: defined, and the pairwise fast path agrees with the scalar.
  zero <- rep(0, 5L)
  ramp <- c(1, 2, 3, 4, 5)
  cosine <- tsn:::.tsn_pairwise_distance(
    list(z = zero, r = ramp, d = 2 * ramp), "cosine"
  )
  expect_equal(cosine$distance, c(1, 1, 0), tolerance = 1e-14)
})

test_that("audit: constant or single-observation series are rejected informatively", {
  y <- c(2, 5, 1, 4)
  expect_error(tsn:::.tsn_distance(rep(1, 4), y, "correlation"),
               "non-constant")
  expect_error(tsn:::.tsn_pairwise_distance(list(a = rep(1, 4), b = y),
                                            "spearman"),
               "non-constant")
  expect_error(tsn:::.tsn_distance(rep(1, 4), y, "ccf"), "non-constant")
  # Length-one units (unit = "time") used to fail with "missing value where
  # TRUE/FALSE needed" in the pairwise path.
  expect_error(tsn:::.tsn_pairwise_distance(list(a = 1, b = 2), "correlation"),
               "non-constant")
  expect_error(tsn:::.tsn_distance(1, 2, "correlation"), "two observations")
  expect_error(tsn:::.tsn_distance(1, 2, "ccf"), "two observations")
})

test_that("audit: ccf distance equals one minus max absolute stats::ccf", {
  set.seed(103)
  x <- stats::rnorm(20L)
  y <- c(0, x[-20L]) + stats::rnorm(20L, sd = 0.1)
  default_lags <- stats::ccf(x, y, plot = FALSE)$acf
  expect_equal(tsn:::.tsn_distance(x, y, "ccf"),
               1 - max(abs(default_lags)), tolerance = 1e-14)
  two_lags <- stats::ccf(x, y, lag.max = 2, plot = FALSE)$acf
  expect_equal(tsn:::.tsn_distance(x, y, "ccf", lag = 2),
               1 - max(abs(two_lags)), tolerance = 1e-14)
  expect_equal(tsn:::.tsn_distance(x, y, "ccf"),
               tsn:::.tsn_distance(y, x, "ccf"), tolerance = 1e-14)
})

test_that("audit: nmi and voi equal hand-computed entropies of the binning", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8)
  y <- c(2, 1, 4, 3, 6, 5, 8, 7)
  bins_x <- tsn:::.tsn_marginal_quantile_bins(x, 4L)
  bins_y <- tsn:::.tsn_marginal_quantile_bins(y, 4L)
  expect_identical(bins_x, c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L))
  joint <- as.vector(table(bins_x, bins_y)) / 8
  entropy_x <- audit_entropy(as.vector(table(bins_x)) / 8)
  entropy_y <- audit_entropy(as.vector(table(bins_y)) / 8)
  mutual <- entropy_x + entropy_y - audit_entropy(joint)
  expect_equal(tsn:::.tsn_distance(x, y, "nmi", bins = 4L),
               1 - mutual / max(entropy_x, entropy_y), tolerance = 1e-14)
  expect_equal(tsn:::.tsn_distance(x, y, "voi", bins = 4L),
               entropy_x + entropy_y - 2 * mutual, tolerance = 1e-14)
  # Invariance to monotone transforms of either series.
  expect_equal(tsn:::.tsn_distance(x, y, "nmi", bins = 3L),
               tsn:::.tsn_distance(exp(x), y^3, "nmi", bins = 3L))
  expect_identical(tsn:::.tsn_distance(x, x, "voi"), 0)
})

test_that("audit: quantile bins handle ties and few unique values", {
  expect_identical(tsn:::.tsn_marginal_quantile_bins(rep(3, 5), 4L),
                   rep(1L, 5L))
  few <- c(0, 0, 0, 5, 5, 5, 5, 9, 9, 9)
  ten <- tsn:::.tsn_marginal_quantile_bins(few, 10L)
  expect_identical(ten, c(1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L))
  # Equal values always share a bin, and bins never exceed unique values.
  set.seed(104)
  invisible(lapply(seq_len(30L), function(index) {
    values <- sample(c(1, 2, 3, 3.5, 10), sample(3:20, 1L), replace = TRUE)
    bins <- sample(2:6, 1L)
    labels <- tsn:::.tsn_marginal_quantile_bins(values, bins)
    expect_true(all(tapply(labels, values, function(l) length(unique(l))) == 1L))
    expect_lte(length(unique(labels)), min(bins, length(unique(values))))
    # Labels are monotone in the values.
    expect_false(is.unsorted(labels[order(values)]))
  }))
  # Constant series carry no information.
  expect_equal(tsn:::.tsn_distance(rep(1, 6), c(1, 4, 2, 6, 3, 5), "nmi"), 1,
               tolerance = 1e-14)
})

test_that("audit: information distances reject single-observation units", {
  # Previously returned 0 for every pair, silently producing a complete
  # graph of zero distances for unit = "time".
  expect_error(tsn:::.tsn_distance(1, 2, "nmi"), "two observations")
  expect_error(tsn:::.tsn_distance(1, 2, "voi"), "two observations")
})

test_that("audit: event synchronization equals a hand count", {
  x <- c(1, 4, 7, 10)
  y <- c(1.2, 3.9, 8, 10)
  expect_equal(tsn:::.tsn_distance(x, y, "event_sync"), audit_event_sync(x, y))
  expect_equal(tsn:::.tsn_distance(x, y, "event_sync", tolerance = 0.15),
               audit_event_sync(x, y, cap = 0.15))
  expect_equal(tsn:::.tsn_distance(x, y, "event_sync", tolerance = 0.15), 0.5)
  set.seed(105)
  invisible(lapply(seq_len(20L), function(index) {
    a <- sort(unique(round(stats::runif(sample(1:7, 1L), 0, 20), 1)))
    b <- sort(unique(round(stats::runif(sample(1:7, 1L), 0, 20), 1)))
    expect_equal(tsn:::.tsn_distance(a, b, "event_sync"),
                 audit_event_sync(a, b))
    expect_equal(tsn:::.tsn_distance(a, b, "event_sync"),
                 tsn:::.tsn_distance(b, a, "event_sync"))
  }))
  expect_identical(tsn:::.tsn_distance(x, x, "event_sync"), 0)
})

test_that("audit: two single-event trains are not synchronous at any distance", {
  # Previously the infinite adaptive window made 5 and 1e6 coincide.
  expect_identical(tsn:::.tsn_distance(5, 1e6, "event_sync"), 1)
  expect_identical(tsn:::.tsn_distance(5, 6, "event_sync"), 1)
  expect_identical(tsn:::.tsn_distance(5, 5, "event_sync"), 0)
  expect_identical(tsn:::.tsn_distance(5, 6, "event_sync", tolerance = 2), 0)
  expect_identical(tsn:::.tsn_distance(5, 6, "event_sync", tolerance = 0.5), 1)
  expect_error(tsn:::.tsn_distance(c(1, 1, 2), c(1, 2), "event_sync"),
               "strictly increasing")
})

test_that("audit: van Rossum equals a numerical integral of the kernels", {
  x <- c(1, 4, 7, 10)
  y <- c(1.2, 3.9, 8, 10)
  tau <- 1
  grid <- seq(0, 60, by = 1e-3)
  trace <- function(events) {
    rowSums(outer(grid, events, function(t, e) (t >= e) * exp(-(t - e) / tau)))
  }
  numeric_value <- sqrt(sum((trace(x) - trace(y))^2) * 1e-3 / tau)
  expect_equal(tsn:::.tsn_distance(x, y, "van_rossum", tolerance = tau),
               numeric_value, tolerance = 1e-3)
  # Two isolated spikes: D^2 = (1 + 1) / 2.
  expect_equal(tsn:::.tsn_distance(1, 1e6, "van_rossum", tolerance = 1), 1)
  expect_identical(tsn:::.tsn_distance(x, x, "van_rossum"), 0)
  # Default time constant is the median positive pooled inter-event interval.
  expect_equal(tsn:::.tsn_event_tolerance(x, y, NULL), 1.5)
  expect_identical(tsn:::.tsn_event_tolerance(5, 5, NULL), 1)
  expect_error(tsn:::.tsn_event_tolerance(x, y, 0))
})

test_that("audit: DTW equals the textbook recursion", {
  set.seed(106)
  invisible(lapply(seq_len(40L), function(index) {
    a <- stats::rnorm(sample(1:8, 1L))
    b <- stats::rnorm(sample(1:8, 1L))
    expect_equal(tsn:::.tsn_distance(a, b, "dtw"), audit_dtw(a, b),
                 tolerance = 1e-12)
    expect_equal(tsn:::.tsn_distance(a, b, "dtw"),
                 tsn:::.tsn_distance(b, a, "dtw"), tolerance = 1e-12)
  }))
  expect_identical(tsn:::.tsn_distance(4, 1, "dtw"), 3)
  expect_identical(tsn:::.tsn_distance(1, c(1, 2, 3), "dtw"), 3)
})

test_that("audit: identical series are at distance zero and metrics are symmetric", {
  set.seed(107)
  x <- stats::rnorm(15L)
  y <- stats::rnorm(15L)
  methods <- setdiff(tsn:::.tsn_distance_methods, c("event_sync", "van_rossum"))
  invisible(lapply(methods, function(method) {
    expect_equal(tsn:::.tsn_distance(x, x, method), 0, tolerance = 1e-12,
                 label = method)
    expect_equal(tsn:::.tsn_distance(x, y, method),
                 tsn:::.tsn_distance(y, x, method), tolerance = 1e-12,
                 label = method)
  }))
})

test_that("audit: pairwise dyads are combn-ordered and form a symmetric matrix", {
  set.seed(108)
  units <- stats::setNames(lapply(seq_len(5L), function(i) stats::rnorm(9L)),
                           c("e", "b", "d", "a", "c"))
  dyads <- tsn:::.tsn_pairwise_distance(units, "euclidean")
  pairs <- utils::combn(names(units), 2L)
  expect_identical(dyads$from, pairs[1L, ])
  expect_identical(dyads$to, pairs[2L, ])
  expect_identical(dyads, tsn:::.tsn_pairwise_distance(units, "euclidean"))
  reference <- as.matrix(stats::dist(do.call(rbind, units)))
  expect_equal(dyads$distance, reference[cbind(dyads$from, dyads$to)],
               tolerance = 1e-14)
  expect_true(isSymmetric(reference))
})

test_that("audit: chain distances link consecutive units within groups only", {
  units <- list(a = c(1, 2, 3), b = c(2, 2, 5), c = c(0, 1, 1), d = c(3, 1, 2))
  chain <- tsn:::.tsn_chain_distance(units, "manhattan")
  expect_identical(nrow(chain), length(units) - 1L)
  expect_identical(chain$from, c("a", "b", "c"))
  expect_identical(chain$to, c("b", "c", "d"))
  expect_equal(chain$distance, c(3, 7, 4))
  grouped <- tsn:::.tsn_chain_distance(units, "manhattan",
                                       groups = c(1, 1, 2, 2))
  expect_identical(grouped$from, c("a", "c"))
  separated <- tsn:::.tsn_chain_distance(units, "manhattan",
                                         groups = c(1, 2, 3, 4))
  expect_identical(nrow(separated), 0L)
  expect_identical(names(separated), c("from", "to", "distance"))
})

test_that("audit: invalid inputs fail early with informative messages", {
  expect_error(tsn:::.tsn_distance(c(1, NA), c(1, 2), "dtw"))
  expect_error(tsn:::.tsn_distance(c(1, Inf), c(1, 2), "euclidean"))
  expect_error(tsn:::.tsn_distance(1:3, 1:4, "euclidean"), "equal length")
  expect_error(tsn:::.tsn_chain_distance(list(a = 1:3, b = 1:4), "cosine"),
               "equal length")
  expect_error(tsn:::.tsn_pairwise_distance(list(a = 1:3, b = 1:4),
                                            "manhattan"),
               "equal length")
  expect_error(tsn:::.tsn_pairwise_distance(list(a = c(1, NaN), b = 1:2),
                                            "euclidean"),
               "non-empty finite numeric")
  expect_error(tsn:::.tsn_distance(1:2, 1:2, "minkowski", p = 0.5))
  # Unequal lengths are allowed for the documented methods.
  expect_equal(tsn:::.tsn_distance(c(1, 2, 3), c(2, 3), "dtw"), 1)
})
