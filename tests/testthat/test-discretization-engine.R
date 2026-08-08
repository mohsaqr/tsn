engine <- function(...) tsn:::.tsn_discretize(...)

state_means <- function(values, result) {
  unname(tapply(values, result$state, mean))
}

expect_engine_invariants <- function(values, result) {
  expect_s3_class(result, "data.frame")
  expect_identical(names(result), c("index", "state", "probability"))
  expect_identical(result$index, seq_along(values))
  expect_length(result$state, length(values))
  expect_true(all(is.finite(result$probability)))
  expect_true(all(result$probability >= 0 & result$probability <= 1))
  expect_true(all(diff(state_means(values, result)) > 0))
  expect_type(attr(result, "model"), "list")
}

test_that("all discretization methods preserve order and label states by mean", {
  values <- c(-5, -4, -3, 0, 1, 2, 8, 9, 10)
  methods <- c("threshold", "width", "quantile", "kde", "kmeans",
               "gaussian", "hclust")
  results <- lapply(methods, function(method) {
    arguments <- list(
      values = values,
      method = method,
      n_states = 3L,
      seed = 41L
    )
    if (method == "threshold") {
      arguments$thresholds <- c(-1, 5)
    }
    do.call(engine, arguments)
  })

  invisible(Map(expect_engine_invariants, MoreArgs = list(values = values),
                result = results))
  expect_true(all(vapply(results, function(x) {
    identical(sort(unique(x$state)), seq_len(3L))
  }, logical(1L))))
})

test_that("manual thresholds are validated and retained", {
  values <- seq(-2, 2, length.out = 21)
  result <- engine(values, method = "threshold", n_states = 3L,
                   thresholds = c(-0.5, 0.5))

  expect_equal(attr(result, "breaks"), c(-Inf, -0.5, 0.5, Inf))
  expect_error(
    engine(values, method = "threshold", n_states = 3L,
           thresholds = 0),
    "exactly 2"
  )
  expect_error(
    engine(values, method = "threshold", n_states = 3L,
           thresholds = c(0, 3)),
    "strictly inside"
  )
})

test_that("constant and insufficiently diverse inputs fail informatively", {
  expect_error(
    engine(rep(2, 20), method = "width", n_states = 2L),
    "1 unique value"
  )
  expect_error(
    engine(c(1, 1, 2, 2), method = "gaussian", n_states = 3L),
    "2 unique value"
  )
  expect_error(engine(c(1, NA_real_), n_states = 2L), "finite numeric")
  expect_error(engine(c(1, Inf), n_states = 2L), "finite numeric")
})

test_that("quantile and KDE handle tied observations deterministically", {
  values <- c(rep(-2, 15), rep(-1, 2), rep(0, 20), rep(1, 2), rep(3, 15))
  quantile_result <- engine(values, method = "quantile", n_states = 3L)
  kde_first <- engine(values, method = "kde", n_states = 3L)
  kde_second <- engine(values, method = "kde", n_states = 3L)

  expect_engine_invariants(values, quantile_result)
  expect_engine_invariants(values, kde_first)
  expect_identical(kde_first, kde_second)
  expect_length(unique(quantile_result$state), 3L)
  expect_length(unique(kde_first$state), 3L)
})

test_that("state relabeling fixes non-self-inverse cluster order", {
  values <- c(20, 21, 0, 1, 10, 11)
  unordered_states <- c(1L, 1L, 2L, 2L, 3L, 3L)
  ordered <- tsn:::.tsn_order_states(values, unordered_states)

  expect_identical(ordered$state, c(3L, 3L, 1L, 1L, 2L, 2L))
  expect_equal(ordered$means, c(0.5, 10.5, 20.5))
})

test_that("seeded iterative methods are deterministic without leaking RNG", {
  values <- c(seq(-3, -1, length.out = 30), seq(1, 3, length.out = 30))
  set.seed(818)
  seed_before <- .Random.seed
  first <- engine(values, method = "gaussian", n_states = 2L, seed = 99L)
  seed_after <- .Random.seed
  second <- engine(values, method = "gaussian", n_states = 2L, seed = 99L)

  expect_identical(first, second)
  expect_identical(seed_before, seed_after)
  expect_identical(
    engine(values, method = "kmeans", n_states = 2L, seed = 14L),
    engine(values, method = "kmeans", n_states = 2L, seed = 14L)
  )
})

test_that("Gaussian mixture posterior and diagnostics are coherent", {
  set.seed(72)
  values <- c(
    stats::rnorm(80, -4, 0.35),
    stats::rnorm(90, 0, 0.45),
    stats::rnorm(70, 5, 0.30)
  )
  result <- engine(
    values,
    method = "gaussian",
    n_states = 3L,
    seed = 202L,
    max_iterations = 300L,
    tolerance = 1e-9
  )
  model <- attr(result, "model")

  expect_engine_invariants(values, result)
  expect_true(model$converged)
  expect_equal(rowSums(model$posterior), rep(1, length(values)), tolerance = 1e-10)
  expect_equal(result$probability, apply(model$posterior, 1L, max),
               tolerance = 1e-12)
  expect_true(all(model$weights > 0))
  expect_equal(sum(model$weights), 1, tolerance = 1e-12)
  expect_true(all(model$variances >= model$variance_floor))
  expect_true(all(diff(model$means) > 0))
  expect_equal(model$means, c(-4, 0, 5), tolerance = 0.2)
  expect_true(all(is.finite(c(model$log_likelihood, model$aic,
                              model$bic, model$icl))))
  expect_true(model$icl >= model$bic)
})

test_that("engine validates its scalar controls", {
  values <- 1:10
  expect_error(engine(values, n_states = 1L), "n_states")
  expect_error(engine(values, n_states = 2.5), "n_states")
  expect_error(engine(values, n_states = 2L, max_iterations = 0L),
               "max_iterations")
  expect_error(engine(values, n_states = 2L, tolerance = 0), "tolerance")
  expect_error(engine(values, n_states = 2L, seed = -1L), "seed")
  expect_error(engine(as.character(values), n_states = 2L), "is.numeric")
})

test_that("new mean-ordered methods preserve order and label states by mean", {
  values <- c(
    seq(1, 5, length.out = 40),
    seq(20, 25, length.out = 40),
    seq(50, 56, length.out = 40)
  )
  methods <- c("symbolic", "change_points", "entropy", "magnitude",
               "adaptive_magnitude", "percentile_magnitude", "dtw")
  results <- lapply(methods, function(method) {
    engine(values, method = method, n_states = 3L, seed = 41L)
  })
  invisible(Map(expect_engine_invariants, MoreArgs = list(values = values),
                result = results))
  expect_true(all(vapply(results, function(x) {
    identical(sort(unique(x$state)), seq_len(3L))
  }, logical(1L))))
})

test_that("ordinal patterns match a hand-computed m = 2 example", {
  result <- engine(c(4, 7, 9, 10, 6, 11, 3), method = "ordinal",
                   m = 2L, tau = 1L)
  model <- attr(result, "model")

  expect_identical(result$state, c(1L, 1L, 1L, 2L, 1L, 2L, 2L))
  expect_identical(model$patterns, c("1-2", "2-1"))
  expect_identical(model$n_windows, 6L)
  expect_identical(model$trailing_filled, 1L)
  expect_true(all(result$probability == 1))
})

test_that("ordinal discretization ignores n_states", {
  values <- c(3, 1, 4, 1, 5, 9, 2, 6)
  reference <- tsn:::.tsn_discretize(
    values,
    method = "ordinal",
    m = 3
  )
  ignored <- tsn:::.tsn_discretize(
    values,
    method = "ordinal",
    n_states = "not used",
    m = 3
  )

  expect_identical(ignored, reference)
})

test_that("ordinal handles m = 3 with a tie via first-occurrence rank", {
  result <- engine(c(1, 3, 2, 2, 5, 4, 4), method = "ordinal",
                   m = 3L, tau = 1L)
  model <- attr(result, "model")

  expect_identical(result$state, c(2L, 3L, 1L, 2L, 3L, 3L, 3L))
  expect_identical(model$patterns, c("1-2-3", "1-3-2", "3-1-2"))
  expect_identical(model$trailing_filled, 2L)
})

test_that("ordinal is deterministic and validates its embedding", {
  values <- as.numeric(c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5))
  expect_identical(
    engine(values, method = "ordinal", m = 3L, tau = 2L),
    engine(values, method = "ordinal", m = 3L, tau = 2L)
  )
  expect_error(engine(1:4, method = "ordinal", m = 3L, tau = 2L),
               "more than 4")
  expect_error(engine(1:10, method = "ordinal", m = 1L), "`m`")
  expect_error(engine(1:10, method = "ordinal", tau = 0L), "`tau`")
  expect_error(engine(1:10, method = "quantile", n_states = 2L, m = 3L),
               "only used")
})

test_that("symbolic breakpoints equal the Gaussian quantiles", {
  values <- as.numeric(seq(-3, 6, length.out = 60))
  result <- engine(values, method = "symbolic", n_states = 4L)
  model <- attr(result, "model")

  expect_equal(model$breakpoints, stats::qnorm(seq_len(3L) / 4L))
  expect_equal(model$mean, mean(values))
  expect_equal(model$sd, stats::sd(values))
  # value-scale boundaries are the z-breakpoints back-transformed
  expect_equal(attr(result, "breaks"),
               c(-Inf, model$breakpoints * model$sd + model$mean, Inf))
})

test_that("symbolic matches the legacy SAX implementation", {
  legacy_file <- Sys.getenv("TSN_LEGACY_DISCRETIZATION_FILE", unset = "")
  skip_if(
    !nzchar(legacy_file) || !file.exists(legacy_file),
    "set TSN_LEGACY_DISCRETIZATION_FILE to run the legacy equivalence check"
  )
  legacy_env <- new.env(parent = globalenv())
  suppressWarnings(sys.source(legacy_file, envir = legacy_env))
  set.seed(7)
  values <- stats::rnorm(50, 3, 2)
  legacy <- legacy_env$.discretize_symbolic(values, num_states = 3)
  mine <- engine(values, method = "symbolic", n_states = 3L)

  expect_identical(as.integer(mine$state), as.integer(legacy$states))
  expect_equal(max(abs(legacy$breakpoints[c(2L, 3L)] -
                         attr(mine, "model")$breakpoints)), 0)
})

test_that("change_points splits at the largest value gaps (hand fixture)", {
  values <- as.numeric(c(1, 2, 3, 10, 11, 12, 20, 21, 22))
  result <- engine(values, method = "change_points", n_states = 3L)

  expect_equal(attr(result, "breaks"), c(-Inf, 6.5, 16, Inf))
  expect_identical(result$state,
                   c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L))
  expect_equal(attr(result, "model")$change_points, c(6.5, 16))
})

test_that("entropy selects the maximum-entropy binning over the width grid", {
  values <- c(seq(1, 5, length.out = 30), seq(20, 24, length.out = 30),
              seq(40, 45, length.out = 30))
  result <- engine(values, method = "entropy", n_states = 3L)
  model <- attr(result, "model")

  lo <- min(values); hi <- max(values)
  grid_entropy <- vapply(model$width_grid, function(width) {
    internal <- lo + width * seq_len(2L)
    if (any(internal <= lo) || any(internal >= hi)) return(-Inf)
    counts <- tabulate(cut(values, c(-Inf, internal, Inf), labels = FALSE,
                           include.lowest = TRUE, right = TRUE), nbins = 3L)
    if (any(counts == 0L)) return(-Inf)
    probabilities <- counts / sum(counts)
    -sum(probabilities * log(probabilities))
  }, numeric(1L))
  expect_equal(model$entropy, max(grid_entropy))
  expect_identical(sort(unique(result$state)), seq_len(3L))
})

test_that("magnitude uses geometric breaks on absolute values", {
  values <- as.numeric(c(1, 2, 4, 8, 16, 32, 64, 128))
  result <- engine(values, method = "magnitude", n_states = 3L)
  scale <- attr(result, "model")$magnitude_scale

  expected <- exp(seq(log(1), log(128), length.out = 4L))[c(2L, 3L)]
  expect_equal(scale, expected)
  # geometric spacing: equal ratios on the log scale
  log_breaks <- log(c(1, scale, 128))
  expect_equal(diff(log_breaks), rep(diff(log_breaks)[1L], 3L))
})

test_that("adaptive_magnitude bins rolling z-scores with a partial start", {
  values <- as.numeric(c(seq(1, 10, length.out = 30),
                         seq(30, 20, length.out = 30)))
  result <- engine(values, method = "adaptive_magnitude", n_states = 3L)
  model <- attr(result, "model")

  expect_length(model$z_scores, length(values))
  expect_equal(model$z_scores[1L], 0)
  expect_identical(model$window, 10L)
  expect_identical(sort(unique(result$state)), seq_len(3L))
})

test_that("percentile_magnitude equals quantile on non-negative series", {
  values <- as.numeric(seq(2, 40, length.out = 45))
  magnitude <- engine(values, method = "percentile_magnitude", n_states = 3L)
  quantile_result <- engine(values, method = "quantile", n_states = 3L)

  expect_identical(magnitude$state, quantile_result$state)
  expect_error(
    engine(c(-2, -1, 1, 2), method = "percentile_magnitude", n_states = 3L),
    "distinct magnitudes"
  )
})

test_that("dtw shape clustering is deterministic and reports vote confidence", {
  values <- c(seq(1, 5, length.out = 40), seq(30, 34, length.out = 40),
              seq(1, 5, length.out = 40))
  first <- engine(values, method = "dtw", n_states = 3L)
  second <- engine(values, method = "dtw", n_states = 3L)
  model <- attr(first, "model")

  expect_identical(first, second)
  expect_true(all(first$probability > 0 & first$probability <= 1))
  expect_length(model$medoids, 3L)
  expect_identical(model$window, 12L)
  expect_error(engine(1:4, method = "dtw", n_states = 4L), "longer series")
})

test_that("deterministic PAM recovers well-separated groups", {
  coords <- c(0, 1, 2, 20, 21, 22)
  distance_matrix <- as.matrix(stats::dist(coords))
  clustering <- tsn:::.tsn_pam(distance_matrix, 2L)

  expect_length(clustering$medoids, 2L)
  low <- clustering$cluster[1L]
  high <- clustering$cluster[4L]
  expect_false(low == high)
  expect_identical(clustering$cluster[1:3], rep(low, 3L))
  expect_identical(clustering$cluster[4:6], rep(high, 3L))
})
