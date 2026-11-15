# Audit of the internal discretization engine (R/discretization.R).
# Each block pins either a confirmed defect fix or a verified invariant,
# checked against an independent base-R computation where one exists.

engine <- function(...) tsn:::.tsn_discretize(...)

trimodal_sample <- function() {
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else {
    NULL
  }
  on.exit(
    if (is.null(old_seed)) {
      rm(".Random.seed", envir = .GlobalEnv)
    } else {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    },
    add = TRUE
  )
  set.seed(42)
  sample(c(rnorm(40, 0, 0.3), rnorm(40, 5, 0.3), rnorm(40, 10, 0.3)))
}

test_that("kmeans records a scalar logical convergence flag", {
  values <- trimodal_sample()
  model <- attr(engine(values, "kmeans"), "model")
  expect_identical(model$converged, TRUE)

  noisy <- seq(-3, 3, length.out = 200)^3
  expect_warning(
    unconverged <- engine(noisy, "kmeans", max_iterations = 1L),
    "did not converge"
  )
  expect_identical(attr(unconverged, "model")$converged, FALSE)
})

test_that("kmeans matches stats::kmeans from the same initial centers", {
  values <- trimodal_sample()
  centers <- tsn:::.tsn_initial_centers(values, 3L)
  reference <- stats::kmeans(values, centers = centers, iter.max = 200L,
                             algorithm = "Lloyd")
  fit <- engine(values, "kmeans")
  expect_identical(fit$state, as.integer(reference$cluster))
  expect_equal(attr(fit, "model")$centers, as.numeric(reference$centers))
})

test_that("gaussian EM runs thousands of iterations without stack overflow", {
  values <- local({
    old <- if (exists(".Random.seed", envir = .GlobalEnv)) {
      get(".Random.seed", envir = .GlobalEnv)
    } else {
      NULL
    }
    on.exit(
      if (is.null(old)) rm(".Random.seed", envir = .GlobalEnv) else
        assign(".Random.seed", old, envir = .GlobalEnv),
      add = TRUE
    )
    set.seed(3)
    rnorm(500)
  })
  fit <- engine(values, "gaussian", max_iterations = 6000L)
  model <- attr(fit, "model")
  expect_true(model$iterations > 200L)
  expect_true(model$iterations <= 6000L)
  expect_type(model$converged, "logical")
  expect_length(model$converged, 1L)
})

test_that("gaussian EM stops at the iteration bound and reports it", {
  values <- trimodal_sample()
  model <- attr(engine(values, "gaussian", max_iterations = 2L), "model")
  expect_identical(model$iterations, 2L)
  expect_false(model$converged)
})

test_that("gaussian responsibilities sum to one per row with tiny variances", {
  values <- c(rep(0, 30), rep(1, 30), rep(2, 30))
  model <- attr(engine(values, "gaussian"), "model")
  expect_equal(rowSums(model$posterior), rep(1, 90), tolerance = 1e-12)
  expect_true(all(model$variances >= model$variance_floor))
  expectation <- tsn:::.tsn_gmm_expectation(
    c(0, 1e6), c(0.5, 0.5), c(0, 1), c(1e-20, 1e-20)
  )
  expect_true(all(is.finite(expectation$posterior)))
  expect_equal(rowSums(expectation$posterior), c(1, 1))
})

test_that("log-sum-exp is stable for very negative rows", {
  log_values <- matrix(c(-1000, -1001, -2000, -2000), nrow = 2, byrow = TRUE)
  expect_equal(
    tsn:::.tsn_log_sum_exp_rows(log_values),
    c(-1000 + log1p(exp(-1)), -2000 + log(2))
  )
})

test_that("count validation rejects out-of-integer-range values cleanly", {
  values <- trimodal_sample()
  expect_error(engine(values, "gaussian", seed = 3e9), "`seed` must be")
  expect_error(engine(values, n_states = 1e10), "`n_states` must be")
  expect_error(engine(values, n_states = 2.5), "`n_states` must be")
  expect_silent(tsn:::.tsn_check_count(.Machine$integer.max, "seed", 0L))
})

test_that("an unrealized state is surfaced with a classed warning", {
  values <- c(seq(-1, 1, length.out = 99), 100)
  expect_warning(
    fit <- engine(values, "width", n_states = 3L),
    class = "tsn_state_shortfall"
  )
  model <- attr(fit, "model")
  expect_identical(model$n_states, 2L)
  expect_identical(model$n_states_requested, 3L)
  expect_identical(sort(unique(fit$state)), 1:2)

  binary_like <- rep(c(0, 1, 2), c(90, 5, 5))
  expect_warning(engine(binary_like, "symbolic"), class = "tsn_state_shortfall")
})

test_that("a fully realized fit raises no shortfall warning", {
  values <- trimodal_sample()
  methods <- c("width", "quantile", "kde", "kmeans", "gaussian", "hclust",
               "symbolic", "change_points", "entropy",
               "percentile_magnitude")
  fits <- lapply(methods, function(method) {
    expect_no_warning(fit <- engine(values, method))
    fit
  })
  realized <- vapply(fits, function(fit) attr(fit, "model")$n_states,
                     integer(1L))
  expect_identical(realized, rep(3L, length(methods)))
  requested <- vapply(fits, function(fit) {
    attr(fit, "model")$n_states_requested
  }, integer(1L))
  expect_identical(requested, rep(3L, length(methods)))
  expect_identical(
    attr(engine(values, "ordinal"), "model")$n_states_requested,
    NA_integer_
  )
})

test_that("labels are ordered by value, not first appearance", {
  values <- c(10, 10.2, 0, 0.1, 5, 5.3, 10.1, 0.2, 5.1)
  methods <- c("width", "quantile", "kmeans", "gaussian", "hclust",
               "change_points", "entropy")
  states <- lapply(methods, function(method) engine(values, method)$state)
  expected <- c(3L, 3L, 1L, 1L, 2L, 2L, 3L, 1L, 2L)
  lapply(states, function(state) expect_identical(state, expected))
})

test_that("quantile breaks equal type-8 quantiles and right-closed cut()", {
  values <- trimodal_sample()
  fit <- engine(values, "quantile")
  interior <- stats::quantile(values, c(1 / 3, 2 / 3), type = 8, names = FALSE)
  expect_equal(attr(fit, "breaks"), c(-Inf, interior, Inf))
  expect_identical(
    fit$state,
    as.integer(cut(values, c(-Inf, interior, Inf), labels = FALSE))
  )
  boundary <- engine(c(1, 2, 3, 4, 5, 6), "threshold", thresholds = c(2, 4))
  expect_identical(boundary$state, c(1L, 1L, 2L, 2L, 3L, 3L))
})

test_that("tied quantile fallback follows the empirical distribution", {
  zero_inflated <- c(rep(0, 50), seq_len(50))
  fit <- engine(zero_inflated, "quantile")
  expect_equal(attr(fit, "breaks"), c(-Inf, 0.5, 17.5, Inf))
  expect_identical(tabulate(fit$state), c(50L, 17L, 33L))
  heavy_ties <- rep(c(1, 2, 3), c(50, 5, 5))
  expect_identical(tabulate(engine(heavy_ties, "quantile")$state),
                   c(50L, 5L, 5L))
  expect_error(
    tsn:::.tsn_quantile_breaks(c(1, 1, 2), 3L),
    "need at least 3 distinct values"
  )
})

test_that("accepted type-8 breaks that leave an empty bin fall back", {
  values <- c(0, 1, 9, 2, 1, 2, 2, 0, 9, 9, 1)
  # Type-8 quartiles are 1, 2, 7.83: distinct and inside the range, yet no
  # value lies in (2, 7.83], so the old check silently returned 3 states.
  expect_equal(
    stats::quantile(values, c(0.25, 0.5, 0.75), type = 8, names = FALSE),
    c(1, 2, 47 / 6)
  )
  fit <- expect_no_warning(engine(values, "quantile", n_states = 4L))
  expect_equal(attr(fit, "breaks"), c(-Inf, 0.5, 1.5, 5.5, Inf))
  expect_identical(tabulate(fit$state), c(2L, 3L, 3L, 3L))
})

test_that("quantile fallback breaks always leave every bin occupied", {
  draws <- lapply(seq_len(40), function(i) {
    old <- if (exists(".Random.seed", envir = .GlobalEnv)) {
      get(".Random.seed", envir = .GlobalEnv)
    } else {
      NULL
    }
    on.exit(
      if (is.null(old)) rm(".Random.seed", envir = .GlobalEnv) else
        assign(".Random.seed", old, envir = .GlobalEnv),
      add = TRUE
    )
    set.seed(i)
    sample(0:4, 30, replace = TRUE, prob = c(0.7, 0.1, 0.1, 0.05, 0.05))
  })
  occupied <- vapply(draws, function(values) {
    k <- min(4L, length(unique(values)))
    breaks <- tsn:::.tsn_quantile_breaks(values, k)
    all(tabulate(cut(values, breaks, labels = FALSE), nbins = k) > 0L)
  }, logical(1L))
  expect_true(all(occupied))
})

test_that("width and hclust agree with base cut() and cutree()", {
  values <- trimodal_sample()
  expect_identical(
    engine(values, "width")$state,
    as.integer(cut(values, 3, labels = FALSE))
  )
  reference <- stats::cutree(stats::hclust(stats::dist(values), "complete"), 3)
  cross <- table(engine(values, "hclust")$state, reference)
  expect_identical(sum(cross > 0L), 3L)
})

test_that("kde breaks sit at stats::density() minima", {
  values <- trimodal_sample()
  fit <- engine(values, "kde")
  estimate <- stats::density(values, n = 512L)
  minima <- which(diff(sign(diff(estimate$y))) == 2) + 1L
  expect_false(attr(fit, "model")$fallback)
  expect_equal(attr(fit, "breaks")[2:3], estimate$x[minima])
  flat <- attr(engine(seq(0, 1, length.out = 300), "kde"), "model")
  expect_true(flat$fallback)
})

test_that(".tsn_with_seed restores RNG state, including on error", {
  old <- if (exists(".Random.seed", envir = .GlobalEnv)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else {
    NULL
  }
  on.exit(
    if (is.null(old)) {
      if (exists(".Random.seed", envir = .GlobalEnv)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    } else {
      assign(".Random.seed", old, envir = .GlobalEnv)
    },
    add = TRUE
  )
  set.seed(99)
  before <- get(".Random.seed", envir = .GlobalEnv)
  expect_error(tsn:::.tsn_with_seed(5L, stop("boom")), "boom")
  expect_identical(get(".Random.seed", envir = .GlobalEnv), before)
  tsn:::.tsn_with_seed(5L, stats::runif(3))
  expect_identical(get(".Random.seed", envir = .GlobalEnv), before)

  rm(".Random.seed", envir = .GlobalEnv)
  tsn:::.tsn_with_seed(5L, stats::runif(1))
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  expect_error(tsn:::.tsn_with_seed(5L, stop("boom")), "boom")
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))

  expect_identical(
    tsn:::.tsn_with_seed(7L, stats::runif(2)),
    tsn:::.tsn_with_seed(7L, stats::runif(2))
  )
})

test_that("ordinal patterns are a bijection ordered by numeric rank vectors", {
  values <- trimodal_sample()
  fit <- tsn:::.tsn_discretize_ordinal(values, m = 10L)
  patterns <- fit$model$patterns
  ranks <- do.call(rbind, lapply(strsplit(patterns, "-"), as.integer))
  expect_identical(do.call(order, as.data.frame(ranks)), seq_along(patterns))
  expect_identical(anyDuplicated(patterns), 0L)

  short <- tsn:::.tsn_discretize_ordinal(c(4, 1, 3, 2, 5, 9, 7),
                                         m = 3L, tau = 2L)
  expect_identical(short$model$n_windows, 3L)
  expect_identical(short$model$trailing_filled, 4L)
  expect_identical(short$model$patterns, c("1-2-3", "2-1-3"))
  expect_identical(short$state, c(2L, 1L, 1L, 1L, 1L, 1L, 1L))
  expect_identical(
    tsn:::.tsn_discretize_ordinal(c(5, 4, 4), m = 3L)$model$patterns,
    "3-1-2"
  )
})

test_that("change points and entropy recover a step function", {
  step <- c(rep(1, 20), rep(5, 20), rep(9, 20)) + rep(c(0, 0.01), 30)
  expected <- rep(1:3, each = 20)
  expect_identical(engine(step, "change_points")$state, expected)
  expect_identical(engine(step, "entropy")$state, expected)
  model <- attr(engine(c(1, 2, 3, 4, 10, 11, 20), "change_points"), "model")
  expect_equal(model$change_points, c(7, 15.5))
  expect_equal(model$gaps, c(6, 9))
})

test_that("rolling z-scores use partial trailing windows at the start", {
  values <- c(1, 3, 2, 5, 4)
  manual <- c(
    0,
    (3 - 2) / stats::sd(c(1, 3)),
    0,
    (5 - mean(c(3, 2, 5))) / stats::sd(c(3, 2, 5)),
    (4 - mean(c(2, 5, 4))) / stats::sd(c(2, 5, 4))
  )
  expect_equal(tsn:::.tsn_rolling_zscore(values, 3L), manual)
  expect_identical(tsn:::.tsn_rolling_zscore(c(1, 1, 1), 3L), c(0, 0, 0))
})

test_that("magnitude decade boundaries fall in the lower right-closed bin", {
  fit <- engine(c(1, 10, 100, 1000, 5, 50, 500), "magnitude")
  expect_identical(attr(fit, "breaks"), c(-Inf, 10, 100, Inf))
  expect_identical(fit$state, c(1L, 1L, 2L, 3L, 1L, 2L, 3L))
})

test_that("percentile magnitude equals quantile on positive data", {
  values <- abs(trimodal_sample()) + 1
  expect_identical(
    engine(values, "percentile_magnitude")$state,
    engine(values, "quantile")$state
  )
})

test_that("symbolic uses Gaussian breakpoints on the z-scored series", {
  values <- trimodal_sample()
  z <- (values - mean(values)) / stats::sd(values)
  reference <- cut(z, c(-Inf, stats::qnorm(c(1 / 3, 2 / 3)), Inf),
                   labels = FALSE)
  expect_identical(engine(values, "symbolic")$state, as.integer(reference))
})

test_that("DTW matrix is symmetric and PAM is deterministic and optimal", {
  values <- cumsum(seq(-1, 1, length.out = 60)^3 + sin(seq_len(60)))
  windows <- lapply(seq_len(50), function(start) values[start + 0:10])
  distances <- tsn:::.tsn_dtw_matrix(windows)
  expect_true(isSymmetric(distances))
  expect_identical(unname(diag(distances)), rep(0, 50))
  expect_equal(distances[3, 17],
               tsn:::.tsn_dtw_distance(windows[[3]], windows[[17]]))
  first <- tsn:::.tsn_pam(distances, 3L)
  expect_identical(first, tsn:::.tsn_pam(distances, 3L))
  skip_if_not_installed("cluster")
  reference <- cluster::pam(stats::as.dist(distances), 3L, diss = TRUE)
  cost <- function(medoids) {
    sum(apply(distances[, medoids, drop = FALSE], 1L, min))
  }
  expect_equal(cost(first$medoids), cost(reference$id.med))
})

test_that("window vote breaks ties toward the smallest cluster index", {
  vote <- tsn:::.tsn_window_vote(c(1L, 2L, 1L, 2L), window = 2L, n = 5L)
  expect_identical(vote$state, c(1L, 1L, 1L, 1L, 2L))
  expect_equal(vote$confidence, c(1, 0.5, 0.5, 0.5, 1))
  reversed <- tsn:::.tsn_window_vote(c(2L, 1L), window = 2L, n = 3L)
  expect_identical(reversed$state, c(2L, 1L, 1L))
})

test_that("degenerate inputs fail early and informatively", {
  expect_error(engine(rep(2, 10)), "1 unique value")
  expect_error(engine(5), "1 unique value")
  expect_error(engine(c(1, NA, 3)), "finite numeric")
  expect_error(engine(c(1, 2), "ordinal"), "needs more than 2")
  expect_error(engine(c(1, 2), "dtw", n_states = 2L), "longer series")
  expect_identical(engine(c(1, 2), "quantile", n_states = 2L)$state, 1:2)
  expect_identical(
    attr(engine(rep(2, 10), "ordinal"), "model")$n_patterns, 1L
  )
})

test_that("threshold errors name the public `breaks` argument", {
  values <- c(1, 2, 3, 4, 5, 6)
  expect_error(
    engine(values, "threshold", n_states = 4L, thresholds = c(2.5, 4.5)),
    "`breaks` must contain exactly 3 finite numeric value\\(s\\)"
  )
  expect_error(
    engine(values, "threshold", n_states = 3L, thresholds = c(2.5, 10)),
    "`breaks` must be distinct and strictly inside"
  )
  expect_error(
    tsn(values, discretization = "threshold", unit = "state",
        breaks = c(2.5, 4.5), n_states = 4),
    "`breaks` must contain exactly 3"
  )
})
