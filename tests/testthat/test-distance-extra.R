# New distance measures (ccf, nmi, voi, event_sync, van_rossum), similarity
# kernels, and normalize modes.

test_that("ccf distance matches stats::ccf and honours lag", {
  set.seed(11)
  x <- as.numeric(scale(cumsum(rnorm(60))))
  y <- as.numeric(scale(0.5 * x + 0.5 * rnorm(60)))
  reference <- 1 - min(1, max(abs(stats::ccf(x, y, plot = FALSE)$acf)))
  expect_equal(.tsn_ccf_distance(x, y), reference)
  reference_lagged <- 1 - min(
    1, max(abs(stats::ccf(x, y, lag.max = 3, plot = FALSE)$acf))
  )
  expect_equal(.tsn_ccf_distance(x, y, lag = 3), reference_lagged)
  expect_equal(.tsn_ccf_distance(x, x), 0)
  expect_error(.tsn_ccf_distance(rep(1, 10), x[1:10]))
})

test_that("nmi and voi distances match entropy identities on marginal bins", {
  set.seed(7)
  x <- as.numeric(scale(cumsum(rnorm(80))))
  y <- as.numeric(scale(0.6 * x + 0.4 * rnorm(80)))
  labels <- list(
    x = .tsn_marginal_quantile_bins(x, bins = 10L),
    y = .tsn_marginal_quantile_bins(y, bins = 10L)
  )
  x_probabilities <- proportions(table(labels$x))
  y_probabilities <- proportions(table(labels$y))
  joint_probabilities <- proportions(table(labels$x, labels$y))
  positive_joint <- joint_probabilities[joint_probabilities > 0]
  entropy_x <- -sum(x_probabilities * log(x_probabilities))
  entropy_y <- -sum(y_probabilities * log(y_probabilities))
  entropy_joint <- -sum(positive_joint * log(positive_joint))
  mutual_information <- entropy_x + entropy_y - entropy_joint

  expect_equal(
    .tsn_information_distance(x, y, bins = 10L, type = "nmi"),
    1 - mutual_information / max(entropy_x, entropy_y),
    tolerance = 1e-10
  )
  expect_equal(
    .tsn_information_distance(x, y, bins = 10L, type = "voi"),
    2 * entropy_joint - entropy_x - entropy_y,
    tolerance = 1e-10
  )
})

test_that("information distances satisfy their identities and bounds", {
  set.seed(3)
  x <- rnorm(100)
  y <- rnorm(100)
  expect_equal(.tsn_information_distance(x, x, type = "nmi"), 0)
  expect_equal(.tsn_information_distance(x, x, type = "voi"), 0)
  nmi_distance <- .tsn_information_distance(x, y, type = "nmi")
  expect_gte(nmi_distance, 0)
  expect_lte(nmi_distance, 1)
  expect_gte(.tsn_information_distance(x, y, type = "voi"), 0)
  constant <- rep(2, 100)
  expect_equal(.tsn_information_distance(constant, constant, type = "nmi"), 0)

  increasing <- seq_len(60L)
  rescaled <- 2 * increasing + 5
  expect_equal(
    .tsn_information_distance(increasing, rescaled, bins = 10, type = "nmi"),
    0
  )
  expect_equal(
    .tsn_information_distance(increasing, rescaled, bins = 10, type = "voi"),
    0
  )
})

event_sync_reference <- function(x, y, tolerance = NULL) {
  stopifnot(
    is.numeric(x), length(x) > 0L, !is.unsorted(x, strictly = TRUE),
    is.numeric(y), length(y) > 0L, !is.unsorted(y, strictly = TRUE)
  )
  local_window <- function(events) {
    pmin(c(Inf, diff(events)), c(diff(events), Inf)) / 2
  }
  windows <- outer(local_window(x), local_window(y), pmin)
  if (!is.null(tolerance)) {
    windows <- pmin(windows, tolerance)
  }
  gaps <- outer(x, y, "-")
  coincidences <- sum(abs(gaps) > 0 & abs(gaps) <= windows) +
    sum(gaps == 0)
  1 - coincidences / sqrt(length(x) * length(y))
}

test_that("event synchronization matches the adaptive Quiroga definition", {
  train <- c(1, 4, 9, 15)
  expect_equal(.tsn_event_sync_distance(train, train, tolerance = 0.1), 0)
  expect_equal(
    .tsn_event_sync_distance(c(1, 2, 3), c(100, 200, 300), tolerance = 0.5),
    1
  )
  half <- .tsn_event_sync_distance(c(1, 2), c(1, 50), tolerance = 0.25)
  expect_equal(half, 0.5)

  first <- c(1, 19, 21, 27, 30)
  second <- c(7, 10, 14, 22, 30)
  expect_equal(
    .tsn_event_sync_distance(first, second, tolerance = 3),
    event_sync_reference(first, second, tolerance = 3)
  )
  # The original definition includes coincidences exactly on the window
  # boundary (`<= tau`), yielding two matches here.
  expect_equal(.tsn_event_sync_distance(first, second, tolerance = 3), 0.6)
  expect_equal(
    .tsn_event_sync_distance(first, second),
    event_sync_reference(first, second)
  )
})

test_that("event-based distances require ordered unique event times", {
  expect_error(
    .tsn_event_sync_distance(c(1, 3, 2), c(1, 2, 3)),
    "strictly increasing"
  )
  expect_error(
    .tsn_event_sync_distance(c(1, 1, 2), c(1, 2, 3)),
    "strictly increasing"
  )
  expect_error(
    .tsn_van_rossum_distance(c(2, 1), c(1, 2), tolerance = 1),
    "strictly increasing"
  )
})

test_that("van Rossum distance matches its closed forms", {
  expect_equal(.tsn_van_rossum_distance(c(1, 5, 9), c(1, 5, 9), tolerance = 2), 0)
  # Single spikes at 0 and t: D = sqrt(1 - exp(-t / tau)).
  tau <- 2
  t_offset <- 3
  expect_equal(
    .tsn_van_rossum_distance(0, t_offset, tolerance = tau),
    sqrt(1 - exp(-t_offset / tau)),
    tolerance = 1e-12
  )
  # Exact kernel-sum form agrees with the legacy grid approximation.
  grid_van_rossum <- function(s1, s2, tau, dt) {
    time_vector <- seq(0, max(s1, s2) + 12 * tau, by = dt)
    filtered <- function(spikes) {
      Reduce(
        `+`,
        lapply(
          spikes,
          function(s) ifelse(time_vector >= s, exp(-(time_vector - s) / tau), 0)
        )
      )
    }
    sqrt(sum((filtered(s1) - filtered(s2))^2) * dt / tau)
  }
  first <- c(1, 5, 9, 14, 20, 26)
  second <- c(2, 6, 10, 15, 22, 25)
  expect_equal(
    .tsn_van_rossum_distance(first, second, tolerance = 3),
    grid_van_rossum(first, second, tau = 3, dt = 0.001),
    tolerance = 1e-3
  )
})

test_that("new distances run through tsn() including chains", {
  set.seed(42)
  series <- list(
    a = as.numeric(scale(cumsum(rnorm(40)))),
    b = as.numeric(scale(cumsum(rnorm(40)))),
    c = as.numeric(scale(cumsum(rnorm(40))))
  )
  invisible(lapply(c("ccf", "nmi", "voi"), function(measure) {
    fit <- tsn(series, method = "distance", unit = "series", distance = measure)
    expect_s3_class(fit, "tsn")
    expect_equal(fit$n_edges, 3L)
    expect_true(all(is.finite(fit$table$distance)))
    expect_true(all(fit$table$distance >= 0))
  }))
  events <- list(
    e1 = c(1, 5, 9, 14, 20, 26),
    e2 = c(1.2, 5.4, 8.8, 14.5, 19.7, 25.8),
    e3 = c(3, 11, 17, 23)
  )
  invisible(lapply(c("event_sync", "van_rossum"), function(measure) {
    fit <- tsn(events, method = "distance", unit = "series", distance = measure)
    expect_s3_class(fit, "tsn")
    expect_equal(fit$n_edges, 3L)
  }))
  chained <- tsn(
    events,
    method = "distance", unit = "series",
    distance = "van_rossum", tolerance = 2, chain = TRUE
  )
  expect_equal(nrow(chained$table), 2L)
})

test_that("event distances accept unequal lengths, others still refuse", {
  events <- list(short = c(1, 2), long = c(1, 2, 3, 4, 5))
  fit <- tsn(events, method = "distance", unit = "series",
             distance = "event_sync", tolerance = 0.5)
  expect_s3_class(fit, "tsn")
  expect_error(
    tsn(events, method = "distance", unit = "series", distance = "euclidean")
  )
})

test_that("similarity kernels drive edge weights", {
  set.seed(1)
  series <- list(
    a = rnorm(30), b = rnorm(30), c = rnorm(30)
  )
  base_fit <- tsn(series, method = "distance", unit = "series",
                  distance = "euclidean")
  expect_equal(base_fit$table$weight, 1 / (1 + base_fit$table$distance))
  explicit <- tsn(series, method = "distance", unit = "series",
                  distance = "euclidean", similarity = "inverse")
  expect_equal(explicit$table$weight, base_fit$table$weight)
  scaled <- tsn(series, method = "distance", unit = "series",
                distance = "euclidean", similarity = "negative_exp",
                bandwidth = 2)
  expect_equal(scaled$table$weight, exp(-scaled$table$distance / 2))
  gaussian_kernel <- tsn(series, method = "distance", unit = "series",
                         distance = "euclidean", similarity = "gaussian",
                         bandwidth = 3)
  expect_equal(
    gaussian_kernel$table$weight,
    exp(-(gaussian_kernel$table$distance^2) / (2 * 9))
  )
  rescaled <- tsn(series, method = "distance", unit = "series",
                  distance = "euclidean", similarity = "normalized_inverse")
  expect_equal(
    rescaled$table$weight,
    1 - rescaled$table$distance / max(rescaled$table$distance)
  )
  expect_true(all(rescaled$table$weight >= 0 & rescaled$table$weight <= 1))
})

test_that("normalize accepts logical and mode names", {
  set.seed(2)
  series <- list(a = rnorm(25), b = rnorm(25), c = rnorm(25), d = rnorm(25))
  raw <- tsn(series, method = "distance", unit = "series",
             distance = "euclidean")
  legacy <- tsn(series, method = "distance", unit = "series",
                distance = "euclidean", normalize = TRUE)
  named_max <- tsn(series, method = "distance", unit = "series",
                   distance = "euclidean", normalize = "max")
  expect_equal(legacy$table$distance, named_max$table$distance)
  expect_equal(
    legacy$table$distance,
    raw$table$distance / max(raw$table$distance)
  )
  minmax <- tsn(series, method = "distance", unit = "series",
                distance = "euclidean", normalize = "minmax")
  expect_equal(min(minmax$table$distance), 0)
  expect_equal(max(minmax$table$distance), 1)
  winsorized <- tsn(series, method = "distance", unit = "series",
                    distance = "euclidean", normalize = "quantile")
  expect_true(all(winsorized$table$distance >= 0))
  expect_true(all(winsorized$table$distance <= 1))
})

test_that("method-specific options are validated", {
  series <- list(a = rnorm(20), b = rnorm(20))
  expect_error(
    tsn(series, method = "distance", unit = "series",
        distance = "euclidean", bins = 5),
    "bins"
  )
  expect_error(
    tsn(series, method = "distance", unit = "series",
        distance = "euclidean", lag = 3),
    "lag"
  )
  expect_error(
    tsn(series, method = "distance", unit = "series",
        distance = "euclidean", tolerance = 1),
    "tolerance"
  )
  expect_error(
    tsn(c(3, 1, 4, 2, 5), "hvg", similarity = "gaussian"),
    "distance networks"
  )
  expect_error(
    tsn(series, method = "distance", unit = "series",
        distance = "euclidean", similarity = "reciprocal")
  )
  expect_error(
    tsn(series, method = "distance", unit = "series",
        distance = "nmi", bins = 1)
  )
})

test_that("bins and lag change the respective distances", {
  set.seed(9)
  x <- as.numeric(scale(cumsum(rnorm(80))))
  y <- as.numeric(scale(cumsum(rnorm(80))))
  coarse <- .tsn_information_distance(x, y, bins = 3L, type = "voi")
  fine <- .tsn_information_distance(x, y, bins = 15L, type = "voi")
  expect_false(isTRUE(all.equal(coarse, fine)))
  narrow <- .tsn_ccf_distance(x, y, lag = 1)
  wide <- .tsn_ccf_distance(x, y, lag = 20)
  expect_lte(wide, narrow)
})
