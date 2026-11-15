# Regression tests from the function-by-function audit of R/input.R and
# R/connection.R.

test_that("id and time without value are rejected instead of ignored", {
  wide <- data.frame(a = c(1, 3, 2, 5), b = c(4, 6, 5, 2))
  expect_error(tsn(wide, id = "a", method = "distance"), "require `value`")
  expect_error(tsn(wide, time = "a", method = "distance"), "require `value`")
})

test_that("column arguments are rejected for non-data-frame input", {
  expect_error(tsn(c(1, 3, 2, 4), id = "a"), "cannot be used with numeric")
  expect_error(
    tsn:::.tsn_normalize_input(list(a = 1:3), time = "a"),
    "cannot be used with list"
  )
  expect_error(tsn(c(1, 2, 3), value = "x"), "`value` name columns")
  expect_error(tsn(list(a = 1:3, b = 2:4), value = "x", method = "distance"),
               "cannot be used with list")
  expect_error(tsn(matrix(1:6, 3), value = "x", method = "distance"),
               "cannot be used with matrix")
  expect_error(tsn(stats::ts(1:3), value = "x"), "cannot be used with ts")
  # A time vector used to be replaced by observation steps without notice.
  expect_error(vg(c(1, 2, 3), time = c(1, 5, 6)), "`time` name columns")
  expect_error(
    vg(c(0.1, 0.2), time = as.Date(c("2020-01-01", "2020-01-03"))),
    "`time` name columns"
  )
  expect_error(vg(c(1, 2, 3), time = "zzz"), "`time` name columns")
})

test_that("empty input reports the observation requirement", {
  expect_error(tsn(numeric(0)), "at least two observations")
  expect_error(tsn(data.frame(a = numeric(0))), "at least two observations")
})

test_that("a multivariate ts is wide data on the series time axis", {
  data <- stats::ts(cbind(p = c(1, 2, 3), q = c(4, 6, 5)),
                    start = 2000, frequency = 4)
  normalized <- tsn:::.tsn_normalize_input(data)
  expect_identical(normalized$id, rep(c("p", "q"), each = 3L))
  expect_equal(normalized$time, rep(c(2000, 2000.25, 2000.5), 2L))
  expect_equal(normalized$value, c(1, 2, 3, 4, 6, 5))
  expect_s3_class(tsn(data, method = "distance"), "tsn")
})

test_that("unnamed matrices use the same labels as unnamed lists", {
  data <- matrix(c(1, 2, 3, 4, 6, 5), ncol = 2L)
  expect_identical(
    unique(tsn:::.tsn_normalize_input(data)$id),
    unique(tsn:::.tsn_normalize_input(list(1:3, 4:6))$id)
  )
  expect_identical(
    unique(tsn:::.tsn_normalize_input(data)$id),
    c("series_1", "series_2")
  )
  expect_error(tsn(data, series = "V1", method = "distance"), "must be named")
})

test_that("partially named lists keep the supplied names", {
  normalized <- tsn:::.tsn_normalize_input(list(a = 1:3, 4:5))
  expect_identical(unique(normalized$id), c("a", "series_2"))
  expect_error(
    tsn:::.tsn_normalize_input(list(series_2 = 1:2, 3:4)),
    "unique"
  )
})

test_that("duplicate wide column names are reported as such", {
  data <- matrix(1:6, ncol = 2L, dimnames = list(NULL, c("a", "a")))
  expect_error(tsn:::.tsn_normalize_input(data), "column names must be unique")
  frame <- data.frame(a = 1:3, b = 4:6)
  names(frame) <- c("a", "a")
  expect_error(tsn:::.tsn_normalize_input(frame), "column names must be unique")
})

test_that("non-finite chronological time is rejected at normalization", {
  long <- data.frame(id = "a", time = c(1, 2, Inf), value = c(1, 3, 2))
  expect_error(
    tsn(long, value = "value", id = "id", time = "time"),
    "Time values must be finite"
  )
})

test_that("an impossible ISO date is an error, not a silent label axis", {
  first_bad <- data.frame(
    id = "a", time = c("2020-02-30", "2020-01-01", "2020-01-02"),
    value = c(1, 2, 3)
  )
  later_bad <- data.frame(
    id = "a", time = c("2020-01-01", "2020-02-30", "2020-01-02"),
    value = c(1, 2, 3)
  )
  expect_error(
    tsn:::.tsn_normalize_input(first_bad, value = "value", id = "id",
                               time = "time"),
    "not valid dates: 2020-02-30"
  )
  expect_error(
    tsn:::.tsn_normalize_input(later_bad, value = "value", id = "id",
                               time = "time"),
    "not valid dates: 2020-02-30"
  )
  valid <- data.frame(
    id = "a", time = c("2020-01-03", "2020-01-01", "2020-01-02"),
    value = c(3, 1, 2)
  )
  normalized <- tsn:::.tsn_normalize_input(valid, value = "value", id = "id",
                                           time = "time")
  expect_s3_class(normalized$time, "Date")
  expect_equal(normalized$value, c(1, 2, 3))
})

test_that("distinct numeric ids that render alike are not merged", {
  long <- data.frame(id = c(0.1 + 0.2, 0.3), time = c(1, 2), value = c(5, 6))
  expect_error(
    tsn:::.tsn_normalize_input(long, value = "value", id = "id",
                               time = "time"),
    "render to the same label"
  )
  integer_ids <- data.frame(id = c(2, 2, 1, 1), time = c(1, 2, 1, 2),
                            value = 1:4)
  expect_identical(
    unique(tsn:::.tsn_normalize_input(integer_ids, value = "value",
                                      id = "id", time = "time")$id),
    c("2", "1")
  )
})

test_that("difftime is a chronological axis", {
  long <- data.frame(id = "a", value = c(30, 10, 20))
  long$time <- as.difftime(c(3, 1, 2), units = "mins")
  normalized <- tsn:::.tsn_normalize_input(long, value = "value", id = "id",
                                           time = "time")
  expect_equal(normalized$value, c(10, 20, 30))
  expect_equal(tsn:::.tsn_time_coordinates(normalized$time), c(1, 2, 3))
})

test_that("time coordinates keep gaps and the raw origin", {
  expect_equal(tsn:::.tsn_time_coordinates(c(10, 12.5, 20)), c(10, 12.5, 20))
  dates <- as.Date("2020-01-01") + c(0, 3, 10)
  expect_equal(
    diff(tsn:::.tsn_time_coordinates(dates)),
    c(3, 7)
  )
  stamps <- as.POSIXct("2020-01-01", tz = "Asia/Tokyo") + c(0, 60, 3600)
  expect_equal(diff(tsn:::.tsn_time_coordinates(stamps)), c(60, 3540))
  expect_equal(tsn:::.tsn_time_coordinates(c("T1", "T10", "T2")), 1:3)
})

test_that("unsorted chronological time is sorted within each series", {
  long <- data.frame(
    id = c("b", "a", "b", "a", "b", "a"),
    time = c(3, 2, 1, 1, 2, 3),
    value = c(30, 20, 10, 1, 20, 3),
    state = c("b3", "a2", "b1", "a1", "b2", "a3")
  )
  network <- tsn(long, value = "value", id = "id", time = "time",
                 state = "state", unit = "state")
  series <- as.data.frame(network, what = "series")
  expect_identical(series$id, rep(c("b", "a"), each = 3L))
  expect_equal(series$time, rep(c(1, 2, 3), 2L))
  expect_identical(as.character(series$state),
                   c("b1", "b2", "b3", "a1", "a2", "a3"))
})

test_that("window and step must be whole numbers", {
  series <- list(a = 1:7, b = 11:15)
  expect_error(
    tsn(series, method = "distance", unit = "window", window = 2.7),
    "whole numbers"
  )
  expect_error(
    tsn(series, method = "distance", unit = "window", window = 3, step = 1.5),
    "whole numbers"
  )
  expect_error(
    tsn(series, method = "distance", unit = "window", window = 1e10),
    "exceeds the length of: a, b"
  )
  source <- tsn:::.tsn_normalize_input(series)
  huge_step <- tsn:::.tsn_window_units(source, window = 3, step = 1e10)
  expect_identical(huge_step$metadata$start, c(1L, 1L))
})

test_that("window units cover non-dividing, sparse, and full-length layouts", {
  source <- tsn:::.tsn_normalize_input(list(a = 1:7, b = 11:15))
  overlapping <- tsn:::.tsn_window_units(source, window = 3, step = 2)
  expect_identical(overlapping$metadata$start, c(1L, 3L, 5L, 1L, 3L))
  expect_identical(overlapping$metadata$end, c(3L, 5L, 7L, 3L, 5L))
  sparse <- tsn:::.tsn_window_units(source, window = 2, step = 3)
  expect_identical(sparse$metadata$start, c(1L, 4L, 1L, 4L))
  whole <- tsn:::.tsn_window_units(source, window = 5, step = 5)
  expect_identical(names(whole$units), c("a:W1", "b:W1"))
  expect_equal(unname(whole$units), list(c(1, 2, 3, 4, 5), c(11, 12, 13, 14, 15)))
})

test_that("window labels stay unique when a series name looks like a window", {
  source <- tsn:::.tsn_normalize_input(list(x = 1:4, "x:W1" = 1:2))
  units <- tsn:::.tsn_window_units(source, window = 2, step = 2)
  expect_false(anyDuplicated(names(units$units)) > 0L)
})

test_that("gaussian connection rejects a competing similarity kernel", {
  series <- list(a = c(1, 2, 3, 4), b = c(2, 2, 3, 5), c = c(5, 4, 3, 1))
  # Before the fix, connect = "gaussian" + similarity = "inverse" returned
  # 1 / (1 + d) weights and silently ignored `bandwidth`.
  expect_error(
    tsn(series, method = "distance", connect = "gaussian",
        similarity = "inverse", bandwidth = 5),
    "gaussian"
  )
  dyads <- data.frame(from = c("a", "a", "b"), to = c("b", "c", "c"),
                      distance = c(1, 2, 3))
  invisible(lapply(
    c("inverse", "normalized_inverse", "negative_exp", "gaussian"),
    function(kernel) {
      expect_error(
        tsn:::.tsn_connect(dyads, "gaussian", similarity = kernel,
                           bandwidth = 5),
        "already sets Gaussian weights"
      )
    }
  ))
  expect_equal(
    tsn:::.tsn_connect(dyads, "full", similarity = "gaussian",
                       bandwidth = 3)$weight,
    tsn:::.tsn_connect(dyads, "gaussian", bandwidth = 3)$weight
  )
})

test_that("a tiny gaussian bandwidth keeps zero distances at unit weight", {
  dyads <- data.frame(from = c("a", "a", "b"), to = c("b", "c", "c"),
                      distance = c(0, 1, 2))
  expect_warning(
    connected <- tsn:::.tsn_connect(dyads, "gaussian", bandwidth = 1e-300),
    class = "tsn_weight_underflow"
  )
  expect_equal(connected$weight, c(1, 0, 0))
  expect_identical(connected$connected, c(TRUE, TRUE, TRUE))
  kernel <- tsn:::.tsn_similarity_weights(c(0, 1), kernel = "gaussian",
                                          sigma = 1e-300)
  expect_equal(kernel, c(1, 0))
  expect_equal(
    tsn:::.tsn_connect(dyads, "gaussian", bandwidth = 2)$weight,
    exp(-(dyads$distance^2) / 8)
  )
})

test_that("percentile retains the type-7 order statistic despite float fuzz", {
  labels <- sprintf("n%03d", seq_len(102L))
  pairs <- data.frame(
    from = rep(labels[1L], 101L),
    to = labels[-1L],
    distance = seq_len(101L) + 0.5
  )
  connected <- tsn:::.tsn_connect(pairs, "percentile", percentile = 0.58)
  expect_identical(sum(connected$connected), 59L)
  interpolated <- tsn:::.tsn_connect(pairs, "percentile", percentile = 0.585)
  expect_identical(sum(interpolated$connected), 59L)
  expect_identical(
    tsn:::.tsn_percentile_cutoff(pairs$distance, 0.5),
    stats::quantile(pairs$distance, 0.5, names = FALSE, type = 7)
  )
  expect_identical(tsn:::.tsn_percentile_cutoff(numeric(0), 0.5), NA_real_)
})

test_that("connection argument errors name the argument", {
  dyads <- data.frame(from = c("a", "a", "b"), to = c("b", "c", "c"),
                      distance = c(1, 2, 3))
  expect_error(tsn:::.tsn_connect(dyads, "nearest"), "`neighbors` is required")
  expect_error(tsn:::.tsn_connect(dyads, "nearest", neighbors = 3),
               "smaller than the number of nodes \\(3\\)")
  expect_error(tsn:::.tsn_connect(dyads, "nearest", neighbors = 1.5),
               "positive whole number")
  expect_error(tsn:::.tsn_connect(dyads, "full", bandwidth = 2),
               "`bandwidth` cannot be used")
  expect_error(tsn:::.tsn_connect(dyads, "full", similarity = "inverse",
                                  bandwidth = 2),
               "`bandwidth` cannot be used")
  expect_error(tsn:::.tsn_connect(dyads, "threshold"), "`threshold` must be")
  expect_error(tsn:::.tsn_connect(dyads, "percentile", percentile = 2),
               "`percentile` must be")
  expect_error(
    tsn:::.tsn_connect(data.frame(from = "a", to = "a", distance = 1)),
    "self-pairs"
  )
  expect_error(
    tsn:::.tsn_connect(data.frame(from = "a", to = "b", distance = Inf)),
    "finite and non-negative"
  )
  series <- list(a = c(1, 2, 3, 4), b = c(2, 2, 3, 5), c = c(5, 4, 3, 1))
  expect_error(
    tsn(series, method = "distance", connect = "nearest", neighbors = 3),
    "smaller than the number of nodes \\(3\\)"
  )
})

test_that("nearest selection is invariant to dyad row order", {
  dyads <- data.frame(
    from = c("a", "a", "a", "b", "b", "c"),
    to = c("b", "c", "d", "c", "d", "d"),
    distance = c(1, 1, 2, 0, 3, 1)
  )
  key <- function(table) {
    sort(paste(table$from, table$to)[table$connected])
  }
  reversed <- dyads[rev(seq_len(nrow(dyads))), ]
  rownames(reversed) <- NULL
  invisible(lapply(c(FALSE, TRUE), function(directed) {
    expect_identical(
      key(tsn:::.tsn_connect(dyads, "nearest", neighbors = 1,
                             directed = directed)),
      key(tsn:::.tsn_connect(reversed, "nearest", neighbors = 1,
                             directed = directed))
    )
  }))
  undirected <- tsn:::.tsn_connect(dyads, "nearest", neighbors = 1)
  # The tie at node `a` (b and c both at distance 1) breaks to label `b`.
  expect_identical(key(undirected), c("a b", "b c", "c d"))
  incident <- table(c(undirected$from[undirected$connected],
                      undirected$to[undirected$connected]))
  expect_true(all(incident[c("a", "b", "c", "d")] >= 1L))
})

test_that("series selection order is authoritative for long and canonical data", {
  long <- data.frame(id = c("a", "b", "a", "c", "b"),
                     time = c(1, 1, 2, 1, 2), value = 1:5)
  selected <- tsn:::.tsn_select_data(long, series = c("c", "a"),
                                     value = "value", id = "id")
  expect_identical(selected$id, c("c", "a", "a"))
  canonical <- tsn:::.tsn_select_canonical(long, series = c("b", "c"))
  expect_identical(canonical$id, c("b", "b", "c"))
})

test_that("a kernel weight that underflows on a connected dyad warns", {
  series <- list(a = 1:5, b = c(500, 100, 400, 200, 300), c = 1:5 + 0.1)
  expect_warning(
    fit <- tsn(series, method = "distance", unit = "series",
               connect = "gaussian", bandwidth = 0.01),
    class = "tsn_weight_underflow"
  )
  edges <- as.data.frame(fit)
  # The rule still connects every pair; the warning flags the zero weights.
  expect_true(all(edges$connected))
  expect_identical(sum(edges$weight == 0), 2L)
  dyads <- data.frame(from = c("a", "a"), to = c("b", "c"),
                      distance = c(1, 1e4))
  expect_warning(
    tsn:::.tsn_connect(dyads, "full", similarity = "negative_exp",
                       bandwidth = 1),
    class = "tsn_weight_underflow"
  )
  # A zero weight that the kernel defines (the maximum distance under
  # normalized_inverse) or that sits on an unconnected dyad is not flagged.
  expect_no_warning(
    tsn:::.tsn_connect(dyads, "full", similarity = "normalized_inverse")
  )
  expect_no_warning(
    tsn:::.tsn_connect(dyads, "threshold", threshold = 1,
                       similarity = "negative_exp", bandwidth = 1)
  )
})

test_that("a state vector works with wide data in series order", {
  wide <- data.frame(a = c(1, 3, 2, 4), b = c(2, 1, 3, 2))
  states <- rep(c("x", "y"), 4L)
  network <- tsn(wide, unit = "state", state = states)
  series <- as.data.frame(network, what = "series")
  expect_identical(series$id, rep(c("a", "b"), each = 4L))
  expect_identical(as.character(series$state), states)
  expect_error(tsn(wide, unit = "state", state = c("x", "y")),
               "one non-missing value")
  single <- data.frame(v = c(1, 3, 2, 4))
  expect_identical(
    tsn:::.tsn_normalize_states(single, state = c("p", "q", "p", "q")),
    c("p", "q", "p", "q")
  )
})
