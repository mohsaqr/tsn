# Audit of the core constructor (R/tsn.R): argument consumption, shortcut and
# abbreviation resolution, state ordering, normalization, and the result
# contract.

audit_series <- list(
  a = c(1, 3, 2, 5, 4, 6),
  b = c(2, 1, 4, 3, 6, 5),
  c = c(5, 4, 6, 3, 2, 4)
)
audit_values <- c(3, 1, 4, 2, 5, 3, 6, 2, 7, 1, 5, 3)

test_that("audit: abbreviated shortcuts cannot bypass the conflict checks", {
  # "hv" used to build a horizontal graph while silently discarding the
  # explicit `visibility = "natural"`.
  expect_error(tsn(audit_values, "hv", visibility = "natural"), "hvg")
  # "quant" used to discard an explicit discretizer, and with `unit = "time"`
  # it built a plain natural visibility graph with no discretization at all.
  expect_error(tsn(audit_values, "quant", discretization = "kmeans"),
               "quantile")
  expect_error(tsn(audit_values, "quant", unit = "time"), "state network")
  expect_identical(
    as.data.frame(tsn(audit_values, "quant")),
    as.data.frame(tsn(audit_values, "quantile"))
  )
  expect_error(tsn(audit_values, method = 1), "single non-missing")
  expect_error(tsn(audit_values, method = NA_character_), "single non-missing")
})

test_that("audit: abbreviated selectors are completed before validation", {
  near <- tsn(audit_series, "distance", connect = "near", neighbors = 1)
  expect_identical(unique(as.data.frame(near)$connection_method), "nearest")
  expect_identical(
    unique(as.data.frame(tsn(audit_series, "distance",
                             distance = "euc"))$distance_method),
    "euclidean"
  )
  # These used to be rejected because the validators compared raw strings.
  minkowski <- tsn(audit_series, "distance", distance = "mink", p = 3)
  expect_identical(unique(as.data.frame(minkowski)$distance_method),
                   "minkowski")
  expect_s3_class(tsn(audit_series, "distance", distance = "nm", bins = 3),
                  "tsn")
  expect_s3_class(
    tsn(audit_values, unit = "state", discretization = "thresh",
        breaks = c(3, 5)),
    "tsn"
  )
  expect_s3_class(
    tsn(audit_series, "distance", similarity = "neg", bandwidth = 2),
    "tsn"
  )
  expect_identical(
    unique(as.data.frame(tsn(audit_values, visibility = "hor"))$connection_method),
    "horizontal"
  )
  expect_s3_class(tsn(audit_values, "quantile", unit = "st"), "tsn")
  # Invalid values are still rejected by their owner.
  expect_error(tsn(audit_series, "distance", connect = "bogus"), "should be one")
  expect_error(tsn(audit_series, "distance", distance = "bogus"),
               "should be one")
})

test_that("audit: connection-rule options are consumed or rejected by name", {
  expect_error(tsn(audit_series, "distance", neighbors = 1),
               "`neighbors` cannot be used unless `connect = \"nearest\"`")
  expect_error(tsn(audit_series, "distance", threshold = 1),
               "`threshold` cannot be used unless")
  expect_error(tsn(audit_series, "distance", percentile = 0.5),
               "`percentile` cannot be used unless")
  expect_error(tsn(audit_series, "distance", bandwidth = 1), "`bandwidth`")
  expect_error(
    tsn(audit_series, "distance", connect = "nearest", neighbors = 1,
        bandwidth = 1),
    "`bandwidth`"
  )
  expect_error(tsn(audit_series, "distance", connect = "nearest"),
               "requires `neighbors`")
  expect_error(tsn(audit_series, "distance", connect = "percentile"),
               "requires `percentile`")
  # connect = "gaussian" with a kernel used to drop the Gaussian rule and the
  # bandwidth silently: bandwidth 2 and 200 gave identical weights.
  expect_error(
    tsn(audit_series, "distance", connect = "gaussian",
        similarity = "inverse", bandwidth = 2),
    "`similarity`"
  )
  # The consuming configurations still work.
  expect_s3_class(
    tsn(audit_series, "distance", similarity = "negative_exp", bandwidth = 1),
    "tsn"
  )
  expect_s3_class(
    tsn(audit_series, "distance", connect = "gaussian", bandwidth = 1), "tsn"
  )
  expect_s3_class(
    tsn(audit_series, "distance", connect = "threshold", threshold = 4,
        similarity = "inverse"),
    "tsn"
  )
})

test_that("audit: distance-only options are rejected on visibility networks by name", {
  expect_error(tsn(audit_values, threshold = 1),
               "`threshold` cannot be used with visibility")
  expect_error(tsn(audit_values, neighbors = 2),
               "`neighbors` cannot be used with visibility")
  expect_error(tsn(audit_values, window = 3L),
               "`window` cannot be used with visibility")
  expect_error(tsn(audit_values, "hvg", bandwidth = 1), "`bandwidth`")
})

test_that("audit: sequence measures are rejected for single-value time nodes", {
  invisible(lapply(
    c("correlation", "spearman", "ccf", "nmi", "voi", "cosine"),
    function(measure) {
      expect_error(
        tsn(audit_series, "distance", unit = "time", distance = measure),
        "unit = \"time\""
      )
    }
  ))
  expect_s3_class(
    tsn(audit_series, "distance", unit = "time", distance = "manhattan"),
    "tsn"
  )
  expect_s3_class(
    tsn(audit_series, "distance", unit = "window", window = 3L,
        distance = "correlation"),
    "tsn"
  )
})

test_that("audit: ccf lag must be shorter than the units", {
  # stats::ccf() silently capped an over-long lag at n - 1.
  expect_error(
    tsn(audit_series, "distance", distance = "ccf", lag = 6),
    "shortest unit length \\(6\\)"
  )
  expect_error(
    tsn(audit_series, "distance", unit = "window", window = 3L,
        distance = "ccf", lag = 3),
    "shortest unit length \\(3\\)"
  )
  expect_s3_class(
    tsn(audit_series, "distance", distance = "ccf", lag = 5), "tsn"
  )
})

test_that("audit: the threshold discretizer requires and sizes from breaks", {
  expect_error(tsn(audit_values, "threshold"), "requires `breaks`")
  # Three breaks used to fail against the default n_states = 3.
  four <- tsn(audit_values, "threshold", breaks = c(2.5, 4.5, 6.5))
  expect_identical(four$nodes$label, c("1", "2", "3", "4"))
  three <- tsn(audit_values, "threshold", breaks = c(2.5, 4.5), n_states = 3)
  expect_identical(three$nodes$label, c("1", "2", "3"))
  expect_error(
    tsn(audit_values, "threshold", breaks = c(2.5, 4.5), n_states = 4)
  )
})

test_that("audit: scalar options give named validation messages", {
  expect_error(tsn(audit_series, "distance", chain = "yes"),
               "`chain` must be TRUE or FALSE")
  expect_error(tsn(audit_values, directed = NA),
               "`directed` must be TRUE or FALSE")
  expect_error(tsn(audit_values, penetrable = 1.5), "`penetrable`")
  # A huge penetrable used to warn about integer coercion in this validator.
  expect_no_warning(tsn:::.tsn_validate_public_arguments(
    method = "visibility", unit = "time", window = NULL, neighbors = NULL,
    threshold = NULL, percentile = NULL, bandwidth = NULL, state = NULL,
    directed = FALSE, limit = NULL, penetrable = 1e10, decay = 0
  ))
  expect_error(tsn(audit_values, decay = Inf), "`decay`")
  expect_error(tsn(audit_values, limit = c(1, 2)), "`limit`")
})

test_that("audit: coded states order nodes by value, characters by appearance", {
  values <- c(3, 1, 4, 2, 5, 3)
  integer_codes <- tsn(values, unit = "state", state = c(2L, 1L, 2L, 10L, 1L, 2L))
  expect_identical(integer_codes$nodes$label, c("1", "2", "10"))
  logical_codes <- tsn(values, unit = "state",
                       state = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  expect_identical(logical_codes$nodes$label, c("FALSE", "TRUE"))
  dates <- as.Date("2020-01-03") - c(0, 1, 0, 1, 2, 2)
  date_codes <- tsn(values, unit = "state", state = dates)
  expect_identical(date_codes$nodes$label,
                   c("2020-01-01", "2020-01-02", "2020-01-03"))
  characters <- tsn(values, unit = "state",
                    state = c("b", "a", "b", "c", "a", "b"))
  expect_identical(characters$nodes$label, c("b", "a", "c"))
  # Integer codes now order the same way as the internal discretizer.
  codes <- as.integer(as.data.frame(
    tsn(audit_values, "quantile"), what = "series"
  )$state)
  expect_identical(
    as.matrix(tsn(audit_values, unit = "state", state = codes)),
    as.matrix(tsn(audit_values, "quantile"))
  )
})

test_that("audit: malformed state vectors are rejected informatively", {
  values <- c(3, 1, 4, 2, 5, 3)
  expect_error(tsn(values, unit = "state", state = as.list(seq_len(6L))),
               "atomic vector")
  expect_error(tsn(values, unit = "state", state = matrix(seq_len(6L), 3L)),
               "atomic vector")
  expect_error(
    tsn(values, unit = "state", state = c("a", "b", "", "a", "b", "a")),
    "non-empty"
  )
  expect_error(tsn(values, unit = "state", state = c("a", "b", "c")),
               "one non-missing")
})

test_that("audit: a row-aligned state vector follows long-data series selection", {
  long <- data.frame(
    id = rep(c("a", "b"), each = 4L), t = rep(4:1, 2L),
    v = c(1, 3, 2, 4, 2, 1, 3, 2),
    st = c("x", "y", "x", "z", "y", "x", "y", "x")
  )
  from_vector <- tsn(long, value = "v", id = "id", time = "t", series = "a",
                     unit = "state", state = long$st)
  from_column <- tsn(long, value = "v", id = "id", time = "t", series = "a",
                     unit = "state", state = "st")
  expect_identical(as.data.frame(from_vector), as.data.frame(from_column))
  expect_identical(as.data.frame(from_vector, what = "series"),
                   as.data.frame(from_column, what = "series"))
  expect_identical(names(as.data.frame(from_vector, what = "series")),
                   c("id", "time", "value", "state"))
  everything <- tsn(long, value = "v", id = "id", time = "t", unit = "state",
                    state = long$st)
  expect_identical(
    as.data.frame(everything),
    as.data.frame(tsn(long, value = "v", id = "id", time = "t",
                      unit = "state", state = "st"))
  )
})

test_that("audit: every normalize mode stays finite, bounded, and defined", {
  rescale <- tsn:::.tsn_normalize_distances
  modes <- c("max", "minmax", "quantile")
  constant <- lapply(modes, function(mode) rescale(c(3, 3, 3), mode))
  expect_identical(constant, list(c(1, 1, 1), c(0, 0, 0), c(0, 0, 0)))
  zeros <- lapply(modes, function(mode) rescale(c(0, 0), mode))
  expect_identical(zeros, rep(list(c(0, 0)), 3L))
  single <- vapply(modes, function(mode) rescale(5, mode), numeric(1L))
  expect_identical(unname(single), c(1, 0, 0))
  spread <- c(1, 2, 3, 10)
  expect_equal(rescale(spread, "max"), spread / 10)
  expect_equal(rescale(spread, "minmax"), (spread - 1) / 9)
  bounds <- stats::quantile(spread, c(0.05, 0.95), names = FALSE)
  expect_equal(rescale(spread, "quantile"),
               pmin(1, pmax(0, (spread - bounds[1]) / diff(bounds))))
  expect_identical(rescale(numeric(), "max"), numeric())
  expect_identical(rescale(spread, NULL), spread)
  expect_error(rescale(c(1, NA), "max"), "finite")
  expect_error(rescale(c(1, NaN), "minmax"), "finite")
  expect_error(rescale(c(1, -1), "quantile"), "non-negative")
  # Documented identity: normalized_inverse weights are 1 - normalized distance.
  fit <- tsn(audit_series, "distance", normalize = "max",
             similarity = "normalized_inverse")
  expect_equal(as.data.frame(fit)$weight, 1 - as.data.frame(fit)$distance)
})

test_that("audit: default window follows max(2, n %/% 10)", {
  default_window <- tsn:::.tsn_default_window
  source_of <- function(n) {
    data.frame(id = "s", time = seq_len(n), value = seq_len(n))
  }
  widths <- vapply(c(2, 3, 25, 30, 100, 101),
                   function(n) as.integer(default_window(source_of(n))),
                   integer(1L))
  expect_identical(widths, c(2L, 2L, 2L, 3L, 10L, 10L))
  expect_error(tsn(c(1, 2), "distance"), "at least two")
  expect_identical(nrow(as.data.frame(tsn(c(1, 2, 3), "distance"))), 1L)
})

test_that("audit: every family returns a unique, finite, documented dyad table", {
  fits <- list(
    tsn(audit_series, "distance"),
    tsn(audit_series, "distance", directed = TRUE, chain = TRUE),
    tsn(audit_series, "distance", unit = "time"),
    tsn(audit_series, "distance", unit = "window", window = 3L),
    tsn(audit_values),
    tsn(audit_values, "hvg", directed = TRUE),
    tsn(audit_values, "quantile"),
    tsn(audit_values, "quantile", directed = TRUE)
  )
  invisible(lapply(fits, function(fit) {
    table <- as.data.frame(fit)
    expect_identical(names(table), c(
      "from", "to", "distance", "weight", "connected", "method", "unit",
      "distance_method", "connection_method", "directed", "from_start",
      "from_end", "to_start", "to_end"
    ))
    directed <- isTRUE(fit$parameters$directed)
    key <- if (directed) paste(table$from, table$to, sep = "\r") else
      paste(pmin(table$from, table$to), pmax(table$from, table$to), sep = "\r")
    expect_identical(anyDuplicated(key), 0L)
    expect_true(all(is.finite(table$weight)) && all(table$weight >= 0))
    expect_true(all(c(table$from, table$to) %in% fit$nodes$label))
    if (!identical(fit$parameters$unit, "state")) {
      expect_identical(sum(table$from == table$to), 0L)
    }
    expect_identical(names(fit), c(
      "nodes", "edges", "directed", "weights", "data", "meta", "node_groups",
      "table", "source", "parameters", "n_nodes", "n_edges"
    ))
    expect_identical(fit$n_nodes, nrow(fit$nodes))
    expect_identical(fit$n_edges, sum(table$connected))
  }))
  # Construction is deterministic.
  expect_identical(as.data.frame(tsn(audit_values, "kmeans", seed = 3L)),
                   as.data.frame(tsn(audit_values, "kmeans", seed = 3L)))
})
