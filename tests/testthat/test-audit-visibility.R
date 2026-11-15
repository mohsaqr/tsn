# Audit of R/visibility.R and R/vg.R against independent brute-force
# references written directly from the published definitions:
# natural visibility (Lacasa et al., 2008, PNAS 105:4972) and horizontal
# visibility (Luque et al., 2009, PRE 80:046103), with the limited-penetrable
# extension (a dyad survives up to `penetrable` blocking intermediates).

# Exact reference on integer data. The natural criterion
#   y_c < y_b + (y_a - y_b) * (t_b - t_c) / (t_b - t_a)
# is cross-multiplied by the positive (t_b - t_a), so every operation on
# integer-valued doubles is exact and ties are decided without rounding.
audit_reference_pairs <- function(values, times = seq_along(values),
                                  method = "natural", limit = NULL,
                                  penetrable = 0L) {
  n <- length(values)
  if (n < 2L) {
    return(character())
  }
  pairs <- utils::combn(n, 2L)
  keep <- vapply(seq_len(ncol(pairs)), function(column) {
    a <- pairs[1L, column]
    b <- pairs[2L, column]
    if (!is.null(limit) && times[b] - times[a] > limit) {
      return(FALSE)
    }
    if (b - a == 1L) {
      return(TRUE)
    }
    middle <- seq.int(a + 1L, b - 1L)
    sees <- if (identical(method, "natural")) {
      values[middle] * (times[b] - times[a]) <
        values[b] * (times[b] - times[a]) +
          (values[a] - values[b]) * (times[b] - times[middle])
    } else {
      values[middle] < min(values[a], values[b])
    }
    sum(!sees) <= penetrable
  }, logical(1L))
  paste(pairs[1L, keep], pairs[2L, keep])
}

audit_engine_pairs <- function(values, times = seq_along(values), ...) {
  dyads <- tsn:::.tsn_visibility(values, times = times, ...)
  paste(dyads$from, dyads$to)
}

audit_fixtures <- function(seed, count) {
  set.seed(seed)
  lapply(seq_len(count), function(index) {
    n <- sample(c(1L, 2L, 3L, sample(4:30, 1L)), 1L)
    values <- switch(
      index %% 6L + 1L,
      sample(-20:20, n, replace = TRUE),           # many ties
      rep(4, n),                                   # one plateau
      2 * seq_len(n),                              # monotone, collinear
      rev(seq_len(n)) - 5,                         # monotone negative
      replace(numeric(n), ceiling(n / 2), 50),     # single spike
      sample(c(-3, -3, 0, 1, 1, 1), n, replace = TRUE)
    )
    times <- if (index %% 2L == 0L) {
      as.numeric(seq_len(n))
    } else {
      cumsum(sample(1:4, n, replace = TRUE))       # irregular elapsed time
    }
    list(values = as.numeric(values), times = as.numeric(times))
  })
}

test_that("the engine matches exact brute force on integer series", {
  fixtures <- audit_fixtures(seed = 101, count = 240)
  configurations <- expand.grid(
    method = c("natural", "horizontal"),
    penetrable = c(0L, 1L, 3L),
    limit = c(NA, 1, 4),
    stringsAsFactors = FALSE
  )
  mismatches <- vapply(seq_len(nrow(configurations)), function(row) {
    method <- configurations$method[row]
    penetrable <- configurations$penetrable[row]
    limit <- if (is.na(configurations$limit[row])) NULL else
      configurations$limit[row]
    sum(!vapply(fixtures, function(fixture) {
      identical(
        audit_engine_pairs(fixture$values, fixture$times, method = method,
                           limit = limit, penetrable = penetrable),
        audit_reference_pairs(fixture$values, fixture$times, method = method,
                              limit = limit, penetrable = penetrable)
      )
    }, logical(1L)))
  }, integer(1L))
  expect_identical(mismatches, integer(nrow(configurations)))
})

test_that("collinear decimal points block exactly as their integer copies", {
  # Regression: slopes on rounded decimals put exactly collinear points one
  # ulp on either side of the boundary, so c(1.1, 1.2, 1.3) gained a 1-3 edge
  # that c(11, 12, 13) correctly lacks.
  expect_identical(audit_engine_pairs(c(1.1, 1.2, 1.3)), c("1 2", "2 3"))
  expect_identical(
    audit_engine_pairs(c(1.1, 1.2, 1.3), times = c(0.1, 0.2, 0.3)),
    c("1 2", "2 3")
  )
  expect_identical(
    audit_engine_pairs(c(0.3, -0.6, 0.7, 0.9), method = "natural"),
    audit_engine_pairs(c(3, -6, 7, 9), method = "natural")
  )

  # Affine invariance: dividing values and times by a constant that makes
  # them inexact, or shifting values, must not change the graph.
  fixtures <- audit_fixtures(seed = 202, count = 200)
  divisors <- c(10, 100, 7, 3)
  offsets <- c(0, 1000, -50)
  configurations <- expand.grid(
    penetrable = c(0L, 2L), limited = c(FALSE, TRUE)
  )
  invariant <- vapply(seq_along(fixtures), function(index) {
    fixture <- fixtures[[index]]
    value_divisor <- divisors[index %% 4L + 1L]
    time_divisor <- divisors[(index + 1L) %% 4L + 1L]
    offset <- offsets[index %% 3L + 1L]
    all(vapply(seq_len(nrow(configurations)), function(row) {
      penetrable <- configurations$penetrable[row]
      limited <- configurations$limited[row]
      identical(
        audit_engine_pairs(
          offset + fixture$values / value_divisor,
          fixture$times / time_divisor,
          method = "natural", penetrable = penetrable,
          limit = if (limited) 4 / time_divisor else NULL
        ),
        audit_reference_pairs(
          fixture$values, fixture$times, method = "natural",
          penetrable = penetrable, limit = if (limited) 4 else NULL
        )
      )
    }, logical(1L)))
  }, logical(1L))
  expect_true(all(invariant))
})

test_that("limit is inclusive on decimal elapsed time", {
  # Regression: 0.8 - 0.7 is 0.10000000000000009 in binary, so a lag equal
  # to `limit = 0.1` was dropped while the same lag at times 7, 8 was kept.
  expect_identical(
    audit_engine_pairs(c(1, 2, 1), times = c(0.7, 0.8, 0.9), limit = 0.1),
    c("1 2", "2 3")
  )
  expect_identical(
    audit_engine_pairs(c(1, 2, 1), times = c(0.7, 0.8, 0.9), limit = 0.1,
                       method = "horizontal"),
    c("1 2", "2 3")
  )
  expect_identical(
    audit_engine_pairs(c(1, 0, 1, 0, 1), limit = 2),
    c("1 2", "1 3", "2 3", "3 4", "3 5", "4 5")
  )
  # A lag beyond the limit stays excluded.
  expect_identical(
    audit_engine_pairs(c(1, 0, 1), times = c(0, 1, 2.0001), limit = 2),
    c("1 2", "2 3")
  )
})

test_that("rounding tolerance leaves continuous and epoch-time data unchanged", {
  # The tolerance is a few ulps; on data without exact ties the natural graph
  # must equal the plain floating-point slope comparison.
  plain_natural <- function(values, times) {
    n <- length(values)
    visible <- lapply(seq_len(n - 1L), function(left) {
      right <- seq.int(left + 1L, n)
      heights <- (values[right] - values[left]) / (times[right] - times[left])
      right[heights > cummax(c(-Inf, heights[-length(heights)]))]
    })
    paste(rep.int(seq_len(n - 1L), lengths(visible)), unlist(visible))
  }
  set.seed(303)
  epoch <- as.numeric(as.POSIXct("2024-01-01", tz = "UTC"))
  unchanged <- vapply(seq_len(60L), function(index) {
    n <- sample(2:250, 1L)
    values <- stats::rnorm(n, mean = 50, sd = 10)
    times <- if (index %% 2L == 0L) {
      epoch + cumsum(sample(1:3600, n, replace = TRUE))
    } else {
      cumsum(stats::runif(n, 0.1, 3))
    }
    identical(audit_engine_pairs(values, times), plain_natural(values, times))
  }, logical(1L))
  expect_true(all(unchanged))
})

test_that("penetrable = 0 is the standard graph and penetrability is monotone", {
  fixtures <- audit_fixtures(seed = 404, count = 80)
  checks <- vapply(fixtures, function(fixture) {
    all(vapply(c("natural", "horizontal"), function(method) {
      base <- audit_engine_pairs(fixture$values, fixture$times, method = method)
      zero <- audit_engine_pairs(fixture$values, fixture$times, method = method,
                                 penetrable = 0L)
      one <- audit_engine_pairs(fixture$values, fixture$times, method = method,
                                penetrable = 1L)
      two <- audit_engine_pairs(fixture$values, fixture$times, method = method,
                                penetrable = 2L)
      identical(base, zero) && all(zero %in% one) && all(one %in% two)
    }, logical(1L)))
  }, logical(1L))
  expect_true(all(checks))
  # Every dyad is visible once the allowance covers all intermediates.
  values <- c(5, 1, 9, 2, 8, 3)
  expect_length(
    audit_engine_pairs(values, penetrable = length(values)),
    choose(length(values), 2L)
  )
})

test_that("horizontal visibility is invariant to the time axis", {
  values <- c(3, 1, 4, 1, 5, 9, 2, 6)
  expect_identical(
    audit_engine_pairs(values, method = "horizontal"),
    audit_engine_pairs(values, times = c(0, 0.5, 4, 4.1, 9, 20, 21, 50),
                       method = "horizontal")
  )
  # whereas the natural rule uses elapsed time, not index positions
  expect_identical(audit_engine_pairs(c(0, 1, 3), times = c(1, 2, 3)),
                   c("1 2", "1 3", "2 3"))
  expect_identical(audit_engine_pairs(c(0, 1, 3), times = c(1, 1.2, 10)),
                   c("1 2", "2 3"))
})

test_that("distance and decay weight use the elapsed time of each dyad", {
  times <- c(0, 0.5, 2, 2.25, 7)
  values <- c(2, 1, 3, 0.5, 4)
  dyads <- tsn:::.tsn_visibility(values, times = times, decay = 0.4)
  from <- as.integer(dyads$from)
  to <- as.integer(dyads$to)
  expect_equal(dyads$distance, times[to] - times[from])
  expect_equal(dyads$weight, exp(-0.4 * (times[to] - times[from])))
  expect_true(all(dyads$weight > 0 & dyads$weight <= 1))
  expect_true(all(from < to))
})

test_that("tiny series and constant series give the expected graphs", {
  for_one <- tsn:::.tsn_visibility(5)
  expect_identical(nrow(for_one), 0L)
  expect_identical(
    names(for_one), c("from", "to", "distance", "weight", "connected")
  )
  expect_identical(nrow(tsn:::.tsn_visibility(5, method = "horizontal")), 0L)
  expect_identical(audit_engine_pairs(c(1, 2)), "1 2")
  expect_identical(audit_engine_pairs(c(2, 2), method = "horizontal"), "1 2")
  # A plateau blocks: only neighbours see each other under both rules.
  expect_identical(audit_engine_pairs(rep(1, 4)), c("1 2", "2 3", "3 4"))
  expect_identical(audit_engine_pairs(rep(1, 4), method = "horizontal"),
                   c("1 2", "2 3", "3 4"))
})

test_that("state aggregation counts, canonicalises, and keeps self-loops", {
  values <- c(1, 3, 2, 5, 1, 4)
  states <- c("a", "b", "a", "b", "a", "b")
  labels <- as.character(seq_along(values))
  dyads <- tsn:::.tsn_visibility(values)
  from_state <- states[as.integer(dyads$from)]
  to_state <- states[as.integer(dyads$to)]

  undirected <- tsn:::.tsn_aggregate_visibility(
    dyads, states, labels, aggregation = "count"
  )
  expect_identical(undirected$from, c("a", "b"))
  expect_identical(undirected$to, c("b", "b"))
  expect_identical(sum(undirected$weight), as.numeric(nrow(dyads)))
  expect_identical(
    undirected$weight,
    as.numeric(c(sum(from_state != to_state),
                 sum(from_state == "b" & to_state == "b")))
  )

  directed <- tsn:::.tsn_aggregate_visibility(
    dyads, states, labels, aggregation = "count", directed = TRUE
  )
  expect_identical(paste(directed$from, directed$to), c("a b", "b a", "b b"))
  expect_identical(
    directed$weight,
    as.numeric(c(sum(from_state == "a" & to_state == "b"),
                 sum(from_state == "b" & to_state == "a"),
                 sum(from_state == "b" & to_state == "b")))
  )

  # Factor states orient the canonical undirected pair by level order.
  reordered <- tsn:::.tsn_aggregate_visibility(
    dyads, factor(states, levels = c("b", "a")), labels, aggregation = "sum"
  )
  expect_identical(paste(reordered$from, reordered$to), c("b a", "b b"))

  expect_error(
    tsn:::.tsn_aggregate_visibility(dyads, states, labels,
                                    aggregation = "median"),
    "should be one of"
  )
})

test_that("vg() records its own call and matches tsn()", {
  values <- c(3, 1, 4, 2, 5, 3, 6, 2, 7)
  network <- vg(values, "horizontal", directed = TRUE)
  expect_identical(network$parameters$call[[1L]], as.name("vg"))
  expect_identical(
    as.data.frame(network),
    as.data.frame(tsn(values, "hvg", directed = TRUE))
  )
  # Directed visibility edges run forward in time.
  table <- as.data.frame(network)
  expect_true(all(table$directed))
  expect_true(all(table$from_start < table$to_start))
})

test_that("vg() rejects arguments that its own type already fixes", {
  values <- c(3, 1, 4, 2, 5)
  expect_error(vg(values, visibility = "horizontal"), "type")
  expect_error(vg(values, method = "distance"), "visibility")
  expect_error(vg(values, "sideways"), "should be one of")
})

test_that("penetrable visibility stays fast on long series", {
  skip_on_cran()
  # Regression: the running top-(penetrable + 1) set was re-sorted for every
  # candidate, so penetrable = 2 at n = 2000 took about 45 s against 0.1 s for
  # penetrable = 0. Skipping the sort when the set is unchanged gives < 1 s.
  set.seed(7)
  values <- stats::rnorm(2000)
  elapsed <- system.time(
    tsn:::.tsn_visibility(values, method = "natural", penetrable = 2L)
  )[["elapsed"]]
  expect_lt(elapsed, 10)
})

test_that("penetrable and state validation give named, early errors", {
  # Regression: penetrable = 1e10 overflowed as.integer() with a coercion
  # warning and the deparsed message "penetrable == as.integer(penetrable)".
  values <- c(1, 3, 2, 4, 0)
  expect_no_warning(
    huge <- tsn:::.tsn_visibility(values, penetrable = 1e10)
  )
  expect_identical(nrow(huge), as.integer(choose(length(values), 2L)))
  expect_identical(
    tsn:::.tsn_visibility(values, penetrable = 1e10),
    tsn:::.tsn_visibility(values, penetrable = length(values) - 2L)
  )
  expect_error(tsn:::.tsn_visibility(values, penetrable = 1.5),
               "non-negative whole number")
  expect_error(tsn:::.tsn_visibility(values, penetrable = -1),
               "non-negative whole number")
  expect_error(tsn:::.tsn_visibility(values, penetrable = TRUE),
               "non-negative whole number")
  expect_error(tsn:::.tsn_visibility(values, penetrable = c(1, 2)),
               "non-negative whole number")
  expect_error(
    tsn:::.tsn_aggregate_visibility(
      tsn:::.tsn_visibility(c(1, 2)), c("a", ""), c("1", "2")
    ),
    "must not contain empty strings"
  )
})
