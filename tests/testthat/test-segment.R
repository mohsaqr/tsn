data("srl", package = "tsn", envir = environment())

seg_one <- subset(subset(srl, !is.na(effort)), name == "Erik")
seg_labels <- c("low", "moderate", "high")

seg_build <- function(data, ...) {
  ts_tna(
    data,
    value = "effort", time = "day",
    discretization = "threshold", breaks = c(40, 70),
    labels = seg_labels,
    ...
  )
}

test_that("segment cuts one series into the expected number of sequences", {
  skip_if_not_installed("Nestimate")
  n <- nrow(seg_one)

  expect_identical(nrow(seg_build(seg_one)$data), 1L)
  # Partitioning n observations into blocks of 10 gives ceiling(n / 10)
  # sequences, minus a trailing block of one observation if there is one.
  expect_identical(nrow(seg_build(seg_one, segment = 10)$data), 16L)
  # Sliding a width-2 window gives one block per transition.
  expect_identical(
    nrow(seg_build(seg_one, segment = 2, overlap = TRUE)$data),
    as.integer(n - 1L)
  )
})

test_that("sliding lag-1 pairs reproduce the unsegmented estimate exactly", {
  skip_if_not_installed("Nestimate")
  whole <- seg_build(seg_one)
  lag_one <- seg_build(seg_one, segment = 2, overlap = TRUE)

  # Every transition appears exactly once under a sliding width-2 window, so
  # the network must be identical -- this is the guarantee that makes lag-1
  # pairs preferable to non-overlapping duplets.
  expect_equal(lag_one$weights, whole$weights)
})

test_that("partitioning loses the transition at each cut", {
  skip_if_not_installed("Nestimate")
  whole <- seg_build(seg_one)
  blocks <- seg_build(seg_one, segment = 10)
  duplets <- seg_build(seg_one, segment = 2)

  expect_false(isTRUE(all.equal(blocks$weights, whole$weights)))
  expect_false(isTRUE(all.equal(duplets$weights, whole$weights)))

  # Non-overlapping duplets are the worst case: they keep only the transitions
  # at odd positions, so they discard about half of them.
  total <- sum(seg_build(seg_one)$frequency_matrix)
  kept_duplets <- sum(
    ts_ftna(
      seg_one,
      value = "effort", time = "day", segment = 2,
      discretization = "threshold", breaks = c(40, 70),
      labels = seg_labels
    )$weights
  )
  expect_lt(kept_duplets / total, 0.55)
  expect_gt(kept_duplets / total, 0.45)
})

test_that("segmentation runs after discretization, so the alphabet is unchanged", {
  skip_if_not_installed("Nestimate")
  whole <- seg_build(seg_one)
  blocks <- seg_build(seg_one, segment = 10)

  # If cut points were relearned per block, the node set or its order could
  # move; fixing the alphabet up front is what keeps blocks comparable.
  expect_identical(
    as.character(blocks$nodes$label),
    as.character(whole$nodes$label)
  )
  expect_identical(dimnames(blocks$weights), dimnames(whole$weights))
})

test_that("blocks never span an ID boundary", {
  skip_if_not_installed("Nestimate")
  two <- subset(subset(srl, !is.na(effort)), name %in% c("Erik", "Eve"))
  model <- ts_tna(
    two,
    value = "effort", id = "name", time = "day", segment = 50,
    discretization = "threshold", breaks = c(40, 70), labels = seg_labels
  )

  # Every generated sequence is named "<participant>.<block>", so the origin
  # survives segmentation and blocks are provably confined to one participant.
  produced <- summary(series_networks(model))
  origins <- sub("\\..*$", "", as.character(produced$series))
  expect_setequal(origins, c("Erik", "Eve"))
  expect_identical(nrow(produced), nrow(model$data))
})

test_that("segmenting gives sequence-based inference something to resample", {
  skip_if_not_installed("Nestimate")
  blocks <- seg_build(seg_one, segment = 10)

  boot <- suppressWarnings(
    Nestimate::bootstrap_network(blocks, iter = 200, seed = 2024)
  )
  edges <- summary(boot)

  expect_s3_class(edges, "data.frame")
  expect_true(all(edges$ci_lower <= edges$weight))
  expect_true(all(edges$weight <= edges$ci_upper))
  # Intervals must have actual width; a single sequence would collapse them.
  expect_true(all(edges$ci_upper > edges$ci_lower))
})

test_that("the two segmentation schemes trade units against the estimate as documented", {
  skip_if_not_installed("Nestimate")
  partitioned <- seg_build(seg_one, segment = 10)
  sliding <- seg_build(seg_one, segment = 2, overlap = TRUE)

  # Sliding yields one block per transition, an order of magnitude more units
  # than partitioning, and preserves the unsegmented estimate exactly. (On
  # this series the two schemes yield near-identical interval widths, which
  # is exactly what the vignette reports; neither direction is a theorem, so
  # no ordering of the widths is asserted here.)
  expect_identical(nrow(sliding$data), nrow(seg_one) - 1L)
  expect_gt(nrow(sliding$data), 5L * nrow(partitioned$data))
  expect_true(isTRUE(all.equal(
    as.matrix(sliding),
    as.matrix(seg_build(seg_one))
  )))
})

test_that("segment rejects widths that cannot carry a transition", {
  skip_if_not_installed("Nestimate")
  expect_error(seg_build(seg_one, segment = 1))
  expect_error(seg_build(seg_one, segment = 2.5))
  expect_error(seg_build(seg_one, segment = 10, overlap = NA))
})

test_that("ts_tna gains tidy accessors, so no model needs reaching into", {
  skip_if_not_installed("Nestimate")
  model <- seg_build(seg_one)

  # as.matrix() used to fall through to the default and coerce the component
  # list into a 19x1 matrix; it must return the weight matrix itself.
  weights <- as.matrix(model)
  expect_true(is.matrix(weights))
  expect_true(is.numeric(weights))
  expect_identical(dim(weights), c(3L, 3L))
  expect_identical(rownames(weights), seg_labels)

  # Row-stochastic, because ts_tna() builds relative probabilities.
  expect_equal(unname(rowSums(weights)), rep(1, 3))

  edges <- as.data.frame(model)
  expect_s3_class(edges, "data.frame")
  expect_identical(nrow(edges), 9L)
  expect_identical(names(edges), c("from", "to", "weight"))
  expect_equal(sum(edges$weight), 3)

  series <- as.data.frame(model, what = "series")
  expect_identical(nrow(series), nrow(seg_one))
  expect_true(all(c("id", "time", "value", "state") %in% names(series)))
})

test_that("the identity of lag-1 pairs is expressible without reaching in", {
  skip_if_not_installed("Nestimate")
  # This is the exact comparison the vignette prints, and the reason the
  # accessor above had to exist: Rule 0 forbids $weights on the public surface.
  expect_true(
    isTRUE(all.equal(
      as.matrix(seg_build(seg_one, segment = 2, overlap = TRUE)),
      as.matrix(seg_build(seg_one))
    ))
  )
})
