# Pins the central claims of vignette("nestimate-workflow"). If a data or code
# change moves these numbers, the vignette prose must be recomputed and
# rewritten, not just re-rendered.

data("esm_srl", package = "tsn", envir = environment())
data("srl", package = "tsn", envir = environment())

nw_jamal <- subset(esm_srl, name == "Jamal")
nw_labels <- c("low", "mid", "high")
nw_students <- function() subset(srl, !is.na(effort))

# The vignette fixes the cut points so every model shares one alphabet.
nw_effort_model <- function(data = nw_students()) {
  ts_tna(
    data,
    value = "effort", id = "name", time = "day",
    discretization = "threshold", breaks = c(40, 70),
    labels = c("low", "moderate", "high")
  )
}

# Edge lookups for the bootstrap claims below. Brackets stay inside these
# helpers so the expectations read as `from -> to` the way the prose does.
nw_edge_ids <- function(edges) paste0(edges$from, "->", edges$to)

nw_edge <- function(edges, from, to) {
  hit <- edges[edges$from == from & edges$to == to, , drop = FALSE]
  stopifnot("edge not present in the bootstrap table" = nrow(hit) == 1L)
  hit
}

test_that("ts_tna returns a Nestimate netobject, which is what the bridge relies on", {
  skip_if_not_installed("Nestimate")
  model <- ts_tna(nw_jamal, series = "anxiety", labels = nw_labels)

  expect_s3_class(model, "ts_tna")
  expect_s3_class(model, "netobject")
  expect_identical(model$n_nodes, 3L)
  expect_identical(as.character(model$nodes$label), nw_labels)
  # The reading the vignette gives the intro matrix: after a high-anxiety
  # report the most likely next state is low.
  expect_equal(unname(as.matrix(model)["high", "low"]), 0.4230769,
               tolerance = 1e-6)
})

test_that("the four constructors share an interface and differ only in edge meaning", {
  skip_if_not_installed("Nestimate")
  args <- list(nw_jamal, series = "anxiety", labels = nw_labels)
  models <- lapply(
    c("ts_tna", "ts_ftna", "ts_atna", "ts_cna"),
    function(verb) do.call(verb, args)
  )

  expect_true(all(vapply(models, inherits, logical(1L), "netobject")))
  expect_identical(
    vapply(models, function(m) m$method, character(1L)),
    c("relative", "frequency", "attention", "co_occurrence")
  )
  expect_identical(
    vapply(models, function(m) m$n_edges, integer(1L)),
    c(9L, 9L, 9L, 6L)
  )
})

test_that("explicit breaks change the model, and are not silently ignored", {
  skip_if_not_installed("Nestimate")
  # breaks is only consumed by discretization = "threshold"; with any other
  # discretizer it is rejected outright rather than silently ignored.
  expect_error(
    ts_tna(
      nw_students(),
      value = "effort", id = "name", time = "day",
      breaks = c(40, 70), labels = c("low", "moderate", "high")
    ),
    "breaks"
  )
  # And when consumed, it must actually move the model.
  quantile_model <- ts_tna(
    nw_students(),
    value = "effort", id = "name", time = "day",
    labels = c("low", "moderate", "high")
  )
  threshold_model <- nw_effort_model()
  expect_false(isTRUE(all.equal(
    as.matrix(quantile_model), as.matrix(threshold_model)
  )))
})

test_that("the pooled model carries one sequence per student and the documented structure", {
  skip_if_not_installed("Nestimate")
  model <- nw_effort_model()

  expect_identical(nrow(model$data), 36L)
  per_person <- summary(series_networks(model))
  expect_identical(nrow(per_person), 36L)
  expect_true(all(per_person$states == 3L))

  distribution <- Nestimate::state_distribution(model)
  expect_equal(subset(distribution, state == "high")$proportion,
               0.4174394, tolerance = 1e-6)
  # The bootstrap section explains `low -> low` by `low` being the rarest
  # state, filling 24.4% of the days.
  expect_equal(subset(distribution, state == "low")$proportion,
               0.2437589, tolerance = 1e-6)
  weights <- as.matrix(model)
  expect_equal(unname(weights["high", "high"]), 0.5748709, tolerance = 1e-6)
  expect_identical(max(weights), unname(weights["high", "high"]))
  # "a low-effort day returns to `low` at only 0.415"
  expect_equal(unname(weights["low", "low"]), 0.4148311, tolerance = 1e-6)
  expect_lt(unname(weights["low", "low"]), unname(weights["high", "high"]))
})

test_that("fixed thresholds recover more structure than tertiles, as the prose reports", {
  skip_if_not_installed("Nestimate")
  fixed <- Nestimate::transition_entropy(nw_effort_model())
  tertiles <- Nestimate::transition_entropy(ts_tna(
    nw_students(),
    value = "effort", id = "name", time = "day",
    labels = c("low", "moderate", "high")
  ))

  # The vignette prose reports normalised rates of 0.93 and 0.96.
  expect_equal(fixed$entropy_rate_norm, 0.933, tolerance = 1e-3)
  expect_equal(tertiles$entropy_rate_norm, 0.960, tolerance = 1e-3)
  expect_lt(fixed$entropy_rate, tertiles$entropy_rate)
})

test_that("a 0.3 threshold discards every descending route, as the prose reads it", {
  skip_if_not_installed("Nestimate")
  pruned <- Nestimate::net_prune(
    nw_effort_model(), method = "threshold", threshold = 0.3
  )
  weights <- as.matrix(pruned$weights)

  expect_identical(unname(weights["moderate", "low"]), 0)
  expect_identical(unname(weights["high", "low"]), 0)
  expect_identical(unname(weights["high", "moderate"]), 0)
  expect_identical(unname(weights["low", "high"]), 0)
  expect_gt(unname(weights["low", "moderate"]), 0.3)
  expect_gt(unname(weights["moderate", "high"]), 0.3)
})

test_that("the seeded inference chunks match the vignette", {
  skip_if_not_installed("Nestimate")
  skip_on_cran()
  model <- nw_effort_model()

  boot <- summary(Nestimate::bootstrap_network(model, iter = 200, seed = 2026))
  expect_identical(sum(boot$sig), 5L)

  # The prose names which five survive and which four do not, so pin the
  # identities, not only the count: the two ascending steps, the `moderate`
  # and `high` loops, and the short descent `high -> moderate`.
  stable <- nw_edge_ids(subset(boot, sig))
  expect_setequal(
    stable,
    c("low->moderate", "moderate->moderate", "moderate->high",
      "high->moderate", "high->high")
  )
  expect_setequal(
    nw_edge_ids(subset(boot, !sig)),
    c("low->low", "low->high", "moderate->low", "high->low")
  )

  # The vignettes call `high -> moderate` the edge pruning dropped but the
  # bootstrap keeps (0.260), and `low -> low` the large loop that just misses
  # the criterion (p = 0.060). Both are stated to three decimals.
  expect_equal(nw_edge(boot, "high", "moderate")$weight, 0.2603270,
               tolerance = 1e-6)
  expect_identical(round(nw_edge(boot, "high", "moderate")$weight, 3), 0.260)
  expect_equal(nw_edge(boot, "low", "low")$weight, 0.4148311, tolerance = 1e-6)
  expect_identical(round(nw_edge(boot, "low", "low")$weight, 3), 0.415)
  expect_equal(nw_edge(boot, "low", "low")$p_value, 0.05970149, tolerance = 1e-6)
  expect_identical(round(nw_edge(boot, "low", "low")$p_value, 3), 0.060)

  set.seed(2026)
  stability <- Nestimate::centrality_stability(model, iter = 100)
  expect_output(print(stability), "InStrength       0.60")
  expect_output(print(stability), "OutStrength      0.40")

  set.seed(2026)
  order_test <- Nestimate::markov_order_test(model)
  expect_output(print(order_test), "BIC: 2   AIC: 3   permutation-LRT: 3")
})

test_that("the two course halves differ in neither composition nor dynamics", {
  skip_if_not_installed("Nestimate")
  skip_on_cran()
  students <- nw_students()
  first_half <- nw_effort_model(subset(students, day <= 78))
  second_half <- nw_effort_model(subset(students, day > 78))

  comparison <- summary(
    Nestimate::permutation(first_half, second_half, iter = 2000, seed = 2026)
  )
  expect_identical(nrow(comparison), 9L)
  expect_false(any(comparison$sig))
  expect_equal(min(comparison$p_value), 0.4937531, tolerance = 1e-6)

  high_first <- subset(Nestimate::state_distribution(first_half),
                       state == "high")
  high_second <- subset(Nestimate::state_distribution(second_half),
                        state == "high")
  expect_lt(abs(high_first$proportion - high_second$proportion), 0.02)
})

test_that("the grouped day-type model matches the vignette", {
  skip_if_not_installed("Nestimate")
  skip_on_cran()
  by_day <- ts_tna(
    subset(esm_srl, !is.na(effort)),
    value = "effort", id = "name", time = "occasion",
    group = "day_type", labels = nw_labels
  )
  index <- as.data.frame(by_day, what = "groups")
  expect_identical(index$sequences, c(238L, 233L))
  expect_identical(index$observations, c(2033L, 783L))

  comparison <- summary(Nestimate::permutation(by_day, iter = 1000,
                                               seed = 2026))
  softening <- subset(comparison, from == "high" & to == "mid")
  expect_true(softening$sig)
  expect_equal(softening$weight_x, 0.2538071, tolerance = 1e-6)
  expect_equal(softening$weight_y, 0.3419689, tolerance = 1e-6)
})
