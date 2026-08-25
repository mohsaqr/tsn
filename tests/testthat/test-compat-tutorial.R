# Pins the central claims of vignette("nestimate-compatibility"). If a data
# or code change moves these numbers, the vignette prose must be recomputed
# and rewritten, not just re-rendered.

ct_model <- function() {
  data(steps, envir = environment())
  walkers <- subset(steps, !is.na(steps))
  ts_tna(
    walkers,
    value = "steps", id = "id", time = "day",
    discretization = "threshold", breaks = c(5000, 10000),
    labels = c("sedentary", "moderate", "active")
  )
}

test_that("the model validates as a netobject and the accessors disagree only on loops", {
  skip_if_not_installed("Nestimate")
  walk_model <- ct_model()

  expect_true(Nestimate::validate_netobject(walk_model))

  edges <- as.data.frame(walk_model)
  between <- Nestimate::extract_edges(walk_model)
  # extract_edges excludes self-transitions; the tsn accessor keeps them.
  expect_identical(nrow(edges), 9L)
  expect_identical(nrow(between), 6L)
  expect_false(any(between$from == between$to))

  initial <- Nestimate::extract_initial_probs(walk_model)
  expect_equal(sum(initial), 1)
  expect_equal(initial[["active"]], 0.5761589, tolerance = 1e-6)
})

test_that("dynamics summaries match the vignette", {
  skip_if_not_installed("Nestimate")
  walk_model <- ct_model()

  stability <- Nestimate::markov_stability(walk_model)$stability
  active <- subset(stability, state == "active")
  sedentary <- subset(stability, state == "sedentary")
  expect_equal(active$sojourn_time, 2.62, tolerance = 1e-2)
  expect_equal(sedentary$return_time, 7.12, tolerance = 1e-2)

  expect_output(print(Nestimate::chain_structure(walk_model)),
                "irreducible: TRUE")
  expect_output(print(Nestimate::chain_structure(walk_model)),
                "reversible: FALSE")

  dependence <- Nestimate::path_dependence(walk_model)
  expect_output(print(dependence), "KL_weighted      = 0.035")

  paths <- Nestimate::path_counts(walk_model)
  top <- subset(paths, path == "active -> active")
  expect_equal(top$proportion, 0.2776, tolerance = 1e-4)
})

test_that("the four reliability verbs agree the model is precisely estimated", {
  skip_if_not_installed("Nestimate")
  skip_on_cran()
  walk_model <- ct_model()

  set.seed(2026)
  certain <- Nestimate::certainty(walk_model)
  expect_true(all(summary(certain)$sig))

  dropped <- Nestimate::casedrop_reliability(walk_model, iter = 100,
                                             seed = 2026)
  expect_output(print(dropped), "CS-coefficient \\(r\\)    : 0.90")

  set.seed(2026)
  halves <- Nestimate::network_reliability(walk_model)
  expect_output(print(halves), "Pearson             mean = 0.99")
})

test_that("the seasonal pair compares as documented across all four verbs", {
  skip_if_not_installed("Nestimate")
  skip_on_cran()
  data(steps, envir = environment())
  walkers <- subset(steps, !is.na(steps))
  cutoff <- as.Date("2019-11-01")
  build <- function(rows) {
    ts_tna(
      rows,
      value = "steps", id = "id", time = "day",
      discretization = "threshold", breaks = c(5000, 10000),
      labels = c("sedentary", "moderate", "active")
    )
  }
  early <- build(subset(walkers, as.Date(day) < cutoff))
  late <- build(subset(walkers, as.Date(day) >= cutoff))

  difference <- Nestimate::subtract_networks(early, late)
  expect_lt(max(abs(difference$difference_matrix)), 0.05)

  correlation <- stats::cor(
    as.vector(as.matrix(early)),
    as.vector(as.matrix(late))
  )
  expect_gt(correlation, 0.99)

  set.seed(2026)
  credible <- Nestimate::bayes_compare(early, late)
  expect_output(print(credible), "Credibly different: 4")

  vertex <- Nestimate::vertex_compare(early, late, iter = 200, seed = 2026)
  expect_output(print(vertex), "Snijders & Borgatti")
})

test_that("the pooled inference chunks match the vignette", {
  skip_if_not_installed("Nestimate")
  skip_on_cran()
  walk_model <- ct_model()

  centrality <- Nestimate::net_centrality(walk_model)
  hub <- subset(as.data.frame(centrality), InStrength == max(InStrength))
  expect_identical(as.character(hub$state), "moderate")
  expect_equal(hub$InStrength, 0.7650336, tolerance = 1e-6)

  boot <- Nestimate::bootstrap_network(walk_model, iter = 200, seed = 2026)
  expect_true(all(summary(boot)$sig))

  set.seed(2026)
  order_test <- Nestimate::markov_order_test(walk_model)
  expect_output(print(order_test), "BIC: 3   AIC: 3   permutation-LRT: 3")

  cutoff <- as.Date("2019-11-01")
  data(steps, envir = environment())
  walkers <- subset(steps, !is.na(steps))
  early <- ts_tna(
    subset(walkers, as.Date(day) < cutoff),
    value = "steps", id = "id", time = "day",
    discretization = "threshold", breaks = c(5000, 10000),
    labels = c("sedentary", "moderate", "active")
  )
  late <- ts_tna(
    subset(walkers, as.Date(day) >= cutoff),
    value = "steps", id = "id", time = "day",
    discretization = "threshold", breaks = c(5000, 10000),
    labels = c("sedentary", "moderate", "active")
  )
  seasons <- summary(Nestimate::permutation(early, late, iter = 1000,
                                            seed = 2026))
  expect_identical(sum(seasons$sig), 0L)
  expect_equal(min(seasons$p_value), 0.07092907, tolerance = 1e-6)
})
