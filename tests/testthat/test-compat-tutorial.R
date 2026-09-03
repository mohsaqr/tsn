# Pins the central claims of vignette("nestimate-compatibility"). If a data
# or code change moves these numbers, the vignette prose must be recomputed
# and rewritten, not just re-rendered.

ct_model <- function() {
  data(srl, envir = environment())
  students <- subset(srl, !is.na(effort))
  ts_tna(
    students,
    value = "effort", id = "name", time = "day",
    discretization = "threshold", breaks = c(40, 70),
    labels = c("low", "moderate", "high")
  )
}

test_that("the model validates as a netobject and the accessors disagree only on loops", {
  skip_if_not_installed("Nestimate")
  effort_model <- ct_model()

  expect_true(Nestimate::validate_netobject(effort_model))

  edges <- as.data.frame(effort_model)
  between <- Nestimate::extract_edges(effort_model)
  # extract_edges excludes self-transitions; the tsn accessor keeps them.
  expect_identical(nrow(edges), 9L)
  expect_identical(nrow(between), 6L)
  expect_false(any(between$from == between$to))

  initial <- Nestimate::extract_initial_probs(effort_model)
  expect_equal(sum(initial), 1)
  expect_equal(initial[["high"]], 0.4166667, tolerance = 1e-6)
})

test_that("dynamics summaries match the vignette", {
  skip_if_not_installed("Nestimate")
  effort_model <- ct_model()

  spells <- Nestimate::markov_stability(effort_model)$stability
  high <- subset(spells, state == "high")
  low <- subset(spells, state == "low")
  expect_equal(high$sojourn_time, 2.35, tolerance = 1e-2)
  expect_equal(low$return_time, 4.10, tolerance = 1e-2)

  expect_output(print(Nestimate::chain_structure(effort_model)),
                "irreducible: TRUE")
  expect_output(print(Nestimate::chain_structure(effort_model)),
                "reversible: FALSE")

  dependence <- Nestimate::path_dependence(effort_model)
  expect_output(print(dependence), "KL_weighted      = 0.038")

  paths <- Nestimate::path_counts(effort_model)
  top <- subset(paths, path == "high -> high")
  expect_equal(top$proportion, 0.2398, tolerance = 1e-4)

  # Every between-state edge carries the same unit betweenness load.
  betweenness <- Nestimate::net_edge_betweenness(effort_model)$weights
  off_diagonal <- betweenness[row(betweenness) != col(betweenness)]
  expect_true(all(off_diagonal == 1))
})

test_that("the reliability verbs return the vignette's moderate verdicts", {
  skip_if_not_installed("Nestimate")
  skip_on_cran()
  effort_model <- ct_model()

  set.seed(2026)
  certain <- Nestimate::certainty(effort_model)
  expect_true(all(summary(certain)$sig))

  boot <- summary(Nestimate::bootstrap_network(effort_model, iter = 200,
                                               seed = 2026))
  expect_identical(sum(boot$sig), 5L)

  # The prose names the five survivors and the four failures, and reports the
  # `low -> low` near miss at p = 0.060.
  edge_id <- function(edges) paste0(edges$from, "->", edges$to)
  expect_setequal(
    edge_id(subset(boot, sig)),
    c("low->moderate", "moderate->moderate", "moderate->high",
      "high->moderate", "high->high")
  )
  expect_setequal(
    edge_id(subset(boot, !sig)),
    c("low->low", "low->high", "moderate->low", "high->low")
  )
  low_loop <- subset(boot, from == "low" & to == "low")
  expect_equal(low_loop$p_value, 0.05970149, tolerance = 1e-6)
  expect_identical(round(low_loop$p_value, 3), 0.060)

  # The trade the prose describes: pruning keeps `low -> low` and drops
  # `high -> moderate`; the bootstrap does the reverse.
  pruned <- as.matrix(Nestimate::net_prune(effort_model, method = "threshold",
                                           threshold = 0.3)$weights)
  expect_gt(unname(pruned["low", "low"]), 0)
  expect_identical(unname(pruned["high", "moderate"]), 0)

  dropped <- Nestimate::casedrop_reliability(effort_model, iter = 100,
                                             seed = 2026)
  expect_output(print(dropped), "CS-coefficient \\(r\\)    : 0.50")

  set.seed(2026)
  halves <- Nestimate::network_reliability(effort_model)
  expect_output(print(halves), "Pearson             mean = 0.85")
})

test_that("the course halves agree under every comparison verb", {
  skip_if_not_installed("Nestimate")
  skip_on_cran()
  data(srl, envir = environment())
  students <- subset(srl, !is.na(effort))
  build <- function(rows) {
    ts_tna(
      rows,
      value = "effort", id = "name", time = "day",
      discretization = "threshold", breaks = c(40, 70),
      labels = c("low", "moderate", "high")
    )
  }
  first_half <- build(subset(students, day <= 78))
  second_half <- build(subset(students, day > 78))

  difference <- Nestimate::subtract_networks(first_half, second_half)
  expect_lt(max(abs(difference$difference_matrix)), 0.04)

  correlation <- stats::cor(
    as.vector(as.matrix(first_half)),
    as.vector(as.matrix(second_half))
  )
  expect_gt(correlation, 0.99)

  set.seed(2026)
  credible <- Nestimate::bayes_compare(first_half, second_half)
  expect_output(print(credible), "Credibly different: 0")

  vertex <- Nestimate::vertex_compare(first_half, second_half, iter = 200,
                                      seed = 2026)
  expect_output(print(vertex), "Snijders & Borgatti")
})

test_that("the pooled inference chunks match the vignette", {
  skip_if_not_installed("Nestimate")
  skip_on_cran()
  effort_model <- ct_model()

  centrality <- Nestimate::net_centrality(effort_model)
  hub <- subset(as.data.frame(centrality), InStrength == max(InStrength))
  expect_identical(as.character(hub$state), "high")
  expect_equal(hub$InStrength, 0.6003974, tolerance = 1e-6)

  set.seed(2026)
  entropy <- Nestimate::entropy_bayes(effort_model)
  expect_output(print(entropy), "1.478")

  set.seed(2026)
  order_test <- Nestimate::markov_order_test(effort_model)
  expect_output(print(order_test), "BIC: 2   AIC: 3   permutation-LRT: 3")

  expect_identical(Nestimate::pathways(
    Nestimate::build_hon(effort_model, max_order = 2)
  ), character(0))
})
