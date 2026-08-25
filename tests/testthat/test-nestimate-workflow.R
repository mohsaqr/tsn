data("motivation", package = "tsn", envir = environment())
data("steps", package = "tsn", envir = environment())

nw_pleasure <- utils::head(motivation, 200L)
nw_labels <- c("low", "mid", "high")

nw_walkers <- function() subset(steps, !is.na(steps))

nw_cutoff <- as.Date("2019-11-01")

# The vignette fixes the cut points so every model shares one alphabet.
nw_walk_model <- function(data = nw_walkers()) {
  ts_tna(
    data,
    value = "steps", id = "id", time = "day",
    discretization = "threshold", breaks = c(5000, 10000),
    labels = c("sedentary", "moderate", "active")
  )
}

test_that("ts_tna returns a Nestimate netobject, which is what the bridge relies on", {
  skip_if_not_installed("Nestimate")
  model <- ts_tna(nw_pleasure, series = "pleasure", labels = nw_labels)

  # The vignette's central claim: no conversion step is needed because the
  # object already inherits the class Nestimate's verbs dispatch on.
  expect_s3_class(model, "ts_tna")
  expect_s3_class(model, "netobject")
  expect_identical(model$n_nodes, 3L)
  expect_identical(as.character(model$nodes$label), nw_labels)
})

test_that("the four constructors share an interface and differ only in edge meaning", {
  skip_if_not_installed("Nestimate")
  args <- list(nw_pleasure, series = "pleasure", labels = nw_labels)
  models <- lapply(
    c("ts_tna", "ts_ftna", "ts_atna", "ts_cna"),
    function(verb) do.call(verb, args)
  )

  expect_true(all(vapply(models, inherits, logical(1L), "netobject")))
  expect_identical(
    vapply(models, function(m) m$method, character(1L)),
    c("relative", "frequency", "attention", "co_occurrence")
  )
  # ts_cna is undirected, so it carries fewer edges than the directed three.
  expect_identical(
    vapply(models, function(m) m$n_edges, integer(1L)),
    c(9L, 9L, 9L, 6L)
  )
})

test_that("descriptive Nestimate verbs accept a single-series model", {
  skip_if_not_installed("Nestimate")
  model <- ts_tna(nw_pleasure, series = "pleasure", labels = nw_labels)

  centrality <- Nestimate::net_centrality(model)
  expect_s3_class(centrality, "data.frame")
  expect_identical(nrow(centrality), 3L)

  distribution <- Nestimate::state_distribution(model)
  expect_s3_class(distribution, "data.frame")
  # Quantile discretization forces near-equal states BY CONSTRUCTION, which is
  # exactly why the vignette switches to explicit breaks for the step data.
  expect_true(all(abs(distribution$proportion - 1 / 3) < 0.05))

  pruned <- Nestimate::net_prune(model, method = "threshold", threshold = 0.3)
  expect_s3_class(pruned, "ts_tna")
  expect_s3_class(pruned, "netobject")
})

test_that("the pooled step model carries one sequence per participant", {
  skip_if_not_installed("Nestimate")
  walkers <- nw_walkers()
  expect_false(anyNA(walkers$steps))

  model <- nw_walk_model(walkers)
  expect_identical(nrow(model$data), 151L)

  per_person <- summary(series_networks(model))
  expect_s3_class(per_person, "data.frame")
  expect_identical(nrow(per_person), 151L)
  # Split models share the pooled alphabet, so node sets stay aligned.
  expect_true(all(per_person$states == 3L))
})

test_that("explicit breaks change the model, and are not silently ignored", {
  skip_if_not_installed("Nestimate")
  # `breaks` is only consumed by discretization = "threshold"; with any other
  # discretizer it is rejected outright rather than silently ignored.
  expect_error(
    ts_tna(
      nw_walkers(),
      value = "steps", id = "id", time = "day",
      breaks = c(5000, 10000), labels = nw_labels
    ),
    "breaks"
  )
  # And when consumed, it must actually move the model: if the wiring broke,
  # these two models would coincide.
  quantile_model <- ts_tna(
    nw_walkers(),
    value = "steps", id = "id", time = "day", labels = nw_labels
  )
  threshold_model <- nw_walk_model()

  quantile_share <- Nestimate::state_distribution(quantile_model)
  threshold_share <- Nestimate::state_distribution(threshold_model)

  expect_true(all(abs(quantile_share$proportion - 1 / 3) < 0.05))
  expect_true(any(abs(threshold_share$proportion - 1 / 3) > 0.15))
})

test_that("bootstrap retains every edge when 151 sequences back the estimate", {
  skip_if_not_installed("Nestimate")
  model <- nw_walk_model()

  boot <- suppressWarnings(
    Nestimate::bootstrap_network(model, iter = 200, seed = 2024)
  )
  edges <- summary(boot)

  expect_s3_class(edges, "data.frame")
  expect_identical(nrow(edges), 9L)
  expect_true(all(edges$sig))
  expect_true(all(edges$ci_lower <= edges$weight))
  expect_true(all(edges$weight <= edges$ci_upper))
})

test_that("the Markov order test prefers an order above one", {
  skip_if_not_installed("Nestimate")
  model <- nw_walk_model()

  order_test <- suppressWarnings(Nestimate::markov_order_test(model))
  # The vignette states the first-order network is a simplification; if this
  # ever selects order 1 that claim needs rewriting.
  expect_gt(order_test$bic_order, 1L)
  expect_gt(order_test$aic_order, 1L)
  expect_identical(order_test$n_sequences, 151L)
})

test_that("summer and winter differ in composition but not in dynamics", {
  skip_if_not_installed("Nestimate")
  walkers <- nw_walkers()
  early <- nw_walk_model(subset(walkers, as.Date(day) < nw_cutoff))
  late <- nw_walk_model(subset(walkers, as.Date(day) >= nw_cutoff))

  comparison <- summary(
    suppressWarnings(Nestimate::permutation(early, late, iter = 2000, seed = 2024))
  )

  # The vignette's claim is a NULL result on the transitions: no edge differs.
  expect_identical(nrow(comparison), 9L)
  expect_false(any(comparison$sig))

  # ... while the composition does shift. If this ever stopped holding, the
  # "marginal moves, dynamics do not" contrast would have no basis.
  early_share <- Nestimate::state_distribution(early)
  late_share <- Nestimate::state_distribution(late)
  active_early <- subset(early_share, state == "active")
  active_late <- subset(late_share, state == "active")
  expect_gt(active_early$proportion, active_late$proportion)
})

test_that("a 0.3 threshold discards every route down into sedentary", {
  skip_if_not_installed("Nestimate")
  pruned <- Nestimate::net_prune(
    nw_walk_model(), method = "threshold", threshold = 0.3
  )
  weights <- as.matrix(pruned$weights)

  # The vignette reads the pruned model as "sedentary is harder to fall into
  # than to climb out of", which depends on exactly these edges going to zero.
  expect_identical(unname(weights["moderate", "sedentary"]), 0)
  expect_identical(unname(weights["active", "sedentary"]), 0)
  expect_identical(unname(weights["sedentary", "active"]), 0)
  expect_gt(unname(weights["sedentary", "moderate"]), 0.4)
})
