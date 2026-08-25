# Pins the central claims of vignette("group-models"). If a data or code
# change moves these numbers, the vignette prose must be recomputed and
# rewritten, not just re-rendered.

gt_labels <- c("low", "mid", "high")

gt_grouped <- function(data) {
  ts_tna(
    data,
    series = "pleasure",
    group = "task_context_type",
    labels = gt_labels
  )
}

test_that("the grouped context model has the documented shape and backing data", {
  skip_if_not_installed("Nestimate")
  data(motivation, envir = environment())

  full <- gt_grouped(motivation)
  full_index <- as.data.frame(full, what = "groups")
  expect_identical(as.character(full_index$group),
                   c("Home", "Other", "Personal", "Work"))
  other <- subset(full_index, group == "Other")
  expect_identical(other$sequences, 2L)
  expect_identical(other$observations, 3L)
  expect_identical(other$edges, 1L)

  contexts <- subset(motivation, task_context_type != "Other")
  by_context <- gt_grouped(contexts)
  index <- as.data.frame(by_context, what = "groups")
  expect_identical(index$sequences, c(832L, 822L, 976L))
  expect_identical(index$observations, c(1324L, 1309L, 2235L))
  expect_true(all(index$states == 3L))
})

test_that("pooled discretization and the hand-subset model disagree as documented", {
  skip_if_not_installed("Nestimate")
  data(motivation, envir = environment())

  home_alone <- ts_tna(
    subset(motivation, task_context_type == "Home"),
    series = "pleasure",
    labels = gt_labels
  )
  # The subset welds all home measurements into one sequence.
  hand_built <- summary(series_networks(home_alone))
  expect_identical(nrow(hand_built), 1L)
  expect_identical(hand_built$observations, 1324L)

  # Its own tertiles put roughly a third of measurements in each state...
  own_scale <- Nestimate::state_distribution(home_alone)
  expect_equal(subset(own_scale, state == "low")$proportion,
               0.3731118, tolerance = 1e-6)

  # ...while the pooled scale puts home at 62.5% low and 6.6% high.
  by_context <- gt_grouped(subset(motivation, task_context_type != "Other"))
  shared_scale <- Nestimate::state_distribution(by_context)
  home_low <- subset(shared_scale, group == "Home" & state == "low")
  home_high <- subset(shared_scale, group == "Home" & state == "high")
  expect_equal(home_low$proportion, 0.6246224, tolerance = 1e-6)
  expect_equal(home_high$proportion, 0.06646526, tolerance = 1e-6)
})

test_that("the low-to-high edge and entropy ordering match the vignette", {
  skip_if_not_installed("Nestimate")
  data(motivation, envir = environment())
  by_context <- gt_grouped(subset(motivation, task_context_type != "Other"))

  jump <- subset(as.data.frame(by_context), from == "low" & to == "high")
  expect_equal(jump$weight, c(0.04024768, 0.35064935, 0.29051988),
               tolerance = 1e-6)

  entropy <- Nestimate::transition_entropy(by_context)
  rates <- vapply(entropy, `[[`, numeric(1L), "entropy_rate")
  expect_lt(rates[["Home"]], rates[["Personal"]])
  expect_lt(rates[["Personal"]], rates[["Work"]])
})

test_that("the seeded permutation and comparison results match the vignette", {
  skip_if_not_installed("Nestimate")
  skip_on_cran()
  data(motivation, envir = environment())
  by_context <- gt_grouped(subset(motivation, task_context_type != "Other"))

  comparison <- Nestimate::permutation(by_context, iter = 1000, seed = 2026)
  edge_table <- summary(comparison)
  significant <- aggregate(sig ~ group, data = edge_table, FUN = sum)
  expect_identical(
    subset(significant, group == "Home vs Personal")$sig, 8L
  )
  expect_identical(
    subset(significant, group == "Home vs Work")$sig, 8L
  )
  expect_identical(
    subset(significant, group == "Personal vs Work")$sig, 3L
  )

  # The Home and Personal weight matrices are negatively correlated.
  home_personal <- stats::cor(
    as.vector(as.matrix(by_context[["Home"]])),
    as.vector(as.matrix(by_context[["Personal"]]))
  )
  expect_lt(home_personal, -0.5)
})
