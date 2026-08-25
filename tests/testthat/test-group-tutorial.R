# Pins the central claims of vignette("group-models"). If a data or code
# change moves these numbers, the vignette prose must be recomputed and
# rewritten, not just re-rendered.

data("esm_srl", package = "tsn", envir = environment())

gt_labels <- c("low", "mid", "high")
gt_rows <- function() subset(esm_srl, !is.na(effort))

gt_grouped <- function() {
  ts_tna(
    gt_rows(),
    value = "effort",
    id = "name",
    time = "occasion",
    group = "day_type",
    labels = gt_labels
  )
}

test_that("the grouped day-type model has the documented shape and backing data", {
  skip_if_not_installed("Nestimate")
  by_day <- gt_grouped()
  index <- as.data.frame(by_day, what = "groups")

  expect_identical(as.character(index$group), c("weekday", "weekend"))
  expect_identical(index$sequences, c(238L, 233L))
  expect_identical(index$observations, c(2033L, 783L))
  expect_true(all(index$states == 3L))
})

test_that("hand-subsetting welds weekend runs and forces uninformative tertiles", {
  skip_if_not_installed("Nestimate")
  weekend_rows <- subset(gt_rows(), day_type == "weekend")

  weekend_alone <- ts_tna(
    weekend_rows,
    value = "effort", id = "name", time = "occasion",
    labels = gt_labels
  )
  # The subset welds each student's weekends into ONE sequence: 41 sequences
  # over 783 observations count 742 transitions, of which the grouped model's
  # 233 genuine runs contain only 550 -- 192 fabricated.
  expect_identical(nrow(weekend_alone$data), 41L)
  expect_identical(nrow(weekend_alone$ts_source), 783L)

  own_scale <- summary(discretize(
    weekend_rows,
    value = "effort", id = "name", time = "occasion",
    labels = gt_labels
  ))
  # Own-quantile cutting forces exactly equal thirds: 261 apiece.
  expect_identical(own_scale$count, c(261L, 261L, 261L))
})

test_that("the high-to-mid edge and summary panel match the vignette", {
  skip_if_not_installed("Nestimate")
  by_day <- gt_grouped()

  softening <- subset(as.data.frame(by_day), from == "high" & to == "mid")
  expect_equal(softening$weight, c(0.2538071, 0.3419689), tolerance = 1e-6)

  weekday <- as.matrix(by_day[["weekday"]])
  weekend <- as.matrix(by_day[["weekend"]])
  expect_equal(unname(weekday["high", "high"]), 0.631, tolerance = 1e-3)
  expect_equal(unname(weekend["high", "high"]), 0.534, tolerance = 1e-3)
  # Weekday rows are the more even ones (SD of out-strength 0.032 vs 0.102).
  expect_output(print(summary(by_day)), "0.03206")
  expect_output(print(summary(by_day)), "0.1015")
})

test_that("grouped dynamics summaries match the vignette", {
  skip_if_not_installed("Nestimate")
  by_day <- gt_grouped()

  entropy <- Nestimate::transition_entropy(by_day)
  expect_equal(entropy[["weekday"]]$entropy_rate, 1.365, tolerance = 1e-3)
  expect_equal(entropy[["weekend"]]$entropy_rate, 1.385, tolerance = 1e-3)

  centrality <- Nestimate::net_centrality(by_day)
  expect_equal(centrality[["weekday"]]["mid", "InStrength"], 0.5360257,
               tolerance = 1e-6)
  expect_equal(centrality[["weekend"]]["mid", "InStrength"], 0.6244548,
               tolerance = 1e-6)
})

test_that("the seeded grouped inference matches the vignette", {
  skip_if_not_installed("Nestimate")
  skip_on_cran()
  by_day <- gt_grouped()

  comparison <- summary(Nestimate::permutation(by_day, iter = 1000,
                                               seed = 2026))
  flagged <- subset(comparison, sig)
  expect_true(all(paste(flagged$from, flagged$to) %in%
                    c("high mid", "high high")))
  softening <- subset(comparison, from == "high" & to == "mid")
  expect_true(softening$sig)
  expect_equal(softening$p_value, 0.02297702, tolerance = 1e-6)

  correlation <- stats::cor(
    as.vector(as.matrix(by_day[["weekday"]])),
    as.vector(as.matrix(by_day[["weekend"]]))
  )
  expect_equal(correlation, 0.9657, tolerance = 1e-3)

  dropped <- Nestimate::casedrop_reliability(by_day, iter = 100, seed = 2026)
  expect_output(print(dropped), "weekday     238       6 0.3")
  expect_output(print(dropped), "weekend     233       6 0.5")
})

test_that("plot on a grouped model draws exactly one selected group", {
  skip_if_not_installed("Nestimate")
  skip_if_not_installed("cograph")
  skip_if_not_installed("igraph")
  by_day <- gt_grouped()
  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output)
  on.exit({
    grDevices::dev.off()
    unlink(output)
  }, add = TRUE)

  expect_invisible(plot(by_day, group = "weekday", type = "network"))
  expect_invisible(plot(by_day, group = "weekend", type = "network"))
  expect_error(plot(by_day), "exactly one")
  expect_error(plot(by_day, group = "missing"), "Unknown group")
})
