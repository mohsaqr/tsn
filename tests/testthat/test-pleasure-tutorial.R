data("motivation", package = "tsn", envir = environment())

pleasure_tutorial_full <- motivation$pleasure
pleasure_tutorial_values <- pleasure_tutorial_full[seq_len(300L)]

test_that("pleasure tutorial uses one direct unchanged-order series", {
  expect_identical(class(pleasure_tutorial_full), "integer")
  expect_identical(length(pleasure_tutorial_full), 4871L)
  expect_false(anyNA(pleasure_tutorial_full))
  expect_identical(
    pleasure_tutorial_values,
    motivation$pleasure[seq_len(300L)]
  )
  expect_identical(length(pleasure_tutorial_values), 300L)
  expect_identical(utils::head(pleasure_tutorial_values), c(
    32L, 32L, 32L, 26L, 16L, 6L
  ))
})

test_that("pleasure tutorial core pipelines return stable objects", {
  states <- discretize(
    pleasure_tutorial_values,
    method = "quantile",
    n_states = 3L,
    labels = c("Low", "Middle", "High")
  )
  trend_result <- trend(
    pleasure_tutorial_values,
    window = 30L,
    slope = "robust",
    align = "center"
  )
  visibility <- vg(pleasure_tutorial_values, type = "horizontal")
  windows <- tsn(
    pleasure_tutorial_values,
    method = "distance",
    unit = "window",
    window = 30L,
    step = 10L,
    distance = "correlation",
    connect = "nearest",
    neighbors = 2L,
    similarity = "normalized_inverse"
  )

  expect_s3_class(states, "tsn_states")
  expect_identical(dim(states), c(300L, 5L))
  expect_identical(levels(states$state), c("Low", "Middle", "High"))
  expect_identical(as.integer(table(states$state)), c(104L, 104L, 92L))
  expect_s3_class(trend_result, "tsn_trend")
  expect_identical(dim(trend_result), c(300L, 5L))
  expect_true(all(is.finite(trend_result$metric[!is.na(trend_result$metric)])))
  expect_s3_class(visibility, "tsn")
  expect_identical(visibility$n_nodes, 300L)
  expect_identical(visibility$n_edges, 560L)
  expect_s3_class(windows, "tsn")
  expect_identical(windows$n_nodes, 28L)
  expect_identical(windows$n_edges, 36L)
  expect_identical(dim(as.matrix(windows)), c(28L, 28L))
  expect_identical(
    nrow(as.data.frame(windows, what = "series")),
    length(pleasure_tutorial_values)
  )
})

test_that("pleasure tutorial transition pipelines use the same state series", {
  skip_if_not_installed("Nestimate")

  states <- discretize(
    pleasure_tutorial_values,
    method = "quantile",
    n_states = 3L,
    labels = c("Low", "Middle", "High")
  )
  models <- list(
    ftna = ts_ftna(states),
    tna = ts_tna(states),
    cna = ts_cna(states),
    atna = ts_atna(states)
  )
  per_series <- series_networks(models$tna)

  expect_true(all(vapply(
    models,
    inherits,
    logical(1L),
    what = "ts_tna"
  )))
  expect_true(all(vapply(
    models,
    function(model) identical(dim(model$weights), c(3L, 3L)),
    logical(1L)
  )))
  expect_identical(sum(models$ftna$weights), 299L)
  expect_equal(unname(rowSums(models$tna$weights)), rep.int(1, 3L))
  expect_true(isSymmetric(models$cna$weights))
  expect_true(all(is.finite(models$atna$weights)))
  expect_identical(length(per_series), 1L)
  expect_identical(names(per_series), "series_1")
  expect_identical(
    per_series$series_1$ts_source$value,
    as.numeric(pleasure_tutorial_values)
  )
})

test_that("pleasure tutorial combined network plots render through cograph", {
  skip_if_not_installed("Nestimate")
  skip_if_not_installed("cograph")

  states <- discretize(
    pleasure_tutorial_values,
    method = "quantile",
    n_states = 3L,
    labels = c("Low", "Middle", "High")
  )
  models <- list(
    ts_ftna(states),
    ts_tna(states),
    ts_cna(states),
    ts_atna(states)
  )
  per_series <- series_networks(models[[2L]])
  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output, width = 11, height = 5)
  on.exit({
    grDevices::dev.off()
    unlink(output)
  }, add = TRUE)

  invisible(lapply(models, function(model) {
    expect_invisible(plot(
      model,
      type = "combined",
      network = "summary",
      ribbon = TRUE,
      show_weights = FALSE,
      layout = "circle"
    ))
  }))
  expect_invisible(plot(
    per_series$series_1,
    type = "combined",
    network = "summary",
    ribbon = TRUE,
    show_weights = FALSE,
    layout = "circle"
  ))
})

test_that("pleasure tutorial covers every export and plots every transition result", {
  tutorial_candidates <- c(
    testthat::test_path(
      "..", "..", "vignettes", "pleasure-all-functions.Rmd"
    ),
    system.file(
      "doc", "pleasure-all-functions.Rmd", package = "tsn"
    ),
    system.file(
      "doc", "pleasure-all-functions.R", package = "tsn"
    )
  )
  tutorial_candidates <- tutorial_candidates[
    nzchar(tutorial_candidates) & file.exists(tutorial_candidates)
  ]
  skip_if(
    !length(tutorial_candidates),
    "Vignette source is unavailable in this installed test context."
  )
  tutorial_path <- tutorial_candidates[[1L]]
  tutorial <- paste(readLines(tutorial_path, warn = FALSE), collapse = "\n")
  exports <- getNamespaceExports("tsn")
  demonstrated <- vapply(
    exports,
    function(export) {
      grepl(
        sprintf("(^|[^[:alnum:]_.])%s[[:space:]]*\\(", export),
        tutorial,
        perl = TRUE
      )
    },
    logical(1L)
  )
  # plot.ts_tna() already defaults to type = "combined", so the tutorial plots
  # each transition result without naming the type. Check that every transition
  # constructor's result reaches plot() rather than counting a literal argument.
  transition_results <- c("counts", "probabilities", "cooccurrence", "attention")
  plotted <- vapply(
    transition_results,
    function(object) {
      grepl(
        sprintf("plot\\([[:space:]]*%s[,)]", object),
        tutorial,
        perl = TRUE
      )
    },
    logical(1L)
  )

  expect_true(all(demonstrated), info = paste(
    "Missing exported verbs:",
    paste(exports[!demonstrated], collapse = ", ")
  ))
  expect_true(all(plotted), info = paste(
    "Transition results never plotted:",
    paste(transition_results[!plotted], collapse = ", ")
  ))
  expect_false(grepl("task_context_type", tutorial, fixed = TRUE))
  expect_false(grepl("occasion_time", tutorial, fixed = TRUE))
})
