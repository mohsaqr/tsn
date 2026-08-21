data("motivation", package = "tsn", envir = environment())

plotting_pleasure <- utils::head(motivation, 60L)
plotting_short <- utils::head(plotting_pleasure, 25L)

test_that("plotting vignette state and trend objects are stable", {
  states <- discretize(
    plotting_pleasure,
    series = "pleasure",
    labels = c("Low", "Middle", "High")
  )
  directions <- trend(
    plotting_pleasure,
    series = "pleasure"
  )

  expect_s3_class(states, "tsn_states")
  expect_identical(dim(states), c(60L, 5L))
  expect_identical(levels(states$state), c("Low", "Middle", "High"))
  expect_s3_class(directions, "tsn_trend")
  expect_identical(dim(directions), c(60L, 5L))
})

test_that("plotting vignette network objects match the documented counts", {
  visibility <- vg(
    data = plotting_short,
    type = "horizontal",
    series = "pleasure"
  )
  windows <- tsn(
    data = plotting_short,
    method = "distance",
    series = "pleasure",
    step = 2L,
    connect = "nearest",
    neighbors = 2L
  )

  expect_s3_class(visibility, "tsn")
  expect_identical(visibility$n_nodes, 25L)
  expect_identical(visibility$n_edges, 43L)
  expect_s3_class(windows, "tsn")
  expect_identical(windows$n_nodes, 12L)
  expect_identical(windows$n_edges, 18L)
})

test_that("plotting vignette network figures render through cograph", {
  skip_if_not_installed("cograph")
  skip_if_not_installed("igraph")

  visibility <- vg(
    data = plotting_short,
    type = "horizontal",
    series = "pleasure"
  )
  windows <- tsn(
    data = plotting_short,
    method = "distance",
    series = "pleasure",
    step = 2L,
    connect = "nearest",
    neighbors = 2L
  )
  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output, width = 7, height = 4.5)
  on.exit({
    grDevices::dev.off()
    unlink(output)
  }, add = TRUE)

  expect_invisible(plot(windows))
  expect_invisible(plot(visibility, layout = "circle"))
})

test_that("plotting vignette keeps public code free of reach-in idioms", {
  vignette_candidates <- c(
    testthat::test_path(
      "..", "..", "vignettes", "plotting-time-series-networks.Rmd"
    ),
    system.file(
      "doc", "plotting-time-series-networks.Rmd", package = "tsn"
    )
  )
  vignette_candidates <- vignette_candidates[
    nzchar(vignette_candidates) & file.exists(vignette_candidates)
  ]
  skip_if(
    !length(vignette_candidates),
    "Vignette source is unavailable in this installed test context."
  )
  vignette_path <- vignette_candidates[[1L]]
  vignette_lines <- readLines(vignette_path, warn = FALSE)
  starts <- which(grepl("^```\\{r", vignette_lines))
  ends <- vapply(
    starts,
    function(start) {
      following <- seq.int(start + 1L, length(vignette_lines))
      following[which(vignette_lines[following] == "```")[1L]]
    },
    integer(1L)
  )
  visible <- !grepl("include=FALSE", vignette_lines[starts], fixed = TRUE)
  public_code <- unlist(
    Map(
      function(start, end) vignette_lines[seq.int(start + 1L, end - 1L)],
      starts[visible],
      ends[visible]
    ),
    use.names = FALSE
  )

  expect_false(any(grepl("$", public_code, fixed = TRUE)))
  expect_false(any(grepl("[", public_code, fixed = TRUE)))

  vignette <- paste(vignette_lines, collapse = "\n")
  expect_match(vignette, "connect = \"nearest\"", fixed = TRUE)
  expect_match(vignette, "plot(visibility, layout = \"circle\")", fixed = TRUE)
})
