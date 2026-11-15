# Audit of R/discretize.R: discretize(), its print/summary/plot methods, and
# the grouped helpers. Each block pins a contract that was probed directly.

audit_long <- function() {
  values <- c(
    0.2, 1.4, -0.7, 2.1, 0.9, -1.3, 0.4, 1.8, -0.2, 0.6,
    1.1, -0.9, 0.3, 2.4, -1.6, 0.8, 1.5, -0.4, 0.1, 1.9,
    5.2, 6.1, 4.4, 7.0, 5.8, 3.9, 6.6, 5.1, 4.8, 6.9,
    5.5, 4.1, 6.3, 7.4, 3.6, 5.9, 6.4, 4.6, 5.3, 6.8
  )
  data.frame(
    id = rep(c("a", "b"), each = 20L),
    t = rep(seq_len(20L), 2L),
    v = values,
    stringsAsFactors = FALSE
  )
}

audit_plot_device <- function(code) {
  path <- tempfile(fileext = ".png")
  grDevices::png(path, width = 900, height = 700)
  on.exit({
    grDevices::dev.off()
    unlink(path)
  }, add = TRUE)
  before <- graphics::par(no.readonly = TRUE)
  force(code)
  after <- graphics::par(no.readonly = TRUE)
  identical(before, after)
}

test_that("m and tau are rejected outside the ordinal discretizer, also on grouped data", {
  long <- audit_long()
  # The grouped temporal path used to drop them silently.
  expect_error(
    discretize(long, value = "v", id = "id", time = "t", method = "dtw", m = 5),
    "`m` and `tau`"
  )
  expect_error(
    discretize(long, value = "v", id = "id", time = "t",
               method = "adaptive_magnitude", tau = 2),
    "`m` and `tau`"
  )
  expect_error(discretize(long$v, method = "quantile", m = 3), "`m` and `tau`")
  # The shared path used by tsn() rejects them too.
  expect_error(
    tsn(long, value = "v", id = "id", time = "t", unit = "state",
        discretization = "dtw", m = 5),
    "`m` and `tau`"
  )
  expect_error(
    tsn:::.tsn_discretize_values(long$v, method = "adaptive_magnitude",
                                 n_states = 3L, breaks = NULL, m = 3L,
                                 groups = long$id),
    "`m` and `tau`"
  )
  ordinal <- discretize(long, value = "v", id = "id", time = "t",
                        method = "ordinal", m = 3, tau = 1)
  expect_s3_class(ordinal, "tsn_states")
})

test_that("threshold requires breaks and reports the n_states mismatch by name", {
  values <- c(1, 2, 3, 8, 9)
  expect_error(discretize(values, method = "threshold"), "requires `breaks`")
  expect_error(
    discretize(values, method = "threshold", n_states = 3,
               breaks = c(3.5, 5, 7)),
    "set `n_states = 4`"
  )
  expect_error(
    discretize(values, method = "threshold", n_states = 2, breaks = c(3, 5)),
    "`n_states - 1` = 1"
  )
  three <- discretize(values, method = "threshold", breaks = c(2.5, 5))
  expect_identical(as.integer(three$state), c(1L, 1L, 2L, 3L, 3L))
})

test_that("threshold drops empty bins, so labels follow the realized count", {
  values <- c(1, 2, 3, 8, 9)
  # The engine announces the shortfall with a classed warning.
  expect_warning(
    dropped <- discretize(values, method = "threshold", n_states = 4,
                          breaks = c(3.5, 5, 7)),
    class = "tsn_state_shortfall"
  )
  expect_identical(levels(dropped$state), c("1", "2"))
  expect_identical(attr(dropped, "parameters")$n_states, 2L)
  expect_warning(
    expect_error(
      discretize(values, method = "threshold", n_states = 4,
                 breaks = c(3.5, 5, 7), labels = c("a", "b", "c", "d")),
      "exactly 2"
    ),
    class = "tsn_state_shortfall"
  )
  expect_warning(
    labelled <- discretize(values, method = "threshold", n_states = 4,
                           breaks = c(3.5, 5, 7), labels = c("low", "high")),
    class = "tsn_state_shortfall"
  )
  expect_identical(as.character(labelled$state),
                   c("low", "low", "low", "high", "high"))
})

test_that("labels must be unique, non-missing, non-empty strings", {
  values <- c(1, 5, 2, 9, 3, 7, 4, 8)
  # NA used to yield NA states silently.
  expect_error(discretize(values, labels = c("a", NA, "b")), "non-missing")
  expect_error(discretize(values, labels = c("a", "", "b")), "non-empty")
  expect_error(discretize(values, labels = c("a", "a", "b")), "unique")
  labelled <- discretize(values, labels = factor(c("lo", "mid", "hi"),
                                                levels = c("hi", "mid", "lo")))
  # Labels are taken in vector order, mapped to increasing state order.
  expect_identical(levels(labelled$state), c("lo", "mid", "hi"))
  expect_false(anyNA(labelled$state))
})

test_that("value, id, and time cannot be combined with a tsn input", {
  long <- audit_long()
  network <- tsn(long, value = "v", id = "id", time = "t")
  expect_error(discretize(network, value = "v"), "`value`")
  expect_error(discretize(network, id = "id", time = "t"), "`id`, `time`")
  from_network <- discretize(network, series = "b")
  expect_identical(unique(from_network$id), "b")
  expect_identical(nrow(from_network), 20L)
})

test_that("the tsn_states contract: columns, classes, and time keys per input form", {
  long <- audit_long()
  with_time <- discretize(long, value = "v", id = "id", time = "t")
  expect_s3_class(with_time, c("tsn_states", "data.frame"), exact = TRUE)
  expect_identical(names(with_time),
                   c("id", "time", "value", "state", "probability"))
  expect_identical(nrow(with_time), nrow(long))
  expect_type(with_time$id, "character")
  expect_s3_class(with_time$state, "factor")
  expect_true(all(with_time$probability >= 0 & with_time$probability <= 1))

  shifted <- long
  shifted$t <- shifted$t + 100L
  raw_time <- discretize(shifted, value = "v", id = "id", time = "t")
  expect_identical(raw_time$time, shifted$t)

  id_only <- discretize(long, value = "v", id = "id")
  expect_identical(id_only$time, rep(seq_len(20L), 2L))

  wide <- data.frame(x = long$v[1:10], y = long$v[21:30])
  from_wide <- discretize(wide)
  expect_identical(from_wide$time, rep(seq_len(10L), 2L))
  expect_identical(unique(from_wide$id), c("x", "y"))

  dated <- long
  dated$t <- as.Date("2024-01-01") + rep(0:19, 2L)
  expect_s3_class(discretize(dated, value = "v", id = "id", time = "t")$time,
                  "Date")
})

test_that("rows follow series first appearance and time order within series", {
  input <- data.frame(id = c("b", "a", "b", "a", "b", "a"),
                      t = c(2, 2, 1, 1, 3, 3), v = c(1, 2, 3, 4, 5, 6))
  states <- discretize(input, value = "v", id = "id", time = "t", n_states = 2)
  expect_identical(states$id, rep(c("b", "a"), each = 3L))
  expect_identical(states$time, c(1, 2, 3, 1, 2, 3))
  expect_identical(states$value, c(3, 1, 5, 4, 2, 6))
})

test_that("missing and non-finite values are an error, never an NA state", {
  long <- audit_long()
  long$v[3] <- NA
  expect_error(discretize(long, value = "v", id = "id", time = "t"), "finite")
  expect_error(discretize(c(1, Inf, 3, 4, 5, 6)), "finite")
  expect_error(discretize(c(1, NaN, 3, 4, 5, 6)), "finite")
})

test_that("pooled scalar discretization shares one alphabet across series", {
  long <- audit_long()
  pooled <- discretize(long, value = "v", id = "id", time = "t")
  # Series b sits entirely above series a, so pooled cut points put b in
  # the top state far more often than a: the alphabet is shared.
  share_top <- tapply(pooled$state == "3", pooled$id, mean)
  expect_gt(share_top[["b"]], share_top[["a"]])
  alone <- discretize(long$v)
  expect_identical(as.integer(pooled$state), as.integer(alone$state))
})

test_that("seed is validated, deterministic, and leaves the global RNG untouched", {
  values <- c(0.1, 1.7, -0.4, 2.2, 0.8, -1.1, 3.0, 2.6, -0.9, 1.2,
              0.5, -1.8, 2.9, 1.0, -0.2, 0.3, 2.4, -1.4, 1.5, 0.0)
  invisible(lapply(c("kmeans", "gaussian"), function(method) {
    set.seed(42)
    before <- .Random.seed
    first <- discretize(values, method = method, seed = 3)
    expect_identical(.Random.seed, before)
    second <- discretize(values, method = method, seed = 3)
    expect_identical(first$state, second$state)
  }))
  expect_error(discretize(values, method = "kmeans", seed = -1), "`seed`")
  expect_error(discretize(values, method = "kmeans", seed = 1.5), "`seed`")
  expect_error(discretize(values, method = "dtw", seed = 1), "`seed`")
})

test_that("n_states is validated before discretizing", {
  values <- seq_len(10L) + 0.5
  expect_error(discretize(values, n_states = 1), "`n_states`")
  expect_error(discretize(values, n_states = 2.5), "`n_states`")
  expect_error(discretize(values, n_states = NA), "`n_states`")
  expect_error(discretize(values, n_states = c(2, 3)), "`n_states`")
  expect_error(discretize(values, n_states = "3"), "`n_states`")
  expect_error(discretize(values, n_states = 11), "unique value")
})

test_that("realized states are contiguous and match the factor levels for every method", {
  methods <- c("threshold", "width", "quantile", "kde", "kmeans", "gaussian",
               "hclust", "ordinal", "symbolic", "change_points", "entropy",
               "magnitude", "adaptive_magnitude", "percentile_magnitude",
               "dtw")
  long <- audit_long()
  invisible(lapply(methods, function(method) {
    arguments <- list(long, value = "v", id = "id", time = "t",
                      method = method)
    if (identical(method, "threshold")) {
      arguments$breaks <- c(0.5, 4.5)
    }
    states <- do.call(discretize, arguments)
    codes <- as.integer(states$state)
    expect_identical(sort(unique(codes)), seq_len(nlevels(states$state)),
                     info = method)
    expect_identical(attr(states, "parameters")$n_states,
                     nlevels(states$state), info = method)
    expect_identical(nrow(states), nrow(long), info = method)
  }))
})

test_that("summary is a tidy one-row-per-state table and print has no `$` hint", {
  states <- discretize(c(1, 5, 2, 9, 3, 7, 4, 8),
                       labels = c("low", "mid", "high"))
  table <- summary(states)
  expect_identical(class(table), "data.frame")
  expect_identical(names(table), c("state", "count", "proportion", "mean_value"))
  expect_identical(table$state, c("low", "mid", "high"))
  expect_identical(sum(table$count), nrow(states))
  expect_equal(sum(table$proportion), 1)
  expect_equal(table$mean_value, c(2, 4.5, 8))
  printed <- capture.output(print(states))
  expect_match(printed[1], "quantile discretization .* 8 observations, 3 states")
  expect_false(any(grepl("$", printed, fixed = TRUE)))
  capture.output(expect_invisible(print(states)))
})

test_that("z-score guide lines use the fitted scale even after rows are dropped", {
  long <- audit_long()
  states <- discretize(long, value = "v", id = "id", time = "t",
                       transform = "zscore")
  parameters <- attr(states, "parameters")
  expect_equal(parameters$center, mean(long$v))
  expect_equal(parameters$scale, stats::sd(long$v))
  full <- tsn:::.tsn_plot_breaks(states)
  # head() keeps the attributes but not the full sample; the guides used to be
  # recomputed from the remaining rows and land in the wrong place.
  expect_equal(tsn:::.tsn_plot_breaks(utils::head(states, 20L)), full)
  cuts <- attr(states, "breaks")
  expect_equal(full, mean(long$v) + cuts[is.finite(cuts)] * stats::sd(long$v))
  plain <- discretize(long, value = "v", id = "id", time = "t")
  expect_null(attr(plain, "parameters")$center)
})

test_that("plot rejects arguments it would otherwise silently ignore", {
  states <- discretize(audit_long(), value = "v", id = "id", time = "t")
  # Names that do not partially match a formal argument reach `...`.
  expect_error(plot(states, colour = "red"), "`colour`")
  expect_error(plot(states, pallete = "okabe", foo = 1), "`pallete`, `foo`")
})

test_that("plot restores graphics state across views, sizes, and time types", {
  long <- audit_long()
  one <- discretize(long$v)
  many <- discretize(
    data.frame(id = rep(sprintf("s%02d", seq_len(30L)), each = 8L),
               t = rep(seq_len(8L), 30L),
               v = rep(c(0.1, 2.3, -1.2, 0.7, 3.1, -0.4, 1.6, 2.8), 30L) +
                 rep(seq_len(30L) / 10, each = 8L)),
    value = "v", id = "id", time = "t"
  )
  nine <- discretize(seq(-4, 4, length.out = 90) +
                       sin(seq_len(90L)), n_states = 9)
  single <- discretize(seq_len(12L) + 0.5, method = "ordinal")
  expect_identical(nlevels(single$state), 1L)
  dated_input <- long
  dated_input$t <- as.Date("2024-01-01") + rep(0:19, 2L)
  dated <- discretize(dated_input, value = "v", id = "id", time = "t")
  objects <- list(one = one, many = many, nine = nine, single = single,
                  dated = dated)
  invisible(lapply(names(objects), function(name) {
    object <- objects[[name]]
    invisible(lapply(c("overlay", "ribbon", "heatmap"), function(view) {
      expect_true(
        expect_no_warning(audit_plot_device(plot(object, type = view,
                                                 max_series = 30))),
        info = paste(name, view)
      )
    }))
    expect_true(
      expect_no_warning(audit_plot_device(
        plot(object, type = "stack", with = object$value + 1)
      )),
      info = paste(name, "stack")
    )
  }))
  expect_true(audit_plot_device(plot(nine, lines = "both")))
  expect_error(plot(nine, palette = c("#E69F00", "#56B4E9")),
               "2 colour\\(s\\) but 9 are needed")
})

test_that("grouped ordinal patterns are ordered by numeric rank, as in the engine", {
  values <- c(
    0.31, -1.20, 0.85, 2.10, -0.44, 1.37, -0.92, 0.05, 1.66, -1.71,
    0.48, 0.97, -0.13, 1.12, -2.05, 0.72, -0.58, 1.89, 0.26, -0.81,
    1.43, -0.37, 0.64, -1.49, 2.33, 0.11, -0.69, 1.02, -0.25, 0.59,
    -1.06, 1.74, 0.38, -0.47, 0.90, -1.88, 1.25, 0.02, -0.75, 1.58
  )
  long <- data.frame(id = rep(c("a", "b"), each = 20L), v = values)
  # m = 10 gives two-digit ranks, where a string sort puts "1-10-..." first.
  grouped <- discretize(long, value = "v", id = "id", method = "ordinal",
                        m = 10)
  patterns <- attr(grouped, "model")$patterns
  ranks <- lapply(strsplit(patterns, "-", fixed = TRUE), as.integer)
  columns <- lapply(seq_len(10L), function(position) {
    vapply(ranks, function(rank) rank[position], integer(1L))
  })
  expect_identical(do.call(order, columns), seq_along(patterns))
  expect_true(is.na(attr(grouped, "model")$n_states_requested))
  # A single series through the engine uses the same rule.
  single <- discretize(values[1:20], method = "ordinal", m = 10)
  single_patterns <- attr(single, "model")$patterns
  expect_identical(single_patterns,
                   patterns[patterns %in% single_patterns])
})

test_that("grouped temporal paths surface a state shortfall like the engine", {
  set.seed(4)
  long <- data.frame(id = c(rep("a", 10L), rep("b", 4L)),
                     v = round(stats::rnorm(14L), 2))
  expect_warning(
    short <- discretize(long, value = "v", id = "id", method = "dtw"),
    class = "tsn_state_shortfall"
  )
  model <- attr(short, "model")
  expect_identical(model$n_states_requested, 3L)
  expect_lt(model$n_states, 3L)
  expect_identical(nlevels(short$state), model$n_states)
  header <- capture.output(print(short))[1]
  expect_match(header, sprintf("%d states \\(3 requested\\)", model$n_states))
  full <- discretize(audit_long(), value = "v", id = "id", method = "dtw")
  expect_identical(attr(full, "model")$n_states_requested, 3L)
  expect_false(grepl("requested", capture.output(print(full))[1]))
  adaptive <- discretize(audit_long(), value = "v", id = "id",
                         method = "adaptive_magnitude")
  expect_identical(attr(adaptive, "model")$n_states_requested, 3L)
})

test_that("threshold breaks define n_states when n_states is not supplied", {
  one_break <- discretize(c(1, 1, 9, 9), method = "threshold", breaks = 5)
  expect_identical(as.integer(one_break$state), c(1L, 1L, 2L, 2L))
  expect_identical(attr(one_break, "parameters")$n_states, 2L)
  four <- discretize(c(1, 4, 6, 8, 10), method = "threshold",
                     breaks = c(3, 5, 7))
  expect_identical(as.integer(four$state), c(1L, 2L, 3L, 4L, 4L))
  # The ts_* builders forward n_states only when supplied.
  network <- ts_ftna(
    data.frame(id = "a", t = 1:4, v = c(1, 1, 9, 9)),
    value = "v", id = "id", time = "t",
    discretization = "threshold", breaks = 5
  )
  expect_s3_class(network, "ts_tna")
})
