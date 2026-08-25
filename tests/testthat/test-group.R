data("srl", package = "tsn", envir = environment())
data("esm_srl", package = "tsn", envir = environment())

grp_labels <- c("low", "mid", "high")

# A panel where each series sits in one group for its first half and the other
# for its second: contiguous runs, the case grouping is normally used for.
grp_panel <- function() {
  data.frame(
    id = rep(seq_len(12), each = 10),
    t = rep(1:10, times = 12),
    v = rep(c(1, 5, 9), length.out = 120) + rep(c(0.1, -0.1), length.out = 120),
    ph = rep(rep(c("early", "late"), each = 5), times = 12),
    stringsAsFactors = FALSE
  )
}

# A series whose group flips at every observation: the pathological case.
grp_alternating <- function() {
  data.frame(
    id = rep(1:2, each = 6),
    t = rep(1:6, times = 2),
    v = c(1, 5, 1, 5, 1, 5, 9, 1, 9, 1, 9, 1),
    ph = rep(c("x", "y"), times = 6),
    stringsAsFactors = FALSE
  )
}

grp_build <- function(data, verb = ts_tna, ...) {
  verb(
    data,
    value = "v", id = "id", time = "t",
    discretization = "threshold", breaks = c(3, 7), labels = grp_labels,
    ...
  )
}

test_that("group returns a Nestimate netobject_group, one network per group", {
  skip_if_not_installed("Nestimate")
  grouped <- grp_build(grp_panel(), group = "ph")

  expect_s3_class(grouped, "ts_tna_group")
  expect_s3_class(grouped, "netobject_group")
  expect_identical(names(grouped), c("early", "late"))
  expect_identical(attr(grouped, "group_col"), "ph")
  # Each element is a full ts_tna model, not a stripped-down summary.
  expect_true(all(vapply(grouped, inherits, logical(1L), "ts_tna")))
  expect_true(all(vapply(grouped, inherits, logical(1L), "netobject")))
})

test_that("groups share one alphabet, so their networks stay comparable", {
  skip_if_not_installed("Nestimate")
  # "high" is never visited in one group; the shared alphabet must keep it as a
  # node anyway, or the two matrices could not be compared cell by cell.
  lopsided <- data.frame(
    id = rep(1:4, each = 6),
    t = rep(1:6, times = 4),
    # Group "a" only ever sits low or mid; group "b" reaches high. The pooled
    # series therefore spans all three states, which is what fixes the alphabet.
    v = c(rep(c(1, 5), times = 6), rep(c(5, 9), times = 6)),
    arm = rep(c("a", "b"), each = 12),
    stringsAsFactors = FALSE
  )
  grouped <- ts_tna(
    lopsided,
    value = "v", id = "id", time = "t", group = "arm",
    discretization = "threshold", breaks = c(3, 7), labels = grp_labels
  )

  nodes <- lapply(grouped, function(model) rownames(as.matrix(model)))
  expect_identical(nodes[["a"]], grp_labels)
  expect_identical(nodes[["b"]], grp_labels)

  visited <- as.data.frame(grouped, what = "series")
  expect_false("high" %in% as.character(subset(visited, group == "a")$state))
})

test_that("contiguous groups reproduce Nestimate's own grouped build exactly", {
  skip_if_not_installed("Nestimate")
  panel <- grp_panel()
  mine <- grp_build(panel, verb = ts_ftna, group = "ph")

  # Feed Nestimate the states tsn produced, so the only thing under test is the
  # grouping logic rather than the discretizer.
  states <- as.data.frame(
    discretize(
      panel,
      value = "v", id = "id", time = "t",
      method = "threshold", breaks = c(3, 7), labels = grp_labels
    )
  )
  states$ph <- panel$ph
  theirs <- Nestimate::build_network(
    states,
    method = "frequency", actor = "id", action = "state", time = "time",
    group = "ph"
  )

  for (key in c("early", "late")) {
    expect_equal(
      as.matrix(mine[[key]]),
      theirs[[key]]$weights[grp_labels, grp_labels]
    )
  }
})

test_that("a sequence never spans a group boundary, so no transition is invented", {
  skip_if_not_installed("Nestimate")
  alternating <- grp_alternating()
  mine <- suppressWarnings(grp_build(alternating, verb = ts_ftna, group = "ph"))

  states <- as.data.frame(
    discretize(
      alternating,
      value = "v", id = "id", time = "t",
      method = "threshold", breaks = c(3, 7), labels = grp_labels
    )
  )
  states$ph <- alternating$ph
  theirs <- Nestimate::build_network(
    states,
    method = "frequency", actor = "id", action = "state", time = "time",
    group = "ph"
  )

  # Nestimate welds observations 1, 3, 5 into one sequence and counts the
  # transitions between them; those pairs were never adjacent in time.
  expect_gt(sum(theirs[["x"]]$weights), 0)
  expect_equal(sum(as.matrix(mine[["x"]])), 0, tolerance = 0)
  expect_equal(sum(as.matrix(mine[["y"]])), 0, tolerance = 0)

  # A zero matrix is also what you get if the observations were simply thrown
  # away, so pin the data down too: every row survives, as its own sequence.
  expect_identical(
    vapply(mine, function(model) nrow(model$ts_source), integer(1L)),
    c(x = 6L, y = 6L)
  )
  expect_identical(
    vapply(mine, function(model) nrow(model$data), integer(1L)),
    c(x = 6L, y = 6L)
  )
  expect_identical(
    nrow(as.data.frame(mine, what = "series")), nrow(alternating)
  )
})

test_that("an alternating group warns instead of returning a silent empty network", {
  skip_if_not_installed("Nestimate")
  # Both groups alternate, so both must warn -- and each warning must name its
  # own group. Nested expect_warning() would accept two warnings about "x".
  seen <- character()
  withCallingHandlers(
    grp_build(grp_alternating(), group = "ph"),
    warning = function(condition) {
      seen <<- c(seen, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(seen, 2L)
  expect_true(all(grepl("no two observations adjacent in time", seen)))
  expect_setequal(sub("^Group \"([^\"]+)\".*$", "\\1", seen), c("x", "y"))
})

test_that("sequence IDs identify their data across groups", {
  skip_if_not_installed("Nestimate")
  # A group that is constant within each series leaves the IDs untouched.
  cohort <- data.frame(
    id = rep(1:4, each = 8), t = rep(1:8, times = 4),
    v = rep(c(1, 5, 9), length.out = 32),
    arm = rep(c("a", "b"), each = 16), stringsAsFactors = FALSE
  )
  plain <- ts_tna(
    cohort, value = "v", id = "id", time = "t", group = "arm",
    discretization = "threshold", breaks = c(3, 7), labels = grp_labels
  )
  expect_setequal(
    unique(as.data.frame(plain, what = "series")$id),
    c("1", "2", "3", "4")
  )

  # A series that appears in two groups is split into runs, and the run index
  # counts across the whole series -- otherwise segmenting would produce the
  # same "<id>.1" in every group and IDs would collide.
  for (segment in list(NULL, 5)) {
    split_ids <- grp_build(grp_panel(), group = "ph", segment = segment)
    series <- as.data.frame(split_ids, what = "series")
    early <- unique(as.character(subset(series, group == "early")$id))
    late <- unique(as.character(subset(series, group == "late")$id))
    expect_length(intersect(early, late), 0L)
    expect_setequal(sub("[.].*$", "", early), as.character(seq_len(12)))
  }
})

test_that("segmenting inside a group supplies sequences without crossing groups", {
  skip_if_not_installed("Nestimate")
  walkers <- subset(subset(srl, !is.na(effort)),
                    name %in% c("Erik", "Eve", "Frank"))
  walkers$half <- ifelse(walkers$day <= 78, "early", "late")
  grouped <- ts_tna(
    walkers,
    value = "effort", id = "name", time = "day", group = "half", segment = 10,
    discretization = "threshold", breaks = c(40, 70),
    labels = c("low", "moderate", "high")
  )
  index <- as.data.frame(grouped, what = "groups")

  expect_identical(nrow(index), 2L)
  expect_true(all(index$sequences > 20L))
  # Every block traces back to one of the three participants.
  origins <- sub("[.].*$", "", as.character(
    unique(as.data.frame(grouped, what = "series")$id)
  ))
  expect_setequal(origins, c("Erik", "Eve", "Frank"))
  # Grouping alone partitions the series: no row is duplicated or dropped.
  whole <- ts_tna(
    walkers,
    value = "effort", id = "name", time = "day", group = "half",
    discretization = "threshold", breaks = c(40, 70),
    labels = c("low", "moderate", "high")
  )
  expect_identical(
    sum(as.data.frame(whole, what = "groups")$observations), nrow(walkers)
  )

  # Segmenting on top of it drops a trailing block of one observation, which
  # carries no transition -- at most one per (participant, group) run.
  expect_lte(sum(index$observations), nrow(walkers))
  expect_gte(sum(index$observations), nrow(walkers) - 6L)
})

test_that("grouped Nestimate verbs dispatch on the tsn subclass", {
  skip_if_not_installed("Nestimate")
  grouped <- grp_build(grp_panel(), group = "ph")

  distribution <- Nestimate::state_distribution(grouped)
  expect_s3_class(distribution, "data.frame")
  expect_true("group" %in% names(distribution))
  expect_setequal(as.character(distribution$group), c("early", "late"))

  pruned <- Nestimate::net_prune(grouped, method = "threshold", threshold = 0.2)
  expect_s3_class(pruned, "ts_tna_group")
  expect_s3_class(pruned, "netobject_group")

  expect_s3_class(Nestimate::compare_model(grouped), "net_comparison")
})

test_that("as.data.frame gives a tidy view of every group without reaching in", {
  skip_if_not_installed("Nestimate")
  grouped <- grp_build(grp_panel(), group = "ph")

  index <- as.data.frame(grouped, what = "groups")
  expect_identical(names(index), c(
    "group", "type", "sequences", "observations", "states", "edges"
  ))
  expect_identical(nrow(index), 2L)
  expect_identical(sum(index$observations), 120L)

  edges <- as.data.frame(grouped)
  expect_identical(names(edges), c("group", "from", "to", "weight"))
  expect_identical(nrow(edges), 18L)
  # ts_tna is row-stochastic, so each group's rows sum to one per state.
  expect_equal(sum(edges$weight), 6)

  # Column types are part of the contract: a factor carries the group and state
  # order, which a character column would silently drop.
  expect_s3_class(edges$group, "factor")
  expect_s3_class(edges$from, "factor")
  expect_s3_class(edges$to, "factor")
  expect_type(edges$weight, "double")
  expect_identical(levels(edges$group), names(grouped))
  expect_identical(levels(edges$from), grp_labels)
  expect_identical(levels(edges$to), grp_labels)

  # Row counts and a total prove nothing about WHICH weight landed where.
  # Every cell of every group's matrix must be recoverable from the table.
  expect_equal(
    lapply(
      split(edges$weight, edges$group),
      function(weights) matrix(weights, nrow = 3L, dimnames = list(grp_labels, grp_labels))
    ),
    lapply(grouped, as.matrix)
  )

  series <- as.data.frame(grouped, what = "series")
  expect_identical(nrow(series), 120L)
  expect_true(all(c("group", "id", "time", "value", "state") %in% names(series)))
})

test_that("group works on wide data, where it is a row-level column", {
  skip_if_not_installed("Nestimate")
  # Selecting one measurement column makes the data wide: the column is one
  # series and the day type varies by row, so the group is matched on the
  # row index.
  rows <- subset(esm_srl, !is.na(anxiety))
  grouped <- ts_tna(
    rows,
    series = "anxiety", group = "day_type", labels = grp_labels
  )
  index <- as.data.frame(grouped, what = "groups")

  expect_setequal(as.character(index$group), c("weekday", "weekend"))
  expect_identical(sum(index$observations), nrow(rows))
  expect_true(all(index$states == 3L))
})

test_that("group rejects input it cannot align", {
  skip_if_not_installed("Nestimate")
  panel <- grp_panel()

  expect_error(ts_tna(rnorm(50), group = "ph"), "data frame")
  expect_error(grp_build(panel, group = "absent"), "not a column")

  missing <- panel
  missing$ph[3L] <- NA_character_
  expect_error(grp_build(missing, group = "ph"), "NA")
})

test_that("the ungrouped model is unchanged by the grouped code path", {
  skip_if_not_installed("Nestimate")
  pooled <- grp_build(grp_panel())

  expect_s3_class(pooled, "ts_tna")
  expect_false(inherits(pooled, "ts_tna_group"))
  # The refactor moved the builder into a shared internal; the provenance
  # fields the rest of the package reads must all survive it.
  expect_true(all(c(
    "type", "discretization", "transform", "n_states", "breaks",
    "segment", "overlap", "series", "builder_args"
  ) %in% names(pooled$meta$tsn)))
  expect_identical(pooled$meta$tsn$type, "tna")
  expect_null(pooled$meta$tsn$group)
})

# ---------------------------------------------------------------------------
# Regressions. Each of these reproduces a defect found by adversarial review;
# every one produced a complete, plausible, WRONG result rather than an error.
# ---------------------------------------------------------------------------

test_that("group labels survive discretization dropping rows", {
  skip_if_not_installed("Nestimate")
  # Ordinal embedding trims the tail of every series, so the tidy table no
  # longer lines up with the input row for row. Assigning labels by position
  # would quietly hand one series another series' groups.
  ordinal_panel <- data.frame(
    id = rep(c("p", "q"), each = 6),
    t = rep(1:6, times = 2),
    v = c(3, 1, 4, 2, 5, 3, 9, 7, 8, 6, 9, 7),
    ph = rep(c("early", "late"), each = 3, times = 2),
    stringsAsFactors = FALSE
  )
  grouped <- ts_ftna(
    ordinal_panel,
    value = "v", id = "id", time = "t", group = "ph",
    discretization = "ordinal", m = 2
  )
  series <- as.data.frame(grouped, what = "series")

  # Rows really were dropped, or this test would prove nothing.
  expect_lt(nrow(series), nrow(ordinal_panel))

  origin <- sub("[.].*$", "", as.character(series$id))
  expected <- ordinal_panel$ph[
    match(paste(origin, series$time), paste(ordinal_panel$id, ordinal_panel$t))
  ]
  expect_identical(as.character(series$group), expected)
})

test_that("group labels key on values, not on their character rendering", {
  skip_if_not_installed("Nestimate")
  # as.character() is not injective on doubles: 1 and 1 + eps both render as
  # "1", as do 0.1 + 0.2 and 0.3. A character key maps two distinct
  # observations onto one input row and assigns one of them the wrong group.
  collide <- data.frame(
    id = "s",
    t = c(1 + .Machine$double.eps, 1, 2, 3),
    v = c(9, 1, 9, 1),
    g = c("B", "A", "A", "B"),
    stringsAsFactors = FALSE
  )
  grouped <- suppressWarnings(ts_ftna(
    collide,
    value = "v", id = "id", time = "t", group = "g",
    discretization = "threshold", n_states = 2, breaks = 5,
    labels = c("low", "high")
  ))
  series <- as.data.frame(grouped, what = "series")

  expected <- collide$g[match(series$time, collide$t)]
  expect_identical(as.character(series$group), expected)
  # In time order the groups alternate, so every run is a singleton and no
  # transition exists in either network.
  expect_equal(sum(as.matrix(grouped[["A"]])), 0, tolerance = 0)
  expect_equal(sum(as.matrix(grouped[["B"]])), 0, tolerance = 0)
})

test_that("group keys off a tsn_states input's own id and time columns", {
  skip_if_not_installed("Nestimate")
  raw <- data.frame(
    id = rep(c("x", "y"), each = 3),
    t = rep(1:3, times = 2),
    v = c(1, 9, 1, 9, 1, 9),
    g = rep(c("A", "B"), each = 3),
    stringsAsFactors = FALSE
  )
  states <- discretize(
    raw,
    value = "v", id = "id", time = "t",
    method = "threshold", n_states = 2, breaks = 5,
    labels = c("low", "high")
  )
  states$g <- raw$g

  # `id`/`time` are not passed again here, but the states object already
  # carries them. Reading the key off the call arguments instead would treat
  # the repeated times 1,2,3 as the same rows and lose group B entirely.
  grouped <- ts_ftna(states, group = "g")
  expect_identical(names(grouped), c("A", "B"))
  expect_identical(
    as.data.frame(grouped, what = "groups")$observations, c(3L, 3L)
  )
})

test_that("generated sequence IDs never collide with the caller's own IDs", {
  skip_if_not_installed("Nestimate")
  # Series "1" split into runs would naively become "1.1", which the caller
  # already used as a series name. .tsn_state_sequences() groups by ID, so the
  # two would be welded together and a transition counted between them.
  aliasing <- data.frame(
    id = c("1", "1", "1.1"),
    t = c(1, 2, 1),
    v = c(1, 9, 9),
    g = c("A", "B", "A"),
    stringsAsFactors = FALSE
  )
  grouped <- suppressWarnings(ts_ftna(
    aliasing,
    value = "v", id = "id", time = "t", group = "g",
    discretization = "threshold", n_states = 2, breaks = 5,
    labels = c("low", "high")
  ))
  series <- as.data.frame(grouped, what = "series")

  # Two separate one-observation sequences in group A, not one of length two.
  expect_identical(nrow(subset(series, group == "A")), 2L)
  expect_identical(length(unique(subset(series, group == "A")$id)), 2L)
  expect_equal(sum(as.matrix(grouped[["A"]])), 0, tolerance = 0)
})

test_that("the group column is not discretized as another measured series", {
  skip_if_not_installed("Nestimate")
  # In wide input every numeric column is a series, so a numeric group column
  # would be discretized, shift the pooled cut points, and contribute its own
  # nodes and transitions.
  wide <- data.frame(x = c(1, 9, 1, 9), y = c(9, 1, 9, 1), g = c(1, 1, 2, 2))
  grouped <- ts_ftna(
    wide,
    group = "g", discretization = "threshold", n_states = 2, breaks = 5,
    labels = c("low", "high")
  )
  series <- as.data.frame(grouped, what = "series")

  expect_setequal(sub("[.].*$", "", as.character(series$id)), c("x", "y"))
  expect_identical(
    as.data.frame(grouped, what = "groups")$observations, c(4L, 4L)
  )
})

test_that("a group discretization removed entirely is reported, not dropped", {
  skip_if_not_installed("Nestimate")
  # Ordinal trimming can consume every observation a group had.
  trimmed <- data.frame(
    id = "s", t = 1:8, v = c(3, 1, 4, 2, 5, 3, 6, 2),
    g = c(rep("A", 6), "B", "B"), stringsAsFactors = FALSE
  )
  expect_warning(
    grouped <- ts_ftna(
      trimmed,
      value = "v", id = "id", time = "t", group = "g",
      discretization = "ordinal", m = 3
    ),
    "none survived discretization"
  )
  expect_identical(names(grouped), "A")
})

test_that("group rejects labels and columns it cannot represent", {
  skip_if_not_installed("Nestimate")
  blank <- data.frame(
    id = "s", t = 1:6, v = c(1, 9, 1, 9, 1, 9),
    g = c("", "", "", "A", "A", "A"), stringsAsFactors = FALSE
  )
  expect_error(
    ts_ftna(
      blank, value = "v", id = "id", time = "t", group = "g",
      discretization = "threshold", n_states = 2, breaks = 5,
      labels = c("low", "high")
    ),
    "empty label"
  )

  panel <- grp_panel()
  expect_error(grp_build(panel, group = "t"), "column of its own")
  expect_error(grp_build(panel, group = "id"), "column of its own")
})

test_that("a factor group column fixes the group order independently of locale", {
  skip_if_not_installed("Nestimate")
  panel <- grp_panel()
  panel$ph <- factor(panel$ph, levels = c("late", "early"))
  grouped <- grp_build(panel, group = "ph")

  # Declared level order wins over alphabetical order.
  expect_identical(names(grouped), c("late", "early"))
  expect_identical(levels(as.data.frame(grouped)$group), c("late", "early"))
})

test_that("each group's model records its own label", {
  skip_if_not_installed("Nestimate")
  grouped <- grp_build(grp_panel(), group = "ph")

  expect_identical(
    vapply(grouped, function(model) model$meta$tsn$group_label, character(1L)),
    c(early = "early", late = "late")
  )
  expect_identical(
    unique(vapply(grouped, function(model) model$meta$tsn$group, character(1L))),
    "ph"
  )
})

test_that("group composes with overlapping segmentation", {
  skip_if_not_installed("Nestimate")
  grouped <- grp_build(grp_panel(), group = "ph", segment = 2, overlap = TRUE)
  series <- as.data.frame(grouped, what = "series")

  early <- unique(as.character(subset(series, group == "early")$id))
  late <- unique(as.character(subset(series, group == "late")$id))
  expect_length(intersect(early, late), 0L)
  expect_setequal(sub("[.].*$", "", early), as.character(seq_len(12)))
  # Sliding windows within a group of five observations give four blocks each.
  expect_identical(as.data.frame(grouped, what = "groups")$sequences, c(48L, 48L))
})

test_that("a single-level group column still returns a collection", {
  skip_if_not_installed("Nestimate")
  panel <- grp_panel()
  panel$ph <- "only"
  grouped <- grp_build(panel, group = "ph")

  expect_s3_class(grouped, "ts_tna_group")
  expect_identical(names(grouped), "only")
  expect_identical(
    as.data.frame(grouped, what = "groups")$observations, 120L
  )
})

test_that("plot on a grouped model draws exactly one selected group", {
  skip_if_not_installed("Nestimate")
  skip_if_not_installed("cograph")
  skip_if_not_installed("igraph")
  grouped <- grp_build(grp_panel(), group = "ph")
  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output)
  on.exit({
    grDevices::dev.off()
    unlink(output)
  }, add = TRUE)

  expect_invisible(plot(grouped, group = "early"))
  expect_invisible(plot(grouped, group = "late", type = "network"))
  expect_error(plot(grouped), "exactly one")
  expect_error(plot(grouped, group = "missing"), "Unknown group")
  expect_error(plot(grouped, group = c("early", "late")), "exactly one")
})
