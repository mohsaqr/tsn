# Audit regression tests for R/object.R, R/data.R (2026-11-01).

audit_valid_parts <- function(directed = FALSE) {
  network <- tsn(c(1, 2, 1, 3, 2, 3), method = "visibility", unit = "state",
                 state = c("a", "b", "a", "c", "b", "c"), directed = directed)
  list(
    table = as.data.frame(network),
    source = as.data.frame(network, what = "series"),
    nodes = network$nodes$label,
    parameters = network$parameters
  )
}

audit_build <- function(parts) {
  .new_tsn(parts$table, source = parts$source, nodes = parts$nodes,
           parameters = parts$parameters)
}

test_that(".new_tsn accepts the constructor's own output", {
  expect_s3_class(audit_build(audit_valid_parts()), "tsn")
  expect_s3_class(audit_build(audit_valid_parts(directed = TRUE)), "tsn")
})

test_that(".new_tsn rejects a dyad endpoint that is not a node", {
  parts <- audit_valid_parts()
  parts$table$to[1L] <- "ghost"
  expect_error(audit_build(parts), "must be one of `nodes`")
})

test_that(".new_tsn rejects missing connected flags", {
  parts <- audit_valid_parts()
  parts$table$connected[1L] <- NA
  expect_error(audit_build(parts), "no missing values")
})

test_that(".new_tsn rejects repeated dyads, including reversed undirected ones", {
  parts <- audit_valid_parts()
  repeated <- parts$table[c(seq_len(nrow(parts$table)), 2L), ]
  parts$table <- repeated
  expect_error(audit_build(parts), "one row per dyad")

  # An undirected a-b row plus a b-a row is the same dyad twice.
  undirected <- audit_valid_parts()
  first_pair <- subset(undirected$table, from != to)[1L, ]
  reversed <- within(first_pair, {
    swap <- from
    from <- to
    to <- swap
    rm(swap)
  })
  undirected$table <- rbind(undirected$table, reversed[names(undirected$table)])
  expect_error(audit_build(undirected), "one row per dyad")

  # The same reversed pair is two distinct dyads in a directed network.
  directed <- audit_valid_parts(directed = TRUE)
  pair <- subset(directed$table, from != to)
  absent <- pair[!paste(pair$to, pair$from) %in% paste(pair$from, pair$to), ]
  expect_gt(nrow(absent), 0L)
  flipped <- absent[1L, ]
  flipped[c("from", "to")] <- flipped[c("to", "from")]
  directed$table <- rbind(directed$table, flipped)
  expect_s3_class(audit_build(directed), "tsn")
})

test_that(".new_tsn rejects malformed parameters and directed mismatch", {
  parts <- audit_valid_parts()
  parts$parameters$directed <- NA
  expect_error(audit_build(parts), "parameters\\$directed")

  parts <- audit_valid_parts()
  parts$parameters$unit <- NULL
  expect_error(audit_build(parts), "parameters\\$unit")

  parts <- audit_valid_parts()
  parts$parameters$directed <- TRUE
  expect_error(audit_build(parts), "must match `parameters\\$directed`")
})

test_that("the weight matrix keeps node order, loops, and zero-degree nodes", {
  states <- factor(c("b", "a", "b", "a", "a"), levels = c("a", "b", "c"))
  undirected <- tsn(c(2, 1, 3, 1, 1), method = "visibility", unit = "state",
                    state = states)
  directed <- tsn(c(2, 1, 3, 1, 1), method = "visibility", unit = "state",
                  state = states, directed = TRUE)
  undirected_matrix <- as.matrix(undirected)
  directed_matrix <- as.matrix(directed)

  expect_identical(dimnames(undirected_matrix),
                   list(c("a", "b", "c"), c("a", "b", "c")))
  expect_true(isSymmetric(undirected_matrix))
  expect_false(isSymmetric(directed_matrix))
  expect_gt(undirected_matrix[["a", "a"]], 0)
  expect_identical(unname(undirected_matrix["c", ]), c(0, 0, 0))
  expect_identical(unname(directed_matrix[, "c"]), c(0, 0, 0))
  expect_identical(undirected_matrix, undirected$weights)
})

test_that("as.data.frame agrees with as.matrix cell by cell", {
  network <- tsn(c(1, 1, 2, 3, 3, 3, 1, 2, 2, 5, 5, 1), unit = "state",
                 discretization = "width", n_states = 3, directed = TRUE)
  edges <- as.data.frame(network)
  weights <- as.matrix(network)
  cells <- weights[cbind(edges$from, edges$to)]
  expect_equal(cells[edges$connected], edges$weight[edges$connected])
  expect_identical(sum(weights > 0), sum(edges$connected))
})

test_that("summary reports NA weight range when no edge is connected", {
  network <- tsn(
    list(a = 1:5, b = c(50, 10, 40, 20, 30), c = c(90, 90, 1, 1, 90)),
    method = "distance", unit = "series", connect = "threshold",
    threshold = 1e-4
  )
  overview <- summary(network)
  expect_identical(overview$edges, 0L)
  expect_identical(overview$density, 0)
  expect_true(is.na(overview$minimum_weight))
  expect_true(is.na(overview$maximum_weight))
  expect_true(all(as.matrix(network) == 0))
})

test_that("a complete directed non-state network has density 0.5", {
  # Directed distance edges run forward in unit order only, so the
  # n * (n - 1) denominator caps density at one half (igraph convention).
  network <- tsn(list(a = 1:5, b = c(5, 1, 4, 2, 3), c = c(9, 9, 1, 1, 9)),
                 method = "distance", unit = "series", directed = TRUE)
  expect_identical(summary(network)$edges, 3L)
  expect_equal(summary(network)$density, 0.5)
})

test_that("a one-state network is a single loop with density 1", {
  network <- tsn(1:6, method = "visibility", unit = "state",
                 state = rep("a", 6L))
  overview <- summary(network)
  expect_identical(overview$nodes, 1L)
  expect_equal(overview$density, 1)
  expect_identical(dim(as.matrix(network)), c(1L, 1L))
})

test_that("as.data.frame.tsn honours row.names instead of ignoring them", {
  network <- tsn(c(3, 1, 4, 2, 5), method = "visibility")
  n_dyads <- nrow(as.data.frame(network))
  expect_identical(n_dyads, 6L)
  edges <- as.data.frame(network, row.names = paste0("dyad", seq_len(n_dyads)))
  expect_identical(rownames(edges), paste0("dyad", seq_len(n_dyads)))
  expect_identical(rownames(as.data.frame(network)),
                   as.character(seq_len(n_dyads)))
  series <- as.data.frame(network, what = "series",
                          row.names = letters[seq_len(5L)])
  expect_identical(rownames(series), letters[seq_len(5L)])
  expect_error(as.data.frame(network, row.names = "only_one"))
})

test_that("print.tsn says when it truncates and never points at `$`", {
  long <- tsn(c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8), method = "visibility")
  printed <- utils::capture.output(print(long))
  expect_true(any(grepl(
    sprintf("Showing 10 of %d dyads", nrow(as.data.frame(long))), printed
  )))
  expect_false(any(grepl("$", printed, fixed = TRUE)))

  short <- tsn(c(3, 1, 4), method = "visibility")
  expect_false(any(grepl("Showing", utils::capture.output(print(short)))))
})

test_that("ts_tna accessors match an independent transition count", {
  skip_if_not_installed("Nestimate")
  set.seed(1)
  values <- cumsum(stats::rnorm(60))
  labels <- c("low", "mid", "high")
  counts <- ts_ftna(values, labels = labels)
  states <- factor(as.data.frame(counts, what = "series")$state,
                   levels = labels)
  expected <- unclass(table(utils::head(states, -1L), utils::tail(states, -1L)))
  dimnames(expected) <- list(labels, labels)

  expect_equal(as.matrix(counts), expected, ignore_attr = "names")
  expect_identical(dimnames(as.matrix(counts)), list(labels, labels))

  edges <- as.data.frame(counts)
  expect_identical(nrow(edges), 9L)
  expect_identical(levels(edges$from), labels)
  expect_equal(edges$weight,
               expected[cbind(as.character(edges$from),
                              as.character(edges$to))])

  probabilities <- ts_tna(values, labels = labels)
  expect_equal(unname(rowSums(as.matrix(probabilities))), rep(1, 3L))
  expect_identical(
    rownames(as.data.frame(probabilities, row.names = paste0("p", 1:9))),
    paste0("p", 1:9)
  )
})

test_that("grouped accessors stack every group on one state set", {
  skip_if_not_installed("Nestimate")
  set.seed(2)
  panel <- data.frame(
    id = rep(c("a", "b", "c", "d"), each = 20L),
    t = rep(seq_len(20L), 4L),
    v = c(stats::rnorm(40L, 0), stats::rnorm(40L, 10)),
    g = rep(c("calm", "calm", "busy", "busy"), each = 20L)
  )
  networks <- ts_tna(panel, value = "v", id = "id", time = "t", group = "g",
                     labels = c("low", "mid", "high"))
  edges <- as.data.frame(networks)
  expect_identical(names(edges), c("group", "from", "to", "weight"))
  expect_identical(as.vector(table(edges$group)), c(9L, 9L))
  expect_identical(levels(edges$from), c("low", "mid", "high"))

  series <- as.data.frame(networks, what = "series")
  expect_identical(nrow(series), nrow(panel))
  groups <- as.data.frame(networks, what = "groups",
                          row.names = c("first", "second"))
  expect_identical(rownames(groups), c("first", "second"))
  expect_identical(groups$observations, c(40L, 40L))
})

test_that("srl matches its documentation", {
  data(srl, package = "tsn", envir = environment())
  indicators <- c("efficacy", "value", "planning", "monitoring", "effort",
                  "control", "help", "social", "organizing")
  expect_identical(names(srl), c("name", "day", indicators))
  expect_identical(dim(srl), c(5616L, 11L))
  expect_identical(length(unique(srl$name)), 36L)
  expect_true(all(table(srl$name) == 156L))
  expect_identical(range(srl$day), c(1L, 156L))
  expect_identical(sum(is.na(srl[indicators])), 131L)
  in_range <- vapply(srl[indicators], function(column) {
    all(column >= 0 & column <= 100, na.rm = TRUE)
  }, logical(1L))
  expect_true(all(in_range))
  expect_identical(order(srl$name, srl$day), seq_len(nrow(srl)))
})

test_that("esm_srl matches its documentation", {
  data(esm_srl, package = "tsn", envir = environment())
  indicators <- c("efficacy", "value", "planning", "monitoring", "effort",
                  "regulation", "motivated", "enjoyment", "anxiety")
  expect_identical(names(esm_srl),
                   c("name", "occasion", "date", "day_type", indicators))
  expect_identical(dim(esm_srl), c(2820L, 13L))
  expect_identical(length(unique(esm_srl$name)), 41L)
  expect_identical(range(as.vector(table(esm_srl$name))), c(47L, 79L))
  expect_identical(sum(is.na(esm_srl[indicators])), 41L)
  expect_s3_class(esm_srl$date, "Date")
  expect_identical(levels(esm_srl$day_type), c("weekday", "weekend"))
  weekend <- format(esm_srl$date, "%u") %in% c("6", "7")
  expect_identical(as.character(esm_srl$day_type),
                   ifelse(weekend, "weekend", "weekday"))
  in_range <- vapply(esm_srl[indicators], function(column) {
    all(column >= 0 & column <= 100, na.rm = TRUE)
  }, logical(1L))
  expect_true(all(in_range))
})
