#' Normalize time-series input
#'
#' Convert every supported public input into the canonical long representation
#' used by the numerical engines.
#'
#' @param data A numeric vector, `ts`, matrix, list of numeric vectors, or data
#'   frame.
#' @param value Optional value-column name.
#' @param id Optional series-ID column name.
#' @param time Optional time-column name.
#' @param allow_missing Whether missing numeric values are allowed.
#' @return A data frame with `id`, `time`, and `value` columns.
#' @noRd
.tsn_normalize_input <- function(data, value = NULL, id = NULL, time = NULL,
                                 allow_missing = FALSE) {
  stopifnot(
    is.logical(allow_missing), length(allow_missing) == 1L,
    !is.na(allow_missing)
  )
  if (is.null(data)) {
    stop("`data` must not be NULL.", call. = FALSE)
  }
  # Column arguments only have meaning for a data frame; for a vector, `ts`,
  # matrix, or list they would be ignored, and a time VECTOR passed as `time`
  # would silently be replaced by observation steps.
  column_arguments <- c(value = !is.null(value), id = !is.null(id),
                        time = !is.null(time))
  if (!is.data.frame(data) && any(column_arguments)) {
    stop(sprintf(
      paste0(
        "%s name columns of a data frame and cannot be used with %s input; ",
        "supply a long data frame with value, id, and time columns instead."
      ),
      paste0("`", names(column_arguments)[column_arguments], "`",
             collapse = ", "),
      class(data)[1L]
    ), call. = FALSE)
  }
  # `id` and `time` name columns of long data, which is recognised by `value`.
  # Without `value` every other form ignores them, so a column meant as the
  # series ID would silently become one more measured series.
  if (is.null(value) && (!is.null(id) || !is.null(time))) {
    stop(
      "`id` and `time` name long-data columns, so they require `value`.",
      call. = FALSE
    )
  }

  canonical <- if (stats::is.ts(data) && is.matrix(data)) {
    # A multivariate `ts` is wide data whose rows share the series time axis.
    wide <- .tsn_normalize_wide(as.data.frame(unclass(data)))
    wide$time <- rep(as.numeric(stats::time(data)), times = ncol(data))
    wide
  } else if (stats::is.ts(data)) {
    data.frame(
      id = rep("series_1", length(data)),
      time = as.numeric(stats::time(data)),
      value = as.numeric(data),
      stringsAsFactors = FALSE
    )
  } else if (is.numeric(data) && is.null(dim(data))) {
    data.frame(
      id = rep("series_1", length(data)),
      time = seq_along(data),
      value = as.numeric(data),
      stringsAsFactors = FALSE
    )
  } else if (is.list(data) && !is.data.frame(data)) {
    .tsn_normalize_list(data)
  } else if (is.matrix(data)) {
    # `as.data.frame()` would invent `V1`, `V2`, ... for unnamed columns; keep
    # the missing names so the wide normalizer applies the same `series_<k>`
    # labels as unnamed list and wide input.
    columns <- as.data.frame(unclass(data), stringsAsFactors = FALSE)
    if (is.null(colnames(data))) {
      names(columns) <- rep("", ncol(data))
    }
    .tsn_normalize_wide(columns)
  } else if (is.data.frame(data)) {
    if (is.null(value)) {
      .tsn_normalize_wide(data)
    } else {
      .tsn_normalize_long(data, value = value, id = id, time = time)
    }
  } else {
    stop("`data` must be numeric, ts, matrix, list, or data.frame.",
         call. = FALSE)
  }

  stopifnot(is.data.frame(canonical))
  stopifnot(identical(names(canonical), c("id", "time", "value")))
  if (nrow(canonical) < 2L) {
    stop("`data` must contain at least two observations.", call. = FALSE)
  }
  invalid_value <- !is.numeric(canonical$value) ||
    any(is.infinite(canonical$value)) ||
    (!allow_missing && anyNA(canonical$value))
  if (invalid_value) {
    qualifier <- if (allow_missing) "numeric or missing" else "finite numeric"
    stop(sprintf("Time-series values must be %s values.", qualifier),
         call. = FALSE)
  }
  if (anyNA(canonical$id) || any(!nzchar(canonical$id))) {
    stop("Series IDs must be non-missing, non-empty values.", call. = FALSE)
  }
  if (anyNA(canonical$time)) {
    stop("Time values must not be missing.", call. = FALSE)
  }
  if (.tsn_is_chronological(canonical$time) &&
      !all(is.finite(as.numeric(canonical$time)))) {
    stop("Time values must be finite.", call. = FALSE)
  }
  if (anyDuplicated(canonical[c("id", "time")])) {
    stop("Each `id` and `time` combination must be unique.", call. = FALSE)
  }

  id_order <- match(canonical$id, unique(canonical$id))
  time_order <- .tsn_time_sort_key(canonical$time)
  canonical <- canonical[order(id_order, time_order), , drop = FALSE]
  rownames(canonical) <- NULL
  canonical
}

#' Create a Stable Time-Sorting Key
#'
#' Numeric, date-time, and elapsed-time values retain chronological order.
#' Other identifiers retain caller order because their spacing and lexical order need not encode
#' chronology.
#'
#' @param time Public time values.
#' @return A numeric ordering key.
#' @noRd
.tsn_time_sort_key <- function(time) {
  stopifnot(length(time) > 0L, !anyNA(time))
  if (.tsn_is_chronological(time)) {
    return(as.numeric(time))
  }
  seq_along(time)
}

#' Whether Time Values Encode Chronology
#'
#' Numeric, `Date`, date-time (`POSIXt`), and elapsed-time (`difftime`) values
#' carry an ordered, spaced time axis. Character and factor labels do not:
#' their lexical or level order need not be chronological.
#'
#' @param time Public time values.
#' @return A single logical.
#' @noRd
.tsn_is_chronological <- function(time) {
  is.numeric(time) || inherits(time, "Date") || inherits(time, "POSIXt") ||
    inherits(time, "difftime")
}

#' Convert Time Values to Numeric Analysis Coordinates
#'
#' Numeric, date-time, and elapsed-time inputs keep their own scale (raw
#' values, so gaps are kept and the origin is not shifted to zero). Other
#' ordered labels use observation steps.
#'
#' @param time Time values from one canonical series.
#' @return A strictly increasing finite numeric vector.
#' @noRd
.tsn_time_coordinates <- function(time) {
  stopifnot(length(time) > 0L, !anyNA(time))
  coordinates <- if (.tsn_is_chronological(time)) {
    as.numeric(time)
  } else {
    seq_along(time)
  }
  stopifnot(
    all(is.finite(coordinates)),
    !is.unsorted(coordinates, strictly = TRUE)
  )
  coordinates
}

#' Select complete series before input normalization
#'
#' @param data Public input supplied to `tsn()`.
#' @param series Optional long-data IDs or wide-data column names.
#' @param value Optional long-data value column.
#' @param id Optional long-data ID column.
#' @return Input of the same broad form, restricted to requested series.
#' @noRd
.tsn_select_data <- function(data, series = NULL, value = NULL, id = NULL) {
  if (is.null(series)) {
    return(data)
  }
  if (!is.atomic(series) || length(series) < 1L || anyNA(series)) {
    stop("`series` must contain one or more non-missing identifiers.",
         call. = FALSE)
  }
  requested <- unique(as.character(series))
  if (any(!nzchar(requested))) {
    stop("`series` identifiers must not be empty.", call. = FALSE)
  }

  if (is.data.frame(data) && !is.null(value)) {
    if (is.null(id)) {
      stop("`id` is required when selecting series from long data.",
           call. = FALSE)
    }
    if (!is.character(id) || length(id) != 1L || !id %in% names(data)) {
      stop("`id` must name an existing column when `series` is used.",
           call. = FALSE)
    }
    available <- unique(as.character(data[[id]]))
    unknown <- setdiff(requested, available)
    if (length(unknown) > 0L) {
      stop(sprintf("Unknown series: %s.", paste(unknown, collapse = ", ")),
           call. = FALSE)
    }
    # Request order is authoritative for every input form: wide, matrix, and
    # list selection index by the requested names, so long rows are reordered
    # the same way (stably within each series) rather than keeping table
    # order. Otherwise the same selection could reverse directed or chained
    # edges depending on the input container.
    rows <- which(as.character(data[[id]]) %in% requested)
    position <- match(as.character(data[[id]])[rows], requested)
    return(data[rows[order(position, seq_along(position))], , drop = FALSE])
  }

  if (is.data.frame(data) || is.matrix(data)) {
    available <- colnames(data)
    if (is.null(available)) {
      stop("Matrix columns must be named before they can be selected.",
           call. = FALSE)
    }
    unknown <- setdiff(requested, available)
    if (length(unknown) > 0L) {
      stop(sprintf("Unknown series: %s.", paste(unknown, collapse = ", ")),
           call. = FALSE)
    }
    return(data[, requested, drop = FALSE])
  }

  if (is.list(data)) {
    available <- names(data)
    if (is.null(available)) {
      stop("List series must be named before they can be selected.",
           call. = FALSE)
    }
    unknown <- setdiff(requested, available)
    if (length(unknown) > 0L) {
      stop(sprintf("Unknown series: %s.", paste(unknown, collapse = ", ")),
           call. = FALSE)
    }
    return(data[requested])
  }

  stop("`series` selection requires long, wide, matrix, or list input.",
       call. = FALSE)
}

#' Select Series From Canonical Tidy Data
#'
#' @param data A data frame containing an `id` column.
#' @param series Optional series identifiers.
#' @return `data` restricted to the requested IDs, preserving class and
#'   attributes.
#' @noRd
.tsn_select_canonical <- function(data, series = NULL) {
  stopifnot(
    is.data.frame(data), "id" %in% names(data),
    is.atomic(data$id), !anyNA(data$id)
  )
  if (is.null(series)) {
    return(data)
  }
  if (!is.atomic(series) || length(series) < 1L || anyNA(series)) {
    stop("`series` must contain one or more non-missing identifiers.",
         call. = FALSE)
  }
  selected <- unique(as.character(series))
  if (any(!nzchar(selected))) {
    stop("`series` identifiers must not be empty.", call. = FALSE)
  }
  available <- unique(as.character(data$id))
  unknown <- setdiff(selected, available)
  if (length(unknown) > 0L) {
    stop(sprintf("Unknown series: %s.", paste(unknown, collapse = ", ")),
         call. = FALSE)
  }
  # Same ordering contract as `.tsn_select_data()`: the requested order is
  # authoritative, stably within each series.
  rows <- which(as.character(data$id) %in% selected)
  position <- match(as.character(data$id)[rows], selected)
  data[rows[order(position, seq_along(position))], , drop = FALSE]
}

#' Series Labels for List and Wide Input
#'
#' Supplied names are kept; only missing or empty names are filled with their
#' positional `series_<k>` label, so a partially named list keeps the caller's
#' labels. Collisions are left for the caller's uniqueness check to reject.
#'
#' @param supplied Names of the list elements or columns, or `NULL`.
#' @param count Number of elements or columns.
#' @return A character vector of length `count`.
#' @noRd
.tsn_series_names <- function(supplied, count) {
  stopifnot(length(count) == 1L, count >= 0)
  positional <- paste0("series_", seq_len(count))
  if (is.null(supplied)) {
    return(positional)
  }
  blank <- is.na(supplied) | !nzchar(supplied)
  supplied[blank] <- positional[blank]
  supplied
}

#' @noRd
.tsn_normalize_list <- function(data) {
  stopifnot(length(data) > 0L)
  valid <- vapply(
    data,
    function(series) is.numeric(series) && is.null(dim(series)) &&
      length(series) > 0L,
    logical(1L)
  )
  if (!all(valid)) {
    stop("Every list element must be a non-empty numeric vector.",
         call. = FALSE)
  }
  series_names <- .tsn_series_names(names(data), length(data))
  if (anyDuplicated(series_names)) {
    stop("List element names must be unique.", call. = FALSE)
  }
  rows <- Map(
    function(series, series_id) {
      data.frame(
        id = rep(series_id, length(series)),
        time = seq_along(series),
        value = as.numeric(series),
        stringsAsFactors = FALSE
      )
    },
    data,
    series_names
  )
  do.call(rbind, rows)
}

#' @noRd
.tsn_normalize_wide <- function(data) {
  stopifnot(is.data.frame(data))
  if (ncol(data) < 1L || !all(vapply(data, is.numeric, logical(1L)))) {
    stop(
      "Wide input must contain only numeric time-series columns; otherwise specify `value`.",
      call. = FALSE
    )
  }
  series_names <- .tsn_series_names(names(data), ncol(data))
  if (anyDuplicated(series_names)) {
    stop("Wide column names must be unique.", call. = FALSE)
  }
  rows <- Map(
    function(series, series_id) {
      data.frame(
        id = rep(series_id, length(series)),
        time = seq_along(series),
        value = as.numeric(series),
        stringsAsFactors = FALSE
      )
    },
    data,
    series_names
  )
  do.call(rbind, rows)
}

#' @noRd
.tsn_normalize_long <- function(data, value, id = NULL, time = NULL) {
  selectors <- list(value = value, id = id, time = time)
  valid_selectors <- vapply(
    selectors[!vapply(selectors, is.null, logical(1L))],
    function(column) is.character(column) && length(column) == 1L &&
      !is.na(column) && nzchar(column),
    logical(1L)
  )
  if (!all(valid_selectors)) {
    stop("Column arguments must be single, non-missing names.", call. = FALSE)
  }
  requested <- unlist(selectors[!vapply(selectors, is.null, logical(1L))],
                      use.names = FALSE)
  missing_columns <- setdiff(requested, names(data))
  if (length(missing_columns) > 0L) {
    stop(sprintf("Unknown column: %s.", paste(missing_columns, collapse = ", ")),
         call. = FALSE)
  }

  id_values <- if (is.null(id)) rep("series_1", nrow(data)) else data[[id]]
  # Series are keyed by their rendered labels. `as.character()` is not
  # injective on doubles (`0.1 + 0.2` and `0.3` both render "0.3"), so distinct
  # IDs that render alike would be merged into one series without notice.
  if (length(unique(id_values)) != length(unique(as.character(id_values)))) {
    stop(
      "Distinct `id` values render to the same label; convert `id` to ",
      "character or integer labels first.",
      call. = FALSE
    )
  }
  time_values <- if (is.null(time)) {
    stats::ave(seq_len(nrow(data)), as.character(id_values), FUN = seq_along)
  } else {
    .tsn_normalize_time(data[[time]])
  }
  data.frame(
    id = as.character(id_values),
    time = time_values,
    value = data[[value]],
    stringsAsFactors = FALSE
  )
}

#' Normalize common calendar time values
#'
#' Character time values that all have the `YYYY-MM-DD` shape are parsed as
#' `Date`; an impossible date among them is an error. Other values are
#' returned unchanged.
#'
#' @param time A time column.
#' @return `time`, or its `Date` parse.
#' @noRd
.tsn_normalize_time <- function(time) {
  if (is.character(time) && length(time) > 0L && !anyNA(time) &&
      all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", time))) {
    # An explicit format makes an impossible date parse to NA instead of
    # erroring inside `as.Date()` or depending on which element comes first.
    parsed <- as.Date(time, format = "%Y-%m-%d")
    if (anyNA(parsed)) {
      # Falling back to caller-ordered labels would silently change the time
      # axis of the whole column because of one typo.
      stop(sprintf(
        "Time values look like ISO dates but are not valid dates: %s.",
        paste(unique(time[is.na(parsed)]), collapse = ", ")
      ), call. = FALSE)
    }
    return(parsed)
  }
  time
}

#' Align optional state values with canonical input ordering
#'
#' @param data Public input after series selection.
#' @param state `NULL`, a state-column name, or a state vector. A vector for
#'   long data with `id`/`time` holds one value per row of `data` and is
#'   reordered with the rows; otherwise it already follows the canonical
#'   (series-concatenated) order and is returned unchanged.
#' @param id,time Optional long-data column names.
#' @return `NULL` or the state values in canonical row order.
#' @noRd
.tsn_normalize_states <- function(data, state, id = NULL, time = NULL) {
  if (is.null(state)) {
    return(NULL)
  }
  if (!is.data.frame(data)) {
    return(state)
  }
  by_column <- is.character(state) && length(state) == 1L
  # Without `id` and `time` there is nothing to reorder: long data is one
  # series in row order, and wide data concatenates its columns in series
  # order, so a state vector already follows the canonical rows. Comparing its
  # length with `nrow(data)` would wrongly reject a vector for wide data; the
  # caller checks it against the canonical observations instead.
  if (!by_column && is.null(id) && is.null(time)) {
    return(state)
  }
  state_values <- if (by_column) {
    if (!state %in% names(data)) {
      stop(sprintf("Unknown state column: %s.", state), call. = FALSE)
    }
    data[[state]]
  } else {
    state
  }
  if (length(state_values) != nrow(data)) {
    stop("`state` must provide one value per observation.", call. = FALSE)
  }
  id_values <- if (is.null(id)) rep("series_1", nrow(data)) else data[[id]]
  time_values <- if (is.null(time)) {
    stats::ave(seq_len(nrow(data)), as.character(id_values), FUN = seq_along)
  } else {
    .tsn_normalize_time(data[[time]])
  }
  aligned <- data.frame(
    id = as.character(id_values),
    time = time_values,
    state = state_values,
    stringsAsFactors = FALSE
  )
  id_order <- match(aligned$id, unique(aligned$id))
  time_order <- .tsn_time_sort_key(aligned$time)
  aligned <- aligned[order(id_order, time_order), , drop = FALSE]
  aligned$state
}

#' Construct complete-series units
#' @noRd
.tsn_series_units <- function(data) {
  ids <- unique(data$id)
  units <- lapply(ids, function(series_id) data$value[data$id == series_id])
  names(units) <- ids
  metadata <- data.frame(
    node = ids,
    series = ids,
    start = 1L,
    end = vapply(units, length, integer(1L)),
    stringsAsFactors = FALSE
  )
  list(units = units, metadata = metadata)
}

#' Construct point-level units (one node per time point)
#' @noRd
.tsn_point_units <- function(data) {
  # The same collision-safe encoder as the visibility family: `(id, time)`
  # pairs whose display strings collide get length-prefixed labels instead of
  # being rejected.
  labels <- .tsn_node_labels(data$id, data$time)
  units <- as.list(data$value)
  names(units) <- labels
  position <- stats::ave(seq_len(nrow(data)), data$id, FUN = seq_along)
  metadata <- data.frame(
    node = labels,
    series = data$id,
    start = as.integer(position),
    end = as.integer(position),
    stringsAsFactors = FALSE
  )
  list(units = units, metadata = metadata)
}

#' Construct sliding-window units
#'
#' Windows of `window` consecutive observations start at 1, 1 + `step`, ...
#' within each series; trailing observations that do not fill a window are
#' not covered. Labels are `<series>:W<k>`.
#'
#' @param data Canonical long data.
#' @param window Whole-number window width, at least 2 and at most the length
#'   of the shortest series.
#' @param step Positive whole-number step.
#' @return A list with named `units` and a `metadata` data frame.
#' @noRd
.tsn_window_units <- function(data, window, step) {
  stopifnot(is.numeric(window), length(window) == 1L, is.finite(window))
  stopifnot(is.numeric(step), length(step) == 1L, is.finite(step))
  if (window < 2 || step < 1) {
    stop("`window` must be at least 2 and `step` must be positive.",
         call. = FALSE)
  }
  # `as.integer()` truncates, so a fractional width or step would otherwise be
  # silently replaced by a different one.
  if (window != round(window) || step != round(step)) {
    stop("`window` and `step` must be whole numbers.", call. = FALSE)
  }

  groups <- split(
    data,
    factor(data$id, levels = unique(data$id)),
    drop = TRUE
  )
  too_short <- vapply(groups, nrow, integer(1L)) < window
  if (any(too_short)) {
    stop(sprintf(
      "`window` exceeds the length of: %s.",
      paste(names(groups)[too_short], collapse = ", ")
    ), call. = FALSE)
  }
  # Both now fit an integer: `window` is at most a series length, and a step
  # beyond the integer range selects the same single window as any step longer
  # than the series.
  window <- as.integer(window)
  step <- as.integer(min(step, .Machine$integer.max))

  per_group <- Map(
    function(group, series_id) {
      starts <- seq.int(1L, nrow(group) - window + 1L, by = step)
      ends <- starts + window - 1L
      labels <- paste0(series_id, ":W", seq_along(starts))
      units <- Map(
        function(start, end) group$value[seq.int(start, end)],
        starts,
        ends
      )
      names(units) <- labels
      list(
        units = units,
        metadata = data.frame(
          node = labels,
          series = rep.int(series_id, length(labels)),
          start = starts,
          end = ends,
          stringsAsFactors = FALSE
        )
      )
    },
    groups,
    names(groups)
  )
  list(
    units = unlist(unname(lapply(per_group, `[[`, "units")), recursive = FALSE),
    metadata = do.call(rbind, lapply(per_group, `[[`, "metadata"))
  )
}
