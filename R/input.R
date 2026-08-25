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

  canonical <- if (stats::is.ts(data)) {
    data.frame(
      id = "series_1",
      time = as.numeric(stats::time(data)),
      value = as.numeric(data),
      stringsAsFactors = FALSE
    )
  } else if (is.numeric(data) && is.null(dim(data))) {
    data.frame(
      id = "series_1",
      time = seq_along(data),
      value = as.numeric(data),
      stringsAsFactors = FALSE
    )
  } else if (is.list(data) && !is.data.frame(data)) {
    .tsn_normalize_list(data)
  } else if (is.matrix(data)) {
    .tsn_normalize_wide(as.data.frame(data, stringsAsFactors = FALSE))
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
#' Numeric and date-time values retain chronological order. Other identifiers
#' retain caller order because their spacing and lexical order need not encode
#' chronology.
#'
#' @param time Public time values.
#' @return A numeric ordering key.
#' @noRd
.tsn_time_sort_key <- function(time) {
  stopifnot(length(time) > 0L, !anyNA(time))
  if (is.numeric(time) || inherits(time, "Date") || inherits(time, "POSIXt")) {
    return(as.numeric(time))
  }
  seq_along(time)
}

#' Convert Time Values to Numeric Analysis Coordinates
#'
#' Numeric and date-time inputs keep their elapsed-time scale. Other ordered
#' labels use observation steps.
#'
#' @param time Time values from one canonical series.
#' @return A strictly increasing finite numeric vector.
#' @noRd
.tsn_time_coordinates <- function(time) {
  stopifnot(length(time) > 0L, !anyNA(time))
  coordinates <- if (is.numeric(time) || inherits(time, "Date") ||
                       inherits(time, "POSIXt")) {
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
  series_names <- names(data)
  if (is.null(series_names) || any(!nzchar(series_names))) {
    series_names <- paste0("series_", seq_along(data))
  }
  if (anyDuplicated(series_names)) {
    stop("List element names must be unique.", call. = FALSE)
  }
  rows <- Map(
    function(series, series_id) {
      data.frame(
        id = series_id,
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
  series_names <- names(data)
  if (is.null(series_names) || any(!nzchar(series_names))) {
    series_names <- paste0("series_", seq_len(ncol(data)))
  }
  rows <- Map(
    function(series, series_id) {
      data.frame(
        id = series_id,
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
#' @noRd
.tsn_normalize_time <- function(time) {
  if (is.character(time) && length(time) > 0L && !anyNA(time) &&
      all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", time))) {
    parsed <- as.Date(time)
    if (!anyNA(parsed)) {
      return(parsed)
    }
  }
  time
}

#' Align optional state values with canonical input ordering
#' @noRd
.tsn_normalize_states <- function(data, state, id = NULL, time = NULL) {
  if (is.null(state)) {
    return(NULL)
  }
  if (!is.data.frame(data)) {
    return(state)
  }
  state_values <- if (is.character(state) && length(state) == 1L) {
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
#' @noRd
.tsn_window_units <- function(data, window, step) {
  stopifnot(is.numeric(window), length(window) == 1L, is.finite(window))
  stopifnot(is.numeric(step), length(step) == 1L, is.finite(step))
  window <- as.integer(window)
  step <- as.integer(step)
  if (window < 2L || step < 1L) {
    stop("`window` must be at least 2 and `step` must be positive.",
         call. = FALSE)
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
