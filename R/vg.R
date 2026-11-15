#' Build a Visibility Graph
#'
#' `vg()` is a discoverable verb for the package's flagship construction, the
#' visibility graph. It is a thin wrapper around [tsn()] with
#' `method = "visibility"`: `vg(x)` is equivalent to `tsn(x, "nvg")` and
#' `vg(x, "horizontal")` is equivalent to `tsn(x, "hvg")`. Every input shape
#' and every visibility-relevant argument that [tsn()] accepts is forwarded
#' through `...`, and the result is the same dual-class `tsn`/`netobject`/
#' `cograph_network` object.
#'
#' @param data A numeric vector, `ts`, numeric matrix, named list of numeric
#'   vectors, or data frame (the same inputs [tsn()] accepts).
#' @param type Visibility rule: `"natural"` (the natural visibility graph, the
#'   default) or `"horizontal"` (the horizontal visibility graph).
#' @param ... Any other [tsn()] argument relevant to visibility networks, for
#'   example `value`, `id`, `time`, `series`, `unit`, `state`, `discretization`,
#'   `n_states`, `m`, `tau`, `directed`, `limit`, `penetrable`, `decay`,
#'   `aggregation`, and `seed`. `method` and `visibility` are fixed by `vg()`
#'   itself and are rejected here.
#' @details
#' With a numeric, `Date`, or `POSIXct` time axis, the natural rule, `limit`,
#' and `decay` use elapsed time (days for `Date`, seconds for `POSIXct`), so
#' irregular sampling changes which points see each other; otherwise
#' observations are one step apart. The horizontal rule depends only on the
#' order of the values. Ties block under both rules, as in the strict
#' published definitions, and `limit` is inclusive.
#' @return A tidy `tsn` network object (also a `netobject` and a
#'   `cograph_network`), equivalent to the corresponding [tsn()] call. The
#'   stored provenance call records `vg()` rather than `tsn()`.
#' @seealso [tsn()] for the full argument set and the distance-network family.
#' @examples
#' vg(c(3, 1, 4, 2, 5, 3, 6, 2, 7))
#' vg(c(3, 1, 4, 2, 5, 3, 6, 2, 7), "horizontal")
#'
#' network <- vg(c(3, 1, 4, 2, 5, 3, 6, 2, 7), "horizontal", directed = TRUE)
#' if (requireNamespace("cograph", quietly = TRUE)) {
#'   plot(network)
#' }
#'
#' data(srl)
#' states <- vg(
#'   srl,
#'   value = "effort",
#'   id = "name",
#'   time = "day",
#'   series = "Erik",
#'   unit = "state",
#'   discretization = "quantile"
#' )
#' plot(states, "series", overlay = "vertical")
#' @export
vg <- function(data, type = c("natural", "horizontal"), ...) {
  type <- match.arg(type)
  fixed <- intersect(
    names(match.call(expand.dots = FALSE)$...),
    c("method", "visibility")
  )
  if (length(fixed) > 0L) {
    stop(sprintf(
      "%s cannot be passed to `vg()`, which always builds a visibility graph; choose the rule with `type` (\"natural\" or \"horizontal\") or call `tsn()` directly.",
      paste0("`", fixed, "`", collapse = " and ")
    ), call. = FALSE)
  }
  network <- tsn(data = data, method = "visibility", visibility = type, ...)
  # Record the caller's own vg() call as provenance, not the forwarding call.
  network$parameters$call <- match.call()
  network
}
