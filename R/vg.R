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
#'   `aggregation`, and `seed`.
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
  tsn(data = data, method = "visibility", visibility = type, ...)
}
