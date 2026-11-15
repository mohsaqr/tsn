#' tsn: Time-Series Network Construction
#'
#' `tsn` constructs distance, visibility, and state-transition networks from
#' time-series data. [tsn()] is the core network constructor; [vg()],
#' [discretize()], [trend()], and the [ts_tna()] family ([ts_tna()],
#' [ts_ftna()], [ts_cna()], [ts_atna()]) provide focused workflows, and
#' [series_networks()] splits a pooled transition model into one network per
#' series. Every result is read through `print()`, `summary()`, `plot()`, and
#' the `as.data.frame()` / `as.matrix()` accessors. Core construction uses only
#' packages distributed with R, while transition-network models require the
#' suggested Nestimate package. Two example data sets ship with the package:
#' [srl] and [esm_srl].
#'
#' @keywords internal
"_PACKAGE"
