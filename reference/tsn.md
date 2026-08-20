# Build a Time-Series Network

`tsn()` is the core entry point for constructing networks from
time-series geometry. It builds distance networks between complete
series or sliding windows, and natural or horizontal visibility networks
between time points or discretized states.

## Usage

``` r
tsn(
  data,
  method = "visibility",
  value = NULL,
  id = NULL,
  time = NULL,
  series = NULL,
  unit = NULL,
  distance = "euclidean",
  connect = "full",
  window = NULL,
  step = 1L,
  neighbors = NULL,
  threshold = NULL,
  percentile = NULL,
  bandwidth = NULL,
  p = 2,
  bins = NULL,
  lag = NULL,
  tolerance = NULL,
  similarity = NULL,
  visibility = "natural",
  state = NULL,
  discretization = "gaussian",
  n_states = 3L,
  breaks = NULL,
  m = NULL,
  tau = NULL,
  directed = FALSE,
  limit = NULL,
  penetrable = 0L,
  decay = 0,
  aggregation = "sum",
  chain = FALSE,
  normalize = FALSE,
  seed = NULL
)
```

## Arguments

- data:

  A numeric vector, `ts`, numeric matrix, named list of numeric vectors,
  or data frame.

- method:

  Network selector. The base families are `"distance"` and
  `"visibility"`. Convenience shortcuts resolve to a family with
  sensible defaults: `"nvg"`/`"natural"` (natural visibility graph),
  `"hvg"`/ `"horizontal"` (horizontal visibility graph), and any
  discretizer name (`"ordinal"`, `"quantile"`, `"symbolic"`, ...) which
  builds a visibility network on states from that discretizer. Shortcuts
  only set defaults; the granular arguments (`unit`, `visibility`,
  `discretization`) still apply.

- value:

  Optional value-column name for long data.

- id:

  Optional series-ID column name for long data.

- time:

  Optional time-column name for long data.

- series:

  Optional series IDs for long data or numeric column names for wide
  data. Selection happens inside `tsn()`.

- unit:

  Node unit. Distance networks use `"series"`, `"window"`, or `"time"`
  (one node per time point); visibility networks use `"time"` or
  `"state"`. When `NULL`, the natural unit for the selected method is
  inferred.

- distance:

  Distance measure for distance networks. One of `"euclidean"`,
  `"manhattan"`, `"maximum"`, `"canberra"`, `"minkowski"`, `"binary"`,
  `"cosine"`, `"correlation"`, `"spearman"`, `"dtw"`, `"ccf"` (one minus
  the maximum absolute cross-correlation across lags), `"nmi"` (one
  minus normalized mutual information after separately quantile-binning
  each series), `"voi"` (variation of information after the same
  marginal binning), `"event_sync"` (one minus the Quiroga
  event-synchronization index; units are interpreted as event times), or
  `"van_rossum"` (exact van Rossum spike-train distance; units are
  interpreted as event times).

- connect:

  Distance-to-network rule.

- window:

  Sliding-window width when `unit = "window"`.

- step:

  Sliding-window step.

- neighbors:

  Number of neighbours when `connect = "nearest"`.

- threshold:

  Maximum distance when `connect = "threshold"`.

- percentile:

  Proportion of shortest distances retained when
  `connect = "percentile"`.

- bandwidth:

  Positive kernel scale. Used by `connect = "gaussian"` and by the
  `"negative_exp"` and `"gaussian"` similarity kernels; defaults to the
  median positive distance.

- p:

  Minkowski power.

- bins:

  Number of marginal quantile bins for `distance = "nmi"` and
  `distance = "voi"`.

- lag:

  Maximum cross-correlation lag for `distance = "ccf"`. `NULL` uses the
  [`stats::ccf()`](https://rdrr.io/r/stats/acf.html) default.

- tolerance:

  Event-time scale for the event-based distances: an optional upper
  bound on the adaptive coincidence window for
  `distance = "event_sync"`, or the kernel time constant for
  `distance = "van_rossum"`. For event synchronization, `NULL` uses the
  uncapped adaptive local window. For van Rossum, `NULL` uses the median
  positive inter-event interval of each pooled pair.

- similarity:

  Optional similarity kernel mapping edge distances to weights:
  `"inverse"` (the default weight rule, `1 / (1 + d)`),
  `"normalized_inverse"` (`1 - d / max(d)`), `"negative_exp"`
  (`exp(-d / bandwidth)`), or `"gaussian"`
  (`exp(-d^2 / (2 * bandwidth^2))`).

- visibility:

  Visibility rule: `"natural"` or `"horizontal"`.

- state:

  Optional state-column name or state vector.

- discretization:

  Internal state-discretization method. One of `"threshold"`, `"width"`,
  `"quantile"`, `"kde"`, `"kmeans"`, `"gaussian"`, `"hclust"`,
  `"ordinal"`, `"symbolic"`, `"change_points"`, `"entropy"`,
  `"magnitude"`, `"adaptive_magnitude"`, `"percentile_magnitude"`, or
  `"dtw"`.

- n_states:

  Number of states. Ignored by `discretization = "ordinal"`, whose state
  count follows the embedding arguments `m` and `tau`.

- breaks:

  Optional internal thresholds when `discretization = "threshold"`.

- m:

  Embedding dimension for `discretization = "ordinal"` (default `3`).
  Only valid with the ordinal discretizer.

- tau:

  Embedding lag for `discretization = "ordinal"` (default `1`). Only
  valid with the ordinal discretizer.

- directed:

  Whether edges follow their ordered direction. Visibility edges then
  run forward in time; distance edges follow the evaluated unit order.
  With `FALSE`, reciprocal distance relations are represented by one
  undirected edge.

- limit:

  Optional maximum visibility distance in the units of `time` (or
  observation steps when no numeric/date-time axis is supplied).

- penetrable:

  Number of intermediate points allowed to block visibility.

- decay:

  Non-negative exponential edge-decay rate per unit of `time` (or per
  observation step when no numeric/date-time axis is supplied).

- aggregation:

  State-edge aggregation rule.

- chain:

  When `TRUE`, distance networks connect only consecutive series or
  windows (a transition chain) instead of all pairs.

- normalize:

  Distance rescaling applied before the connection rule. `FALSE`
  (default) leaves distances unchanged; `TRUE` or `"max"` divides by the
  maximum distance; `"minmax"` rescales to `[0, 1]`; `"quantile"`
  rescales by the 5th-95th percentile range, clamped to `[0, 1]`.

- seed:

  Optional seed used by stochastic discretizers.

## Value

A list-backed network object of class
`c("tsn", "netobject", "cograph_network")`. Its `$table` component is
the tidy dyad table; use
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) for that
table, `as.data.frame(x, what = "series")` for the canonical source
observations, and [`as.matrix()`](https://rdrr.io/r/base/matrix.html)
for the weighted adjacency matrix.

## References

Lacasa, L., Luque, B., Ballesteros, F., Luque, J., & Nuño, J. C. (2008).
From time series to complex networks: The visibility graph. *Proceedings
of the National Academy of Sciences*, 105(13), 4972-4975.
[doi:10.1073/pnas.0709247105](https://doi.org/10.1073/pnas.0709247105)

Luque, B., Lacasa, L., Ballesteros, F., & Luque, J. (2009). Horizontal
visibility graphs: Exact results for random time series. *Physical
Review E*, 80, 046103.
[doi:10.1103/PhysRevE.80.046103](https://doi.org/10.1103/PhysRevE.80.046103)

Quian Quiroga, R., Kreuz, T., & Grassberger, P. (2002). Event
synchronization: A simple and fast method to measure synchronicity and
time delay patterns. *Physical Review E*, 66, 041904.
[doi:10.1103/PhysRevE.66.041904](https://doi.org/10.1103/PhysRevE.66.041904)

## Examples

``` r
series <- list(
  first = c(1, 2, 3, 2, 1),
  second = c(1, 1, 2, 3, 5),
  third = c(5, 4, 3, 2, 1)
)

tsn(
  data = series,
  method = "distance",
  unit = "series",
  distance = "euclidean",
  connect = "full"
)
#> <tsn> distance series network: 3 nodes, 3 connected dyads
#>    from     to distance    weight connected   method   unit distance_method
#>   first second 4.358899 0.1866055      TRUE distance series       euclidean
#>   first  third 4.472136 0.1827440      TRUE distance series       euclidean
#>  second  third 6.557439 0.1323200      TRUE distance series       euclidean
#>  connection_method directed from_start from_end to_start to_end
#>               full    FALSE          1        5        1      5
#>               full    FALSE          1        5        1      5
#>               full    FALSE          1        5        1      5
#> Use plot(x) for the network or plot(x, "series") for the source series.
#> With cograph installed, splot(x) renders a publication-quality network.

# One data argument, one method string.
tsn(c(3, 1, 4, 2, 5), "hvg")
#> <tsn> visibility time network: 5 nodes, 6 connected dyads
#>        from         to distance weight connected     method unit
#>  series_1:1 series_1:2        1      1      TRUE visibility time
#>  series_1:1 series_1:3        2      1      TRUE visibility time
#>  series_1:2 series_1:3        1      1      TRUE visibility time
#>  series_1:3 series_1:4        1      1      TRUE visibility time
#>  series_1:3 series_1:5        2      1      TRUE visibility time
#>  series_1:4 series_1:5        1      1      TRUE visibility time
#>  distance_method connection_method directed from_start from_end to_start to_end
#>             <NA>        horizontal    FALSE          1        1        2      2
#>             <NA>        horizontal    FALSE          1        1        3      3
#>             <NA>        horizontal    FALSE          2        2        3      3
#>             <NA>        horizontal    FALSE          3        3        4      4
#>             <NA>        horizontal    FALSE          3        3        5      5
#>             <NA>        horizontal    FALSE          4        4        5      5
#> Use plot(x) for the network or plot(x, "series") for the source series.
#> With cograph installed, splot(x) renders a publication-quality network.
tsn(c(3, 1, 4, 2, 5, 3, 6, 2, 7), "ordinal")
#> <tsn> visibility state network: 3 nodes, 3 connected dyads
#>  from to distance weight connected     method  unit distance_method
#>     2  1 1.000000      4      TRUE visibility state            <NA>
#>     2  2 1.666667      6      TRUE visibility state            <NA>
#>     2  3 1.000000      2      TRUE visibility state            <NA>
#>  connection_method directed from_start from_end to_start to_end
#>            natural    FALSE         NA       NA       NA     NA
#>            natural    FALSE         NA       NA       NA     NA
#>            natural    FALSE         NA       NA       NA     NA
#> Use plot(x) for the network or plot(x, "series") for the source series.
#> With cograph installed, splot(x) renders a publication-quality network.
tsn(c(3, 1, 4, 2, 5, 3, 6, 2, 7), "distance")
#> <tsn> distance window network: 8 nodes, 28 connected dyads
#>         from          to distance    weight connected   method   unit
#>  series_1:W1 series_1:W2 3.605551 0.2171293      TRUE distance window
#>  series_1:W1 series_1:W3 1.414214 0.4142136      TRUE distance window
#>  series_1:W1 series_1:W4 4.123106 0.1951941      TRUE distance window
#>  series_1:W1 series_1:W5 2.828427 0.2612039      TRUE distance window
#>  series_1:W1 series_1:W6 5.000000 0.1666667      TRUE distance window
#>  series_1:W1 series_1:W7 3.162278 0.2402531      TRUE distance window
#>  series_1:W1 series_1:W8 6.082763 0.1411878      TRUE distance window
#>  series_1:W2 series_1:W3 3.605551 0.2171293      TRUE distance window
#>  series_1:W2 series_1:W4 1.414214 0.4142136      TRUE distance window
#>  series_1:W2 series_1:W5 4.123106 0.1951941      TRUE distance window
#>  distance_method connection_method directed from_start from_end to_start to_end
#>        euclidean              full    FALSE          1        2        2      3
#>        euclidean              full    FALSE          1        2        3      4
#>        euclidean              full    FALSE          1        2        4      5
#>        euclidean              full    FALSE          1        2        5      6
#>        euclidean              full    FALSE          1        2        6      7
#>        euclidean              full    FALSE          1        2        7      8
#>        euclidean              full    FALSE          1        2        8      9
#>        euclidean              full    FALSE          2        3        3      4
#>        euclidean              full    FALSE          2        3        4      5
#>        euclidean              full    FALSE          2        3        5      6
#> Use plot(x) for the network or plot(x, "series") for the source series.
#> With cograph installed, splot(x) renders a publication-quality network.

data(steps)
tsn(
  steps,
  value = "steps",
  id = "id",
  time = "day",
  series = 536,
  unit = "state",
  discretization = "gaussian"
)
#> <tsn> visibility state network: 3 nodes, 4 connected dyads
#>  from to  distance weight connected     method  unit distance_method
#>     2  2  6.641288    683      TRUE visibility state            <NA>
#>     2  1  1.746032     63      TRUE visibility state            <NA>
#>     2  3 26.483871     31      TRUE visibility state            <NA>
#>     1  1  1.571429      7      TRUE visibility state            <NA>
#>  connection_method directed from_start from_end to_start to_end
#>            natural    FALSE         NA       NA       NA     NA
#>            natural    FALSE         NA       NA       NA     NA
#>            natural    FALSE         NA       NA       NA     NA
#>            natural    FALSE         NA       NA       NA     NA
#> Use plot(x) for the network or plot(x, "series") for the source series.
#> With cograph installed, splot(x) renders a publication-quality network.
```
