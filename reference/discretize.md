# Discretize a Time Series into States

`discretize()` exposes every internal state discretizer as a first-class
verb. It accepts the same inputs as
[`tsn()`](https://pak.dynasite.org/tsn/reference/tsn.md) (plus a `tsn`
object) and returns a tidy one-row-per-observation table of discretized
states. [`tsn()`](https://pak.dynasite.org/tsn/reference/tsn.md) routes
its own state aggregation through the same engine, so
`tsn(x, "<method>")` and `discretize(x, method = "<method>")` produce
identical state assignments. Scalar methods learn one shared state space
from all selected values. Temporal methods (`"ordinal"`,
`"adaptive_magnitude"`, and `"dtw"`) first compute patterns or windows
within each series, never across an ID boundary, and then use a shared
alphabet or clustering model.

## Usage

``` r
discretize(
  data,
  value = NULL,
  id = NULL,
  time = NULL,
  series = NULL,
  method = "quantile",
  n_states = 3L,
  breaks = NULL,
  labels = NULL,
  transform = "none",
  m = NULL,
  tau = NULL,
  seed = NULL
)
```

## Arguments

- data:

  A numeric vector, `ts`, matrix, named list of numeric vectors, data
  frame, or a `tsn` object (its source series are used, so `value`,
  `id`, and `time` cannot be supplied with it). Missing or non-finite
  values are an error rather than being dropped or kept as an `NA`
  state; remove them first, for example with
  [`subset()`](https://rdrr.io/r/base/subset.html).

- value:

  Optional value-column name for long data.

- id:

  Optional series-ID column name for long data.

- time:

  Optional time-column name for long data.

- series:

  Optional series IDs or wide-data column names to select.

- method:

  Discretization method. One of `"threshold"`, `"width"`, `"quantile"`,
  `"kde"`, `"kmeans"`, `"gaussian"`, `"hclust"`, `"ordinal"`,
  `"symbolic"`, `"change_points"`, `"entropy"`, `"magnitude"`,
  `"adaptive_magnitude"`, `"percentile_magnitude"`, or `"dtw"`.

- n_states:

  Number of states requested. Not consumed by `"ordinal"`, whose state
  count follows the embedding arguments `m` and `tau`; supplying both is
  an error. Bins that receive no observation are dropped (with a
  `tsn_state_shortfall` warning), so the realized count (reported in the
  printed header) can be smaller, for example with `"threshold"` breaks
  that leave a bin empty, an empty middle `"width"` bin, or a `"dtw"`
  cluster that wins no observation's vote.

- breaks:

  Interior thresholds for `method = "threshold"`, which requires them:
  finite values strictly inside the data range. When `n_states` is not
  supplied it is set to `length(breaks) + 1`; an explicit `n_states`
  must equal that. An error with any other method, which computes its
  own boundaries.

- labels:

  Optional custom state labels: unique, non-missing, non-empty strings,
  one per realized state (in increasing state order). When `NULL`,
  states are numbered consecutively.

- transform:

  Pre-discretization transform: `"none"` (default), `"log"` (uses
  `log1p(abs(x))`), or `"zscore"` (standardized values).

- m:

  Embedding dimension for `method = "ordinal"` (default `3`). An error
  with any other method.

- tau:

  Embedding lag for `method = "ordinal"` (default `1`). An error with
  any other method.

- seed:

  Optional seed for the stochastic discretizers (`"kmeans"` and
  `"gaussian"`). An error with the deterministic methods.

## Value

A tidy data frame of class `tsn_states`, one row per observation, with
columns `id` (character), `time`, `value`, `state` (a factor whose
levels are the realized states in increasing order), and `probability`
(the assignment confidence; `1` for hard assignments). Rows are grouped
by series in order of first appearance and, when `time` is supplied,
sorted by it within each series. `time` is the supplied time column for
long data with `time` and the series time for a `ts`; otherwise it
numbers the observations within each series (long data with `id` only,
named lists) or is the row index (vectors, matrices, wide data frames).
The discretization model and any bin boundaries are stored in the
`model` and `breaks` attributes.

## References

Bandt, C., & Pompe, B. (2002). Permutation entropy: A natural complexity
measure for time series. *Physical Review Letters*, 88, 174102.
[doi:10.1103/PhysRevLett.88.174102](https://doi.org/10.1103/PhysRevLett.88.174102)

## Examples

``` r
discretize(c(1, 5, 2, 9, 3, 7, 4, 8), method = "quantile", n_states = 3)
#> <tsn_states> quantile discretization (transform: none): 8 observations, 3 states
#>        id time value state probability
#>  series_1    1     1     1           1
#>  series_1    2     5     2           1
#>  series_1    3     2     1           1
#>  series_1    4     9     3           1
#>  series_1    5     3     1           1
#>  series_1    6     7     3           1
#>  series_1    7     4     2           1
#>  series_1    8     8     3           1

discretize(
  c(1, 5, 2, 9, 3, 7, 4, 8),
  method = "quantile",
  n_states = 3,
  labels = c("low", "mid", "high")
)
#> <tsn_states> quantile discretization (transform: none): 8 observations, 3 states
#>        id time value state probability
#>  series_1    1     1   low           1
#>  series_1    2     5   mid           1
#>  series_1    3     2   low           1
#>  series_1    4     9  high           1
#>  series_1    5     3   low           1
#>  series_1    6     7  high           1
#>  series_1    7     4   mid           1
#>  series_1    8     8  high           1

discretize(c(3, 1, 4, 1, 5, 9, 2, 6), method = "ordinal", m = 3)
#> <tsn_states> ordinal discretization (transform: none): 8 observations, 5 states
#>        id time value state probability
#>  series_1    1     3     3           1
#>  series_1    2     1     2           1
#>  series_1    3     4     3           1
#>  series_1    4     1     1           1
#>  series_1    5     5     4           1
#>  series_1    6     9     5           1
#>  series_1    7     2     5           1
#>  series_1    8     6     5           1

discretize(c(10, 12, 8, 25, 40, 44, 9), method = "width", transform = "log")
#> <tsn_states> width discretization (transform: log): 7 observations, 3 states
#>        id time value state probability
#>  series_1    1    10     1           1
#>  series_1    2    12     1           1
#>  series_1    3     8     1           1
#>  series_1    4    25     2           1
#>  series_1    5    40     3           1
#>  series_1    6    44     3           1
#>  series_1    7     9     1           1
```
