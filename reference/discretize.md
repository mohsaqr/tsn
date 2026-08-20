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
  frame, or a `tsn` object (its source series are used).

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

  Number of states. Ignored by `"ordinal"`, whose state count follows
  the embedding arguments `m` and `tau`.

- breaks:

  Optional interior thresholds for `method = "threshold"`.

- labels:

  Optional custom state labels. Length must equal the number of states
  produced. When `NULL`, states are numbered consecutively.

- transform:

  Pre-discretization transform: `"none"` (default), `"log"` (uses
  `log1p(abs(x))`), or `"zscore"` (standardized values).

- m:

  Embedding dimension for `method = "ordinal"` (default `3`).

- tau:

  Embedding lag for `method = "ordinal"` (default `1`).

- seed:

  Optional seed used by stochastic discretizers.

## Value

A tidy data frame of class `tsn_states` with columns `id`, `time`,
`value`, `state` (a factor), and `probability`. The discretization model
and any bin boundaries are stored in the `model` and `breaks`
attributes.

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

discretize(c(10, 12, 8, 40, 44, 9), method = "width", transform = "log")
#> <tsn_states> width discretization (transform: log): 6 observations, 2 states
#>        id time value state probability
#>  series_1    1    10     1           1
#>  series_1    2    12     1           1
#>  series_1    3     8     1           1
#>  series_1    4    40     2           1
#>  series_1    5    44     2           1
#>  series_1    6     9     1           1
```
