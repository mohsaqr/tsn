# Transition Network Analysis of Time Series

`ts_tna()`, `ts_ftna()`, `ts_cna()`, and `ts_atna()` bridge tsn's
discretization engine to the Nestimate package: a numeric time series
(or several) is discretized into states with any of tsn's discretizers,
each series becomes one state sequence, and Nestimate builds the
transition network — row-normalized probabilities (`ts_tna`), raw
transition counts (`ts_ftna`), co-occurrence counts (`ts_cna`), or
attention-weighted transitions (`ts_atna`).

## Usage

``` r
ts_tna(
  data,
  value = NULL,
  id = NULL,
  time = NULL,
  series = NULL,
  discretization = "quantile",
  n_states = 3L,
  breaks = NULL,
  labels = NULL,
  transform = "none",
  m = NULL,
  tau = NULL,
  seed = NULL,
  ...
)

ts_ftna(
  data,
  value = NULL,
  id = NULL,
  time = NULL,
  series = NULL,
  discretization = "quantile",
  n_states = 3L,
  breaks = NULL,
  labels = NULL,
  transform = "none",
  m = NULL,
  tau = NULL,
  seed = NULL,
  ...
)

ts_cna(
  data,
  value = NULL,
  id = NULL,
  time = NULL,
  series = NULL,
  discretization = "quantile",
  n_states = 3L,
  breaks = NULL,
  labels = NULL,
  transform = "none",
  m = NULL,
  tau = NULL,
  seed = NULL,
  ...
)

ts_atna(
  data,
  value = NULL,
  id = NULL,
  time = NULL,
  series = NULL,
  discretization = "quantile",
  n_states = 3L,
  breaks = NULL,
  labels = NULL,
  transform = "none",
  m = NULL,
  tau = NULL,
  seed = NULL,
  ...
)
```

## Arguments

- data:

  A numeric vector, `ts`, matrix, named list of numeric vectors, data
  frame, or a `tsn_states` result from
  [`discretize()`](https://pak.dynasite.org/tsn/reference/discretize.md).

- value:

  Optional value-column name for long data.

- id:

  Optional series-ID column name for long data.

- time:

  Optional time-column name for long data.

- series:

  Optional series IDs or wide-data column names to select.

- discretization:

  Discretization method passed to
  [`discretize()`](https://pak.dynasite.org/tsn/reference/discretize.md)
  (default `"quantile"`). Ignored when `data` is already a `tsn_states`.

- n_states:

  Number of states (default `3`).

- breaks:

  Optional interior thresholds for `discretization = "threshold"`.

- labels:

  Optional state labels; these become the network's node names (e.g.
  `c("low", "mid", "high")`).

- transform:

  Pre-discretization transform: `"none"`, `"log"`, or `"zscore"`.

- m, tau:

  Embedding arguments for `discretization = "ordinal"`.

- seed:

  Optional seed used by stochastic discretizers.

- ...:

  Passed on to the corresponding Nestimate builder
  ([`Nestimate::build_tna()`](https://saqr.me/Nestimate/reference/build_tna.html)
  and friends), e.g. `start`, `end`, `scaling`, `threshold`.

## Value

A Nestimate `netobject` of class
`c("ts_tna", "netobject", "cograph_network")` with the additional fields
`$ts_source` (tidy `id`/`time`/`value`/`state` table) and `$meta$tsn`
(discretization settings).

## Details

The result is a full Nestimate `netobject` (also a `cograph_network`),
so compatible Nestimate descriptive and inferential verbs apply and
[`cograph::splot()`](https://sonsoles.me/cograph/reference/splot.html)
renders it directly. Inference still requires the sample its method
assumes: for example, sequence bootstrap is degenerate for a single
sequence. The object keeps its data: `$data` holds the wide state
sequences Nestimate built from, `$ts_source` the tidy per-observation
table (`id`, `time`, `value`, `state`), and `$meta$tsn` the
discretization and builder settings, so the network remains traceable
back to the raw series.

Multiple series are supported through every tsn input form (named list,
matrix, wide or long data frame). Scalar discretizers learn from pooled
values. Temporal discretizers compute patterns or windows separately
within each series before assigning one shared state alphabet. Each
series contributes one sequence. Passing an existing
[`discretize()`](https://pak.dynasite.org/tsn/reference/discretize.md)
result skips discretization and uses its states as-is.

## Examples

``` r
set.seed(1)
series <- list(
  a = cumsum(rnorm(60)),
  b = cumsum(rnorm(60)),
  c = cumsum(rnorm(60))
)
network <- ts_tna(series, n_states = 3, labels = c("low", "mid", "high"))
network$weights
#>            low        mid      high
#> low  0.9322034 0.06779661 0.0000000
#> mid  0.0500000 0.81666667 0.1333333
#> high 0.0000000 0.10344828 0.8965517

# Frequency counts instead of probabilities:
counts <- ts_ftna(series, n_states = 3)

# Reuse an existing discretization:
states <- discretize(series, method = "kmeans", n_states = 3)
ts_tna(states)
#> Transition Network (relative probabilities) [directed]
#>   Weights: [0.013, 0.976]  |  mean: 0.429
#> 
#>   Weight matrix:
#>         1     2     3
#>   1 0.976 0.024 0.000
#>   2 0.013 0.882 0.105
#>   3 0.000 0.100 0.900 
#> 
#>   Initial probabilities:
#>   2             1.000  ████████████████████████████████████████
#>   1             0.000  
#>   3             0.000  
```
