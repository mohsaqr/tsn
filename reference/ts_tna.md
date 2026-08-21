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
  segment = NULL,
  overlap = FALSE,
  group = NULL,
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
  segment = NULL,
  overlap = FALSE,
  group = NULL,
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
  segment = NULL,
  overlap = FALSE,
  group = NULL,
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
  segment = NULL,
  overlap = FALSE,
  group = NULL,
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

- segment:

  Optional block width, in observations, used to cut each series into
  several shorter sequences. Sequence-based inference resamples whole
  sequences, so a single long series offers nothing to resample;
  segmenting supplies the units. Blocks never span an ID boundary, and
  segmentation is applied *after* discretization, so the state alphabet
  is still learned from the whole series. At least `2`.

- overlap:

  When segmenting, slide the block one observation at a time instead of
  partitioning (default `FALSE`). Partitioning loses the transition at
  every cut, roughly one per block. Sliding keeps every transition — at
  `segment = 2` the blocks are the consecutive lag-1 pairs and the
  network is identical to the unsegmented one — but the blocks share
  observations, so they are not independent and intervals computed from
  them run narrow. Partition for conservative intervals that tolerate
  dependence beyond one lag; slide to preserve the estimate exactly.

- group:

  Optional name of a column of `data` holding each observation's group,
  e.g. a condition, a cohort, or a context. Supplying it returns one
  network per group instead of a single pooled network. States are
  discretized from the pooled series first, so every group is cut on one
  common scale and their networks share a node set and are directly
  comparable. A sequence never spans a group boundary: where a series
  stays in one group it contributes a single sequence (unless `segment`
  splits it further), and where the group column alternates within a
  series each contiguous run becomes its own sequence, so no transition
  is ever counted between observations that were not adjacent in time.
  The transition across a group change is dropped from both groups: it
  belongs to neither.

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
(discretization settings). With `group`, a
`c("ts_tna_group", "netobject_group")` collection holding one such
network per group, which Nestimate's compatible grouped verbs
(`net_prune()`, `state_distribution()`, `net_centrality()`,
`compare_model()`, `permutation()`) accept directly and
[as.data.frame()](https://pak.dynasite.org/tsn/reference/as.data.frame.ts_tna_group.md)
renders as a tidy table.

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

A single series supports every descriptive verb but no sequence-based
test, because a bootstrap resamples sequences and one series is one
sequence. The `segment` argument cuts a long series into blocks so that
those tests have units to work with; see its documentation for the
trade-off between partitioned and sliding blocks.

The `group` argument splits the model instead of pooling it: one network
per condition, cohort, or context, all cut from one shared state
alphabet so they can be compared. The result is a Nestimate
`netobject_group`, so the grouped Nestimate verbs that suit a transition
model apply to it directly. Verbs needing a precision matrix or a
clustering attribute do not, and there is no grouped
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) method.

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

# Cut one long series into blocks so sequence-based tests have units:
long <- cumsum(rnorm(300))
ts_tna(long, segment = 10, labels = c("low", "mid", "high"))
#> Transition Network (relative probabilities) [directed]
#>   Weights: [0.078, 0.922]  |  mean: 0.429
#> 
#>   Weight matrix:
#>          low   mid  high
#>   low  0.868 0.132 0.000
#>   mid  0.124 0.775 0.101
#>   high 0.000 0.078 0.922 
#> 
#>   Initial probabilities:
#>   mid           0.400  ████████████████████████████████████████
#>   low           0.333  █████████████████████████████████
#>   high          0.267  ███████████████████████████

# Sliding lag-1 pairs keep every transition and the exact estimate:
ts_tna(long, segment = 2, overlap = TRUE, labels = c("low", "mid", "high"))
#> Transition Network (relative probabilities) [directed]
#>   Weights: [0.090, 0.910]  |  mean: 0.429
#> 
#>   Weight matrix:
#>          low   mid  high
#>   low  0.880 0.120 0.000
#>   mid  0.121 0.788 0.091
#>   high 0.000 0.090 0.910 
#> 
#>   Initial probabilities:
#>   low           0.334  ████████████████████████████████████████
#>   high          0.334  ████████████████████████████████████████
#>   mid           0.331  ████████████████████████████████████████

# One network per context, on a shared alphabet:
data(motivation)
by_context <- ts_tna(
  motivation,
  series = "pleasure", group = "task_context_type",
  labels = c("low", "mid", "high")
)
as.data.frame(by_context, what = "groups")
#>      group type sequences observations states edges
#> 1     Home  tna       832         1324      3     9
#> 2    Other  tna         2            3      3     1
#> 3 Personal  tna       822         1309      3     9
#> 4     Work  tna       976         2235      3     9
```
