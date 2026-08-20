# Classify Rolling Trends in a Time Series

`trend()` computes a rolling trend metric for each observation and
classifies every point as `"Ascending"`, `"Descending"`, `"Flat"`,
`"Turbulent"`, `"Missing Data"`, or `"Initial"`. It is a faithful base-R
port of the upstream `tsn::trend()` behaviour: rolling slope (OLS,
Theil-Sen, Spearman, or Kendall) or growth-factor metrics, an `epsilon`
flat band, and a volatility override that reclassifies noisy segments as
turbulent.

## Usage

``` r
trend(
  data,
  value = NULL,
  id = NULL,
  time = NULL,
  series = NULL,
  window = NULL,
  method = "slope",
  slope = "robust",
  epsilon = 0.05,
  turbulence_threshold = 5,
  flat_to_turbulent_factor = 1.5,
  align = "center"
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

- window:

  Rolling-window width. When `NULL`, the adaptive default
  `max(3, min(n, round(n / 10)))` is used, where `n` is the shortest
  series length. Must satisfy `2 < window < n`.

- method:

  Trend metric: `"slope"` (default) or `"growth_factor"`.

- slope:

  Slope estimator when `method = "slope"`: `"robust"` (Theil-Sen, the
  default), `"ols"`, `"spearman"`, or `"kendall"`.

- epsilon:

  Flat-band half-width. Default `0.05`.

- turbulence_threshold:

  Baseline volatility threshold for the turbulent override. Default `5`.

- flat_to_turbulent_factor:

  Multiplier applied to `turbulence_threshold` for points already
  classified as flat. Default `1.5`.

- align:

  Window alignment: `"center"` (default), `"right"`, or `"left"`. The
  metric is assigned to the centre, rightmost, or leftmost point of the
  window respectively.

## Value

A tidy data frame of class `tsn_trend` with one row per observation and
columns `id`, `time`, `value`, `metric`, and `state` (a factor over the
six trend classes). Print, summary, and plot methods are provided.

## Details

The metric is first thresholded with `epsilon`: values above `+epsilon`
(or above `1 + epsilon` for growth factors) are ascending, values below
`-epsilon` (or `1 - epsilon`) are descending, and the remainder are
flat. A rolling volatility measure (coefficient of variation plus half
the range factor of the metric) then overrides these labels with
`"Turbulent"` when it exceeds `turbulence_threshold`. Segments already
labelled `"Flat"` use the higher threshold
`turbulence_threshold * flat_to_turbulent_factor`, making them more
resistant to noise-driven reclassification.

## Examples

``` r
set.seed(123)
walk <- cumsum(rnorm(120))
trend(walk, window = 15, slope = "ols", epsilon = 0.1)
#> <tsn_trend> slope (ols), window 15: 120 observations across 1 series
#>        id time      value    metric     state
#>  series_1    1 -0.5604756        NA   Initial
#>  series_1    2 -0.7906531        NA   Initial
#>  series_1    3  0.7680552        NA   Initial
#>  series_1    4  0.8385636        NA   Initial
#>  series_1    5  0.9678513        NA   Initial
#>  series_1    6  2.6829163        NA   Initial
#>  series_1    7  3.1438325        NA   Initial
#>  series_1    8  1.8787713 0.1952863 Ascending
#>  series_1    9  1.1919184 0.1988546 Ascending
#>  series_1   10  0.7462564 0.1917457 Ascending

trend(c(1, 2, 3, 4, 5, 4, 3, 2, 1), window = 3, method = "growth_factor")
#> <tsn_trend> growth_factor (growth), window 3: 9 observations across 1 series
#>        id time value    metric      state
#>  series_1    1     1        NA    Initial
#>  series_1    2     2 3.0000000  Ascending
#>  series_1    3     3 2.0000000  Ascending
#>  series_1    4     4 1.6666667  Ascending
#>  series_1    5     5 1.0000000       Flat
#>  series_1    6     4 0.6000000 Descending
#>  series_1    7     3 0.5000000 Descending
#>  series_1    8     2 0.3333333 Descending
#>  series_1    9     1        NA    Initial
```
