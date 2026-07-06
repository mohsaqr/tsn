# Compute trend classification based on various metrics

Calculates rolling metrics for a time series and classifies trends as
ascending, descending, flat, or turbulent.

## Usage

``` r
trend(
  data,
  window,
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

  \[`tsn`, `ts`, `data.frame`,
  [`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  Time-series data.

- window:

  \[`integer(1)`\]  
  The window size for metric calculation. If missing, uses the following
  adaptive sizing: `max(3, min(length(data), round(length(data)/10)))`.

- method:

  \[`character(1)`\]  
  The method for trend calculation. The available options ares:
  `"slope"` (default) and `"growth_factor"`.

- slope:

  \[`character(1)`\]  
  The method for slope calculation for `method = "slope"`. The available
  options are: `"ols"` (ordinary least squares), `"robust"` (Theil-Sen
  estimator), `"spearman"` (Spearman rank correlation based), and
  `"kendall"` (Kendall's tau based). Default: `"robust"`.

- epsilon:

  \[`numeric(1)`\]  
  A threshold value for defining flat trends based on the metric value.
  For `method = "slope"`, values between `(-epsilon, +epsilon)` are
  considered flat. For `"growth_factor"`, values between `(1-epsilon)`
  and `(1+epsilon)` are considered flat. Default: `0.05`.

- turbulence_threshold:

  \[`numeric(1)`\]  
  The baseline threshold value for classifying a segment as "turbulent".
  Based on a custom combined volatility metric (CV + 0.5 \* range factor
  of rolling metric values). Default: 5.

- flat_to_turbulent_factor:

  \[`numeric(1)`\]  
  A multiplier for `turbulence_threshold` when assessing if an already
  "flat" segment should be reclassified as "turbulent". A value \> 1
  makes "flat" trends more resistant to becoming "turbulent". Default:
  1.5.

- align:

  \[`character(1)`\]  
  Alignment of the window. The available options are: `"center"`
  (default), `"right"`, and `"left"`. The calculated metric is assigned
  to the center, rightmost, or leftmost point of the window,
  respectively.

## Value

A `tsn` object with an added `trend` column for the `timeseries` data
with the following classes: `"ascending"`, `"descending"`, `"flat"`,
`"turbulent"`, `"Missing_Data"`, or `"Initial"`.

## Details

Computes rolling metrics. Trend classifications ("ascending",
"descending", "flat") are first determined using `epsilon`. Then, a
"turbulent" classification can override these if the rolling volatility
of the metric exceeds a dynamically adjusted turbulence threshold. For
segments initially classified as "flat", this threshold is
`turbulence_threshold * flat_to_turbulent_factor`, making them more
stable against reclassification as "turbulent" due to minor noise.

## Examples

``` r
set.seed(123)
x <- cumsum(rnorm(200)) # Longer series to see more varied trends
# Using a slightly larger epsilon to catch more "flat" regions
tr <- trend(
  x, window = 15, method = "slope",
  slope = "ols", epsilon = 0.1,
  turbulence_threshold = 5, flat_to_turbulent_factor = 2
)
```
