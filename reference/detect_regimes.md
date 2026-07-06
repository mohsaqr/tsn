# Regime Detection for Time-Series

Detects regime changes in complexity data using multiple sophisticated
methods including cumulative peaks, changepoint detection, variance
shifts, threshold analysis, gradient changes, and entropy analysis.
Provides comprehensive analysis with stability classification and
pattern recognition.

## Usage

``` r
detect_regimes(
  data,
  method = "smart",
  sensitivity = "medium",
  min_change,
  window = 10,
  consolidate = FALSE,
  similarity = 0.6,
  peak = 1.96,
  cumulative = 0.6
)
```

## Arguments

- data:

  \[`tsn`, `ts`, `data.frame`,
  [`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  Time-series data

- method:

  \[`character(1)`\]  
  Detection method. The available options are:

  - `"cumulative_peaks"`: Detects cumulative complexity peaks using
    Z-tests.

  - `"changepoint"`: Change point detection (multi-window mean-shift
    test).

  - `"threshold"`: Adaptive quartile-based regime classification.

  - `"variance_shift"`: Detects changes in variance patterns.

  - `"slope"`: Detects changes in local slope (rolling linear models).

  - `"entropy"`: Detects changes in the Shannon entropy of the
    complexity series, calculated in rolling windows.

  - `"smart"` (default): Combines gradient, peaks, and changepoint
    methods.

  - `"all"`: Applies all individual methods listed above and uses
    ensemble voting.

- sensitivity:

  \[`character(1)`\]  
  Detection sensitivity level. The available options are: `"low"`,
  `"medium"`, `"high"`. The default is `"medium"`. This affects
  thresholds and window sizes within the detection methods.

- min_change:

  \[`integer(1)`\]  
  Minimum number of observations between changes. If missing (default),
  the value is determined automatically (typically 10% of observations,
  minimum of 10).

- window:

  \[`integer(1)`\]  
  base window size for rolling calculations. This is further adjusted by
  `sensitivity`. The default is `10`.

- consolidate:

  \[`logical(1)`\]  
  Should regimes that are statistically similar after initial detection
  be merged? The default is `FALSE`.

- similarity:

  \[`numeric(1)`\]  
  A value between 0 and 1 that defines the threshold for considering
  regimes similar enough to merge if `consolidate = TRUE`. Based on
  normalized mean difference. The default is `0.6`.

- peak:

  \[`numeric(1)`\]  
  Base Z-score threshold for individual peak detection with the
  `"cumulative_peaks"` method. Adjusted by `sensitivity`. The default is
  `1.96`.

- cumulative:

  \[`numeric(1)`\]  
  A value between 0 and 1 that defines the base proportion threshold for
  identifying cumulative peak regions. Adjusted by `sensitivity`. The
  default is `0.6`.

## Value

A `data.frame` containing all original columns from `data` and
additional columns with regime information:

`change`: Logical indicating regime change points `id`: An `integer`
regime identifier `stability`: Categorical stability: "Stable",
"Transitional", "Unstable" `complexity_pattern`: Pattern description:
"Increasing", "Decreasing", "Stable", "Fluctuating" `stability_score`:
Numeric stability score (0-1) `type`: Type of change detected by the
method `magnitude`: Magnitude of the change (method-specific
interpretation) `confidence`: Confidence in the detection
(method-specific interpretation, typically 0-1)

## Examples

``` r
comp <- c(
  rnorm(50, 0.3, 0.05), rnorm(50, 0.8, 0.08),
  rnorm(50, 0.5, 0.05), rnorm(50, 0.9, 0.1)
)

regimes_all <- detect_regimes(
  data = comp,
  method = "all",
  sensitivity = "medium"
)
#> Warning: All methods failed in "all" mode.
```
