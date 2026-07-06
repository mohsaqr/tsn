# Calculate Dynamic Complexity Measures for Time-Series Data

Computes dynamic complexity and other rolling window measures for
univariate or multivariate time series data. Supports various measures
including complexity, fluctuation, distribution, autocorrelation, and
basic statistics.

## Usage

``` r
complexity(data, measures = "complexity", window = 7L, align = "center")
```

## Arguments

- data:

  \[`tsn`, `ts`, `data.frame`,
  [`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  Time-series data.

- measures:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  A vector of measures to calculate. The available options are:
  `"complexity"`, `"fluctuation"`, `"distribution"`,
  `"autocorrelation"`, `"max"`, `"min"`, `"variance"`, `"all"`. The
  default is "complexity". See 'Details' for more information on these
  measures.

- window:

  \[`integer(1)`\]  
  A positive integer specifying the rolling window size. Must be at
  least 2. The default is 7.

- align:

  \[`character(1)`\]  
  Alignment of the window. The available options are: `"center"`
  (default), `"right"`, and `"left"`. The calculated measure is assigned
  to the center, rightmost, or leftmost point of the window,
  respectively.

## Value

A `data.frame` with the time index, the original time-series data, and
the calculated measures.

## Details

The following measures can be calculated:

- `complexity`: Product of fluctuation and distribution measures.

- `fluctuation`: Root mean square of successive differences.

- `distribution`: Deviation from uniform distribution.

- `autocorrelation`: Lag-1 autocorrelation coefficient.

- `max`: Rolling maximum.

- `min`: Rolling minimum.

- `variance`: Rolling variance.

The option `"all"` computes all of the above.

## Examples

``` r
# Basic complexity calculation
set.seed(123)
ts_data <- rnorm(100)
result <- complexity(ts_data, measures = "complexity")
#> Error in complexity(ts_data, measures = "complexity"): object 'fluctuation_degree' not found

# Multiple measures
result_multi <- complexity(ts_data, measures = c("complexity", "variance"))
#> Error in complexity(ts_data, measures = c("complexity", "variance")): object 'fluctuation_degree' not found
```
