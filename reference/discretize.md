# Convert Time-Series Data into Wide Format Sequence Data

Converts time-series data into sequence data via discretization. Various
methods for discretization are available including gaussian mixtures,
K-means clustering and kernel density based binning.

## Usage

``` r
discretize(
  data,
  n_states,
  labels = 1:n_states,
  method = "kmeans",
  unused_fn = dplyr::first,
  ...
)

# S3 method for class 'ts'
discretize(
  data,
  n_states,
  labels = 1:n_states,
  method = "kmeans",
  unused_fn = dplyr::first,
  ...
)

# Default S3 method
discretize(
  data,
  id_col,
  value_col,
  order_col,
  n_states,
  labels = 1:n_states,
  method = "kmeans",
  unused_fn = dplyr::first,
  ...
)
```

## Arguments

- data:

  \[`data.frame`, `ts`, `tsn`,
  [`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  Either time-series data in long format (`data.frame`, `tsn`), a
  time-series object (`ts`), or a vector of values.

- n_states:

  \[`integer(1)`\]  
  The number of states to discretize the data into.

- labels:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  A vector of names for the states. The length must be `n_states` The
  defaults is consecutive numbering, i.e. `1:n_states`.

- method:

  \[`character(1)`\]  
  The name of the discretization method to use.

  - `kmeans`: for K-means clustering (the default).

  - `width`: for equal width binning.

  - `quantile`: for quantile-based binning.

  - `kde`: for binning based on kernel density estimation.

  - `gaussian`: for a Gaussian mixture model.

- unused_fn:

  \[`function`\]  
  How to handle extra columns when pivoting to wide format. See
  [`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html).
  The default is to keep all columns and to use the first value.

- ...:

  Additional arguments passed to the discretization method
  ([`stats::kmeans()`](https://rdrr.io/r/stats/kmeans.html) for
  `kmeans`, [`stats::density()`](https://rdrr.io/r/stats/density.html)
  and `pracma::findpeaks()` for `kde`, and
  [`mclust::Mclust()`](https://mclust-org.github.io/mclust/reference/Mclust.html)
  for `gaussian`).

- id_col:

  \[`character(1)`\]  
  The name of the column that contains the unique identifiers.

- value_col:

  \[`character(1)`\]  
  The name of the column that contains the data values.

- order_col:

  \[`character(1)`\]  
  The name of the column that contains the time values (not required if
  the data is already in order),

## Value

TODO

## Examples

``` r
# Long format data
ts_data <- data.frame(
  id = gl(10, 100),
  series = c(
    replicate(
      10,
      stats::arima.sim(list(order = c(2, 1, 0), ar = c(0.5, 0.2)), n = 99)
    )
  )
)
data <- discretize(ts_data, "id", "series", n_states = 3)

# Time-series data
data <- discretize(EuStockMarkets, n_states = 3)
#> Error in data.frame(series = 1L, value = as.numeric(data), time = stats::time(data)): arguments imply differing number of rows: 1, 7440, 1860
```
