# Plot Time-Series Data with State Frequencies

Plot Time-Series Data with State Frequencies

## Usage

``` r
plot_series(
  data,
  selected,
  overlay = "v",
  points = FALSE,
  ncol = NULL,
  max_series = 10,
  trend = FALSE,
  scales = c("free", "free_x", "free_y", "fixed")
)
```

## Arguments

- data:

  \[`tsn`, `ts`, `data.frame`,
  [`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  Time-series data to be plotted.

- selected:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  A vector of indices or names of individual time-series to plot. If not
  provided (default), all time-series are plotted up to `max_series`
  number of plots.

- overlay:

  \[`logical(1)`\]  
  An option for plotting the overlay that indicates the state assigned
  to each time point. Can be either `"h"` for a horizontal overlay or
  `"v"` for a vertical overlay (default). If `NULL`, no overlay is
  plotted.

- points:

  \[`logical(1)`\]  
  Should a point be added for each observation? Defaults to `FALSE`. The
  points are colored according to the assigned state.

- ncol:

  An `integer` giving the number of columns to use for the facets.

- max_series:

  \[`integer(1)`\]  
  The maximum number of time-series to plot. The default is 10.

- trend:

  \[`logical(1)`\]  
  Should are trend line be added to the plot? The default is `FALSE` for
  no trend line.

- scales:

  \[`character(1)`\]  
  Any of `"fixed"`, `"free_x"`, `"free_y"`, or `"free"` (default).

## Value

A `ggplot` object.

## Examples

``` r
ts_data <- data.frame(
  id = gl(10, 100),
  time = rep(1:100,10),
  series = c(
    replicate(
      10,
      stats::arima.sim(list(order = c(2, 1, 0), ar = c(0.5, 0.2)), n = 99)
    )
  )
)

ts_data_disc <- discretize(ts_data, "id", "series", "time", n_states = 5)
plot_series(ts_data_disc)
#> Error in loadNamespace(x): there is no package called ‘ggplot2’
```
