# Compute a Visibility Graph from Time-Series Data

Creates visibility graphs from time series data, where nodes represent
time points and edges represent visibility between points according to
the selected method. The function supports both Natural Visibility
Graphs (NVG) and Horizontal Visibility Graphs (HVG), with options for
limited penetrable visibility and temporal decay.

## Usage

``` r
visibility_graph(
  data,
  method = "nvg",
  directed = FALSE,
  limit,
  penetrable = 0,
  decay_factor = 0
)
```

## Arguments

- data:

  \[`tsn`, `ts`, `data.frame`,
  [`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  Time-series data.

- method:

  \[`character(1)`\]  
  The visibility graph construction method. The options are `"nvg"` for
  natural visibility graphs and `"hvg"` for horizontal visibility
  graphs.

- directed:

  \[`logical(1)`\]  
  Should the graph be a directed graph? If `TRUE`, edges have direction
  showing temporal order. The default is `FALSE`.

- limit:

  \[`integer(1)`\]  
  Maximum temporal distance (in time steps) for visibility connections.
  If not provided (default), no limit is applied.

- penetrable:

  \[`integer(1)`\]  
  Number of points allowed to penetrate the visibility line for Limited
  Penetrable Visibility Graphs (LPVG). The default is 0 (standard
  visibility graph).

- decay_factor:

  \[`numeric(1)`\]  
  Temporal decay factor of edge weights. The default is 0 (no decay).
  Higher values cause faster decay with increasing temporal distance.

## Value

A `matrix` of edge weights.

## Details

### Visibility Graph Methods

**Natural Visibility Graph (NVG):** Two points can "see" each other if a
straight line connecting them doesn't intersect with any intermediate
points. This preserves more geometric information from the original time
series.

**Horizontal Visibility Graph (HVG):** Two points can "see" each other
if all intermediate points are strictly lower than both endpoints. This
creates sparser graphs that capture different temporal patterns.

### Advanced Features

**Limited Penetrable Visibility (LPVG):** Allows a specified number of
points to penetrate the visibility line, creating more connected graphs.

**Temporal Decay:** Edge weights decrease exponentially with temporal
distance, emphasizing local temporal relationships.

## References

Lacasa, L., Luque, B., Ballesteros, F., Luque, J., & Nuño, J. C. (2008).
From time series to complex networks: The visibility graph. *PNAS*,
**105(13)**, 4972-4975.

Luque, B., Lacasa, L., Ballesteros, F., & Luque, J. (2009). Horizontal
visibility graphs: Exact results for random time series. *Physical
Review E*, **80(4)**, 046103.

## Examples

``` r
set.seed(123)
ts_data <- rnorm(50, mean = 50, sd = 15)

# Create natural visibility graph
nvg <- visibility_graph(ts_data, method = "nvg")

# Create horizontal visibility graph with temporal limit
hvg <- visibility_graph(ts_data, method = "hvg", limit = 10)
```
