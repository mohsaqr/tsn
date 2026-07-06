# Time Series Network Construction

Build networks from distance matrices using various construction methods

## Usage

``` r
build_network(
  data,
  measure = "euclidean",
  window = 1L,
  step = 1L,
  pairwise = TRUE,
  symmetric = TRUE,
  normalize = FALSE,
  method = "full",
  ...
)
```

## Arguments

- data:

  \[`tsn`, `ts`, `data.frame`,
  [`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  Time-series data.

- measure:

  \[`character(1)`\]  
  The distance measure to use.

- window:

  \[`integer(1)`\]  
  Width of the sliding window (default 1).

- step:

  \[`integer(1)`\]  
  Step size for windows (default 1)

- pairwise:

  \[`logical(1)`\]  
  If `TRUE` (default), calculates all pairwise distances between
  windows. If `FALSE`, uses consecutive windows only.

- symmetric:

  \[`logical(1)`\]  
  If `TRUE` (default), ensures that the distance matrix is symmetric. If
  `FALSE`, keeps edge directions when `pairwise = FALSE`.

- normalize:

  \[`logical(1)`\]  
  If `TRUE`, the distance matrix is max-normalized before network
  construction. If `FALSE`, uses the distances as is (default).

- method:

  \[`character(1)`\]  
  Network construction method The available options are:

  - `"full"`: Uses the raw similarity matrix (default).

  - `"knn"`: K-nearest neighbors network (KNN).

  - `"percentile"`: Percentile threshold network.

  - `"threshold"`: Distance threshold network.

  - `"gaussian"`: Gaussian kernel network.

- ...:

  Additional arguments passed to the network construction method. These
  include:

  - `k` Number of nearest neighbors (for KNN method)

  - `percentile` Percentile threshold (for percentile method)

  - `threshold` Distance threshold (for threshold method)

  - `sigma` Gaussian kernel parameter (for Gaussian method)

## Value

TODO

## Examples

``` r
set.seed(123)
x <- cumsum(rnorm(100))
net <- build_network(x, window = 10, step = 10)
```
