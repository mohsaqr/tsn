# Coerce a TSN result to a weighted adjacency matrix

Coerce a TSN result to a weighted adjacency matrix

## Usage

``` r
# S3 method for class 'tsn'
as.matrix(x, rownames.force = NA, ...)
```

## Arguments

- x:

  A `tsn` result.

- rownames.force:

  Ignored.

- ...:

  Ignored.

## Value

A square numeric matrix with one row and one column per node, both named
by the node labels in node order. Unconnected pairs are 0, and an
undirected network gives a symmetric matrix.

## Examples

``` r
as.matrix(tsn(c(3, 1, 4, 1, 5), method = "visibility"))
#>            series_1:1 series_1:2 series_1:3 series_1:4 series_1:5
#> series_1:1          0          1          1          0          0
#> series_1:2          1          0          1          0          0
#> series_1:3          1          1          0          1          1
#> series_1:4          0          0          1          0          1
#> series_1:5          0          0          1          1          0
```
