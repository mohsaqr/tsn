# Print a TSN network

Print a TSN network

## Usage

``` r
# S3 method for class 'tsn'
print(x, ...)
```

## Arguments

- x:

  A `tsn` result.

- ...:

  Additional arguments passed to
  [`print.data.frame()`](https://rdrr.io/r/base/print.dataframe.html).

## Value

`x`, invisibly.

## Examples

``` r
tsn(c(3, 1, 4, 1, 5, 9, 2, 6), method = "visibility")
#> <tsn> visibility time network: 8 nodes, 12 connected dyads
#>        from         to distance weight connected     method unit
#>  series_1:1 series_1:2        1      1      TRUE visibility time
#>  series_1:1 series_1:3        2      1      TRUE visibility time
#>  series_1:1 series_1:6        5      1      TRUE visibility time
#>  series_1:2 series_1:3        1      1      TRUE visibility time
#>  series_1:3 series_1:4        1      1      TRUE visibility time
#>  series_1:3 series_1:5        2      1      TRUE visibility time
#>  series_1:3 series_1:6        3      1      TRUE visibility time
#>  series_1:4 series_1:5        1      1      TRUE visibility time
#>  series_1:5 series_1:6        1      1      TRUE visibility time
#>  series_1:6 series_1:7        1      1      TRUE visibility time
#>  distance_method connection_method directed from_start from_end to_start to_end
#>             <NA>           natural    FALSE          1        1        2      2
#>             <NA>           natural    FALSE          1        1        3      3
#>             <NA>           natural    FALSE          1        1        6      6
#>             <NA>           natural    FALSE          2        2        3      3
#>             <NA>           natural    FALSE          3        3        4      4
#>             <NA>           natural    FALSE          3        3        5      5
#>             <NA>           natural    FALSE          3        3        6      6
#>             <NA>           natural    FALSE          4        4        5      5
#>             <NA>           natural    FALSE          5        5        6      6
#>             <NA>           natural    FALSE          6        6        7      7
#> Showing 10 of 12 dyads; as.data.frame(x) returns the full table.
#> Use plot(x) for the network or plot(x, "series") for the source series.
#> With cograph installed, splot(x) renders a publication-quality network.
```
