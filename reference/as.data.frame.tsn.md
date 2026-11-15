# Coerce a TSN result to a data frame

Coerce a TSN result to a data frame

## Usage

``` r
# S3 method for class 'tsn'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  what = c("edges", "series"),
  connected = FALSE,
  ...
)
```

## Arguments

- x:

  A `tsn` result.

- row.names:

  Optional row names for the returned table; `NULL` (the default)
  numbers the rows.

- optional:

  Ignored.

- what:

  Which table to return: `"edges"` (the stable dyad schema, the default)
  or `"series"` (the tidy source time series, including the discretized
  `state` column when the network uses states).

- connected:

  Return only the retained edges? The edge table lists every evaluated
  dyad; `TRUE` keeps the rows whose `connected` flag is set, i.e. the
  edges that survived the `connect` rule. Ignored when
  `what = "series"`.

- ...:

  Ignored.

## Value

A base data frame: the stable TSN dyad schema when `what = "edges"`, or
the source time series when `what = "series"`.

## Examples

``` r
network <- tsn(c(3, 1, 4, 1, 5, 9, 2, 6), method = "visibility")
as.data.frame(network, connected = TRUE)
#>          from         to distance weight connected     method unit
#> 1  series_1:1 series_1:2        1      1      TRUE visibility time
#> 2  series_1:1 series_1:3        2      1      TRUE visibility time
#> 3  series_1:1 series_1:6        5      1      TRUE visibility time
#> 4  series_1:2 series_1:3        1      1      TRUE visibility time
#> 5  series_1:3 series_1:4        1      1      TRUE visibility time
#> 6  series_1:3 series_1:5        2      1      TRUE visibility time
#> 7  series_1:3 series_1:6        3      1      TRUE visibility time
#> 8  series_1:4 series_1:5        1      1      TRUE visibility time
#> 9  series_1:5 series_1:6        1      1      TRUE visibility time
#> 10 series_1:6 series_1:7        1      1      TRUE visibility time
#> 11 series_1:6 series_1:8        2      1      TRUE visibility time
#> 12 series_1:7 series_1:8        1      1      TRUE visibility time
#>    distance_method connection_method directed from_start from_end to_start
#> 1             <NA>           natural    FALSE          1        1        2
#> 2             <NA>           natural    FALSE          1        1        3
#> 3             <NA>           natural    FALSE          1        1        6
#> 4             <NA>           natural    FALSE          2        2        3
#> 5             <NA>           natural    FALSE          3        3        4
#> 6             <NA>           natural    FALSE          3        3        5
#> 7             <NA>           natural    FALSE          3        3        6
#> 8             <NA>           natural    FALSE          4        4        5
#> 9             <NA>           natural    FALSE          5        5        6
#> 10            <NA>           natural    FALSE          6        6        7
#> 11            <NA>           natural    FALSE          6        6        8
#> 12            <NA>           natural    FALSE          7        7        8
#>    to_end
#> 1       2
#> 2       3
#> 3       6
#> 4       3
#> 5       4
#> 6       5
#> 7       6
#> 8       5
#> 9       6
#> 10      7
#> 11      8
#> 12      8
as.data.frame(network, what = "series")
#>         id time value
#> 1 series_1    1     3
#> 2 series_1    2     1
#> 3 series_1    3     4
#> 4 series_1    4     1
#> 5 series_1    5     5
#> 6 series_1    6     9
#> 7 series_1    7     2
#> 8 series_1    8     6
```
