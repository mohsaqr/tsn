# Coerce a transition network to a tidy edge table

Returns one row per state pair, so a transition network can be read,
sorted, or joined as data rather than reached into.
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) on the
underlying Nestimate object errors, so this method supplies the tidy
view.

## Usage

``` r
# S3 method for class 'ts_tna'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  what = c("edges", "series"),
  ...
)
```

## Arguments

- x:

  A `ts_tna` result.

- row.names:

  Optional row names.

- optional:

  Ignored.

- what:

  Which table to return: `"edges"` (one row per state pair, the default)
  or `"series"` (the tidy per-observation source table with `id`,
  `time`, `value`, and `state`).

- ...:

  Ignored.

## Value

A base data frame.

## Examples

``` r
set.seed(1)
network <- ts_tna(cumsum(rnorm(60)), labels = c("low", "mid", "high"))
as.data.frame(network)
#>   from   to    weight
#> 1  low  low 0.8000000
#> 2  mid  low 0.1500000
#> 3 high  low 0.0000000
#> 4  low  mid 0.2000000
#> 5  mid  mid 0.5500000
#> 6 high  mid 0.2631579
#> 7  low high 0.0000000
#> 8  mid high 0.3000000
#> 9 high high 0.7368421
head(as.data.frame(network, what = "series"))
#>         id time      value state
#> 1 series_1    1 -0.6264538   low
#> 2 series_1    2 -0.4428105   low
#> 3 series_1    3 -1.2784391   low
#> 4 series_1    4  0.3168417   low
#> 5 series_1    5  0.6463495   low
#> 6 series_1    6 -0.1741189   low
```
