# Summarize Per-Series Transition Networks

Summarize Per-Series Transition Networks

## Usage

``` r
# S3 method for class 'tsn_series_networks'
summary(object, ...)

# S3 method for class 'tsn_series_networks'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'tsn_series_networks'
print(x, ...)
```

## Arguments

- object:

  A `tsn_series_networks` result from
  [`series_networks()`](https://pak.dynasite.org/tsn/reference/series_networks.md).

- ...:

  Ignored.

- x:

  A `tsn_series_networks` result.

- row.names, optional:

  Ignored.

## Value

A tidy data frame with one row per series.
