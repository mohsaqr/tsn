# Print a state discretization

Prints a one-line header (method, transform, observation count, and the
realized state count, with the requested count when fewer states were
realized) followed by the first ten rows.

## Usage

``` r
# S3 method for class 'tsn_states'
print(x, ...)
```

## Arguments

- x:

  A `tsn_states` result.

- ...:

  Passed to
  [`print.data.frame()`](https://rdrr.io/r/base/print.dataframe.html).

## Value

`x`, invisibly.
