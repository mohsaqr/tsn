# Summarize a state discretization

Summarize a state discretization

## Usage

``` r
# S3 method for class 'tsn_states'
summary(object, ...)
```

## Arguments

- object:

  A `tsn_states` result.

- ...:

  Ignored.

## Value

A tidy data frame with one row per state giving the `count`,
`proportion`, and mean `value` of observations in that state.
