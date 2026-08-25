# Summarize a TSN network

Summarize a TSN network

## Usage

``` r
# S3 method for class 'tsn'
summary(object, ...)
```

## Arguments

- object:

  A `tsn` result.

- ...:

  Ignored.

## Value

A one-row data frame with columns `method`, `unit`, `nodes`, `dyads`,
`edges`, `density`, `minimum_weight`, `maximum_weight`, and `directed`.
`density` divides the connected edges by the number of possible edges
for the network family: state networks (`unit = "state"`) always count
self-loops among the possible edges, because a state can follow itself,
while series, window, and time-point networks never do, because their
units cannot pair with themselves. The denominator therefore depends
only on the network type, never on which edges happen to be observed, so
densities are comparable across models of the same family.
