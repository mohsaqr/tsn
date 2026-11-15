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
densities are comparable across models of the same family. Directed
series, window, and time-point networks orient every pair forward in
unit order, so only one of the `nodes * (nodes - 1)` ordered pairs per
dyad can ever be an edge and a complete directed network has density 0.5
(the
[`igraph::edge_density()`](https://r.igraph.org/reference/edge_density.html)
convention); compare such a network with its undirected counterpart
through `edges`, not `density`. `minimum_weight` and `maximum_weight`
summarize the connected edges only and are `NA` when no edge is
connected.

## Examples

``` r
summary(tsn(c(3, 1, 4, 1, 5, 9, 2, 6), method = "visibility"))
#>       method unit nodes dyads edges   density minimum_weight maximum_weight
#> 1 visibility time     8    12    12 0.4285714              1              1
#>   directed
#> 1    FALSE
```
