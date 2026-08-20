# Coerce a transition network to a weighted adjacency matrix

The state-to-state weight matrix behind a
[`ts_tna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md)-family
model: transition probabilities for
[`ts_tna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md), counts
for [`ts_ftna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md), and
so on. Without this method
[`as.matrix()`](https://rdrr.io/r/base/matrix.html) falls through to the
default, which coerces the object's component list rather than its
network.

## Usage

``` r
# S3 method for class 'ts_tna'
as.matrix(x, rownames.force = NA, ...)
```

## Arguments

- x:

  A `ts_tna` result.

- rownames.force:

  Ignored.

- ...:

  Ignored.

## Value

A named numeric matrix, states in row and column order.

## Examples

``` r
set.seed(1)
network <- ts_tna(cumsum(rnorm(60)), labels = c("low", "mid", "high"))
as.matrix(network)
#>       low       mid      high
#> low  0.80 0.2000000 0.0000000
#> mid  0.15 0.5500000 0.3000000
#> high 0.00 0.2631579 0.7368421
```
