# Per-Series Transition Networks

Split a pooled
[`ts_tna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md)-family
model into one full model per source series. Each element is rebuilt
with the same network type and the shared state alphabet
(`params = list(alphabet = ...)`), so every network has the same node
set in the same order — directly comparable with each other and with the
pooled summary model. Each element is a complete `ts_tna` netobject: it
keeps its own series data, plots with
[`plot.ts_tna()`](https://pak.dynasite.org/tsn/reference/plot.ts_tna.md),
and works with compatible Nestimate verbs. Statistical procedures still
require enough independent sequences for their sampling design; a
one-series split is descriptive rather than bootstrap-ready.

## Usage

``` r
series_networks(x, series = NULL)
```

## Arguments

- x:

  A `ts_tna` result from
  [`ts_tna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md),
  [`ts_ftna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md),
  [`ts_cna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md), or
  [`ts_atna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md).

- series:

  Optional series IDs to keep (default: all).

## Value

A named `tsn_series_networks` collection containing one `ts_tna` object
per series. Its print, summary, and data-frame methods return a tidy
one-row-per-series index;
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) draws the sole
model or a model selected with its `series` argument.

## Examples

``` r
set.seed(1)
series <- list(a = cumsum(rnorm(80)), b = cumsum(rnorm(80)))
pooled <- ts_tna(series, n_states = 3, labels = c("low", "mid", "high"))
networks <- series_networks(pooled)
plot(networks, series = "a")
```
