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

  Optional row names.

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
