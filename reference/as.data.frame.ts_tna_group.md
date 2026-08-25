# Coerce a grouped transition network to a tidy data frame

A grouped model is a collection of networks, one per group. This method
is the tidy view of it: every table it returns carries a `group` column,
so a comparison across groups is a data frame you can read, sort, or
join rather than a set of objects you have to reach into one at a time.

## Usage

``` r
# S3 method for class 'ts_tna_group'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  what = c("edges", "series", "groups"),
  ...
)
```

## Arguments

- x:

  A `ts_tna_group` result from
  [`ts_tna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md) and
  friends, built with their `group` argument.

- row.names:

  Optional row names.

- optional:

  Ignored.

- what:

  Which table to return: `"edges"` (one row per group and state pair,
  the default), `"series"` (the per-observation source table for every
  group), or `"groups"` (a one-row-per-group index of how much data
  backs each network).

- ...:

  Ignored.

## Value

A base data frame whose first column is `group`.

## See also

[`ts_tna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md) for the
`group` argument that builds these models.

## Examples

``` r
data(esm_srl)
networks <- ts_tna(
  subset(esm_srl, !is.na(effort)),
  value = "effort", id = "name", time = "occasion",
  group = "day_type", labels = c("low", "mid", "high")
)
as.data.frame(networks, what = "groups")
#>     group type sequences observations states edges
#> 1 weekday  tna       238         2033      3     9
#> 2 weekend  tna       233          783      3     9
head(as.data.frame(networks))
#>     group from  to    weight
#> 1 weekday  low low 0.6199021
#> 2 weekday  mid low 0.2944162
#> 3 weekday high low 0.1150592
#> 4 weekday  low mid 0.2822186
#> 5 weekday  mid mid 0.4297800
#> 6 weekday high mid 0.2538071
```
