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
data(motivation)
networks <- ts_tna(
  motivation,
  series = "pleasure", group = "task_context_type",
  labels = c("low", "mid", "high")
)
as.data.frame(networks, what = "groups")
#>      group type sequences observations states edges
#> 1     Home  tna       832         1324      3     9
#> 2    Other  tna         2            3      3     1
#> 3 Personal  tna       822         1309      3     9
#> 4     Work  tna       976         2235      3     9
head(as.data.frame(networks))
#>   group from  to    weight
#> 1  Home  low low 0.7616099
#> 2  Home  mid low 0.4366197
#> 3  Home high low 0.4814815
#> 4  Home  low mid 0.1981424
#> 5  Home  mid mid 0.4718310
#> 6  Home high mid 0.3703704
```
