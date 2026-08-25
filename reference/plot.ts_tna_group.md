# Plot One Network of a Grouped Transition Model

A grouped model holds one complete `ts_tna` network per group, and there
is no single picture of all of them, so one group is drawn at a time.
The selected network is rendered by
[`plot.ts_tna()`](https://pak.dynasite.org/tsn/reference/plot.ts_tna.md)
with its full argument surface — combined series-plus-network views,
state shading, ribbons, and every
[`cograph::splot()`](https://sonsoles.me/cograph/reference/splot.html)
option.

## Usage

``` r
# S3 method for class 'ts_tna_group'
plot(x, y = NULL, group = NULL, ...)
```

## Arguments

- x:

  A `ts_tna_group` result from
  [`ts_tna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md) and
  friends, built with their `group` argument.

- y:

  Ignored.

- group:

  Group label to draw. It may be omitted when the model holds exactly
  one group.

- ...:

  Passed to
  [`plot.ts_tna()`](https://pak.dynasite.org/tsn/reference/plot.ts_tna.md).

## Value

`x`, invisibly.

## See also

[`ts_tna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md) for the
`group` argument;
[`as.data.frame.ts_tna_group()`](https://pak.dynasite.org/tsn/reference/as.data.frame.ts_tna_group.md)
for the tidy views of the collection.

## Examples

``` r
data(esm_srl)
networks <- ts_tna(
  subset(esm_srl, !is.na(effort)),
  value = "effort", id = "name", time = "occasion",
  group = "day_type", labels = c("low", "mid", "high")
)
plot(networks, group = "weekday")

plot(networks, group = "weekend", type = "network")
```
