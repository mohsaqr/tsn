# Package index

## Package

- [`tsn-package`](https://pak.dynasite.org/tsn/reference/tsn-package.md)
  : tsn: Time-Series Network Construction

## Network construction

Build distance, visibility, and state-transition networks from one or
many univariate time series.

- [`tsn()`](https://pak.dynasite.org/tsn/reference/tsn.md) : Build a
  Time-Series Network
- [`vg()`](https://pak.dynasite.org/tsn/reference/vg.md) : Build a
  Visibility Graph

## Discretization and trends

Turn continuous measurements into states, and classify the direction of
change between consecutive observations.

- [`discretize()`](https://pak.dynasite.org/tsn/reference/discretize.md)
  : Discretize a Time Series into States
- [`trend()`](https://pak.dynasite.org/tsn/reference/trend.md) :
  Classify Rolling Trends in a Time Series

## Nestimate bridge

Discretize a series and hand it to the matching Nestimate
transition-network model, keeping the source data attached.

- [`ts_tna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md)
  [`ts_ftna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md)
  [`ts_cna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md)
  [`ts_atna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md) :
  Transition Network Analysis of Time Series
- [`series_networks()`](https://pak.dynasite.org/tsn/reference/series_networks.md)
  : Per-Series Transition Networks

## Methods

Standard interface shared by every result object.

- [`as.data.frame(`*`<tsn>`*`)`](https://pak.dynasite.org/tsn/reference/as.data.frame.tsn.md)
  : Coerce a TSN result to a data frame
- [`as.data.frame(`*`<ts_tna>`*`)`](https://pak.dynasite.org/tsn/reference/as.data.frame.ts_tna.md)
  : Coerce a transition network to a tidy edge table
- [`as.data.frame(`*`<ts_tna_group>`*`)`](https://pak.dynasite.org/tsn/reference/as.data.frame.ts_tna_group.md)
  : Coerce a grouped transition network to a tidy data frame
- [`as.matrix(`*`<tsn>`*`)`](https://pak.dynasite.org/tsn/reference/as.matrix.tsn.md)
  : Coerce a TSN result to a weighted adjacency matrix
- [`as.matrix(`*`<ts_tna>`*`)`](https://pak.dynasite.org/tsn/reference/as.matrix.ts_tna.md)
  : Coerce a transition network to a weighted adjacency matrix
- [`plot(`*`<tsn>`*`)`](https://pak.dynasite.org/tsn/reference/plot.tsn.md)
  : Plot a TSN Result
- [`plot(`*`<tsn_series_networks>`*`)`](https://pak.dynasite.org/tsn/reference/plot.tsn_series_networks.md)
  : Plot One Per-Series Transition Network
- [`plot(`*`<tsn_states>`*`)`](https://pak.dynasite.org/tsn/reference/plot.tsn_states.md)
  : Plot a state discretization
- [`plot(`*`<tsn_trend>`*`)`](https://pak.dynasite.org/tsn/reference/plot.tsn_trend.md)
  : Plot a trend classification
- [`plot(`*`<ts_tna>`*`)`](https://pak.dynasite.org/tsn/reference/plot.ts_tna.md)
  : Plot a Time-Series Transition Network
- [`print(`*`<tsn>`*`)`](https://pak.dynasite.org/tsn/reference/print.tsn.md)
  : Print a TSN network
- [`print(`*`<tsn_states>`*`)`](https://pak.dynasite.org/tsn/reference/print.tsn_states.md)
  : Print a state discretization
- [`print(`*`<tsn_trend>`*`)`](https://pak.dynasite.org/tsn/reference/print.tsn_trend.md)
  : Print a trend classification
- [`summary(`*`<tsn>`*`)`](https://pak.dynasite.org/tsn/reference/summary.tsn.md)
  : Summarize a TSN network
- [`summary(`*`<tsn_series_networks>`*`)`](https://pak.dynasite.org/tsn/reference/summary.tsn_series_networks.md)
  [`as.data.frame(`*`<tsn_series_networks>`*`)`](https://pak.dynasite.org/tsn/reference/summary.tsn_series_networks.md)
  [`print(`*`<tsn_series_networks>`*`)`](https://pak.dynasite.org/tsn/reference/summary.tsn_series_networks.md)
  : Summarize Per-Series Transition Networks
- [`summary(`*`<tsn_states>`*`)`](https://pak.dynasite.org/tsn/reference/summary.tsn_states.md)
  : Summarize a state discretization
- [`summary(`*`<tsn_trend>`*`)`](https://pak.dynasite.org/tsn/reference/summary.tsn_trend.md)
  : Summarize a trend classification

## Data

- [`motivation`](https://pak.dynasite.org/tsn/reference/motivation.md) :
  Repeated Motivation Measurements
- [`steps`](https://pak.dynasite.org/tsn/reference/steps.md) : Daily
  Step Counts
