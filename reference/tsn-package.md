# tsn: Time-Series Network Construction

`tsn` constructs distance, visibility, and state-transition networks
from time-series data.
[`tsn()`](https://pak.dynasite.org/tsn/reference/tsn.md) is the core
network constructor;
[`vg()`](https://pak.dynasite.org/tsn/reference/vg.md),
[`discretize()`](https://pak.dynasite.org/tsn/reference/discretize.md),
[`trend()`](https://pak.dynasite.org/tsn/reference/trend.md), and the
[`ts_tna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md) family
([`ts_tna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md),
[`ts_ftna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md),
[`ts_cna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md),
[`ts_atna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md)) provide
focused workflows, and
[`series_networks()`](https://pak.dynasite.org/tsn/reference/series_networks.md)
splits a pooled transition model into one network per series. Every
result is read through [`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html),
[`plot()`](https://rdrr.io/r/graphics/plot.default.html), and the
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) /
[`as.matrix()`](https://rdrr.io/r/base/matrix.html) accessors. Core
construction uses only packages distributed with R, while
transition-network models require the suggested Nestimate package. Two
example data sets ship with the package:
[srl](https://pak.dynasite.org/tsn/reference/srl.md) and
[esm_srl](https://pak.dynasite.org/tsn/reference/esm_srl.md).

## See also

Useful links:

- <https://github.com/mohsaqr/tsn/>

- <https://pak.dynasite.org/tsn/>

- Report bugs at <https://github.com/mohsaqr/tsn/issues/>

## Author

**Maintainer**: Mohammed Saqr <saqr@saqr.me> \[copyright holder\]

Authors:

- Sonsoles López-Pernas \[copyright holder\]

- Manuel J. Gomez (University of Murcia)
