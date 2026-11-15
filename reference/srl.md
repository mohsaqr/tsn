# Daily Self-Regulated Learning Panel

A balanced intensive longitudinal panel in which 36 students reported
nine self-regulated-learning indicators once per study occasion for 156
occasions. Every indicator is on a 0-100 scale, and `day` is a
within-person occasion index, so the panel feeds
[`tsn()`](https://pak.dynasite.org/tsn/reference/tsn.md),
[`discretize()`](https://pak.dynasite.org/tsn/reference/discretize.md),
and the [`ts_tna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md)
family directly as long data. There are 131 missing indicator values in
total.

## Usage

``` r
srl
```

## Format

A data frame with 5,616 rows and 11 variables:

- name:

  Student identifier (36 unique students).

- day:

  Within-person occasion index, 1 through 156.

- efficacy:

  Self-efficacy, 0-100.

- value:

  Task value, 0-100.

- planning:

  Planning, 0-100.

- monitoring:

  Monitoring, 0-100.

- effort:

  Effort regulation, 0-100.

- control:

  Control of learning, 0-100.

- help:

  Help seeking, 0-100.

- social:

  Social support, 0-100.

- organizing:

  Organizing, 0-100.

## Source

Companion data of Saqr, M., & López-Pernas, S. (Eds.), *Advanced
Learning Analytics Methods: AI, Precision and Complexity*
(<https://github.com/lamethods/data2/tree/main/srl>), created and owned
by the package authors and distributed under CC BY-NC-SA 4.0
(<https://creativecommons.org/licenses/by-nc-sa/4.0/>). Rows are ordered
by `name` and `day`; values are unchanged. See the package `COPYRIGHTS`
file for full attribution.

## Details

The same data set ships with the authors' 'idiographic' package; the
copy here is identical.

## Examples

``` r
data(srl)
str(srl)
#> 'data.frame':    5616 obs. of  11 variables:
#>  $ name      : chr  "Aisha" "Aisha" "Aisha" "Aisha" ...
#>  $ day       : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ efficacy  : num  38.2 14.7 67.6 55.9 55.9 ...
#>  $ value     : num  58.3 47.2 52.8 63.9 36.1 ...
#>  $ planning  : num  0 50 52.3 65.9 52.3 ...
#>  $ monitoring: num  34.21 7.89 19.74 22.37 22.37 ...
#>  $ effort    : num  54 79.4 77.8 93.7 71.4 ...
#>  $ control   : num  39.4 45.5 39.4 42.4 57.6 ...
#>  $ help      : num  78 16 28 24 48 28 16 38 60 40 ...
#>  $ social    : num  60 10 55 47.5 57.5 62.5 45 27.5 52.5 37.5 ...
#>  $ organizing: num  1.61 61.29 77.42 37.1 75.81 ...
```
