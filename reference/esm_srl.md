# Momentary Self-Regulated Learning Experience-Sampling Data

A fully anonymized intensive longitudinal data set in which 41 students
rated their momentary self-regulation, motivation, and anxiety several
times per day (47 to 79 occasions each, one or more per day). Every
indicator is on a 0-100 scale. The participant identifiers are fictional
names and the calendar dates have been shifted by a constant offset —
within-person spacing is preserved while no real dates or identities
remain. There are 41 missing indicator values in total.

## Usage

``` r
esm_srl
```

## Format

A data frame with 2,820 rows and 13 variables:

- name:

  Fictional participant identifier (41 unique students).

- occasion:

  Within-person occasion index, ordered in time.

- date:

  Anonymized (constant-shifted) assessment date.

- day_type:

  Factor with levels `"weekday"` and `"weekend"`, derived from `date`.

- efficacy:

  Momentary self-efficacy (motivation), 0-100.

- value:

  Momentary task value (motivation), 0-100.

- planning:

  Momentary planning (self-regulation), 0-100.

- monitoring:

  Momentary monitoring (self-regulation), 0-100.

- effort:

  Momentary effort regulation (self-regulation), 0-100.

- regulation:

  Momentary strategy regulation (self-regulation), 0-100.

- motivated:

  Momentary felt motivation (motivation), 0-100.

- enjoyment:

  Momentary enjoyment (motivation), 0-100.

- anxiety:

  Momentary anxiety, 0-100.

## Source

Package authors' own experience-sampling study, collected by the authors
and anonymized as described above (fictional identifiers,
constant-shifted dates). Distributed with the package under its license.

## Details

The same data set ships with the authors' 'idiographic' package; the
copy here additionally carries the `day_type` column, derived from
`date`, so grouped models (see the `group` argument of
[`ts_tna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md)) need no
caller-side preparation.

## Examples

``` r
data(esm_srl)
str(esm_srl)
#> 'data.frame':    2820 obs. of  13 variables:
#>  $ name      : chr  "Amara" "Amara" "Amara" "Amara" ...
#>  $ occasion  : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ date      : Date, format: "2026-06-27" "2026-06-28" ...
#>  $ day_type  : Factor w/ 2 levels "weekday","weekend": 2 2 2 1 1 1 1 1 1 1 ...
#>  $ efficacy  : num  13.1 90.2 67.2 52.5 83.6 ...
#>  $ value     : num  51.7 58.3 91.7 65 35 ...
#>  $ planning  : num  92 77 32.2 64.4 67.8 ...
#>  $ monitoring: num  84.9 49.3 89 83.6 23.3 ...
#>  $ effort    : num  84.1 81.2 44.9 100 50.7 ...
#>  $ regulation: num  58.1 33.8 33.8 43.2 85.1 ...
#>  $ motivated : num  20 52.9 58.8 29.4 41.2 ...
#>  $ enjoyment : num  33.3 33.3 34.7 31.9 62.5 ...
#>  $ anxiety   : num  100 94 98 100 100 93 96 100 98 94 ...
```
