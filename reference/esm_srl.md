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
