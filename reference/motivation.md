# Repeated Motivation Measurements

An example intensive-longitudinal data set containing repeated
motivation, context, and mood measurements from 2018-09-29 through
2021-07-12. Each `(day, beep_number)` pair is unique and no values are
missing.

## Usage

``` r
motivation
```

## Format

A data frame with 4,871 rows and 13 variables:

- autonomy, competence, relatedness:

  Integer motivation measures.

- pleasure, interest, importance:

  Integer appraisal measures.

- situation_requires, anxiety_guilt_avoidance, another_wants:

  Integer context and motivation measures.

- mood:

  Integer mood measure.

- task_context_type:

  Character context: `"Home"`, `"Other"`, `"Personal"`, or `"Work"`.

- day:

  Calendar date stored as `YYYY-MM-DD` text.

- beep_number:

  Integer within-day measurement number, 1 through 7.

## Source

Package authors' example data.
