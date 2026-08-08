#' Daily Step Counts
#'
#' An example longitudinal data set containing daily step counts for 151
#' individuals from 2019-04-16 through 2020-03-01. Each `(id, day)` pair is
#' unique. There are 3,046 missing step counts.
#'
#' @format A data frame with 34,089 rows and three variables:
#' \describe{
#'   \item{id}{Integer individual identifier.}
#'   \item{day}{Calendar date stored as `YYYY-MM-DD` text.}
#'   \item{steps}{Daily step count, with some missing values.}
#' }
#' @source Package authors' example data.
"steps"

#' Repeated Motivation Measurements
#'
#' An example intensive-longitudinal data set containing repeated motivation,
#' context, and mood measurements from 2018-09-29 through 2021-07-12. Each
#' `(day, beep_number)` pair is unique and no values are missing.
#'
#' @format A data frame with 4,871 rows and 13 variables:
#' \describe{
#'   \item{autonomy, competence, relatedness}{Integer motivation measures.}
#'   \item{pleasure, interest, importance}{Integer appraisal measures.}
#'   \item{situation_requires, anxiety_guilt_avoidance, another_wants}{Integer
#'     context and motivation measures.}
#'   \item{mood}{Integer mood measure.}
#'   \item{task_context_type}{Character context: `"Home"`, `"Other"`,
#'     `"Personal"`, or `"Work"`.}
#'   \item{day}{Calendar date stored as `YYYY-MM-DD` text.}
#'   \item{beep_number}{Integer within-day measurement number, 1 through 7.}
#' }
#' @source Package authors' example data.
"motivation"
