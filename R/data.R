#' Daily Self-Regulated Learning Panel
#'
#' A balanced intensive longitudinal panel in which 36 students reported nine
#' self-regulated-learning indicators once per study occasion for 156
#' occasions. Every indicator is on a 0-100 scale, and `day` is a
#' within-person occasion index, so the panel feeds [tsn()], [discretize()],
#' and the [ts_tna()] family directly as long data. There are 131 missing
#' indicator values in total.
#'
#' The same data set ships with the authors' 'idiographic' package; the copy
#' here is identical.
#'
#' @format A data frame with 5,616 rows and 11 variables:
#' \describe{
#'   \item{name}{Student identifier (36 unique students).}
#'   \item{day}{Within-person occasion index, 1 through 156.}
#'   \item{efficacy}{Self-efficacy, 0-100.}
#'   \item{value}{Task value, 0-100.}
#'   \item{planning}{Planning, 0-100.}
#'   \item{monitoring}{Monitoring, 0-100.}
#'   \item{effort}{Effort regulation, 0-100.}
#'   \item{control}{Control of learning, 0-100.}
#'   \item{help}{Help seeking, 0-100.}
#'   \item{social}{Social support, 0-100.}
#'   \item{organizing}{Organizing, 0-100.}
#' }
#' @source Companion data of Saqr, M., & López-Pernas, S. (Eds.),
#'   *Advanced Learning Analytics Methods: AI, Precision and Complexity*
#'   (\url{https://github.com/lamethods/data2/tree/main/srl}), created and
#'   owned by the package authors and distributed under CC BY-NC-SA 4.0
#'   (\url{https://creativecommons.org/licenses/by-nc-sa/4.0/}). Rows are
#'   ordered by `name` and `day`; values are unchanged. See the package
#'   `COPYRIGHTS` file for full attribution.
"srl"

#' Momentary Self-Regulated Learning Experience-Sampling Data
#'
#' A fully anonymized intensive longitudinal data set in which 41 students
#' rated their momentary self-regulation, motivation, and anxiety several
#' times per day (47 to 79 occasions each, one or more per day). Every
#' indicator is on a 0-100 scale. The participant identifiers are fictional
#' names and the calendar dates have been shifted by a constant offset —
#' within-person spacing is preserved while no real dates or identities
#' remain. There are 41 missing indicator values in total.
#'
#' The same data set ships with the authors' 'idiographic' package; the copy
#' here additionally carries the `day_type` column, derived from `date`, so
#' grouped models (see the `group` argument of [ts_tna()]) need no
#' caller-side preparation.
#'
#' @format A data frame with 2,820 rows and 13 variables:
#' \describe{
#'   \item{name}{Fictional participant identifier (41 unique students).}
#'   \item{occasion}{Within-person occasion index, ordered in time.}
#'   \item{date}{Anonymized (constant-shifted) assessment date.}
#'   \item{day_type}{Factor with levels `"weekday"` and `"weekend"`, derived
#'     from `date`.}
#'   \item{efficacy}{Momentary self-efficacy (motivation), 0-100.}
#'   \item{value}{Momentary task value (motivation), 0-100.}
#'   \item{planning}{Momentary planning (self-regulation), 0-100.}
#'   \item{monitoring}{Momentary monitoring (self-regulation), 0-100.}
#'   \item{effort}{Momentary effort regulation (self-regulation), 0-100.}
#'   \item{regulation}{Momentary strategy regulation (self-regulation),
#'     0-100.}
#'   \item{motivated}{Momentary felt motivation (motivation), 0-100.}
#'   \item{enjoyment}{Momentary enjoyment (motivation), 0-100.}
#'   \item{anxiety}{Momentary anxiety, 0-100.}
#' }
#' @source Package authors' own experience-sampling study, collected by the
#'   authors and anonymized as described above (fictional identifiers,
#'   constant-shifted dates). Distributed with the package under its
#'   license.
"esm_srl"
