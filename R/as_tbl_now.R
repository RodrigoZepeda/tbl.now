#' Transform an object into a `tbl_now`
#'
#' @description  `r lifecycle::badge("experimental")`
#'
#' Convert a supported object into a [tbl_now]. For a plain `data.frame` /
#' `data.table` (or an existing `tbl_now`) you supply the `event_date` and
#' `report_date` columns yourself. For objects produced by other packages the
#' conversion is delegated to the matching `tbl_now_from_*()` converter, which
#' already knows how to map that format -- so those methods do **not** take
#' `event_date` / `report_date`.
#'
#' @details
#' Package-specific inputs forward to a dedicated converter. **See that
#' converter** for the extra arguments it accepts (e.g. `strata`, `max_delay`,
#' `format`, `delays_unit`, ...), for which columns are carried over, and for
#' the transformation notes / round-trip caveats in its *Round-trip* section:
#'
#' * `enw_preprocess_data` (or a fitted `epinowcast` object) ->
#'   [tbl_now_from_epinowcast()]
#' * `reporting_triangle` (\pkg{baselinenowcast}) ->
#'   [tbl_now_from_baselinenowcast()]
#' * `epidist_linelist_data` / `epidist_aggregate_data` (\pkg{epidist}) ->
#'   [tbl_now_from_epidist()]
#' * `tbl_ts` (\pkg{tsibble}) -> [tbl_now_from_tsibble()]
#' * `data.table` -> [tbl_now_from_data_table()]
#'
#' Anything passed through `...` is forwarded to the underlying converter (and
#' on to [tbl_now()]), so options such as `event_units`, `now` or `verbose` can
#' be supplied here too.
#'
#' @param object An object to convert to a `tbl_now`.
#' @param event_date,report_date For `data.frame` / `data.table` / `tbl_ts`
#'   (and `tbl_now`) inputs, the [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
#'   event- and report-date columns. They are **not** arguments of the
#'   package-conversion methods (epinowcast, baselinenowcast, epidist), which
#'   carry their own date mapping.
#' @param ... Additional arguments forwarded to the relevant `tbl_now_from_*()`
#'   converter (and therefore to [tbl_now()]).
#'
#' @return A `tbl_now` object.
#'
#' @seealso [tbl_now_from_epinowcast()], [tbl_now_from_baselinenowcast()],
#'   [tbl_now_from_epidist()], [tbl_now_from_tsibble()],
#'   [tbl_now_from_data_table()]
#'
#' @examples
#' # Convert a data.frame to tbl_now
#' data(denguedat)
#' as_tbl_now(denguedat, event_date = "onset_week", report_date = "report_week")
#'
#' @md
#' @export
as_tbl_now <- function(object, ...) {
  UseMethod("as_tbl_now")
}

#' @export
#' @rdname as_tbl_now
as_tbl_now.tbl_now <- function(object, event_date, report_date, ...) {
  object |>
    change_event_date({{ event_date }}) |>
    change_report_date({{ report_date }})
}

#' @export
#' @rdname as_tbl_now
as_tbl_now.data.frame <- function(object, event_date, report_date, ...) {
  tbl_now(object, {{ event_date }}, {{ report_date }}, ...)
}

#' @rdname as_tbl_now
#' @export
as_tbl_now.enw_preprocess_data <- function(object, ...) {
  # Date mapping, strata detection and caveats: see tbl_now_from_epinowcast().
  tbl_now_from_epinowcast(object, ...)
}

#' @rdname as_tbl_now
#' @export
as_tbl_now.reporting_triangle <- function(object, ...) {
  # Date mapping and NA/round-trip caveats: see tbl_now_from_baselinenowcast().
  tbl_now_from_baselinenowcast(object, ...)
}

#' @rdname as_tbl_now
#' @export
as_tbl_now.epidist_linelist_data <- function(object, ...) {
  # Date mapping and censoring/units caveats: see tbl_now_from_epidist().
  tbl_now_from_epidist(object, ...)
}

#' @rdname as_tbl_now
#' @export
as_tbl_now.epidist_aggregate_data <- function(object, ...) {
  # The aggregate form (counts with an `n` column) is detected inside
  # tbl_now_from_epidist() and mapped to data_type = "count-incidence".
  tbl_now_from_epidist(object, ...)
}

#' @rdname as_tbl_now
#' @export
as_tbl_now.tbl_ts <- function(object, report_date, event_date = NULL, ...) {
  if (missing(report_date)) {
    cli::cli_abort(
      "Please supply the {.arg report_date} column name for a {.cls tbl_ts} \\
       ({.arg event_date} defaults to the tsibble index)."
    )
  }
  tbl_now_from_tsibble(
    object, report_date = report_date, event_date = event_date, ...
  )
}

#' @rdname as_tbl_now
#' @export
as_tbl_now.data.table <- function(object, event_date, report_date, ...) {
  if (missing(event_date) || missing(report_date)) {
    cli::cli_abort(
      "Please supply {.arg event_date} and {.arg report_date} for a \\
       {.cls data.table}."
    )
  }
  tbl_now_from_data_table(
    object, event_date = event_date, report_date = report_date, ...
  )
}
