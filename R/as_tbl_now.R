#' Transform into a `tbl_now` object
#'
#' @description  `r lifecycle::badge("experimental")`
#'
#' Takes an object  and transforms it into a `tbl_now`.
#'
#' @param object An object to convert to `tbl_now`.
#' @inheritParams tbl_now
#' @param ... Additional parameters to pass to [tbl_now()]
#'
#' @return A `tbl_now` object.
#'
#' @examples
#' # Convert a data.frame to tbl_now
#' data(denguedat)
#' as_tbl_now(denguedat, event_date = "onset_week", report_date = "report_week")
#'
#' @md
#' @export
as_tbl_now <- function(object, event_date, report_date, ...) {
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
as_tbl_now.enw_preprocess_data <- function(object, event_date, report_date, ...) {
  # tbl_now_from_epinowcast() already handles preprocessed objects directly.
  tbl_now_from_epinowcast(object, ...)
}

#' @rdname as_tbl_now
#' @export
as_tbl_now.reporting_triangle <- function(object, event_date, report_date, ...) {
  tbl_now_from_baselinenowcast(object, ...)
}

#' @rdname as_tbl_now
#' @export
as_tbl_now.epidist_linelist_data <- function(object, event_date, report_date, ...) {
  tbl_now_from_epidist(object, ...)
}

#' @rdname as_tbl_now
#' @export
as_tbl_now.tbl_ts <- function(object, event_date, report_date, ...) {
  rp <- if (missing(report_date)) NULL else report_date
  if (missing(event_date)) {
    cli::cli_abort("Please supply the {.arg event_date} column name for a {.cls tbl_ts}.")
  }
  tbl_now_from_tsibble(object, event_date = event_date, report_date = rp, ...)
}

#' @rdname as_tbl_now
#' @export
as_tbl_now.data.table <- function(object, event_date, report_date, ...) {
  if (missing(event_date) || missing(report_date)) {
    cli::cli_abort("Please supply {.arg event_date} and {.arg report_date} for a {.cls data.table}.")
  }
  tbl_now_from_data_table(object, event_date = event_date, report_date = report_date, ...)
}

