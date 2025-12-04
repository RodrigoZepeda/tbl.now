#' Transform into a `tbl_now` object
#'
#' Takes a `data.frame` or `tbl_now` and transforms it into a `tbl_now` object
#'
#' @param object An object to convert to `tbl_now`.
#' @inheritParams tbl_now
#' @param ... Additional parameters to pass to [tbl_now()]
#'
#' @examples
#' #Convert a data.frame to tbl_now
#' data(denguedat)
#' as_tbl_now(denguedat, event_date = "onset_week", report_date = "report_week")
#'
#' @md
#' @export
as_tbl_now <- function(object, event_date, report_date, ...){
  UseMethod("as_tbl_now")
}

#' @export
#' @rdname as_tbl_now
as_tbl_now.tbl_now <- function(object, event_date, report_date, ...) {

  object %>%
    change_event_date({{ event_date }}) %>%
    change_report_date({{ report_date }})

}

#' @export
#' @rdname as_tbl_now
as_tbl_now.data.frame <- function(object, event_date, report_date, ...) {

  tbl_now(object, {{ event_date }}, {{ report_date }}, ...)

}
