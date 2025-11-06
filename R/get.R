#' @title Getters for tbl_now attributes
#' @description
#' These functions extract metadata attributes from a `tbl_now` object.
#' Each function returns a specific attribute (e.g. event date, strata, covariates, etc.).
#'
#' @param x A `tbl_now` object.
#' @return The requested attribute (character, date, logical, etc.).
#' @name nowcast_data_getters
NULL

#' @rdname nowcast_data_getters
#' @export
get_event_date <- function(x) {
  attr(x, "event_date", exact = TRUE)
}

#' @rdname nowcast_data_getters
#' @export
get_report_date <- function(x) {
  attr(x, "report_date", exact = TRUE)
}

#' @rdname nowcast_data_getters
#' @export
get_strata <- function(x) {
  attr(x, "strata", exact = TRUE)
}

#' @rdname nowcast_data_getters
#' @export
get_num_strata <- function(x) {
  attr(x, "num_strata", exact = TRUE)
}

#' @rdname nowcast_data_getters
#' @export
get_covariates <- function(x) {
  attr(x, "covariates", exact = TRUE)
}

#' @rdname nowcast_data_getters
#' @export
get_num_covariates <- function(x) {
  attr(x, "num_covariates", exact = TRUE)
}

#' @rdname nowcast_data_getters
#' @export
get_now <- function(x) {
  attr(x, "now", exact = TRUE)
}

#' @rdname nowcast_data_getters
#' @export
get_date_units <- function(x) {
  attr(x, "date_units", exact = TRUE)
}

#' @rdname nowcast_data_getters
#' @export
get_data_type <- function(x) {
  attr(x, "data_type", exact = TRUE)
}

#' @rdname nowcast_data_getters
#' @export
get_is_batched <- function(x) {
  attr(x, "is_batched", exact = TRUE)
}
