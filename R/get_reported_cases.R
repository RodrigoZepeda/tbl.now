#' Get the latest/first reported cases for each event date
#'
#' @description `r lifecycle::badge('stable')`
#'
#' Function that gets the latest (respectively initially observed) number of cases that has
#' been reported for each `event_date`
#'
#' @param x A `tbl.now` object
#'
#' @returns A `tbl.now` containing the following columns:
#' * `event_date` The date the event happened. Its numerical version is `.event_num`.
#' * `report_date` The date of the latest report for events happening on `event_date`. Its numerical version is `.report_num`.
#' * `n` The total number of events happening at `event_date`
#' * `.delay` The maximum delay observed for that `event_date`
#' * Other columns that include the strata or the censoring indicators and the temporal effects for that event.
#'
#' @examples
#' data(denguedat)
#' dengue <- tbl_now(denguedat,
#'                   report_date = "report_week",
#'                   event_date = "onset_week",
#'                   strata = "gender",
#'                   verbose = FALSE)
#'
#' #Gets the first reported cases (what as initially thought of to be the incidence)
#' get_initial_reported_cases(dengue)
#'
#' #Gets the latest reported cases (what is now thought of to be the incidence)
#' get_latest_reported_cases(dengue)
#'
#'
#' @name get_latest_first
#' @export


#' @rdname get_latest_first
#' @export
get_latest_reported_cases <- function(x){

  if (!inherits(x, "tbl_now")){
    cli::cli_abort(
      "Object x is not a `tbl_now`"
    )
  }

  x %>%
    ungroup() %>%
    to_count(to = "count-cumulative") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(get_event_date(x), get_is_censored(x), get_covariates(x), get_strata(x), get_temporal_effect_cols(x))))) %>%
    dplyr::filter(!!as.symbol(get_report_date(x)) == max(!!as.symbol(get_report_date(x)), na.rm = TRUE)) %>%
    ungroup() %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(c(get_event_date(x), get_strata(x), get_is_censored(x), get_covariates(x)))))

}

#' @rdname get_latest_first
#' @export
get_initial_reported_cases <- function(x){

  if (!inherits(x, "tbl_now")){
    cli::cli_abort(
      "Object x is not a `tbl_now`"
    )
  }

  x %>%
    ungroup() %>%
    to_count(to = "count-cumulative") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(get_event_date(x), get_is_censored(x), get_covariates(x), get_strata(x), get_temporal_effect_cols(x))))) %>%
    dplyr::filter(!!as.symbol(get_report_date(x)) == min(!!as.symbol(get_report_date(x)), na.rm = TRUE)) %>%
    ungroup() %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(c(get_event_date(x), get_strata(x), get_is_censored(x), get_covariates(x)))))

}


