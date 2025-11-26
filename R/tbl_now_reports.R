#' Get the latest/first reported cases for each event date
#'
#' Function that gets the latest (respectively first) number of cases that has
#' been reported for each `event_date`
#'
#' @param x A `tbl_now` object
#'
#' @examples
#' data(denguedat)
#' dengue <- tbl_now(denguedat,
#'                   report_date = "report_week",
#'                   event_date = "onset_week",
#'                   strata = "gender",
#'                   verbose = FALSE)
#'
#' dengue <- to_count(dengue)
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

#FIXME: In my framework delay corresponds to when they arrive but
#in the flusight framework delay corresponds to a new estimate

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
    dplyr::group_by_at(c(get_event_date(x), get_is_batched(x), get_covariates(x), get_strata(x), get_temporal_effects(x))) %>%
    dplyr::filter(!!as.symbol(get_report_date(x)) == max(!!as.symbol(get_report_date(x)), na.rm = TRUE)) %>%
    ungroup()

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
    dplyr::group_by_at(c(get_event_date(x), get_is_batched(x), get_covariates(x), get_strata(x), get_temporal_effects(x))) %>%
    dplyr::filter(!!as.symbol(get_report_date(x)) == min(!!as.symbol(get_report_date(x)), na.rm = TRUE)) %>%
    ungroup()

}


