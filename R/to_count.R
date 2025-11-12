#' Transform an object to count data
#'
#' @description
#' This is an S3 generic. This package provides methods for the
#' following classes:
#'
#' * `tbl_now`: takes a `tbl_now` object and creates a new column with
#' name `n` of counts of observations if `data_type = "linelist"`.
#'
#' @param x Data to be transformed from `linelist` to count data
#' @param ... Additional arguments
#' @examples
#' data(denguedat)
#' ndata <- tbl_now(denguedat,
#'     event_date = "onset_week",
#'     report_date = "report_week",
#'     strata = "gender")
#'
#' to_count(ndata)
#'
#' data("covidat")
#' ndata <- tbl_now(covidat,
#' event_date = "date_of_symptom_onset",
#'               report_date = "date_of_registry",
#'               strata = "sex")
#' to_count(ndata)
#'
#'
#' @export
to_count <- function(x, ...) {
  UseMethod("to_count")
}

#' @export
#' @rdname to_count
to_count.tbl_now <- function(x, ...) {

  #Ungroup just in case
  x <- x %>%
    ungroup()

  #Group by event_date and report_date
  # Group data to generate counts
  x <- x %>%
    group_by(dplyr::across(c(get_event_date(x), get_report_date(x), ".event_num", ".report_num")))

  # Group by delay censoring status first checking that it exists
  if (!is.null(get_is_batched(x))){
    x <- x  %>%
      group_by(dplyr::across(get_is_batched(x)), .add = TRUE)
  }

  # Group by strata first checking that strata exists
  if (get_num_strata(x) > 0) {
    x <- x  %>%
      group_by(dplyr::across(get_strata(x)), .add = TRUE)
  }

  # TODO: What happens with continuous covariates
  # Group by strata first checking that strata exists
  if (get_num_covariates(x) > 0) {
    x <- x  %>%
      group_by(dplyr::across(get_covariates(x)), .add = TRUE)
  }

  #In case it was count just group and sum
  if (get_data_type(x) == "count"){
    #Summarise
    x <- x  %>%
      summarise(!!as.symbol("n") := sum(!!as.symbol("n")), .groups = "drop")
  } else if  (get_data_type(x) == "linelist"){

    #Change the attribute first to avoid the warning from summarise
    attr(x, "data_type") <- "count"

    #Summarise
    x <- x %>%
      summarise(!!as.symbol("n") := dplyr::n(), .groups = "drop")

  } else {
    cli::cli_abort("`data_type` {get_data_type(x)} not implemented")
  }

  #Return the count
  return(x)

}
