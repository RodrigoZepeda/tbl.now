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
#'
#' @param to Either `linelist`, `count-incidence` or `count-cumulative`
#' the resulting data-type to be created.
#'
#' @param ... Additional arguments
#'
#' @note `linelist` data cannot be reconstructed from `count-*` data. Tring
#' this will throw an error as you cannot un-count aggregated data.
#'
#' @examples
#' data(denguedat)
#' ndata <- tbl_now(denguedat,
#'     event_date = "onset_week",
#'     report_date = "report_week",
#'     strata = "gender")
#'
#' to_count(ndata, to = "count-incidence")
#'
#' data("covidat")
#' suppressWarnings({
#' ndata <- tbl_now(covidat,
#' event_date = "date_of_symptom_onset",
#'               report_date = "date_of_registry",
#'               strata = "sex")
#' to_count(ndata)
#' })
#'
#'
#' @export
to_count <- function(x, to = NULL, ...) {
  UseMethod("to_count")
}

#' @export
#' @rdname to_count
to_count.tbl_now <- function(x, to = NULL, ...) {

  #FIXME: If its cumulative transform to count-incidence or itsef
  #if its linelist move to count-incidence
  #if count-incidence move to count-incidence or count-cumulative

  #Ungroup just in case
  x <- x %>% ungroup()

  #Fill the nulls
  case_col <- get_case_col(x)
  if (is.null(case_col)) case_col <- "n"
  if (is.null(to)) to <- get_data_type(x)

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

  # Group by strata first checking that strata exists
  if (length(get_temporal_effects(x)) > 0) {
    x <- x  %>%
      group_by(dplyr::across(get_temporal_effects(x)), .add = TRUE)
  }

  # TODO: What happens with continuous covariates
  # Group by strata first checking that strata exists
  # if (get_num_covariates(x) > 0) {
  #   x <- x  %>%
  #     group_by(dplyr::across(get_covariates(x)), .add = TRUE)
  # }

  #In case it was count just group and sum
  if (get_data_type(x) == "count-incidence" & to == "count-incidence"){
    #Summarise
    x <- x  %>%
      summarise(!!as.symbol(case_col) := sum(!!as.symbol(case_col)), .groups = "drop")

  } else if  (get_data_type(x) == "linelist" & to == "count-incidence"){

    #Change the attribute first to avoid the warning from summarise
    attr(x, "data_type") <- "count-incidence"

    #Summarise
    x <- x %>%
      summarise(!!as.symbol(case_col) := dplyr::n(), .groups = "drop")

  } else if (get_data_type(x) == "linelist" & to == "count-cumulative"){

    #Change the attribute first to avoid the warning from summarise
    attr(x, "data_type") <- "count-cumulative"

    #Summarise
    x <- x %>%
      dplyr::arrange_at(c(get_event_date(x), get_report_date(x))) %>%
      summarise(!!as.symbol(case_col) := cumsum(dplyr::n()), .groups = "drop")

  } else if (get_data_type(x) == "count-incidence" & to == "count-cumulative"){

    #Change the attribute first to avoid the warning from summarise
    attr(x, "data_type") <- "count-cumulative"

    #Summarise
    x <- x  %>%
      dplyr::arrange_at(c(get_event_date(x), get_report_date(x))) %>%
      summarise(!!as.symbol(case_col) := cumsum(sum(!!as.symbol(case_col))), .groups = "drop")

  } else if (get_data_type(x) == "count-cumulative" & to == "count-cumulative"){

    x <- x %>%
      ungroup()

  } else if (get_data_type(x) == "linelist" & to == "linelist"){

    x <- x %>%
      ungroup()

  } else {
    cli::cli_abort("Transformation from `data_type` {get_data_type(x)} to {to} not implemented")
  }

  #Return the count
  return(x)

}
