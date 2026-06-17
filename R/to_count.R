#' Transform an object to count data
#'
#' @description `r lifecycle::badge("stable")`
#' Transforms a `tbl.now` from:
#' * `linelist`: to `count-incidence` or `count-cumulative`
#' * `count-incidence`: to  `count-cumulative`
#'
#' @details
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
#' @note `linelist` data cannot be reconstructed from `count-*` data. Trying
#' this will throw an error as you cannot un-count aggregated data.
#'
#' @return A `tbl_now` object of the requested `to` data type, with the counts
#' aggregated into the `case_count` column.
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

  #Ungroup just in case
  x <- x %>% ungroup()

  #Fill the nulls
  case_count <- get_case_count(x)
  if (is.null(case_count)) case_count <- "n"
  if (is.null(to)) to <- get_data_type(x)

  #Create the grouping vector
  gp_vec <- c(get_event_date(x),
              get_report_date(x),
              ".event_num",
              ".report_num",
              get_is_censored(x),
              get_strata(x),
              get_temporal_effect_cols(x),
              get_covariates(x))

  #Group by event_date and report_date
  # Group data to generate counts
  x <- x %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(gp_vec)))


  #In case it was count just group and sum
  if (get_data_type(x) == "count-incidence" & to == "count-incidence"){

    #Summarise
    x <- x  %>%
      summarise(!!as.symbol(case_count) := sum(!!as.symbol(case_count)), .groups = "drop")

  } else if  (get_data_type(x) == "linelist" & to == "count-incidence"){

    #Change the attribute first to avoid the warning from summarise
    attr(x, "data_type")  <- "count-incidence"
    attr(x, "case_count") <- case_count


    #Summarise
    x <- x %>%
      summarise(!!as.symbol(case_count) := dplyr::n(), .groups = "drop")

  } else if (get_data_type(x) == "linelist" & to == "count-cumulative"){

    #Go linelist -> count incidence -> count cumulative
    x <- x %>%
      to_count(to = "count-incidence") %>%
      to_count(to = "count-cumulative")


  } else if (get_data_type(x) == "count-incidence" & to == "count-cumulative"){

    #Summarise
    x <- x  %>%
      to_count(to = "count-incidence") %>% #Just to make sure 1 obs per
      dplyr::group_by(dplyr::across(dplyr::all_of(c(get_event_date(x), get_is_censored(x), get_strata(x), get_temporal_effect_cols(x), get_covariates(x))))) %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(get_report_date(x))), .by_group = TRUE) %>%
      dplyr::mutate(!!as.symbol(case_count) := cumsum(!!as.symbol(case_count))) %>%
      ungroup()

    attr(x, "data_type") <- "count-cumulative"

  } else if (get_data_type(x) == "count-cumulative" & to == "count-cumulative"){

    x <- x %>%
      summarise(!!as.symbol(case_count) := sum(!!as.symbol(case_count)), .groups = "drop")

  } else if (get_data_type(x) == "linelist" & to == "linelist"){

    x <- x %>%
      ungroup()

  } else {
    cli::cli_abort("Transformation from `data_type` {get_data_type(x)} to {to} not implemented")
  }

  x <- x %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(c(get_event_date(x), get_strata(x), get_is_censored(x), get_covariates(x), get_temporal_effect_cols(x)))))

  #Return the count
  return(x)

}
