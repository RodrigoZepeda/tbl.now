#' Report and event units to numeric
#'
#' Function that takes a dataframe `data` with two columns
#' with names `event_date` and `report_date` which can either be
#' dates or numeric. It transforms the dates
#'
#' @param data A data.frame
#' @param event_date Name of the column corresponding to date of the event
#' @param report_date Name of the column corresponding to date of the report
#' @param event_units Either `days`, `weeks`, `months`, `years` or `numeric`
#' @param report_units Either `days`, `weeks`, `months`, `years` or `numeric`
#'
#' @return The dataframe `data` with two additional columns `.event_num` and
#' `.report_num` corresponding to transforming the dates to numeric.
#'
#' @keywords internal
time_cols_to_numeric <- function(data, event_date, report_date, event_units, report_units, force){

  #Check that if one is numeric both are numeric
  if (any(c(report_units, event_units) == "numeric") & !all(c(report_units, event_units) == "numeric")){
    cli::cli_abort(
      "If one of `report_units` and `event_units` is numeric, the other one should be too."
    )
  }

  #Check that report_units is coarser than or equal to event_units
  if (event_units != "numeric" && report_units != "numeric") {
    granularity_order <- c("days", "weeks", "months", "years")
    event_level       <- which(granularity_order == event_units)
    report_level      <- which(granularity_order == report_units)

    if (is.na(event_level) || is.na(report_level)) {
      cli::cli_abort(
        "Invalid units specified. Must be one of: 'days', 'weeks', 'months', 'years', or 'numeric'."
      )
    }

    if (report_level < event_level) {
      cli::cli_abort(
        "report_units ('{report_units}') must be coarser than or equal to event_units ('{event_units}'). Change them manually to `numeric`"
      )
    }
  }


  #The event date is the anchor date for both the dates
  min_event_date <- data %>%
    dplyr::distinct_at(.vars = event_date) %>%
    dplyr::summarise(!!as.symbol("min_date") := min(!!as.symbol(event_date))) %>%
    dplyr::pull(!!as.symbol("min_date"))

  #If event date is numeric just do difference
  if (event_units == "numeric"){
    data <- data %>%
      dplyr::mutate(!!as.symbol(".event_num") := !!as.symbol(event_date) - !!min_event_date) %>%
      dplyr::mutate(!!as.symbol(".report_num") := !!as.symbol(report_date) - !!min_event_date)
  } else if (event_units %in% c("days", "weeks")){
    data <- data %>%
      dplyr::mutate(
        !!as.symbol(".event_num") := as.numeric(
          difftime(!!as.symbol(event_date), !!min_event_date, units = !!event_units)
        )) %>%
      dplyr::mutate(
        !!as.symbol(".report_num") := as.numeric(
          difftime(!!as.symbol(report_date), !!min_event_date, units = !!event_units)
        ))
  } else if (event_units == "years"){
    data <- data %>%
      dplyr::mutate(!!as.symbol(".event_num") :=
                      lubridate::year(!!as.symbol(event_date)) - lubridate::year(!!min_event_date)) %>%
      dplyr::mutate(!!as.symbol(".report_num") :=
                      lubridate::year(!!as.symbol(report_date)) - lubridate::year(!!min_event_date))
  } else if (event_units == "months"){
    if (".year_difference_temp" %in% colnames(data) && !force){
      cli::cli_abort(
        "This function internally creates a column called `.year_difference_temp` please rename yours."
      )
    }

    if (".month_difference_temp" %in% colnames(data) && !force){
      cli::cli_abort(
        "This function internally creates a column called `.month_difference_temp` please rename yours."
      )
    }

    #For months time difference is calculated two ways:
    # month difference (if positive) + 12*year difference or
    # month difference mod 12 (if negative) and year difference - 1 (to discount the year given by month difference)
    data <- data %>%
      dplyr::mutate(!!as.symbol(".year_difference_temp") :=
                      lubridate::year(!!as.symbol(event_date)) - lubridate::year(!!min_event_date)) %>%
      dplyr::mutate(!!as.symbol(".month_difference_temp") :=
                      lubridate::month(!!as.symbol(event_date)) - lubridate::month(!!min_event_date)) %>%
      dplyr::mutate(!!as.symbol(".year_difference_temp") := dplyr::if_else(
        !!as.symbol(".month_difference_temp") < 0,
        !!as.symbol(".year_difference_temp") - 1,
        !!as.symbol(".year_difference_temp"))) %>%
      dplyr::mutate(!!as.symbol(".month_difference_temp") := dplyr::if_else(
        !!as.symbol(".month_difference_temp") < 0,
        12 - lubridate::month(!!min_event_date) + (lubridate::month(!!as.symbol(event_date)) - 1),
        !!as.symbol(".month_difference_temp")
      )) %>%
      dplyr::mutate(!!as.symbol(".event_num") := 12*!!as.symbol(".year_difference_temp") + !!as.symbol(".month_difference_temp")) %>%
      dplyr::select(-!!as.symbol(".month_difference_temp"), !!as.symbol(".year_difference_temp"))

    # Now calculate report_num using the same logic
    data <- data %>%
      dplyr::mutate(!!as.symbol(".year_difference_temp") :=
                    lubridate::year(!!as.symbol(report_date)) - lubridate::year(!!min_event_date)) %>%
      dplyr::mutate(!!as.symbol(".month_difference_temp") :=
                      lubridate::month(!!as.symbol(report_date)) - lubridate::month(!!min_event_date)) %>%
      dplyr::mutate(!!as.symbol(".year_difference_temp") := dplyr::if_else(
        !!as.symbol(".month_difference_temp") < 0,
        !!as.symbol(".year_difference_temp") - 1,
        !!as.symbol(".year_difference_temp"))) %>%
      dplyr::mutate(!!as.symbol(".month_difference_temp") := dplyr::if_else(
        !!as.symbol(".month_difference_temp") < 0,
        12 - lubridate::month(!!min_event_date) + (lubridate::month(!!as.symbol(report_date)) - 1),
        !!as.symbol(".month_difference_temp")
      )) %>%
      dplyr::mutate(!!as.symbol(".report_num") := 12*!!as.symbol(".year_difference_temp") + !!as.symbol(".month_difference_temp")) %>%
      dplyr::select(-!!as.symbol(".month_difference_temp"), -!!as.symbol(".year_difference_temp"))
  }

  return(data)
}
