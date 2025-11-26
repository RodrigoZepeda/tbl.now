#' Automatically infer the `now`.
#'
#' Function returns the maximum onset date of `data` if `now = NULL`.
#' Else it returns the `now` it was given.
#'
#'  @inheritParams tbl_now
#'
#' @return The `now` value for the nowcast which can be the last date
#' of the data or specified by the user
#'
#' @examples
#' \dontrun{
#' #Get the maximum report date
#' ddata <- data.frame(
#'     event_date   = c(as.Date("2020/07/08"), as.Date("2020/07/09")),
#'     report_date = c(as.Date("2020/07/11"), as.Date("2020/07/12"))
#' )
#' infer_now(ddata, NULL, event_date = "event_date", report_date = "report_date")
#'
#' #Otherwise get the date given
#' infer_now(ddata, as.Date("2020/07/11"), event_date = "event_date",
#'   report_date = "report_date")
#' }
#'
#' @keywords internal
infer_now <- function(data, now, event_date, report_date) {

  #Force conversion of data to avoid loops with dplyr_reconstruct
  data <- dplyr::as_tibble(data)

  #Check that the data frame is not empty
  if (nrow(data) < 1){
    cli::cli_abort(
      "{.arg data} is an empty data.frame with {nrow(data)} rows."
    )
  }

  #Check that the columns are in the data.frame
  check_date_columns(data, event_date, report_date)

  # Now should be the last observed moment in time
  if (is.null(now)) {
    max_report_date <- data %>%
      dplyr::summarise(!!as.symbol("max") := max(!!as.symbol(report_date), na.rm = TRUE)) %>%
      dplyr::pull()

    max_true_date <- data %>%
      dplyr::summarise(!!as.symbol("max") := max(!!as.symbol(event_date), na.rm = TRUE)) %>%
      dplyr::pull()

    now <- max(max_report_date, max_true_date)
  }

  return(now)
}

#' Automatically infer which value is `date_units` for one column.
#'
#' Function returns whether data is daily or weekly
#' `date_column`.
#'
#'  @inheritParams tbl_now
#'
#' @param date_column Name of a column of `data`
#' that contains the dates.
#'
#' @return Whether the data's date_units are `days`, `weeks`, `months`, `years` or `numeric`.
#' @examples
#' \dontrun{
#' #Get the maximum report date and infer the data distribution
#' daily_data <- data.frame(
#'     event_date   = c(as.Date("2020/07/08"), as.Date("2020/07/09"), as.Date("2020/07/10")),
#'     report_date = c(as.Date("2020/07/11"), as.Date("2020/07/12"), as.Date("2020/07/14"))
#' )
#' infer_units_one_column(daily_data, NULL, "report_date")
#'
#' #Get the maximum report date and infer the data distribution
#' weekly_data <- data.frame(
#'     event_date   = c(as.Date("2020/07/08"), as.Date("2020/07/15"), as.Date("2020/07/22")),
#'     report_date = c(as.Date("2020/07/11"), as.Date("2020/07/18"), as.Date("2020/07/25"))
#' )
#' infer_units_one_column(weekly_data, NULL, "report_date")
#'
#' #Or just input the type
#' infer_units_one_column(weekly_data, "weeks", "report_date")
#'
#' #Also works with numeric
#' infer_units_one_column(data.frame(report_date = 1:20), date_units = "auto", "report_date")
#'
#' }
#' @keywords internal
infer_units_one_column <- function(data, date_column, date_units) {

  #Force conversion of data to avoid loops with dplyr_reconstruct
  data <- dplyr::as_tibble(data)

  valid_units <- c("days", "weeks", "months", "years", "numeric")

  if (is.null(date_units) || date_units == "auto") {

    if (nrow(data) < 2){
      cli::cli_abort(
        "Cannot infer time units due to {.arg data}
        having {nrow(data)} < 2 observations"
      )
    }

    # Calculate the differences between consecutive dates
    date_vals <- data %>%
      dplyr::distinct(!!as.symbol(date_column)) %>%
      dplyr::arrange(!!as.symbol(date_column)) %>%
      dplyr::pull(!!as.symbol(date_column))

    # Check if they are date or numeric
    if (lubridate::is.Date(date_vals)){

      date_diffs <- date_vals %>%
        diff()

      # Convert the differences to a period (days)
      min_difference <- date_diffs %>%
        min(na.rm = TRUE) %>%
        as.numeric()

      # Categorize based on the median difference
      if (min_difference <= 6) {
        date_units <- "days"
      } else if (min_difference > 6 & min_difference <= 13) {
        date_units <- "weeks"
      } else if (min_difference >= 27 & min_difference <= 35) {
        date_units <- "months"
      } else if (min_difference >= 360 & min_difference <= 370) {
        date_units <- "years"
      } else {
        cli::cli_abort(
          "Cannot infer time date_units. Specify between {.code date_units = {.val {valid_units}}}"
        )
      }
    } else if (is.numeric(date_vals)) {
      date_units <- "numeric"
    } else {
      cli::cli_abort(
        "Date column {.val {date_column}} has to be either `numeric` or a `Date`"
      )
    }
  } else if (!(date_units %in% valid_units)){
    cli::cli_abort(
      "Date units {date_units} not supported yet. Try transforming them to `numeric` and using the `numeric` format."
    )
  }
  return(date_units)
}

#' Automatically infer which value is `date_units`
#'
#' Function returns whether data is daily or weekly
#' `date_column`.
#'
#'  @inheritParams tbl_now
#'
#' @return Whether the data's date_units are `days` or `weeks`.
#'
#' @keywords internal
infer_units <- function(data, date_column, date_units) {
  infer_units_one_column(data, date_units = date_units, date_column = date_column)
}

#' Automatically infer the `data_type`
#'
#' Infers whether the data is line-data or count-data whether it has (or has not)
#' a column named `n`
#'
#' @inheritParams tbl_now
#'
#' @return Whether the data is `count` or `linelist`
#'
#' @keywords internal
infer_data_type <- function(data, data_type, event_date, report_date, strata = NULL,
                            is_batched = NULL, case_col = NULL, verbose = FALSE) {

  if (length(case_col) > 1){
    cli::cli_abort(
      "Invalid `case_col` must be a vector of length 1 or `NULL`"
    )
  }

  #Force conversion of data to avoid loops with dplyr_reconstruct
  data <- dplyr::as_tibble(data)

  #If data_type is vector keep the first entry
  data_type <- data_type[1]

  # Check that there is no column `n` if linedata and that there is if counts
  if (data_type == "auto" && !is.null(case_col) &&  (case_col %in% colnames(data))) {

    #Check that the data is different
    distinct_data <- data %>%
      dplyr::distinct_at(c(event_date, report_date, strata, is_batched)) %>%
      nrow()

    if (distinct_data != nrow(data)){
      cli::cli_warn(
        "Cannot accurately infer the data-type when rows are repeated across event and report dates"
      )
    }

    #Check that data should be always increasing by strata, event date, report date
    #for that purpose get the differences
    summarized_difs <- data %>%
      dplyr::group_by_at(c(event_date, strata, is_batched)) %>%
      dplyr::arrange(!!as.symbol(report_date)) %>%
      dplyr::mutate(!!as.symbol("difference") := !!as.symbol(case_col) - dplyr::lag(!!as.symbol(case_col))) %>%
      dplyr::filter(!is.na(!!as.symbol("difference"))) %>%
      dplyr::filter(!!as.symbol("difference") < 0) %>%
      nrow()

    if (summarized_difs > 0){
      data_type <- "count-incidence"
    } else {
      data_type <- "count-cumulative"
    }

    # #Check that `n` column is really an integer

    # if (!is.numeric(n_col) || any(ceiling(n_col) != n_col, na.rm = TRUE)){
    #   cli::cli_abort(
    #     paste0(
    #       "Cannot automatically detect data_type. Data has a column named {.val {case_col}}",
    #       "which does not seem to be count data (is not an integer).",
    #       " Please set the {.code data_type} argument to either {.val count}",
    #       " or {.val linelist}."
    #     )
    #   )
    # }

    #Look for missing values
    n_col <- data %>% dplyr::filter(is.na(!!as.symbol(case_col))) %>% nrow()
    if (n_col > 0){
      cli::cli_warn(
        "Some observations in the count column {.val {case_col}} contain missing values."
      )
    }

    if (verbose) {
      cli::cli_alert_info(
        paste0(
          "Identified data as {data_type} with counts in column {.val {case_col}}."
        )
      )
    }

  } else if (data_type == "auto" && (is.null(case_col) || !(case_col %in% colnames(data)))) {

    data_type <- "linelist"
    if (verbose) {
      cli::cli_alert_info(
        paste0(
          "Identified data as linelist-data where each observation is a test."
        )
      )
    }
  } else if (data_type == "linelist" && !is.null(case_col) && case_col %in% colnames(data)) {
    cli::cli_warn(
      paste0(
        "Linelist data contains a column named {.val {case_col}} which will be overwritten.",
        " If you are working with count-data set ",
        "{.code data_type = {.val count-incidence}} or {.code data_type = {.val count-cumulative}}"
      )
    )
  } else if (grepl("count", data_type) && !is.null(case_col) && !(case_col %in% colnames(data))) {
    cli::cli_abort(
      paste0(
        "Count data should have a column named {.val {case_col}} with the number ",
        "of tests per event_date-report_date combination. Otherwise set ",
        "{.code data_type = {.val linelist}} if working with line-list data."
      )
    )
  }

  return(data_type)
}
