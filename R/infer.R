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
      dplyr::summarise(!!as.symbol("max") := max(!!as.symbol(report_date))) %>%
      dplyr::pull()

    max_true_date <- data %>%
      dplyr::summarise(!!as.symbol("max") := max(!!as.symbol(event_date))) %>%
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
#' @return Whether the data's date_units are `days` or `weeks`.
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
#' }
#' @keywords internal
infer_units_one_column <- function(data, date_units, date_column) {

  #Force conversion of data to avoid loops with dplyr_reconstruct
  data <- dplyr::as_tibble(data)

  valid_units <- c("days", "weeks")

  if (is.null(date_units) || date_units == "auto") {

    if (nrow(data) < 2){
      cli::cli_abort(
        "Cannot infer time units due to {.arg data}
        having {nrow(data)} < 2 observations"
      )
    }

    # Calculate the differences between consecutive dates
    date_diffs <- data %>%
      dplyr::distinct(!!as.symbol(date_column)) %>%
      dplyr::arrange(!!as.symbol(date_column)) %>%
      dplyr::pull(!!as.symbol(date_column)) %>%
      diff()

    # Convert the differences to a period (days)
    min_difference <- date_diffs %>%
      min() %>%
      as.numeric()

    # Categorize based on the median difference
    if (min_difference <= 1) {
      date_units <- "days"
    } else if (min_difference >= 6 & min_difference <= 8) {
      date_units <- "weeks"
    } else {
      cli::cli_abort(
        "Cannot infer time date_units. Specify between {.code date_units = {.val {valid_units}}}"
      )
    }
  } else if (!(date_units %in% valid_units)){
    cli::cli_abort(
      "Date units {date_units} not supported by the model yet."
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
infer_units <- function(data, event_date, report_date, date_units) {


  #Force conversion of data to avoid loops with dplyr_reconstruct
  data <- dplyr::as_tibble(data)

  #Infer the date_units for event_date and report_date
  true_date_units   <- infer_units_one_column(data, date_units = date_units, date_column = event_date)
  report_date_units <- infer_units_one_column(data, date_units = date_units, date_column = report_date)

  #Check that the inferred date_units are the same
  if (true_date_units != report_date_units){
    cli::cli_abort(
      paste0(
        "Inferred date_units for {event_date} are {.val {true_date_units}} while ",
        "inferred date_units for {report_date} are {.val {report_date_units}}. ",
        "The model cannot work with different scales. If you think this is a ",
        "mistake use the `date_units` argument to set up the date_units."
      )
    )
  }

  return(true_date_units)
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
infer_data_type <- function(data, data_type, verbose = FALSE) {

  #Force conversion of data to avoid loops with dplyr_reconstruct
  data <- dplyr::as_tibble(data)

  #If data_type is vector keep the first entry
  data_type <- data_type[1]

  # Check that there is no column `n` if linedata and that there is if counts
  if (data_type == "auto" & ("n" %in% colnames(data))) {
    data_type <- "count"

    #Check that `n` column is really an integer
    n_col <- data %>% dplyr::distinct_at("n") %>% dplyr::pull()
    if (!is.numeric(n_col) || any(ceiling(n_col) != n_col)){
      cli::cli_abort(
        paste0(
          "Cannot automatically detect data_type. Data has a column named `n`",
          "which does not seem to be count data (is not an integer).",
          " Please set the {.code data_type} argument to either {.val count}",
          " or {.val linelist}."
        )
      )
    }

    if (verbose) {
      cli::cli_alert_info(
        paste0(
          "Identified data as count-data with counts in column `n`."
        )
      )
    }

  } else if (data_type == "auto" & !("n" %in% colnames(data))) {

    data_type <- "linelist"
    if (verbose) {
      cli::cli_alert_info(
        paste0(
          "Identified data as linelist-data where each observation is a test."
        )
      )
    }
  } else if (data_type == "linelist" & "n" %in% colnames(data)) {
    cli::cli_warn(
      paste0(
        "Linelist data contains a column named `n` which will be overwritten.",
        " If you are working with count-data set ",
        "{.code data_type = {.val count}}"
      )
    )
  } else if (data_type == "count" & !("n" %in% colnames(data))) {
    cli::cli_abort(
      paste0(
        "Count data should have a column named `n` with the number ",
        "of tests per event_date-report_date combination. Otherwise set ",
        "{.code data_type = {.val linelist}} if working with line-list data."
      )
    )
  }

  return(data_type)
}
