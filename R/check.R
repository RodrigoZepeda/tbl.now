#' Check the onset and report dates
#'
#' @inheritParams tbl_now
#'
#' @return `TRUE` (invisible) if the `event_date` and `report_date` are
#' date columns present in the data and `event_date` <= `report_date` for
#' all observations. Called for its side effects
#'
#' @keywords internal
check_date_columns <- function(data, event_date, report_date) {

  #Force to data.frame to avoid dplyr_reconstruct issues
  data <- as.data.frame(data)

  #Check that date column is a character
  if (!is.character(event_date) || length(event_date) != 1) {
    cli::cli_abort("{.arg event_date} must be a single string (column name)")
  }

  #Check that date column is a character
  if (!is.character(report_date) || length(report_date) != 1) {
    cli::cli_abort("{.arg report_date} must be a single string (column name)")
  }

  # Check that columns are in data
  if (!(event_date %in% colnames(data))) {
    cli::cli_abort("Column {.code event_date = {.val {event_date}}} not found in {.code data}")
  }

  if (!(report_date %in% colnames(data))) {
    cli::cli_abort("Column {.code report_date = {.val {report_date}}} not found in {.code data}")
  }

  # Check that they are dates or both are numbers
  true_date_col   <- data %>% dplyr::pull(!!as.symbol(event_date))
  report_date_col <- data %>% dplyr::pull(!!as.symbol(report_date))

  # Check if they are numnbers or dates
  are_numbers <- is.integer(true_date_col) & is.integer(report_date_col)
  are_dates   <- lubridate::is.Date(true_date_col) & lubridate::is.Date(report_date_col)

  if (!are_dates & !are_numbers) {
    cli::cli_abort(
      paste0(
        "column {.code report_date = {.val {report_date}}} is of type {typeof(report_date_col)} ",
        "but column {.code event_date = {.val {event_date}}} is of type {typeof(true_date_col)} ",
        "Use {.code as.Date} or {.code as.integer} to transform them to either both be dates or integers."
      )
    )
  }

  # # Check that none observation has report before onset
  # if (any(true_date_col > report_date_col)) {
  #
  #   # Get the number of observations before (for message)
  #   n_before <- nrow(data)
  #
  #   # Filter to keep only those with onset after report
  #   n_after <- data %>%
  #     dplyr::filter(!!as.symbol(event_date) > !!as.symbol(report_date)) %>%
  #     dplyr::tally() %>%
  #     dplyr::pull()
  #
  #   perc  <- round(100*n_after/n_before, 4)
  #
  #   cli::cli_warn(
  #     "{.val {n_after}} rows ({perc}%) have a {.code report_date} ocurring before the {.code event_date}. This might throw an error later on."
  #   )
  # }

  return(invisible(TRUE))
}

#' Check the strata columns
#'
#' The `strata` columns should be present in the data and should be
#' character, integers, or factors.
#'
#' @inheritParams tbl_now
#'
#' @return (invisible) `TRUE` if the columns in `strata` are NULL or
#' are character, integers, or factors in the tibble `data`.
#'
#' @keywords internal
check_strata <- function(data, strata) {

  #Check that the column is not null and exists in data
  if (!is.null(strata)){

    for (strata_colname in strata){

      #Check they are in the dataset
      if (!(strata_colname %in% colnames(data))) {
        cli::cli_abort("Strata column {.val {strata_col}} not found in ({.code data})")
      }

      #Check that strata has correct values (character, integer or factor)
      strata_col <- data %>%
        dplyr::distinct_at(strata_colname) %>%
        dplyr::pull(!!as.symbol(strata_colname))

      if(rlang::is_integer(strata_col) | rlang::is_character(strata_col) | is.factor(strata_col)){
        cli::cli_abort(
          "Strata column {.val {strata_col}} should contain integer or character values."
        )
      }

    }

    #Check that there is more than one observation per strata
    #Compute the number of observations in strata
    n_strata <- data %>%
      dplyr::distinct_at(strata) %>%
      dplyr::tally() %>%
      dplyr::pull()

    #Compute the number of observations
    n_data <- data %>%
      dplyr::tally() %>%
      dplyr::pull()

    #See if strata and data are different. Otherwise there is 1 obs per strata
    if (n_strata == n_data){
      cli::cli_abort(
        "Cannot create a stratified model: There is only one observation per strata."
      )
    }
  }

  return(invisible(TRUE))
}

#' Check the delay_is_censored column
#'
#' The `delay_is_censored` column should only have values between 0 and 1. The idea
#' is to indicate whether a different column in `data` has
#' censored values.
#'
#' @inheritParams tbl_now
#'
#' @return (invisible) `TRUE` if the column `delay_is_censored` is NULL or
#' has valid values and is in the tibble `data`.
#'
#' @keywords internal
check_delay_is_censored <- function(data, delay_is_censored) {

  #Check that the column is not null and exists in data
  if (!is.null(delay_is_censored)){

    #Abort if not found
    if (!(delay_is_censored %in% colnames(data))) {
      cli::cli_abort("{.code delay_is_censored = {.val {delay_is_censored}}} not found in the dataset ({.code data})")
    }

    #Check values are 0 or 1
    censored_vals <- data %>% dplyr::distinct_at(delay_is_censored) %>% dplyr::pull()

    if (!all(censored_vals %in% c(0, 1))) {
      #Get one of the not censored values
      not_zero_one <- censored_vals[which(censored_vals != 0 & censored_vals != 1)]
      not_zero_one <- not_zero_one[1:min(length(not_zero_one), 5)]

      #Throw message
      cli::cli_abort(
        "Column {.val {delay_is_censored}} of {.code data} should only have values: {.val TRUE} or {.val FALSE}. Found the following invalid values: {.val {not_zero_one}}"
      )
    }
  }

  # if (is.null(delay_is_censored)) {
  #   data <- data %>%
  #     dplyr::mutate(!!as.symbol(".delay_is_censored") := 0)
  #   delay_is_censored <- ".delay_is_censored"
  # } else {
  #   # Check that the column name is in the data
  #   if (!(delay_is_censored %in% colnames(data))) {
  #     cli::cli_abort("{.code delay_is_censored = {.val {delay_is_censored}}} not found in the dataset ({.code data})")
  #   }
  # }
  #
  # # Assign the name .delay_is_censored to the `delay_is_censored` column
  # data <- data %>%
  #   dplyr::mutate(!!as.symbol(".delay_is_censored") := as.numeric(!!as.symbol(delay_is_censored)))
  #
  # # Check that values are only 0's and 1's
  # is_batched_vals <- data %>%
  #   dplyr::distinct_at(".delay_is_censored") %>%
  #   dplyr::pull()
  #
  # if (!all(is_batched_vals %in% c(0, 1))) {
  #   cli::cli_abort(
  #     "Column {.val {delay_is_censored}} of {.code data} should only have values: {.code TRUE} or {.code FALSE}"
  #   )
  # }

  return(invisible(TRUE))
}

#' Check the `now` argument
#'
#' Checks that the `now` argument is a date and within range
#' of the data
#'
#' @inheritParams tbl_now
#'
#' @return (invisible) TRUE if the `now` date is achievable considering
#' the `data` dataset and the columns `event_date` and
#' `report_date`
#'
#' @keywords internal
check_now <- function(data, event_date, report_date, now) {

  if (!is.null(now)){

    # Check that now is a date
    if(!lubridate::is.Date(now)) {
      cli::cli_abort(
        "{.code now = {.val {now}}} is not a {.emph Date}. Use {.code as.Date} to transform it."
      )
    }

    # Check that now falls between dates by obtaining the minimum and
    # maximum dates observed in `data`
    min_date_true <- data %>%
      dplyr::summarise(!!as.symbol("min") := min(!!as.symbol(event_date))) %>%
      dplyr::pull()

    max_date_true <- data %>%
      dplyr::summarise(!!as.symbol("max") := max(!!as.symbol(event_date))) %>%
      dplyr::pull()

    max_date_report <- data %>%
      dplyr::summarise(!!as.symbol("max") := max(!!as.symbol(report_date))) %>%
      dplyr::pull()

    min_date_report <- data %>%
      dplyr::summarise(!!as.symbol("min") := min(!!as.symbol(report_date))) %>%
      dplyr::pull()

    max_date <- max(max_date_true, max_date_report)
    min_date <- min(min_date_true, min_date_report)

    #Check that now is whithin the limits of the data
    if (now < min_date | now > max_date) {
      cli::cli_abort(
        "{.code now = {.val {now}}} is outside the scope of the data which ranges from {format(min_date, '%Y-%b-%d')} to {format(max_date, '%Y-%b-%d')}."
      )
    }
  }

  return(invisible(TRUE))
}

#' Check the `date_units` argument
#'
#' @inheritParams tbl_now
#'
#' @return (invisible) `TRUE` if the date_units are valid or NULL
#'
#' @keywords internal
check_units <- function(data, date_units) {

  valid_units <- c("days", "weeks", "numeric", "months", "years")

  # Check that date_units is in one of the following:
  if (!is.null(date_units) && !(date_units %in% valid_units)) {
    cli::cli_abort(
      "Invalid {.code date_units = {.val {date_units}}}. Specify one of the following: {.val NULL}, {.val {valid_units}}",
    )
  }

  #Check if data only has one row and date_units are NULL
  if (nrow(data) == 1 & is.null(date_units)){
    cli::cli_abort(
      "Cannot infer date_units from just one observation. Specify one of the following: {.val {valid_units}}"
    )
  }

  return(invisible(TRUE))
}

#' Check the `verbose` argument among the options
#'
#' @inheritParams tbl_now
#'
#' @return (invisible) `TRUE` if the verbose argument is valid
#'
#' @keywords internal
check_verbose <- function(verbose) {

  # Check verbose is boolean
  if (!rlang::is_bool(verbose)) {
    cli::cli_abort(
      "`verbose` argument should be either {.val TRUE} or {.val FALSE}"
    )
  }

  return(invisible(TRUE))
}


#' Check the data type
#'
#' @inheritParams tbl_now
#'
#' @return `TRUE` (invisible) if the data_type is one of the valid
#' types for the model's inference.
#'
#' @keywords internal
check_data_type <- function(data_type){

  valid_types <- c("auto", "linelist", "count")
  if (!(data_type %in% valid_types)){
    cli::cli_abort(
      "Unknown data_type {.val {data_type}}. Please select one of the following: {valid_types}"
    )
  }

  return(invisible(TRUE))
}

#' Check a boolean value
#'
#' Function that checks whether a boolean variable is `TRUE` or `FALSE`
#'
#' @param x A variable
#' @param name Name of the argument for printing error messages
#'
#' @examples
#' \dontrun{
#' check_bool(TRUE, "value")
#'
#' #Using a non boolean gives an error message
#' not_a_boolean <- "hello"
#' check_bool(not_a_boolean, "not_a_boolean")
#' }
#'
#' @references
#' From https://stackoverflow.com/a/60346779/5067372
#'
#' @return invisible (TRUE). Called for its side effects.
#'
#' @keywords internal
check_bool <- function(x, name) {
  if (!rlang::is_bool(x)) {
    cli::cli_abort("`{name}` must be either {.val TRUE} or {.val FALSE}. Got {.val {x}} instead.")
  }
  return(invisible(TRUE))
}
