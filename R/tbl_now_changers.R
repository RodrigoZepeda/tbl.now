#' Change attributes of a `tbl_now` object
#'
#' @description Functions to modify the attributes of a `tbl_now` object.
#' All functions validate the object after making changes.
#'
#' @param x A `tbl_now` object
#' @param value The new value for the attribute
#'
#' @return A `tbl_now` object with updated attributes
#'
#' @examples
#' \dontrun{
#' data(denguedat)
#' ndata <- tbl_now(denguedat,
#'                           event_date = "onset_week",
#'                           report_date = "report_week")
#'
#' # Change the event_date column to a different date column
#' ndata$new_onset_week <- ndata$onset_week - lubridate::days(1)
#' ndata <- change_event_date(ndata, "new_onset_week")
#' ndata
#'
#' # Change the report_date column to a different column
#' ndata$new_report_week <- ndata$report_week - lubridate::days(1)
#' ndata <- change_report_date(ndata, "new_report_week")
#' ndata
#'
#' # Change strata to different strata
#' ndata$age_group <- sample(c("< 18","20-60","60+"), nrow(ndata), replace = TRUE)
#' ndata <- change_strata(ndata, c("gender", "age_group"))
#' ndata
#'
#' # Remove some strata
#' ndata <- remove_strata(ndata, "gender")
#' ndata
#'
#' # Add strata
#' ndata <- add_strata(ndata, "gender")
#' ndata
#'
#' # Change covariates
#' ndata$temperature <- rnorm(nrow(ndata), 25, 4)
#' ndata$humidity    <- rbeta(nrow(ndata), 0.6, 0.4)
#' ndata <- change_covariates(ndata, c("temperature", "humidity"))
#' ndata
#'
#' # Remove some covariates
#' ndata <- remove_covariates(ndata, "temperature")
#' ndata
#'
#' # Add covariates
#' ndata <- add_covariates(ndata, "temperature")
#' ndata
#'
#' # Change now
#' ndata <- change_now(ndata, as.Date("2025-01-01"))
#' ndata
#' }
#'
#' @name change
NULL

#' @rdname change
#' @export
#Change the `event_date` to a different column name
change_event_date <- function(x, value) {

  if (!inherits(x, "tbl_now")) {
    cli::cli_abort("{.arg x} must be a {.code tbl_now} object")
  }

  if (!is.character(value) || length(value) != 1) {
    cli::cli_abort("{.arg value} must be a character vector of length 1")
  }

  if (!value %in% colnames(x)) {
    cli::cli_abort("Column {.val {value}} not found in data")
  }

  if (!lubridate::is.Date(x[[value]])) {
    cli::cli_abort("Column {.val {value}} must be of class Date")
  }

  attr(x, "event_date") <-  value

  # Re-infer now if needed
  now <- tryCatch(
    infer_now(x, now = get_now(x), event_date = value, report_date = get_report_date(x)),
    error = function(e) get_now(x)
  )
  attr(x, "now") <-  now

  validate_tbl_now(x)

  x
}

#' @rdname change
#' @export
#Change the `report_date` to a different column name
change_report_date <- function(x, value) {

  if (!inherits(x, "tbl_now")) {
    cli::cli_abort("{.arg x} must be a {.code tbl_now} object")
  }

  if (!is.character(value) || length(value) != 1) {
    cli::cli_abort("{.arg value} must be a character vector of length 1")
  }

  if (!value %in% colnames(x)) {
    cli::cli_abort("Column {.val {value}} not found in data")
  }

  if (!lubridate::is.Date(x[[value]])) {
    cli::cli_abort("Column {.val {value}} must be of class Date")
  }

  attr(x, "report_date") <-  value

  # Re-infer now if needed
  now <- tryCatch(
    infer_now(x, now = get_now(x), event_date = get_event_date(x), report_date = value),
    error = function(e) get_now(x)
  )
  attr(x, "now") <-  now

  validate_tbl_now(x)

  x
}

#' @rdname change
#' @export
# Change all of the strata to whatever is added in value
change_strata <- function(x, value) {

  if (!inherits(x, "tbl_now")) {
    cli::cli_abort("{.arg x} must be a {.code tbl_now} object")
  }

  if (!is.null(value) && !is.character(value)) {
    cli::cli_abort("{.arg value} must be {.val NULL} or a character vector")
  }

  if (!is.null(value)) {
    for (st in value) {
      if (!st %in% colnames(x)) {
        cli::cli_abort("Strata column {.val {st}} not found in data")
      }
    }
  }

  attr(x, "strata") <-  value
  attr(x, "num_strata") <-  length(value)

  validate_tbl_now(x)

  x
}

#' @rdname change
#' @export
# Remove `value`  from strata
remove_strata <- function(x, value) {

  #Get the strata that is not value
  strata_to_keep <- get_strata(x)
  strata_to_keep <- strata_to_keep[which(!(strata_to_keep %in% c(value)))]

  change_strata(x, strata_to_keep)
}

#' @rdname change
#' @export
# Adds `value`  to existing strata
add_strata <- function(x, value) {

  #Add to strata
  change_strata(x, c(value, get_strata(x)))

}


#' @rdname change
#' @export
# Remove all `values`  from strata
remove_all_strata <- function(x) {
  change_strata(x, NULL)
}


#' @rdname change
#' @export
# Change the existing covariates to the ones in value
change_covariates <- function(x, value) {

  if (!inherits(x, "tbl_now")) {
    cli::cli_abort("{.arg x} must be a {.code tbl_now} object")
  }

  if (!is.null(value) && !is.character(value)) {
    cli::cli_abort("{.arg value} must be {.val NULL} or a character vector")
  }

  if (!is.null(value)) {
    for (cv in value) {
      if (!cv %in% colnames(x)) {
        cli::cli_abort("Covariate column {.val {cv}} not found in data")
      }
    }
  }

  attr(x, "covariates") <-  value
  attr(x, "num_covariates") <-  length(value)

  validate_tbl_now(x)

  x
}

#' @rdname change
#' @export
# Removes the specific covariate `value`
remove_covariate <- function(x, value) {

  #Get the strata that is not value
  covariates_to_keep <- get_covariates(x)
  covariates_to_keep <- covariates_to_keep[which(!(covariates_to_keep %in% c(value)))]

  change_covariates(x, covariates_to_keep)
}

#' @rdname change
#' @export
# Adds `value`  to existing covariates
add_covariate <- function(x, value) {

  #Add to strata
  change_covariates(x, c(value, get_covariates(x)))

}


#' @rdname change
#' @export
# Removes all covariates
remove_all_covariates <- function(x) {
  change_covariates(x, NULL)
}

#' @rdname change
#' @export
# Change the `now` for `value`
change_now <- function(x, value) {

  if (!inherits(x, "tbl_now")) {
    cli::cli_abort("{.arg x} must be a {.code tbl_now} object")
  }

  if (!inherits(value, "Date") || length(value) != 1) {
    cli::cli_abort("{.arg value} must be a Date object of length 1")
  }

  # Re-infer now
  now <- tryCatch(
    infer_now(x, now = value, event_date = get_event_date(x), report_date =  get_report_date(x)),
    error = function(e) get_now(x)
  )
  attr(x, "now") <-  now


  validate_tbl_now(x)

  x
}
