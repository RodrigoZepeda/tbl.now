#' Flag reports with an implausibly long delay as censored
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Marks every report whose reporting delay exceeds `max_delay` (in event units)
#' as *censored*: the recorded delay becomes an upper bound rather than an exact
#' value. This is the natural response to a "delay too long" outlier: instead of
#' letting, say, a 300-day delay drag the delay distribution, you tell the model
#' only that the case arrived *by* that delay.
#'
#' The delay is read from the `tbl_now`'s generated `.delay` column (the report
#' date minus the event date, in the data's event units). The `is_censored`
#' column is updated (or created) and merged with any existing censoring flags
#' (already-censored reports stay censored).
#'
#' @param data A `tbl_now` object.
#' @param max_delay Numeric. Reports with delay strictly greater than this
#'   (in the data's event units) are flagged as censored.
#' @param quiet If `TRUE`, suppress the informational message.
#'
#' @returns The `tbl_now` with its `is_censored` column updated (created if
#'   absent).
#'
#' @seealso [add_is_censored()], [change_is_censored()], [remove_is_censored()]
#'
#' @examples
#' df <- data.frame(
#'   onset = as.Date("2020-01-01") + c(0, 0, 1, 2),
#'   reported = as.Date("2020-01-01") + c(1, 5, 2, 300)
#' )
#' tn <- tbl_now(df,
#'   event_date = onset, report_date = reported,
#'   data_type = "linelist", verbose = FALSE
#' )
#'
#' # the 300-day report becomes censored (an upper bound on its delay)
#' tn <- censor_delays_above(tn, max_delay = 60)
#' @md
#' @export
censor_delays_above <- function(data, max_delay, quiet = FALSE) {
  if (!is_tbl_now(data)) {
    cli::cli_abort("{.arg data} must be a {.cls tbl_now} (see {.fn tbl_now}).")
  }
  if (!is.numeric(max_delay) || length(max_delay) != 1L || max_delay < 0) {
    cli::cli_abort("{.arg max_delay} must be a single non-negative number.")
  }

  # `.delay` is already (report - event) in event units.
  delay_in_units <- data[[".delay"]]
  is_too_long <- is.finite(delay_in_units) & delay_in_units > max_delay

  # Merge with any existing censoring flags (don't un-censor what was already
  # censored); create the column if the tbl_now has none yet.
  censored_col_name <- get_is_censored(data)
  if (!is.null(censored_col_name) && censored_col_name %in% names(data)) {
    already_censored <- as.logical(data[[censored_col_name]])
    already_censored[is.na(already_censored)] <- FALSE
    data[[censored_col_name]] <- already_censored | is_too_long
  } else {
    data[[".is_censored"]] <- is_too_long
    data <- add_is_censored(data, ".is_censored")
  }

  if (!quiet) {
    cli::cli_inform(c(
      "i" = "Marked {sum(is_too_long)} report{?s} with delay > {max_delay} {get_event_units(data)}{?s} as censored.",
      "*" = "This delay is now an upper bound (is_censored)."
    ))
  }

  data
}
