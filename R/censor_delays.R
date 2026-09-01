#' Treat implausibly long delays as censored rather than exact
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Every surveillance system has a handful of records that arrive absurdly late
#' -- a case with onset in March that turns up in December. Taken at face value
#' those delays drag the estimated delay distribution to the right and make the
#' nowcast think reporting is far slower than it is.
#'
#' Rather than deleting those records (which throws away a real case) these
#' functions mark them as **censored**: the object keeps the case, but records
#' its delay as *"at least this long"* instead of *"exactly this long"*.
#'
#' * `censor_delays_above()` does this for the **reporting** delay, by setting the
#'   `is_censored` flag.
#' * `censor_validation_delays_above()` does the same for the **validation**
#'   delay -- a case still waiting on a laboratory result months later is, in
#'   practice, never going to be resolved -- by returning it to `"pending"`.
#'
#' @details
#' The reporting delay is read from the generated `.delay` column (report date
#' minus event date, in the object's event units). Existing censoring flags are
#' merged rather than overwritten, so a report that was already censored stays
#' censored.
#'
#' @param x A `tbl_now` object. `censor_validation_delays_above()` requires one
#'   that carries a validation process (see [add_validation_date()][add]).
#' @param max_delay Numeric. Delays strictly greater than this are censored, in
#'   the object's event units (reporting) or validation units (validation).
#' @param verbose Logical. Whether to report how many rows were affected.
#'   Default `TRUE`.
#'
#' @returns
#' `censor_delays_above()` returns the `tbl_now` with its `is_censored` column
#' updated, creating it when absent.
#'
#' `censor_validation_delays_above()` returns the `tbl_now` with the offending
#' rows' `validation_type` set to `"pending"` and their validation date set to
#' `NA` -- a resolution you refuse to believe is not a resolution.
#'
#' @seealso
#' [add_is_censored()][add] and [change_is_censored()][add] to set the flag by
#' hand; [diagnose_validation_delay()] and [plot_delay_distribution()] to find
#' the threshold worth using; [diagnose_truncation()] for the delays that are
#' missing rather than long; [complete_zeroes()] for the opposite problem.
#'
#' @examples
#' # Four cases, one of which took 300 days to be reported.
#' df <- data.frame(
#'   onset = as.Date("2020-01-01") + c(0, 0, 1, 2),
#'   reported = as.Date("2020-01-01") + c(1, 5, 2, 300)
#' )
#' tn <- tbl_now(df,
#'   event_date = onset, report_date = reported,
#'   data_type = "linelist", verbose = FALSE
#' )
#' tn$.delay
#'
#' # Anything slower than 60 days is recorded as a lower bound, not a fact.
#' censored <- censor_delays_above(tn, max_delay = 60)
#' censored[[get_is_censored(censored)]]
#'
#' # The validation counterpart: a laboratory result that took 90 days.
#' cases <- data.frame(
#'   onset = as.Date("2021-01-04") + 0:4,
#'   visit = as.Date("2021-01-05") + 0:4,
#'   result = as.Date("2021-01-05") + 0:4 + c(1, 2, 1, 90, 2),
#'   outcome = rep("confirmed", 5)
#' )
#' flu <- tbl_now(cases,
#'   event_date = onset, report_date = visit,
#'   validation_date = result, validation_type = outcome,
#'   data_type = "linelist", verbose = FALSE
#' )
#'
#' # That one goes back to "pending"; the other four stay confirmed.
#' table(censor_validation_delays_above(flu, 30, verbose = FALSE)[["outcome"]])
#'
#' @name censor_delays_above
#' @export
censor_delays_above <- function(x, max_delay, verbose = TRUE) {
  if (!is_tbl_now(x)) {
    cli::cli_abort("{.arg x} must be a {.cls tbl_now} (see {.fn tbl_now}).")
  }
  if (!is.numeric(max_delay) || length(max_delay) != 1L || max_delay < 0) {
    cli::cli_abort("{.arg max_delay} must be a single non-negative number.")
  }

  # `.delay` is already (report - event) in event units.
  delay_in_units <- x[[".delay"]]
  is_too_long <- is.finite(delay_in_units) & delay_in_units > max_delay

  # Merge with any existing censoring flags (don't un-censor what was already
  # censored); create the column if the tbl_now has none yet.
  censored_col_name <- get_is_censored(x)
  if (!is.null(censored_col_name) && censored_col_name %in% names(x)) {
    already_censored <- as.logical(x[[censored_col_name]])
    already_censored[is.na(already_censored)] <- FALSE
    x[[censored_col_name]] <- already_censored | is_too_long
  } else {
    x[[".is_censored"]] <- is_too_long
    x <- add_is_censored(x, ".is_censored")
  }

  if (verbose) {
    cli::cli_inform(c(
      "i" = "Marked {sum(is_too_long)} report{?s} with delay > {max_delay} {get_event_units(x)}{?s} as censored.",
      "*" = "This delay is now an upper bound (is_censored)."
    ))
  }

  x
}
