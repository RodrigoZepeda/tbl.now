#' Record a report or a delay as a bound rather than a fact
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Surveillance data is full of dates that are not really dates. A case with
#' onset in March turns up in December; a report date is missing altogether; a
#' system codes "never reported" as `2222-02-22`. Deleting those records throws
#' away real cases, and believing them drags the estimated delay distribution to
#' the right until the nowcast thinks reporting is far slower than it is.
#'
#' These functions **censor** instead: the object keeps the case, but records its
#' delay as *"at least this long"* rather than *"exactly this long"*.
#'
#' * `censor_reports()` -- rows matching a condition get a **replacement report
#'   date** (the missing ones become today's date, say) and the
#'   `is_censored_report` flag.
#' * `censor_delays()` -- the same, expressed as a **delay** instead of a date;
#'   with no replacement it only sets the flag.
#' * `censor_delays_above()` -- the common case of `censor_delays()`: everything
#'   slower than a threshold.
#' * `censor_validation_delays_above()` -- the same threshold rule for the
#'   **validation** delay -- a case whose laboratory result took months to come
#'   back -- by setting the `is_censored_validation` flag.
#'
#' @details
#' The reporting delay is read from the generated `.delay` column (report date
#' minus event date, in the object's event units); the validation delay from
#' `.validation_delay` (validation date minus report date, in validation
#' units). Existing censoring flags are merged rather than overwritten, so a
#' delay that was already censored stays censored, and the flag column is
#' created (as `.is_censored_report` / `.is_censored_validation`) when the
#' object has none.
#'
#' The threshold functions keep the case **and its date**. Nothing is deleted
#' and no outcome is rewritten: the flag says the delay is a bound rather than a
#' measurement, and it is up to the model to use that. A case that was confirmed
#' after 200 days is still a confirmed case, and
#' [get_latest_confirmed()][validation_counts] still counts it.
#'
#' `condition` is evaluated inside the data, like a [dplyr::filter()] expression,
#' so it can name any column -- including the generated `.delay`. Rows where it
#' comes out `NA` are **not** censored: a condition that cannot be evaluated is
#' not a condition that was met.
#'
#' `censor_reports()` and `censor_delays(to_delay = )` are the two that *do*
#' move a date, so they rebuild the object: `.delay` and `.report_num` are
#' recomputed, and `now` moves *forward* when a replacement lands after it,
#' never backwards -- `now` is where you are standing, not the last date in the
#' data. Nothing stops a replacement from landing before the event date; that is
#' a negative delay, and [validate_tbl_now()] says so.
#'
#' Any temporal-effect column that was materialised **on the report date**
#' (`.report_*`, from [compute_temporal_effects()]) is dropped by those two,
#' because it describes a date that has just moved; run
#' `compute_temporal_effects()` again to rebuild it. The `.event_*` ones are
#' kept -- the event dates did not move.
#'
#' @param x A `tbl_now` object. `censor_validation_delays_above()` requires one
#'   that carries a validation process (see [add_validation_date()][add]).
#'
#' @param condition An unquoted expression evaluated in `x`, as in
#'   [dplyr::filter()]. Rows where it is `TRUE` are censored.
#'
#' @param to_report The replacement report date for the matching rows: a single
#'   value, or one per row of `x`. Must match the class of the report column
#'   (a `Date`, or a number for a numeric axis). Defaults to `get_now(x)` --
#'   the case has not been reported as of now, which is the whole point of the
#'   censoring flag. `NULL` leaves the dates alone and only sets the flag.
#'
#' @param to_delay The replacement delay for the matching rows, in the object's
#'   event units; the report date becomes `event_date + to_delay`. A single
#'   number or one per row. `NULL` (the default) leaves the dates alone and only
#'   sets the flag. On a calendar axis it is rounded to a whole number of units:
#'   there is no such date as half a day later.
#'
#' @param max_delay Numeric. Delays strictly greater than this are censored, in
#'   the object's event units (reporting) or validation units (validation).
#'
#' @param verbose Logical. Whether to report how many rows were affected.
#'   Default `TRUE`.
#'
#' @returns
#' `censor_reports()`, `censor_delays()` and `censor_delays_above()` return the
#' `tbl_now` with its `is_censored_report` column updated, creating it when
#' absent, and with the report dates replaced where one was asked for.
#'
#' `censor_validation_delays_above()` returns the `tbl_now` with its
#' `is_censored_validation` column updated, creating it when absent.
#'
#' @seealso
#' [add_is_censored_report()][add] and [change_is_censored_report()][add] to set the flag by
#' hand, and [add_is_censored_validation()][add] for the validation axis;
#' [diagnose_validation_delay()] and [plot_delay_distribution()] to find
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
#' censored[[get_is_censored_report(censored)]]
#'
#' # The same rule written by hand, and capped at 60 days as well, so the
#' # 300-day outlier stops dominating the delay distribution.
#' capped <- censor_delays(tn, .delay > 60, to_delay = 60, verbose = FALSE)
#' capped$.delay
#'
#' ## ---- Reports that never arrived ---------------------------------------
#'
#' # A missing report date, and a system that codes "never" as a date in 2222.
#' messy <- data.frame(
#'   onset = as.Date("2020-01-01") + 0:3,
#'   reported = as.Date(c("2020-01-03", NA, "2222-02-22", "2020-01-06"))
#' )
#' messy_now <- suppressWarnings(tbl_now(messy,
#'   event_date = onset, report_date = reported,
#'   data_type = "linelist", units = "days", verbose = FALSE,
#'   now = as.Date("2020-01-10")
#' ))
#'
#' # Both are "not reported yet", so both become `now` and are flagged censored.
#' # (Wrapped because the object keeps warning about the dates being fixed.)
#' fixed <- suppressWarnings(censor_reports(messy_now,
#'   is.na(reported) | reported > as.Date("2100-01-01"),
#'   verbose = FALSE
#' ))
#' fixed[[get_report_date(fixed)]]
#' fixed[[get_is_censored_report(fixed)]]
#'
#' ## ---- The validation counterpart ----------------------------------------
#'
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
#' # That one is flagged; all five stay confirmed, and the date is kept.
#' flagged <- censor_validation_delays_above(flu, 30, verbose = FALSE)
#' flagged[[get_is_censored_validation(flagged)]]
#' table(flagged[["outcome"]])
#'
#' @name censoring
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

  group_columns <- dplyr::group_vars(x)
  x <- .tbl_now_regroup(.censor_mark(ungroup(x), is_too_long), group_columns)

  if (verbose) {
    cli::cli_inform(c(
      "i" = "Marked {sum(is_too_long)} report{?s} with delay > {max_delay} {get_event_units(x)}{?s} as censored.",
      "*" = "This delay is now an upper bound (is_censored_report)."
    ))
  }

  x
}

#' @rdname censoring
#' @export
censor_reports <- function(x, condition, to_report = get_now(x), verbose = TRUE) {
  .assert_tbl_now(x, "censor_reports")
  check_verbose(verbose)
  # The default reads `x`, and `x` is reassigned below. Force it while `x` is
  # still the object the caller passed in.
  force(to_report)

  matched <- .censor_condition(x, rlang::enquo(condition))
  report_column <- get_report_date(x)

  # `tbl_now()` and `add_is_censored_report()` both refuse a grouped object, so the
  # grouping comes off for the rebuild and goes back on afterwards.
  group_columns <- dplyr::group_vars(x)
  x <- .censor_replace_dates(
    ungroup(x),
    matched = matched, column = report_column, values = to_report,
    arg = "to_report", verbose = verbose
  )
  x <- .tbl_now_regroup(x, group_columns)

  if (verbose) {
    fate <- if (is.null(to_report)) {
      "Their delay is now a bound, not a measurement (is_censored_report)."
    } else {
      "Their report date was replaced and their delay is now a bound (is_censored_report)."
    }
    cli::cli_inform(c(
      "i" = "Censored {sum(matched)} report{?s}.",
      "*" = fate
    ))
  }

  x
}

#' @rdname censoring
#' @export
censor_delays <- function(x, condition, to_delay = NULL, verbose = TRUE) {
  .assert_tbl_now(x, "censor_delays")
  check_verbose(verbose)

  matched <- .censor_condition(x, rlang::enquo(condition))

  replacement <- NULL
  if (!is.null(to_delay)) {
    if (!is.numeric(to_delay) || anyNA(to_delay)) {
      cli::cli_abort("{.arg to_delay} must be a number (or one per row), not {.cls {class(to_delay)[1]}}.")
    }
    if (!length(to_delay) %in% c(1L, nrow(x))) {
      cli::cli_abort(
        "{.arg to_delay} must be length 1 or {nrow(x)} (one per row), not {length(to_delay)}."
      )
    }
    # The delay is measured from the event, so that is what the replacement
    # report date is built from -- in the EVENT units, as `.delay` is.
    rebuilt <- .reconstruct_date_from_delay(
      data.frame(
        .event = x[[get_event_date(x)]],
        .delay_value = rep(to_delay, length.out = nrow(x))
      ),
      known_col = ".event", delay_col = ".delay_value",
      units = get_event_units(x), new_col_name = ".report", direction = "add"
    )
    replacement <- rebuilt[[".report"]]
  }

  group_columns <- dplyr::group_vars(x)
  x <- .censor_replace_dates(
    ungroup(x),
    matched = matched, column = get_report_date(x), values = replacement,
    arg = "to_delay", verbose = verbose
  )
  x <- .tbl_now_regroup(x, group_columns)

  if (verbose) {
    fate <- if (is.null(to_delay)) {
      "Their delay is now a bound, not a measurement (is_censored_report)."
    } else {
      "Their report date was moved to match the new delay (is_censored_report)."
    }
    cli::cli_inform(c(
      "i" = "Censored {sum(matched)} report{?s}.",
      "*" = fate
    ))
  }

  x
}

#' Evaluate a censoring condition inside a `tbl_now`
#'
#' A condition that cannot be evaluated on a row (`NA`) is not a condition that
#' was met, so `NA` becomes `FALSE` rather than propagating into the flag.
#'
#' @param x A `tbl_now`.
#' @param quo The quosure captured from the user's `condition`.
#'
#' @return A logical vector of length `nrow(x)`.
#'
#' @keywords internal
#' @noRd
.censor_condition <- function(x, quo) {
  if (rlang::quo_is_missing(quo)) {
    cli::cli_abort(c(
      "{.arg condition} is required.",
      "i" = "It is a {.fn dplyr::filter} expression, e.g.
             {.code is.na(report_date)}."
    ))
  }

  matched <- rlang::eval_tidy(quo, data = .strip_tbl_now(x))

  if (!is.logical(matched)) {
    cli::cli_abort(
      "{.arg condition} must evaluate to a logical vector, not
       {.cls {class(matched)[1]}}."
    )
  }
  if (!length(matched) %in% c(1L, nrow(x))) {
    cli::cli_abort(
      "{.arg condition} must be length 1 or {nrow(x)} (one per row), not
       {length(matched)}."
    )
  }

  matched <- rep(matched, length.out = nrow(x))
  matched[is.na(matched)] <- FALSE
  matched
}

#' The getter, adder and default column for one censoring axis
#'
#' The two axes are the same idea twice over, so the code that sets a flag takes
#' the axis as an argument rather than existing twice. Attribute names stay
#' behind the getters, as everywhere else in the package.
#'
#' @param axis `"report"` or `"validation"`.
#'
#' @return A list with `get`, `add` and `column`.
#'
#' @keywords internal
#' @noRd
.censor_axis <- function(axis = c("report", "validation")) {
  axis <- match.arg(axis)
  if (axis == "report") {
    list(
      get = get_is_censored_report,
      add = add_is_censored_report,
      column = ".is_censored_report"
    )
  } else {
    list(
      get = get_is_censored_validation,
      add = add_is_censored_validation,
      column = ".is_censored_validation"
    )
  }
}

#' Merge a censoring flag into a `tbl_now`
#'
#' Never un-censors: a row that was already flagged stays flagged. Creates the
#' axis's default column when the object has no flag column yet.
#'
#' @param x A `tbl_now`.
#' @param rows A logical vector of length `nrow(x)`.
#' @param axis `"report"` (default) or `"validation"`.
#'
#' @return `x`, with that axis's censoring column set.
#'
#' @keywords internal
#' @noRd
.censor_mark <- function(x, rows, axis = "report") {
  spec <- .censor_axis(axis)
  marked <- .censor_mark_data(.strip_tbl_now(x), x, rows, axis = axis)
  x[[marked$column]] <- marked$data[[marked$column]]
  if (identical(marked$column, spec$get(x))) {
    return(x)
  }
  spec$add(x, marked$column)
}

#' Set the censoring column on a bare data frame
#'
#' Split out from [.censor_mark()] so that a function replacing dates *and*
#' flagging rows rebuilds the object once instead of twice -- two rebuilds mean
#' two runs of `validate_tbl_now()`, so the user is warned twice about the very
#' data problem they are in the middle of fixing.
#'
#' @param data A bare data frame taken from `x`.
#' @param x The `tbl_now` it came from (for the flag column's name).
#' @param rows A logical vector of length `nrow(data)`.
#' @param axis `"report"` (default) or `"validation"`.
#'
#' @return A list with `data` (the frame, flag column written) and `column`
#'   (its name).
#'
#' @keywords internal
#' @noRd
.censor_mark_data <- function(data, x, rows, axis = "report") {
  spec <- .censor_axis(axis)
  column <- spec$get(x)
  if (is.null(column) || !column %in% names(data)) {
    column <- spec$column
    data[[column]] <- rows
    return(list(data = data, column = column))
  }
  already_censored <- as.logical(data[[column]])
  already_censored[is.na(already_censored)] <- FALSE
  data[[column]] <- already_censored | rows
  list(data = data, column = column)
}

#' Replace report dates on the matching rows and flag them censored
#'
#' Writing a date column invalidates `.report_num` and `.delay`, so the object
#' has to be rebuilt; `now` moves forward when a replacement lands after it,
#' because `event <= report <= now` is what makes a `tbl_now` a `tbl_now`.
#'
#' @param x A `tbl_now`.
#' @param matched Logical vector of rows to censor.
#' @param column The date column to write.
#' @param values The replacement values, or `NULL` to leave the dates alone.
#' @param arg The argument the values came from, for the error message.
#' @param verbose Whether to say which stale columns were dropped.
#'
#' @return A `tbl_now`.
#'
#' @keywords internal
#' @noRd
.censor_replace_dates <- function(x, matched, column, values, arg,
                                  verbose = TRUE) {
  if (is.null(values)) {
    return(.censor_mark(x, matched))
  }

  current <- x[[column]]
  if (lubridate::is.Date(current) && !lubridate::is.Date(values)) {
    cli::cli_abort(
      "{.arg {arg}} must be a {.cls Date}, because {.val {column}} is one."
    )
  }
  if (!lubridate::is.Date(current) && !is.numeric(values)) {
    cli::cli_abort(
      "{.arg {arg}} must be numeric, because {.val {column}} is."
    )
  }
  if (!length(values) %in% c(1L, nrow(x))) {
    cli::cli_abort(
      "{.arg {arg}} must be length 1 or {nrow(x)} (one per row), not {length(values)}."
    )
  }

  replacement <- rep(values, length.out = nrow(x))[matched]

  # Writing a double into an integer column upcasts the whole column, and
  # `tbl_now()` then refuses the object because the two axes are no longer the
  # same type -- an error about the OTHER column, from a write to this one.
  if (is.integer(current) && is.numeric(replacement)) {
    if (!all(replacement == trunc(replacement), na.rm = TRUE)) {
      cli::cli_abort(c(
        "{.arg {arg}} must be a whole number: {.val {column}} is an integer axis.",
        "i" = "Convert the column to {.cls double} first if fractional times are
               what you mean."
      ))
    }
    replacement <- as.integer(replacement)
  }

  marked <- .censor_mark_data(.strip_tbl_now(x), x, matched)
  data <- marked$data
  data[[column]][matched] <- replacement

  # A day-of-week term computed on the REPORT date describes a date that has
  # just moved. Drop those columns rather than carry a stale answer -- and drop
  # only those: the event-date terms are still true.
  stale_effects <- grep(
    "^\\.report_", intersect(get_temporal_effect_cols(x), colnames(data)),
    value = TRUE
  )
  if (length(stale_effects) > 0) {
    data <- data[, setdiff(colnames(data), stale_effects), drop = FALSE]
    if (verbose) {
      cli::cli_inform(c(
        "i" = "Dropped computed temporal-effect column{?s} {.val {stale_effects}};
               recompute them with {.fn compute_temporal_effects}."
      ))
    }
  }

  new_now <- get_now(x)
  latest <- suppressWarnings(max(data[[column]], na.rm = TRUE))
  if (!is.na(latest) && is.finite(as.numeric(latest)) && latest > new_now) {
    new_now <- latest
  }

  .tbl_now_rebuild(x, data, now = new_now, is_censored_report = marked$column)
}
