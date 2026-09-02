#' Cases at a chosen point in the reporting process
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The same event date has more than one count, depending on when you look. A
#' week of dengue onsets might show 12 cases the day reporting starts, 40 a week
#' later, and 47 once everything has arrived. These functions let you pick which
#' of those numbers you want.
#'
#' @details
#' * `get_initial_reported_cases()` -- the count as **first** seen: the earliest
#'   report for that event date. This is what a dashboard would have shown you at
#'   the time, and it is always an undercount.
#' * `get_latest_reported_cases()` -- the count as **latest** seen: the most
#'   recent report. This is the current best estimate of what really happened,
#'   and it is what you score a nowcast against.
#' * `get_nth_reported_cases()` -- the count accumulated **within a given
#'   delay**. `r lifecycle::badge("experimental")` `delay = 0` gives the cases
#'   reported on the event date itself, `delay = 1` adds those reported one
#'   period later, and so on. `delay = Inf` is the same as
#'   `get_latest_reported_cases()`.
#'
#' The gap between the first and the latest count *is* the reporting delay
#' problem that nowcasting exists to solve.
#'
#' @param x A `tbl_now` object.
#'
#' @param delay A single non-negative number (or `Inf`) giving the maximum
#'   reporting delay, in event units, to include. Only used by
#'   `get_nth_reported_cases()`.
#'
#' @inheritParams validated_cases
#'
#' @returns A `count-cumulative` `tbl_now` with one row per event date (and
#' stratum, and grouping column), containing:
#' * the event-date column -- when the cases happened. Its numeric version is `.event_num`.
#' * the report-date column -- the report that was selected for that event date. Its numeric version is `.report_num`.
#' * `n` -- the number of cases reported for that event date at the selected point.
#' * `.delay` -- the delay of the selected report.
#' * any strata, covariate, censoring indicator and temporal-effect columns the
#'   object carried, plus the caller's grouping columns.
#'
#' The **validation** columns are not carried: the count pools over many
#' validation dates, so the result has no single one and does not pretend to.
#' `type = "by_type"` is the exception -- it keeps the outcome column, declared
#' as a covariate, because that is the whole point of the call and an undeclared
#' column is one [to_count()] would pool away. Use
#' [get_latest_validated_cases()][validated_cases] when you want the third date
#' on the result.
#'
#' @section Grouping is respected:
#' Unlike [to_count()], these functions **keep the caller's grouping** and answer
#' by it: the grouping columns join the event date and the strata as keys, and
#' come back on the result. That is what lets you ask for the latest count by a
#' **covariate** -- a column that matters but is not something you nowcast by --
#' which grouping is the only way to express.
#'
#' They can do this because they *select* a point in the process rather than
#' reshaping the object: one row in is still one case (or one cell) out.
#' `to_count()` cannot, and warns instead.
#'
#' @seealso
#' [get_latest_validated_cases()][validated_cases] and friends for the same idea
#' on the validation process; [to_count()] for the underlying data shapes;
#' [score_nowcast()], which uses the latest counts as truth;
#' [reporting_completeness()][nowcast_summary_components] for the same
#' information as a proportion.
#'
#' @examples
#' data(denguedat)
#' dengue <- tbl_now(denguedat,
#'   report_date = "report_week",
#'   event_date = "onset_week",
#'   strata = "gender",
#'   verbose = FALSE
#' )
#'
#' # What the surveillance system showed the very first time it reported each
#' # week -- an undercount, because the late reports had not arrived yet.
#' first <- get_initial_reported_cases(dengue)
#' first
#'
#' # What it shows now, after all the corrections.
#' latest <- get_latest_reported_cases(dengue)
#' latest
#'
#' # The difference between them is what a nowcast tries to predict.
#' sum(latest$n) - sum(first$n)
#'
#' # Everything known within two weeks of onset.
#' get_nth_reported_cases(dengue, delay = 2)
#'
#' # A grouping is answered by, not dropped.
#' dengue |>
#'   dplyr::group_by(gender) |>
#'   get_latest_reported_cases() |>
#'   dplyr::group_vars()
#'
#' @name get_latest_first
NULL

#' The values `type` may take
#'
#' `"total"` is every case the axis has, whatever the laboratory said (and is
#' the only one that means anything on an object with no validation process).
#' The three canonical outcomes filter to themselves, `"unknown"` picks the
#' cases whose outcome is `NA`, `"net"` is confirmed minus retracted, and
#' `"by_type"` reports each outcome on its own row instead of choosing one.
#'
#' @return A character vector of the allowed values.
#'
#' @keywords internal
#' @noRd
.case_types <- function() {
  c("total", "confirmed", "retracted", "pending", "unknown", "net", "by_type")
}

#' Resolve the `type` argument against the object it is being asked of
#'
#' @param x A `tbl_now`.
#' @param type The user's `type`.
#' @param axis `"report"` or `"validation"`.
#' @param fn The calling function, for messages.
#'
#' @return A single valid type, possibly demoted to `"total"`.
#'
#' @keywords internal
#' @noRd
.resolve_case_type <- function(x, type, axis, fn) {
  if (!is.character(type) || length(type) != 1L || is.na(type)) {
    # `cli` refuses a `{}` expression starting with a dot, so the vector is
    # built outside the call.
    allowed <- .case_types()
    cli::cli_abort(
      "{.arg type} must be a single string, one of {.val {allowed}}."
    )
  }
  type <- rlang::arg_match0(type, .case_types(), arg_nm = "type")

  if (identical(type, "total")) {
    return("total")
  }

  # Everything except "total" is a question about the outcome, and an object
  # with no validation process has no outcome to answer with. Pooling is what
  # it can honestly do, and it says so rather than returning a number that
  # looks like it was filtered.
  if (!has_validation(x)) {
    cli::cli_warn(c(
      "{.arg x} has no validation process, so {.code type = {.val {type}}}
       cannot be answered; counting every case instead.",
      "i" = "Attach one with {.fn add_validation_date} to count outcomes
             separately."
    ))
    return("total")
  }

  # A pending case is reported and still waiting, so it has no validation date
  # -- that is exactly what separates it from a resolution nobody wrote down.
  # There is no point on the validation axis at which to count it.
  if (identical(axis, "validation") && identical(type, "pending")) {
    cli::cli_abort(c(
      "{.fn {fn}} cannot count {.val pending} cases: a pending case has no
       validation date, so it has not arrived on this axis.",
      "i" = "Count them on the reporting axis instead:
             {.code get_latest_reported_cases(x, type = \"pending\")}."
    ))
  }

  type
}

#' Shared engine for the reported- and validated-cases getters
#'
#' Computes, per `event_date` (and strata, and the caller's grouping), the
#' cumulative count at the selected point on one axis: `"latest"` is the last
#' arrival, `"initial"` the first, and `"nth"` the last among those whose delay
#' is at most `delay`. All the work is done on a *declassed* data frame with
#' plain \pkg{dplyr} (so the expensive `tbl_now` dplyr methods are not
#' re-dispatched on every verb), and the resulting `tbl_now` is constructed
#' **once** at the end.
#'
#' Both axes measure their delay **from the event**, which is what makes them
#' comparable: `.delay` on the reporting axis, and
#' `.validation_num - .event_num` on the validation one. That second one is
#' deliberately *not* `.validation_delay`, which is the laboratory's turnaround
#' measured from the report.
#'
#' @param x A `tbl_now` object.
#' @param axis `"report"` or `"validation"`: which arrival is being selected on.
#' @param which One of `"latest"`, `"initial"` or `"nth"`.
#' @param delay Maximum delay from the event for `"nth"`.
#' @param type Which outcomes to count; see `.case_types()`.
#' @param fn The calling function, for messages.
#'
#' @return A `count-cumulative` `tbl_now`.
#'
#' @keywords internal
#' @noRd
.cases_at <- function(x, axis = c("report", "validation"),
                      which = c("latest", "initial", "nth"),
                      delay = NULL, type = "total", fn) {
  axis <- match.arg(axis)
  which <- match.arg(which)
  .assert_tbl_now(x, fn)

  if (identical(axis, "validation") && !has_validation(x)) {
    cli::cli_abort(c(
      "{.fn {fn}} needs a validation process, and {.arg x} has none.",
      "i" = "Attach one with {.fn add_validation_date}.",
      "i" = "For counts of everything reported, use
             {.fn get_latest_reported_cases}."
    ))
  }

  type <- .resolve_case_type(x, type, axis, fn)

  event_col      <- get_event_date(x)
  report_col     <- get_report_date(x)
  validation_col <- get_validation_date(x)
  type_col       <- get_validation_type(x)
  strata         <- get_strata(x)
  covariates     <- get_covariates(x)
  effects        <- get_temporal_effect_cols(x)
  data_type      <- get_data_type(x)
  count_in       <- get_case_count(x)
  count_out      <- count_in %||% "n"

  # The report-axis flag is a key on both axes: a delay that is only a bound is
  # not the same observation as one measured exactly. The validation-axis flag
  # joins it when that is the axis being selected on.
  censored <- if (identical(axis, "report")) {
    get_is_censored_report(x)
  } else {
    c(get_is_censored_report(x), get_is_censored_validation(x))
  }

  # Grouping is the caller's question, and these verbs SELECT rather than
  # reshape, so it becomes one more key and comes back at the end (#61).
  group_columns <- dplyr::group_vars(x)

  date_col <- if (identical(axis, "report")) report_col else validation_col

  # Declass once: everything below is plain dplyr on a tibble (fast).
  observations <- dplyr::as_tibble(.declass_tbl_now(ungroup(x)))

  # -- which rows this `type` counts, and with what sign ----------------------
  outcome <- if (identical(type, "total")) {
    NULL
  } else {
    as.character(observations[[type_col]])
  }
  keep <- rep(TRUE, nrow(observations))
  weight <- rep(1, nrow(observations))
  if (identical(type, "net")) {
    keep <- !is.na(outcome) & outcome %in% c("confirmed", "retracted")
    weight <- ifelse(!is.na(outcome) & outcome == "retracted", -1, 1)
  } else if (identical(type, "unknown")) {
    keep <- is.na(outcome)
  } else if (!type %in% c("total", "by_type")) {
    keep <- !is.na(outcome) & outcome == type
  }

  # An arrival needs a date. On the validation axis a row with none has not
  # arrived, so there is no point at which to count it.
  if (identical(axis, "validation")) {
    keep <- keep & !is.na(observations[[date_col]])
  }

  # -- and how far after the event they arrived -------------------------------
  axis_delay <- if (identical(axis, "report")) {
    observations[[".delay"]]
  } else {
    observations[[".validation_num"]] - observations[[".event_num"]]
  }
  if (identical(which, "nth")) {
    keep <- keep & !is.na(axis_delay) & axis_delay <= delay
  }

  observations <- observations[keep, , drop = FALSE]
  observations[[".case_weight"]] <- weight[keep]
  observations[[".case_size"]] <- if (is.null(count_in)) {
    rep(1, nrow(observations))
  } else {
    observations[[count_in]]
  }

  if (nrow(observations) == 0L) {
    .abort_no_cases(x, axis, which, delay, type, fn)
  }

  group_cols <- unique(c(
    event_col, censored, strata, effects, covariates, group_columns,
    if (identical(type, "by_type")) type_col
  ))
  cell_cols <- unique(c(group_cols, date_col))

  cells <- observations |>
    dplyr::summarise(
      .increment = sum(.data$.case_weight * .data$.case_size),
      # The report date rides along on the validation axis so the picked row
      # can say which report it belonged to. It is deliberately NOT a key
      # there: two reports resolved on the same day are one arrival, and
      # keying on the report would split them into two partial running totals.
      !!!.cases_carry_report(report_col, cell_cols),
      .by = dplyr::all_of(cell_cols)
    ) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(group_cols, date_col))))

  # Cumulative count at each arrival within a group. Cumulative input already
  # holds the running total, so it is used as-is.
  cells <- if (identical(data_type, "count-cumulative")) {
    dplyr::mutate(cells, .cumulative = .data$.increment)
  } else {
    dplyr::mutate(cells,
      .cumulative = cumsum(.data$.increment),
      .by = dplyr::all_of(group_cols)
    )
  }

  # Rows are already in arrival order within the group, so the target is the
  # first or the last of them. `slice_max()` would not do: on the validation
  # axis two reports can resolve on the same date, and it would pick one of the
  # two partial running totals.
  picked <- if (identical(which, "initial")) {
    dplyr::slice_head(cells, n = 1, by = dplyr::all_of(group_cols))
  } else {
    dplyr::slice_tail(cells, n = 1, by = dplyr::all_of(group_cols))
  }

  # On the validation axis the object being built carries a validation process,
  # so it needs an outcome column: without one `tbl_now()` invents `NA`s and
  # warns about them on every call. What that outcome IS depends on the
  # question. A filtered type is constant by construction; `by_type` is a key;
  # and `"total"` and `"net"` pool outcomes together, so the aggregate row
  # genuinely has none and says `NA` rather than picking one of the cases.
  if (identical(axis, "validation") && !is.null(type_col) &&
    !identical(type, "by_type")) {
    picked[[type_col]] <- switch(type,
      total = NA_character_,
      net = NA_character_,
      unknown = NA_character_,
      type
    )
  }

  carried <- unique(c(
    event_col, report_col, if (identical(axis, "validation")) validation_col,
    censored, strata, effects, covariates, group_columns,
    if (identical(axis, "validation")) type_col,
    if (identical(type, "by_type")) type_col
  ))

  result <- picked |>
    dplyr::mutate("{count_out}" := .data$.cumulative) |>
    dplyr::select(dplyr::all_of(c(carried, count_out))) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(
      c(event_col, strata, censored, covariates, group_columns)
    )))

  # `by_type` on the reporting axis has aggregated over the validation DATES,
  # so the object it returns has no validation process -- but the outcome
  # column is the whole point of the call, and an undeclared column is one
  # `to_count()` would pool away. A covariate is exactly the declaration for
  # "matters, but is not what you nowcast by".
  extra_covariates <- if (identical(type, "by_type") && identical(axis, "report")) {
    type_col
  } else {
    NULL
  }

  rebuild_args <- list(
    as.data.frame(result),
    event_date = event_col, report_date = report_col,
    strata = strata, covariates = unique(c(covariates, extra_covariates)),
    is_censored_report = get_is_censored_report(x),
    now = get_now(x), event_units = get_event_units(x),
    report_units = get_report_units(x), data_type = "count-cumulative",
    case_count = count_out, verbose = FALSE, force = TRUE,
    warn_non_uniqueness = FALSE, align_weeks = FALSE
  )
  if (identical(axis, "validation")) {
    rebuild_args <- c(rebuild_args, .validation_rebuild_args(x, result))
  }

  # Build the tbl_now once (regenerates .event_num / .report_num / .delay).
  out <- do.call(tbl_now, rebuild_args)
  attr(out, "temporal_effects") <- get_temporal_effects(x)
  attr(out, "computed_temporal_effect_cols") <-
    intersect(get_temporal_effect_cols(x), names(out))

  # The constructor appends the generated numeric columns; put them back next to
  # the dates so the column order matches the rest of the package.
  out <- dplyr::relocate(
    out, ".event_num", ".report_num",
    .after = dplyr::all_of(report_col)
  )

  .tbl_now_regroup(out, group_columns)
}

#' The extra `summarise()` expression that carries the report date
#'
#' Empty whenever the report date is already one of the cell's keys -- which it
#' always is on the reporting axis, and can be on the validation axis if the
#' caller grouped by it.
#'
#' @param report_col The report-date column name.
#' @param cell_cols The keys the cells are being built on.
#'
#' @return A named list of quosures, to be spliced into `summarise()`.
#'
#' @keywords internal
#' @noRd
.cases_carry_report <- function(report_col, cell_cols) {
  # On the reporting axis the report date IS the key, and so is a column the
  # caller grouped by. Summarising a `.by` column is an error, not a no-op.
  if (report_col %in% cell_cols) {
    return(list())
  }
  # `min(na.rm = TRUE)` on an all-`NA` Date returns `Inf`, which is not a date;
  # keep the column's own type in that case.
  expression <- rlang::quo({
    .report_values <- .data[[!!report_col]]
    if (all(is.na(.report_values))) {
      .report_values[1]
    } else {
      min(.report_values, na.rm = TRUE)
    }
  })
  stats::setNames(list(expression), report_col)
}

#' Explain an empty selection rather than failing inside `tbl_now()`
#'
#' `tbl_now()` refuses a zero-row data frame, and the error it gives ("`data` is
#' an empty data.frame") says nothing about the question that was asked. Every
#' way of selecting nothing here has a specific cause worth naming.
#'
#' @param x The `tbl_now` the question was asked of.
#' @param axis,which,delay,type,fn The call being answered.
#'
#' @return Never returns; called for the error.
#'
#' @keywords internal
#' @noRd
.abort_no_cases <- function(x, axis, which, delay, type, fn) {
  reasons <- character(0)

  if (identical(axis, "validation")) {
    validation_col <- get_validation_date(x)
    if (all(is.na(x[[validation_col]]))) {
      reasons <- c(reasons, "i" = paste0(
        "Every value of {.val ", validation_col, "} is {.code NA}: nothing has ",
        "been validated yet, so the validation axis is empty."
      ))
    }
  }
  if (!type %in% c("total", "by_type")) {
    reasons <- c(reasons, "i" = paste0(
      "No case has {.code type = {.val ", type, "}}."
    ))
  }
  if (identical(which, "nth")) {
    reasons <- c(reasons, "i" = paste0(
      "No case arrived within a delay of {.val ", delay, "}."
    ))
  }
  if (length(reasons) == 0) {
    reasons <- c("i" = "{.arg x} has no rows to count.")
  }

  cli::cli_abort(c(
    "{.fn {fn}} selected no cases, so there is nothing to return.",
    reasons,
    "i" = "{.fn diagnose} describes what the object does and does not contain."
  ))
}

#' @rdname get_latest_first
#' @export
get_latest_reported_cases <- function(x, type = "total") {
  .cases_at(x,
    axis = "report", which = "latest", type = type,
    fn = "get_latest_reported_cases"
  )
}

#' @rdname get_latest_first
#' @export
get_initial_reported_cases <- function(x, type = "total") {
  .cases_at(x,
    axis = "report", which = "initial", type = type,
    fn = "get_initial_reported_cases"
  )
}

#' @rdname get_latest_first
#' @export
get_nth_reported_cases <- function(x, delay, type = "total") {
  .check_getter_delay(delay)
  .cases_at(x,
    axis = "report", which = "nth", delay = delay, type = type,
    fn = "get_nth_reported_cases"
  )
}

#' Check the `delay` argument of the `get_nth_*` getters
#'
#' @param delay The value supplied.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
.check_getter_delay <- function(delay) {
  if (missing(delay) || length(delay) != 1 || !is.numeric(delay) ||
    is.na(delay) || delay < 0) {
    cli::cli_abort(
      "{.arg delay} must be a single non-negative number (or {.code Inf})."
    )
  }
  invisible(NULL)
}

# The validation axis ---------------------------------------------------------

#' Cases at a chosen point in the validation process
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The same three questions as [get_latest_reported_cases()][get_latest_first],
#' asked of the **third** date: not when the system heard about a case, but when
#' the laboratory settled it.
#'
#' * `get_initial_validated_cases()` -- the count as of the **first** result to
#'   come back for that event date.
#' * `get_latest_validated_cases()` -- the count as of the **most recent**
#'   result: everything settled so far.
#' * `get_nth_validated_cases()` -- the count settled **within a given delay of
#'   the event**.
#'
#' A case that is still `"pending"` has no validation date, so it has not
#' arrived on this axis and none of these count it. That is the point: the gap
#' between [get_latest_reported_cases()][get_latest_first] and
#' `get_latest_validated_cases()` is the backlog the laboratory still owes you.
#'
#' @param x A `tbl_now` with a validation process (see [add_validation_date()]).
#'
#' @param delay A single non-negative number (or `Inf`) giving the longest delay
#'   **from the event** to include, in the object's units. Only used by
#'   `get_nth_validated_cases()`.
#'
#' @param type Which cases to count. One of:
#'   \describe{
#'     \item{`"total"`}{(default) every case, whatever the outcome. On the
#'       validation axis that means every case that has been settled at all.}
#'     \item{`"confirmed"`, `"retracted"`, `"pending"`}{only the cases with that
#'       outcome. `"pending"` is a reporting-axis question only -- a pending case
#'       has no validation date -- and the validation getters refuse it.}
#'     \item{`"unknown"`}{the cases whose `validation_type` is `NA`: settled, but
#'       the data does not say which way.}
#'     \item{`"net"`}{confirmed **minus** retracted -- the running total as a
#'       surveillance system publishes it, which can go **down** when a case is
#'       withdrawn. This is the quantity a `count-cumulative` stream actually
#'       reports, and the one \pkg{diseasenowcasting}'s signed-increment
#'       (Skellam / SkNB) likelihood is built for; see
#'       [diseasenowcasting::confirmation_process()].}
#'     \item{`"by_type"`}{one row per outcome instead of one number: the
#'       outcome column joins the keys, so you get pending, confirmed and
#'       retracted side by side.}
#'   }
#'   On an object with no validation process anything but `"total"` warns and
#'   pools, because there is no outcome to filter on.
#'
#' @returns A `count-cumulative` `tbl_now` with one row per event date (and
#' stratum, grouping column, and outcome when `type = "by_type"`), carrying the
#' event, report and validation dates of the selected arrival, the generated
#' numeric columns, and the count.
#'
#' @section Which date the count is indexed by:
#'
#' By the **event date**, as every other `get_*_cases()` function is. A case
#' confirmed three weeks after onset still belongs to the week it began. If you
#' want counts by validation date instead, group on `get_validation_date(x)`
#' yourself -- that is a different question (how busy was the laboratory) and
#' this package does not silently answer it.
#'
#' @section Which delay `get_nth_validated_cases()` counts:
#'
#' The delay **from the event**, so that `get_nth_reported_cases(x, 7)` and
#' `get_nth_validated_cases(x, 7)` describe the same seven days and can be read
#' against each other. It is deliberately *not* `.validation_delay`, which is
#' the laboratory's turnaround measured from the report. [diagnose_drift()] and
#' [summary()][summary.tbl_now] make the same choice for the same reason.
#'
#' @section Grouping is respected:
#' As with the reporting-axis getters: the caller's grouping becomes a key and
#' comes back on the result. See [get_latest_reported_cases()][get_latest_first].
#'
#' @seealso
#' [get_latest_reported_cases()][get_latest_first] for the same counts on the
#' reporting process; [add_validation_date()][add] to attach a validation;
#' [validation_delay] for how long resolution takes;
#' [plot_validation_status()] to see confirmed, retracted and pending over time.
#'
#' @examples
#' cases <- data.frame(
#'   onset = as.Date("2021-01-04") + c(0, 0, 1, 1, 2),
#'   visit = as.Date("2021-01-05") + c(0, 0, 1, 1, 2),
#'   result = as.Date("2021-01-06") + c(0, 0, 1, 1, 2),
#'   outcome = c("confirmed", "retracted", "confirmed", "confirmed", "retracted")
#' )
#' flu <- tbl_now(cases,
#'   event_date = onset, report_date = visit,
#'   validation_date = result, validation_type = outcome,
#'   data_type = "linelist", verbose = FALSE
#' )
#'
#' # Three answers to "how many cases were there?".
#' get_latest_reported_cases(flu) # everything reported
#' get_latest_validated_cases(flu, type = "confirmed") # only the positives
#' get_latest_validated_cases(flu, type = "net") # positives minus withdrawals
#'
#' # Every outcome side by side.
#' get_latest_validated_cases(flu, type = "by_type")
#'
#' # And the same question asked earlier in the process: what had come back by
#' # the first result, and within two days of onset.
#' get_initial_validated_cases(flu)
#' get_nth_validated_cases(flu, delay = 2)
#'
#' @name validated_cases
NULL

#' @rdname validated_cases
#' @export
get_latest_validated_cases <- function(x, type = "total") {
  .cases_at(x,
    axis = "validation", which = "latest", type = type,
    fn = "get_latest_validated_cases"
  )
}

#' @rdname validated_cases
#' @export
get_initial_validated_cases <- function(x, type = "total") {
  .cases_at(x,
    axis = "validation", which = "initial", type = type,
    fn = "get_initial_validated_cases"
  )
}

#' @rdname validated_cases
#' @export
get_nth_validated_cases <- function(x, delay, type = "total") {
  .check_getter_delay(delay)
  .cases_at(x,
    axis = "validation", which = "nth", delay = delay, type = type,
    fn = "get_nth_validated_cases"
  )
}
