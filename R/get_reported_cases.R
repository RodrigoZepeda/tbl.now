#' Reported cases at a chosen point in the reporting process
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
#'   reporting delay, in report units, to include. Only used by
#'   `get_nth_reported_cases()`.
#'
#' @returns A `count-cumulative` `tbl_now` with one row per event date (and
#' stratum), containing:
#' * the event-date column -- when the cases happened. Its numeric version is `.event_num`.
#' * the report-date column -- the report that was selected for that event date. Its numeric version is `.report_num`.
#' * `n` -- the number of cases reported for that event date at the selected point.
#' * `.delay` -- the delay of the selected report.
#' * any strata, censoring indicator and temporal-effect columns the object carried.
#'
#' @seealso
#' [get_latest_confirmed()][validation_counts] and friends for the same idea on
#' the validation process; [to_count()] for the underlying data shapes;
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
#' @name get_latest_first
NULL

#' Shared engine for the reported-cases getters
#'
#' Computes, per `event_date` (and grouping columns), the cumulative count at the
#' selected report (`"latest"` = max report, `"initial"` = min report, `"nth"` =
#' max report among those with delay `<= delay`). All the work is done on a
#' *declassed* data frame with plain \pkg{dplyr} (so the expensive `tbl_now`
#' dplyr methods are not re-dispatched on every verb), and the resulting
#' `tbl_now` is constructed **once** at the end.
#'
#' @param x A `tbl_now` object.
#' @param which One of `"latest"`, `"initial"` or `"nth"`.
#' @param delay Maximum delay for `"nth"` (in report units).
#'
#' @return A `count-cumulative` `tbl_now`.
#'
#' @keywords internal
#' @noRd
.reported_cases_at <- function(x, which = c("latest", "initial", "nth"), delay = NULL) {
  which <- match.arg(which)
  if (!inherits(x, "tbl_now")) {
    cli::cli_abort("Object x is not a `tbl_now`")
  }

  event_col  <- get_event_date(x)
  report_col <- get_report_date(x)
  strata     <- get_strata(x)
  covariates <- get_covariates(x)
  censored   <- get_is_censored(x)
  effects    <- get_temporal_effect_cols(x)
  data_type  <- get_data_type(x)
  count_in   <- get_case_count(x)
  count_out  <- if (is.null(count_in)) "n" else count_in

  group_cols <- c(event_col, censored, strata, effects, covariates)
  cell_cols  <- c(group_cols, report_col, ".delay")

  # Declass once: everything below is plain dplyr on a tibble (fast).
  observations <- dplyr::as_tibble(.declass_tbl_now(ungroup(x)))

  # Incremental count per reporting cell.
  if (data_type == "linelist") {
    cells <- observations |>
      dplyr::summarise(.increment = dplyr::n(), .by = dplyr::all_of(cell_cols))
  } else {
    cells <- observations |>
      dplyr::summarise(
        .increment = sum(.data[[count_in]]), .by = dplyr::all_of(cell_cols)
      )
  }

  # Cumulative count at each report within a group. Cumulative input already
  # holds the running total, so it is used as-is.
  if (data_type == "count-cumulative") {
    cells <- dplyr::mutate(cells, .cumulative = .data$.increment)
  } else {
    cells <- cells |>
      dplyr::arrange(dplyr::across(dplyr::all_of(c(group_cols, report_col)))) |>
      dplyr::mutate(.cumulative = cumsum(.data$.increment), .by = dplyr::all_of(group_cols))
  }

  if (which == "nth") {
    cells <- dplyr::filter(cells, .data$.delay <= delay)
  }

  # Select the target report per group.
  picked <- if (which == "initial") {
    dplyr::slice_min(cells, .data[[report_col]], n = 1, with_ties = FALSE,
      by = dplyr::all_of(group_cols)
    )
  } else {
    dplyr::slice_max(cells, .data[[report_col]], n = 1, with_ties = FALSE,
      by = dplyr::all_of(group_cols)
    )
  }

  result <- picked |>
    dplyr::mutate("{count_out}" := .data$.cumulative) |>
    dplyr::select(dplyr::all_of(
      c(event_col, report_col, censored, strata, effects, covariates, count_out)
    )) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(
      c(event_col, strata, censored, covariates)
    )))

  # Build the tbl_now once (regenerates .event_num / .report_num / .delay).
  out <- tbl_now(
    as.data.frame(result),
    event_date = event_col, report_date = report_col,
    strata = strata, covariates = covariates, is_censored = censored,
    now = get_now(x), event_units = get_event_units(x),
    report_units = get_report_units(x), data_type = "count-cumulative",
    case_count = count_out, verbose = FALSE, force = TRUE,
    warn_non_uniqueness = FALSE, align_weeks = FALSE
  )
  attr(out, "temporal_effects") <- get_temporal_effects(x)
  attr(out, "computed_temporal_effect_cols") <-
    intersect(get_temporal_effect_cols(x), names(out))

  # The constructor appends the generated numeric columns; put them back next to
  # the dates so the column order matches the rest of the package.
  dplyr::relocate(out, ".event_num", ".report_num", .after = dplyr::all_of(report_col))
}

#' @rdname get_latest_first
#' @export
get_latest_reported_cases <- function(x) {
  .reported_cases_at(x, which = "latest")
}

#' @rdname get_latest_first
#' @export
get_initial_reported_cases <- function(x) {
  .reported_cases_at(x, which = "initial")
}

#' @rdname get_latest_first
#' @export
get_nth_reported_cases <- function(x, delay) {
  if (missing(delay) || length(delay) != 1 || !is.numeric(delay) ||
      is.na(delay) || delay < 0) {
    cli::cli_abort("{.arg delay} must be a single non-negative number (or {.code Inf}).")
  }
  .reported_cases_at(x, which = "nth", delay = delay)
}
