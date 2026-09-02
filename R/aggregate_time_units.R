#' Coarsen a `tbl_now` onto a bigger time unit
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Daily surveillance data is often too sparse to nowcast: most (event date,
#' report date) cells hold a zero or a one, and the delay distribution is mostly
#' noise. The usual fix is to work in weeks instead. `aggregate_time_units()`
#' does that in one call -- it moves every date onto the coarser grid, adds the
#' counts up, and returns a `tbl_now` that *knows* it is now weekly, so
#' `.delay`, the converters and the models all count in weeks from then on.
#'
#' @details
#' Each date is replaced by the **start of the period it falls in**: the Sunday
#' that begins its epidemiological week, the first of its month, the first of
#' January of its year (`label = "end"` names the period by its last day
#' instead). Weeks go through the same epi/ISO machinery [align_weeks()] uses,
#' so `type` and `align_on_day` mean exactly what they mean there.
#'
#' What happens to the rows depends on the data type:
#'
#' * **`linelist`** -- one row is still one case; only the dates move.
#' * **`count-incidence`** -- rows that land in the same (event, report) cell are
#'   summed.
#' * **`count-cumulative`** -- cumulative totals are **not** additive, so the
#'   series is de-accumulated to increments first, aggregated, and accumulated
#'   again on the new grid.
#'
#' Two consequences worth knowing:
#'
#' * **Weeks do not nest inside months.** Aggregating daily data to weeks and
#'   *then* to months is not the same as going straight to months: the second
#'   pass sees only the week's label, so a week beginning 31 December lands in
#'   December even though most of its cases happened in January. Aggregate once,
#'   to the unit you actually want.
#' * An `NA` count means *not yet observed*, so a period containing one has an
#'   unknown total, not a total that quietly leaves it out. Use
#'   [complete_zeroes()] first if the `NA`s are really zeroes.
#'
#' Any temporal-effect **columns** that were materialised by
#' [compute_temporal_effects()] are dropped, because a day-of-week term computed
#' on daily dates is meaningless once those dates are weeks. The lazy
#' [temporal_effects()] spec is kept, so `compute_temporal_effects()` will
#' rebuild them on the new grid.
#'
#' @section Aggregating one axis only:
#' `axes` exists because the two axes do not always move together: a system may
#' record the day a specimen was taken but only publish weekly report batches.
#' Two things follow:
#'
#' * `tbl_now()` requires the report axis to be **at least as coarse** as the
#'   event axis, so aggregating only the event axis of a daily object is
#'   refused. Aggregate the report axis too, or use `axes = "all"`.
#' * A week named by the day it *starts* sits before every date inside it, so
#'   coarsening a **later** axis alone -- the report against a daily event, or
#'   the validation against a daily report -- with `label = "start"` produces
#'   negative delays, and [validate_tbl_now()] warns. Use `label = "end"`: a
#'   report that arrived somewhere in week *W* is known by the end of *W*, which
#'   is the honest bound. When the axes move together the labelling cancels out
#'   and either choice gives the same delays.
#'
#' @param x A [tbl_now()] object.
#'
#' @param to Character. The unit to aggregate **to**: `"weeks"` (the default),
#'   `"months"`, `"years"` or `"days"`. It must be at least as coarse as the
#'   units of every axis being aggregated -- there is no un-aggregating, so
#'   asking a weekly object for `"days"` is an error rather than a guess.
#'
#' @param axes Character. Which time axes to aggregate: `"all"` (the default,
#'   meaning the event and report axes plus the validation axis when there is
#'   one), or any of `"event"`, `"report"` and `"validation"`. The unit they are
#'   aggregated to is `to`.
#'
#' @param label Character. Which end of the period names it: `"start"` (the
#'   default -- the Sunday of the epi week, the first of the month) or `"end"`
#'   (its last day). It only changes the *labels*, never which rows are pooled
#'   together, but see *Aggregating one axis only* for when it matters.
#'
#' @param align_on_day,type Passed to [align_weeks()] when `to = "weeks"`:
#'   the weekday to snap to (ISO numbering, **1 = Monday**, default `7` =
#'   Sunday) and the week convention, `"epi"` (default) or `"iso"`.
#'
#' @param verbose Logical. Whether to report what was aggregated. Default
#'   `TRUE`.
#'
#' @return A `tbl_now` on the coarser grid, with `event_units`, `report_units`
#' and `validation_units` updated for the axes that were aggregated, and `now`
#' moved onto the new grid.
#'
#' @seealso
#' [align_weeks()], which snaps weekly dates to a common weekday without
#' changing the units or the counts; [to_count()] to change data type without
#' touching the dates; [complete_zeroes()] for the cells the coarser grid still
#' leaves empty; [tbl_now()]'s `units` argument to declare the units up front.
#'
#' @examples
#' # A sparse daily line list: one or two cases a day.
#' df <- data.frame(
#'   onset = as.Date("2024-01-01") + c(0, 1, 3, 8, 9, 15),
#'   reported = as.Date("2024-01-01") + c(2, 2, 5, 9, 12, 16),
#'   sex = c("F", "M", "F", "M", "F", "M")
#' )
#' daily <- tbl_now(df,
#'   event_date = onset, report_date = reported, strata = sex,
#'   data_type = "linelist", units = "days", verbose = FALSE
#' )
#' get_event_units(daily)
#'
#' # The same cases, on a weekly grid: every date moves to the Sunday that
#' # starts its epidemiological week, and the delays are whole weeks.
#' weekly <- aggregate_time_units(daily, to = "weeks", verbose = FALSE)
#' get_event_units(weekly)
#' weekly[[get_event_date(weekly)]]
#' weekly$.delay
#'
#' # Count data is added up rather than merely relabelled.
#' counts <- to_count(daily, to = "count-incidence")
#' sum(counts$n)
#' sum(aggregate_time_units(counts, to = "months", verbose = FALSE)$n)
#'
#' @export
aggregate_time_units <- function(x,
                                 to = "weeks",
                                 axes = "all",
                                 label = c("start", "end"),
                                 align_on_day = 7,
                                 type = "epi",
                                 verbose = TRUE) {
  .assert_tbl_now(x, "aggregate_time_units")
  check_time_units(to, "to", allowed = c("days", "weeks", "months", "years"))
  label <- rlang::arg_match(label)
  check_verbose(verbose)

  axes <- .aggregate_resolve_axes(x, axes)

  # Groups are the caller's, not ours: `to_count()` and the rebuild both
  # ungroup, so remember them and put them back at the end.
  group_columns <- dplyr::group_vars(x)
  x <- ungroup(x)

  axis_units <- c(
    event = get_event_units(x),
    report = get_report_units(x),
    validation = get_validation_units(x) %||% NA_character_
  )

  changed <- character(0)
  for (axis in axes) {
    current <- axis_units[[axis]]
    if (identical(current, "numeric")) {
      cli::cli_abort(c(
        "The {axis} axis is {.val numeric}, so it has no calendar to aggregate on.",
        "i" = "Convert it to {.cls Date} before aggregating, or drop {.val {axis}}
               from {.arg axes}."
      ))
    }
    if (.time_unit_rank(to) < .time_unit_rank(current)) {
      cli::cli_abort(c(
        "Cannot aggregate the {axis} axis from {.val {current}} to {.val {to}}.",
        "x" = "{.val {to}} is finer than {.val {current}}, and aggregation only
               ever goes one way.",
        "i" = "Did you mean {.code to = {.val {current}}} or coarser?"
      ))
    }
    if (!identical(to, current)) changed <- c(changed, axis)
  }

  new_units <- axis_units
  new_units[changed] <- to

  # `tbl_now()` refuses a report axis finer than the event axis, and would do so
  # from inside `time_cols_to_numeric()` -- an error about arguments the caller
  # never wrote. Say it here, about the argument they did write.
  if (!anyNA(c(new_units[["event"]], new_units[["report"]])) &&
    !"numeric" %in% c(new_units[["event"]], new_units[["report"]]) &&
    .time_unit_rank(new_units[["report"]]) < .time_unit_rank(new_units[["event"]])) {
    cli::cli_abort(c(
      "Aggregating only the {.val {setdiff(axes, 'report')}} ax{?is/es} would leave
       the event axis ({.val {new_units[['event']]}}) coarser than the report axis
       ({.val {new_units[['report']]}}).",
      "x" = "A report cannot be recorded on a finer grid than the event it reports.",
      "i" = "Use {.code axes = \"all\"}, or add {.val report} to {.arg axes}."
    ))
  }

  if (length(changed) == 0) {
    if (verbose) {
      cli::cli_alert_info("Every requested axis is already in {.val {to}}; nothing to do.")
    }
    return(.tbl_now_regroup(x, group_columns))
  }

  # Cumulative totals cannot be summed across cells, so de-accumulate on the OLD
  # grid, aggregate the increments, and accumulate again on the new one.
  original_type <- get_data_type(x)
  if (original_type == "count-cumulative") {
    x <- to_count(x, to = "count-incidence")
  }

  date_columns <- c(
    event = get_event_date(x),
    report = get_report_date(x),
    validation = get_validation_date(x) %||% NA_character_
  )

  data <- .strip_tbl_now(x)
  for (axis in changed) {
    column <- date_columns[[axis]]
    data[[column]] <- .tbl_now_floor_dates(
      data[[column]],
      to = to, label = label, align_on_day = align_on_day, type = type,
      axis = axis
    )
  }

  # A day-of-week term computed on daily dates says nothing about a week. Drop
  # the materialised columns; the lazy spec is carried over by the rebuild.
  stale_effects <- intersect(get_temporal_effect_cols(x), colnames(data))
  if (length(stale_effects) > 0) {
    data <- data[, setdiff(colnames(data), stale_effects), drop = FALSE]
    if (verbose) {
      cli::cli_alert_info(
        "Dropped computed temporal-effect column{?s} {.val {stale_effects}};
         recompute them with {.fn compute_temporal_effects}."
      )
    }
  }

  rebuilt <- .tbl_now_rebuild(
    x, data,
    now = .aggregate_new_now(x, data, date_columns, to, label, align_on_day, type),
    event_units = new_units[["event"]],
    report_units = new_units[["report"]],
    t_effects = character(0),
    validation_units = if (has_validation(x)) new_units[["validation"]] else NULL
  )

  # Rows that landed in the same cell are one cell now. A linelist keeps one row
  # per case, so there is nothing to collapse there.
  if (original_type != "linelist") {
    rebuilt <- to_count(rebuilt, to = "count-incidence")
    if (original_type == "count-cumulative") {
      rebuilt <- to_count(rebuilt, to = "count-cumulative")
    }
  }

  if (verbose) {
    cli::cli_alert_info(
      "Aggregated the {.val {changed}} ax{?is/es} to {.val {to}}
       ({cli::qty(nrow(x))}{nrow(x)} row{?s} -> {nrow(rebuilt)})."
    )
  }

  .tbl_now_regroup(rebuilt, group_columns)
}

#' Which time axes an `aggregate_time_units()` call asked for
#'
#' @param x A `tbl_now`.
#' @param axes The user's `axes` argument.
#'
#' @return A character vector of `"event"`, `"report"` and/or `"validation"`.
#'
#' @keywords internal
#' @noRd
.aggregate_resolve_axes <- function(x, axes) {
  valid <- c("all", "event", "report", "validation")
  if (!is.character(axes) || length(axes) == 0 || anyNA(axes)) {
    cli::cli_abort("{.arg axes} must be a character vector of {.val {valid}}.")
  }
  unknown <- setdiff(axes, valid)
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "Unknown {.arg axes} value{?s} {.val {unknown}}.",
      "i" = "{.arg axes} names the time axes to aggregate: {.val {valid}}.",
      "*" = "The unit to aggregate them to is {.arg to}."
    ))
  }

  if ("all" %in% axes) {
    return(c("event", "report", if (has_validation(x)) "validation"))
  }

  if ("validation" %in% axes && !has_validation(x)) {
    cli::cli_abort(c(
      "{.code axes = \"validation\"} needs a validation process, and {.arg x} has none.",
      "i" = "Attach one with {.fn add_validation_date}, or aggregate the
             {.val event} and {.val report} axes instead."
    ))
  }

  unique(axes)
}

#' The `now` an aggregated object should carry
#'
#' `now` moves onto the new grid with everything else, but flooring only moves
#' dates *backwards*, so an axis that was left alone can end up holding a date
#' later than the floored `now`. Taking the maximum keeps
#' `event <= report <= validation <= now` true by construction.
#'
#' @param x The (ungrouped) `tbl_now` before aggregation.
#' @param data The aggregated data frame.
#' @param date_columns Named character vector of the three date column names.
#' @param to,label,align_on_day,type As in [aggregate_time_units()].
#'
#' @return A single date.
#'
#' @keywords internal
#' @noRd
.aggregate_new_now <- function(x, data, date_columns, to, label, align_on_day, type) {
  now <- get_now(x)
  candidates <- if (lubridate::is.Date(now)) {
    .tbl_now_floor_dates(now,
      to = to, label = label, align_on_day = align_on_day, type = type,
      axis = "now"
    )
  } else {
    now
  }

  for (column in stats::na.omit(date_columns)) {
    if (!column %in% colnames(data)) next
    observed <- suppressWarnings(max(data[[column]], na.rm = TRUE))
    if (!is.na(observed) && is.finite(as.numeric(observed))) {
      candidates <- c(candidates, observed)
    }
  }

  max(candidates)
}

#' Move dates to the start of the period they fall in
#'
#' Weeks go through [align_weeks()] so there is one implementation of the
#' epi/ISO convention in the package; months and years are a plain
#' [lubridate::floor_date()]. Only the distinct dates are mapped, which matters
#' on a line list where the same day appears thousands of times.
#'
#' @param dates A `Date` vector.
#' @param to `"days"`, `"weeks"`, `"months"` or `"years"`.
#' @param label `"start"` or `"end"` of the period.
#' @param align_on_day,type As in [align_weeks()].
#' @param axis The axis being aggregated, for the error message.
#'
#' @return A `Date` vector the same length as `dates`.
#'
#' @keywords internal
#' @noRd
.tbl_now_floor_dates <- function(dates, to, label = "start", align_on_day = 7,
                                 type = "epi", axis = "event") {
  if (!lubridate::is.Date(dates)) {
    cli::cli_abort(
      "The {axis} date column must be a {.cls Date} to be aggregated, not a
       {.cls {class(dates)[1]}}."
    )
  }
  if (identical(to, "days")) {
    return(dates)
  }
  if (to %in% c("months", "years")) {
    unit <- sub("s$", "", to)
    return(if (label == "start") {
      lubridate::floor_date(dates, unit = unit)
    } else {
      lubridate::ceiling_date(dates, unit = unit) - 1
    })
  }

  # Weeks: map the distinct dates only, then look the answers back up. `NA`s go
  # nowhere near `week_2_date()` -- it builds a calendar from `min(year)`, and
  # an `NA` year makes that `as.Date(NA)`, which errors rather than propagating.
  distinct_dates <- unique(dates[!is.na(dates)])
  if (length(distinct_dates) == 0) {
    return(dates)
  }
  mapped <- align_weeks(
    data.frame(.tbl_now_date = distinct_dates),
    date_col = ".tbl_now_date",
    align_on_day = align_on_day, type = type,
    new_date_col = ".tbl_now_aligned"
  )
  aligned <- mapped[[".tbl_now_aligned"]][match(dates, mapped[[".tbl_now_date"]])]
  if (label == "end") aligned <- aligned + 6
  aligned
}

#' Rank the time units from finest to coarsest
#'
#' @param units One of `"days"`, `"weeks"`, `"months"`, `"years"`.
#'
#' @return An integer; `NA` for anything else.
#'
#' @keywords internal
#' @noRd
.time_unit_rank <- function(units) {
  match(units, c("days", "weeks", "months", "years"))
}
