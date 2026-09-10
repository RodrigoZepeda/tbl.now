#' Fill in the days when nothing was reported
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Surveillance data records what happened, not what didn't. If no dengue case
#' with onset on 3 January was reported on 5 January, there is simply no row for
#' that combination -- which is *not* the same as a row saying zero, even though
#' it means the same thing.
#'
#' Most nowcasting models need the difference spelled out. They work on a
#' complete rectangle of (event date x report date) cells, and a missing cell is
#' ambiguous: it could be a genuine zero, or a delay so long the report has not
#' arrived yet. `complete_zeroes()` writes the genuine zeros in explicitly, for
#' every stratum, leaving only the not-yet-reported cells absent.
#'
#' @details
#' Zeros are only filled where a report *could* have arrived: cells with a report
#' date on or before the event date's `now`, and within `max_delay`. Filling
#' beyond that would invent observations from the future.
#'
#' ## Rows with a missing date
#'
#' A row whose event or report date is `NA` has no cell on the rectangle, so it
#' takes no part in the grid: the bounds (`max_delay`, the first and last event
#' date, the last report date) are all computed ignoring it. It is still a case,
#' though, so it is **carried through unchanged** rather than dropped -- use
#' [censor_reports()] to give it a bound, or `dplyr::filter()` to remove it, if
#' you would rather it were on the grid or gone. Only an object in which *every*
#' row is missing one of the two dates is refused, because then there is no grid
#' to complete at all.
#'
#' @param x A `tbl_now` object.
#' @param max_delay Maximum delay to fill. For example if set to 5 it will complete
#' with 0's all reports with delays 0 to 4. But will not fill other delays (say 6)
#' @param until Event date to complete up to. `NULL` (the default) completes to
#'   whichever is later, the object's [get_now()] or the last event date present
#'   in the data. Completing only up to the last *observed* event date would
#'   leave a gap precisely at the `now` edge, because an event date with no
#'   reports at all does not appear in the data; several downstream converters
#'   build their time grid from the rows they are given and would silently stop
#'   short. A supplied `until` is never allowed to truncate below the data, and
#'   has no effect beyond the `now`: an event date later than the `now` cannot
#'   carry any report on or before it, so no row would survive for it.
#'
#' ## Temporal effects
#'
#' If `x` arrived with materialised [temporal_effects()] columns, they are
#' **recomputed on the completed grid** before the result is returned, so the
#' rows this function adds carry their own calendar effects rather than `NA`.
#' A lazy specification that has not been computed yet stays lazy.
#'
#' @return A `tbl_now` object with the same columns as `x`, plus the rows that
#' were implicitly zero, carrying `0` in the `case_count` column. Explicit
#' missing counts in the input remain `NA`; only cells created by
#' `complete_zeroes()` are filled. The data type is preserved, as are any
#' computed temporal-effect columns (recomputed over the new rows).
#'
#' @seealso
#' [to_count()] for the data shapes this operates on;
#' [censor_reporting_delays_above()] for the opposite problem, delays that are too long;
#' [diagnose_missing()] and [diagnose_truncation()] to find the gaps first;
#' [plot_reporting_triangle()] to see the rectangle being filled.
#'
#' @examples
#' ndata <- dplyr::tibble(
#'   event = rep(c(
#'     as.Date("2020/01/01"), as.Date("2020/01/01"),
#'     as.Date("2020/01/02"), as.Date("2020/01/04"),
#'     as.Date("2020/01/04")
#'   ), 2),
#'   report = rep(c(
#'     as.Date("2020/01/01"), as.Date("2020/01/02"),
#'     as.Date("2020/01/02"), as.Date("2020/01/04"),
#'     as.Date("2020/01/05")
#'   ), 2),
#'   n = rpois(10, lambda = 5),
#'   sex = c(rep("Male", 5), rep("Female", 5))
#' )
#' ndata <- tbl_now(ndata,
#'   event_date = event, report_date = report,
#'   verbose = FALSE, strata = sex, case_count = n, data_type = "count-incidence"
#' )
#'
#' # Nothing happened on 2020-01-03, so the data has no row for it at all.
#' sort(unique(ndata$event))
#'
#' ## complete_zeroes() writes that absence down as an explicit zero, for every
#' # stratum, so a model can tell "no cases" from "not reported yet".
#' filled <- complete_zeroes(ndata)
#' sort(unique(filled$event))
#' nrow(ndata)
#' nrow(filled)
#'
#' # Also works for count-cumulative
#' ndata |>
#'   to_count("count-cumulative") |>
#'   complete_zeroes() |>
#'   dplyr::arrange(event, sex, report)
#'
#' @export
complete_zeroes <- function(x, max_delay = NULL, until = NULL) {
  # Grouping has to come off FIRST. Every bound below is a `filter()`/`distinct()`
  # /`pull()` that a grouping silently turns into one value PER GROUP -- so
  # `min_event` came back length 2 and the date grid was built from a vector.
  # The grid is a property of the object, not of how the caller happened to
  # group it, so it goes back on at the end.
  group_columns <- dplyr::group_vars(x)
  if (length(group_columns) > 0) x <- ungroup(x)

  if (!is_tbl_now(x)) {
    cli::cli_abort(
      "Object `x` should be a `tbl.now`. See {.help tbl_now} on how to create one."
    )
  }

  if (get_data_type(x) == "linelist") {
    cli::cli_abort(c(
      "Can only complete {.val count-incidence} or {.val count-cumulative} data.",
      "i" = "A line list has one row per case, so a week with zero cases cannot \
             be represented in it at all.",
      "i" = "Convert first with {.code to_count(x, to = \"count-incidence\")}."
    ))
  }

  # Every bound below is computed with `na.rm = TRUE`, and none of them may be
  # `NA`. A row whose event or report date is missing cannot be placed on the
  # grid at all -- it has no cell -- but it is still a case, so it is carried
  # through untouched instead of setting the bound to `NA` and taking the whole
  # object with it. Without this, one missing report date made `max_delay` `NA`
  # and `seq(0, NA)` aborted with "'to' must be a finite number" (#66); one
  # missing event date made `report_bound` `NA` and the final filter dropped
  # every row a function meant to ADD zeroes was given.
  event_values <- x |> dplyr::pull(get_event_date(x))
  delay_values <- x |> dplyr::pull(".delay")
  report_values <- x |> dplyr::pull(get_report_date(x))

  if (all(is.na(event_values)) || all(is.na(report_values))) {
    cli::cli_abort(c(
      "Cannot complete an object with no usable {.field event}/{.field report} \\
       date pair.",
      "i" = "Every row has a missing {.val {get_event_date(x)}} or \\
             {.val {get_report_date(x)}}, so there is no grid to complete."
    ))
  }

  if (is.null(max_delay)) {
    # `max(0, ...)` because a data set whose only delays are negative would give
    # `seq(0, max_delay)` a DECREASING sequence -- delays -1, -2, ... -- rather
    # than the single delay-0 column it should complete.
    max_delay <- max(0, delay_values, na.rm = TRUE)
  }

  # Get the initial event
  min_event <- min(event_values, na.rm = TRUE)

  # Get the final event present in the data.
  max_event_observed <- max(event_values, na.rm = TRUE)

  # How far to complete. An event date with NO reports at all does not appear in
  # the data, so stopping at the last observed event date leaves a hole exactly
  # where nowcasting matters most -- at the `now` edge. Default to whichever is
  # later, the `now` or the last event in the data, and never truncate below the
  # data itself.
  if (is.null(until)) {
    until <- max(max_event_observed, get_now(x))
  }
  max_event <- max(max_event_observed, until)

  # Get the final report
  max_report <- max(report_values, na.rm = TRUE)


  # Reports may not run past what could have been observed. Using the later of
  # `now` and the last report in the data guarantees no ORIGINAL row is dropped.
  report_bound <- max(max_report, get_now(x))

  if (get_event_units(x) != get_report_units(x)) {
    cli::cli_abort(
      "Cannot work when event and report units are different. Please input manually,"
    )
  }

  # Materialised temporal effects are a per-ROW property, and this function adds
  # rows, so whatever the caller computed before is about to become stale (the
  # new cells join in as `NA`). Recorded here and repaired at the very end, once
  # the grid is final -- see the recompute just before the return.
  had_computed_effects <- length(get_temporal_effect_cols(x)) > 0

  original_marker <- ".tbl_now_complete_original"
  while (original_marker %in% colnames(x)) {
    original_marker <- paste0(original_marker, "_")
  }
  grid_marker <- ".tbl_now_complete_grid"
  while (grid_marker %in% c(colnames(x), original_marker)) {
    grid_marker <- paste0(grid_marker, "_")
  }
  reconstructed_report <- ".tbl_now_complete_report"
  while (reconstructed_report %in% c(colnames(x), original_marker, grid_marker)) {
    reconstructed_report <- paste0(reconstructed_report, "_")
  }

  # Create a table with all dates
  event_dates <- dplyr::tibble(
    !!as.symbol(get_event_date(x)) :=
      .tbl_now_date_seq(min_event, max_event, get_event_units(x))
  )

  event_dict <- event_dates |>
    dplyr::mutate(.event_num_new = 0:(dplyr::n() - 1))

  # Add report num
  complete_x <- tidyr::expand_grid(
    event_dates,
    .delay = seq(0, max_delay, by = 1),
    x |>
      as.data.frame() |>
      dplyr::distinct(dplyr::across(dplyr::all_of(tbl.now::get_strata(x)))),
    x |>
      as.data.frame() |>
      dplyr::distinct(dplyr::across(dplyr::all_of(tbl.now::get_is_censored_report(x)))),
  ) |>
    # Add the additional rows you lose by not completing them
    dplyr::bind_rows(
      x |>
        as.data.frame() |>
        dplyr::filter(!!as.symbol(".delay") > !!max_delay) |>
        dplyr::distinct(
          dplyr::across(
            dplyr::all_of(
              c(get_strata(x), get_is_censored_report(x), get_event_date(x), get_report_date(x), ".delay")
            )
          )
        )
    ) |>
    dplyr::distinct() |>
    dplyr::left_join(event_dict, by = get_event_date(x)) |>
    dplyr::mutate(
      !!as.symbol(".report_num_new") := !!as.symbol(".event_num_new") + !!as.symbol(".delay"),
      !!as.symbol(grid_marker) := TRUE
    )

  # Now add reports back. Reuse the constructor helper so every supported unit
  # follows the same delay arithmetic.
  complete_x <- .reconstruct_date_from_delay(
    complete_x,
    known_col = get_event_date(x),
    delay_col = ".delay",
    units = get_event_units(x),
    new_col_name = reconstructed_report,
    direction = "add",
    arg = "max_delay"
  )
  if (get_report_date(x) %in% colnames(complete_x)) {
    complete_x <- complete_x |>
      dplyr::mutate(
        !!as.symbol(get_report_date(x)) :=
          dplyr::coalesce(!!as.symbol(get_report_date(x)), !!as.symbol(reconstructed_report))
      ) |>
      dplyr::select(-!!as.symbol(reconstructed_report))
  } else {
    complete_x <- complete_x |>
      dplyr::rename(!!as.symbol(get_report_date(x)) := !!as.symbol(reconstructed_report))
  }

  # Completing out to the `now` generates event/delay pairs whose report date
  # lands after `report_bound`. Drop them HERE rather than at the end: joining
  # them in first would momentarily build a `tbl_now` whose report dates run past
  # its own `now`, which emits a spurious warning at the user even though the
  # returned object is fine.
  complete_x <- complete_x |>
    dplyr::filter(!!as.symbol(get_report_date(x)) <= !!report_bound)

  # Now complete. Include the is_censored_report column in the join key (when present)
  # so the censored indicator is not duplicated/suffixed and lost.
  join_keys <- c(
    get_event_date(x), get_strata(x), get_is_censored_report(x),
    get_report_date(x), ".delay"
  )
  x <- x |>
    dplyr::mutate(!!as.symbol(original_marker) := TRUE) |>
    dplyr::full_join(complete_x, by = join_keys) |>
    dplyr::select(-!!as.symbol(".event_num_new"), -!!as.symbol(".report_num_new"))

  # Renumber from the event dictionary rather than from what the join carried
  # over. A row of `x` that has no counterpart in the grid -- a negative delay,
  # or a missing report date -- matches nothing, so reading `.event_num` off the
  # join set it to `NA` for rows whose event date is perfectly well known.
  # Joining the dictionary on the event date alone numbers every row that has an
  # event date, and leaves only the genuinely undatable ones `NA`.
  x <- x |>
    dplyr::left_join(event_dict, by = get_event_date(x)) |>
    dplyr::mutate(
      !!as.symbol(".event_num") := !!as.symbol(".event_num_new"),
      !!as.symbol(".report_num") := !!as.symbol(".event_num_new") + !!as.symbol(".delay")
    ) |>
    dplyr::select(-!!as.symbol(".event_num_new"))

  # Fix the 0 case for count-cumulative
  if (get_data_type(x) == "count-cumulative") {
    x <- x |>
      dplyr::mutate(!!as.symbol(get_case_count(x)) :=
        dplyr::if_else(
          is.na(!!as.symbol(original_marker)) &
            is.na(!!as.symbol(get_case_count(x))) &
            !!as.symbol(".delay") == 0,
          0,
          !!as.symbol(get_case_count(x))
        ))


    if (max_delay > 0) {
      for (dval in 1:max_delay) {
        x <- x |>
          dplyr::arrange(!!as.symbol(get_report_date(x))) |>
          dplyr::group_by(dplyr::across(dplyr::all_of(
            c(get_event_date(x), get_strata(x), get_is_censored_report(x))
          )))

        x <- x |>
          dplyr::mutate(!!as.symbol(get_case_count(x)) :=
            dplyr::if_else(is.na(!!as.symbol(original_marker)) &
              is.na(!!as.symbol(get_case_count(x))) & !!as.symbol(".delay") == !!dval,
              dplyr::lag(!!as.symbol(get_case_count(x)), default = 0.0), !!as.symbol(get_case_count(x))
            )) |>
          ungroup()
      }
    }
  }

  # Replace only generated incidence cells. An explicit `NA` in the input is an
  # unknown count, not an observed zero.
  if (get_data_type(x) == "count-incidence") {
    x <- x |>
      dplyr::mutate(!!as.symbol(get_case_count(x)) :=
        dplyr::if_else(
          is.na(!!as.symbol(original_marker)) &
            is.na(!!as.symbol(get_case_count(x))),
          0,
          !!as.symbol(get_case_count(x))
        ))
  }

  # Drop generated rows whose report date lies beyond what could have been
  # observed. The bound is `<=`, and is the later of `now` and the last report in
  # the data: with `<` (and the bound at `max_report`) every genuine row reported
  # on the final report date was silently deleted, so a function meant to ADD
  # zeroes removed real cases.
  # `is.na()` first: a missing report date is not a report date past the bound,
  # and `NA <= bound` is `NA`, which `filter()` drops. Those rows are the ones
  # the caller asked about in the first place, so deleting them here would be
  # the same silent loss the `<` bound used to cause.
  x <- x |>
    dplyr::filter(
      is.na(!!as.symbol(get_report_date(x))) |
        !!as.symbol(get_report_date(x)) <= !!report_bound
    ) |>
    dplyr::select(-dplyr::all_of(c(original_marker, grid_marker)))

  # Repair the temporal effects flagged at the top. The completed rows carry
  # `NA` in every effect column, so the object would otherwise claim -- through
  # `computed_temporal_effect_cols` -- to have effects it only half has, and a
  # model fitted on it would silently read zeros or `NA`s for exactly the cells
  # this function was called to create. The lazy spec travels with the object,
  # and the effects are deterministic functions of the calendar date, so the
  # values are recoverable right here; asking the caller to remember a follow-up
  # `compute_temporal_effects()` only moved the trap one line downstream.
  # `overwrite = TRUE` because the stale columns are still in place.
  if (had_computed_effects) {
    if (length(get_temporal_effects(x)) > 0) {
      x <- compute_temporal_effects(x, overwrite = TRUE)
    } else {
      # Computed columns but no spec to rebuild them from -- only reachable if
      # the spec was stripped by hand. Nothing to recompute, so say so.
      cli::cli_alert_warning(
        "Computed temporal-effect columns are stale on the rows
         {.fn complete_zeroes} added, and no {.fn temporal_effects}
         specification is attached to recompute them from."
      )
    }
  }

  return(.tbl_now_regroup(x, group_columns))
}
