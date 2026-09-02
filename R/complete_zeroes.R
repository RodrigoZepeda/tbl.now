#' Fill in the days when nothing was reported
#'
#' @description `r lifecycle::badge("experimental")`
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
#' @return A `tbl_now` object with the same columns as `x`, plus the rows that
#' were implicitly zero, carrying `0` in the `case_count` column. The data type
#' is preserved.
#'
#' @seealso
#' [to_count()] for the data shapes this operates on;
#' [censor_delays_above()] for the opposite problem, delays that are too long;
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

  if (is.null(max_delay)) {
    max_delay <- suppressWarnings(
      x |>
        dplyr::distinct(!!as.symbol(".delay")) |>
        dplyr::pull() |>
        max()
    )
  }

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

  # Get the initial event
  min_event <- suppressWarnings(
    x |>
      dplyr::filter(!!as.symbol(get_event_date(x)) == min(!!as.symbol(get_event_date(x)))) |>
      dplyr::distinct(!!as.symbol(get_event_date(x))) |>
      dplyr::pull()
  )

  # Get the final event present in the data.
  max_event_observed <- suppressWarnings(
    x |>
      dplyr::filter(!!as.symbol(get_event_date(x)) == max(!!as.symbol(get_event_date(x)))) |>
      dplyr::distinct(!!as.symbol(get_event_date(x))) |>
      dplyr::pull()
  )

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
  max_report <- suppressWarnings(
    x |>
      dplyr::distinct(!!as.symbol(get_report_date(x))) |>
      dplyr::pull() |>
      max()
  )


  # Reports may not run past what could have been observed. Using the later of
  # `now` and the last report in the data guarantees no ORIGINAL row is dropped.
  report_bound <- max(max_report, get_now(x))

  if (get_event_units(x) != get_report_units(x)) {
    cli::cli_abort(
      "Cannot work when event and report units are different. Please input manually,"
    )
  }

  # Warn to recalculate temporal effects
  if (length(get_temporal_effect_cols(x)) > 0) {
    cli::cli_alert_warning(
      "Computed temporal-effect columns have been lost. Call {.fn compute_temporal_effects} again after {.fn complete_zeroes}."
    )
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
    dplyr::mutate(!!as.symbol(".report_num_new") := !!as.symbol(".event_num_new") + !!as.symbol(".delay"))

  # Now add reports back
  if (get_event_units(x) == "weeks") {
    complete_x <- complete_x |>
      dplyr::mutate(!!as.symbol(get_report_date(x)) := !!as.symbol(get_event_date(x)) + lubridate::weeks(!!as.symbol(".delay")))
  } else if (get_event_units(x) == "days") {
    complete_x <- complete_x |>
      dplyr::mutate(!!as.symbol(get_report_date(x)) := !!as.symbol(get_event_date(x)) + lubridate::days(!!as.symbol(".delay")))
  } else if (get_event_units(x) == "numeric") {
    complete_x <- complete_x |>
      dplyr::mutate(!!as.symbol(get_report_date(x)) := !!as.symbol(get_event_date(x)) + !!as.symbol(".delay"))
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
    dplyr::full_join(complete_x, by = join_keys)

  # Remove and rename
  x <- x |>
    dplyr::mutate(!!as.symbol(".event_num") := !!as.symbol(".event_num_new")) |>
    dplyr::mutate(!!as.symbol(".report_num") := !!as.symbol(".report_num_new")) |>
    dplyr::select(-!!as.symbol(".event_num_new"), -!!as.symbol(".report_num_new"))

  # Fix the 0 case for count-cumulative
  if (get_data_type(x) == "count-cumulative") {
    x <- x |>
      dplyr::mutate(!!as.symbol(get_case_count(x)) :=
        dplyr::if_else(is.na(!!as.symbol(get_case_count(x)) & !!as.symbol(".delay") == 0), 0, !!as.symbol(get_case_count(x))))


    if (max_delay > 0) {
      for (dval in 1:max_delay) {
        x <- x |>
          dplyr::arrange(!!as.symbol(get_report_date(x))) |>
          dplyr::group_by(dplyr::across(dplyr::all_of(
            c(get_event_date(x), get_strata(x), get_is_censored_report(x))
          )))

        x <- x |>
          dplyr::mutate(!!as.symbol(get_case_count(x)) :=
            dplyr::if_else(is.na(!!as.symbol(get_case_count(x))) & !!as.symbol(".delay") == !!dval,
              dplyr::lag(!!as.symbol(get_case_count(x)), default = 0.0), !!as.symbol(get_case_count(x))
            )) |>
          ungroup()
      }
    }
  }

  # #Replace whatever is missing with 0
  x <- x |>
    dplyr::mutate(!!as.symbol(get_case_count(x)) := tidyr::replace_na(!!as.symbol(get_case_count(x)), 0.0))

  # Drop generated rows whose report date lies beyond what could have been
  # observed. The bound is `<=`, and is the later of `now` and the last report in
  # the data: with `<` (and the bound at `max_report`) every genuine row reported
  # on the final report date was silently deleted, so a function meant to ADD
  # zeroes removed real cases.
  x <- x |>
    dplyr::filter(!!as.symbol(get_report_date(x)) <= !!report_bound)

  return(.tbl_now_regroup(x, group_columns))
}
