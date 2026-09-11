# =============================================================================
# Standalone `plot_*()` twins of the `autoplot()` panels.
#
# `autoplot()` draws the whole grid; these draw one panel each. They are thin
# wrappers, so a panel is identical either way -- same data, same colours, same
# subtitle -- and there is exactly one implementation to keep honest.
# =============================================================================

#' Draw a single `autoplot()` panel
#'
#' @param x A `tbl_now` object.
#' @param key The concrete panel key to draw.
#' @param ... Passed on to [autoplot.tbl_now()].
#'
#' @return A ggplot object (or a plotly widget).
#'
#' @keywords internal
#' @noRd
.tbl_now_plot_panel <- function(x, key, ...) {
  autoplot(x, panels = key, ...)
}

#' The panel key for an effect family and a process
#'
#' @param family The `autoplot()` grouping (`"weekday"`, `"week"`, `"month"`,
#'   `"holiday"`, `"holiday_lag"`).
#' @param type `"epidemic"` (event-date process), `"report"` (reporting-delay
#'   process), or `"revision"` (revision-date process).
#'
#' @return A panel key.
#'
#' @keywords internal
#' @noRd
.tbl_now_effect_key <- function(family, type) {
  if (identical(type, "report")) {
    paste0("delay_", family)
  } else if (identical(type, "revision")) {
    paste0("revision_", family)
  } else {
    paste0("calendar_", family)
  }
}

#' Make sure the object carries a weekend effect to draw
#'
#' `plot_weekend_effects()` is `plot_holiday_effects()` on an object that has a
#' weekend effect attached, so it attaches one when there is none. The spec goes
#' on a copy: the caller's object is not modified.
#'
#' Both day-type panels group on the **event date** (the delay panel averages the
#' delay per event date first), so it is `event_units` that has to be daily for a
#' weekend to exist at all.
#'
#' @param x A `tbl_now` object.
#' @param type Matched `type` from the calling function.
#' @param weekend_days The weekend definition, used only when attaching.
#' @param fn Name of the calling function, for the error messages.
#'
#' @return `x`, with a `temporal_effects(weekend = TRUE)` spec attached when it
#'   did not already carry one.
#'
#' @keywords internal
#' @noRd
.tbl_now_with_weekend_effect <- function(x, type, weekend_days, fn) {
  .assert_tbl_now(x, fn)

  date_type <- switch(type,
    report = "report_date",
    revision = "revision_date",
    "event_date"
  )
  units <- switch(type,
    report = get_report_units(x),
    revision = {
      if (!has_revision(x)) {
        cli::cli_abort(c(
          "{.fn {fn}} needs a revision process for {.code type = \"revision\"}.",
          "i" = "Attach one with {.fn add_revision_date} first."
        ))
      }
      get_revision_units(x)
    },
    get_event_units(x)
  )
  if (!identical(units, "days")) {
    cli::cli_abort(c(
      "A weekend effect needs daily data.",
      "x" = "The selected date axis has units {.val {units}}.",
      "i" = paste0(
        "Every date on a coarser grid falls in the same day type, so there is ",
        "nothing to contrast. Use {.fn plot_holiday_effects} for a holiday ",
        "calendar on this object."
      )
    ))
  }

  already_asked <- any(vapply(
    get_temporal_effects(x),
    function(spec) {
      identical(spec$date_type, date_type) && isTRUE(spec$t_effects@weekend)
    },
    logical(1)
  ))
  if (already_asked) {
    return(x)
  }

  add_temporal_effects(x,
    t_effects = temporal_effects(weekend = TRUE), date_type = date_type,
    weekend_days = weekend_days
  )
}

#' Calendar effects on the case counts or on the reporting delay
#'
#' @description `r lifecycle::badge("stable")`
#'
#' One panel of [autoplot()], drawn on its own. Each function shows the same
#' boxplots the corresponding `autoplot()` panel does, for one calendar grouping:
#'
#' * `plot_day_of_week_effects()` — by day of week (daily data only).
#' * `plot_week_of_year_effects()` — by epidemiological week.
#' * `plot_month_of_year_effects()` — by month (monthly data only).
#' * `plot_holiday_effects()` — by **day type** (`Weekday` / `Weekend` /
#'   `Holiday`, following the attached [temporal_effects()] spec).
#' * `plot_weekend_effects()` — the same panel, on an object that may not carry a
#'   spec yet: it attaches `temporal_effects(weekend = TRUE)` when there is no
#'   weekend effect already, so the weekend boxes appear without a separate
#'   [add_temporal_effects()] call. The spec goes on a copy — your object is not
#'   modified — and a calendar already attached still contributes its `Holiday`
#'   box. Daily data only, since a weekend is a property of the day.
#' * `plot_holiday_lag_effects()` — by position relative to the nearest holiday
#'   (`"1 before"`, `"Holiday"`, `"1 after"`, ..., plus `"Other"`).
#'
#' `type` picks which process to describe: `"epidemic"` (green — how the *cases*
#' vary by calendar group), `"report"` (red — how the *reporting* does), or
#' `"revision"` (ochre — how resolved cases arrive on revision dates).
#'
#' The three day-type / holiday-lag functions have no `measure` argument: they
#' are always normalized. Their categories are not equal-sized parts of a
#' calendar block — the weekend is two days in seven — so a percentage share
#' would mostly restate the calendar rather than the data ("29% of the cases at
#' the weekend" is average, not low). The day-of-week, week-of-year and
#' month-of-year functions keep both measures.
#'
#' Use these when you want one effect, in its own figure, at its own size; use
#' [autoplot()] when you want the diagnostic grid in one call. Everything else is
#' the same: `autoplot(x, panels = "calendar_weekday")` and
#' `plot_day_of_week_effects(x)` return the identical plot.
#'
#' @param x A [tbl_now()] object.
#' @param type `"epidemic"` (default) for the case-count effect, `"report"` for
#'   the reporting-delay one, or `"revision"` for revision-date arrivals.
#' @param measure `"percent"` (default) for the share of cases in each group —
#'   "10% of cases in week 1 versus 3% in week 2" — with the IQR around it, or
#'   `"normalized"` for the value divided by its overall mean (`1` = average).
#'   See [autoplot.tbl_now()] for the blocks the percentages are taken over. The
#'   day-type and holiday-lag functions do not take it; they are always
#'   normalized.
#' @param weekend_days Character vector naming the weekend days (default
#'   `c("Sat", "Sun")`), as in [is_weekday()]. Used **only** when
#'   `plot_weekend_effects()` has to attach the effect itself; an object that
#'   already carries a weekend effect keeps the definition it was given.
#' @param ... Further arguments passed to [autoplot.tbl_now()], e.g. `by_strata`,
#'   `strata`, `plotly` or `palette`.
#'
#' @return A \pkg{ggplot2} object (or a \pkg{plotly} widget when `plotly = TRUE`).
#'
#' @seealso [autoplot.tbl_now()], [plot_cycles()], [plot_delay_distribution()],
#'   [plot_observed_cases()]; [temporal_effects()] and [add_temporal_effects()]
#'   for the specification the day-type and holiday-lag panels describe.
#'
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' data(denguedat)
#' # First few years only, to keep the example quick; the full data works the same.
#' dengue_now <- tbl_now(denguedat[1:2500, ], onset_week, report_week, verbose = FALSE)
#'
#' # How the cases vary by epidemiological week
#' plot_week_of_year_effects(dengue_now)
#'
#' # The weekend on its own, on daily data, with no spec to attach first
#' days <- seq(as.Date("2021-01-01"), as.Date("2021-06-30"), by = "day")
#' daily_now <- tbl_now(
#'   data.frame(event_date = days, report_date = days + 1),
#'   event_date, report_date, verbose = FALSE
#' )
#' plot_weekend_effects(daily_now)
#'
#' # By month, on monthly-unit data. `type` picks the process and `measure`
#' # picks the scale; both compose, and work the same way on every calendar
#' # function here. (The day-type and holiday-lag panels are always
#' # normalized, so they take `type` but not `measure`.)
#' monthly_now <- tbl_now(
#'   data.frame(
#'     event_date  = seq(as.Date("2018-01-01"), as.Date("2021-12-01"), by = "month"),
#'     report_date = seq(as.Date("2018-02-01"), as.Date("2022-01-01"), by = "month")
#'   ),
#'   event_date, report_date,
#'   event_units = "months", report_units = "months", verbose = FALSE
#' )
#' plot_month_of_year_effects(monthly_now, type = "report", measure = "normalized")
#'
#' if (requireNamespace("almanac", quietly = TRUE)){
#'
#'   ## By day type (weekday / weekend / holiday), once a holiday calendar is attached
#'   holiday_now <- dengue_now |>
#'    add_temporal_effects(temporal_effects(weekend = TRUE, holidays = almanac::cal_us_federal()))
#'   plot_holiday_effects(holiday_now)
#'
#'   # By position relative to the nearest holiday
#'   holiday_lag_now <- dengue_now |>
#'     add_temporal_effects(temporal_effects(holidays = almanac::cal_us_federal(), holiday_lags = 2))
#'   plot_holiday_lag_effects(holiday_lag_now)
#'
#' }
#' @name calendar_effect_plots
#' @md
NULL

#' @rdname calendar_effect_plots
#' @export
plot_day_of_week_effects <- function(x, type = c("epidemic", "report", "revision"),
                                     measure = c("percent", "normalized"), ...) {
  .tbl_now_plot_panel(x, .tbl_now_effect_key("weekday", match.arg(type)),
                      measure = match.arg(measure), ...)
}

#' @rdname calendar_effect_plots
#' @export
plot_week_of_year_effects <- function(x, type = c("epidemic", "report", "revision"),
                                      measure = c("percent", "normalized"), ...) {
  .tbl_now_plot_panel(x, .tbl_now_effect_key("week", match.arg(type)),
                      measure = match.arg(measure), ...)
}

#' @rdname calendar_effect_plots
#' @export
plot_month_of_year_effects <- function(x, type = c("epidemic", "report", "revision"),
                                       measure = c("percent", "normalized"), ...) {
  .tbl_now_plot_panel(x, .tbl_now_effect_key("month", match.arg(type)),
                      measure = match.arg(measure), ...)
}

#' @rdname calendar_effect_plots
#' @export
plot_holiday_effects <- function(x, type = c("epidemic", "report", "revision"), ...) {
  .tbl_now_plot_panel(x, .tbl_now_effect_key("holiday", match.arg(type)), ...)
}

#' @rdname calendar_effect_plots
#' @export
plot_weekend_effects <- function(x, type = c("epidemic", "report", "revision"),
                                 weekend_days = c("Sat", "Sun"), ...) {
  type <- match.arg(type)
  x <- .tbl_now_with_weekend_effect(x, type, weekend_days, "plot_weekend_effects")
  .tbl_now_plot_panel(x, .tbl_now_effect_key("holiday", type), ...)
}

#' @rdname calendar_effect_plots
#' @export
plot_holiday_lag_effects <- function(x, type = c("epidemic", "report", "revision"), ...) {
  .tbl_now_plot_panel(x, .tbl_now_effect_key("holiday_lag", match.arg(type)), ...)
}

#' Periodogram of the case counts or of the reporting delay
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The `"seasonality"` / `"delay_seasonality"` panels of [autoplot()], drawn on
#' their own: a periodogram whose dominant peak is marked. For
#' `type = "epidemic"` (green) the peak suggests a Fourier season length to pass
#' to [temporal_effects()]; for `type = "report"` (red) it marks a cycle in the
#' reporting delay itself, such as a weekly reporting rhythm.
#'
#' @param x A [tbl_now()] object.
#' @param type `"epidemic"` (default), `"report"` or `"revision"`.
#' @param ... Further arguments passed to [autoplot.tbl_now()], e.g. `by_strata`,
#'   `strata`, `plotly` or `palette`.
#'
#' @return A \pkg{ggplot2} object (or a \pkg{plotly} widget when `plotly = TRUE`).
#'
#' @seealso [autoplot.tbl_now()], [calendar_effect_plots].
#'
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' data(denguedat)
#' dengue_now <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)
#' plot_cycles(dengue_now)
#'
#' @export
#' @md
plot_cycles <- function(x, type = c("epidemic", "report", "revision"), ...) {
  type <- match.arg(type)
  key <- switch(type,
    report = "delay_seasonality",
    revision = "revision_seasonality",
    "seasonality"
  )
  .tbl_now_plot_panel(x, key, ...)
}

#' Empirical distribution of the reporting or revision delay
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The `"delay_distribution"` panel of [autoplot()], drawn on its own: a
#' case-count weighted histogram of `.delay`. For **`count-cumulative`** data it
#' becomes the *cumulative growth by delay* instead — boxplots, on a log scale,
#' of the ratio of each event date's cumulative count at a delay to its count at
#' the previous delay.
#'
#' `axis = "revision"` draws the same histogram of `.revision_delay`, the time
#' from a report to its resolution, in the revision process's colours. A case
#' still `"pending"` has no resolution, and so no revision delay, and does not
#' appear.
#'
#' @param x A [tbl_now()] object.
#' @param axis Which delay to draw: `"report"` (default), the time from the
#'   event to the report, or `"revision"`, the time from the report to its
#'   resolution. `"revision"` needs a revision process (see
#'   [add_revision_date()][add]).
#' @param by_revision_type Logical (default `TRUE`). Split the histogram by how
#'   each case eventually resolved — `confirmed`, `pending`, `retracted` and
#'   `unknown`, stacked, in the palette's outcome colours (see
#'   [tbl_now_palette()]). Whether a negative result comes back faster than a
#'   positive one is the question the split exists to answer, and
#'   [diagnose_revision_delay()] is the test of it. Ignored on an object with no
#'   revision axis, and when `by_strata = TRUE`, which already uses the fill for
#'   the strata.
#' @param ... Further arguments passed to [autoplot.tbl_now()], e.g. `by_strata`,
#'   `strata`, `delay_distribution_xlim`, `plotly` or `palette`.
#'
#' @return A \pkg{ggplot2} object (or a \pkg{plotly} widget when `plotly = TRUE`).
#'
#' @seealso [autoplot.tbl_now()], [plot_delay_profiles()], [plot_delay_drift()];
#'   [diagnose_revision_delay()] for the test behind the outcome split.
#'
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' data(denguedat)
#' dengue_now <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)
#' plot_delay_distribution(dengue_now)
#'
#' # On the revision axis, split by how each case resolved.
#' cases <- data.frame(
#'   onset = as.Date("2021-01-04") + rep(0:9, each = 4),
#'   visit = as.Date("2021-01-05") + rep(0:9, each = 4),
#'   result = as.Date("2021-01-05") + rep(0:9, each = 4) +
#'     rep(c(1, 1, 5, 6), times = 10),
#'   outcome = rep(c("confirmed", "confirmed", "retracted", "retracted"), times = 10)
#' )
#' flu <- tbl_now(cases,
#'   event_date = onset, report_date = visit,
#'   revision_date = result, revision_type = outcome,
#'   data_type = "linelist", verbose = FALSE
#' )
#' plot_delay_distribution(flu, axis = "revision")
#'
#' @export
#' @md
plot_delay_distribution <- function(x, axis = c("report", "revision"),
                                    by_revision_type = TRUE, ...) {
  axis <- match.arg(axis)
  key <- if (identical(axis, "revision")) {
    "revision_distribution"
  } else {
    "delay_distribution"
  }
  .tbl_now_plot_panel(x, key, by_revision_type = by_revision_type, ...)
}

#' Observed epidemic process with the incompleteness line
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The `"epidemic"` panel of [autoplot()], drawn on its own: the latest reported
#' counts per `event_date`, with a dashed vertical line marking where the data
#' become incomplete (less than `level` of the delay distribution has arrived).
#' Holidays from an attached [temporal_effects()] spec are marked with red dots.
#'
#' [plot_epidemic_process()] draws the same curve without the incompleteness
#' line, next to its reporting twin [plot_reporting_process()].
#'
#' @param x A [tbl_now()] object.
#' @param ... Further arguments passed to [autoplot.tbl_now()], e.g. `level`,
#'   `by_strata`, `strata`, `event_date_xlim`, `plotly` or `palette`.
#'
#' @return A \pkg{ggplot2} object (or a \pkg{plotly} widget when `plotly = TRUE`).
#'
#' @seealso [autoplot.tbl_now()], [plot_epidemic_process()],
#'   [plot_reporting_process()].
#'
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' data(denguedat)
#' dengue_now <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)
#' plot_observed_cases(dengue_now)
#'
#' @export
#' @md
plot_observed_cases <- function(x, ...) {
  .tbl_now_plot_panel(x, "epidemic", ...)
}
