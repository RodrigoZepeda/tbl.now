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
#' @param type `"epidemic"` (event-date process) or `"report"` (reporting-delay
#'   process).
#'
#' @return A panel key.
#'
#' @keywords internal
#' @noRd
.tbl_now_effect_key <- function(family, type) {
  if (identical(type, "report")) paste0("delay_", family) else paste0("calendar_", family)
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
#' @param weekend_days The weekend definition, used only when attaching.
#' @param fn Name of the calling function, for the error messages.
#'
#' @return `x`, with a `temporal_effects(weekend = TRUE)` spec attached when it
#'   did not already carry one.
#'
#' @keywords internal
#' @noRd
.tbl_now_with_weekend_effect <- function(x, weekend_days, fn) {
  .assert_tbl_now(x, fn)

  event_units <- get_event_units(x)
  if (!identical(event_units, "days")) {
    cli::cli_abort(c(
      "A weekend effect needs daily data.",
      "x" = "{.arg event_units} is {.val {event_units}}.",
      "i" = paste0(
        "Every date on a coarser grid falls in the same day type, so there is ",
        "nothing to contrast. Use {.fn plot_holiday_effects} for a holiday ",
        "calendar on this object."
      )
    ))
  }

  already_asked <- any(vapply(
    get_temporal_effects(x),
    function(spec) isTRUE(spec$t_effects@weekend),
    logical(1)
  ))
  if (already_asked) {
    return(x)
  }

  add_temporal_effects(x,
    t_effects = temporal_effects(weekend = TRUE), weekend_days = weekend_days
  )
}

#' Calendar effects on the case counts or on the reporting delay
#'
#' @description `r lifecycle::badge("experimental")`
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
#' vary by calendar group) or `"report"` (red — how the *reporting* does).
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
#' @param type `"epidemic"` (default) for the case-count effect, or `"report"`
#'   for the reporting-delay one.
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
#' # ... and how the weekend shows up in the reporting delay
#' plot_weekend_effects(daily_now, type = "report")
#'
#' # By month, on monthly-unit data
#' monthly_now <- tbl_now(
#'   data.frame(
#'     event_date  = seq(as.Date("2018-01-01"), as.Date("2021-12-01"), by = "month"),
#'     report_date = seq(as.Date("2018-02-01"), as.Date("2022-01-01"), by = "month")
#'   ),
#'   event_date, report_date,
#'   event_units = "months", report_units = "months", verbose = FALSE
#' )
#' plot_month_of_year_effects(monthly_now)
#'
#' # ... and how the reporting does, as a share of the year's cases rather than
#' # normalized. `type` and `measure` compose.
#' plot_week_of_year_effects(dengue_now, type = "report", measure = "percent")
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
plot_day_of_week_effects <- function(x, type = c("epidemic", "report"),
                                     measure = c("percent", "normalized"), ...) {
  .tbl_now_plot_panel(x, .tbl_now_effect_key("weekday", match.arg(type)),
                      measure = match.arg(measure), ...)
}

#' @rdname calendar_effect_plots
#' @export
plot_week_of_year_effects <- function(x, type = c("epidemic", "report"),
                                      measure = c("percent", "normalized"), ...) {
  .tbl_now_plot_panel(x, .tbl_now_effect_key("week", match.arg(type)),
                      measure = match.arg(measure), ...)
}

#' @rdname calendar_effect_plots
#' @export
plot_month_of_year_effects <- function(x, type = c("epidemic", "report"),
                                       measure = c("percent", "normalized"), ...) {
  .tbl_now_plot_panel(x, .tbl_now_effect_key("month", match.arg(type)),
                      measure = match.arg(measure), ...)
}

#' @rdname calendar_effect_plots
#' @export
plot_holiday_effects <- function(x, type = c("epidemic", "report"), ...) {
  .tbl_now_plot_panel(x, .tbl_now_effect_key("holiday", match.arg(type)), ...)
}

#' @rdname calendar_effect_plots
#' @export
plot_weekend_effects <- function(x, type = c("epidemic", "report"),
                                 weekend_days = c("Sat", "Sun"), ...) {
  x <- .tbl_now_with_weekend_effect(x, weekend_days, "plot_weekend_effects")
  .tbl_now_plot_panel(x, .tbl_now_effect_key("holiday", match.arg(type)), ...)
}

#' @rdname calendar_effect_plots
#' @export
plot_holiday_lag_effects <- function(x, type = c("epidemic", "report"), ...) {
  .tbl_now_plot_panel(x, .tbl_now_effect_key("holiday_lag", match.arg(type)), ...)
}

#' Periodogram of the case counts or of the reporting delay
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The `"seasonality"` / `"delay_seasonality"` panels of [autoplot()], drawn on
#' their own: a periodogram whose dominant peak is marked. For
#' `type = "epidemic"` (green) the peak suggests a Fourier season length to pass
#' to [temporal_effects()]; for `type = "report"` (red) it marks a cycle in the
#' reporting delay itself, such as a weekly reporting rhythm.
#'
#' For a *time-resolved* view — which cycles are strong **when** — see
#' [plot_scalogram()].
#'
#' @param x A [tbl_now()] object.
#' @param type `"epidemic"` (default) or `"report"`.
#' @param ... Further arguments passed to [autoplot.tbl_now()], e.g. `by_strata`,
#'   `strata`, `plotly` or `palette`.
#'
#' @return A \pkg{ggplot2} object (or a \pkg{plotly} widget when `plotly = TRUE`).
#'
#' @seealso [autoplot.tbl_now()], [plot_scalogram()], [calendar_effect_plots].
#'
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' data(denguedat)
#' dengue_now <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)
#' plot_cycles(dengue_now)
#'
#' @export
#' @md
plot_cycles <- function(x, type = c("epidemic", "report"), ...) {
  type <- match.arg(type)
  key <- if (identical(type, "report")) "delay_seasonality" else "seasonality"
  .tbl_now_plot_panel(x, key, ...)
}

#' Empirical distribution of the reporting delay
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The `"delay_distribution"` panel of [autoplot()], drawn on its own: a
#' case-count weighted histogram of `.delay`. For **`count-cumulative`** data it
#' becomes the *cumulative growth by delay* instead — boxplots, on a log scale,
#' of the ratio of each event date's cumulative count at a delay to its count at
#' the previous delay.
#'
#' @param x A [tbl_now()] object.
#' @param ... Further arguments passed to [autoplot.tbl_now()], e.g. `by_strata`,
#'   `strata`, `delay_distribution_xlim`, `plotly` or `palette`.
#'
#' @return A \pkg{ggplot2} object (or a \pkg{plotly} widget when `plotly = TRUE`).
#'
#' @seealso [autoplot.tbl_now()], [plot_delay_profiles()], [plot_delay_drift()].
#'
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' data(denguedat)
#' dengue_now <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)
#' plot_delay_distribution(dengue_now)
#'
#' @export
#' @md
plot_delay_distribution <- function(x, ...) {
  .tbl_now_plot_panel(x, "delay_distribution", ...)
}

#' Observed epidemic process with the incompleteness line
#'
#' @description `r lifecycle::badge("experimental")`
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
