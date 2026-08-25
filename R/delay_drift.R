# Tools to answer "Do delay distributions drift over time?"
#
#   plot_delay_drift()  — a rolling fan chart of the reporting-delay
#                         distribution (mean + median + 25/75 & 10/90 bands)
#                         indexed by event date.
#   test_delay_drift()  — an autocorrelation-robust monotonic-trend test on the
#                         per-period delay summaries (via the `modifiedmk`
#                         package).
#
# Both reuse the internal helpers defined in R/autoplot.R
# (`.tbl_now_weighted_quantile()`, `.tbl_now_units_to_days()`,
# `.tbl_now_strata_label()`, `.tbl_now_resolve_strata_cols()`,
# `.tbl_now_palette()`, `.tbl_now_theme()`).


# Shared data helpers-----

#' Long (event_date, delay, weight) table of reporting delays
#'
#' @param object A `tbl_now` object.
#' @param strata_cols Optional character vector of strata columns; when supplied
#'   a combined `strata` label column is added.
#'
#' @return A tibble with `event_date`, `delay`, `weight` (positive) and,
#'   optionally, `strata`.
#'
#' @keywords internal
#' @noRd
.tbl_now_delay_long <- function(object, strata_cols = NULL,
                                axis = c("report", "confirmation")) {
  axis <- match.arg(axis)
  if (identical(axis, "confirmation")) {
    .batch_confirmation_axis(object)
  }
  incidence <- object |>
    ungroup() |>
    to_count(to = "count-incidence")
  case_count_column <- get_case_count(incidence)
  event_date_column <- get_event_date(object)
  observations <- as.data.frame(incidence)

  # On the confirmation axis the delay is still measured FROM THE EVENT, exactly
  # as on the report axis, so the two are directly comparable: plot both and the
  # gap between them is the time the laboratory adds. (That is a different
  # quantity from the `.confirmation_delay` column, which is the laboratory's
  # own turnaround, measured from the report.) Pending rows have no confirmation
  # date and drop out.
  delay <- if (identical(axis, "confirmation")) {
    observations[[".confirmation_num"]] - observations[[".event_num"]]
  } else {
    observations[[".delay"]]
  }

  out <- dplyr::tibble(
    event_date = observations[[event_date_column]],
    delay      = delay,
    weight     = observations[[case_count_column]]
  )
  if (!is.null(strata_cols)) {
    out <- dplyr::mutate(out, strata = .tbl_now_strata_label(observations, strata_cols))
  }
  dplyr::filter(
    out,
    !is.na(.data$event_date), !is.na(.data$delay),
    !is.na(.data$weight), .data$weight > 0
  )
}

#' Date beyond which the reporting is still incomplete ("maturity" cutoff)
#'
#' Reuses the same rule as [autoplot()]: `now - q`, where `q` is the `level`
#' quantile of the (count-weighted) delay distribution. Event dates *after* this
#' cutoff have not had time to be fully reported, so their delay summaries are
#' downward-biased.
#'
#' @param object A `tbl_now` object.
#' @param delay_long A long delay table from `.tbl_now_delay_long()`.
#' @param level Completeness level in `(0, 1)`.
#'
#' @return A `Date`/number (the cutoff), or `NA` when it cannot be computed.
#'
#' @keywords internal
#' @noRd
.tbl_now_maturity_threshold <- function(object, delay_long, level) {
  delay_quantile <- .tbl_now_weighted_quantile(
    delay_long$delay, delay_long$weight, level
  )
  now_value <- get_now(object)
  if (is.na(delay_quantile) || level >= 1 || level <= 0) {
    return(NA)
  }
  if (lubridate::is.Date(now_value)) {
    now_value - delay_quantile * .tbl_now_units_to_days(get_event_units(object))
  } else {
    now_value - delay_quantile
  }
}

#' Rolling count-weighted delay quantiles over event time (one group)
#'
#' For each `center`, pools the delays whose `event_date` falls within
#' `± halfwidth` and returns their weighted mean and 10/25/50/75/90 quantiles.
#'
#' @param delay_long A one-group long delay table (`event_date`, `delay`,
#'   `weight`).
#' @param centers A vector of window-center dates/numbers.
#' @param halfwidth Half the window width, in the same units as `event_date`
#'   minus a date (days for `Date`, raw units otherwise).
#' @param min_n Minimum total weight for a window to be summarised.
#'
#' @return A tibble with `time`, `n`, `mean`, `q10`, `q25`, `q50`, `q75`, `q90`.
#'
#' @keywords internal
#' @noRd
.tbl_now_rolling_delay_one <- function(delay_long, centers, halfwidth, min_n = 1) {
  rows <- lapply(centers, function(center) {
    in_window <- delay_long$event_date >= (center - halfwidth) &
      delay_long$event_date <= (center + halfwidth)
    if (!any(in_window)) {
      return(NULL)
    }
    delays <- delay_long$delay[in_window]
    weights <- delay_long$weight[in_window]
    if (sum(weights) < min_n) {
      return(NULL)
    }
    dplyr::tibble(
      time = center,
      n    = sum(weights),
      mean = stats::weighted.mean(delays, weights),
      q10  = .tbl_now_weighted_quantile(delays, weights, 0.10),
      q25  = .tbl_now_weighted_quantile(delays, weights, 0.25),
      q50  = .tbl_now_weighted_quantile(delays, weights, 0.50),
      q75  = .tbl_now_weighted_quantile(delays, weights, 0.75),
      q90  = .tbl_now_weighted_quantile(delays, weights, 0.90)
    )
  })
  dplyr::bind_rows(rows)
}

#' Rolling delay summaries for every group
#'
#' @inheritParams .tbl_now_rolling_delay_one
#' @param delay_long A long delay table, optionally carrying a `strata` column.
#'
#' @return A tibble of rolling summaries; carries a `strata` column when the
#'   input did.
#'
#' @keywords internal
#' @noRd
.tbl_now_rolling_delay <- function(delay_long, centers, halfwidth, min_n = 1) {
  if (!"strata" %in% names(delay_long)) {
    return(.tbl_now_rolling_delay_one(delay_long, centers, halfwidth, min_n))
  }
  groups <- split(delay_long, delay_long$strata)
  rolled <- lapply(names(groups), function(stratum) {
    out <- .tbl_now_rolling_delay_one(groups[[stratum]], centers, halfwidth, min_n)
    if (nrow(out) > 0) out <- dplyr::mutate(out, strata = stratum)
    out
  })
  dplyr::bind_rows(rolled)
}

#' Per-period count-weighted delay summaries (the test series)
#'
#' One row per distinct `event_date` (and stratum): the count-weighted mean,
#' 10/25/50/75/90 quantiles and the derived spread. Unlike the rolling
#' summaries, these are *not* smoothed, so they are the honest series for a
#' trend test (the test's autocorrelation correction handles the natural serial
#' dependence).
#'
#' @param delay_long A long delay table, optionally carrying a `strata` column.
#'
#' @return A tibble with `strata` (or `"all"`), `event_date`, `n`, `mean`,
#'   `median`, `iqr` (q75 - q25) and `spread` (q90 - q10), sorted by date.
#'
#' @keywords internal
#' @noRd
.tbl_now_delay_period_series <- function(delay_long) {
  if (!"strata" %in% names(delay_long)) {
    delay_long <- dplyr::mutate(delay_long, strata = "all")
  }
  delay_long |>
    dplyr::group_by(.data$strata, .data$event_date) |>
    dplyr::summarise(
      n      = sum(.data$weight),
      mean   = stats::weighted.mean(.data$delay, .data$weight),
      median = .tbl_now_weighted_quantile(.data$delay, .data$weight, 0.50),
      q25    = .tbl_now_weighted_quantile(.data$delay, .data$weight, 0.25),
      q75    = .tbl_now_weighted_quantile(.data$delay, .data$weight, 0.75),
      q10    = .tbl_now_weighted_quantile(.data$delay, .data$weight, 0.10),
      q90    = .tbl_now_weighted_quantile(.data$delay, .data$weight, 0.90),
      .groups = "drop"
    ) |>
    dplyr::mutate(iqr = .data$q75 - .data$q25, spread = .data$q90 - .data$q10) |>
    dplyr::arrange(.data$strata, .data$event_date)
}


# plot_delay_drift()-----

#' Visualise whether the reporting-delay distribution drifts over time
#'
#' `r lifecycle::badge("experimental")`
#'
#' Draws a **rolling fan chart** of the count-weighted reporting-delay
#' distribution indexed by **event date**: a solid line for the rolling median,
#' a dashed line for the rolling mean, and two shaded bands (the 25-75% and
#' 10-90% quantile ranges). Reading it left to right answers "do delays drift?"
#' — a rising/falling centre line is *location* drift, widening/narrowing bands
#' are *spread* drift.
#'
#' Because recent event dates have not had time to be fully reported, their delay
#' summaries are downward-biased (only short delays are observable yet). That
#' immature region — event dates after the `level` incompleteness cutoff — is
#' **shaded grey** and should not be read as drift. Pair the plot with
#' [test_delay_drift()] for a formal test.
#'
#' @param x A `tbl_now` object.
#' @param window Rolling-window width, in event-time **periods**. `NULL`
#'   (default) uses `7` periods regardless of the time unit — that is, 7 days for
#'   daily data and 7 weeks for weekly data.
#' @param step Step between window centres, in periods. `NULL` (default) uses
#'   `max(1, window / 4)`.
#' @param min_n Minimum total case count for a window to be drawn (default `1`).
#' @param by_strata Logical (default `FALSE`). When `TRUE`, one fan is drawn per
#'   stratum (faceted).
#' @param strata Character vector of columns to group on when
#'   `by_strata = TRUE`. `NULL` (default) uses the object's `strata`.
#' @param changepoint Logical (default `FALSE`). When `TRUE`, mark the estimated
#'   abrupt change point of the **median** delay (Pettitt's test, on mature data)
#'   with a vertical line, when one is detected (p < 0.05). See
#'   [test_delay_changepoint()].
#' @param level Completeness level for the immature-region shading (default
#'   `0.95`; see [autoplot()]).
#' @param plotly If `TRUE`, return an interactive \pkg{plotly} widget instead of a
#'   static plot. Default `FALSE`.
#' @param palette A named colour palette (defaults to the package palette).
#' @param ... Unused.
#'
#' @param axis Which time axis the delay is measured to: `"report"` (default)
#'   or `"confirmation"`. Both are measured *from the event*, so the two are
#'   directly comparable -- run each in turn and the gap between them is the
#'   time the laboratory adds. (This is not the same quantity as the
#'   `.confirmation_delay` column, which is the laboratory's own turnaround,
#'   measured from the report.) Needs a confirmation process (see
#'   [add_confirmation()]); cases still `"pending"` are left out.
#' @return A \pkg{ggplot2} object.
#'
#' @seealso [test_delay_drift()], [test_delay_changepoint()], [autoplot.tbl_now()]
#'
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' data(denguedat)
#' dengue <- tbl_now(denguedat,
#'   event_date = "onset_week", report_date = "report_week", verbose = FALSE
#' )
#' plot_delay_drift(dengue)
#'
#' @export
plot_delay_drift <- function(x, ..., window = NULL, step = NULL, min_n = 1,
                             by_strata = FALSE, strata = NULL, changepoint = FALSE,
                             level = 0.95, plotly = FALSE,
                             axis = c("report", "confirmation"),
                             palette = .tbl_now_palette()) {
  axis <- match.arg(axis)
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg ggplot2} is required for {.fn plot_delay_drift}.")
  }
  if (!is_tbl_now(x)) {
    cli::cli_abort("{.arg x} must be a {.cls tbl_now}.")
  }
  if (!rlang::is_bool(by_strata)) {
    cli::cli_abort("{.arg by_strata} must be a single {.val TRUE} or {.val FALSE}.")
  }
  if (!rlang::is_bool(changepoint)) {
    cli::cli_abort("{.arg changepoint} must be a single {.val TRUE} or {.val FALSE}.")
  }

  x <- ungroup(x)
  strata_cols <- if (isTRUE(by_strata)) {
    .tbl_now_resolve_strata_cols(x, strata)
  } else {
    NULL
  }

  delay_long <- .tbl_now_delay_long(x, strata_cols, axis = axis)
  if (nrow(delay_long) == 0) {
    cli::cli_abort("No reporting delays available to plot.")
  }

  event_units <- get_event_units(x)
  period_days <- .tbl_now_units_to_days(event_units)
  event_dates <- sort(unique(delay_long$event_date))

  # A 7-period window regardless of the time unit: 7 days for daily data,
  # 7 weeks for weekly data.
  if (is.null(window)) window <- 7
  if (is.null(step)) step <- max(1, round(window / 4))
  window <- as.integer(window)
  step <- as.integer(step)
  if (window < 1 || step < 1) {
    cli::cli_abort("{.arg window} and {.arg step} must be positive integers.")
  }

  halfwidth <- window / 2 * period_days
  step_days <- step * period_days
  centers <- seq(min(event_dates), max(event_dates), by = step_days)

  rolled <- .tbl_now_rolling_delay(delay_long, centers, halfwidth, min_n)
  if (nrow(rolled) == 0) {
    cli::cli_abort("No windows had at least {.arg min_n} = {min_n} case{?s}.")
  }

  maturity_threshold <- .tbl_now_maturity_threshold(x, delay_long, level)

  # Optional: locate an abrupt change point in the median delay (Pettitt), per
  # group, on the mature portion of the series.
  changepoint_lines <- NULL
  if (isTRUE(changepoint)) {
    cp_long <- delay_long
    if (!is.na(maturity_threshold)) {
      cp_long <- dplyr::filter(cp_long, .data$event_date <= maturity_threshold)
    }
    cp_series <- .tbl_now_delay_period_series(cp_long)
    cp_rows <- lapply(unique(cp_series$strata), function(stratum) {
      one <- dplyr::filter(cp_series, .data$strata == stratum)
      found <- .tbl_now_pettitt(one$median)
      if (!is.na(found$p_value) && found$p_value < 0.05) {
        dplyr::tibble(strata = stratum, xintercept = one$event_date[found$index])
      }
    })
    changepoint_lines <- dplyr::bind_rows(cp_rows)
  }

  x_label <- "Event date"
  unit_label <- switch(event_units,
    days = "days", weeks = "weeks", months = "months", years = "years", "units"
  )

  drift_caption <- paste0(
    "Rolling ", window, "-", unit_label, " window. Grey = incomplete (recent)."
  )
  if (!is.null(changepoint_lines) && nrow(changepoint_lines) > 0) {
    drift_caption <- paste0(drift_caption, " Dotdash line: possible changepoint.")
  }

  # Labels used as the legend keys (series identity is mapped, not fixed, so the
  # median/mean lines and the two bands read off a legend on top of the plot).
  lab_med  <- "Median"
  lab_mean <- "Mean"
  lab_iqr  <- "25-75%"
  lab_idr  <- "10-90%"

  plot <- ggplot2::ggplot(rolled, ggplot2::aes(x = .data$time))

  # Immature (recent) region: shade grey, do not read as drift. Use the last
  # window centre (not Inf) as the right edge so a Date x-scale is not warned.
  if (!is.na(maturity_threshold) && maturity_threshold < max(rolled$time)) {
    plot <- plot +
      ggplot2::annotate(
        "rect",
        xmin = maturity_threshold, xmax = max(rolled$time), ymin = -Inf, ymax = Inf,
        fill = palette[["muted_green"]], alpha = 0.2
      ) +
      ggplot2::geom_vline(
        xintercept = maturity_threshold,
        colour = palette[["muted_green"]], linetype = "dashed", linewidth = 0.5
      )
  }

  plot <- plot +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$q10, ymax = .data$q90, fill = lab_idr),
      alpha = 0.35
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$q25, ymax = .data$q75, fill = lab_iqr),
      alpha = 0.30
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$mean, colour = lab_mean, linetype = lab_mean),
      linewidth = 0.6
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$q50, colour = lab_med, linetype = lab_med),
      linewidth = 0.8
    ) +
    ggplot2::scale_fill_manual(
      name = NULL, breaks = c(lab_iqr, lab_idr),
      values = stats::setNames(c(palette[["accent_red"]], palette[["light_red"]]),
                               c(lab_iqr, lab_idr))
    ) +
    ggplot2::scale_colour_manual(
      name = NULL, breaks = c(lab_med, lab_mean),
      values = stats::setNames(c(palette[["accent_red"]], palette[["near_black"]]),
                               c(lab_med, lab_mean))
    ) +
    ggplot2::scale_linetype_manual(
      name = NULL, breaks = c(lab_med, lab_mean),
      values = stats::setNames(c("solid", "dashed"), c(lab_med, lab_mean))
    ) +
    ggplot2::labs(
      title = "Reporting-delay drift",
      caption = drift_caption,
      x = x_label, y = paste0("Reporting delay (", unit_label, ")")
    ) +
    .tbl_now_theme(palette) +
    ggplot2::theme(legend.position = "top")

  # Change-point marker(s), when requested and detected.
  if (!is.null(changepoint_lines) && nrow(changepoint_lines) > 0) {
    plot <- plot +
      ggplot2::geom_vline(
        data = changepoint_lines,
        ggplot2::aes(xintercept = .data$xintercept),
        colour = palette[["accent_red"]], linetype = "dotdash", linewidth = 0.7
      )
  }

  if (isTRUE(by_strata)) {
    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(.data$strata))
  }

  .as_plotly(plot, plotly)
}


# test_delay_drift()-----

#' Extract the standard fields from a `modifiedmk` result vector
#'
#' The `modifiedmk` functions each return a named numeric vector with slightly
#' different labels; this pulls the p-value, statistic, Sen's slope and tau by
#' fuzzy name match.
#'
#' @param result A named numeric vector from `mmkh()`, `mmky()` or `bbsmk()`.
#'
#' @return A list with `p_value`, `statistic`, `sens_slope` and `tau`.
#'
#' @keywords internal
#' @noRd
.tbl_now_parse_mk <- function(result) {
  pick <- function(patterns) {
    for (pattern in patterns) {
      index <- grep(pattern, names(result), ignore.case = TRUE)
      if (length(index) > 0) {
        return(unname(result[index[1]]))
      }
    }
    NA_real_
  }
  list(
    # "new P-value" (corrected) is preferred over a plain/old "P-value".
    p_value    = pick(c("new P.?value", "^P.?value", "P.?value")),
    statistic  = pick(c("Corrected Zc", "Z.?Value", "Original Z")),
    sens_slope = pick(c("Sen.?s.?slope")),
    tau        = pick(c("^Tau$", "Kendall.?s.?Tau", "Tau"))
  )
}

#' Test whether the reporting-delay distribution drifts over time
#'
#' `r lifecycle::badge("experimental")`
#'
#' Runs an **autocorrelation-robust monotonic-trend test** on the per-period,
#' count-weighted delay summaries, to answer "do delays drift over time?" in a
#' way that respects the fact that a delay series is correlated with itself.
#'
#' For each requested `stat` (and each stratum) it builds the per-event-date
#' series of that statistic and tests it for a monotonic trend with the
#' \pkg{modifiedmk} package, which corrects the Mann-Kendall variance for serial
#' autocorrelation. A plain Mann-Kendall (or an OLS slope) would be
#' anti-conservative here, because positive autocorrelation shrinks the effective
#' sample size.
#'
#' By default the test uses only **mature** event dates (those on or before the
#' `level` incompleteness cutoff), because the recent, not-yet-fully-reported
#' dates would otherwise inject a spurious downward trend.
#'
#' @param x A `tbl_now` object.
#' @param stat Which delay summaries to test: any of `"median"`, `"mean"`,
#'   `"iqr"` (q75 - q25) and `"spread"` (q90 - q10). Defaults to median + spread,
#'   i.e. one *location* and one *dispersion* statistic.
#' @param method Trend test: `"hamed-rao"` (default; Hamed-Rao variance
#'   correction, [modifiedmk::mmkh()]), `"yue-pilon"` (Yue-Pilon,
#'   [modifiedmk::mmky()]) or `"block-bootstrap"` (block-bootstrap MK,
#'   [modifiedmk::bbsmk()]). See the *Choosing a method* section.
#' @param by_strata Logical (default `FALSE`). When `TRUE`, the test is run
#'   separately per stratum.
#' @param strata Character vector of columns to group on when
#'   `by_strata = TRUE`. `NULL` (default) uses the object's `strata`.
#' @param mature_only Logical (default `TRUE`). Drop event dates after the
#'   `level` incompleteness cutoff before testing.
#' @param level Completeness level for the maturity cutoff (default `0.95`).
#' @param alpha Significance level for the `drift` verdict column (default
#'   `0.05`).
#' @param ... Passed to the underlying \pkg{modifiedmk} function (e.g. `nsim`
#'   for `"block-bootstrap"`).
#'
#' @param axis Which time axis the delay is measured to: `"report"` (default)
#'   or `"confirmation"`. Both are measured *from the event*, so the two are
#'   directly comparable -- run each in turn and the gap between them is the
#'   time the laboratory adds. (This is not the same quantity as the
#'   `.confirmation_delay` column, which is the laboratory's own turnaround,
#'   measured from the report.) Needs a confirmation process (see
#'   [add_confirmation()]); cases still `"pending"` are left out.
#' @return A [tibble][tibble::tibble] with **one row per requested `stat` per
#'   stratum**, and the following columns:
#'
#' \describe{
#'   \item{`strata`}{`character`. The stratum the row refers to. When
#'     `by_strata = FALSE` (the default) there is a single stratum labelled
#'     `"all"`; otherwise one level per observed combination of `strata`.}
#'   \item{`stat`}{`character`. Which delay summary was tested — one of
#'     `"median"`, `"mean"`, `"iqr"` or `"spread"`. `"median"`/`"mean"` are
#'     *location* statistics (are delays getting longer?); `"iqr"`/`"spread"`
#'     are *dispersion* statistics (are delays getting more erratic?).}
#'   \item{`n`}{`integer`. **Length of the tested series**, i.e. the number of
#'     event dates contributing a non-missing value after the `mature_only`
#'     filter. This is a count of *periods*, not a count of cases. Series with
#'     `n < 10` (or with zero variance) are not tested and return `NA` for every
#'     test column.}
#'   \item{`tau`}{`numeric` in `[-1, 1]`. Kendall's rank correlation between the
#'     statistic and time — the *effect size*. Positive means delays are growing,
#'     negative means they are shrinking. Roughly, `|tau|` below 0.1 is a
#'     negligible trend even when `p_value` is small.}
#'   \item{`sens_slope`}{`numeric`. Sen's slope: the median pairwise rate of
#'     change, expressed **in delay units per period** — so for weekly data with
#'     delays measured in weeks, "weeks of delay gained per week elapsed".
#'     Multiply by `n` for the total drift implied across the series. Unlike an
#'     OLS slope this is robust to outlying periods.}
#'   \item{`statistic`}{`numeric`. The autocorrelation-corrected Mann-Kendall
#'     `Z` score. Under the null it is standard normal, so `|Z| > 1.96`
#'     corresponds to `p_value < 0.05`.}
#'   \item{`p_value`}{`numeric`. Two-sided p-value for the null hypothesis of
#'     *no monotonic trend*, after the serial-correlation correction implied by
#'     `method`. `NA` when the series was too short or constant.}
#'   \item{`method`}{`character`. The `method` actually used, echoed back so the
#'     result is self-documenting when several runs are bound together.}
#'   \item{`drift`}{`logical`. The verdict: `TRUE` when
#'     `p_value < alpha`. `NA` p-values give `FALSE`, so a `FALSE` means
#'     "no drift detected" and not necessarily "no drift".}
#' }
#'
#' @section Interpreting the result:
#'
#' Read `tau` and `sens_slope` *before* `p_value`. On long surveillance series a
#' tiny, operationally irrelevant trend will still be highly significant, so
#' `drift = TRUE` on its own is not a reason to act. The question to ask is
#' whether `sens_slope * n` — the total drift implied over the observed window —
#' is large relative to the delays themselves.
#'
#' The location and dispersion statistics answer different questions and can
#' disagree, which is informative rather than contradictory:
#'
#' - `median` drifting up, `spread` flat: reporting is uniformly slower.
#' - `median` flat, `spread` drifting up: the typical case is unaffected but the
#'   tail is getting worse — often a subset of reporting sites degrading.
#' - both drifting up: broad deterioration in reporting timeliness.
#'
#' A detected drift means a nowcasting model fitted on a **fixed** delay
#' distribution will be biased, because it is averaging over delay regimes that
#' are not exchangeable. Consider a model with a time-varying delay, or fitting
#' only to the recent, homogeneous stretch of data.
#'
#' Because this is a trend test it will *not* find an abrupt one-off shift; a
#' step change can even cancel out to a non-significant monotonic trend. Pair it
#' with [test_delay_changepoint()], which is built for exactly that case.
#'
#' @section Choosing a method:
#'
#' All three options are Mann-Kendall tests that correct for the serial
#' correlation of a delay series; they differ in what they assume about that
#' correlation, and in cost.
#'
#' \describe{
#'   \item{`"hamed-rao"` (default)}{Inflates the Mann-Kendall variance using all
#'     *significant* autocorrelation lags of the detrended ranks. It makes no
#'     AR(1) assumption, is deterministic, and is effectively instantaneous, so
#'     it is the sensible default for a routine diagnostic. Its variance
#'     correction is known to be unstable on short series — treat results with
#'     `n` below roughly 30 as indicative only.}
#'   \item{`"yue-pilon"`}{Trend-free pre-whitening, which effectively assumes the
#'     series is **AR(1)**. That assumption is a poor fit for daily reporting
#'     delays, which carry strong day-of-week periodicity, and pre-whitening is
#'     known to remove part of the very trend being tested. Offered for
#'     comparability with the hydrology literature; rarely the right choice
#'     here.}
#'   \item{`"block-bootstrap"`}{Resamples contiguous blocks, so it accommodates
#'     arbitrary dependence *within* a block — including weekly periodicity, if
#'     the block length covers it. Statistically the most defensible for daily
#'     data, and the best cross-check when a `hamed-rao` result is borderline.
#'     Two caveats: it is **stochastic**, so call [set.seed()] first for a
#'     reproducible p-value, and it is **thousands of times slower** — it scales
#'     at roughly the square of the series length, so a multi-year daily series
#'     can take many minutes per statistic. Reduce `nsim` (passed through `...`)
#'     or restrict to a shorter window before reaching for it.}
#' }
#'
#' When a decision matters, run the default first and confirm a borderline
#' result with `method = "block-bootstrap"` on a restricted window.
#'
#' @seealso [test_delay_changepoint()] for abrupt shifts,
#'   [plot_delay_drift()] to visualise the series being tested.
#'
#' @examplesIf requireNamespace("modifiedmk", quietly = TRUE)
#' data(denguedat)
#' dengue <- tbl_now(denguedat,
#'   event_date = "onset_week", report_date = "report_week", verbose = FALSE
#' )
#' test_delay_drift(dengue)
#'
#' @export
test_delay_drift <- function(x, ...,
                             stat = c("median", "spread"),
                             method = c("hamed-rao", "yue-pilon", "block-bootstrap"),
                             by_strata = FALSE, strata = NULL,
                             mature_only = TRUE, level = 0.95, alpha = 0.05,
                             axis = c("report", "confirmation")) {
  axis <- match.arg(axis)
  if (!is_tbl_now(x)) {
    cli::cli_abort("{.arg x} must be a {.cls tbl_now}.")
  }
  if (!requireNamespace("modifiedmk", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg modifiedmk} is required for {.fn test_delay_drift}.",
      "i" = "Install it with {.code install.packages(\"modifiedmk\")}."
    ))
  }
  # `stat` may be several; `method` is a single choice.
  stat <- match.arg(stat, c("median", "mean", "iqr", "spread"), several.ok = TRUE)
  method <- match.arg(method)

  cli::cli_warn(
    c(
      "!" = "{.fn test_delay_drift} is {.emph experimental}: results are not \\
             guaranteed and the interface may change.",
      "i" = "Interpret a significant result as a {.emph potential} trend change, \\
             not a confirmed one."
    ),
    .frequency = "regularly",
    .frequency_id = "tbl.now::test_delay_drift"
  )

  x <- ungroup(x)
  strata_cols <- if (isTRUE(by_strata)) {
    .tbl_now_resolve_strata_cols(x, strata)
  } else {
    NULL
  }

  delay_long <- .tbl_now_delay_long(x, strata_cols, axis = axis)
  if (isTRUE(mature_only)) {
    maturity_threshold <- .tbl_now_maturity_threshold(x, delay_long, level)
    if (!is.na(maturity_threshold)) {
      delay_long <- dplyr::filter(delay_long, .data$event_date <= maturity_threshold)
    }
  }

  period_series <- .tbl_now_delay_period_series(delay_long)

  run_test <- function(values) {
    values <- values[!is.na(values)]
    if (length(values) < 10 || stats::var(values) == 0) {
      return(list(
        n = length(values), p_value = NA_real_, statistic = NA_real_,
        sens_slope = NA_real_, tau = NA_real_
      ))
    }
    result <- switch(method,
      "hamed-rao"       = modifiedmk::mmkh(values, ...),
      "yue-pilon"       = modifiedmk::mmky(values, ...),
      "block-bootstrap" = modifiedmk::bbsmk(values, ...)
    )
    parsed <- .tbl_now_parse_mk(result)
    c(list(n = length(values)), parsed)
  }

  strata_levels <- unique(period_series$strata)
  rows <- lapply(strata_levels, function(stratum) {
    series <- dplyr::filter(period_series, .data$strata == stratum)
    per_stat <- lapply(stat, function(one_stat) {
      outcome <- run_test(series[[one_stat]])
      dplyr::tibble(
        strata     = stratum,
        stat       = one_stat,
        n          = outcome$n,
        tau        = outcome$tau,
        sens_slope = outcome$sens_slope,
        statistic  = outcome$statistic,
        p_value    = outcome$p_value,
        method     = method,
        drift      = !is.na(outcome$p_value) & outcome$p_value < alpha
      )
    })
    dplyr::bind_rows(per_stat)
  })

  dplyr::bind_rows(rows)
}


# test_delay_changepoint()-----

#' Pettitt's nonparametric single-change-point test
#'
#' Detects a single abrupt shift in the location of a series. Rank-based (so it
#' is robust and distribution-free) and matches `trend::pettitt.test()`. The
#' change-point statistic uses the identity
#' `U_t = 2 * cumsum(rank(x)) - t * (n + 1)`, `K = max|U_t|`, with the standard
#' approximate p-value `2 * exp(-6 K^2 / (n^3 + n^2))`.
#'
#' @param values A numeric series (ordered in time).
#'
#' @return A list with `n`, `index` (position of the last point *before* the
#'   change, `1..n-1`), `statistic` (`K`) and `p_value`; all `NA` when the series
#'   is too short (`< 8`) or constant.
#'
#' @keywords internal
#' @noRd
.tbl_now_pettitt <- function(values) {
  values <- values[!is.na(values)]
  n <- length(values)
  na_result <- list(n = n, index = NA_integer_, statistic = NA_real_, p_value = NA_real_)
  if (n < 8 || stats::var(values) == 0) {
    return(na_result)
  }
  ranks <- rank(values)
  # U_t for t = 1 .. n-1 (a change is placed *after* position t).
  u_t <- (2 * cumsum(ranks) - seq_len(n) * (n + 1))[-n]
  k_statistic <- max(abs(u_t))
  change_index <- which.max(abs(u_t))
  p_value <- min(1, 2 * exp(-6 * k_statistic^2 / (n^3 + n^2)))
  list(
    n = n, index = change_index, statistic = k_statistic, p_value = p_value
  )
}

#' Detect an abrupt change point in the reporting-delay distribution
#'
#' `r lifecycle::badge("experimental")`
#'
#' Complements [test_delay_drift()]. Where that tests for a *gradual* monotonic
#' trend, this tests for a **single abrupt shift** (e.g. a reporting-system change
#' on some date) in the per-period delay summaries, using **Pettitt's**
#' nonparametric change-point test. As with [test_delay_drift()] it works on both
#' a location statistic (median / mean) and a dispersion statistic (IQR / 10-90
#' spread), on mature data only, and — being rank-based — it is robust to the
#' skew and serial dependence of a delay series.
#'
#' @inheritParams test_delay_drift
#'
#' @param axis Which time axis the delay is measured to: `"report"` (default)
#'   or `"confirmation"`. Both are measured *from the event*, so the two are
#'   directly comparable -- run each in turn and the gap between them is the
#'   time the laboratory adds. (This is not the same quantity as the
#'   `.confirmation_delay` column, which is the laboratory's own turnaround,
#'   measured from the report.) Needs a confirmation process (see
#'   [add_confirmation()]); cases still `"pending"` are left out.
#' @return A [tibble][tibble::tibble] with **one row per requested `stat` per
#'   stratum**, and the following columns:
#'
#' \describe{
#'   \item{`strata`}{`character`. The stratum the row refers to. When
#'     `by_strata = FALSE` (the default) there is a single stratum labelled
#'     `"all"`; otherwise one level per observed combination of `strata`.}
#'   \item{`stat`}{`character`. Which delay summary was tested — one of
#'     `"median"`, `"mean"`, `"iqr"` or `"spread"`. As in
#'     [test_delay_drift()], the first two are *location* statistics and the
#'     last two *dispersion* statistics.}
#'   \item{`n`}{`integer`. **Length of the tested series**: the number of event
#'     dates contributing a non-missing value after the `mature_only` filter —
#'     periods, not cases. Series shorter than 8 periods, or with zero variance,
#'     are not tested and return `NA` throughout.}
#'   \item{`changepoint`}{`Date`. The event date of the **last period before the
#'     estimated change**; the shift is taken to occur immediately after it.
#'     `NA` when the series was too short to test. Note this is reported even
#'     when `changepoint_detected` is `FALSE` — Pettitt's test always returns
#'     the most extreme candidate split, so this field is only meaningful once
#'     the p-value supports it.}
#'   \item{`statistic`}{`numeric`. Pettitt's `K`, the maximum absolute value of
#'     the rank statistic `U_t` over all candidate split points. Larger means a
#'     cleaner separation between the two sides. It is not standardised, so it
#'     grows with `n` and is not comparable across series of different lengths.}
#'   \item{`p_value`}{`numeric`. Two-sided p-value for the null of *no change
#'     point*, from the standard approximation
#'     \eqn{2\exp(-6K^2 / (n^3 + n^2))}, capped at 1. This approximation is
#'     known to be conservative for small `n`.}
#'   \item{`before`, `after`}{`numeric`. The mean of the statistic on each side
#'     of `changepoint`, in the object's delay units. These are plain means of
#'     the per-period summaries, so they describe the two regimes directly.}
#'   \item{`shift`}{`numeric`. `after - before`: the estimated size and
#'     direction of the jump, in delay units. Positive means delays got longer
#'     after the change point. This is the number to judge operational
#'     relevance by.}
#'   \item{`changepoint_detected`}{`logical`. The verdict: `TRUE` when
#'     `p_value < alpha`. `NA` p-values give `FALSE`.}
#' }
#'
#' @section Interpreting the result:
#'
#' Judge `shift` first and `p_value` second. A statistically detected change
#' point with a `shift` far smaller than the day-to-day noise in the delay
#' series is not worth acting on; a large `shift` is, even at a marginal
#' p-value.
#'
#' Two structural caveats matter in practice:
#'
#' - Pettitt's test assumes **exactly one** change point. Given several, it
#'   returns the most prominent and silently ignores the rest. If you suspect
#'   more, re-run on each side of the first `changepoint` to search recursively.
#' - A slow monotonic drift will often trip this test too, with the change point
#'   landing near the middle of the series. Running [test_delay_drift()]
#'   alongside disambiguates: a genuine step shows up here and not
#'   necessarily there, while a gradual drift shows up in both.
#'
#' A confirmed change point usually has an operational explanation — a new
#' laboratory information system, a change in case definition, a reporting
#' mandate, a holiday backlog being cleared. Where it lands is a strong hint
#' about the cause, and about how far back a nowcasting model can safely be
#' fitted: data before the change point comes from a different reporting regime.
#'
#' Unlike [test_delay_drift()], this test has no third-party dependency and no
#' meaningful runtime cost, so it is cheap to run routinely.
#'
#' @seealso [test_delay_drift()] for gradual trends,
#'   [plot_delay_drift()] to visualise the series and mark detected changes.
#'
#' @examples
#' data(denguedat)
#' dengue <- tbl_now(denguedat,
#'   event_date = "onset_week", report_date = "report_week", verbose = FALSE
#' )
#' test_delay_changepoint(dengue)
#'
#' @export
test_delay_changepoint <- function(x, ...,
                                   stat = c("median", "spread"),
                                   by_strata = FALSE, strata = NULL,
                                   mature_only = TRUE, level = 0.95, alpha = 0.05,
                                   axis = c("report", "confirmation")) {
  axis <- match.arg(axis)
  if (!is_tbl_now(x)) {
    cli::cli_abort("{.arg x} must be a {.cls tbl_now}.")
  }
  stat <- match.arg(stat, c("median", "mean", "iqr", "spread"), several.ok = TRUE)

  cli::cli_warn(
    c(
      "!" = "{.fn test_delay_changepoint} is {.emph experimental}: results are \\
             not guaranteed and the interface may change.",
      "i" = "Treat a detected change as a {.emph potential} change point, not a \\
             confirmed one."
    ),
    .frequency = "regularly",
    .frequency_id = "tbl.now::test_delay_changepoint"
  )

  x <- ungroup(x)
  strata_cols <- if (isTRUE(by_strata)) {
    .tbl_now_resolve_strata_cols(x, strata)
  } else {
    NULL
  }

  delay_long <- .tbl_now_delay_long(x, strata_cols, axis = axis)
  if (isTRUE(mature_only)) {
    maturity_threshold <- .tbl_now_maturity_threshold(x, delay_long, level)
    if (!is.na(maturity_threshold)) {
      delay_long <- dplyr::filter(delay_long, .data$event_date <= maturity_threshold)
    }
  }

  period_series <- .tbl_now_delay_period_series(delay_long)

  strata_levels <- unique(period_series$strata)
  rows <- lapply(strata_levels, function(stratum) {
    series <- dplyr::filter(period_series, .data$strata == stratum)
    per_stat <- lapply(stat, function(one_stat) {
      values <- series[[one_stat]]
      outcome <- .tbl_now_pettitt(values)

      changepoint_date <- NA
      before <- NA_real_
      after <- NA_real_
      if (!is.na(outcome$index)) {
        changepoint_date <- series$event_date[outcome$index]
        before <- mean(values[seq_len(outcome$index)], na.rm = TRUE)
        after <- mean(values[(outcome$index + 1):outcome$n], na.rm = TRUE)
      }

      dplyr::tibble(
        strata               = stratum,
        stat                 = one_stat,
        n                    = outcome$n,
        changepoint          = changepoint_date,
        statistic            = outcome$statistic,
        p_value              = outcome$p_value,
        before               = before,
        after                = after,
        shift                = after - before,
        changepoint_detected = !is.na(outcome$p_value) & outcome$p_value < alpha
      )
    })
    dplyr::bind_rows(per_stat)
  })

  dplyr::bind_rows(rows)
}
