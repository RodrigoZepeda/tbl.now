# Detecting "batch" reporting — report dates on which a laboratory dumps a
# backlog of many old cases at once, as opposed to the steady trickle of normal
# reporting.
#
# Operationally a batch is a *report date* that is anomalous on the report axis.
# Four signals (any combination can be required):
#   volume — unusually many cases reported that day
#   delay  — those reports have unusually long delays (a cleared backlog); this
#            is the signal that separates a batch from an EPIDEMIC PEAK, whose
#            reports keep the normal (short) delay distribution
#   span   — the report date covers an unusually large number of distinct event
#            dates
#   gap    — the report date is preceded by a run of unusually low reporting
#            (the "silence then dump" cadence)
#
# `detect_report_batches()` returns a per-report-date diagnostic table with the
# features, the robust anomaly scores and a `batch` flag (the AND of the
# activated signals). `plot_report_batches()` visualises it.


# Internal helpers-----

#' Long (report_date, event_date, delay, weight) table
#'
#' @param object A `tbl_now` object.
#' @param strata_cols Optional strata columns; when supplied a combined `strata`
#'   label is added.
#'
#' @return A tibble with `report_date`, `event_date`, `delay`, `weight` and,
#'   optionally, `strata`.
#'
#' @keywords internal
#' @noRd
.tbl_now_report_long <- function(object, strata_cols = NULL) {
  incidence <- object |>
    ungroup() |>
    to_count(to = "count-incidence")
  case_count_column <- get_case_count(incidence)
  report_date_column <- get_report_date(object)
  event_date_column <- get_event_date(object)
  observations <- as.data.frame(incidence)

  out <- dplyr::tibble(
    report_date = observations[[report_date_column]],
    event_date  = observations[[event_date_column]],
    delay       = observations[[".delay"]],
    weight      = observations[[case_count_column]]
  )
  if (!is.null(strata_cols)) {
    out$strata <- .tbl_now_strata_label(observations, strata_cols)
  }
  dplyr::filter(
    out,
    !is.na(.data$report_date), !is.na(.data$delay),
    !is.na(.data$weight), .data$weight > 0
  )
}

#' Centred rolling median and (scaled) MAD, NA-aware
#'
#' @param values A numeric series.
#' @param window Window width in observations.
#'
#' @return A list with `median` and `mad` vectors (the MAD already multiplied by
#'   1.4826; zero/undefined MADs are replaced by a global fallback so robust
#'   z-scores never blow up).
#'
#' @keywords internal
#' @noRd
.tbl_now_rolling_median_mad <- function(values, window) {
  n <- length(values)
  half <- floor(window / 2)
  med <- rep(NA_real_, n)
  mad_value <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    lo <- max(1L, i - half)
    hi <- min(n, i + half)
    w <- values[lo:hi]
    w <- w[!is.na(w)]
    if (length(w) == 0) next
    med[i] <- stats::median(w)
    mad_value[i] <- stats::mad(w, constant = 1.4826)
  }
  global_mad <- stats::mad(values[!is.na(values)], constant = 1.4826)
  if (is.na(global_mad) || global_mad == 0) {
    global_mad <- stats::sd(values[!is.na(values)])
  }
  if (is.na(global_mad) || global_mad == 0) global_mad <- 1
  mad_value[is.na(mad_value) | mad_value == 0] <- global_mad
  list(median = med, mad = mad_value)
}

#' Per-report-date features on a complete report-date grid, with anomaly scores
#'
#' Builds one row per report-date period (filling report dates with no reports as
#' zeros, so silence is represented), computes the four robust anomaly scores and
#' the `batch` flag for a single group.
#'
#' @param report_long A one-group long table from `.tbl_now_report_long()`.
#' @param period_days Days per report period.
#' @param backlog_delay Delay at/above which a report counts as backlog.
#' @param baseline_window Rolling-baseline width, in report periods.
#' @param gap_window Number of preceding periods used for the `gap` signal.
#' @param threshold Robust-z cutoff a signal must exceed to fire.
#' @param signals Which signals are required (their scores are AND-ed).
#' @param min_reports Minimum reports for a date to be eligible.
#'
#' @return A tibble (the full grid) with features, `score_*` columns and `batch`.
#'
#' @keywords internal
#' @noRd
.tbl_now_report_batch_grid_one <- function(report_long, period_days, backlog_delay,
                                           baseline_window, gap_window, threshold,
                                           signals, min_reports) {
  features <- report_long |>
    dplyr::group_by(.data$report_date) |>
    dplyr::summarise(
      n_reports     = sum(.data$weight),
      n_event_dates = dplyr::n_distinct(.data$event_date),
      mean_delay    = stats::weighted.mean(.data$delay, .data$weight),
      median_delay  = .tbl_now_weighted_quantile(.data$delay, .data$weight, 0.5),
      max_delay     = max(.data$delay),
      backlog       = sum(.data$weight[.data$delay >= backlog_delay]),
      .groups = "drop"
    )

  # Complete grid so silence (zero-report periods) is represented.
  grid_dates <- seq(min(features$report_date), max(features$report_date),
    by = period_days
  )
  grid <- dplyr::tibble(report_date = grid_dates) |>
    dplyr::left_join(features, by = "report_date") |>
    dplyr::arrange(.data$report_date)
  # Counts are zero on silent days; delay features stay NA (undefined).
  grid$n_reports[is.na(grid$n_reports)] <- 0
  grid$n_event_dates[is.na(grid$n_event_dates)] <- 0
  grid$backlog[is.na(grid$backlog)] <- 0

  volume_baseline <- .tbl_now_rolling_median_mad(grid$n_reports, baseline_window)
  delay_baseline <- .tbl_now_rolling_median_mad(grid$mean_delay, baseline_window)
  span_baseline <- .tbl_now_rolling_median_mad(grid$n_event_dates, baseline_window)

  # Mean reporting volume over the preceding `gap_window` periods (grid order).
  n <- nrow(grid)
  prev_mean <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    lo <- i - gap_window
    if (lo < 1L) next
    prev_mean[i] <- mean(grid$n_reports[lo:(i - 1L)])
  }

  grid$score_volume <- (grid$n_reports - volume_baseline$median) / volume_baseline$mad
  grid$score_delay <- (grid$mean_delay - delay_baseline$median) / delay_baseline$mad
  grid$score_span <- (grid$n_event_dates - span_baseline$median) / span_baseline$mad
  # gap fires when the preceding window was unusually LOW relative to baseline.
  grid$score_gap <- (volume_baseline$median - prev_mean) / volume_baseline$mad

  score_columns <- c(
    volume = "score_volume", delay = "score_delay",
    span = "score_span", gap = "score_gap"
  )
  fires <- lapply(signals, function(signal) {
    score <- grid[[score_columns[[signal]]]]
    !is.na(score) & score >= threshold
  })
  all_fire <- Reduce(`&`, fires)

  grid$batch <- all_fire & grid$n_reports >= min_reports
  grid
}

#' Build the report-date batch grid for every group
#'
#' @inheritParams .tbl_now_report_batch_grid_one
#' @param object A `tbl_now`.
#' @param strata_cols Optional strata columns.
#' @param backlog_delay Delay threshold, or `NULL` to use the `backlog_quantile`.
#' @param backlog_quantile Delay quantile used when `backlog_delay` is `NULL`.
#'
#' @return The full grid tibble (all periods), with a `strata` column when
#'   grouped.
#'
#' @keywords internal
#' @noRd
.tbl_now_report_batch_grid <- function(object, strata_cols, signals, threshold,
                                       backlog_delay, backlog_quantile,
                                       baseline_window, gap_window, min_reports) {
  report_long <- .tbl_now_report_long(object, strata_cols)
  if (nrow(report_long) == 0) {
    cli::cli_abort("No reports available to scan for batches.")
  }

  if (is.null(backlog_delay)) {
    backlog_delay <- .tbl_now_weighted_quantile(
      report_long$delay, report_long$weight, backlog_quantile
    )
    if (is.na(backlog_delay)) backlog_delay <- 0
  }

  period_days <- .tbl_now_units_to_days(get_report_units(object))

  if (is.null(baseline_window)) {
    baseline_window <- max(7, round(56 / period_days))
  }

  if (is.null(strata_cols)) {
    return(.tbl_now_report_batch_grid_one(
      report_long, period_days, backlog_delay, baseline_window, gap_window,
      threshold, signals, min_reports
    ))
  }

  groups <- split(report_long, report_long$strata)
  per_group <- lapply(names(groups), function(stratum) {
    out <- .tbl_now_report_batch_grid_one(
      groups[[stratum]], period_days, backlog_delay, baseline_window, gap_window,
      threshold, signals, min_reports
    )
    out$strata <- stratum
    out
  })
  dplyr::bind_rows(per_group)
}


# detect_report_batches()-----

#' Detect batch reporting (backlog dumps) on the report-date axis
#'
#' `r lifecycle::badge("experimental")`
#'
#' Laboratories sometimes withhold results and then release a whole backlog at
#' once. Operationally such a **batch** is a *report date* that is anomalous on
#' the report axis. This scans each report-date period and flags batches using up
#' to four robust signals, of which you choose the combination to require:
#'
#' * `"volume"` — unusually many cases reported that period (robust z of the
#'   report count vs a rolling median/MAD baseline).
#' * `"delay"` — the reports that period have unusually **long** delays (a cleared
#'   backlog). **This is the signal that distinguishes a batch from an epidemic
#'   peak**: an epidemic peak also inflates the volume, but its cases are still
#'   reported with the normal, short delay distribution, so its `delay` score
#'   stays low.
#' * `"span"` — the period covers an unusually large number of **distinct event
#'   dates**.
#' * `"gap"` — the period is preceded by a run of unusually **low** reporting (the
#'   "silence then dump" cadence).
#'
#' A report date is flagged when **all** the activated `signals` fire (each score
#' at or above `threshold`). Requiring `"delay"` (or `"span"`) alongside
#' `"volume"` is what keeps epidemic peaks from being flagged.
#'
#' @param x A `tbl_now` object.
#' @param signals Character vector of the signals to require (AND-ed). Any of
#'   `"volume"`, `"delay"`, `"span"`, `"gap"`. Defaults to
#'   `c("volume", "delay")`.
#' @param threshold Robust-z cutoff a signal must reach to fire (default `3`).
#' @param backlog_delay Delay at/above which a report counts as backlog (for the
#'   `backlog` column). `NULL` (default) uses the `backlog_quantile` of the
#'   observed delays.
#' @param backlog_quantile Delay quantile used to set `backlog_delay` when it is
#'   `NULL` (default `0.9`).
#' @param baseline_window Rolling-baseline width in report periods. `NULL`
#'   (default) uses roughly two months.
#' @param gap_window Number of preceding periods assessed by the `"gap"` signal
#'   (default `3`).
#' @param min_reports Minimum reports for a period to be eligible (default `1`).
#' @param by_strata Logical (default `FALSE`). Detect batches separately per
#'   stratum.
#' @param strata Character vector of columns to group on when `by_strata = TRUE`.
#'   `NULL` (default) uses the object's `strata`.
#' @param ... Unused.
#'
#' @return A tibble with one row per candidate report date (`n_reports > 0`),
#'   carrying `report_date` (and `strata` when grouped), the features
#'   (`n_reports`, `n_event_dates`, `mean_delay`, `median_delay`, `max_delay`,
#'   `backlog`), the robust scores (`score_volume`, `score_delay`, `score_span`,
#'   `score_gap`) and the `batch` flag.
#'
#' @seealso [plot_report_batches()]
#'
#' @examples
#' data(denguedat)
#' dengue <- tbl_now(denguedat,
#'   event_date = "onset_week", report_date = "report_week", verbose = FALSE
#' )
#' batches <- detect_report_batches(dengue)
#' batches[batches$batch, ]
#'
#' @export
detect_report_batches <- function(x, ...,
                                  signals = c("volume", "delay"),
                                  threshold = 3,
                                  backlog_delay = NULL, backlog_quantile = 0.9,
                                  baseline_window = NULL, gap_window = 3,
                                  min_reports = 1,
                                  by_strata = FALSE, strata = NULL) {
  if (!is_tbl_now(x)) {
    cli::cli_abort("{.arg x} must be a {.cls tbl_now}.")
  }
  signals <- match.arg(signals, c("volume", "delay", "span", "gap"),
    several.ok = TRUE
  )
  if (!is.numeric(threshold) || length(threshold) != 1 || threshold <= 0) {
    cli::cli_abort("{.arg threshold} must be a single positive number.")
  }

  x <- ungroup(x)
  strata_cols <- if (isTRUE(by_strata)) {
    .tbl_now_resolve_strata_cols(x, strata)
  } else {
    NULL
  }

  grid <- .tbl_now_report_batch_grid(
    x, strata_cols, signals, threshold, backlog_delay, backlog_quantile,
    baseline_window, gap_window, min_reports
  )

  # Return only real candidate report dates (drop the zero-filled silent grid).
  dplyr::filter(grid, .data$n_reports > 0)
}


# plot_report_batches()-----

#' Visualise batch reporting over the report-date axis
#'
#' `r lifecycle::badge("experimental")`
#'
#' Two stacked panels over the report date: the **reporting volume** and the
#' **mean reporting delay** of each report date, with the batches detected by
#' [detect_report_batches()] marked. A batch shows up as a spike in *both* the
#' volume and the delay (a backlog of old cases cleared at once), which is what
#' separates it from an epidemic peak (a volume spike with a normal, short
#' delay).
#'
#' @inheritParams detect_report_batches
#' @param palette A named colour palette (defaults to the package palette).
#'
#' @return A \pkg{ggplot2} object.
#'
#' @seealso [detect_report_batches()]
#'
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' data(denguedat)
#' dengue <- tbl_now(denguedat,
#'   event_date = "onset_week", report_date = "report_week", verbose = FALSE
#' )
#' plot_report_batches(dengue)
#'
#' @export
plot_report_batches <- function(x, ...,
                                signals = c("volume", "delay"),
                                threshold = 3,
                                backlog_delay = NULL, backlog_quantile = 0.9,
                                baseline_window = NULL, gap_window = 3,
                                min_reports = 1,
                                by_strata = FALSE, strata = NULL,
                                palette = .tbl_now_palette()) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg ggplot2} is required for {.fn plot_report_batches}.")
  }
  if (!is_tbl_now(x)) {
    cli::cli_abort("{.arg x} must be a {.cls tbl_now}.")
  }
  signals <- match.arg(signals, c("volume", "delay", "span", "gap"),
    several.ok = TRUE
  )

  x <- ungroup(x)
  strata_cols <- if (isTRUE(by_strata)) {
    .tbl_now_resolve_strata_cols(x, strata)
  } else {
    NULL
  }

  grid <- .tbl_now_report_batch_grid(
    x, strata_cols, signals, threshold, backlog_delay, backlog_quantile,
    baseline_window, gap_window, min_reports
  )

  unit_label <- switch(get_report_units(x),
    days = "days", weeks = "weeks", months = "months", years = "years", "units"
  )

  # Long form: one facet row per measure (volume, mean delay).
  measure_levels <- c("Reports", paste0("Mean delay (", unit_label, ")"))
  has_strata <- "strata" %in% names(grid)
  to_long <- function(value_col, measure) {
    keep <- c("report_date", "batch", if (has_strata) "strata")
    out <- grid[, keep, drop = FALSE]
    out$value <- grid[[value_col]]
    out$measure <- factor(measure, levels = measure_levels)
    out
  }
  long <- dplyr::bind_rows(
    to_long("n_reports", measure_levels[1]),
    to_long("mean_delay", measure_levels[2])
  )
  batches <- long[long$batch & !is.na(long$value), , drop = FALSE]

  plot <- ggplot2::ggplot(long, ggplot2::aes(x = .data$report_date, y = .data$value)) +
    ggplot2::geom_line(colour = palette[["medium_green"]], linewidth = 0.4) +
    ggplot2::geom_point(
      data = batches,
      colour = palette[["accent_red"]], size = 1.6
    ) +
    ggplot2::labs(
      title = "Batch reporting scan",
      subtitle = paste0(
        "Red points: potential batches (signals: ",
        paste(signals, collapse = " & "), ")."
      ),
      x = "Report date", y = NULL
    ) +
    .tbl_now_theme(palette)

  if (isTRUE(by_strata)) {
    plot <- plot +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data$measure), cols = ggplot2::vars(.data$strata),
        scales = "free_y"
      )
  } else {
    plot <- plot +
      ggplot2::facet_wrap(ggplot2::vars(.data$measure), ncol = 1, scales = "free_y")
  }

  plot
}
