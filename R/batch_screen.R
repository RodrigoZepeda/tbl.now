# =============================================================================
# Model-free batch screening on the registration (report) axis
# =============================================================================
# Everything in this file is *model-free*: it needs only a `tbl_now` and no fitted
# nowcasting model.  That is what lets it live in `tbl.now` rather than in a
# downstream modelling package -- a batch is a property of the reporting process,
# visible in the raw reporting triangle before any model is chosen.
#
# THE PROBLEM.  Every reported item carries two timestamps: when it happened
# (the *event* / origin date `t`) and when it was recorded (the *report* /
# registration date `r`).  A **batch** is a report date at which a recording
# system that had stalled releases a backlog all at once.  Its fingerprint is a
# run of unusually low reporting followed by a spike, with inflated delays and
# an unusually large number of distinct event dates contributing.
#
# THE KEY IDEA.  A batch *moves* items along the report axis; it does not
# *create* them.  So if we draw a window of report dates around a candidate
# spike and count everything inside it, that count is unchanged by the batch --
# every item we would have seen in the window, we still see in the window, just
# on a different day.  A genuine surge, by contrast, adds items and inflates the
# window total.  This is what separates the two.
#
# Writing `R_r` for the number of items reported on date `r` and `mu_r` for its
# expected value under "no batch", over the window W = {r-k, ..., r}:
#
#   S = sum_{j in W} R_j        (window total -- invariant to transport)
#   M = sum_{j in W} mu_j       (its null mean)
#   Delta_r(k) = S - M          (excess *of the whole window*: sees only creation)
#   W_r(k) = sum_{j<r} (mu_j - R_j)   (deficit before r: sees only transport)
#
# and the diagnosis reads off a 2x2 table:
#
#   Delta ~ 0, W >> 0  -> batch (mass merely moved)
#   Delta >> 0, W ~ 0  -> genuine surge (mass created)
#   Delta << 0         -> hold still in progress, or reports lost past `now`
#
# `mu_r` is estimated model-free with a robust local line (never a mean: a batch
# corrupts a run of consecutive dates, and a mean would drag the baseline toward
# the very anomaly we are hunting).  Crucially, the baseline for a candidate
# window is refitted from report dates lying strictly OUTSIDE that window.  The
# transport never crosses the window boundary, so `M` cannot see the batch -- and
# `Delta = S - M` is then invariant to it *pathwise*, not merely on average.
# Smoothing through the episode instead would let the deficits drag `M` down and
# the batch would mask itself as a surge.
# =============================================================================

#' Screen the report axis for batched reporting
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Detects **batches**: report dates at which a stalled reporting system releases
#' a backlog.  A batch *moves* reports along the report axis without creating
#' them, so it shows up as a spike preceded by a deficit, while the total over a
#' window spanning both is unchanged.  `diagnose_batches()` is completely
#' **model-free** -- it needs only a [tbl_now()], not a fitted model -- which
#' makes it the right tool for exploratory data analysis before any nowcasting
#' model is chosen.
#'
#' @details
#' The idea is that a batch **moves** reports along the report axis without
#' creating them. Over a window of report dates that spans both the lull and the
#' release, the *total* is therefore unchanged -- every report you would have seen
#' in the window you still see, just on a different day. A genuine surge instead
#' **adds** reports and inflates the window total. So two quantities separate the
#' two cases: the **deficit** (how many reports the days just before the spike
#' were missing) picks up transport, and the window total (relative to a baseline)
#' picks up creation. A batch has a large deficit but a conserved window total; a
#' surge has an inflated window total but no deficit.
#'
#' The baseline for each candidate window is refit from report dates lying
#' strictly *outside* that window, using a robust local line (Siegel's repeated
#' median). Smoothing through the episode instead would let the deficit drag the
#' baseline down and the batch would mask itself as a surge.
#'
#' The `batch` flag is the trustworthy verdict: it compares dates *within* one
#' window (insensitive to the overall level) and is **Benjamini-Hochberg
#' corrected** across every (report date, stratum) pair, so it controls the false
#' discovery rate rather than firing on every point that crosses a raw threshold.
#' A per-point creation ("surge") label is deliberately *not* returned: it only
#' compares the window total against the baseline, so on a steeply curved epidemic
#' curve it fires on ordinary growth. If you need genuine surges, fit a model.
#' For `"count-cumulative"` data the increments are signed and `reported` can be
#' negative (a down-revision).
#'
#' A reporting system that is always closed at weekends produces every batch
#' symptom, every week, so `diagnose_batches()` needs the length of any scheduled
#' cycle. It reads that from the object's temporal effects when it can: a
#' **day-of-week** effect sets `period = 7`, a **week-of-year** effect
#' `period = 52` (see [add_temporal_effects()]). Pass `period` yourself to
#' override; if the data is daily and carries no temporal effect, the function
#' suggests `period = 7`. With a period set, the baseline is corrected by per-phase
#' medians across cycles, so an irregular batch reads as an excursion relative to
#' the schedule.
#'
#' @param x A [tbl_now()] object of any `data_type`.
#' @param lookback Integer `k`: how many report dates before `r` the window
#'   reaches back.  Should comfortably cover the longest plausible stall.
#'   Default `7` (a week of daily reporting).
#' @param baseline_window Odd integer width of the smoother used to estimate the
#'   baseline (a robust local line, Siegel's repeated median).  Must satisfy
#'   `baseline_window >= 2 * lookback + 3` so that a clean date is never outvoted
#'   by a batch episode.  Defaults to the smallest admissible odd value (adjusted
#'   upward to a multiple of `period` plus one when `period` is supplied).
#' @param period Optional integer cycle length of a *scheduled* reporting pattern
#'   (e.g. `7` for a weekly cycle on daily data). `NULL` (default) means the cycle
#'   is taken from the object's temporal effects if present (day-of-week -> `7`,
#'   week-of-year -> `52`), and otherwise no calendar correction is applied. A
#'   value passed here always wins.
#' @param null_model `"auto"` (default) picks the null from the data. The exact
#'   Poisson/Binomial null assumes Poisson counts *and* a baseline that captures
#'   the mean; real surveillance counts are overdispersed, so on non-negative
#'   counts `auto` uses the exact null only when no overdispersion is detected
#'   (dispersion at most 1.5) and otherwise falls back to the dispersion-corrected
#'   robust normal approximation. Signed (count-cumulative) increments always use
#'   the robust null. `"poisson"` and `"robust"` force the choice; note that
#'   `"poisson"` is anti-conservative (over-flags) on overdispersed counts.
#' @param axis Which time axis to scan for arrivals: `"report"` (default) or
#'   `"validation"`. The question is the same either way -- did an unusual
#'   number of records land on this date? -- so a laboratory clearing its
#'   backlog is found exactly as a surveillance system clearing its inbox is.
#'   `"validation"` needs a validation process (see [add_validation_date()])
#'   and ignores cases that are still `"pending"`, which have no validation
#'   date to arrive on.
#' @param alpha Significance level for the Benjamini-Hochberg `batch` flag.
#'   Default `0.05`.
#'
#' @returns A tibble of class `diagnose_batches`, one row per (report date, stratum),
#'   with a `print()` method that summarises the flagged dates. Columns:
#'   \describe{
#'     \item{`report_date`}{The report (registration) date the row describes.}
#'     \item{`stratum`}{The stratum label, or `"all"` when the data is unstratified.}
#'     \item{`reported`}{Reports recorded on `report_date` (a signed increment for
#'       `"count-cumulative"` data, so it can be negative).}
#'     \item{`baseline`}{The robust expected number of reports on `report_date`
#'       under "no batch", from the leave-window-out local line.}
#'     \item{`deficit`}{How many reports the `lookback` days *before* `report_date`
#'       were missing relative to baseline -- the **transport** signal. Large and
#'       positive when a stall preceded a spike.}
#'     \item{`delta`}{The window total minus its baseline mean -- the **creation**
#'       signal. Near zero for a pure batch (mass only moved), large for a surge.}
#'     \item{`p_transport`}{One-sided p-value that the deficit is larger than noise
#'       (the raw, per-point transport test).}
#'     \item{`p_transport_bh`}{`p_transport` after a Benjamini-Hochberg correction
#'       across all rows; the flag below thresholds this.}
#'     \item{`batch`}{Logical verdict: `TRUE` when `p_transport_bh < alpha` and the
#'       window is not still depleted (a hold). This is the column to trust.}
#'   }
#'
#' @seealso
#' [diagnose_batch_shape()] for the complementary test on *which* event dates a
#' flagged report date drew from; [transport_discriminant()] for the two
#' coordinates behind the test, without the hypothesis test on top;
#' [simulate_batch()] to plant a known batch and check the screen finds it;
#' [plot_reporting_process()] and [plot_reporting_triangle()] to see it;
#' [add_is_censored_report()][add] to record a batch once you believe it. The
#' [*Diagnosing a tbl_now* article](https://rodrigozepeda.github.io/tbl.now/articles/diagnosing-a-tbl-now.html)
#' works through a real example.
#'
#' @examples
#' data(denguedat)
#'
#' dengue_tbl <- tbl_now(
#'   denguedat,
#'   event_date  = onset_week,
#'   report_date = report_week,
#'   data_type   = "linelist",
#'   verbose     = FALSE
#' )
#'
#' # Scan every report date for an unusually large arrival.
#' screened <- diagnose_batches(dengue_tbl, lookback = 2)
#' head(screened)
#'
#' # The dates it flagged, strongest evidence first. Treat these as candidates to
#' # look into, not as confirmed backlog releases. `reported` against `baseline`
#' # says how much bigger the arrival was than the surrounding days led you to
#' # expect.
#' flagged <- screened[screened$batch, ]
#' nrow(flagged)
#' flagged[order(flagged$p_transport_bh), c("report_date", "reported", "baseline")]
#'
#' @export
diagnose_batches <- function(x,
                         lookback        = 7L,
                         baseline_window = NULL,
                         period          = NULL,
                         null_model      = c("auto", "poisson", "robust"),
                         axis            = c("report", "validation"),
                         alpha           = 0.05) {
  null_model      <- match.arg(null_model)
  axis            <- match.arg(axis)
  .batch_experimental_warning("diagnose_batches")
  .batch_check_tbl_now(x)

  lookback <- as.integer(lookback)
  if (lookback < 1L) {
    cli::cli_abort("`lookback` must be a positive integer. Got {lookback}.")
  }
  if (alpha <= 0 || alpha >= 1) {
    cli::cli_abort("`alpha` must lie strictly between 0 and 1. Got {alpha}.")
  }

  # Fill in / sanity-check the calendar period from the object's temporal effects.
  period <- .batch_resolve_period(x, period)

  # -- 1-4. reporting totals, baseline and the window statistics Delta and W ----
  registration <- .batch_registration(x, lookback, baseline_window, period, axis = axis)

  # -- 5. p-values under the appropriate null ---------------------------------
  # The exact Poisson/Binomial null is only valid when the counts are Poisson AND
  # the baseline captures the mean. Real surveillance counts are overdispersed (a
  # shared per-date random effect) and/or carry mean structure the baseline cannot
  # track; the conditional transport test is then anti-conservative and over-flags.
  # `auto` therefore reserves the exact Poisson null for non-negative counts that
  # show no overdispersion, and falls back to the dispersion-corrected robust null
  # for signed increments or whenever overdispersion is detected.
  is_signed  <- identical(get_data_type(x), "count-cumulative")
  null_used  <- if (!identical(null_model, "auto")) {
    null_model
  } else if (is_signed || .batch_dispersion(registration) > 1.5) {
    "robust"
  } else {
    "poisson"
  }
  registration <- .batch_add_p_values(registration, null_used)

  # -- 6. flag after Benjamini-Hochberg correction -----------------------------
  # `.batch_classify()` also builds the raw per-point `classification`, but that is
  # the non-multiplicity-corrected quadrant label and it over-identifies; the
  # trustworthy verdict is the BH-adjusted `batch` flag, so we keep only that.
  registration <- .batch_classify(registration, alpha)
  registration <- dplyr::select(
    registration,
    "report_date", "stratum", "reported", "baseline", "deficit", "delta",
    "p_transport", "p_transport_bh", "batch"
  )

  structure(
    dplyr::as_tibble(registration),
    class      = c("diagnose_batches", class(dplyr::tibble())),
    lookback   = lookback,
    period     = period,
    null_model = null_used,
    alpha      = alpha
  )
}

#' Reporting totals, baseline and the window statistics, shared by
#' `diagnose_batches()` and `transport_discriminant()`.
#'
#' Runs steps 1-4 of the pipeline: reduce to signed counts, lay them on the
#' complete report-date grid, fit the robust (optionally calendar-adjusted)
#' baseline, and attach the window total, its null mean, the deficit `W` and the
#' discriminant `Delta` on a leave-window-out baseline.
#' @keywords internal
#' @noRd
.batch_registration <- function(data, lookback, baseline_window, period,
                               axis = "report") {
  increments      <- .batch_report_increments(data, axis = axis)
  registration    <- .batch_registration_totals(increments, data)
  baseline_window <- .batch_baseline_window(baseline_window, lookback, period)
  registration    <- .batch_add_baseline(registration, baseline_window, period)
  .batch_add_window_statistics(registration, lookback, baseline_window, period)
}

# =============================================================================
# Step 1: one signed count per (event date, report date, stratum)
# =============================================================================

#' Resolve the validation column for a batch scan
#'
#' @param x A `tbl_now`.
#'
#' @return The validation-date column name.
#'
#' @keywords internal
#' @noRd
.batch_validation_axis <- function(x) {
  validation_col <- get_validation_date(x)
  if (is.null(validation_col)) {
    cli::cli_abort(c(
      "{.code axis = \"validation\"} needs a validation process, and
       {.arg x} has none.",
      "i" = "Attach one with {.fn add_validation_date}."
    ))
  }
  validation_col
}

#' Reduce a `tbl_now` to signed counts indexed by (event, report, stratum).
#'
#' Handles the three `tbl.now` data types uniformly:
#' * `"linelist"` -- one item per row, so the count is `1`;
#' * `"count-incidence"` -- the case-count column *is* the count;
#' * `"count-cumulative"` -- the case-count column is a running total, so it is
#'   differenced within each (event, stratum) into signed increments.  The
#'   increment attached to a report date is the change the report announced,
#'   which may be negative when a report revises a total downward.
#'
#' @returns A data frame with `.event_date`, `.report_date`, `.delay`, `.count`,
#'   `.stratum`.
#' @keywords internal
#' @noRd
.batch_report_increments <- function(data, axis = c("report", "validation")) {
  axis <- match.arg(axis)
  observations <- as.data.frame(data)
  event_col    <- get_event_date(data)
  data_type    <- get_data_type(data)
  strata_cols  <- get_strata(data)
  case_count_col <- get_case_count(data)

  # The whole batch machinery asks one question: "did an unusual number of
  # records arrive on this date?". That question is identical on the
  # CONFIRMATION axis -- a laboratory clearing its backlog looks exactly like a
  # surveillance system clearing its inbox -- so the axis is swapped here, at
  # the single point every batch function and every reporting-process plot goes
  # through, rather than duplicating any of them.
  report_col <- if (identical(axis, "validation")) {
    .batch_validation_axis(data)
  } else {
    get_report_date(data)
  }

  if (identical(axis, "validation")) {
    # A pending case has not been confirmed, so it contributes nothing to the
    # validation axis -- counting it on a date it does not have would invent
    # arrivals.
    observations <- observations[!is.na(observations[[report_col]]), , drop = FALSE]
    if (nrow(observations) == 0) {
      cli::cli_abort(c(
        "No confirmed records: every row is still {.val pending}.",
        "i" = "There is nothing to look for batches in on the validation axis."
      ))
    }
  }

  # The per-row count, before any de-accumulation.
  if (identical(data_type, "linelist")) {
    observations <- dplyr::mutate(observations, .count = 1)
  } else {
    if (is.null(case_count_col) || !case_count_col %in% names(observations)) {
      cli::cli_abort(
        "Aggregated data must carry a case-count column; none was found on the {.cls tbl_now}."
      )
    }
    observations <- dplyr::mutate(
      observations,
      .count = as.numeric(!!as.symbol(case_count_col))
    )
  }

  observations <- dplyr::mutate(
    observations,
    .event_date  = !!as.symbol(event_col),
    .report_date = !!as.symbol(report_col),
    .stratum     = .batch_stratum_label(observations, strata_cols)
  )

  # Reports can never precede the event they describe.
  observations <- dplyr::filter(observations, .data$.report_date >= .data$.event_date)
  if (nrow(observations) == 0L) {
    cli::cli_abort("No observations remain after dropping reports that precede their event.")
  }

  # Cumulative streams must be differenced into the increment each report added.
  if (identical(data_type, "count-cumulative")) {
    observations <- .batch_deaccumulate(observations)
  } else {
    observations <- observations |>
      dplyr::group_by(.data$.event_date, .data$.report_date, .data$.stratum) |>
      dplyr::summarise(.count = sum(.data$.count), .groups = "drop")
  }

  # Integer delay, measured in report-grid steps (unit-agnostic).
  date_grid <- .batch_date_grid(observations, data, axis = axis)
  observations |>
    dplyr::mutate(
      .delay = match(.data$.report_date, date_grid) - match(.data$.event_date, date_grid)
    ) |>
    dplyr::filter(!is.na(.data$.delay), .data$.delay >= 0L) |>
    dplyr::arrange(.data$.stratum, .data$.report_date, .data$.event_date)
}

#' Difference each origin's cumulative curve into the increment each report added.
#'
#' Within one (event date, stratum) the cumulative total is observed at a series
#' of report dates.  Later reports may revise the total *down*, so the increments
#' are signed.  When two rows share a report date we keep the last (the report
#' date's final word).
#' @keywords internal
#' @noRd
.batch_deaccumulate <- function(observations) {
  # Last word per (event, stratum, report date), then difference along report date.
  latest_per_report <- observations |>
    dplyr::group_by(.data$.event_date, .data$.stratum, .data$.report_date) |>
    dplyr::summarise(.cumulative = dplyr::last(.data$.count), .groups = "drop")

  origin_groups <- split(
    latest_per_report,
    interaction(latest_per_report$.event_date, latest_per_report$.stratum,
                drop = TRUE, lex.order = TRUE)
  )

  # An explicit loop: each origin needs ordering, then differencing -- two steps
  # with a carried value, which reads more clearly than a grouped pipeline.
  increment_frames <- vector("list", length(origin_groups))
  for (group_index in seq_along(origin_groups)) {
    origin_rows <- origin_groups[[group_index]]
    if (nrow(origin_rows) == 0L) next

    origin_rows <- origin_rows[order(origin_rows$.report_date), , drop = FALSE]
    cumulative_curve  <- as.numeric(origin_rows$.cumulative)
    signed_increments <- c(cumulative_curve[1], diff(cumulative_curve))

    increment_frames[[group_index]] <- data.frame(
      .event_date  = origin_rows$.event_date,
      .report_date = origin_rows$.report_date,
      .stratum     = origin_rows$.stratum,
      .count       = signed_increments,
      row.names    = NULL,
      check.names  = FALSE
    )
  }

  dplyr::bind_rows(increment_frames)
}

#' A single character label per stratum cell (or `"all"` when unstratified).
#' @keywords internal
#' @noRd
.batch_stratum_label <- function(observations, strata_cols) {
  if (length(strata_cols) == 0L) {
    return(rep("all", nrow(observations)))
  }
  stratum_values <- lapply(strata_cols, function(stratum_col) {
    values <- as.character(observations[[stratum_col]])
    values[is.na(values) | values == ""] <- "missing"
    values
  })
  do.call(paste, c(stratum_values, sep = "|"))
}

#' The complete calendar grid spanned by the events and reports, in report units.
#' @keywords internal
#' @noRd
.batch_date_grid <- function(observations, data, axis = "report") {
  report_unit <- if (identical(axis, "validation")) {
    get_validation_units(data) %||% get_report_units(data) %||% "days"
  } else {
    get_report_units(data) %||% "days"
  }
  grid_start  <- min(observations$.event_date,  na.rm = TRUE)
  grid_end    <- max(observations$.report_date, na.rm = TRUE)
  .tbl_now_date_seq(grid_start, grid_end, report_unit)
}

# =============================================================================
# Step 2: totals on the complete report-date grid
# =============================================================================

#' Sum the signed counts onto every (report date, stratum) of the complete grid.
#'
#' Missing report dates are filled with zero: an absent report date is a real
#' observation of "nothing was reported", which is precisely the lull a batch
#' leaves in its wake.
#' @keywords internal
#' @noRd
.batch_registration_totals <- function(increments, data) {
  report_unit <- get_report_units(data) %||% "days"
  report_grid <- seq(
    from = min(increments$.report_date, na.rm = TRUE),
    to   = max(increments$.report_date, na.rm = TRUE),
    by   = as.character(report_unit)
  )

  observed_totals <- increments |>
    dplyr::group_by(.data$.stratum, .data$.report_date) |>
    dplyr::summarise(reported = sum(.data$.count), .groups = "drop")

  complete_grid <- expand.grid(
    report_date = report_grid,
    stratum     = sort(unique(increments$.stratum)),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  complete_grid |>
    dplyr::left_join(
      observed_totals,
      by = c("report_date" = ".report_date", "stratum" = ".stratum")
    ) |>
    dplyr::mutate(reported = dplyr::coalesce(.data$reported, 0)) |>
    dplyr::arrange(.data$stratum, .data$report_date)
}

# =============================================================================
# Step 3: the robust baseline
# =============================================================================

#' Choose an admissible running-median width.
#'
#' A batch episode corrupts `lookback + 1` consecutive report dates (the deficits
#' plus the spike).  A running median of width `w` is exactly unbiased at every
#' clean date provided fewer than half of its window is corrupted, i.e.
#' `w >= 2 * lookback + 3`.  With a scheduled cycle of length `period`, the width
#' is rounded up to an odd multiple of `period` so that every phase of the cycle
#' is represented equally inside the window.
#' @keywords internal
#' @noRd
.batch_baseline_window <- function(baseline_window, lookback, period) {
  minimum_width <- 2L * lookback + 3L

  if (is.null(baseline_window)) {
    baseline_window <- minimum_width
    if (!is.null(period) && period > 1L) {
      # Smallest multiple of `period` that reaches `minimum_width`, made odd.
      cycles          <- ceiling(minimum_width / period)
      baseline_window <- as.integer(cycles * period)
      if (baseline_window %% 2L == 0L) baseline_window <- baseline_window + 1L
    }
  }

  baseline_window <- as.integer(baseline_window)
  if (baseline_window %% 2L == 0L) {
    cli::cli_abort("`baseline_window` must be odd. Got {baseline_window}.")
  }
  if (baseline_window < minimum_width) {
    cli::cli_abort(c(
      "`baseline_window` is too narrow for `lookback = {lookback}`.",
      "x" = "A batch episode would outvote the median it is supposed to be measured against.",
      "i" = "Need `baseline_window >= 2 * lookback + 3 = {minimum_width}`."
    ))
  }
  baseline_window
}

#' Attach the first-pass robust baseline, optionally corrected for a fixed schedule.
#'
#' This baseline smooths the *whole* series, so at a batch episode it is dragged by
#' the episode itself.  It is therefore used only for the two quantities that need
#' a global summary and tolerate a little contamination -- the calendar phase
#' factors and the dispersion -- never for `Delta`.
#' @keywords internal
#' @noRd
.batch_add_baseline <- function(registration, baseline_window, period) {
  stratum_levels    <- sort(unique(registration$stratum))
  smooth_values     <- rep(NA_real_, nrow(registration))   # schedule-blind
  adjusted_values   <- rep(NA_real_, nrow(registration))   # schedule-corrected

  for (stratum_value in stratum_levels) {
    stratum_rows <- which(registration$stratum == stratum_value)
    reported     <- registration$reported[stratum_rows]

    # The schedule-blind smooth is what the phase factors must be estimated from:
    # taking the ratio of the data to an already-corrected baseline would show no
    # cycle at all.
    stratum_smooth <- .batch_repeated_median(reported, baseline_window)
    smooth_values[stratum_rows] <- stratum_smooth

    adjusted_values[stratum_rows] <- if (!is.null(period) && period > 1L) {
      .batch_apply_phase_factors(reported = reported, baseline = stratum_smooth,
                                 period = period)
    } else {
      stratum_smooth
    }
  }

  dplyr::mutate(
    registration,
    baseline_smooth = smooth_values,
    baseline_global = adjusted_values
  )
}

#' Per-phase multiplicative factors of a scheduled reporting cycle.
#'
#' Medians across cycles, so an irregular batch cannot redefine the schedule.
#' Renormalised to geometric mean one, so only the *shape* of the cycle is carried.
#' @keywords internal
#' @noRd
.batch_phase_factors <- function(reported, baseline, period) {
  positive_baseline <- pmax(abs(baseline), .Machine$double.eps)
  observed_ratio    <- reported / positive_baseline
  phase_index       <- (seq_along(reported) - 1L) %% period

  phase_factors <- rep(1, period)
  for (phase in seq_len(period) - 1L) {
    # Keep the zeros!  A phase on which the desk is always shut has every ratio
    # equal to zero; dropping them would leave the phase with no observations and
    # a default factor of one, i.e. "expect normal reporting on a Sunday" -- which
    # then manufactures a deficit, and a phantom batch every Monday.
    phase_ratios <- observed_ratio[phase_index == phase]
    phase_ratios <- phase_ratios[is.finite(phase_ratios)]
    if (length(phase_ratios) > 0L) {
      phase_factors[phase + 1L] <- stats::median(phase_ratios)
    }
  }

  # Renormalise on the positive factors only (a structural zero has no logarithm),
  # then floor so that a closed phase still yields a usable, strictly positive mean.
  phase_factors  <- pmax(phase_factors, 0)
  usable_factors <- phase_factors[phase_factors > 0]
  if (length(usable_factors) > 0L) {
    phase_factors <- phase_factors / exp(mean(log(usable_factors)))
  }
  pmax(phase_factors, 1e-8)
}

#' Running median with median end-rule; falls back to the global median when the
#' series is shorter than the window.
#'
#' Fits a local *constant*, so it is biased wherever the series trends. No longer
#' used by the baseline (the repeated median dominates it -- see
#' `.batch_repeated_median`); retained only as the reference the comparison test
#' measures the repeated median against.
#' @keywords internal
#' @noRd
.batch_running_median <- function(reported, baseline_window) {
  if (length(reported) < baseline_window) {
    return(rep(stats::median(reported, na.rm = TRUE), length(reported)))
  }
  as.numeric(stats::runmed(reported, k = baseline_window, endrule = "median"))
}

#' Repeated-median (Siegel) local-linear robust baseline.
#'
#' Within each window, take the median of the pairwise slopes through each point,
#' then the median of those medians; the intercept is the median of the residuals.
#' A local *line*, fitted with a 50% breakdown point.
#'
#' *Why not just a running median?*  On a **monotone** series a running median is
#' already exact (the median is order-preserving, so the median of the window's
#' values is the value at the window's centre).  Its weakness is elsewhere: at
#' turning points, and -- decisively here -- when an **asymmetric patch of
#' outliers** sits inside the smoothing window.  A batch episode is exactly such a
#' patch: several deficits *and* one large spike, pulling the local order
#' statistics in opposite directions.  Estimating the local slope explicitly is
#' what lets the fit survive it.
#'
#' Measured on real data: with a planted release on FluSight, the residual at the
#' release date falls sharply when the running median is replaced by the repeated
#' median, and clean-data false positives fall to zero on both FluSight and dengue.
#' @keywords internal
#' @noRd
.batch_repeated_median <- function(reported, baseline_window) {
  n_dates   <- length(reported)
  half_width <- (baseline_window - 1L) %/% 2L
  baseline  <- numeric(n_dates)

  for (centre in seq_len(n_dates)) {
    window_positions <- max(1L, centre - half_width):min(n_dates, centre + half_width)
    window_values    <- reported[window_positions]
    window_index     <- as.numeric(window_positions)

    if (length(window_positions) < 3L) {
      baseline[centre] <- stats::median(window_values)
      next
    }

    # Slope at each point: median of the slopes of the lines through it.
    point_slopes <- numeric(length(window_positions))
    for (point_index in seq_along(window_positions)) {
      pairwise_slopes <- (window_values[point_index] - window_values[-point_index]) /
        (window_index[point_index] - window_index[-point_index])
      point_slopes[point_index] <- stats::median(pairwise_slopes, na.rm = TRUE)
    }

    window_slope     <- stats::median(point_slopes, na.rm = TRUE)
    window_intercept <- stats::median(window_values - window_slope * window_index, na.rm = TRUE)
    baseline[centre] <- window_intercept + window_slope * centre
  }

  baseline
}

#' Multiply the baseline by a per-phase factor estimated by medians across cycles.
#' @keywords internal
#' @noRd
.batch_apply_phase_factors <- function(reported, baseline, period) {
  phase_factors <- .batch_phase_factors(reported, baseline, period)
  phase_index   <- (seq_along(reported) - 1L) %% period
  pmax(abs(baseline), .Machine$double.eps) * phase_factors[phase_index + 1L]
}

#' Robust local line (repeated median) through `(x, y)`; returns intercept and slope.
#' @keywords internal
#' @noRd
.batch_repeated_median_line <- function(x_values, y_values) {
  n_points <- length(x_values)
  if (n_points < 3L) {
    return(c(intercept = stats::median(y_values, na.rm = TRUE), slope = 0))
  }

  point_slopes <- numeric(n_points)
  for (point_index in seq_len(n_points)) {
    pairwise_slopes <- (y_values[point_index] - y_values[-point_index]) /
      (x_values[point_index] - x_values[-point_index])
    point_slopes[point_index] <- stats::median(pairwise_slopes, na.rm = TRUE)
  }

  slope     <- stats::median(point_slopes, na.rm = TRUE)
  intercept <- stats::median(y_values - slope * x_values, na.rm = TRUE)
  c(intercept = intercept, slope = slope)
}

#' Predict the baseline over a candidate window from dates OUTSIDE that window.
#'
#' This is the model-free analogue of leaving an observation out before judging it.
#' `Delta = S - M` is an exact pivot when `M` is the *true* window mean.  If instead
#' `M` is smoothed from a series that contains the very episode under test, the
#' deficits drag the smoother down, `M` is biased low, and `Delta` acquires a
#' spurious positive mean -- the batch masks itself as a surge.
#'
#' So for each candidate window we refit the local line to the nearest reference
#' dates lying strictly *outside* the window and extrapolate it across the window.
#' The reference dates cannot contain the episode (the window does), so the
#' baseline is clean by construction.
#' @param deseasonalised The report series with any calendar cycle divided out.
#' @param window_positions Integer positions of the candidate window.
#' @param reference_half_width How far either side of the window to look.
#' @keywords internal
#' @noRd
.batch_leave_window_out_baseline <- function(deseasonalised, window_positions,
                                             reference_half_width) {
  n_dates <- length(deseasonalised)
  window_start <- min(window_positions)
  window_end   <- max(window_positions)

  reference_positions <- c(
    seq.int(from = max(1L, window_start - reference_half_width), to = window_start - 1L),
    seq.int(from = window_end + 1L, to = min(n_dates, window_end + reference_half_width))
  )
  reference_positions <- reference_positions[
    reference_positions >= 1L & reference_positions <= n_dates &
      !(reference_positions %in% window_positions)
  ]
  if (length(reference_positions) < 3L) return(rep(NA_real_, length(window_positions)))

  local_line <- .batch_repeated_median_line(
    x_values = as.numeric(reference_positions),
    y_values = deseasonalised[reference_positions]
  )
  predicted <- local_line[["intercept"]] + local_line[["slope"]] * as.numeric(window_positions)
  pmax(predicted, 0)
}

# =============================================================================
# Step 4: the window statistics
# =============================================================================

#' Attach the window total `S`, its null mean `M`, the deficit `W` and `Delta`.
#'
#' `Delta = S - M` is algebraically identical to
#' `(reported - baseline) - deficit`; the window-total form is used because it is
#' the one with the exact null. Rows whose window runs off the start of the series
#' get `NA`.
#' @keywords internal
#' @noRd
.batch_add_window_statistics <- function(registration, lookback, baseline_window,
                                         period) {
  stratum_levels  <- sort(unique(registration$stratum))
  n_rows          <- nrow(registration)
  baseline_at_r   <- rep(NA_real_, n_rows)
  window_total    <- rep(NA_real_, n_rows)
  window_mean     <- rep(NA_real_, n_rows)
  window_deficit  <- rep(NA_real_, n_rows)
  window_scale    <- rep(NA_real_, n_rows)
  deficit_scale   <- rep(NA_real_, n_rows)

  reference_half_width <- (baseline_window - 1L) %/% 2L + lookback + 1L

  for (stratum_value in stratum_levels) {
    stratum_rows <- which(registration$stratum == stratum_value)
    reported     <- registration$reported[stratum_rows]
    n_dates      <- length(reported)

    # Divide out any scheduled cycle before smoothing, and multiply it back after.
    if (!is.null(period) && period > 1L) {
      phase_factors <- .batch_phase_factors(
        reported, registration$baseline_smooth[stratum_rows], period
      )
      phase_index <- (seq_len(n_dates) - 1L) %% period
      seasonal    <- phase_factors[phase_index + 1L]
    } else {
      seasonal <- rep(1, n_dates)
    }
    deseasonalised <- reported / seasonal

    for (position in seq_len(n_dates)) {
      if (position <= lookback) next          # window would run off the start
      window_positions   <- (position - lookback):position
      previous_positions <- (position - lookback):(position - 1L)

      # The baseline for this window is refitted from dates OUTSIDE it, so the
      # episode we are testing cannot bias the yardstick we test it against.
      window_baseline <- .batch_leave_window_out_baseline(
        deseasonalised, window_positions, reference_half_width
      ) * seasonal[window_positions]
      if (anyNA(window_baseline)) next

      previous_baseline <- window_baseline[seq_along(previous_positions)]
      # Variance grows with the mean for counting data; floor at 1 so an empty
      # date still contributes noise.
      variance_units <- pmax(abs(window_baseline), 1)

      target_row <- stratum_rows[position]
      baseline_at_r[target_row]  <- window_baseline[length(window_baseline)]
      window_total[target_row]   <- sum(reported[window_positions])
      window_mean[target_row]    <- sum(window_baseline)
      window_deficit[target_row] <- sum(previous_baseline - reported[previous_positions])
      window_scale[target_row]   <- sum(variance_units)
      deficit_scale[target_row]  <- sum(variance_units[seq_along(previous_positions)])
    }
  }

  dplyr::mutate(
    registration,
    baseline      = baseline_at_r,
    window_total  = window_total,
    window_mean   = window_mean,
    deficit       = window_deficit,
    delta         = window_total - window_mean,
    window_scale  = window_scale,
    deficit_scale = deficit_scale
  )
}

# =============================================================================
# Step 5: p-values
# =============================================================================

#' One-sided p-values for creation, deletion and transport.
#'
#' **Poisson null (exact).**  The window total is exactly `Poisson(M)` under any
#' transport confined to the window, so `p_creation` and `p_deletion` are exact
#' Poisson tails.  Conditioning on the window total removes the unknown intensity
#' entirely, and the allocation of those reports across the window's dates is
#' multinomial; a batch loads the *last* date, so the transport test is the upper
#' tail of `Binomial(S, baseline_r / M)`.  Both tests are exactly `alpha`-sized
#' whatever the true intensity is.
#'
#' **Robust null (approximate).**  For signed increments the window total is a
#' difference of counting processes rather than a count.  The conservation logic
#' survives, but the reference law does not, so we standardise by a robust
#' (median absolute deviation) estimate of the residual scale and read the tails
#' off a normal distribution.
#' @keywords internal
#' @noRd
.batch_add_p_values <- function(registration, null_used) {
  if (identical(null_used, "poisson")) {
    return(.batch_p_values_poisson(registration))
  }
  .batch_p_values_robust(registration)
}

#' @keywords internal
#' @noRd
.batch_p_values_poisson <- function(registration) {
  window_total <- registration$window_total
  window_mean  <- registration$window_mean
  spike_share  <- registration$baseline / window_mean

  # Tails on the window total: creation inflates it, deletion depletes it.
  #
  # The theory gives `S ~ Poisson(M)` exactly -- but with `M` *known*.  Our `M` is
  # a robust estimate from the same series, so `Delta = S - M_hat` carries the
  # baseline's error on top of the Poisson noise, and an exact Poisson tail would
  # be anti-conservative (it would call an ordinary window a surge).  We therefore
  # correct the creation test by a robustly-estimated dispersion.
  #
  # The *transport* test below needs no such repair: conditioning on the window
  # total removes the level entirely, so it stays exact.  That asymmetry is the
  # practical face of the three-layer conditioning argument.
  dispersion <- .batch_dispersion(registration)

  if (dispersion <= 1) {
    p_creation <- stats::ppois(window_total - 1, lambda = window_mean, lower.tail = FALSE)
    p_deletion <- stats::ppois(window_total,     lambda = window_mean, lower.tail = TRUE)
  } else {
    # Quasi-Poisson: mean `M`, variance `dispersion * M`, realised as a negative
    # binomial with matching first two moments.
    negbin_size <- window_mean / (dispersion - 1)
    p_creation  <- stats::pnbinom(window_total - 1, size = negbin_size,
                                  mu = window_mean, lower.tail = FALSE)
    p_deletion  <- stats::pnbinom(window_total,     size = negbin_size,
                                  mu = window_mean, lower.tail = TRUE)
  }

  # Conditional (exact) allocation test: how much of the window landed on date r?
  spike_share <- pmin(pmax(spike_share, .Machine$double.eps), 1 - .Machine$double.eps)
  p_transport <- stats::pbinom(
    q    = registration$reported - 1,
    size = pmax(round(window_total), 0),
    prob = spike_share,
    lower.tail = FALSE
  )
  # An empty window carries no allocation information.
  p_transport[!is.finite(window_total) | window_total <= 0] <- NA_real_

  dplyr::mutate(
    registration,
    p_creation  = .batch_clamp_p(p_creation),
    p_deletion  = .batch_clamp_p(p_deletion),
    p_transport = .batch_clamp_p(p_transport)
  )
}

#' @keywords internal
#' @noRd
.batch_p_values_robust <- function(registration) {
  # Quasi-Poisson dispersion: the variance of a date's total scales with its mean,
  # so standardise the residuals by sqrt(|baseline|) *before* estimating a single
  # dispersion. A pooled constant variance would call every point of a rising
  # epidemic curve a surge.
  dispersion <- .batch_dispersion(registration)

  delta_z   <- registration$delta   / sqrt(dispersion * registration$window_scale)
  deficit_z <- registration$deficit / sqrt(dispersion * registration$deficit_scale)

  dplyr::mutate(
    registration,
    p_creation  = .batch_clamp_p(stats::pnorm(delta_z,   lower.tail = FALSE)),
    p_deletion  = .batch_clamp_p(stats::pnorm(delta_z,   lower.tail = TRUE)),
    p_transport = .batch_clamp_p(stats::pnorm(deficit_z, lower.tail = FALSE))
  )
}

#' Robust quasi-Poisson dispersion from mean-scaled (Pearson) residuals.
#'
#' `phi` such that `Var(R_j) ~ phi * max(|baseline_j|, 1)`.  A median absolute
#' deviation of the Pearson residuals keeps the batch episode itself from
#' inflating the dispersion it is meant to be judged against.
#' @keywords internal
#' @noRd
.batch_dispersion <- function(registration) {
  # Uses the first-pass (whole-series) baseline: it is defined everywhere, and a
  # robust MAD keeps the episode from inflating the dispersion it is judged against.
  variance_units    <- pmax(abs(registration$baseline_global), 1)
  pearson_residuals <- (registration$reported - registration$baseline_global) /
    sqrt(variance_units)

  dispersion <- stats::mad(pearson_residuals, na.rm = TRUE)^2
  if (!is.finite(dispersion) || dispersion <= 0) {
    dispersion <- stats::var(pearson_residuals, na.rm = TRUE)
  }
  if (!is.finite(dispersion) || dispersion <= 0) {
    cli::cli_abort("Cannot estimate a dispersion: the report series is constant.")
  }
  dispersion
}

#' Keep p-values inside `[0, 1]` and propagate NA.
#' @keywords internal
#' @noRd
.batch_clamp_p <- function(p_value) {
  pmin(pmax(p_value, 0), 1)
}

# =============================================================================
# Step 6: classification and multiplicity
# =============================================================================

#' Classify each (report date, stratum) into the transport/creation quadrants and
#' flag batches after a Benjamini-Hochberg correction across all rows.
#' @keywords internal
#' @noRd
.batch_classify <- function(registration, alpha) {
  p_transport_adjusted <- rep(NA_real_, nrow(registration))
  testable <- which(is.finite(registration$p_transport))
  if (length(testable) > 0L) {
    p_transport_adjusted[testable] <-
      stats::p.adjust(registration$p_transport[testable], method = "BH")
  }

  transport_signal <- !is.na(registration$p_transport) & registration$p_transport < alpha
  creation_signal  <- !is.na(registration$p_creation)  & registration$p_creation  < alpha
  deletion_signal  <- !is.na(registration$p_deletion)  & registration$p_deletion  < alpha

  classification <- rep("none", nrow(registration))
  classification[transport_signal & !creation_signal] <- "batch"
  classification[creation_signal & !transport_signal] <- "surge"
  classification[transport_signal & creation_signal]  <- "batch_and_surge"
  # A significantly *depleted* window means the hold has not released yet (or the
  # release fell past `now`); this dominates, since no spike has arrived.
  classification[deletion_signal] <- "hold_or_deletion"
  classification[is.na(registration$delta)] <- NA_character_

  dplyr::mutate(
    registration,
    p_transport_bh = p_transport_adjusted,
    classification = classification,
    batch          = !is.na(p_transport_adjusted) & p_transport_adjusted < alpha &
                     !deletion_signal
  )
}

# =============================================================================
# Utilities
# =============================================================================

#' Fill in (or sanity-check) the calendar `period` from the object's temporal
#' effects.
#'
#' A scheduled reporting cycle -- a desk that is shut at weekends, say -- produces
#' every batch symptom, every cycle, so `diagnose_batches()` needs to know the cycle
#' length. When the user does not pass `period`, we read it off the `tbl_now`'s
#' temporal-effect specs: a **day-of-week** effect implies a weekly cycle
#' (`period = 7`), a **week-of-year** effect a yearly one (`period = 52`). A
#' user-supplied `period` always wins, but we note when it disagrees with the
#' object's effects. If the data is daily and carries no temporal effect at all,
#' we suggest `period = 7`, since daily reporting usually has a weekly rhythm.
#' @keywords internal
#' @noRd
.batch_resolve_period <- function(data, period) {
  specs  <- get_temporal_effects(data) %||% list()
  # Each spec is `list(t_effects = <temporal_effects>, date_type, ...)`.
  te_has <- function(prop) any(vapply(specs, function(s) {
    te <- tryCatch(s$t_effects, error = function(e) NULL)
    if (is.null(te)) return(FALSE)
    isTRUE(tryCatch(S7::prop(te, prop), error = function(e) FALSE))
  }, logical(1)))
  has_dow <- te_has("day_of_week")
  has_woy <- te_has("week_of_year")
  implied      <- if (has_dow) 7L else if (has_woy) 52L else NULL
  implied_from <- if (has_dow) "day-of-week" else if (has_woy) "week-of-year" else NULL
  report_unit  <- get_report_units(data) %||% "days"

  if (is.null(period)) {
    if (!is.null(implied)) {
      cli::cli_inform(c(
        "i" = "Using {.code period = {implied}} from the object's {implied_from} temporal effect."
      ))
      return(implied)
    }
    if (identical(report_unit, "days")) {
      cli::cli_inform(c(
        "i" = "Your data is daily and carries no temporal effect. If reporting follows a \\
               weekly cycle, pass {.code period = 7} so a weekend lull is not read as a batch."
      ))
    }
    return(NULL)
  }

  if (!is.null(implied) && !isTRUE(period == implied)) {
    cli::cli_inform(c(
      "i" = "You passed {.code period = {period}}, but the object's {implied_from} temporal \\
             effect suggests {.code period = {implied}}."
    ))
  }
  period
}

#' Warn, on every call, that the batch-detection functions are experimental.
#'
#' The batch detectors are new and their statistical behaviour and interface are
#' still settling.  Every user-facing entry point (`diagnose_batches()`,
#' `diagnose_batch_shape()`, `simulate_batch()`) calls this so a user is always told
#' that a flagged batch is a *potential* batch, not a confirmed one.
#' @param function_name The calling function, for the message.
#' @keywords internal
#' @noRd
.batch_experimental_warning <- function(function_name) {
  cli::cli_warn(
    c(
      "!" = "{.fn {function_name}} is {.emph experimental}: results are not \\
             guaranteed and the interface may change.",
      "i" = "Treat a flagged report date as a {.emph potential} batch, not a \\
             confirmed one."
    ),
    # "regularly" (rlang throttles to roughly once per session per id) matches the
    # convention already used by the other experimental tbl.now diagnostics
    # (`diagnose_drift()`, `diagnose_changepoint()`), so a user is warned
    # without being buried when a function is called in a loop.
    .frequency    = "regularly",
    .frequency_id = paste0("tbl.now::", function_name)
  )
  invisible(NULL)
}

#' @keywords internal
#' @noRd
.batch_check_tbl_now <- function(x) {
  if (!is_tbl_now(x)) {
    cli::cli_abort(
      "{.arg x} must be a {.cls tbl_now} object. Got {.cls {class(x)}}."
    )
  }
  invisible(TRUE)
}

#' Print a batch screen
#' @param x A `diagnose_batches` object.
#' @param ... Unused.
#' @export
#' @noRd
print.diagnose_batches <- function(x, ...) {
  flagged <- x[!is.na(x$batch) & x$batch, , drop = FALSE]

  # stdout (`cat_*`), not messages (`cli_*`): print output must survive
  # `message = FALSE`, `sink()` and `capture.output()`.
  cli::cat_rule(left = "Batch screen")
  cli::cat_line(cli::format_inline(paste0(
    "{nrow(x)} (report date, stratum) pair{?s}; ",
    "look-back {attr(x, 'lookback')}; null {.val {attr(x, 'null_model')}}",
    "{if (!is.null(attr(x, 'period'))) paste0('; calendar period ', attr(x, 'period')) else ''}"
  )))

  if (nrow(flagged) == 0L) {
    cli::cat_line(cli::format_inline(
      "{cli::symbol$tick} No batches flagged at alpha = {attr(x, 'alpha')} (BH-adjusted)."
    ))
  } else {
    cli::cat_line(cli::format_inline(paste0(
      "{cli::symbol$warning} {nrow(flagged)} batch{?/es} flagged at ",
      "alpha = {attr(x, 'alpha')} (BH-adjusted):"
    )))
    shown <- seq_len(min(nrow(flagged), 10L))
    cli::cat_bullet(vapply(shown, function(row_index) {
      cli::format_inline(paste0(
        "{format(flagged$report_date[row_index])} ",
        "[{flagged$stratum[row_index]}] -- ",
        "reported {round(flagged$reported[row_index])}, ",
        "baseline {round(flagged$baseline[row_index], 1)}, ",
        "deficit {round(flagged$deficit[row_index], 1)}, ",
        "delta {round(flagged$delta[row_index], 1)}"
      ))
    }, character(1)))
    if (nrow(flagged) > 10L) {
      cli::cat_line(cli::format_inline("... and {nrow(flagged) - 10L} more."))
    }
  }
  invisible(x)
}
