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
# window total.  This is what separates the two, and it is exact (see the
# "The mathematics" section of `?batch_screen`).
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
#' window spanning both is unchanged.  `batch_screen()` is completely
#' **model-free** -- it needs only a [tbl_now()], not a fitted model -- which
#' makes it the right tool for exploratory data analysis before any nowcasting
#' model is chosen.
#'
#' @details
#' # The mathematics
#'
#' Index each item by its **event date** \eqn{t} (when it happened) and its
#' **report date** \eqn{r} (when it was recorded); the reporting delay is
#' \eqn{d = r - t}.  Let \eqn{R_r} be the number of items reported on date \eqn{r}
#' and \eqn{\mu_r} its expected value under a stable reporting process.
#'
#' **A batch is a transport, not a creation.**  When a stalled desk releases a
#' backlog, it *relabels the report date* of items that already exist; it never
#' creates or destroys them, and it can only ever move an item *later*.  This has
#' an exact consequence.  Over a window \eqn{\mathcal{W} = \{r-k,\dots,r\}} that
#' contains both the lull and the spike, write the window total and its null mean
#'
#' \deqn{S = \sum_{j\in\mathcal{W}} R_j, \qquad M = \sum_{j\in\mathcal{W}} \mu_j.}
#'
#' The **transport discriminant** is their difference,
#'
#' \deqn{\Delta_r(k) = (R_r - \mu_r) - \underbrace{\textstyle\sum_{j<r}(\mu_j - R_j)}_{\text{deficit } W_r(k)} = S - M,}
#'
#' i.e. simply the window total, centred.  **Theorem.** Under *any* batch
#' mechanism whose displacements stay inside \eqn{\mathcal{W}} -- deterministic or
#' random, clearing the backlog in any order -- the count in the window is a
#' *pathwise* invariant, so \eqn{S \sim \mathrm{Poisson}(M)} exactly, with
#' \eqn{\mathbb{E}\,\Delta = 0}.  A genuine surge that *creates* items with mean
#' \eqn{\eta} instead gives \eqn{S \sim \mathrm{Poisson}(M + \eta)}, so
#' \eqn{\mathbb{E}\,\Delta = \eta}.  Hence \eqn{\Delta} sees only creation and the
#' deficit \eqn{W} sees only transport, and the pair separates a batch from a
#' surge:
#'
#' \tabular{lll}{
#'   \tab \eqn{W \approx 0} \tab \eqn{W \gg 0} \cr
#'   \eqn{\Delta \approx 0} \tab nothing \tab **batch** \cr
#'   \eqn{\Delta \gg 0} \tab **surge** \tab batch and surge \cr
#'   \eqn{\Delta < 0} \tab -- \tab **hold in progress / deletion** \cr
#' }
#'
#' **Estimating \eqn{M} without cheating.** The theorem is about the *true* mean.
#' If \eqn{M} were smoothed from a series containing the very episode under test,
#' the deficits would drag it down and \eqn{\Delta} would acquire a spurious
#' positive mean -- the batch would mask itself as a surge.  `batch_screen()`
#' therefore refits the baseline for each candidate window from report dates lying
#' strictly *outside* that window (the model-free analogue of leaving an
#' observation out).  Because the transport never crosses the window boundary,
#' \eqn{M} cannot see it, and \eqn{\Delta} is invariant to the batch pathwise.  The
#' baseline is a robust local line (Siegel's repeated median), which keeps a 50%
#' breakdown point against the episode while remaining unbiased under a trend.
#'
#' # What is computed
#'
#' For every report date `r` and stratum, with a look-back window
#' `W = {r - lookback, ..., r}`:
#'
#' \describe{
#'   \item{`reported`}{\eqn{R_r}, the number of items reported on date `r`.  For
#'     `"count-cumulative"` data these are the *signed increments* of the
#'     cumulative curve, so `reported` may be negative (a net down-revision).}
#'   \item{`baseline`}{\eqn{\hat\mu_r}, a robust running-median estimate of how
#'     many reports date `r` *should* have carried.}
#'   \item{`deficit`}{\eqn{W_r(k) = \sum_{j<r}(\hat\mu_j - R_j)}, the reports
#'     that went missing in the `lookback` dates before `r`.}
#'   \item{`delta`}{\eqn{\Delta_r(k) = S - M}, the window total minus its null
#'     mean.  Blind to any transport confined to the window; sensitive to
#'     creation.}
#'   \item{`p_transport`, `p_creation`, `p_deletion`}{One-sided \eqn{p}-values
#'     for the three directions.}
#'   \item{`classification`}{One of `"batch"`, `"surge"`, `"batch_and_surge"`,
#'     `"hold_or_deletion"`, `"none"`.}
#'   \item{`batch`}{Logical: `p_transport` survives a Benjamini-Hochberg
#'     correction across all (report date, stratum) pairs at level `alpha`.}
#' }
#'
#' # The null distribution
#'
#' For non-negative counts (`"linelist"` and `"count-incidence"` data):
#'
#' * *transport* (**exact**): conditionally on the window total \eqn{S}, the
#'   reports allocate across the window's dates multinomially, so the share
#'   landing on the final date is
#'   \eqn{R_r \mid S \sim \mathrm{Binomial}(S, \hat\mu_r / M)}.  A batch pushes
#'   reports to the *last* date, so we test the upper tail.  Conditioning on
#'   \eqn{S} removes the unknown intensity entirely, which is what makes this test
#'   exactly the right size whatever the truth happens to be -- **and what makes it
#'   insensitive to error in the baseline.**
#' * *creation* (**dispersion-corrected**): the theory gives
#'   \eqn{S \sim \mathrm{Poisson}(M)} exactly under any within-window batch, but
#'   with \eqn{M} *known*.  Here \eqn{M} is estimated from the same series, so
#'   \eqn{\Delta} carries the baseline's error as well as Poisson noise.  An exact
#'   Poisson tail would then be anti-conservative, so the creation test is widened
#'   by a robustly-estimated quasi-Poisson dispersion.
#'
#' The asymmetry is not an accident: it is the practical face of the conditioning
#' argument.  The batch verdict is the trustworthy one.
#'
#' For `"count-cumulative"` data the increments are signed, the window total is
#' a difference of two counting processes rather than a count, and the exact
#' Poisson/Binomial reference is replaced by a robust normal approximation
#' (`null_model = "robust"`).  The conservation logic is unchanged; only the
#' reference law degrades.
#'
#' # A caveat on the `"surge"` verdict
#'
#' The two channels are *not* equally trustworthy without a model.  The
#' **transport** verdict (`"batch"`) compares dates *within* one window and is
#' insensitive to the overall level -- in the exact Poisson path it literally
#' conditions on the window total -- so it is stable.  The **creation** verdict
#' (`"surge"`) compares the window total against the baseline, and is therefore
#' only as good as the baseline.  A robust local-linear baseline is unbiased under
#' a *linear* trend but not under a sharply *curved* one, so on a steeply growing
#' epidemic curve `"surge"` will fire on ordinary growth.  That is not a bug: mass
#' really is being created.  It does mean that if you want to detect genuine
#' surges you should fit a model rather than rely on this screen.
#'
#' # Calendar effects
#'
#' A reporting system that is always closed at weekends produces every batch
#' symptom, every week.  Pass `period` (e.g. `period = 7` for daily data) and
#' the baseline is corrected by per-phase medians taken across cycles, which
#' recovers the schedule exactly provided each phase is hit by *irregular*
#' batches in fewer than half of its cycles.  An irregular batch is then an
#' excursion *relative to the schedule*.
#'
#' @param data A [tbl_now()] object of any `data_type`.
#' @param lookback Integer `k`: how many report dates before `r` the window
#'   reaches back.  Should comfortably cover the longest plausible stall.
#'   Default `3`.
#' @param baseline_window Odd integer width of the running median used to
#'   estimate the baseline.  Must satisfy `baseline_window >= 2 * lookback + 3`
#'   so that a clean date is never outvoted by a batch episode.  Defaults to the
#'   smallest admissible odd value (adjusted upward to a multiple of `period`
#'   plus one when `period` is supplied).
#' @param baseline_method How the baseline is smoothed.  `"repeated_median"`
#'   (default) fits a robust local *line* (Siegel's repeated median), which keeps
#'   the 50% breakdown that makes a median immune to the batch episode while
#'   remaining unbiased when the report series trends -- essential on an epidemic
#'   curve, where a local *constant* fit would call every rising date a surge.
#'   `"running_median"` fits a local constant; use it for a flat series.
#' @param period Optional integer cycle length of a *scheduled* reporting
#'   pattern (e.g. `7` for a weekly cycle on daily data).  `NULL` (default)
#'   disables the calendar correction.
#' @param null_model `"auto"` (default) uses the exact Poisson/Binomial null for
#'   non-negative counts and the robust normal approximation for signed
#'   (count-cumulative) increments.  `"poisson"` and `"robust"` force the choice.
#' @param alpha Significance level for the Benjamini-Hochberg flag and for the
#'   `classification` column.  Default `0.05`.
#'
#' @returns A tibble of class `batch_screen`, one row per (report date, stratum),
#'   with the columns described under **Details**.  Has a `print()` method that
#'   summarises the flagged dates.
#'
#' @seealso [batch_shape_test()] for the complementary test on *which* event
#'   dates a report date drew from, and [simulate_batch()] to inject a known
#'   batch for validation.
#'
#' @examples
#' library(tbl.now)
#' data(denguedat, package = "tbl.now")
#'
#' dengue_tbl <- tbl_now(
#'   denguedat,
#'   event_date  = onset_week,
#'   report_date = report_week,
#'   data_type   = "linelist",
#'   verbose     = FALSE
#' )
#'
#' screened <- batch_screen(dengue_tbl, lookback = 2)
#' head(screened)
#'
#' @export
batch_screen <- function(data,
                         lookback        = 3L,
                         baseline_window = NULL,
                         baseline_method = c("repeated_median", "running_median"),
                         period          = NULL,
                         null_model      = c("auto", "poisson", "robust"),
                         alpha           = 0.05) {
  null_model      <- match.arg(null_model)
  baseline_method <- match.arg(baseline_method)
  .batch_experimental_warning("batch_screen")
  .batch_check_tbl_now(data)

  lookback <- as.integer(lookback)
  if (lookback < 1L) {
    cli::cli_abort("`lookback` must be a positive integer. Got {lookback}.")
  }
  if (alpha <= 0 || alpha >= 1) {
    cli::cli_abort("`alpha` must lie strictly between 0 and 1. Got {alpha}.")
  }

  # -- 1. reduce the tbl_now to one signed count per (event, report, stratum) --
  increments <- .batch_report_increments(data)

  # -- 2. lay those counts on the complete report-date grid, per stratum -------
  # A report date with no reports at all is a genuine zero -- that is exactly the
  # lull a batch leaves behind -- so the grid must be completed, not dropped.
  registration <- .batch_registration_totals(increments, data)

  # -- 3. robust baseline, optionally calendar-adjusted -------------------------
  # Two passes.  The first is an ordinary robust smooth of the whole series; it is
  # used only to estimate the calendar factors and the dispersion.  The second
  # re-estimates the baseline for each candidate window from dates OUTSIDE that
  # window (see `.batch_add_window_statistics()`), which is what keeps the episode
  # from contaminating the baseline it is being judged against.
  baseline_window <- .batch_baseline_window(baseline_window, lookback, period)
  registration    <- .batch_add_baseline(registration, baseline_window, period, baseline_method)

  # -- 4. window statistics Delta and W, on a leave-window-out baseline ---------
  registration <- .batch_add_window_statistics(
    registration, lookback, baseline_window, period
  )

  # -- 5. p-values under the appropriate null ---------------------------------
  is_signed  <- identical(get_data_type(data), "count-cumulative")
  null_used  <- if (identical(null_model, "auto")) {
    if (is_signed) "robust" else "poisson"
  } else {
    null_model
  }
  registration <- .batch_add_p_values(registration, null_used)

  # -- 6. classify and flag ----------------------------------------------------
  registration <- .batch_classify(registration, alpha)
  registration <- dplyr::select(registration, -"baseline_global", -"baseline_smooth")

  structure(
    dplyr::as_tibble(registration),
    class      = c("batch_screen", class(dplyr::tibble())),
    lookback   = lookback,
    period     = period,
    null_model = null_used,
    alpha      = alpha
  )
}

# =============================================================================
# Step 1: one signed count per (event date, report date, stratum)
# =============================================================================

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
.batch_report_increments <- function(data) {
  observations <- as.data.frame(data)
  event_col    <- get_event_date(data)
  report_col   <- get_report_date(data)
  data_type    <- get_data_type(data)
  strata_cols  <- get_strata(data)
  case_count_col <- get_case_count(data)

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
  date_grid <- .batch_date_grid(observations, data)
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
.batch_date_grid <- function(observations, data) {
  report_unit <- get_report_units(data) %||% "days"
  grid_start  <- min(observations$.event_date,  na.rm = TRUE)
  grid_end    <- max(observations$.report_date, na.rm = TRUE)
  seq(from = grid_start, to = grid_end, by = as.character(report_unit))
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
.batch_add_baseline <- function(registration, baseline_window, period, baseline_method) {
  stratum_levels    <- sort(unique(registration$stratum))
  smooth_values     <- rep(NA_real_, nrow(registration))   # schedule-blind
  adjusted_values   <- rep(NA_real_, nrow(registration))   # schedule-corrected

  for (stratum_value in stratum_levels) {
    stratum_rows <- which(registration$stratum == stratum_value)
    reported     <- registration$reported[stratum_rows]

    # The schedule-blind smooth is what the phase factors must be estimated from:
    # taking the ratio of the data to an already-corrected baseline would show no
    # cycle at all.
    stratum_smooth <- .batch_smooth(reported, baseline_window, baseline_method)
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

#' Dispatch to the chosen robust smoother.
#' @keywords internal
#' @noRd
.batch_smooth <- function(reported, baseline_window, baseline_method) {
  if (identical(baseline_method, "repeated_median")) {
    .batch_repeated_median(reported, baseline_window)
  } else {
    .batch_running_median(reported, baseline_window)
  }
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
#' Fits a local *constant*, so it is biased wherever the series trends.  Kept as
#' an option because it is the estimator the theory is stated for, and it is the
#' right choice for a flat series.
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
#' Measured on real data (see the "Batch detection" vignette): with a planted
#' release on FluSight, the residual \eqn{|\Delta|} at the release date falls from
#' 424 to 49 when the running median is replaced by the repeated median, and
#' clean-data false positives fall from 1 to 0 on both FluSight and dengue.
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
#' the one with the exact null (see the mathematics vignette, Theorem 1).
#' Rows whose window runs off the start of the series get `NA`.
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

#' Keep p-values inside [0, 1] and propagate NA.
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

#' Warn, on every call, that the batch-detection functions are experimental.
#'
#' The batch detectors are new and their statistical behaviour and interface are
#' still settling.  Every user-facing entry point (`batch_screen()`,
#' `batch_shape_test()`, `simulate_batch()`) calls this so a user is always told
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
    # (`test_delay_drift()`, `test_delay_changepoint()`), so a user is warned
    # without being buried when a function is called in a loop.
    .frequency    = "regularly",
    .frequency_id = paste0("tbl.now::", function_name)
  )
  invisible(NULL)
}

#' @keywords internal
#' @noRd
.batch_check_tbl_now <- function(data) {
  if (!is_tbl_now(data)) {
    cli::cli_abort(
      "{.arg data} must be a {.cls tbl_now} object. Got {.cls {class(data)}}."
    )
  }
  invisible(TRUE)
}

#' Print a batch screen
#' @param x A `batch_screen` object.
#' @param ... Unused.
#' @export
#' @noRd
print.batch_screen <- function(x, ...) {
  flagged <- x[!is.na(x$batch) & x$batch, , drop = FALSE]

  cli::cli_h1("Batch screen")
  cli::cli_text(
    "{nrow(x)} (report date, stratum) pair{?s}; ",
    "look-back {attr(x, 'lookback')}; null {.val {attr(x, 'null_model')}}",
    if (!is.null(attr(x, "period"))) "; calendar period {attr(x, 'period')}" else ""
  )

  if (nrow(flagged) == 0L) {
    cli::cli_alert_success("No batches flagged at alpha = {attr(x, 'alpha')} (BH-adjusted).")
  } else {
    cli::cli_alert_warning(
      "{nrow(flagged)} batch{?/es} flagged at alpha = {attr(x, 'alpha')} (BH-adjusted):"
    )
    for (row_index in seq_len(min(nrow(flagged), 10L))) {
      cli::cli_li(
        "{format(flagged$report_date[row_index])} [{flagged$stratum[row_index]}] -- ",
        "reported {round(flagged$reported[row_index])}, ",
        "baseline {round(flagged$baseline[row_index], 1)}, ",
        "deficit {round(flagged$deficit[row_index], 1)}, ",
        "delta {round(flagged$delta[row_index], 1)}"
      )
    }
    if (nrow(flagged) > 10L) cli::cli_text("... and {nrow(flagged) - 10L} more.")
  }

  classification_counts <- table(x$classification, useNA = "no")
  if (length(classification_counts) > 0L) {
    cli::cli_h3("Classification")
    for (label in names(classification_counts)) {
      cli::cli_li("{label}: {classification_counts[[label]]}")
    }
  }
  invisible(x)
}
