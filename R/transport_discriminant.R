# =============================================================================
# transport_discriminant(): the (deficit W, discriminant Delta) pair per date
#
# diagnose_batches()'s conservation law lives in a plane. For each report date it
# forms two quantities on a leave-window-out baseline:
#
#   * the deficit  W = sum_{j<r} (mu_j - R_j)  -- reports the preceding window is
#     MISSING. Transport (a batch releasing a backlog) leaves a deficit; genuine
#     creation does not. W is the *transport* axis.
#   * the discriminant Delta = S - M           -- the window total, centred. A
#     transport conserves the window total, so Delta ~ 0; only *created* items
#     push Delta up. Delta is the *creation* axis.
#
# Standardised (transport_z, creation_z), every report date is a point in this
# plane and the four quadrants read straight off it:
#
#            creation_z ~ 0        creation_z >> 0
#   W ~ 0        nothing                surge
#   W >> 0        batch            batch and surge
#
# transport_discriminant() returns that plane; diagnose_batches() is the same
# machinery turned into hypothesis tests.
# =============================================================================

#' The transport discriminant of a reporting series
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Computes, for every report date, the two coordinates of [diagnose_batches()]'s
#' conservation law -- the **deficit** (the *transport* axis: how many reports the
#' preceding window is missing) and the window **discriminant** (the *creation*
#' axis: the window total relative to its baseline) -- together with their robust
#' standardised versions `transport_z` and `creation_z`.
#'
#' @details
#' A batch *moves* reports later without creating them, so it leaves a positive
#' deficit while conserving the window total (`transport_z` large, `creation_z`
#' near 0). A genuine surge *creates* reports, lifting the window total without a
#' deficit (`creation_z` large, `transport_z` near 0). Reading the two together
#' separates a backlog release from an epidemic surge: a point sits in the
#' **batch** corner when its transport score is large and its creation score is
#' not. A negative `creation_z` with no transport is a hold in progress (the
#' window is depleted and nothing has been released yet). The `classification`
#' column applies these labels at level `alpha`, exactly as in [diagnose_batches()].
#'
#' @param x A [tbl_now()] object.
#' @param lookback Integer window half-width `k` (report-grid steps) over which the
#'   deficit is accumulated. Default `7` (a week of daily reporting).
#' @param baseline_window,period Baseline controls, passed through to the same
#'   machinery as [diagnose_batches()]. `period` (e.g. `7`) absorbs a scheduled weekly
#'   reporting cadence.
#' @param alpha Level for the `classification` labels. Default `0.05`.
#'
#' @param axis Which time axis to scan for arrivals: `"report"` (default) or
#'   `"confirmation"`. Needs a confirmation process (see [add_confirmation()]);
#'   cases still `"pending"` are left out.
#' @returns A tibble of class `transport_discriminant`, one row per (report date,
#'   stratum), with columns `report_date`, `stratum`, `reported`, `baseline`,
#'   `window_total`, `spike` (reported minus baseline), `deficit`, `delta`,
#'   `transport_z`, `creation_z`, `classification` and `batch`.
#'
#' @seealso [diagnose_batches()] for the hypothesis test, [diagnostic_plot()] to plot
#'   this plane.
#'
#' @examples
#' data(denguedat)
#' dn <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)
#' td <- transport_discriminant(dn)
#' td[td$batch, ]
#'
#' @export
#' @md
transport_discriminant <- function(x,
                                    lookback        = 7L,
                                    baseline_window = NULL,
                                    period          = NULL,
                                    alpha           = 0.05,
                                    axis            = c("report", "confirmation")) {
  axis <- match.arg(axis)
  .batch_experimental_warning("transport_discriminant")
  .batch_check_tbl_now(x)

  lookback <- as.integer(lookback)
  if (lookback < 1L) {
    cli::cli_abort("`lookback` must be a positive integer. Got {lookback}.")
  }
  if (alpha <= 0 || alpha >= 1) {
    cli::cli_abort("`alpha` must lie strictly between 0 and 1. Got {alpha}.")
  }

  registration <- .batch_registration(x, lookback, baseline_window, period, axis = axis)
  dispersion   <- .batch_dispersion(registration)

  registration <- dplyr::mutate(
    registration,
    spike       = .data$reported - .data$baseline,
    transport_z = .data$deficit / sqrt(dispersion * .data$deficit_scale),
    creation_z  = .data$delta   / sqrt(dispersion * .data$window_scale)
  )

  # Reuse diagnose_batches's robust p-values and quadrant classification so the labels
  # match the test exactly.
  registration <- .batch_add_p_values(registration, "robust")
  registration <- .batch_classify(registration, alpha)

  out <- dplyr::transmute(
    registration,
    report_date    = .data$report_date,
    stratum        = .data$stratum,
    reported       = .data$reported,
    baseline       = .data$baseline,
    window_total   = .data$window_total,
    spike          = .data$spike,
    deficit        = .data$deficit,
    delta          = .data$delta,
    transport_z    = .data$transport_z,
    creation_z     = .data$creation_z,
    p_transport    = .data$p_transport,
    p_creation     = .data$p_creation,
    classification = .data$classification,
    batch          = .data$batch
  )

  structure(
    dplyr::as_tibble(out),
    class      = c("transport_discriminant", class(dplyr::tibble())),
    lookback   = lookback,
    period     = period,
    alpha      = alpha,
    dispersion = dispersion
  )
}

#' @export
print.transport_discriminant <- function(x, ...) {
  n_batch <- sum(x$batch, na.rm = TRUE)
  n_surge <- sum(x$classification == "surge", na.rm = TRUE)
  # `cat_line()` (stdout), not `cli_text()` (a message): print output must
  # survive `message = FALSE`, `sink()` and `capture.output()`.
  cli::cat_line(cli::format_inline(paste0(
    "{.cls transport_discriminant}: {nrow(x)} report date{?s}, ",
    "look-back {attr(x, 'lookback')}, {n_batch} batch{?es} and {n_surge} ",
    "surge{?s} at {.field alpha} = {attr(x, 'alpha')}."
  )))
  NextMethod()
  invisible(x)
}
