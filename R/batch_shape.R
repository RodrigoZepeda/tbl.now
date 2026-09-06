# =============================================================================
# Model-free shape test on the report axis
# =============================================================================
# NOTE ON PACKAGE PLACEMENT.  Model-free; destined for `tbl.now`.  See the header
# of `35_batch_test_tbl_now.R`.
#
# `diagnose_batches()` looks only at *how many* reports arrived on each date.  This
# file looks at *which event dates they came from* -- equivalently, at the
# distribution of reporting delays among the reports that arrived on one date.
#
# THE IDEA.  A backlog is old.  When a stalled system releases, the reports it
# dumps describe events that happened a while ago, so their delays are inflated
# relative to a normal reporting date.  We can test that without a model.
#
# WHY IT IS EXACTLY VALID.  Under the "no batch" hypothesis every item draws its
# delay from one fixed distribution `g_D`, independently of its event date.  The
# chance that a report arriving on date `r` came from `delta` steps ago is
# therefore proportional to `lambda_{r-delta} * g_D(delta)`: how many items that
# event date produced, times the chance of waiting exactly `delta`.  Now suppose
# the intensity is *log-linear* over a short window, `lambda_t = exp(a + zeta t)`.
# Substituting,
#
#     q_r(delta)  proportional to  exp(a + zeta(r - delta)) * g_D(delta)
#                 proportional to  exp(-zeta * delta) * g_D(delta)
#
# because `exp(zeta * r)` is constant in `delta` and cancels in the
# normalisation.  The result does not depend on `r` at all: **neighbouring
# report dates share one common delay profile**, whatever `lambda`, `g_D` and
# `zeta` happen to be.  Hence, conditionally on how many reports each date
# received, the date labels are exchangeable, and *any* permutation test is
# exactly distribution-free.  Only the *curvature* of `log lambda` biases it, and
# only at second order.  (Mathematics vignette, Proposition 5.)
# =============================================================================

#' Test whether one report date drew from unusually old event dates
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' A complement to [diagnose_batches()], which sees only report *volumes*.  This test
#' asks whether the reports that arrived on a candidate date came from
#' systematically *older* event dates -- the signature of a released backlog --
#' by comparing their delays with those of neighbouring report dates.  It is
#' **model-free** and, under the conditions below, **exactly distribution-free**.
#'
#' @details
#' The delays of the reports arriving on `at` are compared with the pooled delays
#' of the reports arriving on nearby dates, using a one-sided rank-sum (Wilcoxon)
#' statistic directed at *longer* delays on `at`. The p-value comes from a
#' permutation, so no asymptotic approximation is used.
#'
#' The test is model-free: as long as the epidemic curve is locally smooth,
#' neighbouring report dates share one common delay profile, so their delay labels
#' are exchangeable and the permutation test is (essentially) distribution-free --
#' it needs neither the delay distribution nor the epidemic curve. With Poisson
#' counts `permute = "items"` is exact; if the counts are overdispersed
#' (neighbouring report dates share event dates, so individual items are not
#' exchangeable) use `permute = "blocks"`, which permutes whole report dates.
#'
#' `at` need not be a date that carries rows. A line list cannot represent a zero,
#' so a report date on which nothing arrived has no rows at all; that is the
#' observation "no arrivals", not a missing one, and the test reports zero
#' arrivals for it instead of aborting. Only a date off the object's report grid
#' is an error.
#'
#' The `guard` argument omits report dates immediately adjacent to `at` from the
#' comparison set: if a batch is present, its own deficit dates sit right beside
#' the spike and would contaminate the reference group. For `"count-cumulative"`
#' data only positive increments carry a meaningful delay; negative increments
#' (down-revisions) are dropped with a message.
#'
#' @param x A [tbl_now()] object.
#' @param at The candidate report date (coercible to the class of the report
#'   column), typically one flagged by [diagnose_batches()]. A date on the
#'   report grid that carries no rows is reported as zero arrivals, not an
#'   error.
#' @param neighbours Number of report dates on each side used as the reference
#'   group.  Default `3`.
#' @param guard Number of report dates immediately either side of `at` to skip.
#'   Default `1`.  Increase it to at least the longest plausible stall.
#' @param permute `"items"` (default; exact under log-linear intensity and
#'   Poisson counts) or `"blocks"` (permutes whole report dates; valid under
#'   overdispersion).
#' @param n_permutations Number of permutations. Default `999`.
#' @param drop_censored Logical. Ignore the rows whose date on `axis` is
#'   flagged censored (`is_censored_report`, or `is_censored_validation` on the
#'   validation axis). Default `TRUE`: a censored date is a *bound*, not the
#'   date the record arrived, so those rows would pile up on the censoring date
#'   and be rediscovered as the very batch the censoring already recorded.
#' @param axis Which time axis to scan for arrivals: `"report"` (default) or
#'   `"validation"`. The question is the same either way -- did an unusual
#'   number of records land on this date? -- so a laboratory clearing its
#'   backlog is found exactly as a surveillance system clearing its inbox is.
#'   `"validation"` needs a validation process (see [add_validation_date()])
#'   and ignores cases that are still `"pending"`, which have no validation
#'   date to arrive on.
#' @param seed Optional RNG seed.
#'
#' @returns A tibble, one row per stratum, with `stratum`, `n_at`,
#'   `n_reference`, `mean_delay_at`, `mean_delay_reference`, `statistic`
#'   (standardised rank-sum) and `p_value` (one-sided: longer delays on `at`).
#'
#' @seealso
#' [diagnose_batches()], which finds the report dates worth passing to `at`;
#' [simulate_batch()] to plant a batch of known shape and check it is recovered;
#' [plot_delay_profiles()] to see the delay profile this tests.
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
#' # Pick a report date to interrogate. A real workflow takes this from
#' ## diagnose_batches(); here we simply name one.
#' diagnose_batches2(dengue_tbl, at = as.Date("1990-06-25"), n_permutations = 99)
#'
#' # `n_permutations` sets the resolution of the p-value: 99 keeps the example
#' ## fast, but use the default (999) for anything you intend to report.
#'
#' @export
diagnose_batches2 <- function(x,
                              at,
                              neighbours     = 3L,
                              guard          = 1L,
                              permute        = c("items", "blocks"),
                              n_permutations = 999L,
                              axis           = c("report", "validation"),
                              drop_censored  = TRUE,
                              seed           = NULL) {
  permute <- match.arg(permute)
  axis    <- match.arg(axis)
  check_bool(drop_censored, "drop_censored")
  .batch_experimental_warning("diagnose_batches2")
  .batch_check_tbl_now(x)
  if (!is.null(seed)) set.seed(seed)

  neighbours <- as.integer(neighbours)
  guard      <- as.integer(guard)
  if (neighbours < 1L) cli::cli_abort("`neighbours` must be at least 1. Got {neighbours}.")
  if (guard < 0L)      cli::cli_abort("`guard` must be non-negative. Got {guard}.")

  increments <- .batch_report_increments(x, axis = axis,
                                         drop_censored = drop_censored)

  # Only appearing reports carry a delay; down-revisions do not.
  if (any(increments$.count < 0)) {
    cli::cli_alert_info(
      "Dropping {sum(increments$.count < 0)} negative increment{?s} (down-revisions) \\
       before the delay comparison."
    )
    increments <- dplyr::filter(increments, .data$.count > 0)
  }

  candidate_date <- .batch_match_report_date(increments, at, x, axis)

  stratum_levels <- sort(unique(increments$.stratum))
  result_rows <- vector("list", length(stratum_levels))

  for (stratum_index in seq_along(stratum_levels)) {
    stratum_value    <- stratum_levels[stratum_index]
    stratum_reports  <- dplyr::filter(increments, .data$.stratum == stratum_value)
    result_rows[[stratum_index]] <- .batch_shape_test_one_stratum(
      stratum_reports = stratum_reports,
      stratum_value   = stratum_value,
      candidate_date  = candidate_date,
      neighbours      = neighbours,
      guard           = guard,
      permute         = permute,
      n_permutations  = n_permutations
    )
  }

  dplyr::bind_rows(result_rows)
}

#' Run the shape test within a single stratum.
#' @keywords internal
#' @noRd
.batch_shape_test_one_stratum <- function(stratum_reports, stratum_value, candidate_date,
                                          neighbours, guard, permute, n_permutations) {
  report_dates <- sort(unique(stratum_reports$.report_date))
  candidate_position <- match(candidate_date, report_dates)

  empty_result <- dplyr::tibble(
    stratum = stratum_value, n_at = 0L, n_reference = 0L,
    mean_delay_at = NA_real_, mean_delay_reference = NA_real_,
    statistic = NA_real_, p_value = NA_real_
  )
  if (is.na(candidate_position)) return(empty_result)

  # Reference dates: `neighbours` on each side, skipping `guard` immediately
  # adjacent dates (which a batch would have contaminated with its own deficit).
  offsets <- c(-(guard + neighbours):-(guard + 1L), (guard + 1L):(guard + neighbours))
  reference_positions <- candidate_position + offsets
  reference_positions <- reference_positions[
    reference_positions >= 1L & reference_positions <= length(report_dates)
  ]
  if (length(reference_positions) == 0L) return(empty_result)
  reference_dates <- report_dates[reference_positions]

  candidate_delays <- .batch_expand_delays(
    dplyr::filter(stratum_reports, .data$.report_date == candidate_date)
  )
  reference_frame <- dplyr::filter(stratum_reports, .data$.report_date %in% reference_dates)
  reference_delays <- .batch_expand_delays(reference_frame)

  if (length(candidate_delays) == 0L || length(reference_delays) == 0L) return(empty_result)

  observed_statistic <- .batch_rank_sum_statistic(candidate_delays, reference_delays)

  permuted_statistics <- if (identical(permute, "items")) {
    .batch_permute_items(candidate_delays, reference_delays, n_permutations)
  } else {
    .batch_permute_blocks(stratum_reports, candidate_date, reference_dates, n_permutations)
  }

  # One-sided (longer delays on `at`), with the observed value included, which
  # keeps the permutation p-value valid rather than merely unbiased.
  p_value <- (1 + sum(permuted_statistics >= observed_statistic)) / (1 + n_permutations)

  dplyr::tibble(
    stratum              = stratum_value,
    n_at                 = length(candidate_delays),
    n_reference          = length(reference_delays),
    mean_delay_at        = mean(candidate_delays),
    mean_delay_reference = mean(reference_delays),
    statistic            = observed_statistic,
    p_value              = p_value
  )
}

#' Expand `(delay, count)` rows into one delay value per reported item.
#' @keywords internal
#' @noRd
.batch_expand_delays <- function(report_rows) {
  if (nrow(report_rows) == 0L) return(numeric(0))
  rep(report_rows$.delay, times = round(report_rows$.count))
}

#' Standardised rank-sum: large when `candidate_delays` are stochastically longer.
#' @keywords internal
#' @noRd
.batch_rank_sum_statistic <- function(candidate_delays, reference_delays) {
  pooled_ranks     <- rank(c(candidate_delays, reference_delays))
  # Use doubles: on count data the delays are expanded to one value per item, so
  # the group sizes can exceed the 32-bit integer range and their product would
  # otherwise overflow to NA (`n_candidate * n_reference` for two 50k+ groups).
  n_candidate      <- as.double(length(candidate_delays))
  n_reference      <- as.double(length(reference_delays))
  candidate_sum    <- sum(pooled_ranks[seq_len(n_candidate)])

  expected_sum <- n_candidate * (n_candidate + n_reference + 1) / 2
  variance_sum <- n_candidate * n_reference * (n_candidate + n_reference + 1) / 12
  if (!is.finite(variance_sum) || variance_sum <= 0) return(0)
  (candidate_sum - expected_sum) / sqrt(variance_sum)
}

#' Permute item labels: exact under log-linear intensity with Poisson counts.
#' @keywords internal
#' @noRd
.batch_permute_items <- function(candidate_delays, reference_delays, n_permutations) {
  pooled_delays <- c(candidate_delays, reference_delays)
  n_candidate   <- length(candidate_delays)

  permuted_statistics <- numeric(n_permutations)
  for (permutation_index in seq_len(n_permutations)) {
    shuffled <- sample(pooled_delays)
    permuted_statistics[permutation_index] <- .batch_rank_sum_statistic(
      shuffled[seq_len(n_candidate)],
      shuffled[-seq_len(n_candidate)]
    )
  }
  permuted_statistics
}

#' Permute whole report dates: respects the dependence overdispersion induces
#' between neighbouring report dates (they share event dates, hence random effects).
#' @keywords internal
#' @noRd
.batch_permute_blocks <- function(stratum_reports, candidate_date, reference_dates,
                                  n_permutations) {
  block_dates <- c(candidate_date, reference_dates)
  blocks <- vector("list", length(block_dates))
  for (block_index in seq_along(block_dates)) {
    blocks[[block_index]] <- .batch_expand_delays(
      dplyr::filter(stratum_reports, .data$.report_date == block_dates[block_index])
    )
  }

  permuted_statistics <- numeric(n_permutations)
  for (permutation_index in seq_len(n_permutations)) {
    shuffled_order  <- sample(seq_along(blocks))
    pseudo_candidate <- blocks[[shuffled_order[1]]]
    pseudo_reference <- unlist(blocks[shuffled_order[-1]], use.names = FALSE)
    if (length(pseudo_candidate) == 0L || length(pseudo_reference) == 0L) {
      permuted_statistics[permutation_index] <- 0
      next
    }
    permuted_statistics[permutation_index] <-
      .batch_rank_sum_statistic(pseudo_candidate, pseudo_reference)
  }
  permuted_statistics
}

#' Coerce `at` onto the report-date grid, with a helpful error.
#'
#' A date with no rows is not an error. A line list cannot represent a zero, and
#' `count-incidence` data need not either, so a report date on which nothing
#' arrived simply has no rows -- which is the observation "no arrivals", not a
#' missing one. Such a date is returned unchanged and the test reports zero
#' arrivals against its neighbours. Only a date off the object's report grid
#' altogether, where there is nothing to compare against, is an error.
#'
#' @param increments The increments table.
#' @param x The `tbl_now`, for the report units.
#' @param axis `"report"` or `"validation"`.
#'
#' @keywords internal
#' @noRd
.batch_match_report_date <- function(increments, at, x, axis = "report") {
  report_dates <- sort(unique(increments$.report_date))
  candidate    <- tryCatch(
    methods::as(at, class(report_dates)[1]),
    error = function(e) at
  )
  if (candidate %in% report_dates) return(candidate)

  report_unit <- if (identical(axis, "validation")) {
    get_validation_units(x) %||% get_report_units(x) %||% "days"
  } else {
    get_report_units(x) %||% "days"
  }
  grid <- .tbl_now_date_seq(min(report_dates), max(report_dates), report_unit)
  if (candidate %in% grid) {
    cli::cli_inform(c(
      "i" = "No records arrived on {format(candidate)}; reporting zero arrivals \
             rather than a test."
    ))
    return(candidate)
  }

  cli::cli_abort(c(
    "`at` ({format(at)}) is not on the observed report-date grid.",
    "i" = "Report dates run from {format(min(report_dates))} to \
           {format(max(report_dates))}, stepping in {report_unit}."
  ))
}
