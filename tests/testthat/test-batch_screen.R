# =============================================================================
# Model-free batch detection: batch_screen(), batch_shape_test(), simulate_batch()
# =============================================================================
# These functions are model-free (no nowcast, no RTMB), so every test here is
# fast and uses small synthetic data with a *known* planted batch.

# -- helpers -------------------------------------------------------------------

# A flat linelist stream: constant intensity, geometric reporting lag, so the
# report series has no trend and the theory's assumptions hold exactly.
make_flat_linelist <- function(n_origins = 60L, per_origin = 12L, seed = 1L) {
  set.seed(seed)
  origin_dates <- seq(as.Date("2021-01-01"), by = "day", length.out = n_origins)

  report_rows <- vector("list", n_origins)
  for (origin_index in seq_len(n_origins)) {
    origin_date  <- origin_dates[origin_index]
    n_items      <- stats::rpois(1L, per_origin)
    if (n_items == 0L) next
    reporting_lag <- stats::rgeom(n_items, prob = 0.5)
    report_rows[[origin_index]] <- data.frame(
      onset  = rep(origin_date, n_items),
      report = origin_date + reporting_lag
    )
  }
  observations <- dplyr::bind_rows(report_rows)
  observations <- observations[observations$report <= max(origin_dates), , drop = FALSE]

  tbl_now(
    observations,
    event_date = !!as.symbol("onset"), report_date = !!as.symbol("report"),
    data_type = "linelist", verbose = FALSE
  )
}

# -- simulate_batch() ----------------------------------------------------------

test_that("simulate_batch() conserves items and only ever moves reports later", {
  clean_tbl <- make_flat_linelist()
  closed    <- as.Date(c("2021-02-01", "2021-02-02", "2021-02-03"))

  batched_tbl <- simulate_batch(clean_tbl, closed_dates = closed, verbose = FALSE)

  clean_frame   <- as.data.frame(clean_tbl)
  batched_frame <- as.data.frame(batched_tbl)

  # Mass conservation: the closure is far from the end, so nothing is lost.
  expect_equal(nrow(batched_frame), nrow(clean_frame))
  # The origin coordinate is untouched.
  expect_setequal(batched_frame$onset, clean_frame$onset)
  # No report ever moves earlier.
  expect_true(all(sort(batched_frame$report) >= sort(clean_frame$report)))
  # The closed dates report nothing at all.
  expect_false(any(batched_frame$report %in% closed))
})

test_that("simulate_batch() rejects an empty or fully-closed schedule", {
  clean_tbl <- make_flat_linelist(n_origins = 20L)
  expect_error(simulate_batch(clean_tbl, closed_dates = as.Date(character(0))), "no batch")

  every_date <- seq(as.Date("2021-01-01"), as.Date("2021-01-20"), by = "day")
  expect_error(simulate_batch(clean_tbl, closed_dates = every_date), "nowhere to release")
})

# -- batch_screen(): the conservation law --------------------------------------

test_that("batch_screen() recovers a planted batch and finds none in clean data", {
  clean_tbl   <- make_flat_linelist()
  closed      <- as.Date(c("2021-02-01", "2021-02-02", "2021-02-03"))
  release_date <- as.Date("2021-02-04")
  batched_tbl <- simulate_batch(clean_tbl, closed_dates = closed, verbose = FALSE)

  clean_screen   <- batch_screen(clean_tbl,   lookback = 3L)
  batched_screen <- batch_screen(batched_tbl, lookback = 3L)

  expect_s3_class(batched_screen, "batch_screen")
  expect_equal(sum(clean_screen$batch, na.rm = TRUE), 0L)

  flagged_dates <- batched_screen$report_date[!is.na(batched_screen$batch) & batched_screen$batch]
  expect_true(release_date %in% flagged_dates)
})

test_that("the release date shows a spike paid for by a deficit", {
  clean_tbl    <- make_flat_linelist()
  closed       <- as.Date(c("2021-02-01", "2021-02-02", "2021-02-03"))
  release_date <- as.Date("2021-02-04")
  batched_tbl  <- simulate_batch(clean_tbl, closed_dates = closed, verbose = FALSE)

  screened <- batch_screen(batched_tbl, lookback = 3L)
  release_row <- screened[screened$report_date == release_date, ]

  # The spike is large ...
  expect_gt(release_row$reported, 2 * release_row$baseline)
  # ... the deficit accounts for it ...
  expect_gt(release_row$deficit, 0)
  # ... and the window total is essentially unchanged next to the spike.
  expect_lt(abs(release_row$delta), 0.5 * release_row$reported)
  # The transport signal is overwhelming; a *creation* flag may also fire by
  # chance at rate alpha, so both verdicts containing "batch" are acceptable.
  expect_lt(release_row$p_transport, 1e-4)
  expect_true(release_row$classification %in% c("batch", "batch_and_surge"))
})

test_that("Delta is an exact pivot: a within-window transport cannot move it at all", {
  # Theorem 1 says the window total is *pathwise* invariant to any transport whose
  # displacements stay inside the window: every item we would have seen in the
  # window we still see in the window, just on another date.  And because the
  # baseline is refitted from dates strictly *outside* the window, `M` cannot see
  # the batch either.  So `Delta = S - M` should be **numerically identical** on
  # clean and batched data -- not merely equal in expectation.
  release_date <- as.Date("2021-02-04")
  closed       <- as.Date(c("2021-02-01", "2021-02-02", "2021-02-03"))

  for (replicate_index in seq_len(8L)) {
    clean_tbl   <- make_flat_linelist(seed = 100L + replicate_index)
    batched_tbl <- simulate_batch(clean_tbl, closed_dates = closed, verbose = FALSE)

    clean_screen   <- batch_screen(clean_tbl,   lookback = 3L)
    batched_screen <- batch_screen(batched_tbl, lookback = 3L)

    delta_clean   <- clean_screen$delta[clean_screen$report_date == release_date]
    delta_batched <- batched_screen$delta[batched_screen$report_date == release_date]

    expect_equal(delta_batched, delta_clean, tolerance = 1e-8)
  }
})

test_that("the deficit, by contrast, is exactly what the batch moved", {
  # `Delta` is blind to the batch; `W` is the statistic that sees it.
  release_date <- as.Date("2021-02-04")
  closed       <- as.Date(c("2021-02-01", "2021-02-02", "2021-02-03"))

  clean_tbl   <- make_flat_linelist(seed = 11L)
  batched_tbl <- simulate_batch(clean_tbl, closed_dates = closed, verbose = FALSE)

  clean_screen   <- batch_screen(clean_tbl,   lookback = 3L)
  batched_screen <- batch_screen(batched_tbl, lookback = 3L)

  clean_deficit   <- clean_screen$deficit[clean_screen$report_date == release_date]
  batched_deficit <- batched_screen$deficit[batched_screen$report_date == release_date]

  expect_gt(batched_deficit, clean_deficit)
  expect_gt(batched_deficit, 0)
})

test_that("a hold that never releases is classified as hold_or_deletion, not a batch", {
  clean_tbl <- make_flat_linelist()
  all_dates <- sort(unique(as.data.frame(clean_tbl)$report))
  # Close the final stretch: the release never happens inside the observed window.
  closed <- utils::tail(all_dates, 4L)

  batched_tbl <- simulate_batch(clean_tbl, closed_dates = closed,
                                drop_unreleased = TRUE, verbose = FALSE)
  screened <- batch_screen(batched_tbl, lookback = 3L)

  # No spike ever arrives, so nothing should be called a batch at the tail.
  tail_rows <- utils::tail(screened, 3L)
  expect_false(any(tail_rows$classification %in% "batch", na.rm = TRUE))
})

# -- batch_screen(): calendar effects ------------------------------------------

test_that("a scheduled weekly closure is absorbed by `period` and not called a batch", {
  set.seed(7)
  origin_dates <- seq(as.Date("2021-01-04"), by = "day", length.out = 100L)
  report_rows  <- vector("list", length(origin_dates))
  for (origin_index in seq_along(origin_dates)) {
    origin_date <- origin_dates[origin_index]
    n_items     <- stats::rpois(1L, 25L)
    if (n_items == 0L) next
    report_date <- origin_date + stats::rgeom(n_items, prob = 0.5)
    # Push weekend reports to the following Monday: a *scheduled* transport.
    weekday <- as.POSIXlt(report_date)$wday
    report_date <- report_date + ifelse(weekday == 6L, 2L, ifelse(weekday == 0L, 1L, 0L))
    report_rows[[origin_index]] <- data.frame(onset = origin_date, report = report_date)
  }
  observations <- dplyr::bind_rows(report_rows)
  observations <- observations[observations$report <= max(origin_dates), , drop = FALSE]
  scheduled_tbl <- tbl_now(
    observations, event_date = !!as.symbol("onset"), report_date = !!as.symbol("report"),
    data_type = "linelist", verbose = FALSE
  )

  unadjusted <- batch_screen(scheduled_tbl, lookback = 3L)
  adjusted   <- batch_screen(scheduled_tbl, lookback = 3L, period = 7L)

  unadjusted_flags <- sum(unadjusted$batch, na.rm = TRUE)
  adjusted_flags   <- sum(adjusted$batch,   na.rm = TRUE)

  # Left unmodelled, the schedule fakes a batch on every release day of the cycle.
  expect_gt(unadjusted_flags, 4L)
  # Modelled, essentially all of them go away (a stray flag is a Type-I error).
  expect_lt(adjusted_flags, unadjusted_flags)
  expect_lte(adjusted_flags, 2L)
})

# -- batch_screen(): argument validation ---------------------------------------

test_that("batch_screen() validates its inputs", {
  clean_tbl <- make_flat_linelist(n_origins = 30L)

  expect_error(batch_screen(as.data.frame(clean_tbl)), "tbl_now")
  expect_error(batch_screen(clean_tbl, lookback = 0L), "positive integer")
  expect_error(batch_screen(clean_tbl, alpha = 1.5), "strictly between")
  # An even baseline window has no unique median.
  expect_error(batch_screen(clean_tbl, baseline_window = 8L), "must be odd")
  # Too narrow: a batch episode would outvote the median measuring it.
  expect_error(batch_screen(clean_tbl, lookback = 3L, baseline_window = 5L), "too narrow")
})

test_that("the null model is chosen from the data type", {
  clean_tbl <- make_flat_linelist(n_origins = 40L)
  expect_equal(attr(batch_screen(clean_tbl), "null_model"), "poisson")
  expect_equal(attr(batch_screen(clean_tbl, null_model = "robust"), "null_model"), "robust")
})

# -- the robust baseline -------------------------------------------------------

test_that("both baselines are exact on a monotone trend (the median is order-preserving)", {
  trend    <- 10 + 2 * seq_len(41)
  running  <- .batch_running_median(trend, 11L)
  repeated <- .batch_repeated_median(trend, 11L)

  interior <- 8:34
  expect_lt(max(abs(running[interior]  - trend[interior])), 1e-6)
  expect_lt(max(abs(repeated[interior] - trend[interior])), 1e-6)
})

test_that("the repeated median survives an asymmetric outlier patch inside a trend", {
  # This is where the repeated median earns its keep: a batch episode puts several
  # deficits *and* one large spike inside the smoothing window, dragging the local
  # order statistics in opposite directions.  Estimating the slope explicitly is
  # what lets the fit recover the underlying line.
  trend     <- 10 + 2 * seq_len(41)
  corrupted <- trend
  corrupted[19:21] <- 0                                   # the hold
  corrupted[22]    <- trend[22] + sum(trend[19:21])       # the release

  running  <- .batch_running_median(corrupted, 11L)
  repeated <- .batch_repeated_median(corrupted, 11L)

  # Judge the baseline at the release date itself, which is what `Delta` uses.
  running_error  <- abs(running[22]  - trend[22])
  repeated_error <- abs(repeated[22] - trend[22])
  expect_lt(repeated_error, running_error)
})

test_that("the repeated median resists a batch episode (50% breakdown)", {
  flat <- rep(20, 41)
  corrupted <- flat
  corrupted[19:21] <- 0     # three deficit dates
  corrupted[22]    <- 80    # then the spike

  baseline <- .batch_repeated_median(corrupted, 11L)
  # A clean point outside the episode is unaffected.
  expect_equal(baseline[5], 20, tolerance = 1e-6)
  expect_equal(baseline[38], 20, tolerance = 1e-6)
})

# -- batch_shape_test() --------------------------------------------------------

test_that("batch_shape_test() sees the inflated delays of a released backlog", {
  clean_tbl    <- make_flat_linelist(n_origins = 70L, per_origin = 15L, seed = 3L)
  closed       <- as.Date(c("2021-02-01", "2021-02-02", "2021-02-03"))
  release_date <- as.Date("2021-02-04")
  batched_tbl  <- simulate_batch(clean_tbl, closed_dates = closed, verbose = FALSE)

  batched_result <- batch_shape_test(batched_tbl, at = release_date, guard = 3L,
                                     n_permutations = 199L, seed = 1L)
  clean_result   <- batch_shape_test(clean_tbl, at = release_date, guard = 3L,
                                     n_permutations = 199L, seed = 1L)

  expect_gt(batched_result$mean_delay_at, batched_result$mean_delay_reference)
  expect_lt(batched_result$p_value, 0.05)
  expect_gt(clean_result$p_value, 0.05)
})

test_that("batch_shape_test() rejects a report date that does not exist", {
  clean_tbl <- make_flat_linelist(n_origins = 30L)
  expect_error(
    batch_shape_test(clean_tbl, at = as.Date("1900-01-01")),
    "not one of the observed report dates"
  )
})

test_that("block permutation is available for overdispersed data", {
  clean_tbl   <- make_flat_linelist(n_origins = 70L, per_origin = 15L, seed = 5L)
  closed      <- as.Date(c("2021-02-01", "2021-02-02", "2021-02-03"))
  batched_tbl <- simulate_batch(clean_tbl, closed_dates = closed, verbose = FALSE)

  block_result <- batch_shape_test(batched_tbl, at = as.Date("2021-02-04"), guard = 3L,
                                   permute = "blocks", n_permutations = 199L, seed = 1L)
  expect_true(is.finite(block_result$p_value))
  expect_gte(block_result$p_value, 0)
  expect_lte(block_result$p_value, 1)
})

# -- count-cumulative ----------------------------------------------------------

test_that("count-cumulative data de-accumulates and screens with the robust null", {
  # Two event dates, cumulative totals re-reported over several report dates.
  cumulative_frame <- expand.grid(
    onset  = seq(as.Date("2021-01-01"), by = "day", length.out = 30L),
    report = seq(as.Date("2021-01-01"), by = "day", length.out = 30L)
  )
  cumulative_frame <- cumulative_frame[cumulative_frame$report >= cumulative_frame$onset, ]
  delay <- as.integer(cumulative_frame$report - cumulative_frame$onset)
  # A cumulative curve that saturates: 10 * (1 - 0.5^(delay+1)), rounded.
  cumulative_frame$total <- round(10 * (1 - 0.5^(delay + 1)))

  cumulative_tbl <- tbl_now(
    cumulative_frame,
    event_date = !!as.symbol("onset"), report_date = !!as.symbol("report"),
    case_count = !!as.symbol("total"), data_type = "count-cumulative", verbose = FALSE
  )

  screened <- batch_screen(cumulative_tbl, lookback = 2L)
  expect_s3_class(screened, "batch_screen")
  expect_equal(attr(screened, "null_model"), "robust")
  expect_true(all(c("delta", "deficit", "p_transport") %in% names(screened)))
})
