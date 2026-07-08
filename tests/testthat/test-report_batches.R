# Tests for detect_report_batches() / plot_report_batches().

# A daily tbl_now with three regimes:
#   * baseline reporting with short delays,
#   * an epidemic peak (high volume, still short delays) -> must NOT be a batch,
#   * one batch dump on `batch_report` (many old event dates, long delays)
#     -> must be a batch.
make_batch_now <- function(seed = 42) {
  set.seed(seed)
  days <- seq(as.Date("2020-01-01"), as.Date("2020-06-30"), by = "day")
  rows <- list()
  for (i in seq_along(days)) {
    d <- days[i]
    base <- if (d >= as.Date("2020-03-01") & d <= as.Date("2020-03-31")) 45 else 6
    m <- stats::rpois(1, base)
    if (m > 0) {
      rows[[length(rows) + 1]] <- data.frame(
        event_date = d, report_date = d + stats::rpois(m, 1), n = 1
      )
    }
  }
  normal <- do.call(rbind, rows)
  batch_days <- seq(as.Date("2020-05-01"), as.Date("2020-05-21"), by = "day")
  batch <- data.frame(
    event_date = rep(batch_days, each = 8),
    report_date = as.Date("2020-05-22"), n = 1
  )
  agg <- stats::aggregate(n ~ event_date + report_date, rbind(normal, batch), sum)
  tbl_now(agg,
    event_date = event_date, report_date = report_date, case_count = n,
    data_type = "count-incidence", event_units = "days", report_units = "days",
    verbose = FALSE
  )
}

# --- discrimination: batch vs epidemic peak ---------------------------------

test_that("a batch dump is flagged but an epidemic peak is not", {
  res <- detect_report_batches(make_batch_now(), signals = c("volume", "delay"))

  # the batch report date is flagged
  expect_true(res$batch[res$report_date == as.Date("2020-05-22")])

  # none of the epidemic-peak (March) report dates are flagged
  march <- res[res$report_date >= as.Date("2020-03-03") &
    res$report_date <= as.Date("2020-03-28"), ]
  expect_equal(sum(march$batch), 0)
})

test_that("requiring the delay signal is what excludes the epidemic peak", {
  nowobj <- make_batch_now()
  # Volume alone flags the batch AND (potentially) peak dates; the delay signal
  # keeps the peak out. Check the peak dates' delay score stays low.
  res <- detect_report_batches(nowobj, signals = c("volume", "delay"))
  march <- res[res$report_date >= as.Date("2020-03-03") &
    res$report_date <= as.Date("2020-03-28"), ]
  expect_lt(mean(march$score_delay, na.rm = TRUE), 3)
  # the batch's delay score is large
  batch_row <- res[res$report_date == as.Date("2020-05-22"), ]
  expect_gt(batch_row$score_delay, 3)
})

# --- API / options ----------------------------------------------------------

test_that("detect_report_batches returns the documented columns", {
  res <- detect_report_batches(make_batch_now())
  expect_s3_class(res, "tbl_df")
  expect_true(all(c(
    "report_date", "n_reports", "n_event_dates", "mean_delay", "median_delay",
    "max_delay", "backlog", "score_volume", "score_delay", "score_span",
    "score_gap", "batch"
  ) %in% names(res)))
  # only real report dates (no zero-filled grid rows)
  expect_true(all(res$n_reports > 0))
})

test_that("the batch flag is the AND of the activated signals", {
  nowobj <- make_batch_now()
  n_vol <- sum(detect_report_batches(nowobj, signals = "volume")$batch)
  n_vd <- sum(detect_report_batches(nowobj, signals = c("volume", "delay"))$batch)
  n_all <- sum(detect_report_batches(
    nowobj, signals = c("volume", "delay", "span", "gap")
  )$batch)
  # more required signals -> no more flags (monotone)
  expect_gte(n_vol, n_vd)
  expect_gte(n_vd, n_all)
})

test_that("a higher threshold flags no more report dates", {
  nowobj <- make_batch_now()
  n_low <- sum(detect_report_batches(nowobj, threshold = 3)$batch)
  n_high <- sum(detect_report_batches(nowobj, threshold = 6)$batch)
  expect_gte(n_low, n_high)
  # the strong batch survives even a high threshold
  strong <- detect_report_batches(nowobj, threshold = 6)
  expect_true(strong$batch[strong$report_date == as.Date("2020-05-22")])
})

test_that("detect_report_batches validates its inputs", {
  expect_error(detect_report_batches(data.frame(a = 1)), "tbl_now")
  expect_error(detect_report_batches(make_batch_now(), signals = "nope"), NULL)
  expect_error(detect_report_batches(make_batch_now(), threshold = -1), "threshold")
})

test_that("detect_report_batches runs per stratum", {
  set.seed(1)
  days <- seq(as.Date("2021-01-01"), as.Date("2021-04-30"), by = "day")
  rows <- do.call(rbind, lapply(days, function(d) {
    do.call(rbind, lapply(c("a", "b"), function(g) {
      data.frame(event_date = d, report_date = d + stats::rpois(1, 1), n = 1, grp = g)
    }))
  }))
  agg <- stats::aggregate(n ~ event_date + report_date + grp, rows, sum)
  nowobj <- tbl_now(agg,
    event_date = event_date, report_date = report_date, case_count = n,
    strata = "grp", data_type = "count-incidence",
    event_units = "days", report_units = "days", verbose = FALSE
  )
  res <- detect_report_batches(nowobj, by_strata = TRUE)
  expect_setequal(unique(res$strata), c("a", "b"))
})

# --- plot -------------------------------------------------------------------

test_that("plot_report_batches builds without warnings and marks batches", {
  skip_if_not_installed("ggplot2")
  expect_no_warning({
    p <- plot_report_batches(make_batch_now())
    ggplot2::ggplot_build(p)
  })
  expect_s3_class(p, "ggplot")
})

# --- internal helpers -------------------------------------------------------

test_that("rolling median/MAD never yields a zero denominator", {
  v <- c(rep(5, 30), 100, rep(5, 30))
  rm <- tbl.now:::.tbl_now_rolling_median_mad(v, window = 15)
  expect_length(rm$median, length(v))
  expect_true(all(rm$mad > 0))
})

test_that("report-long helper carries report and event dates", {
  dl <- tbl.now:::.tbl_now_report_long(make_batch_now())
  expect_true(all(c("report_date", "event_date", "delay", "weight") %in% names(dl)))
  expect_true(all(dl$weight > 0))
})
