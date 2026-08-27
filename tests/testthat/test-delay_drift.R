# Tests for plot_delay_drift() and diagnose_drift().

# Daily count-incidence tbl_now whose mean reporting delay rises with `slope`
# per day (slope = 0 is stationary).
make_drift_now <- function(slope = 0, from = "2018-01-01", to = "2020-06-30",
                           strata = FALSE, seed = 1) {
  set.seed(seed)
  dates <- seq(as.Date(from), as.Date(to), by = "day")
  build <- function(g, g_offset = 0) {
    do.call(rbind, lapply(seq_along(dates), function(i) {
      delay <- stats::rpois(1, lambda = 1 + g_offset + slope * i)
      data.frame(event_date = dates[i], report_date = dates[i] + delay,
                 n = 1L, grp = g)
    }))
  }
  rows <- if (strata) rbind(build("a", 0), build("b", 1)) else build("a", 0)
  args <- list(rows,
    event_date = quote(event_date), report_date = quote(report_date),
    case_count = quote(n), data_type = "count-incidence",
    event_units = "days", report_units = "days", verbose = FALSE
  )
  if (strata) args$strata <- "grp"
  do.call(tbl_now, args)
}

# --- plot_delay_drift() -----------------------------------------------------

test_that("plot_delay_drift returns a ggplot that builds", {
  skip_if_not_installed("ggplot2")

  p <- plot_delay_drift(make_drift_now(slope = 0.003))
  expect_s3_class(p, "ggplot")
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  expect_equal(p$labels$y, "Reporting delay (days)")
})

test_that("plot_delay_drift honours window/step and by_strata", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  nowobj <- make_drift_now(slope = 0.002, strata = TRUE)
  p <- plot_delay_drift(nowobj, window = 30, step = 7, by_strata = TRUE)
  expect_s3_class(p, "ggplot")
  # faceted by stratum
  expect_true(inherits(p$facet, "FacetWrap"))
})

test_that("plot_delay_drift validates its input", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  expect_error(plot_delay_drift(data.frame(a = 1)), "tbl_now")
  expect_error(plot_delay_drift(make_drift_now(), by_strata = TRUE), "strata")
})

# --- diagnose_drift() -----------------------------------------------------

test_that("diagnose_drift returns one tidy row per stat", {
  skip_if_not_installed("modifiedmk")

  res <- diagnose_drift(make_drift_now(slope = 0.003), stat = c("median", "spread"))
  expect_s3_class(res, "tbl_df")
  expect_setequal(res$stat, c("median", "spread"))
  expect_true(all(c(
    "strata", "stat", "n", "tau", "sens_slope", "statistic", "p_value",
    "method", "drift"
  ) %in% names(res)))
})

test_that("diagnose_drift detects a real upward drift", {
  skip_on_cran()
  skip_if_not_installed("modifiedmk")

  res <- diagnose_drift(
    make_drift_now(slope = 0.004), stat = "mean", mature_only = FALSE
  )
  expect_true(res$drift)
  expect_gt(res$sens_slope, 0)   # positive trend
  expect_lt(res$p_value, 0.05)
})

test_that("diagnose_drift does not flag a stationary series", {
  skip_on_cran()
  skip_if_not_installed("modifiedmk")

  # Flat mean delay (slope 0); Hamed-Rao correction should keep this null.
  res <- diagnose_drift(
    make_drift_now(slope = 0, seed = 7), stat = "median", mature_only = FALSE
  )
  expect_false(res$drift)
})

test_that("diagnose_drift runs per stratum", {
  skip_on_cran()
  skip_if_not_installed("modifiedmk")

  res <- diagnose_drift(
    make_drift_now(slope = 0.003, strata = TRUE),
    stat = "median", by_strata = TRUE, mature_only = FALSE
  )
  expect_setequal(res$strata, c("a", "b"))
})

test_that("diagnose_drift supports the block-bootstrap method", {
  skip_on_cran()
  skip_if_not_installed("modifiedmk")

  res <- diagnose_drift(
    make_drift_now(slope = 0.004), stat = "mean",
    method = "block-bootstrap", mature_only = FALSE, nsim = 200
  )
  expect_equal(res$method, "block-bootstrap")
  expect_false(is.na(res$p_value))
})

test_that("diagnose_drift errors without modifiedmk installed", {
  # Only meaningful when the package is absent; skip when it is present.
  skip_if(requireNamespace("modifiedmk", quietly = TRUE))
  expect_error(diagnose_drift(make_drift_now()), "modifiedmk")
})

# --- change-point detection -------------------------------------------------

# Daily tbl_now whose mean delay jumps from `before` to `after` at `at`.
make_step_now <- function(before = 1, after = 5, at = "2019-04-01",
                          from = "2018-01-01", to = "2020-06-30", seed = 1) {
  set.seed(seed)
  dates <- seq(as.Date(from), as.Date(to), by = "day")
  at <- as.Date(at)
  rows <- do.call(rbind, lapply(dates, function(d) {
    lambda <- if (d < at) before else after
    data.frame(event_date = d, report_date = d + stats::rpois(1, lambda), n = 1L)
  }))
  tbl_now(rows,
    event_date = event_date, report_date = report_date, case_count = n,
    data_type = "count-incidence", event_units = "days", report_units = "days",
    verbose = FALSE
  )
}

test_that(".tbl_now_pettitt locates a clear step and guards edge cases", {
  skip_on_cran()
  found <- tbl.now:::.tbl_now_pettitt(c(rep(0, 20), rep(5, 20)))
  expect_equal(found$index, 20)
  expect_lt(found$p_value, 0.05)

  # constant series -> NA (no variance)
  expect_true(is.na(tbl.now:::.tbl_now_pettitt(rep(1, 20))$p_value))
  # too short -> NA
  expect_true(is.na(tbl.now:::.tbl_now_pettitt(1:5)$p_value))
})

test_that("diagnose_changepoint returns one tidy row per stat", {
  res <- diagnose_changepoint(make_step_now(), stat = c("median", "mean"))
  expect_s3_class(res, "tbl_df")
  expect_setequal(res$stat, c("median", "mean"))
  expect_true(all(c(
    "strata", "stat", "n", "changepoint", "statistic", "p_value",
    "before", "after", "shift", "changepoint_detected"
  ) %in% names(res)))
})

test_that("diagnose_changepoint detects an abrupt upward shift near the truth", {
  skip_on_cran()
  res <- diagnose_changepoint(
    make_step_now(before = 1, after = 5, at = "2019-04-01"),
    stat = "mean", mature_only = FALSE
  )
  expect_true(res$changepoint_detected)
  expect_gt(res$shift, 0)                       # delay increased
  expect_lt(abs(as.numeric(res$changepoint - as.Date("2019-04-01"))), 21)
})

test_that("diagnose_changepoint runs per stratum", {
  skip_on_cran()
  nowobj <- make_drift_now(slope = 0.003, strata = TRUE)
  res <- diagnose_changepoint(nowobj, stat = "median", by_strata = TRUE)
  expect_setequal(res$strata, c("a", "b"))
})

test_that("plot_delay_drift(changepoint = TRUE) adds a marker layer and validates", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  p <- plot_delay_drift(make_step_now(), window = 30, step = 7, changepoint = TRUE)
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  # one more layer than without the marker
  p0 <- plot_delay_drift(make_step_now(), window = 30, step = 7)
  expect_gt(length(p$layers), length(p0$layers))

  expect_error(plot_delay_drift(make_step_now(), changepoint = "yes"), "changepoint")
})

# --- internal helpers -------------------------------------------------------

test_that("per-period series has one row per event date with spread columns", {
  skip_on_cran()
  nowobj <- make_drift_now(slope = 0.002)
  dl <- tbl.now:::.tbl_now_delay_long(nowobj)
  series <- tbl.now:::.tbl_now_delay_period_series(dl)
  expect_true(all(c("strata", "event_date", "median", "mean", "iqr", "spread")
    %in% names(series)))
  expect_equal(nrow(series), dplyr::n_distinct(dl$event_date))
})

test_that("maturity threshold drops the immature recent tail", {
  skip_on_cran()
  nowobj <- make_drift_now(slope = 0.002)
  dl <- tbl.now:::.tbl_now_delay_long(nowobj)
  thr <- tbl.now:::.tbl_now_maturity_threshold(nowobj, dl, level = 0.95)
  expect_true(lubridate::is.Date(thr))
  expect_lt(thr, max(dl$event_date))
})
