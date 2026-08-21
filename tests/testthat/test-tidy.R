# tidy() must return the SAME table whatever engine produced the fit. That is the
# entire point of the method, so the contract is asserted directly.

TIDY_COLUMNS <- c(
  "event_date", "stratum", "estimate", "conf.low", "conf.high", "level", "engine"
)

expect_tidy_contract <- function(out, engine) {
  expect_s3_class(out, "tbl_df")
  expect_equal(names(out)[seq_along(TIDY_COLUMNS)], TIDY_COLUMNS)
  expect_s3_class(out$event_date, "Date")
  expect_type(out$stratum, "character")
  expect_type(out$estimate, "double")
  expect_type(out$conf.low, "double")
  expect_type(out$conf.high, "double")
  expect_type(out$level, "double")
  expect_equal(unique(out$engine), engine)
  expect_gt(nrow(out), 0L)
}

tidy_test_tbl_now <- function() {
  data(denguedat, envir = environment())
  denguedat |>
    dplyr::filter(
      onset_week  < as.Date("2002-07-15"),
      report_week < as.Date("2002-07-15"),
      onset_week >= as.Date("2000-01-01")
    ) |>
    tbl_now(
      event_date = onset_week, report_date = report_week,
      data_type = "linelist", verbose = FALSE
    )
}

test_that("tidy() on a baselinenowcast fit meets the contract", {
  skip_if_not_installed("baselinenowcast")
  x <- tidy_test_tbl_now()
  triangle <- suppressWarnings(tbl_now_to_baselinenowcast(x, verbose = FALSE))
  fit <- suppressWarnings(suppressMessages(
    baselinenowcast::baselinenowcast(triangle, output_type = "samples", draws = 100)
  ))
  expect_tidy_contract(tidy(fit), "baselinenowcast")
})

test_that("tidy() adds a q* column per requested probability", {
  skip_if_not_installed("baselinenowcast")
  x <- tidy_test_tbl_now()
  triangle <- suppressWarnings(tbl_now_to_baselinenowcast(x, verbose = FALSE))
  fit <- suppressWarnings(suppressMessages(
    baselinenowcast::baselinenowcast(triangle, output_type = "samples", draws = 100)
  ))
  out <- tidy(fit, probs = c(0.05, 0.5, 0.95))

  expect_true(all(c("q5", "q50", "q95") %in% names(out)))
  # A non-integer probability keeps its decimal in the name.
  expect_true("q2.5" %in% names(tidy(fit, probs = 0.025)))
  # Quantiles must be ordered.
  expect_true(all(out$q5 <= out$q95))
})

test_that("tidy() refuses `probs` for engines that keep no draws", {
  skip_if_not_installed("NobBS")
  # A bare list carrying NobBS's shape, so no model has to be fitted.
  fake_nobbs <- list(estimates = data.frame(
    onset_date = as.Date(c("2002-07-01", "2002-07-08")),
    estimate = c(10, 8), lower = c(6, 2), upper = c(15, 19)
  ))
  expect_tidy_contract(tidy(fake_nobbs), "NobBS")
  expect_error(tidy(fake_nobbs, probs = 0.5), "does not keep posterior draws")
})

test_that("tidy() tells NobBS and nowcaster apart by structure", {
  fake_nobbs <- list(estimates = data.frame(
    onset_date = as.Date("2002-07-01"),
    estimate = 10, lower = 6, upper = 15
  ))
  fake_nowcaster <- list(total = data.frame(
    dt_event = as.Date("2002-07-01"), Median = 10, LI = 6, LS = 15
  ))
  expect_equal(unique(tidy(fake_nobbs)$engine), "NobBS")
  expect_equal(unique(tidy(fake_nowcaster)$engine), "nowcaster")

  # An unrecognisable list must say so, and point at the way out.
  expect_error(tidy(list(a = 1)), "Supply")
  # ... and an explicit `engine` overrides the sniffing.
  expect_equal(unique(tidy(fake_nowcaster, engine = "nowcaster")$engine), "nowcaster")
})

test_that("tidy() records the interval width each engine actually returns", {
  # epinowcast's default band is q5-q95, i.e. 90% -- not the 95% the others use.
  # Recording it is what stops a 90% band being compared with a 95% one.
  fake_nowcaster <- list(total = data.frame(
    dt_event = as.Date("2002-07-01"), Median = 10, LI = 6, LS = 15
  ))
  expect_equal(unique(tidy(fake_nowcaster)$level), 0.95)
})
