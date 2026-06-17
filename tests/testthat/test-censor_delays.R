library(dplyr, quietly = TRUE, warn.conflicts = FALSE)

make_delay_data <- function(is_censored = FALSE) {
  df <- data.frame(
    onset    = as.Date("2020-01-01") + c(0, 0, 1, 2),
    reported = as.Date("2020-01-01") + c(1, 5, 2, 300)   # last row: 298-day delay
  )
  if (is_censored) {
    df$flag <- c(TRUE, FALSE, FALSE, FALSE)
    tbl_now(df, event_date = onset, report_date = reported, is_censored = flag,
            data_type = "linelist", verbose = FALSE)
  } else {
    tbl_now(df, event_date = onset, report_date = reported,
            data_type = "linelist", verbose = FALSE)
  }
}

test_that("censor_delays_above flags long delays and creates the column", {
  out <- censor_delays_above(make_delay_data(), max_delay = 60, quiet = TRUE)

  expect_true(is_tbl_now(out))
  expect_equal(get_is_censored(out), ".is_censored")
  # only the 298-day delay exceeds 60
  expect_equal(out[[".is_censored"]], c(FALSE, FALSE, FALSE, TRUE))
})

test_that("censor_delays_above merges with existing censoring (never un-censors)", {
  out <- censor_delays_above(make_delay_data(is_censored = TRUE),
                             max_delay = 60, quiet = TRUE)
  expect_equal(get_is_censored(out), "flag")
  # row 1 was already censored; row 4 newly censored; both stay TRUE
  expect_equal(out[["flag"]], c(TRUE, FALSE, FALSE, TRUE))
})

test_that("censor_delays_above emits an informative message unless quiet", {
  expect_message(censor_delays_above(make_delay_data(), max_delay = 60),
                 "censored")
  expect_silent(censor_delays_above(make_delay_data(), max_delay = 60, quiet = TRUE))
})

test_that("censor_delays_above flags nothing when max_delay is large", {
  out <- censor_delays_above(make_delay_data(), max_delay = 1000, quiet = TRUE)
  expect_false(any(out[[".is_censored"]]))
})

test_that("censor_delays_above works on count data via the .delay column", {
  d <- tibble(
    event  = as.Date("2020-01-01") + c(0, 0, 7),
    report = as.Date("2020-01-01") + c(0, 70, 7),   # middle row: 10-week delay
    n      = c(3, 1, 4)
  )
  tn <- tbl_now(d, event_date = event, report_date = report, case_count = n,
                data_type = "count-incidence", event_units = "weeks",
                report_units = "weeks", verbose = FALSE)
  out <- censor_delays_above(tn, max_delay = 4, quiet = TRUE)   # > 4 weeks
  expect_true(is_tbl_now(out))
  expect_equal(sum(out[[get_is_censored(out)]]), 1L)
})

test_that("censor_delays_above validates its arguments", {
  expect_error(censor_delays_above(data.frame(a = 1), max_delay = 5), "tbl_now")
  expect_error(censor_delays_above(make_delay_data(), max_delay = -1), "max_delay")
  expect_error(censor_delays_above(make_delay_data(), max_delay = c(1, 2)), "max_delay")
})
