# Fractional delays, issue #63.
#
# A calendar has no half-days, so `2.5` had to become something. It used to
# become `round()` -- and `round()` is round-half-to-even, so 2.5 went DOWN and
# 3.5 went UP, silently, while the numeric axis refused the same value with a
# clear error. The package now refuses everywhere it CREATES a delay, and warns
# when it finds one on an object.

daily_fixture <- function() {
  tbl_now(
    data.frame(
      onset = as.Date("2020-01-01") + 0:3,
      reported = as.Date("2020-01-01") + c(1, 2, 3, 40)
    ),
    event_date = "onset", report_date = "reported",
    data_type = "linelist", units = "days", verbose = FALSE
  )
}

numeric_fixture <- function() {
  tbl_now(
    data.frame(e = 1:5, r = c(2L, 3L, 4L, 5L, 40L)),
    event_date = "e", report_date = "r", units = "numeric", verbose = FALSE
  )
}

validation_fixture_days <- function() {
  tbl_now(
    data.frame(
      onset = as.Date("2021-01-04") + 0:3,
      visit = as.Date("2021-01-05") + 0:3,
      result = as.Date("2021-01-05") + 0:3 + c(1, 2, 1, 90),
      outcome = rep("confirmed", 4)
    ),
    event_date = "onset", report_date = "visit",
    validation_date = "result", validation_type = "outcome",
    data_type = "linelist", units = "days", verbose = FALSE
  )
}

# Refused where a delay is created -------------------------------------------

test_that("censor_reporting_delays() refuses a fractional to_delay on a calendar axis", {
  x <- daily_fixture()

  # The two halves that used to disagree with each other.
  expect_error(
    censor_reporting_delays(x, .delay > 10, to_delay = 2.5, verbose = FALSE),
    "whole number"
  )
  expect_error(
    censor_reporting_delays(x, .delay > 10, to_delay = 3.5, verbose = FALSE),
    "whole number"
  )

  # The delays are 1, 1, 1 and 37; a whole number still works, and lands
  # where it says.
  expect_equal(x[[".delay"]], c(1, 1, 1, 37))
  capped <- censor_reporting_delays(x, .delay > 10, to_delay = 2, verbose = FALSE)
  expect_equal(capped[[".delay"]], c(1, 1, 1, 2))
})

test_that("censor_reporting_delays() still refuses one on a numeric axis", {
  # This axis always refused; the point of #63 is that the calendar one now
  # agrees with it rather than rounding.
  expect_error(
    censor_reporting_delays(numeric_fixture(), .delay > 10, to_delay = 2.5,
      verbose = FALSE
    ),
    "whole number"
  )
})

test_that("censor_validation_delays() refuses a fractional to_delay", {
  x <- validation_fixture_days()

  expect_error(
    censor_validation_delays(x, .validation_delay > 30, to_delay = 1.5,
      verbose = FALSE
    ),
    "whole number"
  )

  capped <- suppressMessages(
    censor_validation_delays(x, .validation_delay > 30, to_delay = 2)
  )
  expect_equal(capped[[".validation_delay"]], c(1, 2, 1, 2))
})

test_that("tbl_now(delay =) refuses a fractional delay column", {
  frame <- data.frame(
    onset = as.Date("2020-01-01") + 0:3,
    lag = c(1, 2.5, 3, 4)
  )
  expect_error(
    tbl_now(frame, event_date = onset, delay = lag, units = "days",
      verbose = FALSE
    ),
    "whole number"
  )

  # Reconstructing the EVENT date from the report is the same helper, so it is
  # refused the same way.
  backwards <- data.frame(
    reported = as.Date("2020-01-10") + 0:3,
    lag = c(1, 2, 3.5, 4)
  )
  expect_error(
    tbl_now(backwards, report_date = reported, delay = lag, units = "days",
      verbose = FALSE
    ),
    "whole number"
  )

  # Whole numbers reconstruct exactly, with no rounding to hide behind.
  built <- tbl_now(
    data.frame(onset = as.Date("2020-01-01") + 0:3, lag = c(1, 2, 3, 4)),
    event_date = onset, delay = lag, units = "days", verbose = FALSE
  )
  expect_equal(built[[".delay"]], c(1, 2, 3, 4))
})

test_that("a grouped object is refused the same way", {
  x <- tbl_now(
    data.frame(
      onset = as.Date("2020-01-01") + rep(0:3, 2),
      reported = as.Date("2020-01-01") + rep(c(1, 2, 3, 40), 2),
      sex = rep(c("F", "M"), each = 4)
    ),
    event_date = "onset", report_date = "reported", strata = "sex",
    data_type = "linelist", units = "days", verbose = FALSE,
    warn_non_uniqueness = FALSE
  )

  expect_error(
    censor_reporting_delays(dplyr::group_by(x, sex), .delay > 10,
      to_delay = 2.5, verbose = FALSE
    ),
    "whole number"
  )

  # And a whole one goes through, grouping intact.
  capped <- censor_reporting_delays(dplyr::group_by(x, sex), .delay > 10,
    to_delay = 2, verbose = FALSE
  )
  expect_true(is_tbl_now(capped))
  expect_equal(dplyr::group_vars(capped), "sex")
  expect_equal(capped[[".delay"]], rep(c(1, 1, 1, 2), 2))
})

# Reported on an object that already has one ---------------------------------

misaligned_weeks <- function() {
  # Two weekly columns on different weekdays: every delay is 3/7 of a week.
  # This is the ONLY way a fractional `.delay` can still get into an object,
  # and `align_weeks()` is what fixes it.
  data.frame(
    onset = as.Date("2024-01-07") + 7 * 0:4,
    reported = as.Date("2024-01-10") + 7 * 0:4,
    n = c(1L, 2L, 4L, 8L, 16L)
  )
}

test_that("validate_tbl_now() warns about a fractional .delay", {
  expect_warning(
    x <- tbl_now(misaligned_weeks(),
      event_date = "onset", report_date = "reported", case_count = "n",
      data_type = "count-incidence", units = "weeks", verbose = FALSE
    ),
    "fractional"
  )
  expect_warning(validate_tbl_now(x), "fractional `.delay`")

  # It is a WARNING and not an error on purpose: `align_weeks()` needs to be
  # handed exactly this object, so refusing to build it would remove the fix.
  aligned <- align_weeks(x)
  expect_true(is_tbl_now(aligned))
  expect_true(all(aligned[[".delay"]] == round(aligned[[".delay"]])))
  expect_no_warning(validate_tbl_now(aligned))

  # `tbl_now(align_weeks = TRUE)` reaches the same place, which it could not
  # do if the validator aborted first.
  expect_warning(
    inline <- tbl_now(misaligned_weeks(),
      event_date = "onset", report_date = "reported", case_count = "n",
      data_type = "count-incidence", units = "weeks", align_weeks = TRUE,
      verbose = FALSE
    ),
    "fractional"
  )
  expect_true(all(inline[[".delay"]] == round(inline[[".delay"]])))
})

test_that("the fractional finding is the same one diagnose() reports", {
  x <- suppressWarnings(tbl_now(misaligned_weeks(),
    event_date = "onset", report_date = "reported", case_count = "n",
    data_type = "count-incidence", units = "weeks", verbose = FALSE
  ))

  findings <- suppressWarnings(diagnose(x))
  row <- findings[findings$check == "units" & findings$scope == "delay", ]

  expect_equal(nrow(row), 1L)
  expect_equal(as.character(row$status), "warning")
  expect_equal(row$n_affected, 5L)

  # A clean object reports the same row as `ok`, not as absent: a check that
  # cannot be seen is not a check.
  clean <- suppressWarnings(diagnose(align_weeks(x)))
  clean_row <- clean[clean$check == "units" & clean$scope == "delay", ]
  expect_equal(as.character(clean_row$status), "ok")
})

test_that("validate_tbl_now() still does not run the expensive grid checks", {
  # The `units` block is in the validator's list for the fractional delay
  # alone. Its two grid checks cost a pass over the date columns and have
  # never been more than notes, so they stay in `diagnose()`.
  x <- suppressWarnings(tbl_now(misaligned_weeks(),
    event_date = "onset", report_date = "reported", case_count = "n",
    data_type = "count-incidence", units = "weeks", verbose = FALSE
  ))

  deep <- suppressWarnings(diagnose(x))
  expect_true("report_grid" %in% deep$scope)

  shallow <- suppressWarnings(tbl.now:::.tbl_now_findings(
    x,
    checks = tbl.now:::.diagnose_validation_checks(),
    by_strata = FALSE, warn_non_uniqueness = FALSE, warn_now = TRUE,
    floor = "note", deep = FALSE, assert = FALSE, fn = "validate_tbl_now"
  ))
  expect_true("delay" %in% shallow$scope)
  expect_false("report_grid" %in% shallow$scope)
  expect_false("event_grid" %in% shallow$scope)
})
