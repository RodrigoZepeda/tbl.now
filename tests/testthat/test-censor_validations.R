# `censor_validations()` and `censor_validation_delays()` are the validation-axis
# twins of `censor_reports()` and `censor_reporting_delays()`. The asymmetry
# worth testing is `"pending"`: it means reported and STILL WAITING, so a pending
# case has no validation date, and writing one would assert a resolution that
# never happened.

library(dplyr, quietly = TRUE, warn.conflicts = FALSE)

validated <- function() {
  cases <- data.frame(
    onset = as.Date("2021-01-04") + 0:4,
    visit = as.Date("2021-01-05") + 0:4,
    # row 3 pending (no date); row 5 resolved but its date was never recorded
    result = as.Date(c("2021-01-08", "2021-01-09", NA, "2021-04-10", NA)),
    outcome = c("confirmed", "confirmed", "pending", "confirmed", "confirmed"),
    sex = c("F", "M", "F", "M", "F"),
    stringsAsFactors = FALSE
  )
  tbl_now(cases,
    event_date = onset, report_date = visit,
    validation_date = result, validation_type = outcome, strata = sex,
    data_type = "linelist", units = "days", verbose = FALSE
  )
}

# ---- Wrong inputs -----------------------------------------------------------

test_that("the validation verbs refuse a non-tbl_now", {
  df <- data.frame(a = 1)
  expect_error(censor_validations(df, a > 0), "must be a <tbl_now>")
  expect_error(censor_validation_delays(df, a > 0), "must be a <tbl_now>")
})

test_that("the validation verbs need a validation process", {
  plain <- tbl_now(
    data.frame(
      onset = as.Date("2020-01-01") + 0:3,
      reported = as.Date("2020-01-02") + 0:3
    ),
    event_date = onset, report_date = reported,
    data_type = "linelist", units = "days", verbose = FALSE
  )
  expect_false(has_validation(plain))
  expect_error(censor_validations(plain, TRUE), "needs a validation process")
  expect_error(censor_validation_delays(plain, TRUE), "needs a validation process")
})

test_that("the condition is required and must be logical", {
  x <- validated()
  expect_error(censor_validations(x), "condition")
  expect_error(censor_validation_delays(x), "condition")
  expect_error(censor_validations(x, result), "logical")
  expect_error(censor_validations(x, c(TRUE, FALSE)), "length 1 or 5")
})

test_that("a replacement of the wrong type or length is refused", {
  x <- validated()
  expect_error(
    censor_validations(x, is.na(result), to_validation = 100),
    "must be a <Date>"
  )
  expect_error(
    censor_validations(x, is.na(result), to_validation = as.Date("2021-05-01") + 0:1),
    "length 1 or 5"
  )
  expect_error(
    censor_validation_delays(x, TRUE, to_delay = "ten"), "must be a number"
  )
  expect_error(
    censor_validation_delays(x, TRUE, to_delay = c(1, 2)), "length 1 or 5"
  )
})

# ---- The pending rule -------------------------------------------------------

test_that("censor_validations skips pending cases and says so", {
  x <- validated()

  expect_warning(
    out <- censor_validations(x, is.na(result),
      to_validation = as.Date("2021-05-01"), verbose = TRUE
    ),
    "pending"
  )

  # Row 3 is pending: no date written, not flagged.
  expect_true(is.na(out[[get_validation_date(out)]][3]))
  expect_false(out[[get_is_censored_validation(out)]][3])

  # Row 5 was resolved and merely undated: it gets the bound and the flag.
  expect_equal(out[[get_validation_date(out)]][5], as.Date("2021-05-01"))
  expect_true(out[[get_is_censored_validation(out)]][5])

  # Nothing else moved, and no outcome was rewritten.
  expect_equal(
    out[[get_validation_date(out)]][c(1, 2, 4)],
    x[[get_validation_date(x)]][c(1, 2, 4)]
  )
  expect_equal(out[["outcome"]], x[["outcome"]])
})

test_that("flagging without a replacement does not skip pending cases", {
  x <- validated()
  # No date is written, so nothing contradicts `pending` and the guard is off.
  out <- expect_no_warning(
    censor_validations(x, is.na(result), to_validation = NULL, verbose = FALSE)
  )
  expect_equal(
    out[[get_is_censored_validation(out)]],
    c(FALSE, FALSE, TRUE, FALSE, TRUE)
  )
  expect_true(is.na(out[[get_validation_date(out)]][3]))
})

test_that("censor_validation_delays skips pending cases too", {
  x <- validated()
  expect_warning(
    out <- censor_validation_delays(x, TRUE, to_delay = 1),
    "pending"
  )
  expect_true(is.na(out[[get_validation_date(out)]][3]))
  expect_false(out[[get_is_censored_validation(out)]][3])
})

test_that("a selection with no pending rows warns about nothing", {
  x <- validated()
  expect_no_warning(
    censor_validations(x, !is.na(result) & result > as.Date("2021-03-01"),
      to_validation = as.Date("2021-05-01"), verbose = FALSE
    )
  )
})

# ---- Results worked out by hand ---------------------------------------------

test_that("censor_validation_delays caps the turnaround from the REPORT date", {
  x <- validated()
  # Row 4: reported 2021-01-08, resolved 2021-04-10 -> 92 days.
  expect_equal(x$.validation_delay[4], 92)

  out <- censor_validation_delays(x, .validation_delay > 10,
    to_delay = 10, verbose = FALSE
  )

  expect_equal(out$.validation_delay[4], 10)
  # 10 days after the REPORT (2021-01-08), not after the event.
  expect_equal(out[[get_validation_date(out)]][4], as.Date("2021-01-18"))
  expect_equal(
    out[[get_is_censored_validation(out)]],
    c(FALSE, FALSE, FALSE, TRUE, FALSE)
  )
})

test_that("censor_validations defaults the replacement to `now`", {
  x <- validated()
  out <- suppressWarnings(
    censor_validations(x, is.na(result), verbose = FALSE)
  )
  expect_equal(out[[get_validation_date(out)]][5], get_now(x))
})

test_that("a replacement after `now` drags `now` forward, never back", {
  x <- validated()
  out <- suppressWarnings(censor_validations(x, is.na(result),
    to_validation = as.Date("2022-01-01"), verbose = FALSE
  ))
  expect_equal(get_now(out), as.Date("2022-01-01"))

  earlier <- censor_validation_delays(x, .validation_delay > 10,
    to_delay = 1, verbose = FALSE
  )
  expect_equal(get_now(earlier), get_now(x))
})

test_that("existing validation flags are merged, never cleared", {
  x <- suppressMessages(censor_validation_delays_above(validated(), 10))
  already <- x[[".is_censored_validation"]]
  expect_true(any(already))

  out <- censor_validation_delays(x, .validation_delay > 1e6, verbose = FALSE)
  expect_equal(out[[".is_censored_validation"]], already)
})

test_that("the two censoring axes stay independent", {
  x <- validated()
  # Flag only on the report axis: moving a report date past a validation date
  # would be a separate (real) complaint from the validator, and this test is
  # about the two flags not treading on each other.
  both <- censor_reports(
    censor_validation_delays(x, .validation_delay > 10, verbose = FALSE),
    .delay > 0,
    to_report = NULL, verbose = FALSE
  )
  expect_equal(get_is_censored_report(both), ".is_censored_report")
  expect_equal(get_is_censored_validation(both), ".is_censored_validation")
  expect_true(any(both[[".is_censored_report"]]))
  expect_true(any(both[[".is_censored_validation"]]))
})

test_that("validation censoring leaves the report axis untouched", {
  x <- validated()
  out <- censor_validation_delays(x, .validation_delay > 10,
    to_delay = 10, verbose = FALSE
  )
  expect_equal(out[[get_report_date(out)]], x[[get_report_date(x)]])
  expect_equal(out$.delay, x$.delay)
  expect_null(get_is_censored_report(out))
})

# ---- Grouped objects --------------------------------------------------------

test_that("censor_validations works on a grouped tbl_now", {
  x <- validated()
  ungrouped <- suppressWarnings(
    censor_validations(x, is.na(result), verbose = FALSE)
  )

  out <- suppressWarnings(
    censor_validations(x |> group_by(sex), is.na(result), verbose = FALSE)
  )

  expect_true(is_tbl_now(out))
  expect_equal(dplyr::group_vars(out), "sex")
  expect_equal(as_tibble(ungroup(out)), as_tibble(ungroup(ungrouped)))
})

test_that("censor_validation_delays works on a grouped tbl_now", {
  x <- validated()
  ungrouped <- censor_validation_delays(x, .validation_delay > 10,
    to_delay = 10, verbose = FALSE
  )

  out <- censor_validation_delays(x |> group_by(sex), .validation_delay > 10,
    to_delay = 10, verbose = FALSE
  )

  expect_true(is_tbl_now(out))
  expect_equal(dplyr::group_vars(out), "sex")
  expect_equal(as_tibble(ungroup(out)), as_tibble(ungroup(ungrouped)))

  # Grouping by a column the verb does not care about changes nothing either.
  by_other <- censor_validation_delays(
    x |> group_by(!!as.symbol("outcome")), .validation_delay > 10,
    to_delay = 10, verbose = FALSE
  )
  expect_equal(as_tibble(ungroup(by_other)), as_tibble(ungroup(ungrouped)))
})

# ---- Messages ---------------------------------------------------------------

test_that("the validation verbs report what they did unless silenced", {
  x <- validated()
  expect_message(
    censor_validation_delays(x, .validation_delay > 10, verbose = TRUE),
    "Censored"
  )
  expect_silent(
    censor_validation_delays(x, .validation_delay > 10, verbose = FALSE)
  )
})
