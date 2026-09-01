# `change_now()` and the validation process -- issue #51.
#
# Moving `now` BACKWARDS is what this verb is for: it is how a backtest asks
# "what did this data look like as of an earlier date". Before the fix, an
# object carrying a validation process refused every such move, because
# `validate_tbl_now()` correctly observed that a validation cannot post-date
# the as-of moment -- and the wrong conclusion was drawn from it. A validation
# dated after the new `now` has simply not happened yet.

# `as.list()` on a `tbl_now` carries the attributes along, and `now` is exactly
# the attribute these comparisons are meant to ignore.
bare_cols <- function(x) {
  out <- lapply(names(x), function(column) x[[column]])
  names(out) <- names(x)
  out
}

validation_fixture <- function() {
  cases <- data.frame(
    onset   = as.Date("2021-01-04") + 0:9,
    visit   = as.Date("2021-01-05") + 0:9,
    result  = as.Date("2021-01-08") + 0:9,
    outcome = rep(c("confirmed", "retracted"), 5),
    stringsAsFactors = FALSE
  )
  tbl_now(cases,
    event_date = "onset", report_date = "visit",
    validation_date = "result", validation_type = "outcome",
    data_type = "linelist", verbose = FALSE
  )
}

test_that("change_now() moves `now` backwards past a validation (#51)", {
  flu <- validation_fixture()
  expect_equal(get_now(flu), as.Date("2021-01-17"))

  # This is the reproducer from the issue, verbatim. It used to abort.
  earlier <- suppressMessages(suppressWarnings(
    change_now(flu, as.Date("2021-01-10"))
  ))

  expect_s3_class(earlier, "tbl_now")
  expect_equal(get_now(earlier), as.Date("2021-01-10"))
})

test_that("change_now() masks validations dated after the new `now`", {
  flu <- validation_fixture()
  earlier <- suppressMessages(suppressWarnings(
    change_now(flu, as.Date("2021-01-10"))
  ))

  future <- flu$result > as.Date("2021-01-10")
  expect_equal(sum(future), 7L)

  # Masked: no date, no delay, and back to the state a case is in before it is
  # resolved.
  expect_true(all(is.na(earlier$result[future])))
  expect_true(all(earlier$outcome[future] == "pending"))
  expect_true(all(is.na(earlier$.validation_num[future])))
  expect_true(all(is.na(earlier$.validation_delay[future])))

  # Untouched: everything that had already happened by then, INCLUDING the
  # validation dated exactly on the new `now` -- that one has happened.
  expect_equal(earlier$result[!future], flu$result[!future])
  expect_equal(earlier$outcome[!future], flu$outcome[!future])
  expect_equal(earlier$.validation_delay[!future], flu$.validation_delay[!future])
  expect_true(as.Date("2021-01-10") %in% earlier$result)
})

test_that("the re-censored object is valid, and the counts follow", {
  flu <- validation_fixture()
  earlier <- suppressMessages(suppressWarnings(
    change_now(flu, as.Date("2021-01-10"))
  ))

  # No error-level finding about the validation sitting after `now`: that is
  # the check that used to fire.
  findings <- suppressWarnings(diagnose(earlier))
  expect_equal(sum(findings$status == "error"), 0L)
  validation_row <- findings[
    findings$check == "now" & findings$scope == "validation_date",
  ]
  expect_equal(nrow(validation_row), 1L)
  expect_equal(as.character(validation_row$status), "ok")

  # As of 10 January only two of the five confirmations had come back.
  expect_equal(sum(get_latest_confirmed(flu)[["n"]]), 5)
  expect_equal(sum(suppressWarnings(get_latest_confirmed(earlier))[["n"]]), 2)
})

test_that("a whole backtest walk never errors (#51 regression guard)", {
  flu <- validation_fixture()

  # The loop the issue says users will write. Every one of these dates is
  # earlier than `max(validation_date)`, which is precisely the case that used
  # to abort, so a single failure anywhere fails the test.
  as_of_dates <- seq(as.Date("2021-01-05"), as.Date("2021-01-17"), by = "day")
  for (as_of in as_of_dates) {
    as_of <- as.Date(as_of, origin = "1970-01-01")
    snapshot <- expect_no_error(suppressMessages(suppressWarnings(
      change_now(flu, as_of)
    )))
    expect_equal(get_now(snapshot), as_of)
    # The invariant the validator enforces: nothing is known after `now`.
    expect_true(all(is.na(snapshot$result) | snapshot$result <= as_of))
  }
})

test_that("masking is idempotent and only ever looks backwards", {
  flu <- validation_fixture()
  once <- suppressMessages(suppressWarnings(change_now(flu, as.Date("2021-01-10"))))
  twice <- suppressMessages(suppressWarnings(change_now(once, as.Date("2021-01-10"))))
  expect_equal(bare_cols(twice), bare_cols(once))

  # Moving `now` forward has nothing to mask, so the data is untouched.
  later <- suppressMessages(change_now(flu, as.Date("2021-02-01")))
  expect_equal(bare_cols(later), bare_cols(flu))
  expect_equal(get_now(later), as.Date("2021-02-01"))
})

test_that("update_now() does not mask -- a validation is an observation", {
  flu <- validation_fixture()
  # `now` is inferred as the max over all three axes, so no validation can
  # post-date it and nothing is masked.
  refreshed <- suppressMessages(update_now(flu))
  expect_equal(get_now(refreshed), as.Date("2021-01-17"))
  expect_equal(bare_cols(refreshed), bare_cols(flu))
})

test_that("masking reports what it did, and `verbose = FALSE` silences it", {
  flu <- validation_fixture()

  expect_message(
    suppressWarnings(change_now(flu, as.Date("2021-01-10"))),
    "Returned 7 validations"
  )
  expect_no_message(
    suppressWarnings(change_now(flu, as.Date("2021-01-10"), verbose = FALSE))
  )
  # Nothing to mask means nothing to say.
  expect_no_message(change_now(flu, as.Date("2021-02-01")))
})

test_that("change_now() still works on an object with no validation", {
  plain <- tbl_now(
    data.frame(
      onset = as.Date("2021-01-04") + 0:9,
      visit = as.Date("2021-01-05") + 0:9
    ),
    event_date = "onset", report_date = "visit",
    data_type = "linelist", verbose = FALSE
  )

  earlier <- suppressWarnings(change_now(plain, as.Date("2021-01-08")))
  expect_equal(get_now(earlier), as.Date("2021-01-08"))
  expect_equal(bare_cols(earlier), bare_cols(plain))
})

test_that("change_now() masks count data too, and resets the censoring flag", {
  cases <- data.frame(
    onset   = as.Date("2021-01-04") + rep(0:4, each = 2),
    visit   = as.Date("2021-01-05") + rep(0:4, each = 2),
    result  = as.Date("2021-01-06") + rep(0:4, each = 2),
    outcome = rep(c("confirmed", "retracted"), 5),
    n       = 1:10,
    stringsAsFactors = FALSE
  )
  counts <- tbl_now(cases,
    event_date = "onset", report_date = "visit",
    validation_date = "result", validation_type = "outcome",
    case_count = "n", data_type = "count-incidence", verbose = FALSE
  )
  # Flag everything, so the reset below has something to reset.
  counts <- suppressMessages(censor_validation_delays_above(counts, 0))
  flag <- get_is_censored_validation(counts)
  expect_true(all(counts[[flag]]))

  earlier <- suppressMessages(suppressWarnings(
    change_now(counts, as.Date("2021-01-07"))
  ))
  future <- cases$result > as.Date("2021-01-07")

  expect_true(all(is.na(earlier$result[future])))
  expect_true(all(earlier$outcome[future] == "pending"))
  # A resolution that has not happened has no delay, so there is no bound on
  # one either.
  expect_true(all(!earlier[[flag]][future]))
  expect_true(all(earlier[[flag]][!future]))
  # No case is lost -- masking is about dates, not rows.
  expect_equal(sum(earlier$n), sum(counts$n))
})
