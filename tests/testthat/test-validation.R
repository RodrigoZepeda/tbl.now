# The validation process: a third date, after the event and the report.
#
# The influenza shape is the one to keep in mind -- onset, then the medical
# visit, then the laboratory result, which can come back either way. The tests
# below are written against the failures that would actually bite:
#
#   * a retraction summed together with the case it retracts,
#   * an outcome invented from a date that cannot imply one,
#   * `now` left behind a validation that has already happened,
#   * the attributes quietly lost by a dplyr verb.

flu_fixture <- function(n_days = 10L, seed = 20260825L) {
  set.seed(seed)
  outcomes <- rep(c("confirmed", "confirmed", "retracted", "pending"), length.out = 4L * n_days)
  cases <- data.frame(
    onset = as.Date("2021-01-04") + rep(seq_len(n_days) - 1L, each = 4L),
    visit = as.Date("2021-01-04") + rep(seq_len(n_days) - 1L, each = 4L) + 1L,
    result = as.Date("2021-01-04") + rep(seq_len(n_days) - 1L, each = 4L) + 3L,
    outcome = outcomes,
    stringsAsFactors = FALSE
  )
  # A pending case has not been resolved, so it has no result date.
  cases$result[cases$outcome == "pending"] <- as.Date(NA)

  tbl_now(cases,
    event_date = "onset", report_date = "visit",
    validation_date = "result", validation_type = "outcome",
    data_type = "linelist", verbose = FALSE
  )
}

test_that("a validation process is recorded on the object", {
  flu <- flu_fixture()

  expect_true(has_validation(flu))
  expect_equal(get_validation_date(flu), "result")
  expect_equal(get_validation_type(flu), "outcome")
  expect_equal(get_validation_units(flu), "days")

  # The generated pair, on the same anchor as the other numeric columns.
  expect_true(all(c(".validation_num", ".validation_delay") %in% colnames(flu)))
  resolved <- !is.na(flu$result)
  expect_equal(
    flu$.validation_delay[resolved],
    (flu$.validation_num - flu$.report_num)[resolved]
  )

  # Protected, so removing one downgrades the object -- as `.delay` does.
  expect_true(all(
    c(".validation_num", ".validation_delay", "result", "outcome") %in%
      get_protected_cols(flu)
  ))
})

test_that("an object with no validation is unchanged", {
  plain <- tbl_now(
    data.frame(
      e = as.Date("2021-01-04") + 0:4, r = as.Date("2021-01-05") + 0:4
    ),
    event_date = "e", report_date = "r", data_type = "linelist", verbose = FALSE
  )

  expect_false(has_validation(plain))
  expect_null(get_validation_date(plain))
  expect_null(get_validation_units(plain))
  expect_false(any(
    c(".validation_num", ".validation_delay") %in% colnames(plain)
  ))
})

test_that("a validation date with no outcome is NA, not a guess", {
  cases <- data.frame(
    e = as.Date("2021-01-04") + 0:2,
    r = as.Date("2021-01-05") + 0:2,
    cf = as.Date("2021-01-06") + 0:2
  )

  # THE decision this guards: a date alone cannot say whether the test came
  # back positive or negative. Calling it "confirmed" would invert the meaning
  # of every negative result in the data.
  expect_warning(
    x <- tbl_now(cases,
      event_date = "e", report_date = "r", validation_date = "cf",
      data_type = "linelist", verbose = FALSE
    ),
    "cannot say whether"
  )
  expect_true(all(is.na(x[[get_validation_type(x)]])))
})

test_that("a case with no validation date is pending", {
  cases <- data.frame(
    e = as.Date("2021-01-04") + 0:2,
    r = as.Date("2021-01-05") + 0:2,
    # Only ONE validation: a single date has no spacing to infer a grid from,
    # so the units fall back to the report units rather than erroring.
    cf = c(as.Date("2021-01-06"), NA, NA)
  )
  x <- suppressWarnings(tbl_now(cases,
    event_date = "e", report_date = "r", validation_date = "cf",
    data_type = "linelist", verbose = FALSE
  ))

  outcome <- x[[get_validation_type(x)]]
  expect_equal(outcome[2:3], c("pending", "pending"))
  expect_true(is.na(outcome[1]))
})

test_that("an unrecognised outcome is refused", {
  cases <- data.frame(
    e = as.Date("2021-01-04") + 0:2, r = as.Date("2021-01-05") + 0:2,
    cf = as.Date("2021-01-06") + 0:2,
    ty = c("confirmed", "probable", "retracted")
  )
  expect_error(
    tbl_now(cases,
      event_date = "e", report_date = "r",
      validation_date = "cf", validation_type = "ty",
      data_type = "linelist", verbose = FALSE
    ),
    "unrecognised"
  )
})

test_that("an outcome without a date is refused", {
  cases <- data.frame(
    e = as.Date("2021-01-04") + 0:2, r = as.Date("2021-01-05") + 0:2,
    ty = c("confirmed", "pending", "retracted")
  )
  expect_error(
    tbl_now(cases,
      event_date = "e", report_date = "r", validation_type = "ty",
      data_type = "linelist", verbose = FALSE
    ),
    "without a"
  )
})

# `now` -----------------------------------------------------------------------

test_that("a validation moves `now` forward", {
  cases <- data.frame(
    e = as.Date("2021-01-01") + 0:1,
    r = as.Date("2021-01-02") + 0:1,
    cf = as.Date("2021-01-10") + 0:1
  )
  x <- suppressWarnings(tbl_now(cases,
    event_date = "e", report_date = "r", validation_date = "cf",
    data_type = "linelist", verbose = FALSE
  ))

  # A validation is an observation: the as-of moment is the last thing
  # anybody knew, not the last thing anybody reported.
  expect_equal(get_now(x), as.Date("2021-01-11"))
})

test_that("a `now` behind the last validation is refused", {
  flu <- flu_fixture()
  broken <- flu
  attr(broken, "now") <- min(flu$result, na.rm = TRUE) - 1

  expect_error(validate_tbl_now(broken), "AFTER")
})

test_that("the timeline is checked, not assumed", {
  cases <- data.frame(
    e = as.Date("2021-01-04") + 0:2,
    r = as.Date("2021-01-08") + 0:2,
    cf = as.Date("2021-01-06") + 0:2, # validated BEFORE reported
    ty = rep("confirmed", 3)
  )
  expect_warning(
    tbl_now(cases,
      event_date = "e", report_date = "r",
      validation_date = "cf", validation_type = "ty",
      data_type = "linelist", verbose = FALSE
    ),
    "validated BEFORE"
  )
})

# Persistence -----------------------------------------------------------------

test_that("the validation attributes survive dplyr verbs", {
  flu <- flu_fixture()

  verbs <- list(
    filter = dplyr::filter(flu, .data$outcome != "pending"),
    mutate = dplyr::mutate(flu, extra = 1),
    arrange = dplyr::arrange(flu, .data$onset),
    slice = dplyr::slice(flu, 1:8)
  )

  for (name in names(verbs)) {
    result <- verbs[[name]]
    expect_true(is_tbl_now(result), label = paste0(name, " keeps tbl_now"))
    expect_equal(
      get_validation_date(result), "result",
      label = paste0(name, " keeps validation_date")
    )
    expect_equal(
      get_validation_type(result), "outcome",
      label = paste0(name, " keeps validation_type")
    )
    expect_equal(
      get_validation_units(result), "days",
      label = paste0(name, " keeps validation_units")
    )
  }
})

test_that("tbl_now_attributes() lists the validation attributes", {
  # It used to diff against a DEFAULT tbl_now, which has none of the optional
  # attributes -- so every optional attribute was silently missing from the
  # listing, which is exactly what somebody uses this function to check.
  listed <- names(tbl_now_attributes(flu_fixture()))
  expect_true(all(
    c("validation_date", "validation_type", "validation_units") %in% listed
  ))
})

test_that("add / change / remove round-trip", {
  cases <- data.frame(
    e = as.Date("2021-01-04") + 0:4, r = as.Date("2021-01-05") + 0:4,
    cf = as.Date("2021-01-06") + 0:4, other = as.Date("2021-01-07") + 0:4,
    ty = rep("confirmed", 5)
  )
  plain <- tbl_now(cases,
    event_date = "e", report_date = "r", data_type = "linelist", verbose = FALSE
  )
  expect_false(has_validation(plain))

  added <- add_validation_date(plain, cf, ty)
  expect_true(has_validation(added))
  expect_equal(get_validation_date(added), "cf")

  # Adding twice is a mistake worth naming.
  expect_error(add_validation_date(added, other), "already has")

  changed <- change_validation_date(added, other, ty)
  expect_equal(get_validation_date(changed), "other")

  removed <- remove_validation_date(changed)
  expect_false(has_validation(removed))
  expect_false(any(
    c(".validation_num", ".validation_delay") %in% colnames(removed)
  ))
  # The user's own columns stay; only the generated ones go.
  expect_true(all(c("cf", "other", "ty") %in% colnames(removed)))
})

# Counting --------------------------------------------------------------------

test_that("to_count() keeps confirmed and retracted apart", {
  counts <- data.frame(
    e = rep(as.Date("2021-01-04") + 0:2, each = 2),
    r = rep(as.Date("2021-01-05") + 0:2, each = 2),
    cf = rep(as.Date("2021-01-06") + 0:2, each = 2),
    ty = rep(c("confirmed", "retracted"), 3),
    n = c(5L, 2L, 6L, 1L, 7L, 3L)
  )
  x <- tbl_now(counts,
    event_date = "e", report_date = "r", case_count = "n",
    validation_date = "cf", validation_type = "ty",
    data_type = "count-incidence", verbose = FALSE
  )

  aggregated <- suppressWarnings(suppressMessages(to_count(x, to = "count-incidence")))

  # A case and its retraction share an (event, report) pair without being
  # duplicates, so building this must not warn about non-uniqueness.
  expect_no_warning(tbl_now(counts,
    event_date = "e", report_date = "r", case_count = "n",
    validation_date = "cf", validation_type = "ty",
    data_type = "count-incidence", verbose = FALSE
  ))

  # THE failure: summing a case together with its own retraction. Six rows in,
  # six rows out, totals untouched.
  expect_equal(nrow(aggregated), 6L)
  expect_equal(sum(aggregated$n), sum(counts$n))
  expect_setequal(unique(aggregated$ty), c("confirmed", "retracted"))
})

test_that("the three counts answer three different questions", {
  flu <- flu_fixture(n_days = 3L)

  reported <- get_latest_reported_cases(flu)
  confirmed <- get_latest_confirmed(flu)
  net <- get_net_confirmed(flu)

  # Per day the fixture has 2 confirmed, 1 retracted, 1 pending.
  expect_equal(unique(reported[["n"]]), 4)
  expect_equal(unique(confirmed[["n"]]), 2)
  expect_equal(unique(net[["n"]]), 1) # 2 confirmed - 1 retracted

  # Net can go NEGATIVE, which is the whole point: a published cumulative total
  # revises downward when cases are withdrawn.
  withdrawn <- data.frame(
    e = as.Date("2021-01-04") + c(0, 1),
    r = as.Date("2021-01-05") + c(0, 1),
    cf = as.Date("2021-01-06") + c(0, 1),
    ty = c("retracted", "retracted")
  )
  x <- tbl_now(withdrawn,
    event_date = "e", report_date = "r",
    validation_date = "cf", validation_type = "ty",
    data_type = "linelist", verbose = FALSE, warn_non_uniqueness = FALSE
  )
  expect_equal(get_net_confirmed(x)[["n"]], c(-1, -1))
  expect_equal(get_latest_confirmed(x)[["n"]], c(0, 0))
})

test_that("the counting getters refuse an object with no validation", {
  plain <- tbl_now(
    data.frame(e = as.Date("2021-01-04") + 0:2, r = as.Date("2021-01-05") + 0:2),
    event_date = "e", report_date = "r", data_type = "linelist", verbose = FALSE
  )
  expect_error(get_latest_confirmed(plain), "needs a validation process")
  expect_error(get_net_confirmed(plain), "needs a validation process")
})

# Does the delay depend on the outcome? ---------------------------------------

test_that("diagnose_validation_delay() finds a difference that is really there", {
  # Retracted results deliberately take 5-6 days against the confirmed 1-2, so
  # a test that cannot see this cannot see anything.
  cases <- data.frame(
    onset = as.Date("2021-01-04") + rep(0:19, each = 4),
    visit = as.Date("2021-01-05") + rep(0:19, each = 4),
    result = as.Date("2021-01-05") + rep(0:19, each = 4) +
      rep(c(1, 2, 5, 6), times = 20),
    outcome = rep(c("confirmed", "confirmed", "retracted", "retracted"), times = 20)
  )
  flu <- tbl_now(cases,
    event_date = "onset", report_date = "visit",
    validation_date = "result", validation_type = "outcome",
    data_type = "linelist", verbose = FALSE
  )

  result <- diagnose_validation_delay(flu)

  expect_equal(nrow(result), 1L)
  expect_equal(result$n_confirmed, 40L)
  expect_equal(result$n_retracted, 40L)
  expect_equal(result$median_confirmed, 1.5)
  expect_equal(result$median_retracted, 5.5)
  expect_equal(result$difference, -4)
  expect_lt(result$p.value, 0.001)
})

test_that("no difference is reported when there is none", {
  cases <- data.frame(
    onset = as.Date("2021-01-04") + rep(0:19, each = 4),
    visit = as.Date("2021-01-05") + rep(0:19, each = 4),
    # The SAME spread of delays for both outcomes. Two identical constants
    # would make the test degenerate (NaN), which is not "no difference".
    result = as.Date("2021-01-05") + rep(0:19, each = 4) +
      rep(c(1, 4, 1, 4), times = 20),
    outcome = rep(c("confirmed", "confirmed", "retracted", "retracted"), times = 20)
  )
  flu <- tbl_now(cases,
    event_date = "onset", report_date = "visit",
    validation_date = "result", validation_type = "outcome",
    data_type = "linelist", verbose = FALSE
  )

  result <- diagnose_validation_delay(flu)
  expect_equal(result$difference, 0)
  expect_gt(result$p.value, 0.05)
})

test_that("unusable delays are dropped and counted", {
  cases <- data.frame(
    onset = as.Date("2021-01-04") + 0:5,
    visit = as.Date("2021-01-10") + 0:5,
    # Two rows are validated BEFORE they were reported: a negative delay.
    result = as.Date("2021-01-12") + c(0, 1, -5, -6, 2, 3),
    outcome = c(rep("confirmed", 3), rep("retracted", 3))
  )
  flu <- suppressWarnings(tbl_now(cases,
    event_date = "onset", report_date = "visit",
    validation_date = "result", validation_type = "outcome",
    data_type = "linelist", verbose = FALSE
  ))

  result <- diagnose_validation_delay(flu)
  expect_equal(attr(result, "dropped"), 2L)
  expect_equal(result$n_confirmed + result$n_retracted, 4L)
})

test_that("the comparison can be made within a stratum", {
  cases <- data.frame(
    onset = as.Date("2021-01-04") + rep(0:19, each = 4),
    visit = as.Date("2021-01-05") + rep(0:19, each = 4),
    result = as.Date("2021-01-05") + rep(0:19, each = 4) +
      rep(c(1, 2, 5, 6), times = 20),
    outcome = rep(c("confirmed", "confirmed", "retracted", "retracted"), times = 20),
    site = rep(c("north", "south"), length.out = 80)
  )
  flu <- tbl_now(cases,
    event_date = "onset", report_date = "visit", strata = "site",
    validation_date = "result", validation_type = "outcome",
    data_type = "linelist", verbose = FALSE
  )

  result <- diagnose_validation_delay(flu, by = "site")
  expect_setequal(result$stratum, c("north", "south"))
  expect_equal(nrow(result), 2L)
})

test_that("plot_validation_delay() draws the reporting process", {
  skip_if_not_installed("ggplot2")
  expect_s3_class(plot_validation_delay(flu_fixture(n_days = 20L)), "ggplot")
})

test_that("complete_zeroes() extends the grid to the validation-aware now", {
  counts <- data.frame(
    e = as.Date("2021-01-01") + c(0, 1),
    r = as.Date("2021-01-01") + c(1, 2),
    cf = as.Date("2021-01-01") + c(2, 9),
    n = c(1L, 1L)
  )
  x <- suppressWarnings(tbl_now(counts,
    event_date = "e", report_date = "r", case_count = "n",
    validation_date = "cf", data_type = "count-incidence", verbose = FALSE
  ))
  expect_equal(get_now(x), as.Date("2021-01-10"))

  completed <- suppressWarnings(suppressMessages(complete_zeroes(x)))
  expect_equal(max(completed$e), as.Date("2021-01-10"))
})

test_that("update() keeps the validation process and de-duplicates on it", {
  # `update.tbl_now()` de-duplicates on everything EXCEPT the generated
  # columns. Adding `.validation_num`/`.validation_delay` to that generated
  # set broke the call, which passed no object and so could not know they
  # existed -- 16 tests went red. This is the case the fix exists for.
  counts <- data.frame(
    e = as.Date("2021-01-04") + 0:2,
    r = as.Date("2021-01-05") + 0:2,
    cf = as.Date("2021-01-06") + 0:2,
    ty = rep("confirmed", 3),
    n = c(1L, 2L, 3L)
  )
  x <- tbl_now(counts,
    event_date = "e", report_date = "r", case_count = "n",
    validation_date = "cf", validation_type = "ty",
    data_type = "count-incidence", verbose = FALSE
  )

  # Updating with the same rows must not duplicate them...
  same <- suppressWarnings(suppressMessages(update(x, new_data = counts)))
  expect_equal(nrow(same), nrow(x))
  expect_true(has_validation(same))
  expect_equal(get_validation_date(same), "cf")
  expect_equal(get_validation_type(same), "ty")

  # ...and a genuinely new row must arrive.
  extra <- data.frame(
    e = as.Date("2021-01-08"), r = as.Date("2021-01-09"),
    cf = as.Date("2021-01-10"), ty = "retracted", n = 4L
  )
  grown <- suppressWarnings(suppressMessages(update(x, new_data = extra)))
  expect_equal(nrow(grown), nrow(x) + 1L)
  expect_true("retracted" %in% grown[["ty"]])
})

# The validation axis ------------------------------------------------------

validation_axis_fixture <- function(seed = 7L) {
  set.seed(seed)
  days <- as.Date("2021-01-04") + 0:59
  per_day <- stats::rpois(60, 6) + 2
  onset <- rep(days, per_day)
  cases <- data.frame(onset = onset, visit = onset + 1, result = onset + 3)
  # A LABORATORY BACKLOG: a week of specimens all resolved on one day. Nothing
  # unusual happens on the report axis -- reporting was regular throughout.
  backlog <- cases$onset >= as.Date("2021-01-24") & cases$onset <= as.Date("2021-01-30")
  cases$result[backlog] <- as.Date("2021-02-03")
  cases$outcome <- "confirmed"

  suppressWarnings(tbl_now(cases,
    event_date = "onset", report_date = "visit",
    validation_date = "result", validation_type = "outcome",
    data_type = "linelist", verbose = FALSE
  ))
}

test_that("diagnose_batches() finds a laboratory backlog only on the validation axis", {
  x <- validation_axis_fixture()

  on_report <- suppressWarnings(suppressMessages(diagnose_batches(x, lookback = 5)))
  on_validation <- suppressWarnings(suppressMessages(
    diagnose_batches(x, lookback = 5, axis = "validation")
  ))

  # The point of the option: reporting was regular, so the report axis sees
  # nothing. The backlog is only visible where it happened.
  expect_equal(sum(on_report$batch, na.rm = TRUE), 0L)
  expect_equal(sum(on_validation$batch, na.rm = TRUE), 1L)

  flagged <- on_validation[which(on_validation$batch), ]
  expect_equal(flagged$report_date, as.Date("2021-02-03"))
  # An order of magnitude above baseline, so this cannot pass by chance.
  expect_gt(flagged$reported, 5 * flagged$baseline)
})

test_that("the validation axis needs a validation process", {
  plain <- suppressWarnings(tbl_now(
    data.frame(
      e = as.Date("2021-01-04") + rep(0:19, each = 3),
      r = as.Date("2021-01-05") + rep(0:19, each = 3)
    ),
    event_date = "e", report_date = "r", data_type = "linelist", verbose = FALSE
  ))
  expect_error(
    suppressWarnings(suppressMessages(diagnose_batches(plain, axis = "validation"))),
    "needs a validation process"
  )
})

test_that("pending cases are excluded from the validation axis", {
  # A pending case has no validation date, so counting it would invent an
  # arrival on a date it does not have.
  x <- flu_fixture(n_days = 20L)
  increments <- tbl.now:::.batch_report_increments(x, axis = "validation")

  resolved <- sum(!is.na(x[[get_validation_date(x)]]))
  expect_equal(sum(increments$.count), resolved)
  expect_lt(resolved, nrow(x))
})

test_that("the reporting-process plots accept the validation axis", {
  skip_if_not_installed("ggplot2")
  x <- validation_axis_fixture()

  for (fn in c("plot_reporting_process", "plot_epidemic_process",
               "plot_reporting_hexamap")) {
    drawn <- suppressWarnings(suppressMessages(
      do.call(fn, list(x, axis = "validation"))
    ))
    expect_s3_class(drawn, "ggplot")
  }
})

# The smaller analogues -------------------------------------------------------

test_that("get_initial_confirmed() and get_nth_confirmed() count by delay", {
  cases <- data.frame(
    onset = as.Date("2021-01-04") + 0:4,
    visit = as.Date("2021-01-05") + 0:4,
    # Validation delays of 0, 2, 1, 90 and 2 periods.
    result = as.Date("2021-01-05") + 0:4 + c(0, 2, 1, 90, 2),
    outcome = rep("confirmed", 5)
  )
  flu <- tbl_now(cases,
    event_date = "onset", report_date = "visit",
    validation_date = "result", validation_type = "outcome",
    data_type = "linelist", verbose = FALSE
  )

  expect_equal(get_latest_confirmed(flu)[["n"]], rep(1, 5))
  # Same-period resolution only: the first case.
  expect_equal(get_initial_confirmed(flu)[["n"]], c(1, 0, 0, 0, 0))
  # Within two periods: everything except the 90-day straggler.
  expect_equal(get_nth_confirmed(flu, 2)[["n"]], c(1, 1, 1, 0, 1))

  expect_error(get_nth_confirmed(flu, "two"), "single number")
})

test_that("censor_validation_delays_above() returns stragglers to pending", {
  cases <- data.frame(
    onset = as.Date("2021-01-04") + 0:4,
    visit = as.Date("2021-01-05") + 0:4,
    result = as.Date("2021-01-05") + 0:4 + c(1, 2, 1, 90, 2),
    outcome = rep("confirmed", 5)
  )
  flu <- tbl_now(cases,
    event_date = "onset", report_date = "visit",
    validation_date = "result", validation_type = "outcome",
    data_type = "linelist", verbose = FALSE
  )

  censored <- suppressMessages(censor_validation_delays_above(flu, 30))

  expect_equal(sum(censored[["outcome"]] == "pending"), 1L)
  expect_equal(sum(censored[["outcome"]] == "confirmed"), 4L)
  # The date goes with the outcome: a "confirmed" row with no date is the
  # contradiction `tbl_now()` warns about, and a date we refuse to believe is
  # not a resolution.
  expect_true(is.na(censored[["result"]][4]))
  expect_true(is.na(censored[[".validation_delay"]][4]))
  # ...and the confirmed count falls by exactly that one case.
  expect_equal(sum(get_latest_confirmed(censored)[["n"]]), 4)
})

test_that("plot_validation_status() shows the resolution front", {
  skip_if_not_installed("ggplot2")
  x <- flu_fixture(n_days = 20L)

  expect_s3_class(plot_validation_status(x), "ggplot")
  expect_s3_class(plot_validation_status(x, proportion = FALSE), "ggplot")

  plain <- tbl_now(
    data.frame(e = as.Date("2021-01-04") + 0:4, r = as.Date("2021-01-05") + 0:4),
    event_date = "e", report_date = "r", data_type = "linelist", verbose = FALSE
  )
  expect_error(plot_validation_status(plain), "needs a validation process")
})

test_that("align_weeks() aligns the validation date too", {
  weekly <- data.frame(
    e = as.Date("2021-01-04") + 7 * (0:5),
    r = as.Date("2021-01-06") + 7 * (0:5),
    cf = as.Date("2021-01-09") + 7 * (0:5),
    ty = rep("confirmed", 6), n = 2L
  )
  x <- suppressWarnings(tbl_now(weekly,
    event_date = "e", report_date = "r", case_count = "n",
    validation_date = "cf", validation_type = "ty",
    data_type = "count-incidence", event_units = "weeks", report_units = "weeks",
    verbose = FALSE
  ))

  # Three dates on three different weekdays give FRACTIONAL delays, which is
  # exactly what this function exists to fix -- and it used to fix only two of
  # them, leaving `.validation_delay` fractional.
  expect_false(all(x$.delay == round(x$.delay)))
  expect_false(all(x$.validation_delay == round(x$.validation_delay)))

  aligned <- suppressWarnings(suppressMessages(align_weeks(x)))

  expect_true(has_validation(aligned))
  expect_true(all(aligned$.delay == round(aligned$.delay)))
  expect_true(all(aligned$.validation_delay == round(aligned$.validation_delay)))
})

test_that("the delay family measures to the validation when asked", {
  # Every case is reported one period after onset and confirmed three, so the
  # two axes have KNOWN, different answers.
  x <- validation_axis_fixture()

  on_report <- tbl.now:::.tbl_now_delay_long(x, NULL, axis = "report")
  on_validation <- tbl.now:::.tbl_now_delay_long(x, NULL, axis = "validation")

  expect_equal(stats::median(rep(on_report$delay, on_report$weight)), 1)
  expect_equal(stats::median(rep(on_validation$delay, on_validation$weight)), 3)
  # Measured from the event on both axes, so the validation delay is never
  # the shorter of the two.
  expect_gt(sum(on_validation$delay * on_validation$weight),
            sum(on_report$delay * on_report$weight))
})

test_that("the delay diagnostics accept the validation axis", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("modifiedmk")
  x <- validation_axis_fixture()

  quietly <- function(expr) suppressWarnings(suppressMessages(expr))

  expect_s3_class(quietly(plot_reporting_triangle(x, axis = "validation")), "ggplot")
  expect_s3_class(quietly(plot_delay_profiles(x, axis = "validation")), "ggplot")
  expect_s3_class(quietly(plot_delay_drift(x, axis = "validation")), "ggplot")
  expect_s3_class(quietly(diagnostic_plot(x, axis = "validation")), "ggplot")

  expect_s3_class(quietly(diagnose_drift(x, axis = "validation")), "data.frame")
  expect_s3_class(quietly(transport_discriminant(x, axis = "validation")), "data.frame")
})

test_that("the validation triangle is labelled as one", {
  skip_if_not_installed("ggplot2")
  x <- validation_axis_fixture()

  drawn <- suppressWarnings(suppressMessages(
    plot_reporting_triangle(x, axis = "validation")
  ))
  expect_equal(drawn$labels$title, "Validation triangle")
})

test_that("simulate_batch() carries the validation through", {
  set.seed(3)
  days <- as.Date("2021-01-04") + 0:39
  onset <- rep(days, stats::rpois(40, 8) + 3)
  cases <- data.frame(
    onset = onset, visit = onset + 1, result = onset + 5,
    outcome = "confirmed"
  )
  x <- suppressWarnings(tbl_now(cases,
    event_date = "onset", report_date = "visit",
    validation_date = "result", validation_type = "outcome",
    data_type = "linelist", verbose = FALSE
  ))

  sim <- suppressWarnings(suppressMessages(
    simulate_batch(x, closed_dates = as.Date("2021-01-20") + 0:2)
  ))

  expect_true(has_validation(sim))
  expect_equal(get_validation_date(sim), "result")
  expect_equal(nrow(sim), nrow(x))

  # The mirror of the laboratory-backlog test: a simulated REPORTING backlog is
  # found on the report axis and is correctly invisible on the validation
  # axis, because the laboratory never paused.
  on_report <- suppressWarnings(suppressMessages(diagnose_batches(sim)))
  on_validation <- suppressWarnings(suppressMessages(
    diagnose_batches(sim, axis = "validation")
  ))
  expect_gte(sum(on_report$batch, na.rm = TRUE), 1L)
  expect_equal(sum(on_validation$batch, na.rm = TRUE), 0L)
})
