# `censor_reports()` / `censor_delays()` take a condition and record the
# matching rows as bounds rather than measurements. The behaviour worth pinning
# down is: the condition is evaluated in the data, `NA` is not a match, existing
# flags are never cleared, and replacing a date leaves a coherent object.
#
# `censor_delays_above()` is exercised in test-censor_delays.R; the tests here
# check that it still agrees with the general form it now shares helpers with.

library(dplyr, quietly = TRUE, warn.conflicts = FALSE)

make_messy <- function(flag = FALSE) {
  df <- data.frame(
    onset = as.Date("2020-01-01") + 0:3,
    reported = as.Date(c("2020-01-03", NA, "2222-02-22", "2020-01-06")),
    sex = c("F", "M", "F", "M")
  )
  if (flag) df$was_censored <- c(TRUE, FALSE, FALSE, FALSE)
  suppressWarnings(tbl_now(df,
    event_date = onset, report_date = reported, strata = sex,
    is_censored_report = if (flag) "was_censored" else NULL,
    data_type = "linelist", units = "days", verbose = FALSE,
    now = as.Date("2020-01-10")
  ))
}

make_delays <- function() {
  df <- data.frame(
    onset = as.Date("2020-01-01") + c(0, 0, 1, 2),
    reported = as.Date("2020-01-01") + c(1, 5, 2, 300)
  )
  tbl_now(df,
    event_date = onset, report_date = reported,
    data_type = "linelist", units = "days", verbose = FALSE
  )
}

# ---- Wrong inputs -----------------------------------------------------------

test_that("censor_reports and censor_delays refuse a non-tbl_now", {
  df <- data.frame(a = 1)
  expect_error(censor_reports(df, a > 0), "must be a <tbl_now>")
  expect_error(censor_delays(df, a > 0), "must be a <tbl_now>")
})

test_that("the condition is required", {
  x <- make_delays()
  expect_error(censor_reports(x), "condition")
  expect_error(censor_delays(x), "condition")
})

test_that("a condition naming a column that is not there is an error", {
  x <- make_delays()
  expect_error(suppressWarnings(censor_reports(x, no_such_column > 1)), "no_such_column")
  expect_error(suppressWarnings(censor_delays(x, no_such_column > 1)), "no_such_column")
})

test_that("a condition that is not logical, or the wrong length, is refused", {
  x <- make_delays()
  expect_error(censor_reports(x, .delay), "logical")
  expect_error(censor_delays(x, onset), "logical")
  expect_error(censor_reports(x, c(TRUE, FALSE)), "length 1 or 4")
})

test_that("a replacement of the wrong type or length is refused", {
  x <- make_delays()
  expect_error(
    censor_reports(x, .delay > 60, to_report = 100),
    "must be a <Date>"
  )
  expect_error(
    censor_reports(x, .delay > 60, to_report = as.Date("2020-06-01") + 0:1),
    "length 1 or 4"
  )
  expect_error(censor_delays(x, .delay > 60, to_delay = "sixty"), "must be a number")
  expect_error(censor_delays(x, .delay > 60, to_delay = c(1, 2)), "length 1 or 4")
  expect_error(censor_delays(x, .delay > 60, to_delay = NA_real_), "must be a number")
})

test_that("verbose is checked", {
  x <- make_delays()
  expect_error(censor_reports(x, .delay > 60, verbose = "yes"), "verbose")
  expect_error(censor_delays(x, .delay > 60, verbose = 1), "verbose")
})

# ---- Results worked out by hand ---------------------------------------------

test_that("censor_reports replaces the matching dates with `now` by default", {
  x <- make_messy()
  out <- suppressWarnings(censor_reports(
    x, is.na(reported) | reported > as.Date("2100-01-01"),
    verbose = FALSE
  ))

  expect_equal(
    out[[get_report_date(out)]],
    as.Date(c("2020-01-03", "2020-01-10", "2020-01-10", "2020-01-06"))
  )
  expect_equal(out[[get_is_censored_report(out)]], c(FALSE, TRUE, TRUE, FALSE))
  # onset 1..4 Jan against reports 3, 10, 10, 6 Jan.
  expect_equal(out$.delay, c(2, 8, 7, 2))
  expect_equal(get_now(out), as.Date("2020-01-10"))
  expect_true(is_tbl_now(out))
  expect_equal(get_strata(out), "sex")
})

test_that("censor_reports honours an explicit replacement date", {
  x <- make_messy()
  out <- suppressWarnings(censor_reports(
    x, is.na(reported),
    to_report = as.Date("2020-01-20"), verbose = FALSE
  ))

  expect_equal(out[[get_report_date(out)]][2], as.Date("2020-01-20"))
  # A replacement past `now` drags `now` with it -- report <= now is the rule.
  expect_equal(get_now(out), as.Date("2222-02-22"))
})

test_that("a per-row replacement vector is applied row by row", {
  x <- make_delays()
  out <- censor_reports(
    x, .delay > 3,
    to_report = as.Date("2020-01-01") + c(9, 9, 9, 9), verbose = FALSE
  )
  # Rows 2 (delay 5) and 4 (delay 298) match; rows 1 and 3 keep their dates.
  expect_equal(
    out[[get_report_date(out)]],
    as.Date("2020-01-01") + c(1, 9, 2, 9)
  )
})

test_that("censor_reports with to_report = NULL only sets the flag", {
  x <- make_messy()
  out <- suppressWarnings(censor_reports(x, is.na(reported), to_report = NULL, verbose = FALSE))

  expect_equal(out[[get_is_censored_report(out)]], c(FALSE, TRUE, FALSE, FALSE))
  expect_true(is.na(out[[get_report_date(out)]][2]))
})

test_that("censor_delays caps the delay by moving the report date", {
  x <- make_delays()
  out <- censor_delays(x, .delay > 60, to_delay = 60, verbose = FALSE)

  expect_equal(out$.delay, c(1, 5, 1, 60))
  expect_equal(out[[get_is_censored_report(out)]], c(FALSE, FALSE, FALSE, TRUE))
  # Row 4's onset is 2020-01-03, so 60 days later is 2020-03-03.
  expect_equal(out[[get_report_date(out)]][4], as.Date("2020-03-03"))
  # `now` never moves backwards just because a delay was capped.
  expect_equal(get_now(out), get_now(x))
})

test_that("censor_delays without a replacement only sets the flag", {
  x <- make_delays()
  out <- censor_delays(x, .delay > 60, verbose = FALSE)

  expect_equal(out$.delay, x$.delay)
  expect_equal(out[[get_is_censored_report(out)]], c(FALSE, FALSE, FALSE, TRUE))
})

test_that("censor_delays agrees with censor_delays_above on the same rule", {
  x <- make_delays()
  general <- censor_delays(x, is.finite(.delay) & .delay > 60, verbose = FALSE)
  special <- censor_delays_above(x, max_delay = 60, verbose = FALSE)

  expect_equal(
    general[[get_is_censored_report(general)]],
    special[[get_is_censored_report(special)]]
  )
})

test_that("an NA condition is not a match", {
  x <- make_messy()
  # `reported > date` is NA for the row with no report date.
  out <- suppressWarnings(censor_reports(x, reported > as.Date("2100-01-01"), verbose = FALSE))

  expect_equal(out[[get_is_censored_report(out)]], c(FALSE, FALSE, TRUE, FALSE))
  expect_true(is.na(out[[get_report_date(out)]][2]))
})

test_that("a condition matching nothing leaves the data alone", {
  x <- make_delays()
  out <- censor_delays(x, .delay > 1e6, verbose = FALSE)

  expect_equal(out$.delay, x$.delay)
  expect_false(any(out[[get_is_censored_report(out)]]))
})

test_that("a length-1 condition is recycled over every row", {
  x <- make_delays()
  expect_true(all(censor_delays(x, TRUE, verbose = FALSE)[[".is_censored_report"]]))
  expect_false(any(censor_delays(x, FALSE, verbose = FALSE)[[".is_censored_report"]]))
})

# ---- The existing flag ------------------------------------------------------

test_that("an existing censoring column is merged, never cleared", {
  x <- make_messy(flag = TRUE)
  expect_equal(get_is_censored_report(x), "was_censored")

  out <- suppressWarnings(censor_reports(x, is.na(reported), verbose = FALSE))

  expect_equal(get_is_censored_report(out), "was_censored")
  # Row 1 was already censored and stays so; row 2 is newly censored.
  expect_equal(out[["was_censored"]], c(TRUE, TRUE, FALSE, FALSE))
  expect_false(".is_censored_report" %in% colnames(out))
})

test_that("the flag column is created when the object has none", {
  x <- make_delays()
  expect_null(get_is_censored_report(x))

  out <- censor_delays(x, .delay > 60, verbose = FALSE)
  expect_equal(get_is_censored_report(out), ".is_censored_report")
})

# ---- Grouped objects --------------------------------------------------------

test_that("a grouped tbl_now gets the same answer as an ungrouped one", {
  x <- make_delays()
  grouped <- x |> group_by(!!as.symbol(get_event_date(x)))

  flagged <- censor_delays(grouped, .delay > 60, verbose = FALSE)
  expect_true(is_tbl_now(flagged))
  expect_equal(dplyr::group_vars(flagged), get_event_date(x))
  expect_equal(flagged[[".is_censored_report"]], c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(
    as_tibble(ungroup(flagged)),
    as_tibble(ungroup(censor_delays(x, .delay > 60, verbose = FALSE)))
  )

  capped <- censor_delays(grouped, .delay > 60, to_delay = 60, verbose = FALSE)
  expect_equal(capped$.delay, c(1, 5, 1, 60))
  expect_equal(
    as_tibble(ungroup(capped)),
    as_tibble(ungroup(
      censor_delays(x, .delay > 60, to_delay = 60, verbose = FALSE)
    ))
  )

  reported <- censor_reports(grouped, .delay > 60, verbose = FALSE)
  expect_equal(
    as_tibble(ungroup(reported)),
    as_tibble(ungroup(censor_reports(x, .delay > 60, verbose = FALSE)))
  )
})

test_that("a condition may name a grouping column", {
  x <- make_messy()
  out <- suppressWarnings(
    censor_reports(x |> group_by(sex), sex == "M" & is.na(reported), verbose = FALSE)
  )
  expect_equal(out[[get_is_censored_report(out)]], c(FALSE, TRUE, FALSE, FALSE))
})

# ---- Count data and the validation process ----------------------------------

test_that("count data is censored cell by cell, and the totals are untouched", {
  df <- data.frame(
    event = as.Date("2020-01-01") + c(0, 0, 1),
    report = as.Date("2020-01-01") + c(0, 70, 1),
    n = c(3, 1, 4)
  )
  x <- tbl_now(df,
    event_date = event, report_date = report, case_count = n,
    data_type = "count-incidence", units = "days", verbose = FALSE
  )
  out <- censor_delays(x, .delay > 30, to_delay = 30, verbose = FALSE)

  expect_equal(sum(out$n), sum(df$n))
  expect_equal(out[[get_is_censored_report(out)]], c(FALSE, TRUE, FALSE))
  expect_equal(get_data_type(out), "count-incidence")
})

test_that("censoring a report keeps the validation process attached", {
  cases <- data.frame(
    onset = as.Date("2021-01-04") + 0:4,
    visit = as.Date(c("2021-01-05", NA, "2021-01-07", "2021-01-08", "2021-01-09")),
    result = as.Date("2021-01-12") + 0:4,
    outcome = rep("confirmed", 5)
  )
  flu <- suppressWarnings(tbl_now(cases,
    event_date = onset, report_date = visit,
    validation_date = result, validation_type = outcome,
    data_type = "linelist", units = "days", verbose = FALSE
  ))

  out <- suppressWarnings(censor_reports(
    flu, is.na(visit),
    to_report = as.Date("2021-01-06"), verbose = FALSE
  ))

  expect_true(has_validation(out))
  expect_equal(get_validation_date(out), "result")
  expect_equal(get_validation_type(out), "outcome")
  expect_equal(out[[get_report_date(out)]][2], as.Date("2021-01-06"))
  expect_true(".validation_delay" %in% colnames(out))
})

test_that("censor_validation_delays_above still needs a validation process", {
  expect_error(
    censor_validation_delays_above(make_delays(), max_delay = 10),
    "needs a validation process"
  )
})

# ---- Messages ---------------------------------------------------------------

test_that("the censoring functions report what they did unless silenced", {
  x <- make_delays()
  expect_message(censor_reports(x, .delay > 60), "Censored")
  expect_message(censor_delays(x, .delay > 60), "Censored")
  expect_silent(censor_delays(x, .delay > 60, verbose = FALSE))
})

# ---- Second pass: composition, idempotence, and the other data shapes -------

test_that("censoring twice is the same as censoring once", {
  x <- make_delays()
  once <- censor_delays(x, .delay > 60, to_delay = 60, verbose = FALSE)
  twice <- censor_delays(once, .delay > 60, to_delay = 60, verbose = FALSE)

  expect_equal(once$.delay, twice$.delay)
  expect_equal(
    once[[get_is_censored_report(once)]],
    twice[[get_is_censored_report(twice)]]
  )
  expect_equal(get_now(once), get_now(twice))
})

test_that("a later, looser censoring never clears an earlier, stricter one", {
  x <- make_delays()
  strict <- censor_delays(x, .delay > 3, verbose = FALSE)
  loose <- censor_delays(strict, .delay > 1000, verbose = FALSE)

  # Rows 2 and 4 were flagged by the strict rule and stay flagged.
  expect_equal(loose[[".is_censored_report"]], c(FALSE, TRUE, FALSE, TRUE))
})

test_that("the condition sees variables from the calling environment", {
  x <- make_delays()
  threshold <- 60
  out <- censor_delays(x, .delay > threshold, verbose = FALSE)
  expect_equal(out[[".is_censored_report"]], c(FALSE, FALSE, FALSE, TRUE))
})

test_that("a replacement before the event date is allowed, and warned about", {
  x <- make_delays()
  expect_warning(
    censor_reports(x, .delay > 60, to_report = as.Date("2019-12-01"), verbose = FALSE),
    "before"
  )
})

test_that("a per-row to_delay is applied row by row", {
  x <- make_delays()
  out <- censor_delays(x, .delay > 3, to_delay = c(0, 2, 0, 10), verbose = FALSE)
  # Rows 2 (delay 5) and 4 (delay 298) match, and take positions 2 and 4.
  expect_equal(out$.delay, c(1, 2, 1, 10))
})

test_that("censoring works on a numeric axis, and refuses a fractional replacement", {
  x <- tbl_now(data.frame(event = 1:10, report = 1:10 + 2L),
    event_date = event, report_date = report,
    units = "numeric", verbose = FALSE
  )

  out <- censor_reports(x, event > 8, to_report = 20, verbose = FALSE)
  expect_equal(out[["report"]], c(3:10, 20, 20))
  expect_equal(out[[get_is_censored_report(out)]], c(rep(FALSE, 8), TRUE, TRUE))

  expect_error(
    censor_reports(x, event > 8, to_report = 20.5, verbose = FALSE),
    "whole number"
  )
  expect_error(
    censor_reports(x, event > 8, to_report = as.Date("2020-01-01")),
    "must be numeric"
  )
})

test_that("the delay is expressed in the object's own units, not always days", {
  onset <- as.Date("2024-01-07") + 7 * (0:4)
  df <- data.frame(
    onset = onset,
    reported = onset + 7 * c(1, 1, 9, 1, 1)
  )
  x <- tbl_now(df,
    event_date = onset, report_date = reported,
    data_type = "linelist", units = "weeks", verbose = FALSE
  )
  # Row 3 is reported eight weeks after the others' one-week delay.
  expect_equal(x$.delay, c(1, 1, 9, 1, 1))

  out <- censor_delays(x, .delay > 4, to_delay = 4, verbose = FALSE)
  expect_equal(out$.delay, c(1, 1, 4, 1, 1))
  # Four WEEKS after 2024-01-21, not four days.
  expect_equal(out[[get_report_date(out)]][3], as.Date("2024-02-18"))
})

test_that("censor_reports keeps a count-cumulative object cumulative", {
  df <- data.frame(
    event = as.Date("2024-01-01") + rep(c(0, 1), each = 3),
    report = as.Date("2024-01-01") + c(0, 1, 2, 1, 2, 3),
    n = c(1, 3, 5, 2, 2, 4)
  )
  x <- tbl_now(df,
    event_date = event, report_date = report, case_count = n,
    data_type = "count-cumulative", units = "days", verbose = FALSE
  )
  out <- censor_delays(x, .delay > 1, verbose = FALSE)

  expect_equal(get_data_type(out), "count-cumulative")
  expect_equal(out$n, df$n)
  expect_equal(out[[get_is_censored_report(out)]], c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE))
})

test_that("covariates and the temporal-effects spec survive a censoring rebuild", {
  df <- data.frame(
    onset = as.Date("2020-01-01") + 0:5,
    reported = as.Date("2020-01-01") + 0:5 + c(1, 2, 1, 2, 1, 300),
    rain = 1:6 / 10
  )
  x <- tbl_now(df,
    event_date = onset, report_date = reported, covariates = rain,
    data_type = "linelist", units = "days", verbose = FALSE
  ) |>
    add_temporal_effects(t_effects = temporal_effects(day_of_week = TRUE))

  out <- censor_delays(x, .delay > 60, to_delay = 60, verbose = FALSE)

  expect_equal(get_covariates(out), "rain")
  expect_equal(out[["rain"]], df$rain)
  expect_equal(get_temporal_effects(out), get_temporal_effects(x))

  # An EVENT-date effect describes a date that did not move, so it stays.
  with_event_effect <- compute_temporal_effects(x)
  event_col <- get_temporal_effect_cols(with_event_effect)
  kept <- censor_delays(with_event_effect, .delay > 60, to_delay = 60, verbose = FALSE)
  expect_true(all(event_col %in% colnames(kept)))
  expect_equal(get_temporal_effect_cols(kept), event_col)
})

test_that("materialised temporal-effect columns are recomputed, not left stale", {
  df <- data.frame(
    onset = as.Date("2020-01-01") + 0:5,
    reported = as.Date("2020-01-01") + 0:5 + c(1, 2, 1, 2, 1, 300)
  )
  x <- tbl_now(df,
    event_date = onset, report_date = reported,
    data_type = "linelist", units = "days", verbose = FALSE
  ) |>
    add_temporal_effects(
      t_effects = temporal_effects(day_of_week = TRUE),
      date_type = "report_date"
    ) |>
    compute_temporal_effects()

  effect_col <- get_temporal_effect_cols(x)
  expect_gt(length(effect_col), 0)
  stale <- x[[effect_col[1]]][6]

  censored <- censor_delays(x, .delay > 60, to_delay = 60, verbose = FALSE)

  # The stale column is gone rather than carried forward wrong ...
  expect_false(effect_col[1] %in% colnames(censored))
  expect_equal(get_temporal_effect_cols(censored), character(0))

  # ... and recomputing it describes the report date it now has.
  moved <- compute_temporal_effects(censored)
  expect_true(effect_col[1] %in% colnames(moved))
  expect_false(identical(moved[[effect_col[1]]][6], stale))
})

test_that("censoring a report date leaves the object valid and coherent", {
  x <- make_messy()
  out <- suppressWarnings(censor_reports(
    x, is.na(reported) | reported > as.Date("2100-01-01"),
    verbose = FALSE
  ))

  expect_silent(validate_tbl_now(out))
  expect_true(all(out[[get_report_date(out)]] <= get_now(out)))
  expect_true(all(out$.delay >= 0))
  expect_equal(out$.report_num - out$.event_num, out$.delay)
})

test_that("the issue's own use case works on the shipped messy dataset", {
  data(hai_bucaramanga, envir = environment())
  x <- suppressWarnings(tbl_now(hai_bucaramanga,
    event_date = specimen_date, report_date = report_date, strata = sex,
    data_type = "linelist", units = "days", verbose = FALSE
  ))
  missing_reports <- sum(is.na(hai_bucaramanga$report_date))
  expect_gt(missing_reports, 0)

  out <- suppressWarnings(censor_reports(
    x, is.na(report_date),
    to_report = as.Date("2023-12-31"), verbose = FALSE
  ))

  expect_equal(sum(is.na(out[[get_report_date(out)]])), 0)
  expect_equal(sum(out[[get_is_censored_report(out)]]), missing_reports)
  expect_equal(nrow(out), nrow(x))
  expect_equal(get_strata(out), "sex")
})

test_that("censoring reports then aggregating keeps every case", {
  x <- make_messy()
  fixed <- suppressWarnings(censor_reports(
    x, is.na(reported) | reported > as.Date("2100-01-01"),
    verbose = FALSE
  ))
  weekly <- aggregate_time_units(fixed, to = "weeks", verbose = FALSE)

  expect_equal(nrow(weekly), nrow(x))
  expect_equal(get_event_units(weekly), "weeks")
  # The censoring flag is a declared column, so it comes along.
  expect_equal(get_is_censored_report(weekly), ".is_censored_report")
  expect_equal(sum(weekly[[".is_censored_report"]]), 2)
})

# ---- The attributes 0.29.0 added --------------------------------------------

test_that("validation_levels and is_censored_validation survive a censoring rebuild", {
  cases <- data.frame(
    onset = as.Date("2021-01-04") + 0:4,
    visit = as.Date("2021-01-05") + 0:4,
    result = as.Date("2021-01-12") + 0:4,
    outcome = c(rep("confirmado", 4), "pendiente")
  )
  levels_map <- c(
    confirmado = "confirmed", retractado = "retracted", pendiente = "pending"
  )
  flu <- tbl_now(cases,
    event_date = onset, report_date = visit,
    validation_date = result, validation_type = outcome,
    validation_levels = levels_map,
    data_type = "linelist", units = "days", verbose = FALSE
  )
  flagged <- censor_validation_delays_above(flu, 3, verbose = FALSE)

  out <- censor_delays(flagged, .delay > 0, to_delay = 0, verbose = FALSE)

  expect_equal(get_validation_levels(out), levels_map)
  expect_equal(get_is_censored_validation(out), ".is_censored_validation")
  expect_equal(
    out[[".is_censored_validation"]],
    flagged[[".is_censored_validation"]]
  )
  # The two censoring axes are independent: this touched only the report one.
  expect_equal(get_is_censored_report(out), ".is_censored_report")
})
