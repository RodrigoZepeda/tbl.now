# Tests for the after-holiday / after-weekend lag temporal effects
# (holiday_lags / weekend_lags in temporal_effects()).

# A small daily dataset around Christmas 2020 (Fri Dec 25) and New Year 2021
# (Fri Jan 1). With Sat/Sun weekends the first working day back is the Monday.
make_daily <- function(from = "2020-12-20", to = "2021-01-10") {
  days <- seq(as.Date(from), as.Date(to), by = "day")
  data.frame(onset = days, report = days, n = 1L)
}

xmas_cal <- function() {
  almanac::rcalendar(almanac::hol_christmas(), almanac::hol_new_years_day())
}

# ---------------------------------------------------------------------------
# Constructor + validation
# ---------------------------------------------------------------------------

test_that("temporal_effects() stores lag depths", {
  te <- temporal_effects(holidays = xmas_cal(), holiday_lags = 3, weekend_lags = 2)
  expect_identical(te@holiday_lags, 3L)
  expect_identical(te@weekend_lags, 2L)
})

test_that("lag depths default to 0 (off)", {
  te <- temporal_effects()
  expect_identical(te@holiday_lags, 0L)
  expect_identical(te@weekend_lags, 0L)
})

test_that("holiday_lags without a holidays calendar errors", {
  expect_error(temporal_effects(holiday_lags = 2), "holidays")
})

test_that("weekend_lags does not require a holidays calendar", {
  expect_no_error(temporal_effects(weekend_lags = 2))
})

test_that("non-integer / negative lag depths error", {
  expect_error(temporal_effects(weekend_lags = 1.5), "non-negative integer")
  expect_error(temporal_effects(weekend_lags = -1), "non-negative integer")
  expect_error(temporal_effects(weekend_lags = c(1, 2)), "non-negative integer")
})

# ---------------------------------------------------------------------------
# Column materialisation + correctness
# ---------------------------------------------------------------------------

test_that("after-holiday lags create the right indicator columns", {
  skip_if_not_installed("almanac")
  x <- tbl_now(make_daily(), event_date = "onset", report_date = "report",
               case_count = "n", data_type = "count-incidence", verbose = FALSE) |>
    add_temporal_effects(temporal_effects(holidays = xmas_cal(), holiday_lags = 3))
  d <- as.data.frame(compute_temporal_effects(x))

  expect_true(all(
    c(".event_holiday_lag_1", ".event_holiday_lag_2", ".event_holiday_lag_3")
    %in% names(d)
  ))

  # Christmas (Fri Dec 25) -> first working day back is Mon Dec 28 (lag 1),
  # Tue 29 (lag 2), Wed 30 (lag 3). Weekend (Sat/Sun) is skipped.
  lag1 <- d$onset[d$.event_holiday_lag_1 == 1]
  lag2 <- d$onset[d$.event_holiday_lag_2 == 1]
  lag3 <- d$onset[d$.event_holiday_lag_3 == 1]
  expect_true(as.Date("2020-12-28") %in% lag1)
  expect_true(as.Date("2020-12-29") %in% lag2)
  expect_true(as.Date("2020-12-30") %in% lag3)
  # New Year (Fri Jan 1) -> Mon Jan 4 is lag 1
  expect_true(as.Date("2021-01-04") %in% lag1)
  # The holiday itself is not an after-effect
  expect_false(as.Date("2020-12-25") %in% c(lag1, lag2, lag3))
  # Weekend days never carry a working-day indicator
  expect_equal(sum(d$.event_holiday_lag_1[weekdays(d$onset) %in% c("Saturday", "Sunday")]), 0)
})

test_that("after-weekend lags flag the first working day(s) after a weekend", {
  x <- tbl_now(make_daily(), event_date = "onset", report_date = "report",
               case_count = "n", data_type = "count-incidence", verbose = FALSE) |>
    add_temporal_effects(temporal_effects(weekend_lags = 1))
  d <- as.data.frame(compute_temporal_effects(x))

  # weekend_lag_1 with no holidays = every Monday
  flagged <- d$onset[d$.event_weekend_lag_1 == 1]
  expect_true(all(weekdays(flagged) == "Monday"))
  expect_true(as.Date("2020-12-21") %in% flagged)
  expect_true(as.Date("2020-12-28") %in% flagged)
})

test_that("weekend lags skip holidays when counting working days", {
  skip_if_not_installed("almanac")
  # New Year's Day is Fri Jan 1 2021. The weekend Sat Jan 2/Sun Jan 3 is followed
  # by Mon Jan 4, which is the first working day -> weekend_lag_1.
  x <- tbl_now(make_daily(), event_date = "onset", report_date = "report",
               case_count = "n", data_type = "count-incidence", verbose = FALSE) |>
    add_temporal_effects(temporal_effects(holidays = xmas_cal(), weekend_lags = 1))
  d <- as.data.frame(compute_temporal_effects(x))
  expect_true(as.Date("2021-01-04") %in% d$onset[d$.event_weekend_lag_1 == 1])
})

test_that("get_temporal_effect_cols() reports the lag columns", {
  x <- tbl_now(make_daily(), event_date = "onset", report_date = "report",
               case_count = "n", data_type = "count-incidence", verbose = FALSE) |>
    add_temporal_effects(temporal_effects(holidays = xmas_cal(),
                                          holiday_lags = 2, weekend_lags = 1))
  cols <- get_temporal_effect_cols(compute_temporal_effects(x))
  expect_true(all(c(".event_holiday_lag_1", ".event_holiday_lag_2",
                    ".event_weekend_lag_1") %in% cols))
})

# ---------------------------------------------------------------------------
# Report-date association (date_type = "report_date")
# ---------------------------------------------------------------------------

test_that("lag effects can be attached to the report date", {
  skip_if_not_installed("almanac")
  x <- tbl_now(make_daily(), event_date = "onset", report_date = "report",
               case_count = "n", data_type = "count-incidence", verbose = FALSE) |>
    add_temporal_effects(temporal_effects(holidays = xmas_cal(), holiday_lags = 2),
                         date_type = "report_date")
  cols <- get_temporal_effect_cols(compute_temporal_effects(x))
  expect_true(all(c(".report_holiday_lag_1", ".report_holiday_lag_2") %in% cols))
  expect_false(any(grepl("^\\.event_holiday_lag", cols)))
})

test_that("event and report lag effects can coexist on one object", {
  skip_if_not_installed("almanac")
  cal <- xmas_cal()
  x <- tbl_now(make_daily(), event_date = "onset", report_date = "report",
               case_count = "n", data_type = "count-incidence", verbose = FALSE) |>
    add_temporal_effects(temporal_effects(holidays = cal, holiday_lags = 1),
                         date_type = "event_date") |>
    add_temporal_effects(temporal_effects(weekend_lags = 1),
                         date_type = "report_date")
  cols <- get_temporal_effect_cols(compute_temporal_effects(x))
  expect_true(".event_holiday_lag_1" %in% cols)
  expect_true(".report_weekend_lag_1" %in% cols)
})

# ---------------------------------------------------------------------------
# Overwrite guard
# ---------------------------------------------------------------------------

test_that("re-computing lag columns without overwrite errors", {
  x <- tbl_now(make_daily(), event_date = "onset", report_date = "report",
               case_count = "n", data_type = "count-incidence", verbose = FALSE) |>
    add_temporal_effects(temporal_effects(weekend_lags = 1))
  xc <- compute_temporal_effects(x)
  expect_error(compute_temporal_effects(xc), "already exist")
  expect_no_error(compute_temporal_effects(xc, overwrite = TRUE))
})

# ---------------------------------------------------------------------------
# Converters carry the lag columns
# ---------------------------------------------------------------------------

test_that("converters carry the after-lag columns", {
  skip_if_not_installed("data.table")
  x <- tbl_now(make_daily(), event_date = "onset", report_date = "report",
               case_count = "n", data_type = "count-incidence", verbose = FALSE) |>
    add_temporal_effects(temporal_effects(holidays = xmas_cal(),
                                          holiday_lags = 2, weekend_lags = 1))
  dt <- tbl_now_to_data_table(x, verbose = FALSE)
  expect_true(all(c(".event_holiday_lag_1", ".event_holiday_lag_2",
                    ".event_weekend_lag_1") %in% names(dt)))
})
