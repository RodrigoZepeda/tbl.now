# Test file for align_week.R functions

# Setup test data ----
setup_test_data <- function() {
  list(
    # Basic date data
    basic_dates = data.frame(
      date = as.Date(c("2020-10-31", "2022-11-07", "2022-11-13"))
    ),

    # Data with different weekdays
    mixed_weekdays = data.frame(
      date = as.Date(c(
        "2022-11-07", "2022-11-08", "2022-11-09",
        "2022-11-10", "2022-11-11", "2022-11-12", "2022-11-13"
      ))
    ),

    # Year boundary data
    year_boundary = data.frame(
      date = as.Date(c("2022-12-26", "2023-01-02", "2023-01-09"))
    ),

    # Leap year data
    leap_year = data.frame(
      date = as.Date(c("2020-02-24", "2020-02-29", "2020-03-07"))
    ),

    # Single observation
    single_obs = data.frame(
      date = as.Date("2022-11-09")
    ),

    # Week/year data for week_2_date
    week_year_data = data.frame(
      epidemiological_week = 1:5,
      epidemiological_year = rep(2024, 5)
    ),

    # Week/year with duplicates
    week_year_duplicates = data.frame(
      week_col = c(5, 5, 6),
      year_col = c(2024, 2024, 2024)
    )
  )
}

# ============================================================================
# Tests for align_weeks.data.frame()
# ============================================================================

test_that("align_weeks returns a data.frame with new aligned date column", {
  test_data <- setup_test_data()

  out <- align_weeks(test_data$basic_dates, date_col = date)

  expect_s3_class(out, "data.frame")
  expect_true("date_aligned" %in% names(out))
  expect_equal(ncol(out), 2) # Original + aligned
})

test_that("align_weeks works with and without quotes", {
  test_data <- setup_test_data()

  expect_equal(
    align_weeks(test_data$basic_dates, date_col = "date"),
    align_weeks(test_data$basic_dates, date_col = date)
  )
})

test_that("align_weeks aligns dates to the specified weekday", {
  test_data <- setup_test_data()

  # Align to Sunday (1)
  out_sun <- align_weeks(test_data$mixed_weekdays, date_col = date, align_on_day = 1)
  expect_true(all(lubridate::wday(out_sun$date_aligned) == 1))

  # Align to Monday (2)
  out_mon <- align_weeks(test_data$mixed_weekdays, date_col = date, align_on_day = 2)
  expect_true(all(lubridate::wday(out_mon$date_aligned) == 2))

  # Align to Tuesday (3)
  out_tue <- align_weeks(test_data$mixed_weekdays, date_col = date, align_on_day = 3)
  expect_true(all(lubridate::wday(out_tue$date_aligned) == 3))
})

test_that("align_weeks supports both epi and iso week types", {
  test_data <- setup_test_data()

  out_epi <- align_weeks(test_data$year_boundary, date_col = date, type = "epi")
  out_iso <- align_weeks(test_data$year_boundary, date_col = date, type = "iso")

  expect_s3_class(out_epi, "data.frame")
  expect_s3_class(out_iso, "data.frame")

  # They should produce valid dates
  expect_true(all(!is.na(out_epi$date_aligned)))
  expect_true(all(!is.na(out_iso$date_aligned)))

  # They may differ for year boundaries
  # (not testing exact difference, just that both work)
})

test_that("align_weeks errors with incorrect type", {
  test_data <- setup_test_data()

  expect_error(
    align_weeks(test_data$basic_dates, date_col = date, type = "something"),
    "Invalid type"
  )

  expect_error(
    align_weeks(test_data$basic_dates, date_col = date, type = "gregorian"),
    "Invalid type"
  )
})

test_that("align_weeks does not modify original columns", {
  test_data <- setup_test_data()

  out <- align_weeks(test_data$basic_dates, date_col = date)

  expect_true("date" %in% names(out))
  expect_true("date_aligned" %in% names(out))
  expect_false("week_col" %in% names(out))
  expect_false("year_col" %in% names(out))

  # Original dates should be unchanged
  expect_equal(out$date, test_data$basic_dates$date)
})

test_that("align_weeks works with custom new_date_col name", {
  test_data <- setup_test_data()

  out <- align_weeks(test_data$basic_dates,
    date_col = date,
    new_date_col = "custom_name"
  )

  expect_true("custom_name" %in% names(out))
  expect_false("date_aligned" %in% names(out))
})

test_that("align_weeks handles single observation", {
  test_data <- setup_test_data()

  out <- align_weeks(test_data$single_obs, date_col = date)

  expect_equal(nrow(out), 1)
  expect_true(!is.na(out$date_aligned))
  expect_equal(lubridate::wday(out$date_aligned), 1) # Sunday
})

test_that("align_weeks handles leap years correctly", {
  test_data <- setup_test_data()

  out <- align_weeks(test_data$leap_year, date_col = date)

  expect_equal(nrow(out), 3)
  expect_true(all(!is.na(out$date_aligned)))
  expect_true(all(lubridate::wday(out$date_aligned) == 1))
})

test_that("align_weeks fails with multiple date columns specified", {
  test_data <- setup_test_data()
  df_multi <- test_data$basic_dates
  df_multi$date2 <- df_multi$date + 7

  expect_error(
    align_weeks(df_multi, date_col = c(date, date2)),
    "Can only operate on one column at a time"
  )
})

test_that("align_weeks fails when date column doesn't exist", {
  test_data <- setup_test_data()

  expect_error(
    align_weeks(test_data$basic_dates, date_col = nonexistent),
    "Can't select columns that don't exist|object 'nonexistent' not found"
  )
})

# ============================================================================
# Tests for align_weeks.tbl_now()
# ============================================================================

test_that("align_weeks.tbl_now preserves tbl_now class", {
  data(flusight)

  flu_tbl <- tbl_now(flusight[1:100, ],
    event_date = "target_end_date",
    report_date = "as_of",
    case_count = "observation",
    strata = "location_name",
    data_type = "count-cumulative",
    report_units = "weeks",
    event_units = "weeks",
    verbose = FALSE
  )

  result <- align_weeks(flu_tbl)

  expect_s3_class(result, "tbl_now")
  expect_true(is_tbl_now(result))
})

test_that("align_weeks.tbl_now aligns both event and report dates", {
  data(flusight)

  flu_tbl <- tbl_now(flusight[1:100, ],
    event_date = "target_end_date",
    report_date = "as_of",
    case_count = "observation",
    strata = "location_name",
    data_type = "count-cumulative",
    report_units = "weeks",
    event_units = "weeks",
    verbose = FALSE
  )

  result <- align_weeks(flu_tbl, align_on_day = 1)

  # Both dates should be aligned to Sunday (1)
  expect_true(all(lubridate::wday(result[[get_event_date(result)]]) == 1))
  expect_true(all(lubridate::wday(result[[get_report_date(result)]]) == 1))
})

test_that("align_weeks.tbl_now produces integer delays", {
  data(flusight)

  flu_tbl <- tbl_now(flusight[1:100, ],
    event_date = "target_end_date",
    report_date = "as_of",
    case_count = "observation",
    strata = "location_name",
    data_type = "count-cumulative",
    report_units = "weeks",
    event_units = "weeks",
    verbose = FALSE
  )

  # Before alignment, there might be decimal delays
  # After alignment, all delays should be integers (in weeks)
  result <- align_weeks(flu_tbl)

  # Check that delays are all integers (or very close to integers)
  expect_true(all(abs(result$.delay - round(result$.delay)) < 1e-10))
})

test_that("align_weeks.tbl_now preserves all attributes", {
  data(denguedat)

  dengue_tbl <- tbl_now(denguedat[1:100, ],
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    strata = "gender",
    verbose = FALSE
  )

  result <- align_weeks(dengue_tbl)

  expect_equal(get_strata(result), get_strata(dengue_tbl))
  expect_equal(get_data_type(result), get_data_type(dengue_tbl))
  expect_equal(get_event_units(result), get_event_units(dengue_tbl))
  expect_equal(get_report_units(result), get_report_units(dengue_tbl))
})

test_that("align_weeks.tbl_now works with different align_on_day values", {
  data(denguedat)

  dengue_tbl <- tbl_now(denguedat[1:1000, ],
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    verbose = FALSE
  )

  # Test multiple alignment days
  for (day in 1:7) {
    result <- align_weeks(dengue_tbl, align_on_day = day)
    expect_true(all(lubridate::wday(result[[get_event_date(result)]]) == day))
    expect_true(all(lubridate::wday(result[[get_report_date(result)]]) == day))
  }
})

test_that("align_weeks.tbl_now maintains data integrity", {
  data(denguedat)

  dengue_tbl <- tbl_now(denguedat[1:100, ],
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    strata = "gender",
    verbose = FALSE
  )

  original_rows <- nrow(dengue_tbl)
  result <- align_weeks(dengue_tbl)

  expect_equal(nrow(result), original_rows)
  expect_true(all(names(dengue_tbl) %in% names(result)))
})

test_that("align_weeks.tbl_now works with both epi and iso types", {
  data(denguedat)

  dengue_tbl <- tbl_now(denguedat[1:50, ],
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    verbose = FALSE
  )

  result_epi <- align_weeks(dengue_tbl, type = "epi")
  result_iso <- align_weeks(dengue_tbl, type = "iso")

  expect_s3_class(result_epi, "tbl_now")
  expect_s3_class(result_iso, "tbl_now")

  # Both should have integer delays
  expect_true(all(abs(result_epi$.delay - round(result_epi$.delay)) < 1e-10))
  expect_true(all(abs(result_iso$.delay - round(result_iso$.delay)) < 1e-10))
})

# ============================================================================
# Tests for week_2_date()
# ============================================================================

test_that("week_2_date creates a date column", {
  test_data <- setup_test_data()

  out <- week_2_date(test_data$week_year_data,
    week_col = "epidemiological_week",
    year_col = "epidemiological_year"
  )

  expect_s3_class(out, "data.frame")
  expect_true("date" %in% names(out))
  expect_s3_class(out$date, "Date")
})

test_that("week_2_date aligns to the correct weekday", {
  test_data <- setup_test_data()

  # Test different alignment days
  for (day in 1:7) {
    out <- week_2_date(test_data$week_year_data,
      week_col = "epidemiological_week",
      year_col = "epidemiological_year",
      align_on_day = day
    )

    expect_true(all(lubridate::wday(out$date) == day))
  }
})

test_that("week_2_date works for epiweek vs isoweek", {
  df <- data.frame(
    week_col = 1,
    year_col = 2023
  )

  out_epi <- week_2_date(df,
    week_col = "week_col",
    year_col = "year_col",
    week_fun = lubridate::epiweek,
    year_fun = lubridate::epiyear
  )

  out_iso <- week_2_date(df,
    week_col = "week_col",
    year_col = "year_col",
    week_fun = lubridate::isoweek,
    year_fun = lubridate::isoyear
  )

  # They often differ in early January
  expect_false(identical(out_epi$date, out_iso$date))
})

test_that("week_2_date merges correctly with duplicated rows", {
  test_data <- setup_test_data()

  out <- week_2_date(test_data$week_year_duplicates,
    week_col = "week_col",
    year_col = "year_col"
  )

  expect_equal(nrow(out), 3)
  expect_false(any(is.na(out$date)))
})

test_that("week_2_date fails when date_col_name already exists", {
  test_data <- setup_test_data()
  df_with_date <- test_data$week_year_data
  df_with_date$date <- as.Date("2024-01-01")

  expect_error(
    week_2_date(df_with_date,
      week_col = "epidemiological_week",
      year_col = "epidemiological_year",
      date_col_name = "date"
    ),
    "already in"
  )
})

test_that("week_2_date fails with multiple week columns", {
  df <- data.frame(
    week1 = 1:5,
    week2 = 2:6,
    year = rep(2024, 5)
  )

  expect_error(
    week_2_date(df, week_col = c("week1", "week2"), year_col = "year"),
    "Can only operate on one column at a time"
  )
})

test_that("week_2_date fails with multiple year columns", {
  df <- data.frame(
    week = 1:5,
    year1 = rep(2024, 5),
    year2 = rep(2025, 5)
  )

  expect_error(
    week_2_date(df, week_col = "week", year_col = c("year1", "year2")),
    "Can only operate on one column at a time"
  )
})

test_that("week_2_date handles year boundaries correctly", {
  # Week 52 and week 1 transitions
  year_boundary_weeks <- data.frame(
    week_col = c(51, 52, 1, 2),
    year_col = c(2023, 2023, 2024, 2024)
  )

  out <- week_2_date(year_boundary_weeks,
    week_col = "week_col",
    year_col = "year_col"
  )

  expect_equal(nrow(out), 4)
  expect_false(any(is.na(out$date)))

  # Dates should be in chronological order
  expect_true(all(diff(out$date) >= 0))
})

test_that("week_2_date custom date_col_name works", {
  test_data <- setup_test_data()

  out <- week_2_date(test_data$week_year_data,
    week_col = "epidemiological_week",
    year_col = "epidemiological_year",
    date_col_name = "my_date"
  )

  expect_true("my_date" %in% names(out))
  expect_false("date" %in% names(out))
})

# ============================================================================
# Integration tests
# ============================================================================

test_that("align_weeks integrates correctly with tbl_now workflow", {
  data(denguedat)

  # Create tbl_now, align weeks, then convert to count
  result <- denguedat[1:100, ] |>
    tbl_now(
      event_date = "onset_week",
      report_date = "report_week",
      strata = "gender",
      verbose = FALSE
    ) |>
    align_weeks() |>
    to_count(to = "count-incidence")

  expect_s3_class(result, "tbl_now")
  expect_equal(get_data_type(result), "count-incidence")

  # Delays should be integers
  expect_true(all(abs(result$.delay - round(result$.delay)) < 1e-10))
})

test_that("align_weeks preserves covariates and temporal effects", {
  data(denguedat)

  dengue_tbl <- denguedat[1:100, ] |>
    tbl_now(
      event_date = "onset_week",
      report_date = "report_week",
      strata = "gender",
      verbose = FALSE
    ) |>
    dplyr::mutate(temperature = rnorm(dplyr::n(), 25, 3)) |>
    add_covariates(temperature) |>
    add_temporal_effects(temporal_effects(week_of_year = TRUE))

  result <- align_weeks(dengue_tbl)

  expect_equal(get_covariates(result), "temperature")
  expect_true(length(get_temporal_effects(result)) > 0)
})

test_that("Example from vignette works correctly", {
  data(flusight)

  # This is the example from Example.Rmd
  flutbl <- tbl_now(flusight[1:1000, ],
    event_date = "target_end_date",
    report_date = "as_of",
    report_units = "weeks",
    event_units = "weeks",
    case_count = "observation",
    strata = c("location_name"),
    data_type = "count-cumulative",
    verbose = FALSE
  )

  # Before alignment, some delays might have decimals
  has_decimals_before <- any(abs(flutbl$.delay - round(flutbl$.delay)) > 1e-10)

  # Align the weeks
  flutbl_aligned <- flutbl |> align_weeks()

  # After alignment, all delays should be integers
  has_decimals_after <- any(abs(flutbl_aligned$.delay - round(flutbl_aligned$.delay)) > 1e-10)

  expect_s3_class(flutbl_aligned, "tbl_now")
  expect_false(has_decimals_after)
})
