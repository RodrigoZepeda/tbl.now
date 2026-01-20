
# ============================================================================
# Tests for add_temporal_effects.data.frame()
# ============================================================================

test_that("add_temporal_effects.data.frame returns data.frame unchanged when t_effects is NULL", {
  df <- data.frame(
    date = as.Date(c("2020-01-01", "2020-01-08", "2020-01-15")),
    value = 1:3
  )

  result <- add_temporal_effects.data.frame(
    df,
    t_effects = NULL,
    date_col = "date",
    numeric_col = NULL
  )

  expect_identical(result, df)
})

test_that("add_temporal_effects.data.frame fails with non-temporal_effects object", {
  df <- data.frame(
    date = as.Date(c("2020-01-01", "2020-01-08")),
    value = 1:2
  )

  expect_error(
    add_temporal_effects.data.frame(
      df,
      t_effects = list(day_of_week = TRUE),
      date_col = "date"
    ),
    "should be a.*temporal_effects.*object"
  )
})

test_that("add_temporal_effects.data.frame fails when date_col doesn't exist", {
  df <- data.frame(
    date = as.Date(c("2020-01-01", "2020-01-08")),
    value = 1:2
  )

  t_eff <- temporal_effects(day_of_week = TRUE)

  expect_error(
    add_temporal_effects.data.frame(
      df,
      t_effects = t_eff,
      date_col = "nonexistent_col"
    ),
    "not a column in"
  )
})

test_that("add_temporal_effects.data.frame fails when numeric_col doesn't exist", {
  df <- data.frame(
    date = as.Date(c("2020-01-01", "2020-01-08")),
    value = 1:2
  )

  t_eff <- temporal_effects(seasons = 52)

  expect_error(
    add_temporal_effects.data.frame(
      df,
      t_effects = t_eff,
      date_col = "date",
      numeric_col = "nonexistent_numeric"
    ),
    "not a column in"
  )
})

test_that("add_temporal_effects.data.frame fails when column already exists and overwrite = FALSE", {
  df <- data.frame(
    date = as.Date(c("2020-01-01", "2020-01-08")),
    value = 1:2,
    .date_day_of_week = c(3, 4)  # Pre-existing column
  )

  t_eff <- temporal_effects(day_of_week = TRUE)

  expect_error(
    add_temporal_effects.data.frame(
      df,
      t_effects = t_eff,
      date_col = "date",
      name_prefix = ".date",
      overwrite = FALSE
    ),
    "already exists.*overwrite = TRUE"
  )
})

test_that("add_temporal_effects.data.frame overwrites when overwrite = TRUE", {
  df <- data.frame(
    date = as.Date(c("2020-01-01", "2020-01-08")),
    value = 1:2,
    .date_day_of_week = c(999, 999)  # Pre-existing column with wrong values
  )

  t_eff <- temporal_effects(day_of_week = TRUE)

  result <- add_temporal_effects.data.frame(
    df,
    t_effects = t_eff,
    date_col = "date",
    name_prefix = ".date",
    overwrite = TRUE
  )

  # Should have overwritten with correct values
  expect_true(".date_day_of_week" %in% names(result))
  expect_false(any(result$.date_day_of_week == 999))
  expect_true(all(result$.date_day_of_week %in% 1:7))
})

test_that("add_temporal_effects.data.frame weekend effect works with custom weekend_days", {
  df <- data.frame(
    date = as.Date(c("2020-04-17", "2020-04-18", "2020-04-19", "2020-04-20"))
  )

  # Friday-Saturday weekend (Middle East)
  t_eff <- temporal_effects(weekend = TRUE)

  result <- add_temporal_effects.data.frame(
    df,
    t_effects = t_eff,
    date_col = "date",
    name_prefix = ".date",
    weekend_days = c("Fri", "Sat")
  )

  expect_true(".date_weekend" %in% names(result))

  # 2020-04-17 is Friday (weekend)
  # 2020-04-18 is Saturday (weekend)
  # 2020-04-19 is Sunday (weekday)
  # 2020-04-20 is Monday (weekday)
  expect_equal(result$.date_weekend, c(1, 1, 0, 0))
})

test_that("add_temporal_effects.data.frame month_of_year cycles correctly", {
  df <- data.frame(
    date = seq(as.Date("2020-01-15"), as.Date("2020-12-15"), by = "month")
  )

  t_eff <- temporal_effects(month_of_year = TRUE)

  result <- add_temporal_effects.data.frame(
    df,
    t_effects = t_eff,
    date_col = "date",
    name_prefix = ".date"
  )

  # Month effect should start at 1 for the first month
  expect_equal(result$.date_month_of_year[1], 1)

  # Should cycle through 1-12
  expect_true(all(result$.date_month_of_year %in% 1:12))

  # Should be sequential (given monthly data starting in Jan)
  expect_equal(result$.date_month_of_year, 1:12)
})

test_that("add_temporal_effects.data.frame month_of_year handles year boundary", {
  df <- data.frame(
    date = as.Date(c("2020-11-15", "2020-12-15", "2021-01-15", "2021-02-15"))
  )

  t_eff <- temporal_effects(month_of_year = TRUE)

  result <- add_temporal_effects.data.frame(
    df,
    t_effects = t_eff,
    date_col = "date",
    name_prefix = ".date"
  )

  # Should cycle: starts at Nov (1), Dec (2), Jan (3), Feb (4)
  expect_equal(result$.date_month_of_year, c(1, 2, 3, 4))
})

test_that("add_temporal_effects.data.frame week_of_year cycles correctly", {
  df <- data.frame(
    date = seq(as.Date("2020-01-08"), as.Date("2020-12-30"), by = "week")
  )

  t_eff <- temporal_effects(week_of_year = TRUE)

  result <- add_temporal_effects.data.frame(
    df,
    t_effects = t_eff,
    date_col = "date",
    name_prefix = ".date"
  )

  # Week effect should start at 1
  expect_equal(result$.date_week_of_year[1], 1)

  # Should cycle through 1-52 (week 53 is collapsed to 1)
  expect_true(all(result$.date_week_of_year %in% 1:52))
})

test_that("add_temporal_effects.data.frame week_of_year handles year boundary", {
  df <- data.frame(
    date = as.Date(c("2020-12-23", "2020-12-30", "2021-01-06", "2021-01-13"))
  )

  t_eff <- temporal_effects(week_of_year = TRUE)

  result <- add_temporal_effects.data.frame(
    df,
    t_effects = t_eff,
    date_col = "date",
    name_prefix = ".date"
  )

  # Should wrap around (modulo 52)
  expect_true(all(result$.date_week_of_year %in% 1:52))
})

test_that("add_temporal_effects.data.frame seasons creates sin and cos columns", {
  df <- data.frame(
    date = as.Date(c("2020-01-01", "2020-07-01")),
    numeric_col = c(1, 183)
  )

  t_eff <- temporal_effects(seasons = 365)

  result <- add_temporal_effects.data.frame(
    df,
    t_effects = t_eff,
    date_col = "date",
    numeric_col = "numeric_col",
    name_prefix = ".date"
  )

  expect_true(".date_season_365_cos" %in% names(result))
  expect_true(".date_season_365_sin" %in% names(result))
})

test_that("add_temporal_effects.data.frame multiple seasons create multiple columns", {
  df <- data.frame(
    date = as.Date(c("2020-01-01", "2020-07-01")),
    numeric_col = c(1, 183)
  )

  t_eff <- temporal_effects(seasons = c(7, 52, 365))

  result <- add_temporal_effects.data.frame(
    df,
    t_effects = t_eff,
    date_col = "date",
    numeric_col = "numeric_col",
    name_prefix = ".date"
  )

  # Should have 2 columns per season (sin and cos)
  expect_true(".date_season_7_cos" %in% names(result))
  expect_true(".date_season_7_sin" %in% names(result))
  expect_true(".date_season_52_cos" %in% names(result))
  expect_true(".date_season_52_sin" %in% names(result))
  expect_true(".date_season_365_cos" %in% names(result))
  expect_true(".date_season_365_sin" %in% names(result))
})

test_that("add_temporal_effects.data.frame seasons fail when column exists and overwrite = FALSE", {
  df <- data.frame(
    date = as.Date(c("2020-01-01", "2020-07-01")),
    numeric_col = c(1, 183),
    .date_season_365_cos = c(1, 1)  # Pre-existing
  )

  t_eff <- temporal_effects(seasons = 365)

  expect_error(
    add_temporal_effects.data.frame(
      df,
      t_effects = t_eff,
      date_col = "date",
      numeric_col = "numeric_col",
      name_prefix = ".date",
      overwrite = FALSE
    ),
    "already exist.*overwrite = TRUE"
  )
})

test_that("add_temporal_effects.data.frame holidays fails without almanac", {
  if (requireNamespace("almanac", quietly = TRUE)) {
    skip("Package 'almanac' is installed")
  }

  df <- data.frame(
    date = as.Date(c("2020-01-01", "2020-12-25"))
  )

  # Create a mock temporal_effects with holidays
  # This will fail because almanac is not installed
  expect_error(
    {
      # This should fail earlier at temporal_effects() creation
      t_eff <- temporal_effects(holidays = "some_calendar")
    },
    "almanac"
  )
})

test_that("add_temporal_effects.data.frame holidays creates binary column", {
  skip_if_not_installed("almanac")

  df <- data.frame(
    date = as.Date(c("2020-12-24", "2020-12-25", "2020-12-26"))
  )

  cal <- almanac::rcalendar(almanac::hol_christmas())
  t_eff <- temporal_effects(holidays = cal)

  result <- add_temporal_effects.data.frame(
    df,
    t_effects = t_eff,
    date_col = "date",
    name_prefix = ".date"
  )

  expect_true(".date_holiday" %in% names(result))

  # Christmas (2020-12-25) should be 1, others 0
  expect_equal(result$.date_holiday, c(0, 1, 0))
})

test_that("add_temporal_effects.data.frame holidays fail when column exists and overwrite = FALSE", {
  skip_if_not_installed("almanac")

  df <- data.frame(
    date = as.Date(c("2020-12-25")),
    .date_holiday = 999  # Pre-existing
  )

  cal <- almanac::rcalendar(almanac::hol_christmas())
  t_eff <- temporal_effects(holidays = cal)

  expect_error(
    add_temporal_effects.data.frame(
      df,
      t_effects = t_eff,
      date_col = "date",
      name_prefix = ".date",
      overwrite = FALSE
    ),
    "already exists.*overwrite = TRUE"
  )
})

test_that("add_temporal_effects.data.frame custom name_prefix works", {
  df <- data.frame(
    my_date = as.Date(c("2020-01-01", "2020-01-08"))
  )

  t_eff <- temporal_effects(day_of_week = TRUE)

  result <- add_temporal_effects.data.frame(
    df,
    t_effects = t_eff,
    date_col = "my_date",
    name_prefix = ".custom"
  )

  expect_true(".custom_day_of_week" %in% names(result))
  expect_false(".my_date_day_of_week" %in% names(result))
})

# ============================================================================
# Tests for add_temporal_effects.tbl_now()
# ============================================================================

test_that("add_temporal_effects.tbl_now returns unchanged when t_effects is NULL", {
  data(denguedat)

  df_now <- denguedat %>%
    tbl_now(
      event_date = "onset_week",
      report_date = "report_week",
      verbose = FALSE,
      t_effects = NULL
    )

  result <- add_temporal_effects(df_now, t_effects = NULL)

  expect_identical(result, df_now)
})

test_that("add_temporal_effects.tbl_now adds to event_date by default", {
  data(denguedat)

  df_now <- denguedat %>%
    tbl_now(
      event_date = "onset_week",
      report_date = "report_week",
      verbose = FALSE
    )

  t_eff <- temporal_effects(day_of_week = TRUE)

  result <- add_temporal_effects(df_now, t_effects = t_eff)

  # Should add effect to event_date
  expect_true(".event_day_of_week" %in% names(result))
  expect_false(".report_day_of_week" %in% names(result))
})

test_that("add_temporal_effects.tbl_now adds to report_date when specified", {
  data(denguedat)

  df_now <- denguedat %>%
    tbl_now(
      event_date = "onset_week",
      report_date = "report_week",
      verbose = FALSE
    )

  t_eff <- temporal_effects(day_of_week = TRUE)

  result <- add_temporal_effects(df_now, t_effects = t_eff, date_type = "report_date")

  # Should add effect to report_date
  expect_true(".report_day_of_week" %in% names(result))
  expect_false(".event_day_of_week" %in% names(result))
})

test_that("add_temporal_effects.tbl_now fails with invalid date_type", {
  data(denguedat)

  df_now <- denguedat %>%
    tbl_now(
      event_date = "onset_week",
      report_date = "report_week",
      verbose = FALSE
    )

  t_eff <- temporal_effects(day_of_week = TRUE)

  expect_error(
    add_temporal_effects(df_now, t_effects = t_eff, date_type = "invalid"),
    "Invalid.*date_type"
  )
})

test_that("add_temporal_effects.tbl_now updates temporal_effects attribute", {
  data(denguedat)

  df_now <- denguedat %>%
    tbl_now(
      event_date = "onset_week",
      report_date = "report_week",
      verbose = FALSE
    )

  # Initially no temporal effects
  expect_equal(get_temporal_effects(df_now), character(0))

  t_eff <- temporal_effects(day_of_week = TRUE, week_of_year = TRUE)

  result <- add_temporal_effects(df_now, t_effects = t_eff)

  # Should update attribute
  temporal_cols <- get_temporal_effects(result)
  expect_gt(length(temporal_cols), 0)
  expect_true(".event_day_of_week" %in% temporal_cols)
  expect_true(".event_week_of_year" %in% temporal_cols)
})

test_that("add_temporal_effects.tbl_now appends to existing temporal_effects", {
  data(denguedat)

  df_now <- denguedat %>%
    tbl_now(
      event_date = "onset_week",
      report_date = "report_week",
      verbose = FALSE
    ) %>%
    add_temporal_effects(temporal_effects(day_of_week = TRUE))

  # Add more effects
  result <- add_temporal_effects(df_now, temporal_effects(week_of_year = TRUE))

  temporal_cols <- get_temporal_effects(result)

  # Should have both old and new
  expect_true(".event_day_of_week" %in% temporal_cols)
  expect_true(".event_week_of_year" %in% temporal_cols)
})

test_that("add_temporal_effects.tbl_now uses .event_num for seasons on event_date", {
  data(denguedat)

  df_now <- denguedat %>%
    tbl_now(
      event_date = "onset_week",
      report_date = "report_week",
      verbose = FALSE
    )

  t_eff <- temporal_effects(seasons = 52)

  result <- add_temporal_effects(df_now, t_effects = t_eff, date_type = "event_date")

  # Should create season columns using .event_num
  expect_true(".event_season_52_cos" %in% names(result))
  expect_true(".event_season_52_sin" %in% names(result))
})

test_that("add_temporal_effects.tbl_now uses .report_num for seasons on report_date", {
  data(denguedat)

  df_now <- denguedat %>%
    tbl_now(
      event_date = "onset_week",
      report_date = "report_week",
      verbose = FALSE
    )

  t_eff <- temporal_effects(seasons = 52)

  result <- add_temporal_effects(df_now, t_effects = t_eff, date_type = "report_date")

  # Should create season columns using .report_num
  expect_true(".report_season_52_cos" %in% names(result))
  expect_true(".report_season_52_sin" %in% names(result))
})

test_that("add_temporal_effects.tbl_now preserves tbl_now class", {
  data(denguedat)

  df_now <- denguedat %>%
    tbl_now(
      event_date = "onset_week",
      report_date = "report_week",
      verbose = FALSE
    )

  t_eff <- temporal_effects(day_of_week = TRUE)

  result <- add_temporal_effects(df_now, t_effects = t_eff)

  expect_s3_class(result, "tbl_now")
  expect_true(is_tbl_now(result))
})

test_that("add_temporal_effects.tbl_now preserves all tbl_now attributes", {
  data(denguedat)

  df_now <- denguedat %>%
    tbl_now(
      event_date = "onset_week",
      report_date = "report_week",
      strata = "gender",
      verbose = FALSE
    )

  t_eff <- temporal_effects(week_of_year = TRUE)

  result <- add_temporal_effects(df_now, t_effects = t_eff)

  # Check attributes preserved
  expect_equal(get_event_date(result), get_event_date(df_now))
  expect_equal(get_report_date(result), get_report_date(df_now))
  expect_equal(get_strata(result), get_strata(df_now))
  expect_equal(get_now(result), get_now(df_now))
  expect_equal(get_data_type(result), get_data_type(df_now))
})

test_that("add_temporal_effects.tbl_now works with count data", {
  data(denguedat)

  df_now <- denguedat %>%
    tbl_now(
      event_date = "onset_week",
      report_date = "report_week",
      verbose = FALSE
    ) %>%
    to_count(to = "count-incidence")

  t_eff <- temporal_effects(day_of_week = TRUE)

  result <- add_temporal_effects(df_now, t_effects = t_eff)

  expect_s3_class(result, "tbl_now")
  expect_equal(get_data_type(result), "count-incidence")
  expect_true(".event_day_of_week" %in% names(result))
})

test_that("add_temporal_effects.tbl_now weekend effect respects weekend_days parameter", {
  data(denguedat)

  df_now <- denguedat %>%
    tbl_now(
      event_date = "onset_week",
      report_date = "report_week",
      verbose = FALSE
    )

  t_eff <- temporal_effects(weekend = TRUE)

  # Use Friday-Saturday weekend
  result <- add_temporal_effects(
    df_now,
    t_effects = t_eff,
    weekend_days = c("Fri", "Sat")
  )

  expect_true(".event_weekend" %in% names(result))
  expect_true(all(result$.event_weekend %in% c(0, 1)))
})

# ============================================================================
# Edge cases and integration
# ============================================================================

test_that("add_temporal_effects handles all effects together on tbl_now", {
  skip_if_not_installed("almanac")

  data(denguedat)

  df_now <- denguedat %>%
    tbl_now(
      event_date = "onset_week",
      report_date = "report_week",
      verbose = FALSE
    )

  t_eff <- temporal_effects(
    day_of_week = TRUE,
    weekend = TRUE,
    day_of_month = TRUE,
    month_of_year = TRUE,
    week_of_year = TRUE,
    seasons = c(52, 365),
    holidays = almanac::cal_us_federal()
  )

  result <- add_temporal_effects(df_now, t_effects = t_eff)

  # Check all effects were added
  temporal_cols <- get_temporal_effects(result)
  expect_true(".event_day_of_week" %in% temporal_cols)
  expect_true(".event_weekend" %in% temporal_cols)
  expect_true(".event_day_of_month" %in% temporal_cols)
  expect_true(".event_month_of_year" %in% temporal_cols)
  expect_true(".event_week_of_year" %in% temporal_cols)
  expect_true(".event_season_52_cos" %in% temporal_cols)
  expect_true(".event_season_52_sin" %in% temporal_cols)
  expect_true(".event_season_365_cos" %in% temporal_cols)
  expect_true(".event_season_365_sin" %in% temporal_cols)
  expect_true(".event_holiday" %in% temporal_cols)
})

test_that("add_temporal_effects can add to both event and report dates separately", {
  data(denguedat)

  df_now <- denguedat %>%
    tbl_now(
      event_date = "onset_week",
      report_date = "report_week",
      verbose = FALSE
    )

  t_eff_event <- temporal_effects(day_of_week = TRUE)
  t_eff_report <- temporal_effects(weekend = TRUE)

  result <- df_now %>%
    add_temporal_effects(t_eff_event, date_type = "event_date") %>%
    add_temporal_effects(t_eff_report, date_type = "report_date")

  # Should have effects for both dates
  expect_true(".event_day_of_week" %in% names(result))
  expect_true(".report_weekend" %in% names(result))

  temporal_cols <- get_temporal_effects(result)
  expect_true(".event_day_of_week" %in% temporal_cols)
  expect_true(".report_weekend" %in% temporal_cols)
})

test_that("add_temporal_effects preserves data integrity", {
  data(denguedat)

  df_now <- denguedat %>%
    tbl_now(
      event_date = "onset_week",
      report_date = "report_week",
      verbose = FALSE
    )

  original_rows <- nrow(df_now)
  original_event <- df_now$onset_week

  t_eff <- temporal_effects(day_of_week = TRUE, week_of_year = TRUE)
  result <- add_temporal_effects(df_now, t_effects = t_eff)

  # Data should be unchanged except for new columns
  expect_equal(nrow(result), original_rows)
  expect_equal(result$onset_week, original_event)
})
