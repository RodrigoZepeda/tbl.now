# Test file for time_cols_to_numeric.R functions

# Setup test data ----
setup_test_data <- function() {
  list(
    daily_data = data.frame(
      event_date = as.Date(c("2020-07-08", "2020-07-09", "2020-07-10", "2020-07-11")),
      report_date = as.Date(c("2020-07-11", "2020-07-12", "2020-07-14", "2020-07-15")),
      gender = c("Male", "Female", "Male", "Female")
    ),
    weekly_data = data.frame(
      event_date = as.Date(c("2020-07-08", "2020-07-15", "2020-07-22", "2020-07-29")),
      report_date = as.Date(c("2020-07-11", "2020-07-18", "2020-07-25", "2020-08-01"))
    ),
    monthly_data = data.frame(
      event_date = as.Date(c("2020-01-15", "2020-02-15", "2020-03-15", "2020-04-15")),
      report_date = as.Date(c("2020-01-20", "2020-02-20", "2020-03-20", "2020-04-20"))
    ),
    yearly_data = data.frame(
      event_date = as.Date(c("2018-06-15", "2019-06-15", "2020-06-15", "2021-06-15")),
      report_date = as.Date(c("2018-07-01", "2019-07-01", "2020-07-01", "2021-07-01"))
    ),
    numeric_data = data.frame(
      event_date = c(1L, 2L, 3L, 4L),
      report_date = c(5L, 6L, 7L, 8L)
    )
  )
}

# Tests for time_cols_to_numeric() with days ----
test_that("time_cols_to_numeric creates .event_num and .report_num columns", {
  test_data <- setup_test_data()

  result <- time_cols_to_numeric(
    test_data$daily_data,
    event_date = "event_date",
    report_date = "report_date",
    event_units = "days",
    report_units = "days",
    force = FALSE
  )

  expect_true(".event_num" %in% colnames(result))
  expect_true(".report_num" %in% colnames(result))
})

test_that("time_cols_to_numeric correctly converts daily data", {
  test_data <- setup_test_data()

  result <- time_cols_to_numeric(
    test_data$daily_data,
    event_date = "event_date",
    report_date = "report_date",
    event_units = "days",
    report_units = "days",
    force = FALSE
  )

  # First event_date should be 0 (anchor)
  expect_equal(result$.event_num[1], 0)

  # Second event_date should be 1 day after
  expect_equal(result$.event_num[2], 1)

  # First report_date should be 3 days after first event_date
  expect_equal(result$.report_num[1], 3)
})

test_that("time_cols_to_numeric anchors to minimum event_date", {
  test_data <- setup_test_data()

  result <- time_cols_to_numeric(
    test_data$daily_data,
    event_date = "event_date",
    report_date = "report_date",
    event_units = "days",
    report_units = "days",
    force = FALSE
  )

  min_event_num <- min(result$.event_num)
  expect_equal(min_event_num, 0)
})

# Tests for time_cols_to_numeric() with weeks ----
test_that("time_cols_to_numeric correctly converts weekly data", {
  test_data <- setup_test_data()

  result <- time_cols_to_numeric(
    test_data$weekly_data,
    event_date = "event_date",
    report_date = "report_date",
    event_units = "weeks",
    report_units = "weeks",
    force = FALSE
  )

  # First event_date should be 0 (anchor)
  expect_equal(result$.event_num[1], 0)

  # Second event_date should be 1 week after
  expect_equal(result$.event_num[2], 1)

  # Third event_date should be 2 weeks after
  expect_equal(result$.event_num[3], 2)
})

# Tests for time_cols_to_numeric() with months ----
test_that("time_cols_to_numeric correctly converts monthly data", {
  test_data <- setup_test_data()

  result <- time_cols_to_numeric(
    test_data$monthly_data,
    event_date = "event_date",
    report_date = "report_date",
    event_units = "months",
    report_units = "months",
    force = FALSE
  )

  # First event_date should be 0 (anchor)
  expect_equal(result$.event_num[1], 0)

  # Second event_date should be 1 month after
  expect_equal(result$.event_num[2], 1)

  # Third event_date should be 2 months after
  expect_equal(result$.event_num[3], 2)

  # Fourth event_date should be 3 months after
  expect_equal(result$.event_num[4], 3)
})

test_that("time_cols_to_numeric handles month differences correctly", {
  # Test data with different month scenarios
  month_edge_data <- data.frame(
    event_date = as.Date(c("2020-01-31", "2020-02-29", "2020-03-31")),
    report_date = as.Date(c("2020-02-15", "2020-03-15", "2020-04-15"))
  )

  result <- time_cols_to_numeric(
    month_edge_data,
    event_date = "event_date",
    report_date = "report_date",
    event_units = "months",
    report_units = "months",
    force = FALSE
  )

  expect_equal(result$.event_num[1], 0)
  expect_equal(result$.event_num[2], 1)
  expect_equal(result$.event_num[3], 2)
})

test_that("time_cols_to_numeric handles negative month differences", {
  # Test when month difference would be negative
  month_neg_data <- data.frame(
    event_date = as.Date(c("2020-12-15", "2021-01-10")),
    report_date = as.Date(c("2021-01-20", "2021-02-10"))
  )

  result <- time_cols_to_numeric(
    month_neg_data,
    event_date = "event_date",
    report_date = "report_date",
    event_units = "months",
    report_units = "months",
    force = FALSE
  )

  expect_equal(result$.event_num[1], 0)
  expect_equal(result$.event_num[2], 1)
})

test_that("time_cols_to_numeric doesn't create temp columns in final output", {
  test_data <- setup_test_data()

  result <- time_cols_to_numeric(
    test_data$monthly_data,
    event_date = "event_date",
    report_date = "report_date",
    event_units = "months",
    report_units = "months",
    force = FALSE
  )

  expect_false(".year_difference_temp" %in% colnames(result))
  expect_false(".month_difference_temp" %in% colnames(result))
})

# Tests for time_cols_to_numeric() with years ----
test_that("time_cols_to_numeric correctly converts yearly data", {
  test_data <- setup_test_data()

  result <- time_cols_to_numeric(
    test_data$yearly_data,
    event_date = "event_date",
    report_date = "report_date",
    event_units = "years",
    report_units = "years",
    force = FALSE
  )

  # First event_date should be 0 (anchor)
  expect_equal(result$.event_num[1], 0)

  # Second event_date should be 1 year after
  expect_equal(result$.event_num[2], 1)

  # Third event_date should be 2 years after
  expect_equal(result$.event_num[3], 2)
})

# Tests for time_cols_to_numeric() with numeric ----
test_that("time_cols_to_numeric correctly converts numeric data", {
  test_data <- setup_test_data()

  result <- time_cols_to_numeric(
    test_data$numeric_data,
    event_date = "event_date",
    report_date = "report_date",
    event_units = "numeric",
    report_units = "numeric",
    force = FALSE
  )

  # First event_date should be 0 (1 - 1 = 0)
  expect_equal(result$.event_num[1], 0)

  # Second event_date should be 1 (2 - 1 = 1)
  expect_equal(result$.event_num[2], 1)

  # First report_date should be 4 (5 - 1 = 4)
  expect_equal(result$.report_num[1], 4)
})

# Tests for validation and error handling ----
test_that("time_cols_to_numeric fails when mixing numeric with date units", {
  test_data <- setup_test_data()

  expect_error(
    time_cols_to_numeric(
      test_data$numeric_data,
      event_date = "event_date",
      report_date = "report_date",
      event_units = "numeric",
      report_units = "days",
      force = FALSE
    ),
    "If one of.*report_units.*and.*event_units.*is numeric"
  )

  expect_error(
    time_cols_to_numeric(
      test_data$daily_data,
      event_date = "event_date",
      report_date = "report_date",
      event_units = "days",
      report_units = "numeric",
      force = FALSE
    ),
    "If one of.*report_units.*and.*event_units.*is numeric"
  )
})

test_that("time_cols_to_numeric fails when report_units is finer than event_units", {
  test_data <- setup_test_data()

  expect_error(
    time_cols_to_numeric(
      test_data$daily_data,
      event_date = "event_date",
      report_date = "report_date",
      event_units = "weeks",
      report_units = "days",
      force = FALSE
    ),
    "must be coarser than or equal to event_units"
  )

  expect_error(
    time_cols_to_numeric(
      test_data$daily_data,
      event_date = "event_date",
      report_date = "report_date",
      event_units = "months",
      report_units = "weeks",
      force = FALSE
    ),
    "must be coarser than or equal to event_units"
  )
})

test_that("time_cols_to_numeric accepts report_units equal to event_units", {
  test_data <- setup_test_data()

  expect_silent({
    time_cols_to_numeric(
      test_data$daily_data,
      event_date = "event_date",
      report_date = "report_date",
      event_units = "days",
      report_units = "days",
      force = FALSE
    )
  })
})

test_that("time_cols_to_numeric accepts report_units coarser than event_units", {
  test_data <- setup_test_data()

  expect_silent({
    time_cols_to_numeric(
      test_data$daily_data,
      event_date = "event_date",
      report_date = "report_date",
      event_units = "days",
      report_units = "weeks",
      force = FALSE
    )
  })

  expect_silent({
    time_cols_to_numeric(
      test_data$weekly_data,
      event_date = "event_date",
      report_date = "report_date",
      event_units = "weeks",
      report_units = "months",
      force = FALSE
    )
  })
})

test_that("time_cols_to_numeric fails with invalid units", {
  test_data <- setup_test_data()

  expect_error(
    time_cols_to_numeric(
      test_data$daily_data,
      event_date = "event_date",
      report_date = "report_date",
      event_units = "invalid_unit",
      report_units = "days",
      force = FALSE
    ),
    'Invalid.*date_units.*invalid_unit'
  )
})

test_that("time_cols_to_numeric fails when .year_difference_temp exists without force", {
  test_data <- setup_test_data()
  data_with_temp <- test_data$monthly_data
  data_with_temp$.year_difference_temp <- 1

  expect_error(
    time_cols_to_numeric(
      data_with_temp,
      event_date = "event_date",
      report_date = "report_date",
      event_units = "months",
      report_units = "months",
      force = FALSE
    ),
    "internally creates a column called.*year_difference_temp"
  )
})

test_that("time_cols_to_numeric fails when .month_difference_temp exists without force", {
  test_data <- setup_test_data()
  data_with_temp <- test_data$monthly_data
  data_with_temp$.month_difference_temp <- 1

  expect_error(
    time_cols_to_numeric(
      data_with_temp,
      event_date = "event_date",
      report_date = "report_date",
      event_units = "months",
      report_units = "months",
      force = FALSE
    ),
    "internally creates a column called.*month_difference_temp"
  )
})

test_that("time_cols_to_numeric overwrites with force = TRUE", {
  test_data <- setup_test_data()
  data_with_temp <- test_data$monthly_data
  data_with_temp$.year_difference_temp <- 999

  result <- time_cols_to_numeric(
    data_with_temp,
    event_date = "event_date",
    report_date = "report_date",
    event_units = "months",
    report_units = "months",
    force = TRUE
  )

  # Should not contain the temp column
  expect_false(".year_difference_temp" %in% colnames(result))
})

# Tests for different unit combinations ----
test_that("time_cols_to_numeric works with days for event and weeks for report", {
  test_data <- setup_test_data()

  result <- time_cols_to_numeric(
    test_data$daily_data,
    event_date = "event_date",
    report_date = "report_date",
    event_units = "days",
    report_units = "weeks",
    force = FALSE
  )

  expect_true(".event_num" %in% colnames(result))
  expect_true(".report_num" %in% colnames(result))
  expect_equal(result$.event_num[1], 0)
})

test_that("time_cols_to_numeric works with weeks for event and months for report", {
  test_data <- setup_test_data()

  result <- time_cols_to_numeric(
    test_data$weekly_data,
    event_date = "event_date",
    report_date = "report_date",
    event_units = "weeks",
    report_units = "months",
    force = FALSE
  )

  expect_true(".event_num" %in% colnames(result))
  expect_true(".report_num" %in% colnames(result))
})

test_that("time_cols_to_numeric works with months for event and years for report", {
  test_data <- setup_test_data()

  result <- time_cols_to_numeric(
    test_data$monthly_data,
    event_date = "event_date",
    report_date = "report_date",
    event_units = "months",
    report_units = "years",
    force = FALSE
  )

  expect_true(".event_num" %in% colnames(result))
  expect_true(".report_num" %in% colnames(result))
})

# Tests for data integrity ----
test_that("time_cols_to_numeric preserves original columns", {
  test_data <- setup_test_data()

  result <- time_cols_to_numeric(
    test_data$daily_data,
    event_date = "event_date",
    report_date = "report_date",
    event_units = "days",
    report_units = "days",
    force = FALSE
  )

  expect_true("event_date" %in% colnames(result))
  expect_true("report_date" %in% colnames(result))
  expect_true("gender" %in% colnames(result))
})

test_that("time_cols_to_numeric preserves number of rows", {
  test_data <- setup_test_data()

  original_rows <- nrow(test_data$daily_data)

  result <- time_cols_to_numeric(
    test_data$daily_data,
    event_date = "event_date",
    report_date = "report_date",
    event_units = "days",
    report_units = "days",
    force = FALSE
  )

  expect_equal(nrow(result), original_rows)
})

test_that("time_cols_to_numeric produces non-negative numbers", {
  test_data <- setup_test_data()

  result <- time_cols_to_numeric(
    test_data$daily_data,
    event_date = "event_date",
    report_date = "report_date",
    event_units = "days",
    report_units = "days",
    force = FALSE
  )

  expect_true(all(result$.event_num >= 0))
  expect_true(all(result$.report_num >= 0))
})

# Tests for edge cases ----
test_that("time_cols_to_numeric handles single row data", {
  single_row <- data.frame(
    event_date = as.Date("2020-07-08"),
    report_date = as.Date("2020-07-11")
  )

  result <- time_cols_to_numeric(
    single_row,
    event_date = "event_date",
    report_date = "report_date",
    event_units = "days",
    report_units = "days",
    force = FALSE
  )

  expect_equal(result$.event_num, 0)
  expect_equal(result$.report_num, 3)
})

test_that("time_cols_to_numeric handles data with same event and report dates", {
  same_date_data <- data.frame(
    event_date = as.Date(c("2020-07-08", "2020-07-09")),
    report_date = as.Date(c("2020-07-08", "2020-07-09"))
  )

  result <- time_cols_to_numeric(
    same_date_data,
    event_date = "event_date",
    report_date = "report_date",
    event_units = "days",
    report_units = "days",
    force = FALSE
  )

  expect_equal(result$.event_num, result$.report_num)
})

test_that("time_cols_to_numeric handles leap years correctly", {
  leap_year_data <- data.frame(
    event_date = as.Date(c("2020-02-28", "2020-02-29", "2020-03-01")),
    report_date = as.Date(c("2020-03-01", "2020-03-02", "2020-03-03"))
  )

  result <- time_cols_to_numeric(
    leap_year_data,
    event_date = "event_date",
    report_date = "report_date",
    event_units = "days",
    report_units = "days",
    force = FALSE
  )

  expect_equal(result$.event_num[2], 1)
  expect_equal(result$.event_num[3], 2)
})

test_that("time_cols_to_numeric handles year boundaries for monthly data", {
  year_boundary_data <- data.frame(
    event_date = as.Date(c("2019-11-15", "2019-12-15", "2020-01-15", "2020-02-15")),
    report_date = as.Date(c("2019-11-20", "2019-12-20", "2020-01-20", "2020-02-20"))
  )

  result <- time_cols_to_numeric(
    year_boundary_data,
    event_date = "event_date",
    report_date = "report_date",
    event_units = "months",
    report_units = "months",
    force = FALSE
  )

  expect_equal(result$.event_num[1], 0)
  expect_equal(result$.event_num[2], 1)
  expect_equal(result$.event_num[3], 2)
  expect_equal(result$.event_num[4], 3)
})

# Tests for numeric consistency ----
test_that("time_cols_to_numeric produces consistent differences", {
  test_data <- setup_test_data()

  result <- time_cols_to_numeric(
    test_data$daily_data,
    event_date = "event_date",
    report_date = "report_date",
    event_units = "days",
    report_units = "days",
    force = FALSE
  )

  # Differences between consecutive event_nums should match date differences
  for (i in 2:nrow(result)) {
    date_diff <- as.numeric(difftime(
      result$event_date[i],
      result$event_date[i-1],
      units = "days"
    ))
    num_diff <- result$.event_num[i] - result$.event_num[i-1]
    expect_equal(num_diff, date_diff)
  }
})
