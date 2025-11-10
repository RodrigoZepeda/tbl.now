# Test file for infer.R functions

# Setup test data ----
setup_test_data <- function() {
  list(
    daily_data = data.frame(
      event_date = as.Date(c("2020-07-08", "2020-07-09", "2020-07-10", "2020-07-11")),
      report_date = as.Date(c("2020-07-11", "2020-07-12", "2020-07-14", "2020-07-15"))
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
    ),
    single_row = data.frame(
      event_date = as.Date("2020-07-08"),
      report_date = as.Date("2020-07-11")
    ),
    count_data = data.frame(
      event_date = as.Date(c("2020-07-08", "2020-07-09", "2020-07-08")),
      report_date = as.Date(c("2020-07-11", "2020-07-12", "2020-07-11")),
      n = c(5L, 3L, 2L)
    ),
    linelist_data = data.frame(
      event_date = as.Date(c("2020-07-08", "2020-07-09", "2020-07-10")),
      report_date = as.Date(c("2020-07-11", "2020-07-12", "2020-07-14")),
      gender = c("Male", "Female", "Male")
    )
  )
}

# Tests for infer_now() ----
test_that("infer_now returns max report date when now is NULL", {
  test_data <- setup_test_data()

  result <- infer_now(
    test_data$daily_data,
    now = NULL,
    event_date = "event_date",
    report_date = "report_date"
  )

  expect_equal(result, as.Date("2020-07-15"))
  expect_s3_class(result, "Date")
})

test_that("infer_now returns max of event and report dates", {
  # Case where report_date is later
  data1 <- data.frame(
    event_date = as.Date(c("2020-07-08", "2020-07-09")),
    report_date = as.Date(c("2020-07-11", "2020-07-20"))
  )

  result1 <- infer_now(data1, NULL, "event_date", "report_date")
  expect_equal(result1, as.Date("2020-07-20"))

  # Case where event_date is later
  data2 <- data.frame(
    event_date = as.Date(c("2020-07-08", "2020-07-25")),
    report_date = as.Date(c("2020-07-11", "2020-07-20"))
  )

  result2 <- infer_now(data2, NULL, "event_date", "report_date")
  expect_equal(result2, as.Date("2020-07-25"))
})

test_that("infer_now returns provided now when not NULL", {
  test_data <- setup_test_data()

  provided_now <- as.Date("2020-07-13")
  result <- infer_now(
    test_data$daily_data,
    now = provided_now,
    event_date = "event_date",
    report_date = "report_date"
  )

  expect_equal(result, provided_now)
})

test_that("infer_now fails with empty data frame", {
  empty_data <- data.frame(
    event_date = as.Date(character(0)),
    report_date = as.Date(character(0))
  )

  expect_error(
    infer_now(empty_data, NULL, "event_date", "report_date"),
    "empty data.frame"
  )
})

test_that("infer_now works with integer dates", {
  test_data <- setup_test_data()

  result <- infer_now(
    test_data$numeric_data,
    now = NULL,
    event_date = "event_date",
    report_date = "report_date"
  )

  expect_equal(result, 8L)
})

test_that("infer_now fails when columns don't exist", {
  test_data <- setup_test_data()

  expect_error(
    infer_now(test_data$daily_data, NULL, "nonexistent", "report_date"),
    "not found"
  )

  expect_error(
    infer_now(test_data$daily_data, NULL, "event_date", "nonexistent"),
    "not found"
  )
})

# Tests for infer_units_one_column() ----
test_that("infer_units_one_column detects daily data", {
  test_data <- setup_test_data()

  result <- infer_units_one_column(
    test_data$daily_data,
    date_column = "event_date",
    date_units = "auto"
  )

  expect_equal(result, "days")
})

test_that("infer_units_one_column detects weekly data", {
  test_data <- setup_test_data()

  result <- infer_units_one_column(
    test_data$weekly_data,
    date_column = "event_date",
    date_units = "auto"
  )

  expect_equal(result, "weeks")
})

test_that("infer_units_one_column detects monthly data", {
  test_data <- setup_test_data()

  result <- infer_units_one_column(
    test_data$monthly_data,
    date_column = "event_date",
    date_units = "auto"
  )

  expect_equal(result, "months")
})

test_that("infer_units_one_column detects yearly data", {
  test_data <- setup_test_data()

  result <- infer_units_one_column(
    test_data$yearly_data,
    date_column = "event_date",
    date_units = "auto"
  )

  expect_equal(result, "years")
})

test_that("infer_units_one_column detects numeric data", {
  test_data <- setup_test_data()

  result <- infer_units_one_column(
    test_data$numeric_data,
    date_column = "event_date",
    date_units = "auto"
  )

  expect_equal(result, "numeric")
})

test_that("infer_units_one_column returns provided units when not auto", {
  test_data <- setup_test_data()

  result <- infer_units_one_column(
    test_data$daily_data,
    date_column = "event_date",
    date_units = "weeks"
  )

  expect_equal(result, "weeks")
})

test_that("infer_units_one_column accepts NULL and treats as auto", {
  test_data <- setup_test_data()

  result <- infer_units_one_column(
    test_data$daily_data,
    date_column = "event_date",
    date_units = NULL
  )

  expect_equal(result, "days")
})

test_that("infer_units_one_column fails with less than 2 observations", {
  test_data <- setup_test_data()

  expect_error(
    infer_units_one_column(
      test_data$single_row,
      date_column = "event_date",
      date_units = "auto"
    ),
    "Cannot infer time units"
  )
})

test_that("infer_units_one_column fails with unsupported units", {
  test_data <- setup_test_data()

  expect_error(
    infer_units_one_column(
      test_data$daily_data,
      date_column = "event_date",
      date_units = "hours"
    ),
    "not supported yet"
  )
})

test_that("infer_units_one_column fails with non-Date non-numeric column", {
  char_data <- data.frame(
    event_date = c("2020-07-08", "2020-07-09"),
    report_date = as.Date(c("2020-07-11", "2020-07-12"))
  )

  expect_error(
    infer_units_one_column(
      char_data,
      date_column = "event_date",
      date_units = "auto"
    ),
    "has to be either.*numeric.*or a.*Date"
  )
})

test_that("infer_units_one_column handles irregular spacing", {
  # Data with irregular spacing that doesn't fit standard categories
  irregular_data <- data.frame(
    event_date = as.Date(c("2020-01-01", "2020-01-15", "2020-02-01"))
  )

  expect_error(
    infer_units_one_column(
      irregular_data,
      date_column = "event_date",
      date_units = "auto"
    ),
    "Cannot infer time date_units"
  )
})

test_that("infer_units_one_column uses minimum difference for detection", {
  # Mixed spacing but minimum is weekly
  mixed_data <- data.frame(
    event_date = as.Date(c("2020-01-01", "2020-01-08", "2020-01-15", "2020-02-15"))
  )

  result <- infer_units_one_column(
    mixed_data,
    date_column = "event_date",
    date_units = "auto"
  )

  expect_equal(result, "weeks")
})

# Tests for infer_units() ----
test_that("infer_units is a wrapper for infer_units_one_column", {
  test_data <- setup_test_data()

  result1 <- infer_units(
    test_data$daily_data,
    date_column = "event_date",
    date_units = "auto"
  )

  result2 <- infer_units_one_column(
    test_data$daily_data,
    date_column = "event_date",
    date_units = "auto"
  )

  expect_equal(result1, result2)
})

# Tests for infer_data_type() ----
test_that("infer_data_type detects count data when n column exists", {
  test_data <- setup_test_data()

  result <- infer_data_type(
    test_data$count_data,
    data_type = "auto",
    verbose = FALSE
  )

  expect_equal(result, "count")
})

test_that("infer_data_type detects linelist data when n column missing", {
  test_data <- setup_test_data()

  result <- infer_data_type(
    test_data$linelist_data,
    data_type = "auto",
    verbose = FALSE
  )

  expect_equal(result, "linelist")
})

test_that("infer_data_type shows message when verbose = TRUE", {
  test_data <- setup_test_data()

  expect_message(
    infer_data_type(test_data$count_data, data_type = "auto", verbose = TRUE),
    "count-data"
  )

  expect_message(
    infer_data_type(test_data$linelist_data, data_type = "auto", verbose = TRUE),
    "linelist-data"
  )
})

test_that("infer_data_type returns provided data_type when not auto", {
  test_data <- setup_test_data()

  result <- infer_data_type(
    test_data$linelist_data,
    data_type = "linelist",
    verbose = FALSE
  )

  expect_equal(result, "linelist")
})

test_that("infer_data_type warns when linelist has n column", {
  linelist_with_n <- data.frame(
    event_date = as.Date(c("2020-07-08", "2020-07-09")),
    report_date = as.Date(c("2020-07-11", "2020-07-12")),
    n = c(1, 1)
  )

  expect_warning(
    infer_data_type(linelist_with_n, data_type = "linelist", verbose = FALSE),
    "contains a column named.*n.*will be overwritten"
  )
})

test_that("infer_data_type fails when count data missing n column", {
  test_data <- setup_test_data()

  expect_error(
    infer_data_type(test_data$linelist_data, data_type = "count", verbose = FALSE),
    "Count data should have a column named.*n"
  )
})

test_that("infer_data_type fails when n column is not numeric", {
  invalid_count <- data.frame(
    event_date = as.Date(c("2020-07-08", "2020-07-09")),
    report_date = as.Date(c("2020-07-11", "2020-07-12")),
    n = c("five", "three")
  )

  expect_error(
    infer_data_type(invalid_count, data_type = "auto", verbose = FALSE),
    "Cannot automatically detect data_type"
  )
})

test_that("infer_data_type fails when n column has non-integer values", {
  invalid_count <- data.frame(
    event_date = as.Date(c("2020-07-08", "2020-07-09")),
    report_date = as.Date(c("2020-07-11", "2020-07-12")),
    n = c(1.5, 2.7)
  )

  expect_error(
    infer_data_type(invalid_count, data_type = "auto", verbose = FALSE),
    "Cannot automatically detect data_type"
  )
})

test_that("infer_data_type handles vector data_type input", {
  test_data <- setup_test_data()

  # Should use first element
  result <- infer_data_type(
    test_data$count_data,
    data_type = c("auto", "linelist"),
    verbose = FALSE
  )

  expect_equal(result, "count")
})

# Integration tests ----
test_that("infer functions work together for daily data", {
  test_data <- setup_test_data()

  now <- infer_now(
    test_data$daily_data,
    now = NULL,
    event_date = "event_date",
    report_date = "report_date"
  )

  event_units <- infer_units(
    test_data$daily_data,
    date_column = "event_date",
    date_units = "auto"
  )

  report_units <- infer_units(
    test_data$daily_data,
    date_column = "report_date",
    date_units = "auto"
  )

  data_type <- infer_data_type(
    test_data$daily_data,
    data_type = "auto",
    verbose = FALSE
  )

  expect_s3_class(now, "Date")
  expect_equal(event_units, "days")
  expect_equal(report_units, "days")
  expect_equal(data_type, "linelist")
})

test_that("infer functions work with count data", {
  test_data <- setup_test_data()

  now <- infer_now(
    test_data$count_data,
    now = NULL,
    event_date = "event_date",
    report_date = "report_date"
  )

  data_type <- infer_data_type(
    test_data$count_data,
    data_type = "auto",
    verbose = FALSE
  )

  expect_s3_class(now, "Date")
  expect_equal(data_type, "count")
})

test_that("infer functions work with numeric dates", {
  test_data <- setup_test_data()

  now <- infer_now(
    test_data$numeric_data,
    now = NULL,
    event_date = "event_date",
    report_date = "report_date"
  )

  event_units <- infer_units(
    test_data$numeric_data,
    date_column = "event_date",
    date_units = "auto"
  )

  expect_equal(now, 8L)
  expect_equal(event_units, "numeric")
})

test_that("infer functions handle edge cases", {
  # Two observations (minimum for inference)
  two_obs <- data.frame(
    event_date = as.Date(c("2020-07-08", "2020-07-09")),
    report_date = as.Date(c("2020-07-11", "2020-07-12"))
  )

  expect_silent({
    infer_now(two_obs, NULL, "event_date", "report_date")
    infer_units(two_obs, "event_date", "auto")
    infer_data_type(two_obs, "auto", verbose = FALSE)
  })
})

test_that("infer_units handles boundary cases for time periods", {
  # Exactly 6 days (boundary between days and weeks)
  six_day_data <- data.frame(
    event_date = as.Date(c("2020-01-01", "2020-01-07", "2020-01-13"))
  )

  result <- infer_units_one_column(
    six_day_data,
    date_column = "event_date",
    date_units = "auto"
  )

  expect_equal(result, "days")

  # Exactly 13 days (boundary between weeks and undefined)
  thirteen_day_data <- data.frame(
    event_date = as.Date(c("2020-01-01", "2020-01-14", "2020-01-27"))
  )

  result2 <- infer_units_one_column(
    thirteen_day_data,
    date_column = "event_date",
    date_units = "auto"
  )

  expect_equal(result2, "weeks")
})

test_that("infer_data_type accepts integer n values", {
  count_with_int <- data.frame(
    event_date = as.Date(c("2020-07-08", "2020-07-09")),
    report_date = as.Date(c("2020-07-11", "2020-07-12")),
    n = c(5L, 3L)
  )

  result <- infer_data_type(
    count_with_int,
    data_type = "auto",
    verbose = FALSE
  )

  expect_equal(result, "count")
})
