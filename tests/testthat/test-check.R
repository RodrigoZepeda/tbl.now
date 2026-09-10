# Test file for check.R functions

# Setup test data ----
setup_test_data <- function() {
  list(
    valid_data = data.frame(
      event_date = as.Date(c("2020-07-08", "2020-07-09", "2020-07-10")),
      report_date = as.Date(c("2020-07-11", "2020-07-12", "2020-07-14")),
      gender = c("Male", "Female", "Male"),
      age_group = c("20-30", "30-40", "20-30"),
      temperature = c(25.5, 26.0, 24.8),
      delay_censored = c(0, 1, 0)
    ),
    integer_data = data.frame(
      event_date = c(1L, 2L, 3L),
      report_date = c(4L, 5L, 6L),
      gender = c("Male", "Female", "Male")
    )
  )
}

# Tests for check_date_columns() ----
test_that("check_date_columns accepts valid date columns", {
  skip_on_cran()
  test_data <- setup_test_data()

  expect_invisible(
    check_date_columns(
      test_data$valid_data,
      event_date = "event_date",
      report_date = "report_date"
    )
  )

  expect_true(
    check_date_columns(
      test_data$valid_data,
      event_date = "event_date",
      report_date = "report_date"
    )
  )
})

test_that("check_date_columns accepts valid integer columns", {
  test_data <- setup_test_data()

  expect_invisible(
    check_date_columns(
      test_data$integer_data,
      event_date = "event_date",
      report_date = "report_date"
    )
  )
})

test_that("check_date_columns fails when event_date is not character", {
  skip_on_cran()
  test_data <- setup_test_data()

  expect_error(
    check_date_columns(
      test_data$valid_data,
      event_date = 123,
      report_date = "report_date"
    ),
    "must be a single string"
  )
})

test_that("check_date_columns fails when report_date is not character", {
  skip_on_cran()
  test_data <- setup_test_data()

  expect_error(
    check_date_columns(
      test_data$valid_data,
      event_date = "event_date",
      report_date = c("report_date", "another")
    ),
    "must be a single string"
  )
})

test_that("check_date_columns fails when event_date column not found", {
  skip_on_cran()
  test_data <- setup_test_data()

  expect_error(
    check_date_columns(
      test_data$valid_data,
      event_date = "nonexistent",
      report_date = "report_date"
    ),
    "not found in"
  )
})

test_that("check_date_columns fails when report_date column not found", {
  skip_on_cran()
  test_data <- setup_test_data()

  expect_error(
    check_date_columns(
      test_data$valid_data,
      event_date = "event_date",
      report_date = "nonexistent"
    ),
    "not found in"
  )
})

test_that("check_date_columns fails when columns are mismatched types", {
  skip_on_cran()
  mixed_data <- data.frame(
    event_date = as.Date(c("2020-07-08", "2020-07-09")),
    report_date = c(1L, 2L)
  )

  expect_error(
    check_date_columns(
      mixed_data,
      event_date = "event_date",
      report_date = "report_date"
    ),
    "Use.*as.Date.*or.*as.integer"
  )
})

# Tests for check_strata() ----
test_that("check_strata accepts NULL strata", {
  skip_on_cran()
  test_data <- setup_test_data()

  expect_invisible(
    check_strata(test_data$valid_data, strata = NULL)
  )

  expect_true(
    check_strata(test_data$valid_data, strata = NULL)
  )
})

test_that("check_strata accepts valid character strata", {
  skip_on_cran()
  test_data <- setup_test_data()

  expect_invisible(
    check_strata(test_data$valid_data, strata = "gender")
  )

  expect_invisible(
    check_strata(test_data$valid_data, strata = c("gender", "age_group"))
  )
})

test_that("check_strata accepts valid integer strata", {
  skip_on_cran()
  int_strata_data <- data.frame(
    event_date = as.Date(c("2020-07-08", "2020-07-08", "2020-07-08", "2020-07-09")),
    report_date = as.Date(c("2020-07-11", "2020-07-08", "2020-07-08", "2020-07-12")),
    group_id = c(1L, 2L, 1L, 1L)
  )

  expect_invisible(
    check_strata(int_strata_data, strata = "group_id")
  )
})

test_that("check_strata accepts valid factor strata", {
  skip_on_cran()
  factor_strata_data <- data.frame(
    event_date = as.Date(c("2020-07-08", "2020-07-09")),
    report_date = as.Date(c("2020-07-11", "2020-07-12")),
    category = factor(c("A", "B"))
  )

  expect_warning(
    check_strata(factor_strata_data, strata = "category")
  )
})

test_that("check_strata fails with invalid strata", {
  skip_on_cran()
  double_strata_data <- data.frame(
    event_date = as.Date(c("2020-07-08", "2020-07-09")),
    report_date = as.Date(c("2020-07-11", "2020-07-12")),
    category = c(0.237, 1248.13)
  )

  expect_error(
    check_strata(double_strata_data, strata = "category"),
    "should contain integer or character values"
  )
})

test_that("check_strata fails when strata column not found", {
  skip_on_cran()
  test_data <- setup_test_data()

  expect_error(
    check_strata(test_data$valid_data, strata = "nonexistent"),
    "not found"
  )
})

test_that("check_strata fails when one observation per strata", {
  skip_on_cran()
  one_per_strata <- data.frame(
    event_date = as.Date(c("2020-07-08", "2020-07-09")),
    report_date = as.Date(c("2020-07-11", "2020-07-12")),
    unique_id = c(1L, 2L)
  )

  expect_warning(
    check_strata(one_per_strata, strata = "unique_id"),
    "one observation per strata"
  )
})

# Tests for check_delay_is_censored() ----
test_that("check_delay_is_censored accepts NULL", {
  skip_on_cran()
  test_data <- setup_test_data()

  expect_invisible(
    check_delay_is_censored(test_data$valid_data, delay_is_censored = NULL)
  )

  expect_true(
    check_delay_is_censored(test_data$valid_data, delay_is_censored = NULL)
  )
})

test_that("check_delay_is_censored accepts valid column with 0/1 values", {
  skip_on_cran()
  test_data <- setup_test_data()

  expect_invisible(
    check_delay_is_censored(
      test_data$valid_data,
      delay_is_censored = "delay_censored"
    )
  )
})

test_that("check_delay_is_censored fails when column not found", {
  skip_on_cran()
  test_data <- setup_test_data()

  expect_error(
    check_delay_is_censored(
      test_data$valid_data,
      delay_is_censored = "nonexistent"
    ),
    "not found in the dataset"
  )
})

test_that("check_delay_is_censored fails with invalid values", {
  skip_on_cran()
  invalid_data <- data.frame(
    event_date = as.Date(c("2020-07-08", "2020-07-09")),
    report_date = as.Date(c("2020-07-11", "2020-07-12")),
    delay_censored = c(0, 2)
  )

  expect_error(
    check_delay_is_censored(
      invalid_data,
      delay_is_censored = "delay_censored"
    ),
    "should only have values.*TRUE.*FALSE"
  )
})

test_that("check_delay_is_censored fails with character values", {
  skip_on_cran()
  invalid_data <- data.frame(
    event_date = as.Date(c("2020-07-08", "2020-07-09")),
    report_date = as.Date(c("2020-07-11", "2020-07-12")),
    delay_censored = c("yes", "no")
  )

  expect_error(
    check_delay_is_censored(
      invalid_data,
      delay_is_censored = "delay_censored"
    ),
    "should only have values.*TRUE.*FALSE"
  )
})

# Tests for check_now() ----
test_that("check_now accepts NULL", {
  skip_on_cran()
  test_data <- setup_test_data()

  expect_invisible(
    check_now(
      test_data$valid_data,
      event_date = "event_date",
      report_date = "report_date",
      now = NULL
    )
  )

  expect_true(
    check_now(
      test_data$valid_data,
      event_date = "event_date",
      report_date = "report_date",
      now = NULL
    )
  )
})

test_that("check_now accepts valid date within range", {
  skip_on_cran()
  test_data <- setup_test_data()

  expect_invisible(
    check_now(
      test_data$valid_data,
      event_date = "event_date",
      report_date = "report_date",
      now = as.Date("2020-07-12")
    )
  )
})

test_that("check_now fails when now is not a Date", {
  skip_on_cran()
  test_data <- setup_test_data()

  expect_error(
    check_now(
      test_data$valid_data,
      event_date = "event_date",
      report_date = "report_date",
      now = "2020-07-12"
    ),
    "is not a.*Date"
  )
})

test_that("check_now fails when now is before data range", {
  skip_on_cran()
  test_data <- setup_test_data()

  expect_error(
    check_now(
      test_data$valid_data,
      event_date = "event_date",
      report_date = "report_date",
      now = as.Date("2020-07-01")
    ),
    "outside the scope of the data"
  )
})

test_that("check_now fails when now is after data range", {
  skip_on_cran()
  test_data <- setup_test_data()

  expect_error(
    check_now(
      test_data$valid_data,
      event_date = "event_date",
      report_date = "report_date",
      now = as.Date("2020-12-31")
    ),
    "outside the scope of the data"
  )
})

# Tests for check_units() ----
test_that("check_units accepts NULL", {
  skip_on_cran()
  test_data <- setup_test_data()

  expect_invisible(
    check_units(test_data$valid_data, date_units = NULL)
  )

  expect_true(
    check_units(test_data$valid_data, date_units = NULL)
  )
})

test_that("check_units accepts valid units", {
  skip_on_cran()
  test_data <- setup_test_data()

  valid_units <- c("days", "weeks", "numeric", "months", "years")

  for (unit in valid_units) {
    expect_invisible(
      check_units(test_data$valid_data, date_units = unit)
    )
  }
})

test_that("check_units fails with invalid units", {
  skip_on_cran()
  test_data <- setup_test_data()

  expect_error(
    check_units(test_data$valid_data, date_units = "hours"),
    "Invalid.*date_units"
  )

  expect_error(
    check_units(test_data$valid_data, date_units = "invalid"),
    "Invalid.*date_units"
  )
})

test_that("check_units fails when data has one row and units are NULL", {
  skip_on_cran()
  single_row_data <- data.frame(
    event_date = as.Date("2020-07-08"),
    report_date = as.Date("2020-07-11")
  )

  expect_error(
    check_units(single_row_data, date_units = NULL),
    "Cannot infer date_units from just one observation"
  )
})

# Tests for check_verbose() ----
test_that("check_verbose accepts TRUE", {
  skip_on_cran()
  expect_invisible(check_verbose(TRUE))
  expect_true(check_verbose(TRUE))
})

test_that("check_verbose accepts FALSE", {
  skip_on_cran()
  expect_invisible(check_verbose(FALSE))
  expect_true(check_verbose(FALSE))
})

test_that("check_verbose fails with non-boolean", {
  skip_on_cran()
  expect_error(
    check_verbose("TRUE"),
    "should be either.*TRUE.*FALSE"
  )

  expect_error(
    check_verbose(1),
    "should be either.*TRUE.*FALSE"
  )

  expect_error(
    check_verbose(NULL),
    "should be either.*TRUE.*FALSE"
  )
})

# Tests for check_data_type() ----
test_that("check_data_type accepts valid types", {
  skip_on_cran()
  valid_types <- c("auto", "linelist", "count")

  for (type in valid_types) {
    expect_invisible(
      check_data_type(type)
    )

    expect_true(
      check_data_type(type)
    )
  }
})

test_that("check_data_type fails with invalid type", {
  skip_on_cran()
  expect_error(
    check_data_type("invalid"),
    "Unknown data_type"
  )

  expect_error(
    check_data_type("aggregated"),
    "Unknown data_type"
  )
})

# Tests for check_bool() ----
test_that("check_bool accepts TRUE", {
  skip_on_cran()
  expect_invisible(check_bool(TRUE, "test_var"))
  expect_true(check_bool(TRUE, "test_var"))
})

test_that("check_bool accepts FALSE", {
  skip_on_cran()
  expect_invisible(check_bool(FALSE, "test_var"))
  expect_true(check_bool(FALSE, "test_var"))
})

test_that("check_bool fails with non-boolean values", {
  skip_on_cran()
  expect_error(
    check_bool(1, "test_var"),
    "must be either.*TRUE.*FALSE"
  )

  expect_error(
    check_bool("TRUE", "test_var"),
    "must be either.*TRUE.*FALSE"
  )

  expect_error(
    check_bool(NULL, "test_var"),
    "must be either.*TRUE.*FALSE"
  )

  expect_error(
    check_bool(NA, "test_var"),
    "must be either.*TRUE.*FALSE"
  )
})

test_that("check_bool includes variable name in error message", {
  skip_on_cran()
  expect_error(
    check_bool("not_bool", "my_variable"),
    "my_variable"
  )
})

# Integration tests ----
test_that("check functions work together for valid data", {
  skip_on_cran()
  test_data <- setup_test_data()

  # All checks should pass
  expect_silent({
    check_date_columns(
      test_data$valid_data,
      event_date = "event_date",
      report_date = "report_date"
    )
    check_strata(test_data$valid_data, strata = "gender")
    check_delay_is_censored(
      test_data$valid_data,
      delay_is_censored = "delay_censored"
    )
    check_now(
      test_data$valid_data,
      event_date = "event_date",
      report_date = "report_date",
      now = as.Date("2020-07-12")
    )
    check_units(test_data$valid_data, date_units = "days")
    check_verbose(TRUE)
    check_data_type("linelist")
    check_bool(TRUE, "test")
  })
})

test_that("check functions handle edge cases", {
  skip_on_cran()
  # Empty strata
  test_data <- setup_test_data()
  expect_silent(check_strata(test_data$valid_data, strata = NULL))

  # Maximum date as now
  max_date <- max(test_data$valid_data$report_date)
  expect_silent(
    check_now(
      test_data$valid_data,
      event_date = "event_date",
      report_date = "report_date",
      now = max_date
    )
  )

  # Minimum date as now
  min_date <- min(test_data$valid_data$event_date)
  expect_silent(
    check_now(
      test_data$valid_data,
      event_date = "event_date",
      report_date = "report_date",
      now = min_date
    )
  )
})
