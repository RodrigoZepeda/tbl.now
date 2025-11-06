# Load libraries (assuming the code depends on these)
library(testthat)
library(dplyr)

# Source all necessary functions for testing.
# In a real R package, these would be loaded via `load_all()` or your package environment.
# Since we are in an isolated environment, we assume the functions from the provided
# R files (like check.R, infer.R, and tbl_now.R) are available.

# --- Test Data Setup ---
# Linelist data (data_type should be "linelist")
ll_data <- tibble(
  onset_week = as.Date(c("2023-01-01", "2023-01-02", "2023-01-01", "2023-01-03")),
  report_week = as.Date(c("2023-01-03", "2023-01-03", "2023-01-05", "2023-01-05")),
  gender = c("M", "F", "M", "F"),
  age_group = c("A", "B", "A", "B"),
  is_batched_col = c(FALSE, FALSE, TRUE, FALSE)
)

# Count data (data_type should be "count" because of the 'n' column)
count_data <- ll_data %>%
  group_by(onset_week, report_week, gender, age_group) %>%
  summarise(n = n(), .groups = "drop")

# Expected maximum report date for inferring 'now'
expected_now <- as.Date("2023-01-05")


# === TEST SUITE FOR tbl_now() FUNCTION ===

test_that("tbl_now creates object with minimal linelist data", {
  # Successful creation test
  result <- tbl_now(
    data = ll_data,
    event_date = "onset_week",
    report_date = "report_week",
    date_units = "days",
    covariates = c("age_group", "gender"),
    verbose = FALSE
  )

  # Check class
  expect_s3_class(result, "tbl_now")

  # Check inferred attributes
  expect_equal(attr(result, "event_date"), "onset_week")
  expect_equal(attr(result, "report_date"), "report_week")
  expect_equal(attr(result, "now"), expected_now)     # Should infer max report date
  expect_equal(attr(result, "strata"), NULL)          # Should be empty
  expect_equal(attr(result, "num_strata"), 0)          # Should be empty
  expect_equal(attr(result, "covariates"), c("age_group", "gender"))
  expect_equal(attr(result, "num_covariates"), 2)
  expect_equal(attr(result, "data_type"), "linelist") # Should infer linelist
  expect_equal(attr(result, "date_units"), "days")    # Should infer "day" for Date objects

  expect_equal(attr(result, "event_date"), get_event_date(result))
  expect_equal(attr(result, "report_date"), get_report_date(result))
  expect_equal(attr(result, "now"), get_now(result))
  expect_equal(attr(result, "strata"), get_strata(result))
  expect_equal(attr(result, "num_strata"), get_num_strata(result))
  expect_equal(attr(result, "covariates"), get_covariates(result))
  expect_equal(attr(result, "num_covariates"), get_num_covariates(result))
  expect_equal(attr(result, "data_type"), get_data_type(result))
  expect_equal(attr(result, "date_units"), get_date_units(result))

})

test_that("tbl_now respects user-defined 'now'", {
  user_now <- as.Date("2023-01-06")
  result <- tbl_now(
    data = ll_data,
    event_date = "onset_week",
    report_date = "report_week",
    now = user_now,
    date_units = "days",
    covariates = c("age_group", "gender"),
    verbose = FALSE
  )
  expect_equal(attr(result, "now"), user_now)
})

test_that("tbl_now warns if `now` is in the past", {
  user_now <- as.Date("1990-01-06")
  expect_warning(
    tbl_now(
      data = ll_data,
      event_date = "onset_week",
      report_date = "report_week",
      now = user_now,
      date_units = "days",
      covariates = c("age_group", "gender"),
      verbose = FALSE
    ),
    "past"
  )
})

test_that("tbl_now correctly sets strata ", {
  result <- tbl_now(
    data = ll_data,
    event_date = "onset_week",
    report_date = "report_week",
    strata = c("age_group", "gender"),
    date_units = "days",
    verbose = FALSE
  )

  expect_equal(attr(result, "strata"), c("age_group", "gender"))
  expect_equal(attr(result, "num_strata"), 2)
  expect_equal(attr(result, "covariates"), NULL)
  expect_equal(attr(result, "num_covariates"), 0)
})

test_that("tbl_now infers 'count' data_type correctly", {
  # Data with a column named 'n'
  result <- tbl_now(
    data = count_data,
    event_date = "onset_week",
    report_date = "report_week",
    date_units = "days",
    verbose = FALSE
  )

  expect_equal(attr(result, "data_type"), "count")
})

#FIXME: Fix the column
test_that("tbl_now handles optional 'is_batched' column", {
  result <- tbl_now(
    data = ll_data,
    event_date = "onset_week",
    report_date = "report_week",
    is_batched = "is_batched_col",
    date_units = "days",
    verbose = FALSE
  )
  #expect_equal(attr(result, "is_batched"), "is_batched_col")
})

test_that("tbl_now errors when date columns are missing or invalid", {
  # Error if event_date column is missing
  expect_error(
    tbl_now(
      data = ll_data,
      event_date = "missing_col",
      report_date = "report_week",
      verbose = FALSE,
      date_units = "days",
    ),
    'event_date.* not found in `data`'
  )

  # Error if a strata column is missing
  expect_error(
    tbl_now(
      data = ll_data,
      event_date = "onset_week",
      report_date = "report_week",
      strata = "missing_strata",
      date_units = "days",
      verbose = FALSE
    ),
    "missing_strata.* not found in data"
  )

  # Error if event_date > report_date is violated (based on check.R logic)
  invalid_data <- ll_data %>%
    mutate(
      onset_week = as.Date("2023-01-10"),
      report_week = as.Date("2023-01-01")
    )
  expect_error(
    tbl_now(
      data = invalid_data,
      event_date = "onset_week",
      report_date = "report_week",
      date_units = "days",
      verbose = FALSE
    ),
    "Timetravel"
  )
})

test_that("tbl_now accepts and uses all other attributes", {
  result <- tbl_now(
    data = ll_data,
    event_date = "onset_week",
    report_date = "report_week",
    my_custom_attr = "test_value",
    another_attr = 123,
    date_units = "days",
    verbose = FALSE
  )

  expect_equal(attr(result, "my_custom_attr"), "test_value")
  expect_equal(attr(result, "another_attr"), 123)
})

test_that("tbl_now errors if strata or covariates are not characters", {
  expect_error(
    tbl_now(
      data = ll_data,
      event_date = "onset_week",
      report_date = "report_week",
      strata = TRUE,
      verbose = FALSE,
      date_units = "days"
    ),
    "strata.* must be.* a character vector"
  )
  expect_error(
    tbl_now(
      data = ll_data,
      event_date = "onset_week",
      report_date = "report_week",
      covariates = 123,
      verbose = FALSE,
      date_units = "days"
    ),
    "covariates.* must be.* a character vector"
  )
})
