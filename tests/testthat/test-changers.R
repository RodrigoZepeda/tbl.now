# Test file for changers.R functions

# Setup test data ----
setup_test_data <- function() {
  # Create a basic tbl_now object for testing
  base_data <- data.frame(
    onset_week = as.Date(c("2020-07-08", "2020-07-15", "2020-07-22", "2020-07-29")),
    report_week = as.Date(c("2020-07-11", "2020-07-18", "2020-07-25", "2020-08-01")),
    gender = c("Male", "Female", "Male", "Female"),
    age_group = c("20-30", "30-40", "20-30", "40-50"),
    temperature = c(25.5, 26.0, 24.8, 25.2),
    humidity = c(0.6, 0.65, 0.7, 0.68)
  )

  ndata <- tbl_now(
    base_data,
    event_date = "onset_week",
    report_date = "report_week",
    strata = "gender",
    covariates = "temperature",
    verbose = FALSE
  )

  list(
    ndata = ndata,
    base_data = base_data
  )
}

# Tests for change_event_date() ----
test_that("change_event_date changes event_date to new column", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  # Add new date column
  ndata$new_onset <- ndata$onset_week - 1

  result <- change_event_date(ndata, "new_onset")

  expect_s3_class(result, "tbl_now")
  expect_equal(get_event_date(result), "new_onset")
  expect_true(validate_tbl_now(result))

  result <- change_event_date(ndata, new_onset)

  expect_s3_class(result, "tbl_now")
  expect_equal(get_event_date(result), "new_onset")
  expect_true(validate_tbl_now(result))
})

test_that("change_event_date fails with non-tbl_now object", {
  regular_df <- data.frame(
    onset_week = as.Date("2020-07-08"),
    report_week = as.Date("2020-07-11")
  )

  expect_error(
    change_event_date(regular_df, "onset_week"),
    "must be a.*tbl_now.*object"
  )
})

test_that("change_event_date fails with non-character value", {
  test_data <- setup_test_data()

  expect_error(
    change_event_date(test_data$ndata, 123),
    "Can't select columns past the end"
  )
})

test_that("change_event_date fails with multiple values", {
  test_data <- setup_test_data()

  expect_error(
    change_event_date(test_data$ndata, c("onset_week", "report_week")),
    "length 1"
  )
})

test_that("change_event_date fails when column not found", {
  test_data <- setup_test_data()

  expect_error(
    change_event_date(test_data$ndata, "nonexistent"),
    "doesn't exist"
  )
})

test_that("change_event_date fails when column is not Date", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  ndata$char_col <- "not a date"

  expect_error(
    change_event_date(ndata, "char_col"),
    "must be of class Date"
  )
})

test_that("change_event_date updates now if needed", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  original_now <- get_now(ndata)

  # Add new date column that extends beyond current now
  ndata$new_onset <- ndata$onset_week + 1

  result <- change_event_date(ndata, "new_onset")

  # Now should potentially be updated
  expect_s3_class(get_now(result), "Date")
})

# Tests for change_report_date() ----
test_that("change_report_date changes report_date to new column", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  # Add new date column
  ndata$new_report <- ndata$report_week + 1

  expect_warning(
    change_report_date(ndata, "new_report"),
    "seems to be in the past"
  )

  result <- suppressWarnings(
    change_report_date(ndata, "new_report")
  )

  expect_s3_class(result, "tbl_now")
  expect_equal(get_report_date(result), "new_report")

  expect_warning(
    validate_tbl_now(result),
    "seems to be in the past"
  )

  expect_true(
    suppressWarnings(validate_tbl_now(result))
  )
})

test_that("change_report_date fails with non-tbl_now object", {
  regular_df <- data.frame(
    onset_week = as.Date("2020-07-08"),
    report_week = as.Date("2020-07-11")
  )

  expect_error(
    change_report_date(regular_df, "report_week"),
    "must be a.*tbl_now.*object"
  )
})

test_that("change_report_date fails with non-character value", {
  test_data <- setup_test_data()

  expect_error(
    change_report_date(test_data$ndata, TRUE),
    "must be numeric or character, not `TRUE`"
  )
})

test_that("change_report_date fails when column not found", {
  test_data <- setup_test_data()

  expect_error(
    change_report_date(test_data$ndata, "missing_col"),
    "doesn't exist"
  )
})

test_that("change_report_date fails when column is not Date", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  ndata$numeric_col <- 123

  expect_error(
    change_report_date(ndata, "numeric_col"),
    "must be of class Date"
  )
})

# Tests for change_strata() ----
test_that("change_strata changes strata to new columns", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  result <- change_strata(ndata, c("gender", "age_group"))

  expect_s3_class(result, "tbl_now")
  expect_equal(get_strata(result), c("gender", "age_group"))
  expect_equal(get_num_strata(result), 2)
  expect_true(validate_tbl_now(result))
})

test_that("change_strata accepts NULL to remove all strata", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  result <- change_strata(ndata, NULL)

  expect_s3_class(result, "tbl_now")
  expect_null(get_strata(result))
  expect_equal(get_num_strata(result), 0)
})

test_that("change_strata fails with non-tbl_now object", {
  regular_df <- data.frame(gender = "Male")

  expect_error(
    change_strata(regular_df, "gender"),
    "must be a.*tbl_now.*object"
  )
})

test_that("change_strata fails with non-character non-NULL value", {
  test_data <- setup_test_data()

  expect_error(
    change_strata(test_data$ndata, 123),
    "doesn't exist"
  )
})

test_that("change_strata fails when column not found", {
  test_data <- setup_test_data()

  expect_error(
    change_strata(test_data$ndata, "nonexistent_strata"),
    "doesn't exist"
  )
})

# Tests for remove_strata() ----
test_that("remove_strata removes specified strata", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  # Add multiple strata first
  ndata <- change_strata(ndata, c("gender", "age_group"))

  result <- remove_strata(ndata, "gender")

  expect_s3_class(result, "tbl_now")
  expect_equal(get_strata(result), "age_group")
  expect_equal(get_num_strata(result), 1)
})

test_that("remove_strata removes all if only one strata", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  result <- remove_strata(ndata, "gender")

  expect_s3_class(result, "tbl_now")
  expect_equal(get_num_strata(result), 0)
})

test_that("remove_strata can remove multiple strata", {
  test_data <- setup_test_data()
  test_data$ndata$temperature2 <- test_data$ndata$temperature * 2
  ndata <- test_data$ndata

  # Add three strata
  ndata <- change_strata(ndata, c("gender", "age_group", "temperature2"))

  result <- remove_strata(ndata, c("gender", "age_group"))

  expect_equal(get_strata(result), "temperature2")
  expect_equal(get_num_strata(result), 1)
})

# Tests for add_strata() ----
test_that("add_strata adds new strata", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  result <- add_strata(ndata, "age_group")

  expect_s3_class(result, "tbl_now")
  expect_equal(get_strata(result), c("age_group", "gender"))
  expect_equal(get_num_strata(result), 2)
})

test_that("add_strata adds to existing strata", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  # Start with one strata
  expect_equal(get_strata(ndata), "gender")

  # Add another
  result <- add_strata(ndata, "age_group")

  expect_equal(get_strata(result), c("age_group", "gender"))
})

test_that("add_strata works when no existing strata", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  # Remove all strata first
  ndata <- change_strata(ndata, NULL)

  result <- add_strata(ndata, "gender")

  expect_equal(get_strata(result), "gender")
  expect_equal(get_num_strata(result), 1)
})

# Tests for remove_all_strata() ----
test_that("remove_all_strata removes all strata", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  # Add multiple strata
  ndata <- change_strata(ndata, c("gender", "age_group"))

  result <- remove_all_strata(ndata)

  expect_s3_class(result, "tbl_now")
  expect_null(get_strata(result))
  expect_equal(get_num_strata(result), 0)
})

test_that("remove_all_strata works when no strata exist", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  ndata <- change_strata(ndata, NULL)

  result <- remove_all_strata(ndata)

  expect_null(get_strata(result))
  expect_equal(get_num_strata(result), 0)
})

# Tests for change_covariates() ----
test_that("change_covariates changes covariates to new columns", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  result <- change_covariates(ndata, c("temperature", "humidity"))

  expect_s3_class(result, "tbl_now")
  expect_equal(get_covariates(result), c("temperature", "humidity"))
  expect_equal(get_num_covariates(result), 2)
  expect_true(validate_tbl_now(result))
})

test_that("change_covariates accepts NULL to remove all covariates", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  result <- change_covariates(ndata, NULL)

  expect_s3_class(result, "tbl_now")
  expect_null(get_covariates(result))
  expect_equal(get_num_covariates(result), 0)
})

test_that("change_covariates fails with non-tbl_now object", {
  regular_df <- data.frame(temperature = 25.5)

  expect_error(
    change_covariates(regular_df, "temperature"),
    "must be a.*tbl_now.*object"
  )
})

test_that("change_covariates fails with non-character non-NULL value", {
  test_data <- setup_test_data()

  expect_error(
    change_covariates(test_data$ndata, list("temperature")),
    "must be numeric or character"
  )
})

test_that("change_covariates fails when column not found", {
  test_data <- setup_test_data()

  expect_error(
    change_covariates(test_data$ndata, "nonexistent_covariate"),
    "doesn't exist"
  )
})

# Tests for remove_covariates() ----
test_that("remove_covariates removes specified covariate", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  # Add multiple covariates first
  ndata <- change_covariates(ndata, c("temperature", "humidity"))

  result <- remove_covariates(ndata, "temperature")

  expect_s3_class(result, "tbl_now")
  expect_equal(get_covariates(result), "humidity")
  expect_equal(get_num_covariates(result), 1)
})

test_that("remove_covariates removes all if only one covariate", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  result <- remove_covariates(ndata, "temperature")

  expect_s3_class(result, "tbl_now")
  expect_equal(get_num_covariates(result), 0)
})

test_that("remove_covariates can remove multiple covariates", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  # Add three covariates
  ndata$rainfall <- c(10, 20, 15, 18)
  ndata <- change_covariates(ndata, c("temperature", "humidity", "rainfall"))

  result <- remove_covariates(ndata, c("temperature", "humidity"))

  expect_equal(get_covariates(result), "rainfall")
  expect_equal(get_num_covariates(result), 1)
})

# Tests for add_covariates() ----
test_that("add_covariates adds new covariate", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  result <- add_covariates(ndata, "humidity")

  expect_s3_class(result, "tbl_now")
  expect_equal(get_covariates(result), c("humidity", "temperature"))
  expect_equal(get_num_covariates(result), 2)
})

test_that("add_covariates adds to existing covariates", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  # Start with one covariate
  expect_equal(get_covariates(ndata), "temperature")

  # Add another
  result <- add_covariates(ndata, "humidity")

  expect_equal(get_covariates(result), c("humidity", "temperature"))
})

test_that("add_covariates works when no existing covariates", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  # Remove all covariates first
  ndata <- change_covariates(ndata, NULL)

  result <- add_covariates(ndata, "temperature")

  expect_equal(get_covariates(result), "temperature")
  expect_equal(get_num_covariates(result), 1)
})

# Tests for remove_all_covariates() ----
test_that("remove_all_covariates removes all covariates", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  # Add multiple covariates
  ndata <- change_covariates(ndata, c("temperature", "humidity"))

  result <- remove_all_covariates(ndata)

  expect_s3_class(result, "tbl_now")
  expect_null(get_covariates(result))
  expect_equal(get_num_covariates(result), 0)
})

test_that("remove_all_covariates works when no covariates exist", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  ndata <- change_covariates(ndata, NULL)

  result <- remove_all_covariates(ndata)

  expect_null(get_covariates(result))
  expect_equal(get_num_covariates(result), 0)
})

# Tests for change_now() ----
test_that("change_now changes the now date", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  new_now <- as.Date("2020-08-05")
  result <- change_now(ndata, new_now)

  expect_s3_class(result, "tbl_now")
  expect_equal(get_now(result), new_now)
  expect_true(validate_tbl_now(result))
})

test_that("change_now fails with non-tbl_now object", {
  regular_df <- data.frame(
    onset_week = as.Date("2020-07-08"),
    report_week = as.Date("2020-07-11")
  )

  expect_error(
    change_now(regular_df, as.Date("2020-07-15")),
    "must be a.*tbl_now.*object"
  )
})

test_that("change_now fails with non-Date value", {
  test_data <- setup_test_data()

  expect_error(
    change_now(test_data$ndata, "2020-08-01"),
    "must be a Date of length 1"
  )
})

test_that("change_now fails with multiple dates", {
  test_data <- setup_test_data()

  expect_error(
    change_now(test_data$ndata, as.Date(c("2020-08-01", "2020-08-02"))),
    "must be a Date of length 1"
  )
})

test_that("change_now re-infers now if needed", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  # Try to set now to a date
  new_now <- as.Date("2021-07-20")
  result <- change_now(ndata, new_now)

  # Check that now was set (might be adjusted by infer_now)
  expect_s3_class(get_now(result), "Date")
})

# Integration tests ----
test_that("multiple changer functions work together", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  # Chain multiple operations
  ndata$new_onset <- ndata$onset_week - 1
  ndata$new_report <- ndata$report_week - 1

  suppressWarnings(
    result <- ndata |>
      change_event_date("new_onset") |>
      change_report_date("new_report") |>
      add_strata("age_group") |>
      add_covariates("humidity") |>
      change_now(as.Date("2020-08-10"))
  )

  expect_s3_class(result, "tbl_now")
  expect_equal(get_event_date(result), "new_onset")
  expect_equal(get_report_date(result), "new_report")
  expect_equal(get_strata(result), c("age_group", "gender"))
  expect_equal(get_covariates(result), c("humidity", "temperature"))
  expect_true(validate_tbl_now(result))
})

test_that("changer functions preserve other attributes", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  original_units <- get_event_units(ndata)
  original_data_type <- get_data_type(ndata)

  result <- change_strata(ndata, c("gender", "age_group"))

  expect_equal(get_event_units(result), original_units)
  expect_equal(get_data_type(result), original_data_type)
})

test_that("changer functions maintain data integrity", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  original_nrows <- nrow(ndata)
  original_ncols <- ncol(ndata)

  result <- change_strata(ndata, c("gender", "age_group"))

  expect_equal(nrow(result), original_nrows)
  # ncol might change due to .event_num and .report_num
  expect_gte(ncol(result), original_ncols - 2)
})

test_that("removing and adding same strata works correctly", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  result <- ndata |>
    remove_strata("gender") |>
    add_strata("gender")

  expect_equal(get_strata(result), "gender")
  expect_equal(get_num_strata(result), 1)
})

test_that("removing and adding same covariate works correctly", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  result <- ndata |>
    remove_covariates("temperature") |>
    add_covariates("temperature")

  expect_equal(get_covariates(result), "temperature")
  expect_equal(get_num_covariates(result), 1)
})

test_that("changer functions work with count data", {
  count_data <- data.frame(
    onset_week = as.Date(c("2020-07-08", "2020-07-15", "2020-07-22")),
    report_week = as.Date(c("2020-07-11", "2020-07-18", "2020-07-25")),
    gender = c("Male", "Female", "Male"),
    n = c(5L, 3L, 7L)
  )

  ndata <- tbl_now(
    count_data,
    event_date = "onset_week",
    report_date = "report_week",
    case_count = n,
    strata = "gender",
    data_type = "count-incidence"
  )

  result <- add_strata(ndata, gender)

  expect_s3_class(result, "tbl_now")
  expect_equal(get_data_type(result), "count-incidence")
})

test_that("changer functions validate after each change", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  # Each of these should trigger validation
  expect_silent({
    result1 <- change_strata(ndata, "gender")
    result2 <- change_covariates(ndata, "temperature")
    result3 <- change_now(ndata, as.Date("2020-08-01"))
  })

  # All results should be valid
  expect_true(validate_tbl_now(result1))
  expect_true(validate_tbl_now(result2))
  expect_true(validate_tbl_now(result3))
})

# Additional tests for adders_changers_and_removers.R
# These complement existing tests in test-changers.R

# Setup test data ----
setup_additional_test_data <- function() {
  base_data <- data.frame(
    onset_week = as.Date(c("2020-07-08", "2020-07-15", "2020-07-22", "2020-07-29")),
    report_week = as.Date(c("2020-07-11", "2020-07-18", "2020-07-25", "2020-08-01")),
    gender = c("Male", "Female", "Male", "Female"),
    age_group = c("20-30", "30-40", "20-30", "40-50"),
    region = c("North", "South", "North", "South"),
    temperature = c(25.5, 26.0, 24.8, 25.2),
    humidity = c(0.6, 0.65, 0.7, 0.68),
    is_censored = c(FALSE, FALSE, TRUE, FALSE)
  )

  ndata <- tbl_now(
    base_data,
    event_date = "onset_week",
    report_date = "report_week",
    strata = "gender",
    covariates = "temperature",
    is_censored = "is_censored",
    verbose = FALSE
  )

  list(ndata = ndata, base_data = base_data)
}

# ============================================================================
# Additional tests for change_event_date()
# ============================================================================

test_that("change_event_date works with tidy select helpers", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  # Add a column that starts with 'onset'
  ndata$onset_week_new <- ndata$onset_week - 7
  result <- change_event_date(ndata, dplyr::starts_with("onset_week_new"))

  expect_equal(get_event_date(result), "onset_week_new")
})

test_that("change_event_date recalculates delay correctly", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  original_delay <- ndata$.delay

  # Add new event date that's 7 days earlier
  ndata$new_onset <- ndata$onset_week - 7
  result <- change_event_date(ndata, "new_onset")

  # Delay should increase by 7 days (or 1 week if units are weeks)
  expect_true(all(result$.delay > original_delay))
})

test_that("change_event_date updates .event_num correctly", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  # Add new event date
  ndata$new_onset <- ndata$onset_week - 14


  result <- change_event_date(ndata, "new_onset")

  # .event_num should be recalculated
  expect_true(".event_num" %in% names(result))
  # First event should still be 0 (anchor point)
  expect_equal(min(result$.event_num), 0)
})

test_that("change_event_date fails with integer column when original was Date", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  ndata$int_col <- 1:4

  expect_error(
    change_event_date(ndata, "int_col"),
    "must be of class Date"
  )
})

# ============================================================================
# Additional tests for change_report_date()
# ============================================================================

test_that("change_report_date works with tidy select helpers", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  ndata$report_week_updated <- ndata$report_week + 7

  suppressWarnings(
    result <- change_report_date(ndata, dplyr::starts_with("report_week_upd"))
  )

  expect_equal(get_report_date(result), "report_week_updated")
})

test_that("change_report_date recalculates delay correctly", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  original_delay <- ndata$.delay

  # Add new report date that's 7 days later
  ndata$new_report <- ndata$report_week + 7

  suppressWarnings(
    result <- change_report_date(ndata, "new_report")
  )

  # Delay should increase by 7 days
  expect_true(all(result$.delay > original_delay))
})

test_that("change_report_date updates .report_num correctly", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  ndata$new_report <- ndata$report_week + 14

  suppressWarnings(
    result <- change_report_date(ndata, "new_report")
  )

  # .report_num should be recalculated
  expect_true(".report_num" %in% names(result))
})

test_that("change_report_date warns when report is before event", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  # Create report dates before event dates
  ndata$bad_report <- ndata$onset_week - 7

  expect_warning(
    change_report_date(ndata, "bad_report"),
    "before"
  )
})

# ============================================================================
# Additional tests for change_case_count()
# ============================================================================

test_that("change_case_count works with count data", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata |>
    to_count(to = "count-incidence")

  ndata$n_adjusted <- ndata$n * 1.1

  result <- change_case_count(ndata, n_adjusted)

  expect_equal(get_case_count(result), "n_adjusted")
  expect_s3_class(result, "tbl_now")
})

test_that("change_case_count cannot remove case_count", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata |>
    to_count(to = "count-incidence")

  expect_error(change_case_count(ndata, NULL), "Dropped")
})

test_that("change_case_count fails with non-numeric column", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata |>
    to_count(to = "count-incidence")

  ndata$char_col <- "not_numeric"

  expect_error(
    change_case_count(ndata, char_col),
    "must be numeric"
  )
})

test_that("change_case_count works with tidy select", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata |>
    to_count(to = "count-incidence")

  ndata$n_new <- ndata$n * 2

  result <- change_case_count(ndata, dplyr::starts_with("n_new"))

  expect_equal(get_case_count(result), "n_new")
})

# ============================================================================
# Additional tests for change_is_censored()
# ============================================================================

test_that("change_is_censored accepts NULL", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  result <- change_is_censored(ndata, NULL)

  expect_null(get_is_censored(result))
})

test_that("change_is_censored fails with non-logical column", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  ndata$numeric_censor <- c(0, 1, 0, 1)

  expect_error(
    change_is_censored(ndata, numeric_censor),
    "must be logical"
  )
})

test_that("change_is_censored works with tidy select", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  ndata$is_censored_new <- c(TRUE, FALSE, TRUE, FALSE)

  result <- change_is_censored(ndata, dplyr::starts_with("is_censored_new"))

  expect_equal(get_is_censored(result), "is_censored_new")
})

# ============================================================================
# Additional tests for remove_is_censored()
# ============================================================================

test_that("remove_is_censored sets is_censored to NULL", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  expect_equal(get_is_censored(ndata), "is_censored")

  result <- remove_is_censored(ndata)

  expect_null(get_is_censored(result))
})

test_that("remove_is_censored works when is_censored is already NULL", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata |>
    change_is_censored(NULL)

  expect_null(get_is_censored(ndata))

  result <- remove_is_censored(ndata)

  expect_null(get_is_censored(result))
  expect_s3_class(result, "tbl_now")
})

# ============================================================================
# Additional tests for add_is_censored()
# ============================================================================

test_that("add_is_censored adds is_censored when none exists", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata |>
    remove_is_censored()

  expect_null(get_is_censored(ndata))

  ndata$new_censored <- c(TRUE, TRUE, FALSE, FALSE)
  result <- add_is_censored(ndata, new_censored)

  expect_equal(get_is_censored(result), "new_censored")
})

test_that("add_is_censored fails when is_censored already exists", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  expect_equal(get_is_censored(ndata), "is_censored")

  ndata$new_censored <- c(FALSE, FALSE, FALSE, FALSE)

  expect_error(
    add_is_censored(ndata, new_censored),
    "Already has value"
  )
})

# ============================================================================
# Additional tests for change_strata() - edge cases
# ============================================================================

test_that("change_strata works with multiple columns via tidy select", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  result <- change_strata(ndata, dplyr::starts_with("age"), dplyr::starts_with("region"))

  expect_equal(sort(get_strata(result)), c("age_group", "region"))
})

test_that("change_strata handles empty selection gracefully", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  # This should set strata to NULL
  result <- change_strata(ndata, character(0))

  expect_null(get_strata(result))
})

test_that("change_strata preserves column order", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  result <- change_strata(ndata, region, age_group, gender)

  expect_equal(get_strata(result), c("region", "age_group", "gender"))
})

# ============================================================================
# Additional tests for add_strata() - edge cases
# ============================================================================

test_that("add_strata prevents duplicate strata", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  # gender is already a stratum
  result <- add_strata(ndata, gender)

  # Should not duplicate
  expect_equal(sum(get_strata(result) == "gender"), 1)
})

test_that("add_strata works with multiple columns at once", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  result <- add_strata(ndata, age_group, region)

  strata <- get_strata(result)
  expect_true("age_group" %in% strata)
  expect_true("region" %in% strata)
  expect_true("gender" %in% strata)
})

test_that("add_strata works with tidy select helpers", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  result <- add_strata(ndata, dplyr::starts_with("age"))

  expect_true("age_group" %in% get_strata(result))
})

# ============================================================================
# Additional tests for remove_strata() - edge cases
# ============================================================================

test_that("remove_strata ignores non-existent strata", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  # Try to remove strata that doesn't exist
  result <- remove_strata(ndata, region)

  # Should still have gender
  expect_equal(get_strata(result), "gender")
})

test_that("remove_strata can remove multiple strata at once", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata |>
    change_strata(gender, age_group, region)

  result <- remove_strata(ndata, gender, age_group)

  expect_equal(get_strata(result), "region")
})

test_that("remove_strata with tidy select", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata |>
    change_strata(gender, age_group)

  result <- remove_strata(ndata, dplyr::starts_with("age"))

  expect_equal(get_strata(result), "gender")
})

# ============================================================================
# Additional tests for change_covariates() - edge cases
# ============================================================================

test_that("change_covariates works with multiple columns via tidy select", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  result <- change_covariates(ndata, dplyr::starts_with("temp"), dplyr::starts_with("hum"))

  expect_equal(sort(get_covariates(result)), c("humidity", "temperature"))
})

test_that("change_covariates handles empty selection", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  result <- change_covariates(ndata, character(0))

  expect_null(get_covariates(result))
})

test_that("change_covariates preserves column order", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  result <- change_covariates(ndata, humidity, temperature)

  expect_equal(get_covariates(result), c("humidity", "temperature"))
})

# ============================================================================
# Additional tests for add_covariates() - edge cases
# ============================================================================

test_that("add_covariates prevents duplicates", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  # temperature is already a covariate
  result <- add_covariates(ndata, temperature)

  # Should not duplicate
  expect_equal(sum(get_covariates(result) == "temperature"), 1)
})

test_that("add_covariates works with multiple columns", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  result <- add_covariates(ndata, humidity, age_group)

  covs <- get_covariates(result)
  expect_true("humidity" %in% covs)
  expect_true("age_group" %in% covs)
  expect_true("temperature" %in% covs)
})

test_that("add_covariates with tidy select", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  result <- add_covariates(ndata, dplyr::starts_with("hum"))

  expect_true("humidity" %in% get_covariates(result))
})

# ============================================================================
# Additional tests for remove_covariates() - edge cases
# ============================================================================

test_that("remove_covariates ignores non-existent covariates", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  # Try to remove covariate that doesn't exist
  result <- remove_covariates(ndata, region)

  # Should still have temperature
  expect_equal(get_covariates(result), "temperature")
})

test_that("remove_covariates can remove multiple at once", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata |>
    change_covariates(temperature, humidity, age_group)

  result <- remove_covariates(ndata, temperature, humidity)

  expect_equal(get_covariates(result), "age_group")
})

test_that("remove_covariates with tidy select", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata |>
    change_covariates(temperature, humidity)

  result <- remove_covariates(ndata, dplyr::starts_with("temp"))

  expect_equal(get_covariates(result), "humidity")
})

# ============================================================================
# Additional tests for temporal effects
# ============================================================================

test_that("replace_temporal_effects removes old effects", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata |>
    add_temporal_effects(temporal_effects(day_of_week = TRUE))

  old_cols <- get_temporal_effects(ndata)
  expect_gt(length(old_cols), 0)

  result <- replace_temporal_effects(ndata, t_effects = temporal_effects(week_of_year = TRUE))

  new_cols <- get_temporal_effects(result)

  # Old columns should be removed
  expect_false(any(old_cols %in% names(result)))
  # New columns should exist
  expect_gt(length(new_cols), 0)
})

test_that("replace_temporal_effects with NULL removes all", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata |>
    add_temporal_effects(temporal_effects(day_of_week = TRUE))

  result <- replace_temporal_effects(ndata, NULL)

  expect_equal(length(get_temporal_effects(result)), 0L)
})

test_that("replace_temporal_effects fails with non-temporal_effects object", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  expect_error(
    replace_temporal_effects(ndata, list(day_of_week = TRUE)),
    "must be.*NULL.*temporal_effects"
  )
})

test_that("remove_temporal_effects removes all temporal effect columns", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata |>
    add_temporal_effects(temporal_effects(
      day_of_week = TRUE,
      week_of_year = TRUE
    ))

  # Spec is stored (lazy — no columns yet)
  old_spec <- get_temporal_effects(ndata)
  expect_gt(length(old_spec), 0)

  result <- remove_temporal_effects(ndata)

  # Spec cleared
  expect_equal(length(get_temporal_effects(result)), 0L)
  # No computed columns (none were created)
  expect_equal(get_temporal_effect_cols(result), character(0))
})

# ============================================================================
# Integration tests - chaining operations
# ============================================================================

test_that("chaining multiple changers preserves tbl_now", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  ndata$new_onset <- ndata$onset_week - 7
  ndata$new_report <- ndata$report_week - 7

  result <- ndata |>
    change_event_date(new_onset) |>
    change_report_date(new_report) |>
    add_strata(age_group) |>
    add_covariates(humidity) |>
    change_now(as.Date("2020-08-15"))

  expect_s3_class(result, "tbl_now")
  expect_equal(get_event_date(result), "new_onset")
  expect_equal(get_report_date(result), "new_report")
  expect_true("age_group" %in% get_strata(result))
  expect_true("humidity" %in% get_covariates(result))
})

test_that("add and remove operations can be chained", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  result <- ndata |>
    add_strata(age_group, region) |>
    remove_strata(gender) |>
    add_covariates(humidity) |>
    remove_covariates(temperature)

  expect_equal(sort(get_strata(result)), c("age_group", "region"))
  expect_equal(get_covariates(result), "humidity")
})

test_that("changers work with grouped tbl_now", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata |>
    dplyr::group_by(gender)

  result <- ndata |>
    add_strata(age_group) |>
    dplyr::ungroup()

  expect_s3_class(result, "tbl_now")
  expect_true("age_group" %in% get_strata(result))
})

test_that("changers preserve count data type", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata |>
    add_strata(age_group) |>
    add_covariates(humidity) |>
    to_count(to = "count-incidence") |>
    remove_strata(age_group) |>
    remove_covariates(humidity)

  result <- ndata |>
    add_strata(age_group) |>
    add_covariates(humidity)

  expect_equal(get_data_type(result), "count-incidence")
  expect_s3_class(result, "tbl_now")
})

# ============================================================================
# Validation tests
# ============================================================================

test_that("changers trigger validation", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  # Each changer should validate
  expect_silent(change_strata(ndata, gender))
  expect_silent(add_strata(ndata, age_group))
  expect_silent(remove_strata(ndata, gender))
  expect_silent(change_covariates(ndata, temperature))
  expect_silent(add_covariates(ndata, humidity))
  expect_silent(remove_covariates(ndata, temperature))
})

test_that("changers maintain .delay column", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  result <- ndata |>
    add_strata(age_group) |>
    add_covariates(humidity)

  expect_true(".delay" %in% names(result))
  expect_true(all(!is.na(result$.delay)))
})

test_that("changers maintain protected columns", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  result <- ndata |>
    add_strata(age_group) |>
    change_covariates(humidity)

  # Check all protected columns exist
  expect_true(".event_num" %in% names(result))
  expect_true(".report_num" %in% names(result))
  expect_true(".delay" %in% names(result))
  expect_true(get_event_date(result) %in% names(result))
  expect_true(get_report_date(result) %in% names(result))
})

# ============================================================================
# Error handling and edge cases
# ============================================================================

test_that("add functions fail gracefully with non-existent columns", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  expect_error(
    add_strata(ndata, nonexistent_col),
    "doesn't exist|Can't select columns"
  )

  expect_error(
    add_covariates(ndata, another_missing),
    "doesn't exist|Can't select columns"
  )
})

test_that("change functions validate column types", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  # Try to set non-Date column as event_date
  ndata$char_col <- "not_a_date"

  expect_error(
    change_event_date(ndata, char_col),
    "must be of class Date"
  )
})

test_that("remove functions handle already-removed attributes", {
  test_data <- setup_additional_test_data()
  ndata <- test_data$ndata

  result <- ndata |>
    remove_all_strata() |>
    remove_all_strata() # Second removal should work

  expect_null(get_strata(result))
  expect_s3_class(result, "tbl_now")
})
