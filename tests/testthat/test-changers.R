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
    "must be a character vector of length 1"
  )
})

test_that("change_event_date fails with multiple values", {
  test_data <- setup_test_data()

  expect_error(
    change_event_date(test_data$ndata, c("onset_week", "report_week")),
    "must be a character vector of length 1"
  )
})

test_that("change_event_date fails when column not found", {
  test_data <- setup_test_data()

  expect_error(
    change_event_date(test_data$ndata, "nonexistent"),
    "not found in data"
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
    "must be a character vector of length 1"
  )
})

test_that("change_report_date fails when column not found", {
  test_data <- setup_test_data()

  expect_error(
    change_report_date(test_data$ndata, "missing_col"),
    "not found in data"
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
    "must be.*NULL.*or a character vector"
  )
})

test_that("change_strata fails when column not found", {
  test_data <- setup_test_data()

  expect_error(
    change_strata(test_data$ndata, "nonexistent_strata"),
    "not found in data"
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
  ndata <- test_data$ndata

  # Add three strata
  ndata <- change_strata(ndata, c("gender", "age_group", "temperature"))

  result <- remove_strata(ndata, c("gender", "age_group"))

  expect_equal(get_strata(result), "temperature")
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
    "must be.*NULL.*or a character vector"
  )
})

test_that("change_covariates fails when column not found", {
  test_data <- setup_test_data()

  expect_error(
    change_covariates(test_data$ndata, "nonexistent_covariate"),
    "not found in data"
  )
})

# Tests for remove_covariate() ----
test_that("remove_covariate removes specified covariate", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  # Add multiple covariates first
  ndata <- change_covariates(ndata, c("temperature", "humidity"))

  result <- remove_covariate(ndata, "temperature")

  expect_s3_class(result, "tbl_now")
  expect_equal(get_covariates(result), "humidity")
  expect_equal(get_num_covariates(result), 1)
})

test_that("remove_covariate removes all if only one covariate", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  result <- remove_covariate(ndata, "temperature")

  expect_s3_class(result, "tbl_now")
  expect_equal(get_num_covariates(result), 0)
})

test_that("remove_covariate can remove multiple covariates", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  # Add three covariates
  ndata$rainfall <- c(10, 20, 15, 18)
  ndata <- change_covariates(ndata, c("temperature", "humidity", "rainfall"))

  result <- remove_covariate(ndata, c("temperature", "humidity"))

  expect_equal(get_covariates(result), "rainfall")
  expect_equal(get_num_covariates(result), 1)
})

# Tests for add_covariate() ----
test_that("add_covariate adds new covariate", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  result <- add_covariate(ndata, "humidity")

  expect_s3_class(result, "tbl_now")
  expect_equal(get_covariates(result), c("humidity", "temperature"))
  expect_equal(get_num_covariates(result), 2)
})

test_that("add_covariate adds to existing covariates", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  # Start with one covariate
  expect_equal(get_covariates(ndata), "temperature")

  # Add another
  result <- add_covariate(ndata, "humidity")

  expect_equal(get_covariates(result), c("humidity", "temperature"))
})

test_that("add_covariate works when no existing covariates", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  # Remove all covariates first
  ndata <- change_covariates(ndata, NULL)

  result <- add_covariate(ndata, "temperature")

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
    "must be a Date object of length 1"
  )
})

test_that("change_now fails with multiple dates", {
  test_data <- setup_test_data()

  expect_error(
    change_now(test_data$ndata, as.Date(c("2020-08-01", "2020-08-02"))),
    "must be a Date object of length 1"
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

  result <- ndata %>%
    change_event_date("new_onset") %>%
    change_report_date("new_report") %>%
    add_strata("age_group") %>%
    add_covariate("humidity") %>%
    change_now(as.Date("2020-08-10"))

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

  result <- ndata %>%
    remove_strata("gender") %>%
    add_strata("gender")

  expect_equal(get_strata(result), "gender")
  expect_equal(get_num_strata(result), 1)
})

test_that("removing and adding same covariate works correctly", {
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  result <- ndata %>%
    remove_covariate("temperature") %>%
    add_covariate("temperature")

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
    strata = "gender",
    data_type = "count"
  )

  result <- add_strata(ndata, "gender")

  expect_s3_class(result, "tbl_now")
  expect_equal(get_data_type(result), "count")
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
