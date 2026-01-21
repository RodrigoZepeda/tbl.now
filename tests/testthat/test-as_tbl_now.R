# Test file for as_tbl_now.R functions

# Setup test data ----
setup_test_data <- function() {
  list(
    # Simple data.frame
    simple_df = data.frame(
      onset_week = as.Date(c("2020-07-08", "2020-07-15", "2020-07-22")),
      report_week = as.Date(c("2020-07-11", "2020-07-18", "2020-07-25")),
      gender = c("Male", "Female", "Male"),
      value = 1:3
    ),

    # Tibble
    simple_tbl = dplyr::tibble(
      onset_week = as.Date(c("2020-07-08", "2020-07-15", "2020-07-22")),
      report_week = as.Date(c("2020-07-11", "2020-07-18", "2020-07-25")),
      gender = c("Male", "Female", "Male"),
      value = 1:3
    ),

    # Data with multiple potential date columns
    multi_dates = data.frame(
      event_date = as.Date(c("2020-07-08", "2020-07-15")),
      report_date = as.Date(c("2020-07-11", "2020-07-18")),
      other_date = as.Date(c("2020-07-10", "2020-07-17"))
    ),

    # Count data
    count_df = data.frame(
      onset_week = as.Date(c("2020-07-08", "2020-07-15")),
      report_week = as.Date(c("2020-07-11", "2020-07-18")),
      n = c(10L, 15L)
    ),

    # Data with covariates
    with_covariates = data.frame(
      onset_week = as.Date(c("2020-07-08", "2020-07-15")),
      report_week = as.Date(c("2020-07-11", "2020-07-18")),
      temperature = c(25.5, 26.0),
      humidity = c(0.6, 0.65)
    )
  )
}

# ============================================================================
# Tests for as_tbl_now() generic
# ============================================================================

test_that("as_tbl_now is a function", {
  expect_true(is.function(as_tbl_now))
  #expect_true(isS3stdGeneric("as_tbl_now"))
})

test_that("as_tbl_now has correct methods", {
  methods_list <- methods("as_tbl_now")

  expect_true("as_tbl_now.data.frame" %in% methods_list)
  expect_true("as_tbl_now.tbl_now" %in% methods_list)
})

# ============================================================================
# Tests for as_tbl_now.data.frame()
# ============================================================================

test_that("as_tbl_now.data.frame creates tbl_now from data.frame", {
  test_data <- setup_test_data()

  result <- as_tbl_now(
    test_data$simple_df,
    event_date = "onset_week",
    report_date = "report_week",
    verbose = FALSE
  )

  expect_s3_class(result, "tbl_now")
  expect_true(is_tbl_now(result))
})

test_that("as_tbl_now.data.frame works with tibble", {
  test_data <- setup_test_data()

  result <- as_tbl_now(
    test_data$simple_tbl,
    event_date = "onset_week",
    report_date = "report_week",
    verbose = FALSE
  )

  expect_s3_class(result, "tbl_now")
  expect_true(is_tbl_now(result))
})

test_that("as_tbl_now.data.frame accepts tbl_now parameters", {
  test_data <- setup_test_data()

  result <- as_tbl_now(
    test_data$simple_df,
    event_date = "onset_week",
    report_date = "report_week",
    strata = "gender",
    data_type = "linelist",
    verbose = FALSE
  )

  expect_equal(get_event_date(result), "onset_week")
  expect_equal(get_report_date(result), "report_week")
  expect_equal(get_strata(result), "gender")
  expect_equal(get_data_type(result), "linelist")
})

test_that("as_tbl_now.data.frame works with count data", {
  test_data <- setup_test_data()

  result <- as_tbl_now(
    test_data$count_df,
    event_date = "onset_week",
    report_date = "report_week",
    case_count = "n",
    data_type = "count-incidence",
    verbose = FALSE
  )

  expect_s3_class(result, "tbl_now")
  expect_equal(get_data_type(result), "count-incidence")
  expect_equal(get_case_count(result), "n")
})

test_that("as_tbl_now.data.frame passes extra arguments to tbl_now", {
  test_data <- setup_test_data()

  result <- as_tbl_now(
    test_data$with_covariates,
    event_date = "onset_week",
    report_date = "report_week",
    covariates = "temperature",
    now = as.Date("2020-08-01"),
    verbose = FALSE
  )

  expect_equal(get_covariates(result), "temperature")
  expect_equal(get_now(result), as.Date("2020-08-01"))
})

test_that("as_tbl_now.data.frame works with quoted column names", {
  test_data <- setup_test_data()

  result <- as_tbl_now(
    test_data$simple_df,
    event_date = "onset_week",
    report_date = "report_week",
    verbose = FALSE
  )

  expect_s3_class(result, "tbl_now")
  expect_equal(get_event_date(result), "onset_week")
})

test_that("as_tbl_now.data.frame works with unquoted column names", {
  test_data <- setup_test_data()

  result <- as_tbl_now(
    test_data$simple_df,
    event_date = onset_week,
    report_date = report_week,
    verbose = FALSE
  )

  expect_s3_class(result, "tbl_now")
  expect_equal(get_event_date(result), "onset_week")
})

test_that("as_tbl_now.data.frame fails with missing event_date", {
  test_data <- setup_test_data()

  expect_error(
    as_tbl_now(test_data$simple_df, report_date = "report_week")
  )
})

test_that("as_tbl_now.data.frame fails with missing report_date", {
  test_data <- setup_test_data()

  expect_error(
    as_tbl_now(test_data$simple_df, event_date = "onset_week")
  )
})

test_that("as_tbl_now.data.frame fails with non-existent event_date column", {
  test_data <- setup_test_data()

  expect_error(
    as_tbl_now(
      test_data$simple_df,
      event_date = "nonexistent",
      report_date = "report_week"
    ),
    "doesn't exist|not found"
  )
})

test_that("as_tbl_now.data.frame fails with non-existent report_date column", {
  test_data <- setup_test_data()

  expect_error(
    as_tbl_now(
      test_data$simple_df,
      event_date = "onset_week",
      report_date = "nonexistent"
    ),
    "doesn't exist|not found"
  )
})

test_that("as_tbl_now.data.frame is equivalent to tbl_now", {
  test_data <- setup_test_data()

  result1 <- as_tbl_now(
    test_data$simple_df,
    event_date = "onset_week",
    report_date = "report_week",
    strata = "gender",
    verbose = FALSE
  )

  result2 <- tbl_now(
    test_data$simple_df,
    event_date = "onset_week",
    report_date = "report_week",
    strata = "gender",
    verbose = FALSE
  )

  expect_equal(result1, result2)
})

# ============================================================================
# Tests for as_tbl_now.tbl_now()
# ============================================================================

test_that("as_tbl_now.tbl_now changes event_date", {
  test_data <- setup_test_data()

  original <- tbl_now(
    test_data$multi_dates,
    event_date = "event_date",
    report_date = "report_date",
    verbose = FALSE
  )

  expect_equal(get_event_date(original), "event_date")

  # Add other_date as new event_date
  result <- as_tbl_now(
    original,
    event_date = "other_date",
    report_date = "report_date"
  )

  expect_equal(get_event_date(result), "other_date")
  expect_s3_class(result, "tbl_now")
})

test_that("as_tbl_now.tbl_now changes report_date", {
  test_data <- setup_test_data()

  original <- tbl_now(
    test_data$multi_dates,
    event_date = "event_date",
    report_date = "report_date",
    verbose = FALSE
  )

  expect_equal(get_report_date(original), "report_date")

  suppressWarnings(
    result <- as_tbl_now(
      original,
      event_date = "event_date",
      report_date = "other_date"
    )
  )

  expect_equal(get_report_date(result), "other_date")
  expect_s3_class(result, "tbl_now")
})

test_that("as_tbl_now.tbl_now changes both dates", {
  test_data <- setup_test_data()

  # Create data with multiple date columns
  df <- data.frame(
    date1 = as.Date(c("2020-07-08", "2020-07-15")),
    date2 = as.Date(c("2020-07-11", "2020-07-18")),
    date3 = as.Date(c("2020-07-09", "2020-07-16")),
    date4 = as.Date(c("2020-07-12", "2020-07-19"))
  )

  original <- tbl_now(
    df,
    event_date = "date1",
    report_date = "date2",
    verbose = FALSE
  )

  suppressWarnings(
    result <- as_tbl_now(
      original,
      event_date = "date3",
      report_date = "date4"
    )
  )

  expect_equal(get_event_date(result), "date3")
  expect_equal(get_report_date(result), "date4")
})

test_that("as_tbl_now.tbl_now uses change_event_date internally", {
  test_data <- setup_test_data()

  original <- tbl_now(
    test_data$multi_dates,
    event_date = "event_date",
    report_date = "report_date",
    verbose = FALSE
  )

  result1 <- as_tbl_now(
    original,
    event_date = "other_date",
    report_date = "report_date"
  )

  result2 <- change_event_date(original, "other_date")

  expect_equal(result1, result2)
})

test_that("as_tbl_now.tbl_now uses change_report_date internally", {
  test_data <- setup_test_data()

  original <- tbl_now(
    test_data$multi_dates,
    event_date = "event_date",
    report_date = "report_date",
    verbose = FALSE
  )

  suppressWarnings({
    result1 <- as_tbl_now(
      original,
      event_date = "event_date",
      report_date = "other_date"
    )

    result2 <- change_report_date(original, "other_date")
  })

  expect_equal(result1, result2)
})

test_that("as_tbl_now.tbl_now preserves other attributes", {
  test_data <- setup_test_data()

  original <- tbl_now(
    test_data$simple_df,
    event_date = "onset_week",
    report_date = "report_week",
    strata = "gender",
    covariates = "value",
    verbose = FALSE
  )

  # Add new date column
  original$new_onset <- original$onset_week - 7

  result <- as_tbl_now(
    original,
    event_date = "new_onset",
    report_date = "report_week"
  )

  # Should preserve strata and covariates
  expect_equal(get_strata(result), get_strata(original))
  expect_equal(get_covariates(result), get_covariates(original))
  expect_equal(get_data_type(result), get_data_type(original))
})

test_that("as_tbl_now.tbl_now recalculates delay", {
  test_data <- setup_test_data()

  original <- tbl_now(
    test_data$multi_dates,
    event_date = "event_date",
    report_date = "report_date",
    verbose = FALSE
  )

  original_delay <- original$.delay

  # Change to other_date (which is between event and report)
  result <- as_tbl_now(
    original,
    event_date = "other_date",
    report_date = "report_date"
  )

  # Delays should be different
  expect_false(identical(original_delay, result$.delay))
})

test_that("as_tbl_now.tbl_now works with count data", {
  test_data <- setup_test_data()

  original <- tbl_now(
    test_data$count_df,
    event_date = "onset_week",
    report_date = "report_week",
    case_count = "n",
    data_type = "count-incidence",
    verbose = FALSE
  )

  # Add new date column
  original$new_onset <- original$onset_week - 7

  result <- as_tbl_now(
    original,
    event_date = "new_onset",
    report_date = "report_week"
  )

  expect_equal(get_data_type(result), "count-incidence")
  expect_equal(get_case_count(result), "n")
})

test_that("as_tbl_now.tbl_now fails with non-Date column", {
  test_data <- setup_test_data()

  original <- tbl_now(
    test_data$simple_df,
    event_date = "onset_week",
    report_date = "report_week",
    verbose = FALSE
  )

  expect_error(
    as_tbl_now(original, event_date = "gender", report_date = "report_week"),
    "must be of class Date"
  )
})

# ============================================================================
# Tests for method dispatch
# ============================================================================

test_that("as_tbl_now dispatches to correct method for data.frame", {
  test_data <- setup_test_data()

  result <- as_tbl_now(
    test_data$simple_df,
    event_date = "onset_week",
    report_date = "report_week",
    verbose = FALSE
  )

  expect_s3_class(result, "tbl_now")
})

test_that("as_tbl_now dispatches to correct method for tibble", {
  test_data <- setup_test_data()

  result <- as_tbl_now(
    test_data$simple_tbl,
    event_date = "onset_week",
    report_date = "report_week",
    verbose = FALSE
  )

  expect_s3_class(result, "tbl_now")
})

test_that("as_tbl_now dispatches to correct method for tbl_now", {
  test_data <- setup_test_data()

  original <- tbl_now(
    test_data$simple_df,
    event_date = "onset_week",
    report_date = "report_week",
    verbose = FALSE
  )

  original$new_onset <- original$onset_week - 7

  result <- as_tbl_now(
    original,
    event_date = "new_onset",
    report_date = "report_week",
    verbose = FALSE
  )

  expect_s3_class(result, "tbl_now")
  expect_equal(get_event_date(result), "new_onset")
})

# ============================================================================
# Integration tests
# ============================================================================

test_that("as_tbl_now works in pipe workflow with data.frame", {
  test_data <- setup_test_data()

  result <- test_data$simple_df %>%
    dplyr::mutate(adjusted_onset = onset_week - 7) %>%
    as_tbl_now(
      event_date = "adjusted_onset",
      report_date = "report_week",
      strata = "gender",
      verbose = FALSE
    )

  expect_s3_class(result, "tbl_now")
  expect_equal(get_event_date(result), "adjusted_onset")
  expect_equal(get_strata(result), "gender")
})

test_that("as_tbl_now works in pipe workflow with tbl_now", {
  test_data <- setup_test_data()

  result <- test_data$simple_df %>%
    tbl_now(
      event_date = "onset_week",
      report_date = "report_week",
      verbose = FALSE
    ) %>%
    dplyr::mutate(new_onset = onset_week - 7) %>%
    as_tbl_now(
      event_date = "new_onset",
      report_date = "report_week",
      verbose = FALSE
    )

  expect_s3_class(result, "tbl_now")
  expect_equal(get_event_date(result), "new_onset")
})

test_that("as_tbl_now can be chained multiple times", {
  df <- data.frame(
    date1 = as.Date(c("2020-07-08", "2020-07-15")),
    date2 = as.Date(c("2020-07-09", "2020-07-16")),
    date3 = as.Date(c("2020-07-11", "2020-07-18")),
    date4 = as.Date(c("2020-07-12", "2020-07-19"))
  )

  suppressWarnings(
    result <- df %>%
      as_tbl_now(event_date = "date1", report_date = "date3", verbose = FALSE) %>%
      as_tbl_now(event_date = "date2", report_date = "date3", verbose = FALSE) %>%
      as_tbl_now(event_date = "date2", report_date = "date4", verbose = FALSE)
  )

  expect_s3_class(result, "tbl_now")
  expect_equal(get_event_date(result), "date2")
  expect_equal(get_report_date(result), "date4")
})

test_that("as_tbl_now works with grouped data", {
  test_data <- setup_test_data()

  grouped <- test_data$simple_df %>%
    dplyr::group_by(gender)


  expect_warning(
    result <- as_tbl_now(
      grouped,
      event_date = "onset_week",
      report_date = "report_week",
      verbose = FALSE
    ),
    "grouped by gender"
  )

  suppressWarnings(
  result <- as_tbl_now(
    grouped,
    event_date = "onset_week",
    report_date = "report_week",
    verbose = FALSE
  )
  )

  expect_s3_class(result, "tbl_now")
  # Should be ungrouped after tbl_now
  expect_false(dplyr::is_grouped_df(result))
})

test_that("as_tbl_now preserves row order", {
  test_data <- setup_test_data()

  result <- as_tbl_now(
    test_data$simple_df,
    event_date = "onset_week",
    report_date = "report_week",
    verbose = FALSE
  )

  expect_equal(result$onset_week, test_data$simple_df$onset_week)
  expect_equal(result$gender, test_data$simple_df$gender)
})

# ============================================================================
# Documentation example tests
# ============================================================================

test_that("example from documentation works", {
  data(denguedat)

  result <- as_tbl_now(
    denguedat[1:100, ],
    event_date = onset_week,
    report_date = report_week,
    verbose = FALSE
  )

  expect_s3_class(result, "tbl_now")
  expect_equal(get_event_date(result), "onset_week")
  expect_equal(get_report_date(result), "report_week")
})

test_that("converting existing tbl_now works", {
  data(denguedat)

  # Create initial tbl_now
  df_now <- denguedat[1:100, ] %>%
    tbl_now(
      event_date = "onset_week",
      report_date = "report_week",
      verbose = FALSE
    )

  # Add new date column
  df_now$adjusted_onset <- df_now$onset_week - 7

  # Convert to use new date
  result <- as_tbl_now(
    df_now,
    event_date = "adjusted_onset",
    report_date = "report_week",
    verbose = FALSE
  )

  expect_s3_class(result, "tbl_now")
  expect_equal(get_event_date(result), "adjusted_onset")
})

# ============================================================================
# Edge cases
# ============================================================================

test_that("as_tbl_now handles single row data.frame", {
  single_row <- data.frame(
    onset_week = as.Date("2020-07-08"),
    report_week = as.Date("2020-07-11")
  )

  result <- as_tbl_now(
    single_row,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    verbose = FALSE
  )

  expect_s3_class(result, "tbl_now")
  expect_equal(nrow(result), 1)
})

test_that("as_tbl_now crashes with empty data.frame", {
  empty_df <- data.frame(
    onset_week = as.Date(character(0)),
    report_week = as.Date(character(0))
  )

  expect_error(
    as_tbl_now(
      empty_df,
      event_date = "onset_week",
      report_date = "report_week",
      report_units = "weeks",
      event_units = "weeks",
      verbose = FALSE
    ),
    "empty data.frame"
  )

})

test_that("as_tbl_now preserves column types", {
  test_data <- setup_test_data()

  result <- as_tbl_now(
    test_data$simple_df,
    event_date = "onset_week",
    report_date = "report_week",
    verbose = FALSE
  )

  expect_true(lubridate::is.Date(result$onset_week))
  expect_true(lubridate::is.Date(result$report_week))
  expect_true(is.character(result$gender))
  expect_true(is.integer(result$value))
})

test_that("as_tbl_now handles NA values", {
  df_with_na <- data.frame(
    onset_week = as.Date(c("2020-07-08", "2020-07-15", NA)),
    report_week = as.Date(c("2020-07-11", "2020-07-18", "2020-07-20")),
    gender = c("Male", "Female", NA)
  )

  expect_warning(
    result <- as_tbl_now(
      df_with_na,
      event_date = "onset_week",
      report_date = "report_week",
      report_units = "weeks",
      event_units = "weeks",
      strata = "gender",
      verbose = FALSE
    ),
    "have NULL or NA values"
  )

  suppressWarnings(
    result <- as_tbl_now(
      df_with_na,
      event_date = "onset_week",
      report_date = "report_week",
      report_units = "weeks",
      event_units = "weeks",
      strata = "gender",
      verbose = FALSE
    )
  )

  expect_s3_class(result, "tbl_now")
  expect_equal(nrow(result), 3)
})

test_that("as_tbl_now validates result", {
  test_data <- setup_test_data()

  result <- as_tbl_now(
    test_data$simple_df,
    event_date = "onset_week",
    report_date = "report_week",
    verbose = FALSE
  )

  # Should pass validation
  expect_true(validate_tbl_now(result))
})
