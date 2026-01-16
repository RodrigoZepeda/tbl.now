# Test file for report.R functions

# Setup test data ----
setup_test_data <- function() {
  list(
    # Simple linelist data
    linelist_simple = data.frame(
      event_date = as.Date(c(
        "2020-07-08", "2020-07-08", "2020-07-08",
        "2020-07-09", "2020-07-09",
        "2020-07-10"
      )),
      report_date = as.Date(c(
        "2020-07-11", "2020-07-12", "2020-07-13",
        "2020-07-12", "2020-07-13",
        "2020-07-13"
      ))
    ),

    # Count data with revisions
    count_revisions = data.frame(
      event_date = as.Date(c(
        rep("2020-07-08", 4),
        rep("2020-07-09", 3)
      )),
      report_date = as.Date(c(
        "2020-07-11", "2020-07-12", "2020-07-13", "2020-07-14",
        "2020-07-12", "2020-07-13", "2020-07-14"
      )),
      n = c(10, 15, 18, 20,  # Revisions for event 2020-07-08
            5, 8, 10)        # Revisions for event 2020-07-09
    ),

    # Count data with strata
    count_with_strata = data.frame(
      event_date = as.Date(c(
        rep("2020-07-08", 6),
        rep("2020-07-09", 6)
      )),
      report_date = as.Date(c(
        "2020-07-11", "2020-07-12", "2020-07-13",
        "2020-07-11", "2020-07-12", "2020-07-13",
        "2020-07-12", "2020-07-13", "2020-07-14",
        "2020-07-12", "2020-07-13", "2020-07-14"
      )),
      gender = c(
        "Male", "Male", "Male",
        "Female", "Female", "Female",
        "Male", "Male", "Male",
        "Female", "Female", "Female"
      ),
      n = c(10, 12, 15,  # Male revisions for 2020-07-08
            8, 10, 12,   # Female revisions for 2020-07-08
            5, 7, 9,     # Male revisions for 2020-07-09
            4, 6, 8)     # Female revisions for 2020-07-09
    ),

    # Count data with covariates
    count_with_covariates = data.frame(
      event_date = as.Date(c(
        rep("2020-07-08", 4),
        rep("2020-07-09", 4)
      )),
      report_date = as.Date(c(
        "2020-07-11", "2020-07-12", "2020-07-13", "2020-07-14",
        "2020-07-11", "2020-07-12", "2020-07-13", "2020-07-14"
      )),
      temperature = c(25.5, 25.5, 25.5, 25.5,
                      26.0, 26.0, 26.0, 26.0),
      n = c(10, 12, 14, 16,
            5, 7, 9, 11)
    ),

    # Count data with is_censored
    count_with_censored = data.frame(
      event_date = as.Date(c(
        rep("2020-07-08", 6)
      )),
      report_date = as.Date(c(
        "2020-07-11", "2020-07-12", "2020-07-13",
        "2020-07-11", "2020-07-12", "2020-07-13"
      )),
      is_censored = c(FALSE, FALSE, FALSE,
                      TRUE, TRUE, TRUE),
      n = c(10, 12, 15,  # Non-censored
            8, 10, 12)   # Censored
    ),

    # Single event date with multiple reports
    single_event = data.frame(
      event_date = as.Date(rep("2020-07-08", 5)),
      report_date = as.Date(c(
        "2020-07-09", "2020-07-10", "2020-07-11",
        "2020-07-12", "2020-07-13"
      )),
      n = c(5, 8, 12, 15, 18)
    ),

    # Data with temporal effects
    count_with_temporal = data.frame(
      event_date = as.Date(c(
        rep("2020-07-08", 3),
        rep("2020-07-15", 3)
      )),
      report_date = as.Date(c(
        "2020-07-09", "2020-07-10", "2020-07-11",
        "2020-07-16", "2020-07-17", "2020-07-18"
      )),
      n = c(10, 15, 20,
            8, 12, 16)
    )
  )
}

# ============================================================================
# Tests for get_initial_reported_cases()
# ============================================================================

test_that("get_initial_reported_cases fails on non-tbl_now object", {
  regular_df <- data.frame(
    event_date = as.Date("2020-07-08"),
    report_date = as.Date("2020-07-11"),
    n = 10
  )

  expect_error(
    get_initial_reported_cases(regular_df),
    "not a.*tbl_now"
  )
})

test_that("get_initial_reported_cases returns earliest report for each event", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$count_revisions,
    event_date = "event_date",
    report_date = "report_date",
    case_count = n,
    data_type = "count-cumulative",
    verbose = FALSE
  )

  result <- get_initial_reported_cases(ndata)

  # Should have one row per event_date
  expect_equal(nrow(result), 2)

  # Should have the earliest report_date for each event
  event_2020_07_08 <- result %>%
    dplyr::filter(event_date == as.Date("2020-07-08"))
  expect_equal(event_2020_07_08$report_date, as.Date("2020-07-11"))
  expect_equal(event_2020_07_08$n, 10)

  event_2020_07_09 <- result %>%
    dplyr::filter(event_date == as.Date("2020-07-09"))
  expect_equal(event_2020_07_09$report_date, as.Date("2020-07-12"))
  expect_equal(event_2020_07_09$n, 5)
})

test_that("get_initial_reported_cases converts to count-cumulative", {
  test_data <- setup_test_data()

  # Start with linelist data
  ndata <- tbl_now(
    test_data$linelist_simple,
    event_date = "event_date",
    report_date = "report_date",
    data_type = "linelist",
    verbose = FALSE
  )

  result <- get_initial_reported_cases(ndata)

  expect_s3_class(result, "tbl_now")
  expect_equal(get_data_type(result), "count-cumulative")
})

test_that("get_initial_reported_cases preserves strata", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$count_with_strata,
    event_date = "event_date",
    report_date = "report_date",
    strata = "gender",
    case_count = n,
    data_type = "count-cumulative",
    verbose = FALSE
  )

  result <- get_initial_reported_cases(ndata)

  # Should have separate rows for each event_date x gender combination
  expect_equal(nrow(result), 4)  # 2 events x 2 genders

  # Check strata is preserved
  expect_equal(get_strata(result), "gender")
  expect_true("gender" %in% names(result))

  # Check Male initial report for 2020-07-08
  male_2020_07_08 <- result %>%
    dplyr::filter(event_date == as.Date("2020-07-08"), gender == "Male")
  expect_equal(male_2020_07_08$n, 10)
  expect_equal(male_2020_07_08$report_date, as.Date("2020-07-11"))

  # Check Female initial report for 2020-07-08
  female_2020_07_08 <- result %>%
    dplyr::filter(event_date == as.Date("2020-07-08"), gender == "Female")
  expect_equal(female_2020_07_08$n, 8)
  expect_equal(female_2020_07_08$report_date, as.Date("2020-07-11"))
})

test_that("get_initial_reported_cases preserves covariates", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$count_with_covariates,
    event_date = "event_date",
    report_date = "report_date",
    covariates = "temperature",
    case_count = n,
    data_type = "count-cumulative",
    verbose = FALSE
  )

  result <- get_initial_reported_cases(ndata)

  expect_equal(get_covariates(result), "temperature")
  expect_true("temperature" %in% names(result))
})

test_that("get_initial_reported_cases preserves is_censored", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$count_with_censored,
    event_date = "event_date",
    report_date = "report_date",
    is_censored = "is_censored",
    report_units = "days",
    event_units = "days",
    case_count = n,
    data_type = "count-cumulative",
    verbose = FALSE
  )

  result <- get_initial_reported_cases(ndata)

  # Should have separate rows for censored and non-censored
  expect_equal(nrow(result), 2)
  expect_true("is_censored" %in% names(result))

  # Check non-censored initial report
  non_censored <- result %>% dplyr::filter(!is_censored)
  expect_equal(non_censored$n, 10)
  expect_equal(non_censored$report_date, as.Date("2020-07-11"))

  # Check censored initial report
  censored <- result %>% dplyr::filter(is_censored)
  expect_equal(censored$n, 8)
  expect_equal(censored$report_date, as.Date("2020-07-11"))
})

test_that("get_initial_reported_cases preserves temporal effects", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$count_with_temporal,
    event_date = "event_date",
    report_units = "days",
    event_units = "days",
    report_date = "report_date",
    case_count = n,
    data_type = "count-cumulative",
    verbose = FALSE
  ) %>%
    add_temporal_effects(temporal_effects(week_of_year = TRUE))

  result <- get_initial_reported_cases(ndata)

  temporal_cols <- get_temporal_effects(result)
  expect_true(length(temporal_cols) > 0)
  expect_true(all(temporal_cols %in% names(result)))
})

test_that("get_initial_reported_cases is ungrouped", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$count_with_strata,
    event_date = "event_date",
    report_date = "report_date",
    strata = "gender",
    case_count = n,
    data_type = "count-cumulative",
    verbose = FALSE
  )

  result <- get_initial_reported_cases(ndata)

  expect_false(dplyr::is_grouped_df(result))
})

test_that("get_initial_reported_cases is sorted correctly", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$count_with_strata,
    event_date = "event_date",
    report_date = "report_date",
    strata = "gender",
    case_count = n,
    data_type = "count-cumulative",
    verbose = FALSE
  )

  result <- get_initial_reported_cases(ndata)

  # Should be sorted by event_date, strata, is_censored, covariates
  # Check that event_dates are in order
  expect_true(all(diff(as.numeric(result$event_date)) >= 0))
})

test_that("get_initial_reported_cases handles single event date", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$single_event,
    event_date = "event_date",
    report_date = "report_date",
    case_count = n,
    report_units = "days",
    event_units = "days",
    data_type = "count-cumulative",
    verbose = FALSE
  )

  result <- get_initial_reported_cases(ndata)

  expect_equal(nrow(result), 1)
  expect_equal(result$n, 5)  # Earliest report
  expect_equal(result$report_date, as.Date("2020-07-09"))
})

# ============================================================================
# Tests for get_latest_reported_cases()
# ============================================================================

test_that("get_latest_reported_cases fails on non-tbl_now object", {
  regular_df <- data.frame(
    event_date = as.Date("2020-07-08"),
    report_date = as.Date("2020-07-11"),
    n = 10
  )

  expect_error(
    get_latest_reported_cases(regular_df),
    "not a.*tbl_now"
  )
})

test_that("get_latest_reported_cases returns latest report for each event", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$count_revisions,
    event_date = "event_date",
    report_date = "report_date",
    case_count = n,
    data_type = "count-cumulative",
    verbose = FALSE
  )

  result <- get_latest_reported_cases(ndata)

  # Should have one row per event_date
  expect_equal(nrow(result), 2)

  # Should have the latest report_date for each event
  event_2020_07_08 <- result %>%
    dplyr::filter(event_date == as.Date("2020-07-08"))
  expect_equal(event_2020_07_08$report_date, as.Date("2020-07-14"))
  expect_equal(event_2020_07_08$n, 20)

  event_2020_07_09 <- result %>%
    dplyr::filter(event_date == as.Date("2020-07-09"))
  expect_equal(event_2020_07_09$report_date, as.Date("2020-07-14"))
  expect_equal(event_2020_07_09$n, 10)
})

test_that("get_latest_reported_cases converts to count-cumulative", {
  test_data <- setup_test_data()

  # Start with linelist data
  ndata <- tbl_now(
    test_data$linelist_simple,
    event_date = "event_date",
    report_date = "report_date",
    data_type = "linelist",
    verbose = FALSE
  )

  result <- get_latest_reported_cases(ndata)

  expect_s3_class(result, "tbl_now")
  expect_equal(get_data_type(result), "count-cumulative")
})

test_that("get_latest_reported_cases preserves strata", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$count_with_strata,
    event_date = "event_date",
    report_date = "report_date",
    strata = "gender",
    case_count = n,
    data_type = "count-cumulative",
    verbose = FALSE
  )

  result <- get_latest_reported_cases(ndata)

  # Should have separate rows for each event_date x gender combination
  expect_equal(nrow(result), 4)  # 2 events x 2 genders

  # Check strata is preserved
  expect_equal(get_strata(result), "gender")
  expect_true("gender" %in% names(result))

  # Check Male latest report for 2020-07-08
  male_2020_07_08 <- result %>%
    dplyr::filter(event_date == as.Date("2020-07-08"), gender == "Male")
  expect_equal(male_2020_07_08$n, 15)
  expect_equal(male_2020_07_08$report_date, as.Date("2020-07-13"))

  # Check Female latest report for 2020-07-08
  female_2020_07_08 <- result %>%
    dplyr::filter(event_date == as.Date("2020-07-08"), gender == "Female")
  expect_equal(female_2020_07_08$n, 12)
  expect_equal(female_2020_07_08$report_date, as.Date("2020-07-13"))
})

test_that("get_latest_reported_cases preserves covariates", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$count_with_covariates,
    event_date = "event_date",
    report_date = "report_date",
    covariates = "temperature",
    case_count = n,
    data_type = "count-cumulative",
    verbose = FALSE
  )

  result <- get_latest_reported_cases(ndata)

  expect_equal(get_covariates(result), "temperature")
  expect_true("temperature" %in% names(result))
})

test_that("get_latest_reported_cases preserves is_censored", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$count_with_censored,
    event_date = "event_date",
    report_date = "report_date",
    is_censored = "is_censored",
    case_count = n,
    report_units = "days",
    event_units = "days",
    data_type = "count-cumulative",
    verbose = FALSE
  )

  result <- get_latest_reported_cases(ndata)

  # Should have separate rows for censored and non-censored
  expect_equal(nrow(result), 2)
  expect_true("is_censored" %in% names(result))

  # Check non-censored latest report
  non_censored <- result %>% dplyr::filter(!is_censored)
  expect_equal(non_censored$n, 15)
  expect_equal(non_censored$report_date, as.Date("2020-07-13"))

  # Check censored latest report
  censored <- result %>% dplyr::filter(is_censored)
  expect_equal(censored$n, 12)
  expect_equal(censored$report_date, as.Date("2020-07-13"))
})

test_that("get_latest_reported_cases preserves temporal effects", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$count_with_temporal,
    event_date = "event_date",
    report_date = "report_date",
    report_units = "days",
    event_units = "days",
    case_count = n,
    data_type = "count-cumulative",
    verbose = FALSE
  ) %>%
    add_temporal_effects(temporal_effects(week_of_year = TRUE))

  result <- get_latest_reported_cases(ndata)

  temporal_cols <- get_temporal_effects(result)
  expect_true(length(temporal_cols) > 0)
  expect_true(all(temporal_cols %in% names(result)))
})

test_that("get_latest_reported_cases is ungrouped", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$count_with_strata,
    event_date = "event_date",
    report_date = "report_date",
    strata = "gender",
    case_count = n,
    data_type = "count-cumulative",
    verbose = FALSE
  )

  result <- get_latest_reported_cases(ndata)

  expect_false(dplyr::is_grouped_df(result))
})

test_that("get_latest_reported_cases is sorted correctly", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$count_with_strata,
    event_date = "event_date",
    report_date = "report_date",
    strata = "gender",
    case_count = n,
    data_type = "count-cumulative",
    verbose = FALSE
  )

  result <- get_latest_reported_cases(ndata)

  # Should be sorted by event_date, strata, is_censored, covariates
  # Check that event_dates are in order
  expect_true(all(diff(as.numeric(result$event_date)) >= 0))
})

test_that("get_latest_reported_cases handles single event date", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$single_event,
    report_units = "days",
    event_units = "days",
    event_date = "event_date",
    report_date = "report_date",
    case_count = n,
    data_type = "count-cumulative",
    verbose = FALSE
  )

  result <- get_latest_reported_cases(ndata)

  expect_equal(nrow(result), 1)
  expect_equal(result$n, 18)  # Latest report
  expect_equal(result$report_date, as.Date("2020-07-13"))
})

# ============================================================================
# Comparison tests (initial vs latest)
# ============================================================================

test_that("initial and latest reports differ appropriately", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$count_revisions,
    event_date = "event_date",
    report_date = "report_date",
    case_count = n,
    data_type = "count-cumulative",
    verbose = FALSE
  )

  initial <- get_initial_reported_cases(ndata)
  latest <- get_latest_reported_cases(ndata)

  # Same number of rows
  expect_equal(nrow(initial), nrow(latest))

  # Initial counts should be <= latest counts
  expect_true(all(initial$n <= latest$n))

  # Initial report dates should be <= latest report dates
  expect_true(all(initial$report_date <= latest$report_date))
})

test_that("initial equals latest when no revisions exist", {
  # Create data with no revisions (one report per event)
  no_revisions <- data.frame(
    event_date = as.Date(c("2020-07-08", "2020-07-09", "2020-07-10")),
    report_date = as.Date(c("2020-07-11", "2020-07-12", "2020-07-13")),
    n = c(10, 15, 20)
  )

  ndata <- tbl_now(
    no_revisions,
    event_date = "event_date",
    report_date = "report_date",
    case_count = n,
    data_type = "count-cumulative",
    verbose = FALSE
  )

  initial <- get_initial_reported_cases(ndata)
  latest <- get_latest_reported_cases(ndata)

  # Should be identical
  expect_equal(initial$n, latest$n)
  expect_equal(initial$report_date, latest$report_date)
})

# ============================================================================
# Integration tests with real data
# ============================================================================

test_that("report functions work with denguedat", {
  data(denguedat)

  dengue_subset <- denguedat[1:500, ] %>%
    tbl_now(
      event_date = "onset_week",
      report_units = "weeks",
      event_units = "weeks",
      report_date = "report_week",
      strata = "gender",
      verbose = FALSE
    )

  initial <- get_initial_reported_cases(dengue_subset)
  latest <- get_latest_reported_cases(dengue_subset)

  expect_s3_class(initial, "tbl_now")
  expect_s3_class(latest, "tbl_now")

  # Should have data
  expect_gt(nrow(initial), 0)
  expect_gt(nrow(latest), 0)

  # Initial should be <= latest
  suppressWarnings(
    combined <- initial %>%
      dplyr::rename(n_initial = n, report_initial = report_week) %>%
      dplyr::select(event_date = onset_week, gender, n_initial, report_initial) %>%
      dplyr::inner_join(
        latest %>%
          dplyr::rename(n_latest = n, report_latest = report_week) %>%
          dplyr::select(event_date = onset_week, gender, n_latest, report_latest),
        by = c("event_date", "gender")
      )
  )

  expect_true(all(combined$n_initial <= combined$n_latest))
  expect_true(all(combined$report_initial <= combined$report_latest))
})

test_that("report functions work with count-incidence data", {
  test_data <- setup_test_data()

  # Create count-incidence data
  incidence_data <- data.frame(
    event_date = as.Date(c(
      "2020-07-08", "2020-07-08", "2020-07-08",
      "2020-07-09", "2020-07-09"
    )),
    report_date = as.Date(c(
      "2020-07-11", "2020-07-12", "2020-07-13",
      "2020-07-12", "2020-07-13"
    )),
    n = c(10, 5, 3,  # Increments for event 2020-07-08
          5, 3)      # Increments for event 2020-07-09
  )

  ndata <- tbl_now(
    incidence_data,
    event_date = "event_date",
    report_date = "report_date",
    case_count = n,
    data_type = "count-incidence",
    verbose = FALSE
  )

  # Both functions should convert to cumulative
  initial <- get_initial_reported_cases(ndata)
  latest <- get_latest_reported_cases(ndata)

  expect_equal(get_data_type(initial), "count-cumulative")
  expect_equal(get_data_type(latest), "count-cumulative")

  # Latest should be cumulative sum
  event_08 <- latest %>% dplyr::filter(event_date == as.Date("2020-07-08"))
  expect_equal(event_08$n, 18)  # 10 + 5 + 3 = 18
})

test_that("example from vignette works correctly", {
  data(denguedat)

  df_pr <- denguedat %>%
    tbl_now(
      event_date = "onset_week",
      report_date = "report_week",
      strata = "gender",
      verbose = FALSE
    ) %>%
    dplyr::filter(onset_week >= as.Date("1990-01-01"),
                  onset_week <= as.Date("1990-12-31"))

  initial_reports <- get_initial_reported_cases(df_pr)
  latest_reports <- get_latest_reported_cases(df_pr)

  expect_s3_class(initial_reports, "tbl_now")
  expect_s3_class(latest_reports, "tbl_now")

  # Initial should have earlier or same report dates
  expect_true(all(initial_reports$report_week <= latest_reports$report_week))
})

# ============================================================================
# Edge cases
# ============================================================================

test_that("report functions handle empty tbl_now", {

  data(denguedat)

  ndata <- tbl_now(
    denguedat,
    event_date = "onset_week",
    report_date = "report_week",
    strata = "gender",
    verbose = FALSE
  ) %>%
    dplyr::filter(onset_week < as.Date("1989-01-01"))


  expect_error(
    get_initial_reported_cases(ndata),
    "empty"
  )

  expect_error(
    get_latest_reported_cases(ndata),
    "empty"
  )

})

