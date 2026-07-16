# Test file for to_count.R functions

# Setup test data ----
setup_test_data <- function() {
  # Linelist data (each row is one observation)
  linelist_data <- data.frame(
    onset_week = as.Date(c(
      "2020-07-08", "2020-07-08", "2020-07-08",
      "2020-07-15", "2020-07-15",
      "2020-07-22"
    )),
    report_week = as.Date(c(
      "2020-07-11", "2020-07-11", "2020-07-12",
      "2020-07-18", "2020-07-18",
      "2020-07-25"
    )),
    gender = c("Male", "Female", "Male", "Female", "Male", "Female")
  )

  # Count data (already aggregated)
  count_data <- data.frame(
    onset_week = as.Date(c("2020-07-08", "2020-07-08", "2020-07-15")),
    report_week = as.Date(c("2020-07-11", "2020-07-12", "2020-07-18")),
    gender = c("Male", "Female", "Male"),
    n = c(5L, 3L, 7L)
  )

  # Linelist with strata
  linelist_with_strata <- data.frame(
    onset_week = as.Date(c(
      "2020-07-08", "2020-07-08", "2020-07-08", "2020-07-08",
      "2020-07-15", "2020-07-15"
    )),
    report_week = as.Date(c(
      "2020-07-11", "2020-07-11", "2020-07-12", "2020-07-12",
      "2020-07-18", "2020-07-18"
    )),
    gender = c("Male", "Female", "Male", "Female", "Male", "Female"),
    age_group = c("20-30", "20-30", "30-40", "30-40", "20-30", "30-40")
  )

  # Linelist with covariates
  linelist_with_covariates <- data.frame(
    onset_week = as.Date(c(
      "2020-07-08", "2020-07-08", "2020-07-08",
      "2020-07-15", "2020-07-15"
    )),
    report_week = as.Date(c(
      "2020-07-11", "2020-07-11", "2020-07-12",
      "2020-07-18", "2020-07-18"
    )),
    temperature = c(25.5, 25.5, 26.0, 25.5, 26.0)
  )

  # Linelist with is_censored
  linelist_with_censored <- data.frame(
    onset_week = as.Date(c(
      "2020-07-08", "2020-07-08", "2020-07-08",
      "2020-07-15", "2020-07-15"
    )),
    report_week = as.Date(c(
      "2020-07-11", "2020-07-11", "2020-07-12",
      "2020-07-18", "2020-07-18"
    )),
    is_censored = c(TRUE, FALSE, TRUE, FALSE, TRUE)
  )

  list(
    linelist_data = linelist_data,
    count_data = count_data,
    linelist_with_strata = linelist_with_strata,
    linelist_with_covariates = linelist_with_covariates,
    linelist_with_censored = linelist_with_censored
  )
}

# # Tests for to_count() generic ----
# test_that("to_count is an S3 generic", {
#    expect_true(isS3stdGeneric("to_count"))
# })

# Tests for to_count.tbl_now() with linelist data ----
test_that("to_count converts linelist data to count incidence data", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$linelist_data,
    event_date = "onset_week",
    report_date = "report_week",
    data_type = "linelist",
    report_units = "days",
    event_units = "days",
    warn_non_uniqueness = FALSE
  )


  result <- to_count(ndata, to = "count-incidence")
  expect_s3_class(result, "tbl_now")
  expect_true("n" %in% colnames(result))
  expect_equal(get_data_type(result), "count-incidence")

  result <- to_count(ndata, to = "count-cumulative")
  expect_equal(get_data_type(result), "count-cumulative")
})

test_that("to_count creates correct counts for linelist data", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$linelist_data,
    event_date = "onset_week",
    report_date = "report_week",
    data_type = "linelist",
    report_units = "days",
    event_units = "days",
    warn_non_uniqueness = FALSE
  )

  result <- to_count(ndata, to = "count-incidence")

  # Should have 4 unique combinations (onset_week, report_week)
  # 2020-07-08 & 2020-07-11: 2 observations
  # 2020-07-08 & 2020-07-12: 1 observation
  # 2020-07-15 & 2020-07-18: 2 observations
  # 2020-07-22 & 2020-07-25: 1 observation

  expect_equal(nrow(result), 4)
  expect_true(all(result$n > 0))
  expect_equal(result$n, c(2, 1, 2, 1))
  expect_equal(sum(result$n), nrow(test_data$linelist_data))

  result2 <- to_count(ndata, to = "count-cumulative")
  expect_equal(nrow(result2), 4)
  expect_true(all(result2$n > 0))
  expect_equal(result2$n, c(2, 3, 2, 1))
})

test_that("to_count groups by event_date and report_date", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$linelist_data,
    event_date = "onset_week",
    report_date = "report_week",
    data_type = "linelist",
    report_units = "days",
    event_units = "days",
    warn_non_uniqueness = FALSE
  )

  result <- to_count(ndata, to = "count-incidence")

  # Check that each combination of event_date and report_date is unique
  unique_combos <- suppressWarnings(
    result |>
      dplyr::select(onset_week, report_week) |>
      dplyr::distinct()
  )

  expect_equal(nrow(unique_combos), nrow(result))
})

test_that("to_count preserves .event_num and .report_num columns", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$linelist_data,
    event_date = "onset_week",
    report_date = "report_week",
    data_type = "linelist",
    report_units = "days",
    event_units = "days",
    warn_non_uniqueness = FALSE
  )

  result <- to_count(ndata)

  expect_true(".event_num" %in% colnames(result))
  expect_true(".report_num" %in% colnames(result))
})

# Tests for to_count.tbl_now() with strata ----
test_that("to_count groups by strata when present", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$linelist_with_strata,
    event_date = "onset_week",
    report_date = "report_week",
    strata = c("gender", "age_group"),
    data_type = "linelist",
    report_units = "days",
    event_units = "days",
    warn_non_uniqueness = FALSE
  )

  result <- to_count(ndata, to = "count-incidence")

  expect_s3_class(result, "tbl_now")
  expect_true("n" %in% colnames(result))
  expect_true("gender" %in% colnames(result))
  expect_true("age_group" %in% colnames(result))

  # Total count should equal original rows
  expect_equal(sum(result$n), nrow(test_data$linelist_with_strata))
})

test_that("to_count creates separate counts for each stratum", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$linelist_with_strata,
    event_date = "onset_week",
    report_date = "report_week",
    strata = "gender",
    data_type = "linelist",
    report_units = "days",
    event_units = "days",
    warn_non_uniqueness = FALSE
  )

  result <- to_count(ndata, to = "count-incidence")

  # Should have separate rows for Male and Female for same dates
  male_rows <- result |> dplyr::filter(gender == "Male")
  female_rows <- result |> dplyr::filter(gender == "Female")

  expect_gt(nrow(male_rows), 0)
  expect_gt(nrow(female_rows), 0)
})

test_that("to_count preserves strata attributes", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$linelist_with_strata,
    event_date = "onset_week",
    report_date = "report_week",
    strata = c("gender", "age_group"),
    data_type = "linelist",
    report_units = "days",
    event_units = "days",
    warn_non_uniqueness = FALSE
  )

  result <- to_count(ndata, to = "count-incidence")

  expect_equal(get_strata(result), c("gender", "age_group"))
})

# Tests for to_count.tbl_now() with covariates ----
test_that("to_count groups by covariates when present", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$linelist_with_covariates,
    event_date = "onset_week",
    report_date = "report_week",
    covariates = "temperature",
    data_type = "linelist",
    report_units = "days",
    event_units = "days",
    warn_non_uniqueness = FALSE
  )

  result <- to_count(ndata, to = "count-incidence")

  expect_s3_class(result, "tbl_now")
  expect_true("n" %in% colnames(result))
})

test_that("to_count preserves covariate attributes", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$linelist_with_covariates,
    event_date = "onset_week",
    report_date = "report_week",
    covariates = "temperature",
    data_type = "linelist",
    report_units = "days",
    event_units = "days",
    warn_non_uniqueness = FALSE
  )

  result <- to_count(ndata, to = "count-incidence")

  expect_equal(get_covariates(result), "temperature")
  expect_equal(get_num_covariates(result), 1)
})

# Tests for to_count.tbl_now() with is_censored ----
test_that("to_count groups by is_censored when present", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$linelist_with_censored,
    event_date = "onset_week",
    report_date = "report_week",
    is_censored = "is_censored",
    data_type = "linelist",
    report_units = "days",
    event_units = "days",
    warn_non_uniqueness = FALSE
  )

  result <- to_count(ndata, to = "count-incidence")

  expect_s3_class(result, "tbl_now")
  expect_true("n" %in% colnames(result))
  expect_true("is_censored" %in% colnames(result))
})

test_that("to_count creates separate counts for censored vs non-censored", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$linelist_with_censored,
    event_date = "onset_week",
    report_date = "report_week",
    is_censored = "is_censored",
    data_type = "linelist",
    report_units = "days",
    event_units = "days",
    warn_non_uniqueness = FALSE
  )

  result <- to_count(ndata, to = "count-incidence")

  # Should have rows with TRUE and FALSE
  censored_vals <- unique(result$is_censored)
  expect_true(any(censored_vals == TRUE))
  expect_true(any(censored_vals == FALSE))
})

# Tests for to_count.tbl_now() with count data ----
test_that("to_count handles count data by summing", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$count_data,
    event_date = "onset_week",
    report_date = "report_week",
    case_count = n,
    strata = "gender",
    data_type = "count-incidence",
    report_units = "days",
    event_units = "days",
    warn_non_uniqueness = FALSE
  )

  result <- to_count(ndata, to = "count-incidence")

  expect_s3_class(result, "tbl_now")
  expect_true("n" %in% colnames(result))
  expect_equal(get_data_type(result), "count-incidence")
})

test_that("to_count sums counts correctly for count data", {
  # Create count data with duplicates that should be summed
  count_data_dup <- data.frame(
    onset_week = as.Date(c("2020-07-08", "2020-07-08", "2020-07-15")),
    report_week = as.Date(c("2020-07-11", "2020-07-11", "2020-07-18")),
    gender = c("Male", "Male", "Female"),
    n = c(5L, 3L, 7L)
  )

  ndata <- tbl_now(
    count_data_dup,
    event_date = "onset_week",
    report_date = "report_week",
    strata = "gender",
    case_count = n,
    data_type = "count-incidence",
    report_units = "days",
    event_units = "days",
    warn_non_uniqueness = FALSE
  )

  result <- to_count(ndata, to = "count-incidence")

  # Should sum the two Male entries on 2020-07-08/2020-07-11
  male_row <- result |>
    dplyr::filter(
      onset_week == as.Date("2020-07-08"),
      report_week == as.Date("2020-07-11"),
      gender == "Male"
    )

  expect_equal(nrow(male_row), 1)
  expect_equal(male_row$n, 8)
})

test_that("to_count maintains count data_type", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$count_data,
    event_date = "onset_week",
    report_date = "report_week",
    data_type = "count-incidence",
    case_count = n,
    report_units = "days",
    event_units = "days",
    warn_non_uniqueness = FALSE
  )

  result <- to_count(ndata)

  expect_equal(get_data_type(result), "count-incidence")
})

test_that("to_count works even when column is not n", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$count_data |> rename(obs = n),
    event_date = "onset_week",
    report_date = "report_week",
    data_type = "count-incidence",
    case_count = obs,
    report_units = "days",
    event_units = "days",
    warn_non_uniqueness = FALSE
  )

  ndata2 <- tbl_now(
    test_data$count_data,
    event_date = "onset_week",
    report_date = "report_week",
    data_type = "count-incidence",
    case_count = n,
    report_units = "days",
    event_units = "days",
    warn_non_uniqueness = FALSE
  )

  result_obs <- to_count(ndata) |> rename(n = obs)
  result_n <- to_count(ndata2)

  expect_equal(result_obs, result_n)
})


# Tests for to_count() behavior ----
test_that("to_count ungroups data before processing", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$linelist_data,
    event_date = "onset_week",
    report_date = "report_week",
    data_type = "linelist",
    report_units = "days",
    event_units = "days",
    warn_non_uniqueness = FALSE
  )

  # Manually group the data
  grouped_ndata <- ndata |> dplyr::group_by(onset_week)

  result <- to_count(grouped_ndata, to = "count-incidence")

  # Result should be ungrouped
  expect_false(dplyr::is_grouped_df(result))
})

test_that("to_count returns ungrouped data", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$linelist_data,
    event_date = "onset_week",
    report_date = "report_week",
    strata = "gender",
    data_type = "linelist",
    report_units = "days",
    event_units = "days",
    warn_non_uniqueness = FALSE
  )

  result <- to_count(ndata, to = "count-incidence")

  expect_false(dplyr::is_grouped_df(result))
})

test_that("to_count preserves tbl_now attributes", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$linelist_data,
    event_date = "onset_week",
    report_date = "report_week",
    data_type = "linelist",
    report_units = "days",
    event_units = "days",
    warn_non_uniqueness = FALSE
  )

  result <- to_count(ndata, to = "count-incidence")

  expect_equal(get_event_date(result), "onset_week")
  expect_equal(get_report_date(result), "report_week")
  expect_equal(get_now(result), get_now(ndata))
  expect_equal(get_event_units(result), get_event_units(ndata))
  expect_equal(get_report_units(result), get_report_units(ndata))
})

# Tests for edge cases ----
test_that("to_count handles single row data", {
  single_row <- data.frame(
    onset_week = as.Date("2020-07-08"),
    report_week = as.Date("2020-07-11")
  )

  ndata <- tbl_now(
    single_row,
    event_date = "onset_week",
    report_date = "report_week",
    data_type = "linelist",
    report_units = "days",
    event_units = "days",
    warn_non_uniqueness = FALSE
  )

  result <- to_count(ndata, to = "count-incidence")

  expect_equal(nrow(result), 1)
  expect_equal(result$n, 1)
})

test_that("to_count handles all identical rows", {
  identical_rows <- data.frame(
    onset_week = as.Date(rep("2020-07-08", 5)),
    report_week = as.Date(rep("2020-07-11", 5))
  )

  ndata <- tbl_now(
    identical_rows,
    event_date = "onset_week",
    report_date = "report_week",
    data_type = "linelist",
    report_units = "days",
    event_units = "days",
    warn_non_uniqueness = FALSE
  )

  result <- to_count(ndata, to = "count-incidence")

  expect_equal(nrow(result), 1)
  expect_equal(result$n, 5)
})

test_that("to_count handles data with no strata or covariates", {
  simple_data <- data.frame(
    onset_week = as.Date(c("2020-07-08", "2020-07-08", "2020-07-15")),
    report_week = as.Date(c("2020-07-11", "2020-07-11", "2020-07-18"))
  )

  ndata <- tbl_now(
    simple_data,
    event_date = "onset_week",
    report_date = "report_week",
    data_type = "linelist",
    report_units = "days",
    event_units = "days",
    warn_non_uniqueness = FALSE
  )

  result <- to_count(ndata, to = "count-incidence")

  expect_equal(nrow(result), 2)
  expect_equal(sum(result$n), 3)
})

# Tests for data validation ----
test_that("to_count maintains data integrity", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$linelist_data,
    event_date = "onset_week",
    report_date = "report_week",
    data_type = "linelist",
    report_units = "days",
    event_units = "days",
    warn_non_uniqueness = FALSE
  )

  original_count <- nrow(ndata)
  result <- to_count(ndata, to = "count-incidence")

  # Sum of n should equal original number of rows
  expect_equal(sum(result$n), original_count)
})

test_that("to_count produces positive counts", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$linelist_data,
    event_date = "onset_week",
    report_date = "report_week",
    data_type = "linelist",
    report_units = "days",
    event_units = "days",
    warn_non_uniqueness = FALSE
  )

  result <- to_count(ndata, to = "count-incidence")

  expect_true(all(result$n > 0))
})

test_that("to_count produces integer counts", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$linelist_data,
    event_date = "onset_week",
    report_date = "report_week",
    data_type = "linelist",
    report_units = "days",
    event_units = "days",
    warn_non_uniqueness = FALSE
  )

  result <- to_count(ndata, to = "count-incidence")

  expect_true(is.integer(result$n) || all(result$n == floor(result$n)))
})

# Tests for complex scenarios ----
test_that("to_count handles multiple strata and covariates together", {
  complex_data <- data.frame(
    onset_week = as.Date(c(
      "2020-07-08", "2020-07-08", "2020-07-08", "2020-07-08",
      "2020-07-15", "2020-07-15"
    )),
    report_week = as.Date(c(
      "2020-07-11", "2020-07-11", "2020-07-11", "2020-07-11",
      "2020-07-18", "2020-07-18"
    )),
    gender = c("Male", "Female", "Male", "Female", "Male", "Female"),
    age_group = c("20-30", "20-30", "30-40", "30-40", "20-30", "30-40"),
    temperature = c(25.5, 25.5, 26.0, 26.0, 25.5, 26.0)
  )

  ndata <- tbl_now(
    complex_data,
    event_date = "onset_week",
    report_date = "report_week",
    strata = c("gender", "age_group"),
    covariates = "temperature",
    data_type = "linelist",
    report_units = "days",
    event_units = "days",
    warn_non_uniqueness = FALSE
  )

  result <- to_count(ndata, to = "count-incidence")

  expect_s3_class(result, "tbl_now")
  expect_true("n" %in% colnames(result))
  expect_equal(sum(result$n), nrow(complex_data))
})

test_that("to_count handles all combinations correctly", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$linelist_with_strata,
    event_date = "onset_week",
    report_date = "report_week",
    strata = c("gender", "age_group"),
    data_type = "linelist",
    report_units = "days",
    event_units = "days",
    warn_non_uniqueness = FALSE
  )

  result <- to_count(ndata, to = "count-incidence")

  # Each unique combination should appear once
  unique_combos <- suppressWarnings(
    result |>
      dplyr::select(onset_week, report_week, gender, age_group) |>
      dplyr::distinct()
  )

  expect_equal(nrow(unique_combos), nrow(result))
})

# Tests for error handling ----
test_that("to_count fails gracefully with invalid data_type", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$linelist_data,
    event_date = "onset_week",
    report_date = "report_week",
    data_type = "linelist",
    report_units = "days",
    event_units = "days",
    warn_non_uniqueness = FALSE
  )

  # Manually corrupt the data_type
  attr(ndata, "data_type") <- "invalid"

  expect_error(
    to_count(ndata),
    "not implemented"
  )
})

test_that("to_count validates output", {
  test_data <- setup_test_data()

  ndata <- tbl_now(
    test_data$linelist_data,
    event_date = "onset_week",
    report_date = "report_week",
    data_type = "linelist",
    report_units = "days",
    event_units = "days",
    warn_non_uniqueness = FALSE
  )

  result <- to_count(ndata)

  # Result should be a valid tbl_now
  expect_true(validate_tbl_now(result))
})

test_that("count-cumulative -> count-incidence de-accumulates (round-trips)", {
  data(denguedat)
  dengue <- tbl_now(denguedat[1:3000, ],
    event_date = "onset_week", report_date = "report_week", strata = "gender",
    data_type = "linelist", verbose = FALSE
  )
  incidence <- to_count(dengue, to = "count-incidence")
  cumulative <- to_count(incidence, to = "count-cumulative")
  back <- to_count(cumulative, to = "count-incidence")

  expect_equal(get_data_type(back), "count-incidence")

  canon <- function(x) {
    x <- as.data.frame(x)[, c("onset_week", "report_week", "gender", "n")]
    x <- x[do.call(order, x), ]
    rownames(x) <- NULL
    x
  }
  # incidence -> cumulative -> incidence recovers the original increments
  expect_equal(canon(incidence), canon(back))
})

test_that("de-accumulation yields negative increments for downward revisions", {
  # cumulative total revised DOWN from 10 to 7 -> increment of -3
  revised <- data.frame(
    event_date = as.Date(rep("2020-07-08", 3)),
    report_date = as.Date(c("2020-07-09", "2020-07-10", "2020-07-11")),
    n = c(5, 10, 7)
  )
  ndata <- tbl_now(revised,
    event_date = "event_date", report_date = "report_date", case_count = n,
    report_units = "days", event_units = "days",
    data_type = "count-cumulative", verbose = FALSE
  )
  incidence <- to_count(ndata, to = "count-incidence")
  # cumulative 5, 10, 7 -> increments 5, 10-5 = 5, 7-10 = -3
  expect_equal(sort(incidence$n), c(-3, 5, 5))
})

test_that("autoplot works on count-cumulative data (issue #26)", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  data(flusight)
  california <- flusight |>
    dplyr::filter(
      location_name == "California" & target_end_date >= as.Date("2023-10-01")
    ) |>
    dplyr::filter(
      target_end_date <= as.Date("2024-01-27") & as_of <= as.Date("2024-01-27")
    ) |>
    dplyr::distinct()
  flu_tbl <- tbl_now(california,
    event_date = target_end_date, report_date = as_of,
    case_count = observation, data_type = "count-cumulative", verbose = FALSE
  )

  expect_s3_class(ggplot2::autoplot(flu_tbl), "patchwork")
})
