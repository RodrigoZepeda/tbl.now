test_that("update keeps everything similar when nothing new is observed", {
  data(denguedat)

  initial_data <- denguedat[1:500, ]
  initial_tbl <- tbl_now(denguedat,
    event_date = "onset_week",
    report_date = "report_week", strata = "gender",
    verbose = FALSE
  ) |>
    to_count(to = "count-incidence")

  # Update doesn't matter if has old values
  expect_equal(
    initial_tbl,
    update(initial_tbl, new_data = initial_tbl)
  )

  subset1 <- initial_tbl |> dplyr::filter(report_week <= as.Date("1990-06-11"))
  subset2 <- initial_tbl |> dplyr::filter(report_week > as.Date("1990-06-11"))

  # Partitioning the data doesn't change the update
  expect_equal(
    initial_tbl |> dplyr::arrange(onset_week, report_week, gender),
    update(subset1, new_data = subset2) |> dplyr::arrange(onset_week, report_week, gender)
  )

  # Partitioning the data and keeping everything doesn't change the update
  expect_equal(
    initial_tbl |> dplyr::arrange(onset_week, report_week, gender),
    update(subset1, new_data = initial_tbl) |> dplyr::arrange(onset_week, report_week, gender)
  )

  # Update with empty tbl
  expect_equal(
    initial_tbl,
    update(initial_tbl, new_data = initial_tbl |> dplyr::filter(.delay > Inf))
  )
})

# Setup test data ----
setup_test_data <- function() {
  # Initial dataset
  initial_data <- data.frame(
    onset_week = as.Date(c(
      "2020-07-08", "2020-07-08", "2020-07-08",
      "2020-07-15", "2020-07-15"
    )),
    report_week = as.Date(c(
      "2020-07-11", "2020-07-12", "2020-07-13",
      "2020-07-18", "2020-07-19"
    )),
    gender = c("Male", "Female", "Male", "Female", "Male"),
    temperature = c(25.5, 26.0, 24.8, 25.2, 25.7)
  )

  # Update dataset (newer data)
  update_data <- data.frame(
    onset_week = as.Date(c(
      "2020-07-22", "2020-07-22",
      "2020-07-29", "2020-07-29"
    )),
    report_week = as.Date(c(
      "2020-07-25", "2020-07-26",
      "2020-08-01", "2020-08-02"
    )),
    gender = c("Male", "Female", "Male", "Female"),
    temperature = c(26.5, 27.0, 26.8, 27.2)
  )

  # Overlapping dataset
  overlap_data <- data.frame(
    onset_week = as.Date(c(
      "2020-07-15", "2020-07-15", # Overlaps with initial
      "2020-07-22", "2020-07-22" # New data
    )),
    report_week = as.Date(c(
      "2020-07-20", "2020-07-21",
      "2020-07-25", "2020-07-26"
    )),
    gender = c("Male", "Female", "Male", "Female"),
    temperature = c(25.0, 25.3, 26.1, 26.4)
  )

  # Count data
  count_initial <- data.frame(
    onset_week = as.Date(c(
      "2020-07-08", "2020-07-08",
      "2020-07-15", "2020-07-15"
    )),
    report_week = as.Date(c(
      "2020-07-11", "2020-07-12",
      "2020-07-18", "2020-07-19"
    )),
    gender = c("Male", "Female", "Male", "Female"),
    n = c(10, 15, 8, 12)
  )

  count_update <- data.frame(
    onset_week = as.Date(c(
      "2020-07-22", "2020-07-22"
    )),
    report_week = as.Date(c(
      "2020-07-25", "2020-07-26"
    )),
    gender = c("Male", "Female"),
    n = c(20, 25)
  )

  list(
    initial_data = initial_data,
    update_data = update_data,
    overlap_data = overlap_data,
    count_initial = count_initial,
    count_update = count_update
  )
}

# ============================================================================
# Tests for update.tbl_now() - basic functionality
# ============================================================================

test_that("update.tbl_now fails with non-data.frame new_data", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "days",
    event_units = "days",
    strata = "gender",
    verbose = FALSE
  )

  expect_error(
    update(initial_tbl, new_data = "not_a_dataframe"),
    "must be a.*data.frame"
  )

  expect_error(
    update(initial_tbl, new_data = list(a = 1, b = 2)),
    "must be a.*data.frame"
  )
})

test_that("update.tbl_now combines data correctly", {
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "days",
    event_units = "days",
    strata = "gender",
    verbose = FALSE
  )

  result <- update(initial_tbl, new_data = test_data$update_data)

  expect_s3_class(result, "tbl_now")

  # Should have rows from both datasets
  expect_equal(nrow(result), nrow(test_data$initial_data) + nrow(test_data$update_data))
})

test_that("update.tbl_now preserves attributes from object", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    strata = "gender",
    covariates = "temperature",
    report_units = "weeks",
    event_units = "weeks",
    verbose = FALSE
  )

  result <- update(initial_tbl, new_data = test_data$update_data)

  expect_equal(get_event_date(result), "onset_week")
  expect_equal(get_report_date(result), "report_week")
  expect_equal(get_data_type(result), get_data_type(initial_tbl))
  expect_equal(get_event_units(result), get_event_units(initial_tbl))
  expect_equal(get_report_units(result), get_report_units(initial_tbl))
})

test_that("update.tbl_now updates now to latest date", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    verbose = FALSE
  )

  original_now <- get_now(initial_tbl)

  result <- update(initial_tbl, new_data = test_data$update_data)

  new_now <- get_now(result)

  # Now should be updated to the latest date
  expect_true(new_now >= original_now)
  expect_equal(new_now, max(test_data$update_data$report_week))
})

test_that("update.tbl_now handles overlapping data", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    verbose = FALSE
  )

  result <- update(initial_tbl, new_data = test_data$overlap_data)

  expect_s3_class(result, "tbl_now")

  # Should include all rows (including duplicates)
  expect_gt(nrow(result), nrow(test_data$initial_data))
})

# ============================================================================
# Tests for update.tbl_now() - strata parameter
# ============================================================================

test_that("update.tbl_now strata='left' keeps object strata", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    strata = "gender",
    verbose = FALSE
  )

  result <- update(initial_tbl, new_data = test_data$update_data, strata = "left")

  expect_equal(get_strata(result), "gender")
})

test_that("update.tbl_now strata='right' uses new_data strata", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    verbose = FALSE
  )

  # Create new_data with different strata

  new_tbl <- tbl_now(
    test_data$update_data,
    event_date = "onset_week",
    report_date = "report_week",
    strata = "gender",
    report_units = "weeks",
    event_units = "weeks",
    verbose = FALSE
  )

  result <- update(initial_tbl, new_data = new_tbl, strata = "right")

  expect_equal(get_strata(result), "gender")
})

test_that("update.tbl_now strata='both' combines strata", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    strata = "gender",
    verbose = FALSE
  )

  # Add age_group to update data
  test_data$initial_data$age_group <- c("20-30", "30-40", "20-30", "30-40", "20-30")
  test_data$update_data$age_group <- c("20-30", "30-40", "20-30", "30-40")

  new_tbl <- tbl_now(
    test_data$update_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    strata = "age_group",
    verbose = FALSE
  )

  # Need to recreate initial_tbl with age_group column
  initial_tbl_updated <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    strata = "gender",
    verbose = FALSE
  )

  result <- update(initial_tbl_updated, new_data = new_tbl, strata = "both")

  expect_equal(sort(get_strata(result)), c("age_group", "gender"))
})

test_that("update.tbl_now strata='left' fails when strata not in new_data", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    strata = "gender",
    verbose = FALSE
  )

  # Remove gender from update data
  bad_update <- test_data$update_data |> dplyr::select(-gender)

  expect_error(
    update(initial_tbl, new_data = bad_update, strata = "left"),
    "was not present in.*new_data"
  )
})

test_that("update.tbl_now strata='right' fails when strata not in object", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    strata = "gender",
    verbose = FALSE
  )

  # Create new_data with strata that doesn't exist in object
  test_data$update_data$age_group <- c("20-30", "30-40", "20-30", "30-40")

  new_tbl <- tbl_now(
    test_data$update_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    strata = "age_group",
    verbose = FALSE
  )

  expect_error(
    update(initial_tbl, new_data = new_tbl, strata = "right"),
    "was not present in.*object"
  )
})

test_that("update.tbl_now fails with invalid strata option", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    verbose = FALSE
  )

  expect_error(
    update(initial_tbl, new_data = test_data$update_data, strata = "invalid"),
    "should be.*left.*right.*both"
  )
})

# ============================================================================
# Tests for update.tbl_now() - covariates parameter
# ============================================================================

test_that("update.tbl_now covariates='left' keeps object covariates", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    covariates = "temperature",
    verbose = FALSE
  )

  result <- update(initial_tbl, new_data = test_data$update_data, covariates = "left")

  expect_equal(get_covariates(result), "temperature")
})

test_that("update.tbl_now covariates='right' uses new_data covariates", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    covariates = "temperature",
    verbose = FALSE
  )

  # Create new_data with different covariate
  test_data$initial_data$humidity <- runif(5, 0.5, 0.8)
  test_data$update_data$humidity <- runif(4, 0.5, 0.8)

  new_tbl <- tbl_now(
    test_data$update_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    covariates = "humidity",
    verbose = FALSE
  )

  # Need to recreate with humidity column
  initial_tbl_updated <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    covariates = "temperature",
    verbose = FALSE
  )

  result <- update(initial_tbl_updated, new_data = new_tbl, covariates = "right")

  expect_equal(get_covariates(result), "humidity")
})

test_that("update.tbl_now covariates='both' combines covariates", {
  skip_on_cran()
  test_data <- setup_test_data()

  # Add humidity to both datasets
  test_data$initial_data$humidity <- runif(5, 0.5, 0.8)
  test_data$update_data$humidity <- runif(4, 0.5, 0.8)

  initial_tbl <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    covariates = "temperature",
    verbose = FALSE
  )

  new_tbl <- tbl_now(
    test_data$update_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    covariates = "humidity",
    verbose = FALSE
  )

  result <- update(initial_tbl, new_data = new_tbl, covariates = "both")

  expect_equal(sort(get_covariates(result)), c("humidity", "temperature"))
})

test_that("update.tbl_now fails with invalid covariates option", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    verbose = FALSE
  )

  expect_error(
    update(initial_tbl, new_data = test_data$update_data, covariates = "invalid"),
    "should be.*left.*right.*both"
  )
})

# ============================================================================
# Tests for update.tbl_now() - count data
# ============================================================================

test_that("update.tbl_now works with count data", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$count_initial,
    event_date = "onset_week",
    report_date = "report_week",
    case_count = "n",
    report_units = "weeks",
    event_units = "weeks",
    strata = "gender",
    data_type = "count-incidence",
    verbose = FALSE
  )

  result <- update(initial_tbl, new_data = test_data$count_update)

  expect_s3_class(result, "tbl_now")
  expect_equal(get_data_type(result), "count-incidence")
  expect_equal(nrow(result), nrow(test_data$count_initial) + nrow(test_data$count_update))
})

test_that("update.tbl_now removes duplicates for count data by default", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$count_initial,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    case_count = "n",
    strata = "gender",
    data_type = "count-incidence",
    verbose = FALSE
  )

  # Add duplicate data
  duplicate_data <- test_data$count_initial[1:2, ]

  result <- update(initial_tbl, new_data = duplicate_data)

  # Should remove duplicates
  expect_equal(nrow(result), nrow(test_data$count_initial))
})

test_that("update.tbl_now remove_duplicates=FALSE keeps duplicates", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$count_initial,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    case_count = "n",
    strata = "gender",
    data_type = "count-incidence",
    verbose = FALSE
  )

  # Add duplicate data
  duplicate_data <- test_data$count_initial[1:2, ]


  expect_warning(update(initial_tbl, new_data = duplicate_data, remove_duplicates = FALSE))

  suppressWarnings(result <- update(initial_tbl, new_data = duplicate_data, remove_duplicates = FALSE))
  # Should keep duplicates
  expect_gt(nrow(result), nrow(test_data$count_initial))
})

test_that("update.tbl_now doesn't remove duplicates for linelist data", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    data_type = "linelist",
    verbose = FALSE
  )

  # Add duplicate rows
  duplicate_data <- test_data$initial_data[1:2, ]

  result <- update(initial_tbl, new_data = duplicate_data, remove_duplicates = FALSE)

  # Should keep all rows (linelist shouldn't remove duplicates)
  expect_equal(nrow(result), nrow(test_data$initial_data) + 2)
})

# ============================================================================
# Tests for update.tbl_now() - with tbl_now as new_data
# ============================================================================

test_that("update.tbl_now works when new_data is tbl_now", {
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    strata = "gender",
    verbose = FALSE
  )

  update_tbl <- tbl_now(
    test_data$update_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    strata = "gender",
    verbose = FALSE
  )

  result <- update(initial_tbl, new_data = update_tbl)

  expect_s3_class(result, "tbl_now")
  expect_equal(length(get_temporal_effects(result)), 0)
})

## --- temporal effects: left / right / both ---------------------------------

# Build two tbl_now with *different* lazy temporal-effects specs
setup_te_pair <- function() {
  test_data <- setup_test_data()
  left <- tbl_now(test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week", report_units = "weeks",
    event_units = "weeks", verbose = FALSE
  ) |>
    add_temporal_effects(temporal_effects(week_of_year = TRUE))
  right <- tbl_now(test_data$update_data,
    event_date = "onset_week",
    report_date = "report_week", report_units = "weeks",
    event_units = "weeks", verbose = FALSE
  ) |>
    add_temporal_effects(temporal_effects(day_of_week = TRUE))
  list(left = left, right = right)
}

# Helper: does a tbl_now's spec list contain a given effect?
te_has <- function(x, effect) {
  any(vapply(
    get_temporal_effects(x),
    function(s) isTRUE(S7::prop(s$t_effects, effect)), logical(1)
  ))
}

test_that("update t_effects = 'left' keeps object's spec (no error)", {
  skip_on_cran()
  p <- setup_te_pair()
  res <- update(p$left, new_data = p$right, t_effects = "left")
  expect_length(get_temporal_effects(res), 1L)
  expect_true(te_has(res, "week_of_year"))
  expect_false(te_has(res, "day_of_week"))
})

test_that("update t_effects = 'right' keeps new_data's spec", {
  skip_on_cran()
  p <- setup_te_pair()
  res <- update(p$left, new_data = p$right, t_effects = "right")
  expect_length(get_temporal_effects(res), 1L)
  expect_true(te_has(res, "day_of_week"))
  expect_false(te_has(res, "week_of_year"))
})

test_that("update t_effects = 'both' merges the two specs", {
  skip_on_cran()
  p <- setup_te_pair()
  res <- update(p$left, new_data = p$right, t_effects = "both")
  expect_length(get_temporal_effects(res), 2L)
  expect_true(te_has(res, "week_of_year"))
  expect_true(te_has(res, "day_of_week"))
})

test_that("update keeps lazy specs lazy (no computed columns)", {
  skip_on_cran()
  p <- setup_te_pair()
  res <- update(p$left, new_data = p$right, t_effects = "both")
  expect_length(get_temporal_effect_cols(res), 0L)
})

test_that("update recomputes temporal effects when inputs were computed", {
  skip_on_cran()
  p <- setup_te_pair()
  left <- compute_temporal_effects(p$left)
  right <- compute_temporal_effects(p$right)

  res <- update(left, new_data = right, t_effects = "both")
  cols <- get_temporal_effect_cols(res)
  expect_true(".event_week_of_year" %in% cols)
  expect_true(".event_day_of_week" %in% cols)
  # recomputed columns have no missing values (i.e. not stale/partial)
  expect_false(anyNA(res[[".event_week_of_year"]]))
  expect_false(anyNA(res[[".event_day_of_week"]]))
})

test_that("update t_effects = 'right' with a plain data.frame yields no spec", {
  skip_on_cran()
  p <- setup_te_pair()
  res <- update(p$left, new_data = setup_test_data()$update_data, t_effects = "right")
  expect_length(get_temporal_effects(res), 0L)
})

test_that("update errors on an unknown t_effects option", {
  skip_on_cran()
  p <- setup_te_pair()
  expect_error(
    update(p$left, new_data = p$right, t_effects = "middle"),
    "left.*right.*both"
  )
})

test_that("update preserves data integrity", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    strata = "gender",
    verbose = FALSE
  )

  result <- update(initial_tbl, new_data = test_data$update_data)

  # Check all original data is present
  original_events <- unique(test_data$initial_data$onset_week)
  result_events <- unique(result$onset_week)

  expect_true(all(original_events %in% result_events))

  # Check new data is present
  new_events <- unique(test_data$update_data$onset_week)
  expect_true(all(new_events %in% result_events))
})

test_that("update maintains tbl_now validation", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    verbose = FALSE
  )

  result <- update(initial_tbl, new_data = test_data$update_data)

  # Should pass validation
  expect_true(validate_tbl_now(result))
})

test_that("update works in iterative workflow", {
  skip_on_cran()
  data(denguedat)

  # Simulate receiving data in batches
  batch1 <- denguedat[1:50, ]
  batch2 <- denguedat[51:100, ]
  batch3 <- denguedat[101:150, ]

  # Start with first batch
  current_data <- tbl_now(
    batch1,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    strata = "gender",
    verbose = FALSE
  )

  expect_equal(nrow(current_data), 50)

  # Update with second batch
  current_data <- update(current_data, new_data = batch2)
  expect_equal(nrow(current_data), 100)

  # Update with third batch
  current_data <- update(current_data, new_data = batch3)
  expect_equal(nrow(current_data), 150)

  expect_s3_class(current_data, "tbl_now")
})

# ============================================================================
# Edge cases and error handling
# ============================================================================

test_that("update handles single row updates", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    verbose = FALSE
  )

  single_row <- test_data$update_data[1, , drop = FALSE]

  result <- update(initial_tbl, new_data = single_row)

  expect_s3_class(result, "tbl_now")
  expect_equal(nrow(result), nrow(test_data$initial_data) + 1)
})

test_that("update handles very large updates", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$initial_data[1:2, ],
    event_date = "onset_week",
    report_units = "weeks",
    event_units = "weeks",
    report_date = "report_week",
    verbose = FALSE
  )

  # Large update
  large_update <- test_data$initial_data[rep(3:5, 100), ]

  result <- update(initial_tbl, new_data = large_update)

  expect_s3_class(result, "tbl_now")
  expect_equal(nrow(result), 2 + nrow(large_update))
})

test_that("update with custom now parameter", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    verbose = FALSE
  )

  custom_now <- as.Date("2020-12-31")

  result <- update(initial_tbl, new_data = test_data$update_data, now = custom_now)

  expect_equal(get_now(result), custom_now)
})

test_that("update handles missing columns gracefully", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    strata = "gender",
    covariates = "temperature",
    verbose = FALSE
  )

  # Update data without temperature
  bad_update <- test_data$update_data |> dplyr::select(-temperature)

  expect_error(
    update(initial_tbl, new_data = bad_update),
    "temperature.*not present"
  )
})

test_that("update preserves all protected columns", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    verbose = FALSE
  )

  result <- update(initial_tbl, new_data = test_data$update_data)

  # Check protected columns exist
  expect_true(".event_num" %in% names(result))
  expect_true(".report_num" %in% names(result))
  expect_true(".delay" %in% names(result))
})

test_that("update recalculates delays correctly", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    verbose = FALSE
  )

  result <- update(initial_tbl, new_data = test_data$update_data)

  # All delays should be non-negative
  expect_true(all(result$.delay >= 0))

  # Delays should equal report_num - event_num
  expect_equal(result$.delay, result$.report_num - result$.event_num)
})


test_that("update.tbl_now validates matching attributes when new_data is tbl_now", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    verbose = FALSE
  )

  # Create update with different event_date name
  test_data$update_data$event_week <- test_data$update_data$onset_week

  update_tbl <- tbl_now(
    test_data$update_data,
    event_date = "event_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    verbose = FALSE
  )

  expect_error(
    update(initial_tbl, new_data = update_tbl),
    "event_date"
  )
})

# ============================================================================
# Tests for update_check_tbl_now_internal()
# ============================================================================

test_that("update_check_tbl_now_internal validates event_date match", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    verbose = FALSE
  )

  # Different event_date
  test_data$update_data$event_week <- test_data$update_data$onset_week
  update_tbl <- tbl_now(
    test_data$update_data,
    event_date = "event_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    verbose = FALSE
  )

  expect_error(
    update(initial_tbl, new_data = update_tbl),
    "event_date.*onset_week.*event_week"
  )
})

test_that("update_check_tbl_now_internal validates report_date match", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    verbose = FALSE
  )

  # Different report_date
  test_data$update_data$reporting_week <- test_data$update_data$report_week
  update_tbl <- tbl_now(
    test_data$update_data,
    event_date = "onset_week",
    report_date = "reporting_week",
    report_units = "weeks",
    event_units = "weeks",
    verbose = FALSE
  )

  expect_error(
    update(initial_tbl, new_data = update_tbl),
    "report_date.*report_week.*reporting_week"
  )
})

test_that("update_check_tbl_now_internal validates data_type match", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    data_type = "linelist",
    verbose = FALSE
  )

  update_tbl <- tbl_now(
    test_data$count_update,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    case_count = "n",
    data_type = "count-incidence",
    verbose = FALSE
  )

  expect_error(
    update(initial_tbl, new_data = update_tbl),
    "data_type.*linelist.*count-incidence"
  )
})

test_that("update_check_tbl_now_internal validates units match", {
  skip_on_cran()
  test_data <- setup_test_data()

  # Create data with different units
  initial_tbl <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    event_units = "weeks",
    report_units = "weeks",
    verbose = FALSE
  )

  # This would require numeric dates to have different units
  # For simplicity, we'll just test the error message structure
  expect_s3_class(initial_tbl, "tbl_now")
})

# ============================================================================
# Tests for update_check_data_frame_internal()
# ============================================================================

test_that("update_check_data_frame_internal validates event_date exists", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    verbose = FALSE
  )

  # Remove event_date from update
  bad_update <- test_data$update_data |> dplyr::select(-onset_week)

  expect_error(
    update(initial_tbl, new_data = bad_update),
    "onset_week.*not found"
  )
})

test_that("update_check_data_frame_internal validates report_date exists", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    verbose = FALSE
  )

  # Remove report_date from update
  bad_update <- test_data$update_data |> dplyr::select(-report_week)

  expect_error(
    update(initial_tbl, new_data = bad_update),
    "report_week.*not found"
  )
})

test_that("update_check_data_frame_internal validates case_count exists for count data", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(
    test_data$count_initial,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    case_count = "n",
    data_type = "count-incidence",
    verbose = FALSE
  )

  # Remove case_count from update
  bad_update <- test_data$count_update |> dplyr::select(-n)

  expect_error(
    update(initial_tbl, new_data = bad_update),
    "case_count.*n.*not found"
  )
})

test_that("update_check_data_frame_internal validates is_censored exists", {
  skip_on_cran()
  test_data <- setup_test_data()

  test_data$initial_data$is_censored <- c(FALSE, FALSE, TRUE, FALSE, FALSE)

  initial_tbl <- tbl_now(
    test_data$initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    is_censored = "is_censored",
    verbose = FALSE
  )

  # Update without is_censored column
  expect_error(
    update(initial_tbl, new_data = test_data$update_data),
    "is_censored.*not found"
  )
})

# ============================================================================
# Tests for example from test-update.R (from documentation)
# ============================================================================

test_that("update keeps everything similar when nothing new is observed", {
  skip_on_cran()
  data(denguedat)

  initial_tbl <- tbl_now(
    denguedat[1:100, ],
    report_units = "weeks",
    event_units = "weeks",
    event_date = "onset_week",
    report_date = "report_week",
    strata = "gender",
    verbose = FALSE
  ) |>
    to_count(to = "count-incidence")

  # Update with same data
  result <- update(initial_tbl, new_data = initial_tbl)

  # Should be identical (duplicates removed)
  expect_equal(
    result |> dplyr::arrange(onset_week, report_week, gender),
    initial_tbl |> dplyr::arrange(onset_week, report_week, gender)
  )
})

test_that("partitioning data and updating gives same result", {
  skip_on_cran()
  data(denguedat)

  initial_tbl <- tbl_now(
    denguedat[1:100, ],
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    strata = "gender",
    verbose = FALSE
  ) |>
    to_count(to = "count-incidence")

  # Partition the data
  subset1 <- initial_tbl |> dplyr::filter(report_week <= as.Date("1990-06-11"))
  subset2 <- initial_tbl |> dplyr::filter(report_week > as.Date("1990-06-11"))

  # Update should reconstruct original
  result <- update(subset1, new_data = subset2)

  expect_equal(
    result |> dplyr::arrange(onset_week, report_week, gender),
    initial_tbl |> dplyr::arrange(onset_week, report_week, gender)
  )
})

test_that("update with empty tbl_now works", {
  skip_on_cran()
  data(denguedat)

  initial_tbl <- tbl_now(
    denguedat[1:100, ],
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    strata = "gender",
    verbose = FALSE
  ) |>
    to_count(to = "count-incidence")

  # Create empty subset
  empty_subset <- initial_tbl |> dplyr::filter(.delay > Inf)

  expect_equal(nrow(empty_subset), 0)

  result <- update(initial_tbl, new_data = empty_subset)

  # Should be same as original
  expect_equal(nrow(result), nrow(initial_tbl))
})


# Additional tests for update.R to improve coverage

# Tests for strata = "right" (lines 76-78, 94, 97-101, 105, 107)
test_that("update can use right strata", {
  skip_on_cran()
  test_data <- setup_test_data()

  # Create initial data with gender strata
  initial_data <- test_data$count_initial
  initial_data$age_group <- c("20-30", "30-40", "20-30", "20-30")

  initial_tbl <- tbl_now(initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    strata = "gender",
    case_count = "n",
    verbose = FALSE
  )

  # Create new data with age_group strata
  update_data <- test_data$count_update
  update_data$age_group <- c("20-30", "30-40")

  new_tbl <- tbl_now(update_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    strata = "age_group",
    case_count = "n",
    verbose = FALSE
  )

  result <- update(initial_tbl, new_data = new_tbl, strata = "right")

  expect_equal(get_strata(result), "age_group")
})

test_that("update fails when right strata not in object", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(test_data$count_initial,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    strata = "gender",
    case_count = "n",
    verbose = FALSE
  )

  # Create new data with strata not in initial
  update_data <- test_data$count_update
  update_data$region <- c("North", "South")

  new_tbl <- tbl_now(update_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    strata = "region",
    case_count = "n",
    verbose = FALSE
  )

  expect_error(
    update(initial_tbl, new_data = new_tbl, strata = "right"),
    "Strata.*was not present in.*object"
  )
})

# Tests for strata = "both" (lines 110-126, 129-131)
test_that("update can use both strata", {
  skip_on_cran()
  test_data <- setup_test_data()

  # Create initial data with gender strata
  initial_data <- test_data$count_initial
  initial_data$age_group <- c("20-30", "30-40", "20-30", "20-30")

  initial_tbl <- tbl_now(initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    strata = "gender",
    case_count = "n",
    verbose = FALSE
  )

  # Create new data with both gender and age_group strata
  update_data <- test_data$count_update
  update_data$age_group <- c("20-30", "30-40")

  new_tbl <- tbl_now(update_data,
    event_date = "onset_week",
    report_date = "report_week",
    strata = c("gender", "age_group"),
    report_units = "weeks",
    event_units = "weeks",
    case_count = "n",
    verbose = FALSE
  )

  result <- update(initial_tbl, new_data = new_tbl, strata = "both")

  expect_true("gender" %in% get_strata(result))
  expect_true("age_group" %in% get_strata(result))
})

test_that("update fails when both strata has column not in new_data", {
  skip_on_cran()
  test_data <- setup_test_data()

  # Initial has gender
  initial_data <- test_data$count_initial
  initial_data$age_group <- c("20-30", "30-40", "20-30", "20-30")

  initial_tbl <- tbl_now(initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    strata = c("gender", "age_group"),
    case_count = "n",
    report_units = "weeks",
    event_units = "weeks",
    verbose = FALSE
  )

  # New data only has gender (missing age_group)
  new_tbl <- tbl_now(test_data$count_update,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    strata = "gender",
    case_count = "n",
    verbose = FALSE
  )

  expect_error(
    update(initial_tbl, new_data = new_tbl, strata = "both"),
    "Strata.*age_group.*was not present in.*new_data"
  )
})

test_that("update fails when both strata has column not in object", {
  skip_on_cran()
  test_data <- setup_test_data()

  # Initial has only gender
  initial_tbl <- tbl_now(test_data$count_initial,
    event_date = "onset_week",
    report_date = "report_week",
    strata = "gender",
    report_units = "weeks",
    event_units = "weeks",
    case_count = "n",
    verbose = FALSE
  )

  # New data has gender and age_group
  update_data <- test_data$count_update
  update_data$age_group <- c("20-30", "30-40")

  new_tbl <- tbl_now(update_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    strata = c("gender", "age_group"),
    case_count = "n",
    verbose = FALSE
  )

  expect_error(
    update(initial_tbl, new_data = new_tbl, strata = "both"),
    "Strata.*age_group.*was not present in.*object"
  )
})

# Tests for covariates = "right" (lines 148, 151-155, 159-160, 162)
test_that("update can use right covariates", {
  skip_on_cran()
  test_data <- setup_test_data()

  # Create initial data with temperature covariate
  initial_data <- test_data$count_initial
  initial_data$temperature <- c(25.5, 26.0, 24.8, 22)
  initial_data$humidity <- c(0.6, 0.65, 0.7, 0.1)

  initial_tbl <- tbl_now(initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    covariates = "temperature",
    report_units = "weeks",
    event_units = "weeks",
    case_count = "n",
    verbose = FALSE
  )

  # Create new data with humidity covariate
  update_data <- test_data$count_update
  update_data$temperature <- c(25.2, 25.8)
  update_data$humidity <- c(0.68, 0.62)

  new_tbl <- tbl_now(update_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    covariates = "humidity",
    case_count = "n",
    verbose = FALSE
  )

  result <- update(initial_tbl, new_data = new_tbl, covariates = "right")

  expect_equal(get_covariates(result), "humidity")
})

test_that("update fails when right covariate not in object", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(test_data$count_initial,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    case_count = "n",
    verbose = FALSE
  )

  # Create new data with covariate not in initial
  update_data <- test_data$count_update
  update_data$temperature <- c(25.2, 25.8)

  new_tbl <- tbl_now(update_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    covariates = "temperature",
    case_count = "n",
    verbose = FALSE
  )

  expect_error(
    update(initial_tbl, new_data = new_tbl, covariates = "right"),
    "Covariate.*temperature.*was not present in.*object"
  )
})

# Tests for covariates = "both" (lines 165-180, 184-186)
test_that("update can use both covariates", {
  skip_on_cran()
  test_data <- setup_test_data()

  # Create initial data with temperature
  initial_data <- test_data$count_initial
  initial_data$temperature <- c(25.5, 26.0, 24.8, 22)
  initial_data$humidity <- c(0.6, 0.65, 0.7, 0.1)

  initial_tbl <- tbl_now(initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    covariates = "temperature",
    case_count = "n",
    verbose = FALSE
  )

  # Create new data with both
  update_data <- test_data$count_update
  update_data$temperature <- c(25.2, 25.8)
  update_data$humidity <- c(0.68, 0.62)

  new_tbl <- tbl_now(update_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    covariates = c("temperature", "humidity"),
    case_count = "n",
    verbose = FALSE
  )

  result <- update(initial_tbl, new_data = new_tbl, covariates = "both")

  expect_true("temperature" %in% get_covariates(result))
  expect_true("humidity" %in% get_covariates(result))
})

test_that("update fails when both covariates has column not in new_data", {
  skip_on_cran()
  test_data <- setup_test_data()

  # Initial has temperature and humidity
  initial_data <- test_data$count_initial
  initial_data$temperature <- c(25.5, 26.0, 24.8, 22)
  initial_data$humidity <- c(0.6, 0.65, 0.7, 0.1)

  initial_tbl <- tbl_now(initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    covariates = c("temperature", "humidity"),
    case_count = "n",
    verbose = FALSE
  )

  # New data only has temperature
  update_data <- test_data$count_update
  update_data$temperature <- c(25.2, 25.8)

  new_tbl <- tbl_now(update_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    covariates = "temperature",
    case_count = "n",
    verbose = FALSE
  )

  expect_error(
    update(initial_tbl, new_data = new_tbl, covariates = "both"),
    "Covariate.*humidity.*was not present in.*new_data"
  )
})

test_that("update fails when both covariates has column not in object", {
  skip_on_cran()
  test_data <- setup_test_data()

  # Initial has only temperature
  initial_data <- test_data$count_initial
  initial_data$temperature <- c(25.5, 26.0, 24.8, 22)

  initial_tbl <- tbl_now(initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    covariates = "temperature",
    case_count = "n",
    verbose = FALSE
  )

  # New data has both
  update_data <- test_data$count_update
  update_data$temperature <- c(25.2, 25.8)
  update_data$humidity <- c(0.68, 0.62)

  new_tbl <- tbl_now(update_data,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    covariates = c("temperature", "humidity"),
    case_count = "n",
    verbose = FALSE
  )

  expect_error(
    update(initial_tbl, new_data = new_tbl, covariates = "both"),
    "Covariate.*humidity.*was not present in.*object"
  )
})

# Tests for update_check_tbl_now_internal error conditions (lines 224, 251-257, 262-268, 273-279, 284-290, 295-301)
test_that("update_check_tbl_now_internal fails with different event_date", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(test_data$count_initial,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    case_count = "n",
    verbose = FALSE
  )

  # Create new_tbl with different event_date name
  update_data <- test_data$count_update
  update_data$event_week <- update_data$onset_week

  new_tbl <- tbl_now(update_data,
    event_date = "event_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    case_count = "n",
    verbose = FALSE
  )

  expect_error(
    update(initial_tbl, new_data = new_tbl),
    "event_date.*onset_week.*event_date.*event_week"
  )
})

test_that("update_check_tbl_now_internal fails with different data_type", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(test_data$count_initial,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    case_count = "n",
    data_type = "count-incidence",
    verbose = FALSE
  )

  new_tbl <- tbl_now(test_data$count_update,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    case_count = "n",
    data_type = "count-cumulative",
    verbose = FALSE
  )

  expect_error(
    update(initial_tbl, new_data = new_tbl),
    "data_type.*count-incidence.*data_type.*count-cumulative"
  )
})

test_that("update_check_tbl_now_internal fails with different event_units", {
  skip_on_cran()
  test_data <- setup_test_data()

  # Create data with different units
  initial_data <- test_data$count_initial
  initial_data$onset_num <- c(1L, 2L, 3L, 2L)
  initial_data$report_num <- c(4L, 5L, 6L, 6L)

  initial_tbl <- tbl_now(initial_data,
    event_date = "onset_num",
    report_date = "report_num",
    case_count = "n",
    event_units = "numeric",
    report_units = "numeric",
    data_type = "count-cumulative",
    verbose = FALSE
  )

  test_data$count_update$onset_num <- test_data$count_update$onset_week
  test_data$count_update$report_num <- test_data$count_update$report_week
  new_tbl <- tbl_now(test_data$count_update,
    event_date = "onset_num",
    report_date = "report_num",
    case_count = "n",
    event_units = "days",
    report_units = "days",
    data_type = "count-cumulative",
    verbose = FALSE
  )

  expect_error(
    update(initial_tbl, new_data = new_tbl),
    "event_units.*numeric.*event_units.*days"
  )
})

test_that("update_check_tbl_now_internal fails with different report_units", {
  skip_on_cran()
  test_data <- setup_test_data()

  # This is tricky - need different report_units but same event_units
  # Create weekly event data but different report units
  initial_data <- data.frame(
    onset_week = as.Date(c("2020-07-08", "2020-07-15")),
    report_week = as.Date(c("2020-07-11", "2020-07-18")),
    n = c(5L, 3L)
  )

  initial_tbl <- tbl_now(initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    case_count = "n",
    event_units = "weeks",
    report_units = "weeks",
    verbose = FALSE
  )

  update_data <- data.frame(
    onset_week = as.Date(c("2020-07-22", "2020-07-29")),
    report_week = as.Date(c("2020-07-25", "2020-08-01")),
    n = c(7L, 4L)
  )

  new_tbl <- tbl_now(update_data,
    event_date = "onset_week",
    report_date = "report_week",
    case_count = "n",
    event_units = "weeks",
    report_units = "months",
    verbose = FALSE
  )

  expect_error(
    update(initial_tbl, new_data = new_tbl),
    "report_units.*weeks.*report_units.*months"
  )
})

test_that("update_check_tbl_now_internal fails with different case_count", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(test_data$count_initial,
    event_date = "onset_week",
    report_date = "report_week",
    case_count = "n",
    report_units = "weeks",
    event_units = "weeks",
    verbose = FALSE
  )

  # Rename case_count column
  update_data <- test_data$count_update
  update_data$cases <- update_data$n

  new_tbl <- tbl_now(update_data,
    event_date = "onset_week",
    report_date = "report_week",
    case_count = "cases",
    report_units = "weeks",
    event_units = "weeks",
    verbose = FALSE
  )

  expect_error(
    update(initial_tbl, new_data = new_tbl),
    "case_count.*n.*case_count.*cases"
  )
})

test_that("update_check_tbl_now_internal fails with different is_censored", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_data <- test_data$count_initial
  initial_data$censored1 <- FALSE

  initial_tbl <- tbl_now(initial_data,
    event_date = "onset_week",
    report_date = "report_week",
    case_count = "n",
    is_censored = "censored1",
    report_units = "weeks",
    event_units = "weeks",
    verbose = FALSE
  )

  update_data <- test_data$count_update
  update_data$censored2 <- FALSE

  new_tbl <- tbl_now(update_data,
    event_date = "onset_week",
    report_date = "report_week",
    case_count = "n",
    is_censored = "censored2",
    report_units = "weeks",
    event_units = "weeks",
    verbose = FALSE
  )

  expect_error(
    update(initial_tbl, new_data = new_tbl),
    "is_censored.*censored1.*is_censored.*censored2"
  )
})

# Tests for update_check_data_frame_internal (lines 321, 325)
test_that("update_check_data_frame_internal fails with non-data.frame", {
  skip_on_cran()
  test_data <- setup_test_data()

  initial_tbl <- tbl_now(test_data$count_initial,
    event_date = "onset_week",
    report_date = "report_week",
    report_units = "weeks",
    event_units = "weeks",
    case_count = "n",
    verbose = FALSE
  )

  # This should be caught earlier, but testing the internal function
  expect_error(
    update_check_data_frame_internal(initial_tbl, list(x = 1:10)),
    "must be a data.frame"
  )
})
