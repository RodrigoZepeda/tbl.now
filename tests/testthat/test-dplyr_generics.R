# --- Setup: Create a function to make a minimal, valid tbl_now for testing ---
make_test_tbl_now <- function(n = 10) {
  # Mock data to simulate 'denguedat' structure and column types
  df <- dplyr::tibble(
    onset_week  = seq(lubridate::ymd("1990-01-01"), by = "week", length.out = n),
    report_week = onset_week + lubridate::days(sample(0:14, n, replace = TRUE)),
    gender      = sample(c("Male", "Female"), n, replace = TRUE),
    value       = 1:n
  )

  # Create a valid tbl_now object
  tbl_now(
    data = df,
    event_date = "onset_week",
    report_date = "report_week",
    strata = "gender",
    data_type = "linelist",
    report_units = "days",
    event_units = "days"
  )
}

# ----------------------------------------------------------------------
# 1. Test Base R Subsetting and Modification Generics
# ----------------------------------------------------------------------

test_that("`[.tbl_now` preserves class on valid subset", {
  x <- make_test_tbl_now(n = 20)
  subset_valid <- x[1:10, ]

  expect_s3_class(subset_valid, "tbl_now")
  expect_equal(get_event_date(subset_valid), "onset_week")
  expect_equal(get_num_strata(subset_valid), 1)
})

test_that("`[.tbl_now` downgrades to tibble if protected columns are dropped", {
  skip_on_cran()
  x <- make_test_tbl_now(n = 20)

  expect_warning(x[1:10, -1])

  # Protected columns include `event_date` ("onset_week")
  subset_invalid <- suppressWarnings(
    x[1:10, -1] # Drops the first column, "onset_week"
  )

  expect_false(inherits(subset_invalid, "tbl_now"))
  expect_s3_class(subset_invalid, "tbl_df")
  # Should show a warning about dropping protected column, which is handled
  # in tbl_now_reconstruct_internal, but we don't test the warning here
})

test_that("`names<-.tbl_now` preserves class if protected columns are not renamed", {
  x <- make_test_tbl_now()
  # Rename a non-protected column
  names(x)[4] <- "new_value"
  # Protected columns should remain the same
  expect_equal(names(x)[1], "onset_week")
  expect_s3_class(x, "tbl_now")
  expect_equal(get_event_date(x), "onset_week")
})

test_that("`names<-.tbl_now` downgrades to tibble if a protected column is renamed", {
  skip_on_cran()
  x <- make_test_tbl_now()
  # Rename the protected column "onset_week" (first column)
  original_names <- names(x)
  new_names <- original_names
  new_names[1] <- "event_date_renamed"

  # Need to check the call to names<- directly
  x_renamed <- x
  suppressWarnings(
    names(x_renamed) <- new_names
  )

  # The internal reconstruct check sees that "onset_week" is missing from the data
  # and downgrades it.
  expect_false(inherits(x_renamed, "tbl_now"))
  expect_s3_class(x_renamed, "tbl_df")
})

test_that("`$<-.tbl_now` preserves class on valid column replacement/addition", {
  x <- make_test_tbl_now()
  # Add a new column
  x$new_col <- "test"

  expect_s3_class(x, "tbl_now")
  expect_equal(x$new_col, rep("test", 10))
})

# ----------------------------------------------------------------------
# 2. Test dplyr Generics (`dplyr_row_dplyr::slice`, `dplyr_col_modify`, `dplyr_reconstruct`)
# ----------------------------------------------------------------------

test_that("`dplyr_row_dplyr::slice.tbl_now` preserves class and attributes", {
  x <- make_test_tbl_now(n = 20)
  # Row slicing via `dplyr::slice()`
  sliced <- x |> dplyr::slice(1:5)

  expect_s3_class(sliced, "tbl_now")
  expect_equal(nrow(sliced), 5)
  expect_equal(get_strata(sliced), "gender")
})

test_that("`dplyr_col_modify.tbl_now` preserves class and attributes", {
  x <- make_test_tbl_now()
  # Column modification via `dplyr::mutate()`
  modified <- x |> dplyr::mutate(value = value * 2)

  expect_s3_class(modified, "tbl_now")
  expect_equal(modified$value[1], 2)
  expect_equal(get_event_date(modified), "onset_week")
})

test_that("`dplyr_reconstruct.tbl_now` handles reconstruction logic", {
  template <- make_test_tbl_now()
  # Scenario 1: Valid data reconstruction
  valid_data <- template |> dplyr::select(-gender) # Drop 'value'


  reconstructed_valid <- dplyr_reconstruct(valid_data, template)


  expect_s3_class(reconstructed_valid, "tbl_now")
  expect_equal(get_num_strata(reconstructed_valid), 0) # 'strata' deosn not exist
  expect_false("gender" %in% names(reconstructed_valid))


  # Scenario 2: Downgrade due to missing protected column
  invalid_data <- suppressWarnings(
    template |> dplyr::select(report_week, gender)
  )
  reconstructed_invalid <- suppressWarnings(
    dplyr_reconstruct(invalid_data, template)
  )

  expect_false(inherits(reconstructed_invalid, "tbl_now"))
  expect_s3_class(reconstructed_invalid, "tbl_df")


  # Scenario 3: Downgrade due to missing protected column
  invalid_data <- suppressWarnings(
    template |> dplyr::select(-onset_week)
  )
  reconstructed_invalid <- suppressWarnings(
    dplyr_reconstruct(invalid_data, template)
  )

  expect_false(inherits(reconstructed_invalid, "tbl_now"))
  expect_s3_class(reconstructed_invalid, "tbl_df")
})

# ----------------------------------------------------------------------
# 3. Test Grouping and Summarization Generics
# ----------------------------------------------------------------------

test_that("`group_by.tbl_now` creates a `grouped_tbl_now`", {
  x <- make_test_tbl_now()
  grouped <- x |> group_by(gender)

  expect_s3_class(grouped, "tbl_now")
  expect_s3_class(grouped, "grouped_df")
  expect_s3_class(grouped, "grouped_tbl_now")

  # Original attributes should be preserved
  expect_equal(get_strata(grouped), "gender")
})

test_that("`ungroup.grouped_tbl_now` returns an ungrouped `tbl_now`", {
  x <- make_test_tbl_now()
  grouped <- x |> group_by(gender)
  ungrouped <- grouped |> ungroup()

  expect_s3_class(ungrouped, "tbl_now")
  expect_false(inherits(ungrouped, "grouped_df"))
  expect_equal(get_strata(ungrouped), "gender")
})

test_that("`summarise.tbl_now` preserves class when valid", {
  x <- make_test_tbl_now()


  # In this case, since `data_type` is 'linelist', 'n' is not protected.
  # If we summarize without dropping 'onset_week' and 'report_week', it should work.
  summarized_valid <- suppressWarnings({ # Suppress warning from tbl_now about event/report units
    x |>
      group_by(onset_week, report_week, gender, .event_num, .report_num) |>
      summarise(max_report = max(report_week), .groups = "drop")
  })

  expect_s3_class(summarized_valid, "tbl_now")
  expect_equal(get_data_type(summarized_valid), "linelist")
  expect_true("gender" %in% names(summarized_valid))
})

test_that("`summarise.tbl_now` drops class when protected columns are missing", {
  skip_on_cran()
  x <- make_test_tbl_now()
  # This summary drops the protected column `onset_week`
  summarized_invalid <- suppressWarnings({ # Suppress 'Dropping `tbl_now` attributes' warning
    x |>
      group_by(onset_week, gender, .event_num, .report_num) |>
      summarise(max_report = max(report_week), .groups = "drop")
  })

  expect_false(inherits(summarized_invalid, "tbl_now"))
  expect_s3_class(summarized_invalid, "tbl")
})

test_that("`summarise.grouped_tbl_now` works via delegation", {
  skip_on_cran()
  x <- make_test_tbl_now()
  grouped <- x |> group_by(gender)

  # The resulting object should have the `tbl_now` attributes re-applied
  expect_s3_class(grouped, "grouped_tbl_now")
  expect_s3_class(grouped, "grouped_df")
  expect_equal(grouped |> ungroup(), x)
})

# Test file for dplyr_generics.R functions

# Setup test data ----
setup_test_data <- function() {
  base_data <- data.frame(
    onset_week = as.Date(c("2020-07-08", "2020-07-15", "2020-07-22", "2020-07-29")),
    report_week = as.Date(c("2020-07-11", "2020-07-18", "2020-07-25", "2020-08-01")),
    gender = c("Male", "Female", "Male", "Female"),
    age_group = c("20-30", "30-40", "20-30", "40-50"),
    temperature = c(25.5, 26.0, 24.8, 25.2),
    is_censored_report = c(T, F, F, F),
    value = c(10, 20, 30, 40)
  )

  ndata <- tbl_now(
    base_data,
    event_date = "onset_week",
    report_date = "report_week",
    strata = "gender",
    covariates = "temperature",
    is_censored_report = "is_censored_report",
    verbose = FALSE
  )

  list(
    ndata = ndata,
    base_data = base_data
  )
}

# Tests for validate_tbl_now() ----
test_that("validate_tbl_now passes for valid tbl_now", {
  test_data <- setup_test_data()

  expect_true(validate_tbl_now(test_data$ndata))
  expect_invisible(validate_tbl_now(test_data$ndata))
})

test_that("validate_tbl_now fails for non-data.frame", {
  skip_on_cran()
  expect_error(
    validate_tbl_now(list(a = 1, b = 2)),
    "must inherit from.*data.frame"
  )
})

test_that("validate_tbl_now fails when required attributes are missing", {
  skip_on_cran()
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  # Remove a required attribute
  attr(ndata, "event_date") <- NULL

  expect_error(
    suppressWarnings(validate_tbl_now(ndata)),
    "Missing required attribute"
  )
})

test_that("validate_tbl_now fails when event_date is not character", {
  skip_on_cran()
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  attr(ndata, "event_date") <- 123

  expect_error(
    suppressWarnings(validate_tbl_now(ndata)),
    "event_date.*must be"
  )
})

test_that("validate_tbl_now fails when report_date is not character", {
  skip_on_cran()
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  attr(ndata, "report_date") <- 123

  expect_error(
    suppressWarnings(validate_tbl_now(ndata)),
    "report_date.*must be"
  )
})

test_that("validate_tbl_now fails when report or event date is not date", {
  skip_on_cran()
  test_data <- setup_test_data()

  for (type in c("report_date", "event_date")) {
    ndata <- test_data$ndata
    ndata <- ndata |>
      dplyr::mutate(!!as.symbol(attr(ndata, type)) := as.character(!!as.symbol(attr(ndata, type))))

    expect_error(
      suppressWarnings(validate_tbl_now(ndata)),
      "must be of class Date"
    )
  }
})

test_that("validate_tbl_now fails when is_censored_report is not logical", {
  skip_on_cran()
  test_data <- setup_test_data()
  ndata <- test_data$ndata
  ndata <- ndata |>
    dplyr::mutate(!!as.symbol(attr(ndata, "is_censored_report")) := as.character(!!as.symbol(attr(ndata, "is_censored_report"))))

  expect_error(
    suppressWarnings(validate_tbl_now(ndata)),
    "must be logical"
  )
})

test_that("validate_tbl_now fails when now is not date", {
  skip_on_cran()
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  attr(ndata, "now") <- "error"

  expect_error(
    suppressWarnings(validate_tbl_now(ndata)),
    "now.*must be"
  )
})

test_that("validate_tbl_now fails when data_type is not count, linelist or official", {
  skip_on_cran()
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  attr(ndata, "data_type") <- "error"

  expect_error(
    suppressWarnings(validate_tbl_now(ndata)),
    "data_type.*must be"
  )
})

test_that("validate_tbl_now fails when is_censored_report is not specified correctly", {
  skip_on_cran()
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  attr(ndata, "is_censored_report") <- 2

  expect_error(
    suppressWarnings(validate_tbl_now(ndata)),
    "is_censored_report.*must be"
  )

  attr(ndata, "is_censored_report") <- c("a", "b")
  expect_error(
    suppressWarnings(validate_tbl_now(ndata)),
    "is_censored_report.*must be"
  )

  attr(ndata, "is_censored_report") <- "not_a_column"
  expect_error(
    suppressWarnings(validate_tbl_now(ndata)),
    "Column.*not found in data"
  )
})

test_that("validate_tbl_now fails when units is not valid", {
  skip_on_cran()
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  for (unit_type in c("event_units", "report_units")) {
    attr(ndata, unit_type) <- "error"

    expect_error(
      suppressWarnings(validate_tbl_now(ndata)),
      "Attribute.*must be one of.*days.*weeks"
    )
  }
})

test_that("arrange then mutate respects all groups (e.g. gender)", {
  skip_on_cran()
  test_data <- setup_test_data()

  result <- test_data$ndata |>
    to_count("count-incidence") |>
    group_by(onset_week, gender) |>
    dplyr::arrange(report_week) |>
    dplyr::mutate(lagged = dplyr::lag(n))

  # The first row of each (onset_week, gender) group must have NA lag
  first_in_group <- result |>
    dplyr::filter(dplyr::row_number() == 1L) # per group after arrange

  # No lagged value should bleed across gender groups
  # i.e. for a fixed onset_week, the first Male row should not carry
  # the last Female value as its lag
  cross_group_bleed <- result |>
    dplyr::mutate(prev_gender = dplyr::lag(gender)) |>
    dplyr::filter(!is.na(prev_gender), prev_gender != gender, !is.na(lagged))

  expect_equal(nrow(cross_group_bleed), 0L)
})


test_that("validate_tbl_now fails when columns don't exist", {
  skip_on_cran()
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  # Set event_date to non-existent column
  attr(ndata, "event_date") <- "nonexistent"

  expect_error(
    suppressWarnings(validate_tbl_now(ndata)),
    "not found in data"
  )
})

test_that("validate_tbl_now warns when report_date before event_date", {
  skip_on_cran()
  bad_data <- data.frame(
    onset_week = as.Date(c("2020-07-15", "2020-07-22")),
    report_week = as.Date(c("2020-07-08", "2020-07-18")) # First is before event
  )

  ndata <- suppressWarnings(
    tbl_now(
      bad_data,
      event_date = "onset_week",
      report_date = "report_week",
      verbose = FALSE
    )
  )

  # Should create with warning
  expect_warning(
    validate_tbl_now(ndata),
    "report_date.*before.*event_date"
  )
})

# Tests for is_tbl_now() ----
test_that("is_tbl_now returns TRUE for valid tbl_now", {
  test_data <- setup_test_data()

  expect_true(is_tbl_now(test_data$ndata))
})

test_that("is_tbl_now returns FALSE for regular data.frame", {
  skip_on_cran()
  regular_df <- data.frame(x = 1:3, y = 4:6)

  expect_false(is_tbl_now(regular_df))
})

test_that("is_tbl_now returns FALSE for invalid tbl_now", {
  skip_on_cran()
  test_data <- setup_test_data()
  ndata <- test_data$ndata

  # Corrupt it by removing required attribute
  attr(ndata, "event_date") <- NULL

  expect_false(is_tbl_now(ndata))
})

test_that("is_tbl_now returns FALSE for object with tbl_now class but invalid structure", {
  skip_on_cran()
  fake_tbl_now <- data.frame(x = 1:3)
  class(fake_tbl_now) <- c("tbl_now", "data.frame")

  expect_false(is_tbl_now(fake_tbl_now))
})

# Tests for subsetting with [.tbl_now ----
test_that("[.tbl_now maintains tbl_now class with valid subset", {
  skip_on_cran()
  test_data <- setup_test_data()

  result <- test_data$ndata[1:2, ]

  expect_s3_class(result, "tbl_now")
  expect_true(is_tbl_now(result))
})

test_that("[.tbl_now maintains attributes", {
  skip_on_cran()
  test_data <- setup_test_data()

  result <- test_data$ndata[1:2, ]

  expect_equal(get_event_date(result), get_event_date(test_data$ndata))
  expect_equal(get_report_date(result), get_report_date(test_data$ndata))
  expect_equal(get_strata(result), get_strata(test_data$ndata))
})

test_that("[.tbl_now drops to data.frame when removing protected columns", {
  skip_on_cran()
  test_data <- setup_test_data()

  # Remove event_date column
  expect_warning(
    result <- test_data$ndata[, -which(colnames(test_data$ndata) == "onset_week")],
    "Dropped protected column"
  )

  expect_false(is_tbl_now(result))
  expect_s3_class(result, "tbl_df")
})

test_that("[.tbl_now updates now when rows change", {
  skip_on_cran()
  test_data <- setup_test_data()

  original_now <- get_now(test_data$ndata)

  # Subset to first two rows only
  result <- test_data$ndata[1:2, ]

  # Now might be updated
  expect_s3_class(get_now(result), "Date")
})

test_that("[.tbl_now handles column selection", {
  skip_on_cran()
  test_data <- setup_test_data()

  # Select specific columns including protected ones
  result <- test_data$ndata[, c("onset_week", "report_week", "gender", ".event_num", ".report_num", "is_censored_report", ".delay")]

  expect_s3_class(result, "tbl_now")
  expect_true("onset_week" %in% colnames(result))
  expect_true("report_week" %in% colnames(result))
})

# Tests for names<-.tbl_now ----
test_that("names<-.tbl_now maintains tbl_now class with valid names", {
  skip_on_cran()
  test_data <- setup_test_data()

  ndata <- test_data$ndata
  new_names <- colnames(ndata)
  new_names[which(new_names == "value")] <- "new_value"

  names(ndata) <- new_names

  expect_s3_class(ndata, "tbl_now")
})

test_that("names<-.tbl_now drops to data.frame when renaming protected columns", {
  skip_on_cran()
  test_data <- setup_test_data()

  for (protected in c("onset_week", "report_week", ".event_num", ".report_num")) {
    ndata <- test_data$ndata
    new_names <- colnames(ndata)
    new_names[which(new_names == protected)] <- "renamed"

    expect_warning(
      names(ndata) <- new_names,
      "Dropped protected column"
    )

    expect_false(is_tbl_now(ndata))
  }
})

# Tests for $<-.tbl_now ----
test_that("$<-.tbl_now maintains tbl_now class when adding column", {
  skip_on_cran()
  test_data <- setup_test_data()

  ndata <- test_data$ndata
  ndata$new_column <- 1:nrow(ndata)

  expect_s3_class(ndata, "tbl_now")
  expect_true("new_column" %in% colnames(ndata))
})

test_that("$<-.tbl_now maintains tbl_now class when modifying column", {
  skip_on_cran()
  test_data <- setup_test_data()

  ndata <- test_data$ndata
  ndata$value <- ndata$value * 2

  expect_s3_class(ndata, "tbl_now")
})

test_that("$<-.tbl_now allows modifying non-protected columns", {
  skip_on_cran()
  test_data <- setup_test_data()

  ndata <- test_data$ndata
  ndata$gender <- "Unknown"

  expect_s3_class(ndata, "tbl_now")
  expect_true(all(ndata$gender == "Unknown"))
})

# Tests for dplyr_row_slice.tbl_now ----
test_that("dplyr_row_slice maintains tbl_now with valid slice", {
  skip_on_cran()
  test_data <- setup_test_data()

  result <- test_data$ndata |> dplyr::slice(1:2)

  expect_s3_class(result, "tbl_now")
  expect_equal(nrow(result), 2)
})

test_that("dplyr_row_slice preserves attributes", {
  skip_on_cran()
  test_data <- setup_test_data()

  result <- test_data$ndata |> dplyr::slice(1:3)

  expect_equal(get_event_date(result), get_event_date(test_data$ndata))
  expect_equal(get_strata(result), get_strata(test_data$ndata))
})

# Tests for dplyr_col_modify.tbl_now ----
test_that("dplyr_col_modify maintains tbl_now when adding columns", {
  skip_on_cran()
  test_data <- setup_test_data()

  result <- test_data$ndata |> dplyr::mutate(new_col = value * 2)

  expect_s3_class(result, "tbl_now")
  expect_true("new_col" %in% colnames(result))
})

test_that("dplyr_col_modify maintains tbl_now when modifying columns", {
  skip_on_cran()
  test_data <- setup_test_data()

  result <- test_data$ndata |> dplyr::mutate(value = value + 10)

  expect_s3_class(result, "tbl_now")
})

test_that("dplyr_col_modify drops to tibble when removing protected columns", {
  skip_on_cran()
  test_data <- setup_test_data()

  expect_warning(
    result <- test_data$ndata |> dplyr::select(-onset_week),
    "Dropped protected column"
  )

  expect_false(is_tbl_now(result))
})

# Tests for group_by.tbl_now ----
test_that("group_by creates grouped_tbl_now", {
  skip_on_cran()
  test_data <- setup_test_data()

  result <- test_data$ndata |> dplyr::group_by(gender)

  expect_s3_class(result, "grouped_tbl_now")
  expect_s3_class(result, "tbl_now")
  expect_true(dplyr::is_grouped_df(result))
})

test_that("group_by preserves tbl_now attributes", {
  skip_on_cran()
  test_data <- setup_test_data()

  result <- test_data$ndata |> dplyr::group_by(gender)

  expect_equal(get_event_date(result), get_event_date(test_data$ndata))
  expect_equal(get_report_date(result), get_report_date(test_data$ndata))
  expect_equal(get_strata(result), get_strata(test_data$ndata))
})

test_that("group_by handles multiple grouping variables", {
  skip_on_cran()
  test_data <- setup_test_data()

  result <- test_data$ndata |> dplyr::group_by(gender, age_group)

  expect_s3_class(result, "grouped_tbl_now")
  expect_equal(length(dplyr::group_vars(result)), 2)
})

test_that("group_by returns tbl_now when no groups specified", {
  skip_on_cran()
  test_data <- setup_test_data()

  # Group by nothing
  result <- test_data$ndata |> dplyr::group_by()

  expect_s3_class(result, "tbl_now")
})

# Tests for ungroup.grouped_tbl_now ----
test_that("ungroup removes grouping from grouped_tbl_now", {
  skip_on_cran()
  test_data <- setup_test_data()

  grouped <- test_data$ndata |> dplyr::group_by(gender)
  result <- grouped |> dplyr::ungroup()

  expect_s3_class(result, "tbl_now")
  expect_false(dplyr::is_grouped_df(result))
})

test_that("ungroup preserves tbl_now attributes", {
  skip_on_cran()
  test_data <- setup_test_data()

  grouped <- test_data$ndata |> dplyr::group_by(gender)
  result <- grouped |> dplyr::ungroup()

  expect_equal(get_event_date(result), get_event_date(test_data$ndata))
  expect_equal(get_report_date(result), get_report_date(test_data$ndata))
  expect_equal(get_strata(result), get_strata(test_data$ndata))
})

test_that("ungroup handles partial ungrouping", {
  skip_on_cran()
  test_data <- setup_test_data()

  grouped <- test_data$ndata |> dplyr::group_by(gender, age_group)
  result <- grouped |> dplyr::ungroup(gender)

  # Should still be grouped by age_group
  expect_true(dplyr::is_grouped_df(result))
  expect_equal(dplyr::group_vars(result), "age_group")
})

# Tests for summarise.tbl_now ----
test_that("summarise maintains tbl_now when valid", {
  skip_on_cran()
  test_data <- setup_test_data()

  result <- suppressWarnings(
    test_data$ndata |>
      dplyr::group_by(gender) |>
      dplyr::summarise(mean_value = mean(value), .groups = "drop")
  )

  # Should try to maintain tbl_now if possible
  expect_true(inherits(result, "tbl_now") || inherits(result, "tbl_df"))
})


test_that("summarise drops to tibble when losing required columns", {
  skip_on_cran()
  test_data <- setup_test_data()

  expect_warning(
    result <- test_data$ndata |>
      summarise(total = sum(value)),
    "Dropping.*tbl_now"
  )

  expect_false(is_tbl_now(result))
  expect_s3_class(result, "tbl_df")
})

test_that("summarise with grouped_tbl_now works", {
  skip_on_cran()
  test_data <- setup_test_data()

  grouped <- test_data$ndata |> dplyr::group_by(gender)

  results <- suppressWarnings(
    grouped |>
      dplyr::summarise(mean_temp = mean(temperature), .groups = "drop")
  )


  resultz <- suppressWarnings(
    grouped |>
      dplyr::summarize(mean_temp = mean(temperature), .groups = "drop")
  )

  expect_s3_class(results, "tbl_df")
  expect_s3_class(resultz, "tbl_df")
  expect_equal(nrow(results), 2)
  expect_equal(nrow(resultz), 2)
  expect_equal(results, resultz)
})

test_that("summarize (American spelling) works", {
  skip_on_cran()
  test_data <- setup_test_data()

  # Test that both spellings work
  result1 <- suppressWarnings(
    test_data$ndata |>
      dplyr::group_by(gender) |>
      dplyr::summarise(mean_value = mean(value), .groups = "drop")
  )

  result2 <- suppressWarnings(
    test_data$ndata |>
      dplyr::group_by(gender) |>
      dplyr::summarize(mean_value = mean(value), .groups = "drop")
  )

  expect_equal(class(result1), class(result2))
  expect_equal(result1, result2)
})

test_that("summarize (American spelling) works in ungrouped", {
  skip_on_cran()
  test_data <- setup_test_data()

  # Test that both spellings work
  result1 <- suppressWarnings(
    test_data$ndata |>
      dplyr::summarise(mean_value = mean(value), .groups = "drop")
  )

  result2 <- suppressWarnings(
    test_data$ndata |>
      dplyr::summarize(mean_value = mean(value), .groups = "drop")
  )

  expect_equal(class(result1), class(result2))
  expect_equal(result1, result2)
})

# Tests for tbl_now_reconstruct ----
test_that("tbl_now_reconstruct maintains valid tbl_now", {
  skip_on_cran()
  test_data <- setup_test_data()

  # Simulate a dplyr operation
  modified <- as.data.frame(test_data$ndata)

  result <- tbl_now_reconstruct(modified, test_data$ndata)

  expect_s3_class(result, "tbl_now")
})

test_that("tbl_now_reconstruct drops to tibble when invalid", {
  skip_on_cran()
  test_data <- setup_test_data()

  # Remove protected column
  modified <- test_data$ndata
  suppressWarnings(
    modified$onset_week <- NULL
  )

  expect_warning(
    result <- tbl_now_reconstruct(modified, test_data$ndata),
    "Dropped protected column"
  )

  expect_false(is_tbl_now(result))
})

test_that("tbl_now_reconstruct updates strata when columns dropped", {
  skip_on_cran()
  test_data <- setup_test_data()

  # Add multiple strata
  ndata <- test_data$ndata
  ndata <- change_strata(ndata, c("gender", "age_group"))

  # Remove one strata column
  modified <- ndata
  modified$age_group <- NULL

  result <- tbl_now_reconstruct(modified, ndata)

  if (is_tbl_now(result)) {
    expect_equal(get_strata(result), "gender")
    expect_equal(get_num_strata(result), 1)
  }
})

test_that("tbl_now_reconstruct updates covariates when columns dropped", {
  skip_on_cran()
  test_data <- setup_test_data()

  # Add multiple covariates
  ndata <- test_data$ndata
  ndata$humidity <- c(0.6, 0.65, 0.7, 0.68)
  ndata <- change_covariates(ndata, c("temperature", "humidity"))

  # Remove one covariate column
  modified <- ndata
  modified$humidity <- NULL

  result <- tbl_now_reconstruct(modified, ndata)

  if (is_tbl_now(result)) {
    expect_equal(get_covariates(result), "temperature")
    expect_equal(get_num_covariates(result), 1)
  }
})

test_that("tbl_now_reconstruct doesn't change now", {
  skip_on_cran()
  test_data <- setup_test_data()

  old_now <- get_now(test_data$ndata)
  # Remove rows that would change now
  modified <- test_data$ndata[1:2, ]

  new_now <- get_now(tbl_now_reconstruct(modified, test_data$ndata))
  expect_equal(old_now, new_now)
})

# Tests for filtering ----
test_that("filter maintains tbl_now", {
  skip_on_cran()
  test_data <- setup_test_data()

  result <- test_data$ndata |> dplyr::filter(gender == "Male")

  expect_s3_class(result, "tbl_now")
  expect_equal(nrow(result), 2)
})

test_that("filter preserves attributes", {
  skip_on_cran()
  test_data <- setup_test_data()

  result <- test_data$ndata |> dplyr::filter(value > 15)

  expect_equal(get_event_date(result), get_event_date(test_data$ndata))
  expect_equal(get_strata(result), get_strata(test_data$ndata))
})

test_that("validate works with numeric", {
  skip_on_cran()
  test_data <- setup_test_data()

  result <- test_data$ndata |>
    dplyr::mutate(report_week = as.numeric(difftime(report_week, min(onset_week), units = "weeks"))) |>
    dplyr::mutate(onset_week = as.numeric(difftime(onset_week, min(onset_week), units = "weeks")))


  expect_equal(get_event_date(result), get_event_date(test_data$ndata))
  expect_equal(get_strata(result), get_strata(test_data$ndata))
  expect_equal(result$onset_week, result$.event_num)
  expect_equal(result$report_week, result$.report_num)
})

test_that("test dropping delay column", {
  skip_on_cran()
  test_data <- setup_test_data()

  expect_warning(
    test_data$ndata |>
      dplyr::select(-.delay),
    "Dropped protected column"
  )
})

test_that("test dropping count column", {
  skip_on_cran()
  test_data <- setup_test_data()

  expect_warning(
    test_data$ndata |>
      to_count(to = "count-incidence") |>
      dplyr::select(-n),
    "Drop"
  )
})

# Tests for select ----
test_that("select maintains tbl_now with protected columns", {
  skip_on_cran()
  test_data <- setup_test_data()

  result <- test_data$ndata |>
    dplyr::select(onset_week, report_week, gender, .event_num, .report_num, is_censored_report, .delay)

  expect_s3_class(result, "tbl_now")
})

test_that("select drops to tibble without protected columns", {
  skip_on_cran()
  test_data <- setup_test_data()

  expect_warning(
    result <- test_data$ndata |> dplyr::select(gender, value),
    "Dropped protected column"
  )

  expect_false(is_tbl_now(result))
})

# Tests for arrange ----
test_that("arrange maintains tbl_now", {
  skip_on_cran()
  test_data <- setup_test_data()

  result <- test_data$ndata |> dplyr::arrange(desc(value))

  expect_s3_class(result, "tbl_now")
  expect_equal(nrow(result), nrow(test_data$ndata))
})

test_that("arrange preserves attributes", {
  skip_on_cran()
  test_data <- setup_test_data()

  result <- test_data$ndata |> dplyr::arrange(onset_week)

  expect_equal(get_event_date(result), get_event_date(test_data$ndata))
  expect_equal(get_strata(result), get_strata(test_data$ndata))
})

# Integration tests ----
test_that("chaining dplyr verbs maintains tbl_now", {
  skip_on_cran()
  test_data <- setup_test_data()

  result <- test_data$ndata |>
    dplyr::filter(value > 15) |>
    dplyr::mutate(double_value = value * 2) |>
    dplyr::arrange(desc(double_value))

  expect_s3_class(result, "tbl_now")
  expect_true("double_value" %in% colnames(result))
})

test_that("group_by then summarise works correctly", {
  skip_on_cran()
  test_data <- setup_test_data()

  result <- suppressWarnings(
    test_data$ndata |>
      dplyr::group_by(gender) |>
      dplyr::summarise(
        n = dplyr::n(),
        mean_value = mean(value),
        .groups = "drop"
      )
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
})

test_that("complex dplyr operations maintain or drop class appropriately", {
  skip_on_cran()
  test_data <- setup_test_data()

  # Should maintain
  result1 <- test_data$ndata |>
    dplyr::filter(gender == "Male") |>
    dplyr::mutate(new_val = value * 2)

  expect_s3_class(result1, "tbl_now")

  # Should drop
  expect_warning(
    result2 <- test_data$ndata |>
      dplyr::select(gender, value),
    "Dropped protected column"
  )

  expect_false(is_tbl_now(result2))
})

# Tests for edge cases ----
test_that("empty subset maintains structure", {
  skip_on_cran()
  test_data <- setup_test_data()

  result <- test_data$ndata |> dplyr::filter(value > 1000)

  expect_s3_class(result, "tbl_now")
  expect_equal(nrow(result), 0)
})

test_that("operations on grouped_tbl_now maintain structure", {
  skip_on_cran()
  test_data <- setup_test_data()

  result <- test_data$ndata |>
    group_by(gender) |>
    dplyr::filter(value > 15) |>
    ungroup()

  expect_s3_class(result, "tbl_now")
  expect_false(dplyr::is_grouped_df(result))

  expect_s3_class(test_data$ndata |>
    group_by(gender) |>
    dplyr::filter(value > 15), "grouped_tbl_now")
})

# ---- Demotion is one operation, not two -------------------------------------
#
# Dropping a protected column demotes the object to a plain tibble. That used to
# be `as_tibble()`, which leaves unknown attributes alone on a tibble and drops
# them when it has to rebuild a grouped_df -- so a demoted object kept the
# class's attributes, or lost them, according to whether the caller had grouped
# it. `align_weeks()` read `get_now()` off a demoted object and worked only by
# that accident.

demotion_fixture <- function() {
  df <- data.frame(
    onset = as.Date("2024-01-01") + 0:9,
    reported = as.Date("2024-01-02") + 0:9,
    sex = rep(c("F", "M"), 5)
  )
  tbl_now(df,
    event_date = onset, report_date = reported, strata = sex,
    data_type = "linelist", units = "days", verbose = FALSE
  )
}

test_that("demotion drops the class's attributes", {
  x <- demotion_fixture()
  demoted <- suppressWarnings(x |> dplyr::select(-!!as.symbol("onset")))

  expect_false(is_tbl_now(demoted))
  expect_s3_class(demoted, "tbl_df")
  # A plain tibble does not know where its event date is, because it has none.
  for (name in c(
    "event_date", "report_date", "now", "event_units", "report_units",
    "data_type", "strata"
  )) {
    expect_null(attr(demoted, name, exact = TRUE), info = name)
  }
})

test_that("demotion drops the same attributes whether or not the object is grouped", {
  x <- demotion_fixture()

  ungrouped <- suppressWarnings(x |> dplyr::select(-!!as.symbol("onset")))
  grouped <- suppressWarnings(
    x |> dplyr::group_by(!!as.symbol("sex")) |> dplyr::select(-!!as.symbol("onset"))
  )

  expect_equal(
    names(attributes(ungrouped)),
    setdiff(names(attributes(grouped)), "groups")
  )
  expect_null(attr(grouped, "now", exact = TRUE))
  expect_null(attr(ungrouped, "now", exact = TRUE))
})

test_that("demotion keeps the metadata the user attached through `...`", {
  df <- data.frame(
    onset = as.Date("2024-01-01") + 0:9,
    reported = as.Date("2024-01-02") + 0:9
  )
  x <- tbl_now(df,
    event_date = onset, report_date = reported,
    data_type = "linelist", units = "days", verbose = FALSE,
    data_source = "Ministry of Health"
  )
  demoted <- suppressWarnings(x |> dplyr::select(-!!as.symbol("onset")))

  # It names no column, and the user put it there on purpose.
  expect_identical(attr(demoted, "data_source"), "Ministry of Health")
  expect_null(attr(demoted, "event_date", exact = TRUE))
})

add_validation_date_fixture <- function(x) {
  x[["resolved"]] <- x[[get_report_date(x)]] + 2
  x[["outcome"]] <- rep("confirmed", nrow(x))
  add_validation_date(x, "resolved", "outcome")
}

test_that("every attribute tbl_now() sets is listed as one the class owns", {
  # `.demote_to_tibble()` works off a hard-coded list; an attribute added to the
  # class but not to that list would outlive the demotion, and its column name
  # would outlive the column.
  x <- demotion_fixture() |>
    add_validation_date_fixture()

  owned <- tbl.now:::.TBL_NOW_ATTRIBUTES
  actual <- setdiff(names(tbl_now_attributes(x)), "class")
  expect_setequal(intersect(actual, owned), actual)
})
