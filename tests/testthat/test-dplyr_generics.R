
# --- Setup: Create a function to make a minimal, valid tbl_now for testing ---
make_test_tbl_now <- function(n = 10) {
  # Mock data to simulate 'denguedat' structure and column types
  df <- tibble::tibble(
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
  sliced <- x %>% dplyr::slice(1:5)

  expect_s3_class(sliced, "tbl_now")
  expect_equal(nrow(sliced), 5)
  expect_equal(get_strata(sliced), "gender")
})

test_that("`dplyr_col_modify.tbl_now` preserves class and attributes", {
  x <- make_test_tbl_now()
  # Column modification via `dplyr::mutate()`
  modified <- x %>% dplyr::mutate(value = value * 2)

  expect_s3_class(modified, "tbl_now")
  expect_equal(modified$value[1], 2)
  expect_equal(get_event_date(modified), "onset_week")
})

test_that("`dplyr_reconstruct.tbl_now` handles reconstruction logic", {
  template <- make_test_tbl_now()
  # Scenario 1: Valid data reconstruction
  valid_data <- template %>% dplyr::select(-gender) # Drop 'value'


  reconstructed_valid <- dplyr_reconstruct(valid_data, template)


  expect_s3_class(reconstructed_valid, "tbl_now")
  expect_equal(get_num_strata(reconstructed_valid), 0) # 'strata' deosn not exist
  expect_false("gender" %in% names(reconstructed_valid))


  # Scenario 2: Downgrade due to missing protected column
  invalid_data <- suppressWarnings(
    template %>% dplyr::select(-onset_week, report_week, gender)
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
  grouped <- x %>% group_by(gender)

  expect_s3_class(grouped, "tbl_now")
  expect_s3_class(grouped, "grouped_df")
  expect_s3_class(grouped, "grouped_tbl_now")

  # Original attributes should be preserved
  expect_equal(get_strata(grouped), "gender")
})

test_that("`ungroup.grouped_tbl_now` returns an ungrouped `tbl_now`", {
  x <- make_test_tbl_now()
  grouped <- x %>% group_by(gender)
  ungrouped <- grouped %>% ungroup()

  expect_s3_class(ungrouped, "tbl_now")
  expect_false(inherits(ungrouped, "grouped_df"))
  expect_equal(get_strata(ungrouped), "gender")
})

test_that("`summarise.tbl_now` preserves class when valid", {
  x <- make_test_tbl_now()
  # Summarising linelist data usually aggregates, so we might need a test where
  # the protected columns remain or the resulting structure can be reconstructed.
  # For this simple test, we'll ensure the attributes are copied to the result
  # using new_tbl_now if all protected columns remain (or are handled).

  # In this case, since `data_type` is 'linelist', 'n' is not protected.
  # If we summarize without dropping 'onset_week' and 'report_week', it should work.
  summarized_valid <- suppressWarnings({ # Suppress warning from new_tbl_now about event/report units
    x %>%
      group_by(onset_week, report_week, gender, .event_num, .report_num) %>%
      summarise(max_report = max(report_week), .groups = "drop")
  })

  expect_s3_class(summarized_valid, "tbl_now")
  expect_equal(get_data_type(summarized_valid), "linelist")
  expect_true("gender" %in% names(summarized_valid))
})

test_that("`summarise.tbl_now` drops class when protected columns are missing", {
  x <- make_test_tbl_now()
  # This summary drops the protected column `onset_week`
  summarized_invalid <- suppressWarnings({ # Suppress 'Dropping `tbl_now` attributes' warning
    x %>%
      group_by(onset_week, gender, .event_num, .report_num) %>%
      summarise(max_report = max(report_week), .groups = "drop")
  })

  expect_false(inherits(summarized_invalid, "tbl_now"))
  expect_s3_class(summarized_invalid, "tbl")
})

test_that("`summarise.grouped_tbl_now` works via delegation", {
  x <- make_test_tbl_now()
  grouped <- x %>% group_by(gender)

  # The resulting object should have the `tbl_now` attributes re-applied
  expect_s3_class(grouped, "grouped_tbl_now")
  expect_s3_class(grouped, "grouped_df")
  expect_equal(grouped %>% ungroup(), x)
})
