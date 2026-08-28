# Load libraries (assuming the code depends on these)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)

# --- Test Data Setup ---
# Linelist data (data_type should be "linelist")
ll_data <- tibble(
  onset_week = as.Date(c("2023-01-01", "2023-01-02", "2023-01-01", "2023-01-03")),
  report_week = as.Date(c("2023-01-03", "2023-01-03", "2023-01-03", "2023-01-05")),
  gender = c("M", "F", "M", "F"),
  age_group = c("A", "B", "A", "B"),
  is_censored_col = c(FALSE, FALSE, TRUE, FALSE)
)

# Count data (data_type should be "count" because of the 'n' column)
count_data <- ll_data |>
  group_by(onset_week, report_week, gender, age_group) |>
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
  expect_equal(attr(result, "now"), expected_now) # Should infer max report date
  expect_equal(attr(result, "strata"), NULL) # Should be empty
  expect_equal(attr(result, "covariates"), c("age_group", "gender"))
  expect_equal(attr(result, "data_type"), "linelist") # Should infer linelist
  expect_equal(attr(result, "report_units"), "days") # Should infer "day" for Date objects
  expect_equal(attr(result, "event_units"), "days") # Should infer "day" for Date objects

  expect_equal(attr(result, "event_date"), get_event_date(result))
  expect_equal(attr(result, "report_date"), get_report_date(result))
  expect_equal(attr(result, "now"), get_now(result))
  expect_equal(attr(result, "strata"), get_strata(result))
  expect_equal(length(get_strata(result)), get_num_strata(result))
  expect_equal(attr(result, "covariates"), get_covariates(result))
  expect_equal(length(get_covariates(result)), get_num_covariates(result))
  expect_equal(attr(result, "data_type"), get_data_type(result))
  expect_equal(attr(result, "report_units"), get_report_units(result))
  expect_equal(attr(result, "event_units"), get_event_units(result))
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
  expect_equal(attr(result, "covariates"), NULL)
})

test_that("tbl_now infers 'count' data_type correctly", {
  # Data with a column named 'n'
  result <- tbl_now(
    data = count_data,
    event_date = "onset_week",
    report_date = "report_week",
    date_units = "days",
    case_count = n,
    verbose = FALSE
  )

  expect_equal(attr(result, "data_type"), "count-cumulative")
})

test_that("tbl_now handles optional 'is_censored' column", {
  result <- tbl_now(
    data = ll_data,
    event_date = "onset_week",
    report_date = "report_week",
    is_censored = "is_censored_col",
    date_units = "days",
    verbose = FALSE
  )
  expect_equal(attr(result, "is_censored"), "is_censored_col")
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
    "doesn't exist|not found"
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
    "doesn't exist|not found"
  )

  # Error if event_date > report_date is violated (based on check.R logic)
  invalid_data <- ll_data |>
    mutate(
      onset_week = as.Date("2023-01-10"),
      report_week = as.Date("2023-01-01")
    )
  suppressWarnings(
    expect_warning(
      tbl_now(
        data = invalid_data,
        event_date = "onset_week",
        report_date = "report_week",
        report_units = "days",
        event_units = "days",
        verbose = FALSE
      ),
      "before"
    )
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
    "Can't select columns|doesn't exist|not found"
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
    "Can't select columns|doesn't exist|not found"
  )
})

test_that("tbl_now throws warning when repeated rows", {
  data("flusight")

  # Three locations, not all 53. This test is about `tbl_now()`'s response to
  # repeated rows, and one location shows that as well as fifty-three do -- but
  # the full `flusight` is 452,000 rows and every one of them is validated
  # twice below, which made this single test 4% of the whole CRAN suite. Both
  # warnings asserted below still fire on the subset; that was checked, not
  # assumed.
  flusight <- flusight |>
    dplyr::filter(!is.na(observation)) |>
    dplyr::filter(location_name %in% sort(unique(location_name))[1:3]) |>
    dplyr::mutate(epiweek_as_of = lubridate::epiweek(as_of)) |>
    dplyr::mutate(epiyear_as_of = lubridate::epiyear(as_of)) |>
    dplyr::left_join(
      dplyr::tibble(report_date = seq(min(flusight$as_of), max(flusight$as_of) + lubridate::days(7), by = "1 day")) |>
        dplyr::mutate(epiweek_as_of = lubridate::epiweek(report_date)) |>
        dplyr::mutate(epiyear_as_of = lubridate::epiyear(report_date)) |>
        dplyr::mutate(day = lubridate::wday(report_date)) |>
        dplyr::filter(day == 7),
      by = dplyr::join_by(epiweek_as_of, epiyear_as_of)
    ) |>
    dplyr::select(-epiweek_as_of, -epiyear_as_of, -day)

  # `flusight` used to ship exact duplicate rows and this test relied on them.
  # They were removed in issue #25, so the repeated event-report combinations
  # are now created explicitly. This test is about how `tbl_now()` handles
  # repeated rows, not about the dataset happening to contain them -- do not
  # re-couple it to the data.
  flusight <- dplyr::bind_rows(flusight, dplyr::slice_head(flusight, n = 500))

  expect_warning(
    tbl_now(flusight,
      event_date = "target_end_date",
      report_date = "report_date",
      strata = "location_name",
      case_count = "observation",
      data_type = "count-cumulative",
      verbose = FALSE
    ),
    # Reworded in 0.19.0: the warning now leads with how many rows collide and
    # goes on to name the cause. These are genuine duplicate rows (sliced from
    # the head), not an undeclared column, so it must recommend `distinct()`.
    "Non-unique"
  )


  suppressWarnings(
    expect_warning(
      tbl_now(flusight,
        event_date = "target_end_date",
        report_date = "report_date",
        strata = "location_name",
        case_count = "observation",
        verbose = FALSE
      ),
      "Cannot accurately infer the data-type"
    )
  )
})

test_that("tbl_now correctly identifies data type", {
  # lINELIST
  df1 <- data.frame(
    patient = 1:6,
    event_date = c(
      rep(as.Date("2020/09/12"), 3),
      rep(as.Date("2020/09/13"), 3)
    ),
    report_date = c(
      as.Date("2020/09/12"),
      as.Date("2020/09/13"),
      as.Date("2020/09/14"),
      as.Date("2020/09/13"),
      as.Date("2020/09/14"),
      as.Date("2020/09/15")
    )
  )

  dtbl1 <- tbl_now(df1, event_date = "event_date", report_date = "report_date", verbose = FALSE)
  expect_equal(get_data_type(dtbl1), "linelist")

  # COUNT INCIDENCE
  df2 <- data.frame(
    n = c(7, 1, 9, 5, 0, 2),
    event_date = c(
      rep(as.Date("2020/09/12"), 3),
      rep(as.Date("2020/09/13"), 3)
    ),
    report_date = c(
      as.Date("2020/09/12"),
      as.Date("2020/09/13"),
      as.Date("2020/09/14"),
      as.Date("2020/09/13"),
      as.Date("2020/09/14"),
      as.Date("2020/09/15")
    )
  )

  dtbl2 <- tbl_now(df2, event_date = "event_date", report_date = "report_date", case_count = "n", verbose = FALSE)
  expect_equal(get_data_type(dtbl2), "count-incidence")

  # COUNT CUMULATIVE
  df3 <- data.frame(
    n = c(1, 5, 8, 2, 2, 4),
    event_date = c(
      rep(as.Date("2020/09/12"), 3),
      rep(as.Date("2020/09/13"), 3)
    ),
    report_date = c(
      as.Date("2020/09/12"),
      as.Date("2020/09/13"),
      as.Date("2020/09/14"),
      as.Date("2020/09/13"),
      as.Date("2020/09/14"),
      as.Date("2020/09/15")
    )
  )

  dtbl3 <- tbl_now(df3,
    event_date = "event_date", report_date = "report_date",
    case_count = "n", verbose = FALSE
  )
  expect_equal(get_data_type(dtbl3), "count-cumulative")
})

test_that("tbl_now fails when strata/covariate have repeated variables", {
  data(denguedat)

  expect_error(
    tbl_now(denguedat,
      event_date = onset_week,
      report_date = report_week,
      strata = gender,
      covariates = gender,
      verbose = FALSE
    ),
    "Strata .* covariate"
  )

  expect_error(
    tbl_now(denguedat,
      event_date = onset_week,
      report_date = report_week,
      strata = gender,
      covariates = onset_week,
      verbose = FALSE
    ),
    "Event .* covariate"
  )

  expect_error(
    tbl_now(denguedat,
      event_date = onset_week,
      report_date = report_week,
      strata = onset_week,
      covariates = gender,
      verbose = FALSE
    ),
    "Event .* strata"
  )

  expect_error(
    tbl_now(denguedat,
      event_date = onset_week,
      report_date = report_week,
      strata = gender,
      covariates = report_week,
      verbose = FALSE
    ),
    "Report .* covariate"
  )

  expect_error(
    tbl_now(denguedat,
      event_date = onset_week,
      report_date = report_week,
      strata = report_week,
      covariates = gender,
      verbose = FALSE
    ),
    "Report .* strata"
  )
})
