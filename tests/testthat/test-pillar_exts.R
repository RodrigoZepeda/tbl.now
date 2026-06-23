# --- Test Data Setup ---
# Simple data to create a tbl_now object
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)

ll_data <- tibble(
  event_date_col = as.Date(c("2023-01-01", "2023-01-02")),
  report_date_col = as.Date(c("2023-01-03", "2023-01-04")),
  strata_col = c("A", "B"),
  cov_col = c(1, 2)
)


# === TEST SUITE FOR PILLAR EXTENSIONS (Pretty Printing) ===

test_that("tbl_sum.tbl_now shows correct class and frequency", {
  result <- tbl_now(
    data = ll_data,
    event_date = "event_date_col",
    report_date = "report_date_col",
    strata = "strata_col",
    covariate = "cov_col",
    date_units = "days",
    verbose = FALSE
  )

  # Check that the output summary includes the custom line
  output <- format(tbl_sum(result))

  # Expected format: "tbl_now (frequency = day)"
  expect_true(any(grepl("Data type", names(output))))
  expect_true(any(grepl("Frequency", names(output))))
  expect_true(any(grepl("linelist", output)))
  expect_true(any(grepl("Event: `days`", output)))
  expect_true(any(grepl("Report: `days`", output)))
})

test_that("tbl_format_footer.tbl_now displays mandatory attributes", {
  result <- tbl_now(
    data = ll_data,
    event_date = "event_date_col",
    report_date = "report_date_col",
    strata = "strata_col",
    covariate = "cov_col",
    date_units = "days",
    verbose = FALSE
  )

  # Check that the footer contains the mandatory attributes
  # The output is formatted with cli::cli_fmt, so we must check for the content.
  # We test the raw output (not formatted with ansi codes)
  output <- capture.output(print(result))

  # Check for Now, Event Date, and Report Date in the output string
  expect_true(any(grepl("Now: 2023-01-04", output)))
  expect_true(any(grepl("Event date: \"event_date_col\"", output)))
  expect_true(any(grepl("Report date: \"report_date_col\"", output)))

  # Check strata and covariates
  expect_true(any(grepl("Strata: \"strata_col\"", output)))
  expect_true(any(grepl("Covariates: \"cov_col\"", output)))
})

test_that("tbl_format_footer.tbl_now displays Strata and Covariates when present", {
  result <- tbl_now(
    data = ll_data,
    event_date = "event_date_col",
    report_date = "report_date_col",
    date_units = "days",
    verbose = FALSE
  )

  output <- capture.output(print(result))

  expect_false(any(grepl("Strata", output)))
  expect_false(any(grepl("Covariates", output)))
})

test_that("ctl_new_pillar.tbl_now annotates event and report date columns", {
  result <- tbl_now(
    data = ll_data,
    event_date = "event_date_col",
    report_date = "report_date_col",
    strata = "strata_col",
    covariate = "cov_col",
    date_units = "days",
    verbose = FALSE
  )

  # The tibble needs to be printed to trigger ctl_new_pillar/format
  # We suppress console output and capture the text representation
  output <- capture.output(print(result))

  # 1. The annotation for event_date
  # We expect "event_date_col [event_date]" to appear in the header area.
  expect_true(any(grepl("event_date_col \\<", output))) # Check column name is present
  expect_true(any(grepl("event_date_col", output[4]))) # Check if it's in the header line (line 1 is the header for tibbles)
  expect_true(any(grepl("\\[event_date\\]", output))) # Check annotation is present

  # 2. The annotation for report_date
  # We expect "report_date_col [report_date]" to appear
  expect_true(any(grepl("report_date_col \\<", output)))
  expect_true(any(grepl("\\[report_date\\]", output)))

  # 3. Non-annotated columns
  # We expect strata_col to *not* have a custom annotation (like [event_date])
  expect_false(any(grepl("strata_col \\[", output)))
  expect_false(any(grepl("cov_col \\[", output)))
})

# === Temporal-effects footer + annotations =================================

# A small daily tbl_now for temporal-effect printing
daily_te <- tibble(
  event   = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04")),
  report  = as.Date(c("2023-01-03", "2023-01-03", "2023-01-05", "2023-01-06"))
)

test_that("footer shows a LAZY temporal-effects spec with every effect kind", {
  skip_if_not_installed("almanac")

  spec_event <- temporal_effects(
    day_of_week = TRUE, weekend = TRUE, day_of_month = TRUE,
    month_of_year = TRUE, week_of_year = TRUE, seasons = c(7, 52),
    holidays = almanac::rcalendar(almanac::hol_christmas())
  )

  x <- tbl_now(daily_te,
    event_date = event, report_date = report,
    event_units = "days", report_units = "days", verbose = FALSE
  ) |>
    add_temporal_effects(spec_event, date_type = "event_date") |>
    add_temporal_effects(temporal_effects(weekend = TRUE), date_type = "report_date")

  out <- capture.output(print(x))

  expect_true(any(grepl("T\\. effects \\(lazy\\)", out))) # lazy branch
  expect_true(any(grepl("day_of_week", out)))
  expect_true(any(grepl("weekend", out)))
  expect_true(any(grepl("day_of_month", out)))
  expect_true(any(grepl("month_of_year", out)))
  expect_true(any(grepl("week_of_year", out)))
  expect_true(any(grepl("season\\(7,52\\)", out))) # seasons branch
  expect_true(any(grepl("holidays", out))) # holidays branch
  expect_true(any(grepl("\\[event_date\\]", out))) # event_date label
  expect_true(any(grepl("\\[report_date\\]", out))) # report_date label
})

test_that("footer shows COMPUTED temporal effects and pillars get [t_effect]", {
  x <- tbl_now(daily_te,
    event_date = event, report_date = report,
    event_units = "days", report_units = "days", verbose = FALSE
  ) |>
    add_temporal_effects(temporal_effects(day_of_week = TRUE)) |>
    compute_temporal_effects()

  out <- capture.output(print(x))

  expect_true(any(grepl("T\\. effects:", out))) # computed footer branch
  expect_true(any(grepl("T\\. effect cols:", out))) # computed column listing
  expect_true(any(grepl("\\[t_effect\\]", out))) # the [t_effect] pillar annotation
})

test_that(".format_temporal_effects_spec returns NULL for an empty spec list", {
  expect_null(tbl.now:::.format_temporal_effects_spec(list()))
  expect_null(tbl.now:::.format_temporal_effects_spec(NULL))
})

test_that("footer shows the right-censored indicator when set", {
  x <- tbl_now(
    daily_te |> mutate(flag = c(FALSE, TRUE, FALSE, FALSE)),
    event_date = event, report_date = report, is_censored = flag,
    event_units = "days", report_units = "days", verbose = FALSE
  )
  out <- capture.output(print(x))
  expect_true(any(grepl("Right-censored indicator: \"flag\"", out)))
})
