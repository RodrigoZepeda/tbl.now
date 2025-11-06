# --- Test Data Setup ---
# Simple data to create a tbl_now object
library(dplyr)

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
  expect_true(any(grepl("tbl_now", names(output))))
  expect_true(any(grepl("linelist \\(frequency = days\\)", output)))
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
  expect_true(any(grepl("event_date_col", output[3]))) # Check if it's in the header line (line 1 is the header for tibbles)
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
