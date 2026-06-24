library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(lubridate, quietly = TRUE, warn.conflicts = FALSE)

dtf <- expand.grid(
  event = seq(ymd("2010/01/01"), ymd("2021/01/01"), by = "1 week"),
  delay = 1:10,
  sex = c("M", "F"),
  region = c("North", "East", "West", "South"),
  censored = c(T, F)
) |>
  mutate(report_date = event + weeks(delay)) |>
  rowwise() |>
  mutate(cases = rpois(n(), 22)) |>
  ungroup()

dfnow <- tbl_now(dtf,
  event_date = "event", report_date = "report_date",
  strata = c("sex", "region"), is_censored = "censored",
  case_count = "cases", verbose = FALSE
)


test_that("It doesn't loose class after two applications", {
  expect_identical(
    dfnow |> filter(region == "North") |> class(),
    c("tbl_now", "tbl_df", "tbl", "data.frame")
  )


  expect_true(
    dfnow |> filter(region == "North") |> filter(censored) |> is_tbl_now()
  )

  # Filtering and then binding should have same
  expect_identical(
    capture.output(print(dfnow |>
      filter(region == "Does not exist") |>
      bind_rows(dfnow))),
    capture.output(print(dfnow))
  )
})

test_that("tbl_format_footer.tbl_now displays mandatory attributes when mutating / filtering / grouping / etc", {
  # Test the original data
  output <- capture.output(print(dfnow))

  # Check for Now, Event Date, and Report Date in the output string
  expect_true(any(grepl(paste0("Now: ", get_now(dfnow)), output)))
  expect_true(any(grepl("Event date: \"event\"", output)))
  expect_true(any(grepl("Report date: \"report_date\"", output)))
  expect_true(any(grepl("Strata: \"sex\" and \"region\"", output)))

  # Now apply filter and test after one filter
  result <- dfnow |> filter(sex == "M")

  output <- capture.output(print(result))

  # Check for Now, Event Date, and Report Date in the output string
  expect_true(any(grepl(paste0("Now: ", get_now(dfnow)), output)))
  expect_true(any(grepl("Event date: \"event\"", output)))
  expect_true(any(grepl("Report date: \"report_date\"", output)))
  expect_true(any(grepl("Strata: \"sex\" and \"region\"", output)))

  # Now apply filter and test after more than one filter
  result <- dfnow |>
    filter(sex == "M") |>
    filter(region == "North")

  output <- capture.output(print(result))

  # Check for Now, Event Date, and Report Date in the output string
  expect_true(any(grepl(paste0("Now: ", get_now(dfnow)), output)))
  expect_true(any(grepl("Event date: \"event\"", output)))
  expect_true(any(grepl("Report date: \"report_date\"", output)))
  expect_true(any(grepl("Strata: \"sex\" and \"region\"", output)))

  # Now apply mutate and test after mutate
  result <- dfnow |> mutate(new_var = 2)

  output <- capture.output(print(result))

  # Check for Now, Event Date, and Report Date in the output string
  expect_true(any(grepl(paste0("Now: ", get_now(dfnow)), output)))
  expect_true(any(grepl("Event date: \"event\"", output)))
  expect_true(any(grepl("Report date: \"report_date\"", output)))
  expect_true(any(grepl("Strata: \"sex\" and \"region\"", output)))

  # Now apply mutate and test after two mutates
  result <- dfnow |>
    mutate(new_var = 2) |>
    mutate(other_var = 3)

  output <- capture.output(print(result))

  # Check for Now, Event Date, and Report Date in the output string
  expect_true(any(grepl(paste0("Now: ", get_now(dfnow)), output)))
  expect_true(any(grepl("Event date: \"event\"", output)))
  expect_true(any(grepl("Report date: \"report_date\"", output)))
  expect_true(any(grepl("Strata: \"sex\" and \"region\"", output)))

  # Now apply mutate and test after grouping
  result <- dfnow |> group_by(region)

  output <- capture.output(print(result))

  # Check for Now, Event Date, and Report Date in the output string
  expect_true(any(grepl(paste0("Now: ", get_now(dfnow)), output)))
  expect_true(any(grepl("Event date: \"event\"", output)))
  expect_true(any(grepl("Report date: \"report_date\"", output)))
  expect_true(any(grepl("Strata: \"sex\" and \"region\"", output)))

  # Now apply mutate and test after grouping twice
  result <- dfnow |>
    group_by(region) |>
    group_by(sex, .add = TRUE)

  output <- capture.output(print(result))

  # Check for Now, Event Date, and Report Date in the output string
  expect_true(any(grepl(paste0("Now: ", get_now(dfnow)), output)))
  expect_true(any(grepl("Event date: \"event\"", output)))
  expect_true(any(grepl("Report date: \"report_date\"", output)))
  expect_true(any(grepl("Strata: \"sex\" and \"region\"", output)))

  # Now apply mutate and test after grouping and filter
  result <- dfnow |>
    group_by(region) |>
    filter(sex == "M")

  output <- capture.output(print(result))

  # Check for Now, Event Date, and Report Date in the output string
  expect_true(any(grepl(paste0("Now: ", get_now(dfnow)), output)))
  expect_true(any(grepl("Event date: \"event\"", output)))
  expect_true(any(grepl("Report date: \"report_date\"", output)))
  expect_true(any(grepl("Strata: \"sex\" and \"region\"", output)))

  # Now apply mutate and test after grouping and mutate twice
  result <- dfnow |>
    group_by(region) |>
    mutate(new_var = 1)

  output <- capture.output(print(result))

  # Check for Now, Event Date, and Report Date in the output string
  expect_true(any(grepl(paste0("Now: ", get_now(dfnow)), output)))
  expect_true(any(grepl("Event date: \"event\"", output)))
  expect_true(any(grepl("Report date: \"report_date\"", output)))
  expect_true(any(grepl("Strata: \"sex\" and \"region\"", output)))

  # Now apply mutate and test after grouping and mutate twice
  result <- dfnow |>
    group_by(region) |>
    mutate(new_var = 1) |>
    mutate(new_col = "a")

  output <- capture.output(print(result))

  # Check for Now, Event Date, and Report Date in the output string
  expect_true(any(grepl(paste0("Now: ", get_now(dfnow)), output)))
  expect_true(any(grepl("Event date: \"event\"", output)))
  expect_true(any(grepl("Report date: \"report_date\"", output)))
  expect_true(any(grepl("Strata: \"sex\" and \"region\"", output)))

  # Group and ungroup should return same output
  result <- dfnow |>
    group_by(region) |>
    ungroup()

  output <- capture.output(print(result))

  # Check for Now, Event Date, and Report Date in the output string
  expect_identical(output, capture.output(print(dfnow)))
})
