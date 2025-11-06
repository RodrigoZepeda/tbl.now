test_that("infer_now works", {

  #Check that it returns the maximum if no null is specified
  disease_data <- dplyr::tibble(
    event_date   = seq(as.Date("2020/01/01"), as.Date("2020/12/01")),
    report_date = seq(as.Date("2020/01/05"), as.Date("2020/12/05")),
    wrong_date  = "a"
  )

  expect_equal(
    infer_now(disease_data, now = NULL, report_date = "report_date", event_date = "event_date"),
    max(disease_data$report_date)
  )

  #Check that it returns the value given if now is specified
  now <- as.Date("2020/05/16")
  expect_equal(
    infer_now(disease_data, now = now, report_date = "report_date", event_date = "event_date"),
    now
  )

  #Check what happens with an empty tibble
  disease_empty <- disease_data |> dplyr::filter(report_date > as.Date("2021/01/01"))
  expect_error(
    infer_now(disease_empty, now = NULL, report_date = "report_date", event_date = "event_date"),
    "empty data.frame"
  )

  #Input incorrect columns
  expect_error(
    infer_now(disease_data, now = now, report_date = "ERROR", event_date = "event_date"),
    "Column `report_date .* not found"
  )

  expect_error(
    infer_now(disease_data, now = now, report_date = "report_date", event_date = "ERROR"),
    "Column `event_date .* not found"
  )

  expect_error(
    infer_now(disease_data, now = now, report_date = "wrong_date", event_date = "event_date"),
    "report_date.* `as.Date`"
  )

  expect_error(
    infer_now(disease_data, now = now, report_date = "report_date", event_date = "wrong_date"),
    "event_date.* `as.Date`"
  )


})
