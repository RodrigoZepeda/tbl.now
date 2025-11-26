test_that("align_week returns a data.frame with new aligned date column", {
  df <- data.frame(
    date = as.Date(c("2020-10-31", "2022-11-07", "2022-11-13"))
  )

  out <- align_week(df, date_col = date)

  expect_s3_class(out, "data.frame")
  expect_true("date_aligned" %in% names(out))
})

test_that("align_week aligns dates to the specified weekday", {
  df <- data.frame(date = as.Date("2022-11-09"))  # Wednesday

  # Align to Monday (2)
  out <- align_week(df, date_col = date, align_on_day = 2)
  aligned <- out$date_aligned

  expect_equal(lubridate::wday(aligned), 2)
})

test_that("align_week supports isoweek", {
  df <- data.frame(date = as.Date("2022-12-31"))

  out <- align_week(df, date_col = date, type = "isoweek")
  aligned <- out$date_aligned

  expect_s3_class(out, "data.frame")
  # should produce a valid date
  expect_true(!is.na(aligned))
})

test_that("align_week does not modify original columns", {
  df <- data.frame(date = as.Date("2022-11-09"))

  out <- align_week(df, date_col = date)

  expect_true("date" %in% names(out))
  expect_true("date_aligned" %in% names(out))
  expect_false("week_col" %in% names(out))
  expect_false("year_col" %in% names(out))
})

test_that("week_2_date creates a date column", {
  df <- data.frame(
    week_col = 1:5,
    year_col = rep(2024, 5)
  )

  out <- week_2_date(df, week_col = "week_col", year_col = "year_col")

  expect_s3_class(out, "data.frame")
  expect_true("date" %in% names(out))
})

test_that("week_2_date aligns to the correct weekday", {
  df <- data.frame(
    week_col = 10,
    year_col = 2024
  )

  out <- week_2_date(df,
                     week_col = "week_col",
                     year_col = "year_col",
                     align_on_day = 4)  # Wednesday

  aligned <- out$date

  expect_equal(lubridate::wday(aligned), 4)
})

test_that("week_2_date works for epiweek vs isoweek", {
  df <- data.frame(
    week_col = 1,
    year_col = 2023
  )

  out_epi <- week_2_date(df,
                         week_col = "week_col",
                         year_col = "year_col",
                         week_fun = lubridate::epiweek,
                         year_fun = lubridate::epiyear)

  out_iso <- week_2_date(df,
                         week_col = "week_col",
                         year_col = "year_col",
                         week_fun = lubridate::isoweek,
                         year_fun = lubridate::isoyear)

  # They often differ in early January
  expect_false(identical(out_epi$date, out_iso$date))
})

test_that("week_2_date merges correctly with duplicated rows", {
  df <- data.frame(
    week_col = c(5, 5, 6),
    year_col = c(2024, 2024, 2024)
  )

  out <- week_2_date(df, "week_col", "year_col")

  expect_equal(nrow(out), 3)
  expect_false(any(is.na(out$date)))
})
