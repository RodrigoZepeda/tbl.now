test_that("test it returns the attributes correctly", {
  data(denguedat)

  df_now <- tbl_now(denguedat, event_date = onset_week,
    report_date = report_week, strata = gender, verbose = FALSE)

  #Attributes gets all attributes
  df_atr  <- attributes(df_now) %>% names()

  #Remove optional attributes
  tbl_atr <- c(attributes(dplyr::tibble(denguedat)) %>% names(), "strata", "covariates")

  #tbl_now_attributes gets only those associated to the `tbl_now` class
  expect_equal(
    names(tbl_now_attributes(df_now)),
    df_atr[which(!(df_atr %in% tbl_atr))]
  )
})

test_that("is_weekday works with default weekend (Sat-Sun)", {
  # Weekdays
  expect_true(is_weekday(as.Date("2020-04-20")))   # Monday
  expect_true(is_weekday(as.Date("2020-04-21")))   # Tuesday
  expect_true(is_weekday(as.Date("2020-04-22")))   # Wednesday
  expect_true(is_weekday(as.Date("2020-04-23")))   # Thursday
  expect_true(is_weekday(as.Date("2020-04-24")))   # Friday

  # Weekends
  expect_false(is_weekday(as.Date("2020-04-25")))  # Saturday
  expect_false(is_weekday(as.Date("2020-04-26")))  # Sunday
  expect_false(is_weekday(as.Date("2020-04-19")))  # Sunday
})

test_that("is_weekday works with character weekend_days", {
  # Middle East weekend (Fri-Sat)
  expect_false(is_weekday(as.Date("2020-04-17"), weekend_days = c("Fri", "Sat")))  # Friday
  expect_false(is_weekday(as.Date("2020-04-18"), weekend_days = c("Fri", "Sat")))  # Saturday
  expect_true(is_weekday(as.Date("2020-04-19"), weekend_days = c("Fri", "Sat")))   # Sunday

  # Single day weekend
  expect_false(is_weekday(as.Date("2020-04-17"), weekend_days = "Friday"))
  expect_true(is_weekday(as.Date("2020-04-18"), weekend_days = "Friday"))

  # Full day names (case variations)
  expect_false(is_weekday(as.Date("2020-04-17"), weekend_days = "friday"))
  expect_false(is_weekday(as.Date("2020-04-18"), weekend_days = "Saturday"))
  expect_false(is_weekday(as.Date("2020-04-19"), weekend_days = c("sun", "SAT")))
})

test_that("is_weekday works with numeric weekend_days", {
  # Sunday-Monday weekend (7 = Sun, 1 = Mon)
  expect_false(is_weekday(as.Date("2020-04-19"), weekend_days = c(7, 1)))  # Sunday
  expect_false(is_weekday(as.Date("2020-04-20"), weekend_days = c(7, 1)))  # Monday
  expect_true(is_weekday(as.Date("2020-04-21"), weekend_days = c(7, 1)))   # Tuesday

  # Single numeric weekend day
  expect_false(is_weekday(as.Date("2020-04-22"), weekend_days = 3))  # Wednesday (3)
  expect_true(is_weekday(as.Date("2020-04-23"), weekend_days = 3))   # Thursday

  # All days are weekend
  expect_false(is_weekday(as.Date("2020-04-20"), weekend_days = 1:7))
})

test_that("is_weekday works with POSIXt objects", {
  posix_date <- as.POSIXct("2020-04-22 14:30:00", tz = "UTC")
  expect_true(is_weekday(posix_date))  # Wednesday

  posix_weekend <- as.POSIXlt("2020-04-25 09:00:00", tz = "UTC")
  expect_false(is_weekday(posix_weekend))  # Saturday
})

test_that("is_weekday handles vectors", {
  dates <- as.Date(c("2020-04-20", "2020-04-21", "2020-04-25", "2020-04-26"))
  result <- is_weekday(dates)

  expect_equal(result, c(TRUE, TRUE, FALSE, FALSE))
  expect_length(result, 4)
})

test_that("is_weekday handles abbreviated and full day names", {
  # Abbreviated names
  expect_false(is_weekday(as.Date("2020-04-20"), weekend_days = c("Mon", "Tue")))

  # Full names
  expect_false(is_weekday(as.Date("2020-04-20"), weekend_days = "Monday"))
  expect_false(is_weekday(as.Date("2020-04-21"), weekend_days = "Tuesday"))

  # Mixed lengths
  expect_false(is_weekday(as.Date("2020-04-22"), weekend_days = c("wed", "Thursday")))
})

test_that("is_weekday errors on invalid weekend_days", {
  # Invalid character
  expect_error(
    is_weekday(as.Date("2020-04-20"), weekend_days = "Funday"),
    "Invalid `weekend_days` provided"
  )

  # Invalid numeric (out of range)
  expect_error(
    is_weekday(as.Date("2020-04-20"), weekend_days = 8),
    "Invalid `weekend_days` provided"
  )

  expect_error(
    is_weekday(as.Date("2020-04-20"), weekend_days = 0),
    "Invalid `weekend_days` provided"
  )

  # Mix of valid and invalid
  expect_error(
    is_weekday(as.Date("2020-04-20"), weekend_days = c("Mon", "InvalidDay")),
    "Invalid `weekend_days` provided"
  )
})

test_that("is_weekday handles edge cases", {
  # Empty vector (if your function supports it)
  # expect_equal(is_weekday(as.Date(character(0))), logical(0))

  # Single date
  expect_length(is_weekday(as.Date("2020-04-20")), 1)

  # Long vector
  many_dates <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day")
  result <- is_weekday(many_dates)
  expect_length(result, 366)  # 2020 is a leap year
  expect_type(result, "logical")
})
