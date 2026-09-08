test_that("test it returns the attributes correctly", {
  data(denguedat)

  df_now <- tbl_now(denguedat,
    event_date = onset_week,
    report_date = report_week, strata = gender, verbose = FALSE
  )

  # `tbl_now_attributes()` is everything the object carries that a bare tibble
  # does not -- including the OPTIONAL ones.
  #
  # Until 0.21.0 it diffed against a default `tbl_now`, so any attribute the
  # default did not happen to carry was silently missing from the listing:
  # `strata`, `covariates`, and every revision attribute. That is precisely
  # what somebody calls this function to check, so the omission was the bug.
  listed <- names(tbl_now_attributes(df_now))
  tibble_attributes <- names(attributes(dplyr::tibble(denguedat)))

  expect_setequal(
    listed,
    setdiff(names(attributes(df_now)), tibble_attributes)
  )
  # `strata` was declared here, so it must be listed. (`covariates` was not
  # declared, so the object never gained the attribute at all.)
  expect_true("strata" %in% listed)
  expect_false(any(tibble_attributes %in% listed))
})

test_that("is_weekday works with default weekend (Sat-Sun)", {
  # Weekdays
  expect_true(is_weekday(as.Date("2020-04-20"))) # Monday
  expect_true(is_weekday(as.Date("2020-04-21"))) # Tuesday
  expect_true(is_weekday(as.Date("2020-04-22"))) # Wednesday
  expect_true(is_weekday(as.Date("2020-04-23"))) # Thursday
  expect_true(is_weekday(as.Date("2020-04-24"))) # Friday

  # Weekends
  expect_false(is_weekday(as.Date("2020-04-25"))) # Saturday
  expect_false(is_weekday(as.Date("2020-04-26"))) # Sunday
  expect_false(is_weekday(as.Date("2020-04-19"))) # Sunday
})

test_that("is_weekday works with character weekend_days", {
  # Middle East weekend (Fri-Sat)
  expect_false(is_weekday(as.Date("2020-04-17"), weekend_days = c("Fri", "Sat"))) # Friday
  expect_false(is_weekday(as.Date("2020-04-18"), weekend_days = c("Fri", "Sat"))) # Saturday
  expect_true(is_weekday(as.Date("2020-04-19"), weekend_days = c("Fri", "Sat"))) # Sunday

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
  expect_false(is_weekday(as.Date("2020-04-19"), weekend_days = c(7, 1))) # Sunday
  expect_false(is_weekday(as.Date("2020-04-20"), weekend_days = c(7, 1))) # Monday
  expect_true(is_weekday(as.Date("2020-04-21"), weekend_days = c(7, 1))) # Tuesday

  # Single numeric weekend day
  expect_false(is_weekday(as.Date("2020-04-22"), weekend_days = 3)) # Wednesday (3)
  expect_true(is_weekday(as.Date("2020-04-23"), weekend_days = 3)) # Thursday

  # All days are weekend
  expect_false(is_weekday(as.Date("2020-04-20"), weekend_days = 1:7))
})

test_that("is_weekday works with POSIXt objects", {
  posix_date <- as.POSIXct("2020-04-22 14:30:00", tz = "UTC")
  expect_true(is_weekday(posix_date)) # Wednesday

  posix_weekend <- as.POSIXlt("2020-04-25 09:00:00", tz = "UTC")
  expect_false(is_weekday(posix_weekend)) # Saturday
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

# A locale is only usable if the platform actually has it installed, which is
# not guaranteed on a check machine, so every test below skips itself when the
# locale it needs is unavailable.
local_time_locale <- function(locale, env = parent.frame()) {
  old <- Sys.getlocale("LC_TIME")
  set <- suppressWarnings(Sys.setlocale("LC_TIME", locale))
  if (!nzchar(set)) {
    testthat::skip(paste0("LC_TIME locale '", locale, "' is not available"))
  }
  withr::defer(Sys.setlocale("LC_TIME", old), envir = env)
  invisible(set)
}

test_that("is_weekday accepts English day names in any locale", {
  # The bug: day names were matched against `lubridate::wday(label = TRUE)`,
  # which speaks the locale's language, so under `es_ES` the DEFAULT
  # `weekend_days = c("Sat", "Sun")` had nothing to match and errored.
  for (locale in c("es_ES.UTF-8", "fr_FR.UTF-8", "de_DE.UTF-8", "ja_JP.UTF-8")) {
    old <- Sys.getlocale("LC_TIME")
    if (!nzchar(suppressWarnings(Sys.setlocale("LC_TIME", locale)))) next
    on.exit(Sys.setlocale("LC_TIME", old), add = TRUE)

    expect_false(is_weekday(as.Date("2020-04-18")), info = locale) # Saturday
    expect_true(is_weekday(as.Date("2020-04-17")), info = locale) # Friday
    expect_false(
      is_weekday(as.Date("2020-04-17"), weekend_days = c("Fri", "Sat")),
      info = locale
    )
    expect_false(
      is_weekday(as.Date("2020-04-19"), weekend_days = "Sunday"),
      info = locale
    )
  }
})

test_that("is_weekday accepts the day names of the current locale", {
  # Whatever the locale is, its own names for Saturday and Sunday work.
  saturday <- as.Date("2020-04-18")
  sunday <- as.Date("2020-04-19")
  monday <- as.Date("2020-04-20")

  for (fmt in c("%a", "%A")) {
    weekend <- format(c(saturday, sunday), fmt)
    expect_false(is_weekday(saturday, weekend_days = weekend), info = fmt)
    expect_false(is_weekday(sunday, weekend_days = weekend), info = fmt)
    expect_true(is_weekday(monday, weekend_days = weekend), info = fmt)
  }
})

test_that("Spanish day names work, with or without their accents", {
  local_time_locale("es_ES.UTF-8")

  saturday <- as.Date("2020-04-18")
  wednesday <- as.Date("2020-04-22")

  # Abbreviated, full, accentless and upper case all resolve to the same day.
  expect_false(is_weekday(saturday, weekend_days = c("s\u00e1b", "dom")))
  expect_false(is_weekday(saturday, weekend_days = c("sab", "dom")))
  expect_false(is_weekday(saturday, weekend_days = c("SAB", "DOM")))
  expect_false(is_weekday(saturday, weekend_days = c("s\u00e1bado", "domingo")))
  expect_false(is_weekday(wednesday, weekend_days = "mi\u00e9rcoles"))
  expect_false(is_weekday(wednesday, weekend_days = "miercoles"))

  # English and locale names can be mixed.
  expect_false(is_weekday(saturday, weekend_days = c("Sat", "domingo")))
  expect_true(is_weekday(as.Date("2020-04-20"), weekend_days = c("Sat", "domingo")))

  # A name that is a day in neither language is still an error.
  expect_error(
    is_weekday(saturday, weekend_days = "Funday"),
    "Invalid `weekend_days` provided"
  )
})

test_that("day_of_week effects are labelled in English in any locale", {
  # `lubridate::wday(label = TRUE)` labels in the locale's language, which used
  # to leave the whole column `NA` outside an English locale, since the factor
  # levels are (deliberately) the English names `epinowcast` uses.
  local_time_locale("es_ES.UTF-8")

  df <- data.frame(date = as.Date("2020-04-13") + 0:6) # Monday .. Sunday
  out <- add_temporal_effects(
    df,
    date_col = "date",
    t_effects = temporal_effects(day_of_week = TRUE, weekend = TRUE)
  )

  expect_equal(
    as.character(out$.date_day_of_week),
    c(
      "Monday", "Tuesday", "Wednesday", "Thursday",
      "Friday", "Saturday", "Sunday"
    )
  )
  expect_equal(out$.date_weekend, c(0L, 0L, 0L, 0L, 0L, 1L, 1L))
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
  expect_length(result, 366) # 2020 is a leap year
  expect_type(result, "logical")
})
