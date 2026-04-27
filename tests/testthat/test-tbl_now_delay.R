library(dplyr, quietly = TRUE, warn.conflicts = FALSE)

# ============================================================
# Test data helpers
# ============================================================

# Daily data: event_date + report_date explicitly known
make_daily_data <- function() {
  tibble(
    event_date  = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03",
                             "2023-01-01", "2023-01-02")),
    report_date = as.Date(c("2023-01-03", "2023-01-03", "2023-01-05",
                             "2023-01-04", "2023-01-04")),
    gender      = c("M", "F", "M", "F", "M")
  ) %>%
    mutate(delay_days = as.numeric(report_date - event_date))
}

# Weekly data (7-day spacing)
make_weekly_data <- function() {
  tibble(
    event_date  = as.Date(c("2023-01-01", "2023-01-08", "2023-01-15",
                             "2023-01-01", "2023-01-08")),
    report_date = as.Date(c("2023-01-15", "2023-01-15", "2023-01-22",
                             "2023-01-08", "2023-01-22"))
  ) %>%
    mutate(delay_weeks = as.numeric(difftime(report_date, event_date, units = "weeks")))
}

# Monthly data (1-month spacing)
make_monthly_data <- function() {
  tibble(
    event_date  = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01",
                             "2023-01-01", "2023-02-01")),
    report_date = as.Date(c("2023-03-01", "2023-03-01", "2023-04-01",
                             "2023-02-01", "2023-04-01"))
  ) %>%
    mutate(
      delay_months = (lubridate::year(report_date) - lubridate::year(event_date)) * 12L +
                     (lubridate::month(report_date) - lubridate::month(event_date))
    )
}

# Yearly data
make_yearly_data <- function() {
  tibble(
    event_date  = as.Date(c("2018-01-01", "2019-01-01", "2020-01-01",
                             "2018-01-01", "2019-01-01")),
    report_date = as.Date(c("2020-01-01", "2020-01-01", "2021-01-01",
                             "2019-01-01", "2021-01-01"))
  ) %>%
    mutate(delay_years = lubridate::year(report_date) - lubridate::year(event_date))
}

# Numeric data
make_numeric_data <- function() {
  tibble(
    event_num  = c(1L, 2L, 3L, 1L, 2L),
    report_num = c(3L, 3L, 5L, 4L, 4L)
  ) %>%
    mutate(delay_num = report_num - event_num)
}

# ============================================================
# 1. event_date + delay  →  report_date reconstructed
# ============================================================

test_that("event_date + delay (days) reconstructs report_date correctly", {
  d <- make_daily_data()
  result <- tbl_now(d,
                    event_date = event_date, delay = delay_days,
                    event_units = "days", report_units = "days",
                    verbose = FALSE)

  expect_s3_class(result, "tbl_now")
  expect_equal(get_event_date(result), "event_date")
  expect_equal(get_report_date(result), ".report_date")
  expect_true(".report_date" %in% colnames(result))
  # Reconstructed report_date must equal original
  expect_equal(result[[".report_date"]], d$report_date)
  # .delay must equal original delay column
  expect_equal(result[[".delay"]], d$delay_days)
})

test_that("event_date + delay (weeks) reconstructs report_date correctly", {
  d <- make_weekly_data()
  result <- tbl_now(d,
                    event_date = event_date, delay = delay_weeks,
                    event_units = "weeks",
                    verbose = FALSE)

  expect_s3_class(result, "tbl_now")
  expect_equal(result[[".report_date"]], d$report_date)
  expect_equal(result[[".delay"]], d$delay_weeks)
})

test_that("event_date + delay (months) reconstructs report_date correctly", {
  d <- make_monthly_data()
  result <- tbl_now(d,
                    event_date = event_date, delay = delay_months,
                    event_units = "months",
                    verbose = FALSE)

  expect_s3_class(result, "tbl_now")
  expect_equal(result[[".report_date"]], d$report_date)
  expect_equal(result[[".delay"]], d$delay_months)
})

test_that("event_date + delay (years) reconstructs report_date correctly", {
  d <- make_yearly_data()
  result <- tbl_now(d,
                    event_date = event_date, delay = delay_years,
                    event_units = "years",
                    verbose = FALSE)

  expect_s3_class(result, "tbl_now")
  expect_equal(result[[".report_date"]], d$report_date)
  expect_equal(result[[".delay"]], d$delay_years)
})

test_that("event_date + delay (numeric) reconstructs report_num correctly", {
  d <- make_numeric_data()
  result <- tbl_now(d,
                    event_date = event_num, delay = delay_num,
                    event_units = "numeric", report_units = "numeric",
                    verbose = FALSE)

  expect_s3_class(result, "tbl_now")
  expect_equal(result[[".report_date"]], d$report_num)
  expect_equal(result[[".delay"]], d$delay_num)
})

# ============================================================
# 2. report_date + delay  →  event_date reconstructed
# ============================================================

test_that("report_date + delay (days) reconstructs event_date correctly", {
  d <- make_daily_data()
  result <- tbl_now(d,
                    report_date = report_date, delay = delay_days,
                    event_units = "days",
                    verbose = FALSE)

  expect_s3_class(result, "tbl_now")
  expect_equal(get_event_date(result), ".event_date")
  expect_equal(get_report_date(result), "report_date")
  expect_true(".event_date" %in% colnames(result))
  expect_equal(result[[".event_date"]], d$event_date)
  expect_equal(result[[".delay"]], d$delay_days)
})

test_that("report_date + delay (weeks) reconstructs event_date correctly", {
  d <- make_weekly_data()
  result <- tbl_now(d,
                    report_date = report_date, delay = delay_weeks,
                    event_units = "weeks",
                    verbose = FALSE)

  expect_s3_class(result, "tbl_now")
  expect_equal(result[[".event_date"]], d$event_date)
  expect_equal(result[[".delay"]], d$delay_weeks)
})

test_that("report_date + delay (months) reconstructs event_date correctly", {
  d <- make_monthly_data()
  result <- tbl_now(d,
                    report_date = report_date, delay = delay_months,
                    event_units = "months",
                    verbose = FALSE)

  expect_s3_class(result, "tbl_now")
  expect_equal(result[[".event_date"]], d$event_date)
  expect_equal(result[[".delay"]], d$delay_months)
})

test_that("report_date + delay (years) reconstructs event_date correctly", {
  d <- make_yearly_data()
  result <- tbl_now(d,
                    report_date = report_date, delay = delay_years,
                    event_units = "years",
                    verbose = FALSE)

  expect_s3_class(result, "tbl_now")
  expect_equal(result[[".event_date"]], d$event_date)
  expect_equal(result[[".delay"]], d$delay_years)
})

test_that("report_date + delay (numeric) reconstructs event_num correctly", {
  d <- make_numeric_data()
  result <- tbl_now(d,
                    report_date = report_num, delay = delay_num,
                    event_units = "numeric",
                    verbose = FALSE)

  expect_s3_class(result, "tbl_now")
  expect_equal(result[[".event_date"]], d$event_num)
  expect_equal(result[[".delay"]], d$delay_num)
})

# ============================================================
# 3. Equivalence: delay-based == both-dates explicit
# ============================================================

test_that("event_date + delay produces same tbl_now as both dates explicit (days)", {
  d <- make_daily_data()

  # Build via both dates (select only the needed columns to avoid delay col)
  explicit <- tbl_now(d %>% select(event_date, report_date),
                      event_date = event_date, report_date = report_date,
                      event_units = "days",
                      verbose = FALSE)

  # Build via event_date + delay
  via_delay <- tbl_now(d %>% select(event_date, delay_days),
                       event_date = event_date, delay = delay_days,
                       event_units = "days",
                       verbose = FALSE)

  # Core numeric columns must match
  expect_equal(via_delay[[".event_num"]], explicit[[".event_num"]])
  expect_equal(via_delay[[".report_num"]], explicit[[".report_num"]])
  expect_equal(via_delay[[".delay"]],      explicit[[".delay"]])
  # Attributes must match
  expect_equal(get_event_units(via_delay),  get_event_units(explicit))
  expect_equal(get_report_units(via_delay), get_report_units(explicit))
  expect_equal(get_now(via_delay),          get_now(explicit))
})

test_that("report_date + delay produces same numerics as both dates explicit (weeks)", {
  d <- make_weekly_data()

  explicit <- tbl_now(d %>% select(event_date, report_date),
                      event_date = event_date, report_date = report_date,
                      event_units = "weeks",
                      verbose = FALSE)

  via_delay <- tbl_now(d %>% select(report_date, delay_weeks),
                       report_date = report_date, delay = delay_weeks,
                       event_units = "weeks",
                       verbose = FALSE)

  expect_equal(via_delay[[".event_num"]], explicit[[".event_num"]])
  expect_equal(via_delay[[".report_num"]], explicit[[".report_num"]])
  expect_equal(via_delay[[".delay"]],      explicit[[".delay"]])
})

# ============================================================
# 4. Strata and covariates work with delay construction
# ============================================================

test_that("delay-based construction preserves strata and covariates", {
  d <- make_daily_data()
  result <- tbl_now(d,
                    event_date = event_date, delay = delay_days,
                    strata = gender,
                    event_units = "days",
                    verbose = FALSE)

  expect_s3_class(result, "tbl_now")
  expect_equal(get_strata(result), "gender")
  expect_true("gender" %in% colnames(result))
})

# ============================================================
# 5. Auto-inference of units when event_units = "auto"
# ============================================================

test_that("units auto-inferred from event_date when delay provided (days)", {
  d <- make_daily_data()
  result <- tbl_now(d,
                    event_date = event_date, delay = delay_days,
                    # event_units deliberately left as "auto"
                    verbose = FALSE)

  expect_s3_class(result, "tbl_now")
  expect_equal(get_event_units(result), "days")
  expect_equal(result[[".delay"]], d$delay_days)
})

test_that("units auto-inferred from report_date when delay provided (weeks)", {
  d <- make_weekly_data()
  result <- tbl_now(d,
                    report_date = report_date, delay = delay_weeks,
                    # event_units deliberately left as "auto"
                    verbose = FALSE)

  expect_s3_class(result, "tbl_now")
  expect_equal(get_event_units(result), "weeks")
  expect_equal(result[[".delay"]], d$delay_weeks)
})

# ============================================================
# 6. Delay column named ".delay" is handled (recomputed)
# ============================================================

test_that("delay column named '.delay' is dropped and recomputed", {
  d <- make_daily_data() %>% rename(.delay = delay_days)
  result <- tbl_now(d,
                    event_date = event_date, delay = .delay,
                    event_units = "days",
                    verbose = FALSE)

  expect_s3_class(result, "tbl_now")
  # .delay should exist as the internal recomputed column, not duplicated
  expect_equal(sum(colnames(result) == ".delay"), 1L)
  # Value should be correct
  expect_equal(result[[".delay"]], d[[".delay"]])
})

# ============================================================
# 7. Error cases
# ============================================================

test_that("error when neither event_date nor report_date provided", {
  d <- make_daily_data()
  expect_error(
    tbl_now(d, delay = delay_days, event_units = "days", verbose = FALSE),
    "at least one"
  )
})

test_that("error when event_date missing and no delay provided", {
  d <- make_daily_data()
  expect_error(
    tbl_now(d, report_date = report_date, verbose = FALSE),
    "delay"
  )
})

test_that("error when report_date missing and no delay provided", {
  d <- make_daily_data()
  expect_error(
    tbl_now(d, event_date = event_date, verbose = FALSE),
    "delay"
  )
})

test_that("error when delay column is not numeric", {
  d <- make_daily_data() %>%
    mutate(str_delay = as.character(delay_days))
  expect_error(
    tbl_now(d, event_date = event_date, delay = str_delay,
            event_units = "days", verbose = FALSE),
    "numeric"
  )
})

# ============================================================
# 8. verbose messaging
# ============================================================

test_that("verbose=TRUE emits message when report_date is reconstructed", {
  d <- make_daily_data()
  expect_message(
    tbl_now(d, event_date = event_date, delay = delay_days,
            event_units = "days", verbose = TRUE),
    "\\.report_date"
  )
})

test_that("verbose=TRUE emits message when event_date is reconstructed", {
  d <- make_daily_data()
  expect_message(
    tbl_now(d, report_date = report_date, delay = delay_days,
            event_units = "days", verbose = TRUE),
    "\\.event_date"
  )
})

test_that("verbose=FALSE suppresses reconstruction message", {
  d <- make_daily_data()
  expect_no_message(
    tbl_now(d, event_date = event_date, delay = delay_days,
            event_units = "days", verbose = FALSE)
  )
})
