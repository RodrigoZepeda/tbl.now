library(dplyr, quietly = TRUE, warn.conflicts = FALSE)

# ---- helpers ----

make_count_incidence <- function(units = "days") {
  if (units == "days") {
    tibble(
      event  = as.Date(c("2020-01-01", "2020-01-01",
                          "2020-01-02",
                          "2020-01-04", "2020-01-04")),
      report = as.Date(c("2020-01-01", "2020-01-02",
                          "2020-01-02",
                          "2020-01-04", "2020-01-05")),
      n      = c(3L, 2L, 4L, 1L, 5L)
    ) |>
      tbl_now(event_date = event, report_date = report,
              case_count = n, data_type = "count-incidence",
              verbose = FALSE)
  } else if (units == "weeks") {
    tibble(
      event  = as.Date(c("2020-01-05", "2020-01-05",
                          "2020-01-12",
                          "2020-01-26", "2020-01-26")),
      report = as.Date(c("2020-01-05", "2020-01-12",
                          "2020-01-12",
                          "2020-01-26", "2020-02-02")),
      n      = c(3L, 2L, 4L, 1L, 5L)
    ) |>
      tbl_now(event_date = event, report_date = report,
              case_count = n, data_type = "count-incidence",
              verbose = FALSE)
  }
}

make_count_incidence_with_strata <- function() {
  tibble(
    event  = rep(as.Date(c("2020-01-01", "2020-01-01",
                            "2020-01-02",
                            "2020-01-04", "2020-01-04")), 2),
    report = rep(as.Date(c("2020-01-01", "2020-01-02",
                            "2020-01-02",
                            "2020-01-04", "2020-01-05")), 2),
    n      = c(3L, 2L, 4L, 1L, 5L, 1L, 2L, 3L, 4L, 5L),
    sex    = c(rep("Male", 5), rep("Female", 5))
  ) |>
    tbl_now(event_date = event, report_date = report,
            case_count = n, strata = sex,
            data_type = "count-incidence",
            verbose = FALSE)
}

make_count_cumulative <- function() {
  tibble(
    event  = as.Date(c("2020-01-01", "2020-01-01",
                        "2020-01-02",
                        "2020-01-04", "2020-01-04")),
    report = as.Date(c("2020-01-01", "2020-01-02",
                        "2020-01-02",
                        "2020-01-04", "2020-01-05")),
    n      = c(3L, 5L, 4L, 1L, 6L)  # cumulative
  ) |>
    tbl_now(event_date = event, report_date = report,
            case_count = n, data_type = "count-cumulative",
            verbose = FALSE)
}

# ---- basic structure ----

test_that("complete_zeroes returns a tbl_now", {
  x <- make_count_incidence()
  result <- complete_zeroes(x)
  expect_s3_class(result, "tbl_now")
})

test_that("complete_zeroes fills missing event dates with 0", {
  x <- make_count_incidence()
  # 2020-01-03 is missing from the data
  result <- complete_zeroes(x)
  event_dates <- result[[get_event_date(result)]]
  expect_true(as.Date("2020-01-03") %in% event_dates)
})

test_that("complete_zeroes filled rows have n = 0", {
  x <- make_count_incidence()
  result <- complete_zeroes(x)
  filled <- result |>
    filter(event == as.Date("2020-01-03"))
  expect_true(nrow(filled) > 0)
  expect_true(all(filled[[get_case_count(result)]] == 0))
})

test_that("complete_zeroes: max_delay argument limits delay range", {
  x <- make_count_incidence()
  result <- complete_zeroes(x, max_delay = 2)
  expect_true(max(result[[".delay"]]) <= 2)
})

test_that("complete_zeroes infers max_delay when not supplied", {
  x <- make_count_incidence()
  result_auto  <- complete_zeroes(x)
  result_manual <- complete_zeroes(x, max_delay = max(x[[".delay"]]))
  expect_equal(nrow(result_auto), nrow(result_manual))
})

# ---- with strata ----

test_that("complete_zeroes works with strata", {
  x <- make_count_incidence_with_strata()
  result <- complete_zeroes(x)
  expect_s3_class(result, "tbl_now")
  # Both strata levels should have zeroed rows for missing event date
  filled <- result |> filter(event == as.Date("2020-01-03"))
  expect_equal(sort(unique(filled$sex)), c("Female", "Male"))
})

# ---- weekly units ----

test_that("complete_zeroes works with weekly data", {
  x <- make_count_incidence(units = "weeks")
  result <- complete_zeroes(x)
  expect_s3_class(result, "tbl_now")
  # 2020-01-19 is the missing week
  event_dates <- result[[get_event_date(result)]]
  expect_true(as.Date("2020-01-19") %in% event_dates)
})

# ---- count-cumulative ----

test_that("complete_zeroes works with count-cumulative data", {
  x <- make_count_cumulative()
  result <- suppressWarnings(complete_zeroes(x))
  expect_s3_class(result, "tbl_now")
})

# ---- error cases ----

test_that("complete_zeroes errors on linelist data", {
  x <- tbl_now(
    tibble(
      event  = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
      report = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03"))
    ),
    event_date = event, report_date = report,
    verbose = FALSE
  )
  expect_error(complete_zeroes(x), "count")
})

test_that("complete_zeroes errors on non-tbl_now input", {
  # The is_tbl_now check is after the max_delay computation, so we just
  # confirm that passing a non-tbl_now always results in an error.
  expect_error(complete_zeroes(data.frame(x = 1)))
})

test_that("complete_zeroes emits message when temporal-effect columns exist", {
  # cli_alert_warning fires as a message, not an R warning
  x <- make_count_incidence() |>
    add_temporal_effects(temporal_effects(day_of_week = TRUE)) |>
    compute_temporal_effects()
  expect_message(complete_zeroes(x), "compute_temporal_effects")
})
