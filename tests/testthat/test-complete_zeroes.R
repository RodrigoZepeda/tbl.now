library(dplyr, quietly = TRUE, warn.conflicts = FALSE)

# ---- helpers ----

make_count_incidence <- function(units = "days") {
  if (units == "days") {
    tibble(
      event = as.Date(c(
        "2020-01-01", "2020-01-01",
        "2020-01-02",
        "2020-01-04", "2020-01-04"
      )),
      report = as.Date(c(
        "2020-01-01", "2020-01-02",
        "2020-01-02",
        "2020-01-04", "2020-01-05"
      )),
      n = c(3L, 2L, 4L, 1L, 5L)
    ) |>
      tbl_now(
        event_date = event, report_date = report,
        case_count = n, data_type = "count-incidence",
        verbose = FALSE
      )
  } else if (units == "weeks") {
    tibble(
      event = as.Date(c(
        "2020-01-05", "2020-01-05",
        "2020-01-12",
        "2020-01-26", "2020-01-26"
      )),
      report = as.Date(c(
        "2020-01-05", "2020-01-12",
        "2020-01-12",
        "2020-01-26", "2020-02-02"
      )),
      n = c(3L, 2L, 4L, 1L, 5L)
    ) |>
      tbl_now(
        event_date = event, report_date = report,
        case_count = n, data_type = "count-incidence",
        verbose = FALSE
      )
  }
}

make_count_incidence_with_strata <- function() {
  tibble(
    event = rep(as.Date(c(
      "2020-01-01", "2020-01-01",
      "2020-01-02",
      "2020-01-04", "2020-01-04"
    )), 2),
    report = rep(as.Date(c(
      "2020-01-01", "2020-01-02",
      "2020-01-02",
      "2020-01-04", "2020-01-05"
    )), 2),
    n = c(3L, 2L, 4L, 1L, 5L, 1L, 2L, 3L, 4L, 5L),
    sex = c(rep("Male", 5), rep("Female", 5))
  ) |>
    tbl_now(
      event_date = event, report_date = report,
      case_count = n, strata = sex,
      data_type = "count-incidence",
      verbose = FALSE
    )
}

make_count_cumulative <- function() {
  tibble(
    event = as.Date(c(
      "2020-01-01", "2020-01-01",
      "2020-01-02",
      "2020-01-04", "2020-01-04"
    )),
    report = as.Date(c(
      "2020-01-01", "2020-01-02",
      "2020-01-02",
      "2020-01-04", "2020-01-05"
    )),
    n = c(3L, 5L, 4L, 1L, 6L) # cumulative
  ) |>
    tbl_now(
      event_date = event, report_date = report,
      case_count = n, data_type = "count-cumulative",
      verbose = FALSE
    )
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
  result_auto <- complete_zeroes(x)
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

# === complete_zeroes with an is_censored_report column ===========================

make_censored_incidence <- function() {
  d <- dplyr::tibble(
    event = as.Date(c(
      "2020-01-01", "2020-01-01", "2020-01-02",
      "2020-01-04", "2020-01-04"
    )),
    report = as.Date(c(
      "2020-01-01", "2020-01-02", "2020-01-02",
      "2020-01-04", "2020-01-05"
    )),
    n = c(3, 1, 2, 4, 1),
    cens = c(FALSE, FALSE, TRUE, FALSE, TRUE)
  )
  tbl_now(d,
    event_date = event, report_date = report, case_count = n,
    is_censored_report = cens, data_type = "count-incidence",
    event_units = "days", report_units = "days", verbose = FALSE
  )
}

test_that("complete_zeroes works with an is_censored_report column (count-incidence)", {
  cz <- complete_zeroes(make_censored_incidence())

  expect_true(is_tbl_now(cz))
  expect_equal(get_is_censored_report(cz), "cens")
  # the censored column survives and has no NAs
  expect_false(anyNA(cz[["cens"]]))
  # completion happens for BOTH censored states
  expect_setequal(unique(cz[["cens"]]), c(FALSE, TRUE))
  # the missing event date (2020-01-03) is filled for both censored states
  filled <- dplyr::filter(as.data.frame(cz), event == as.Date("2020-01-03"))
  expect_true(all(filled[["n"]] == 0))
  expect_setequal(unique(filled[["cens"]]), c(FALSE, TRUE))
})

test_that("complete_zeroes works with is_censored_report on count-cumulative data", {
  cz <- make_censored_incidence() |>
    to_count("count-cumulative") |>
    complete_zeroes()

  expect_true(is_tbl_now(cz))
  expect_equal(get_data_type(cz), "count-cumulative")
  expect_false(anyNA(cz[["cens"]]))
  expect_false(anyNA(cz[[get_case_count(cz)]]))
})

# Regression tests for two problems found in 2026-08: the closing
# "don't look into the future" filter deleted genuine rows at the final report
# date, and completion stopped at the last *observed* event date, leaving a hole
# exactly at the `now` edge (which is where nowcasting matters).

make_gappy_tbl_now <- function() {
  ndata <- dplyr::tibble(
    event = rep(c(
      as.Date("2020/01/01"), as.Date("2020/01/01"),
      as.Date("2020/01/02"), as.Date("2020/01/04"),
      as.Date("2020/01/04")
    ), 2),
    report = rep(c(
      as.Date("2020/01/01"), as.Date("2020/01/02"),
      as.Date("2020/01/02"), as.Date("2020/01/04"),
      as.Date("2020/01/05")
    ), 2),
    n = c(4, 3, 7, 6, 3, 5, 2, 8, 4, 2),
    sex = c(rep("Male", 5), rep("Female", 5))
  )
  tbl_now(ndata,
    event_date = event, report_date = report, strata = sex,
    case_count = n, data_type = "count-incidence", verbose = FALSE
  )
}

test_that("complete_zeroes() does not drop cases at the final report date", {
  x <- make_gappy_tbl_now()
  completed <- complete_zeroes(x)

  # Completing with zeroes must never change the number of cases.
  expect_equal(
    sum(completed[[get_case_count(completed)]]),
    sum(x[[get_case_count(x)]])
  )

  # The rows reported on the last report date must survive.
  last_report <- max(x[[get_report_date(x)]])
  expect_equal(
    sum(dplyr::filter(completed, report == last_report)[["n"]]),
    sum(dplyr::filter(x, report == last_report)[["n"]])
  )
})

test_that("complete_zeroes() completes up to the `now`, not just the last event", {
  x <- make_gappy_tbl_now()
  completed <- complete_zeroes(x)

  # `now` (2020-01-05) is later than the last observed event date (2020-01-04).
  expect_true(get_now(x) > max(x[["event"]]))
  expect_equal(max(completed[["event"]]), get_now(x))

  # Interior gaps are still filled.
  expect_true(as.Date("2020-01-03") %in% completed[["event"]])

  # Every event date from the first to the `now` is present, for every stratum.
  expected <- seq(min(x[["event"]]), get_now(x), by = "1 day")
  for (stratum in unique(completed[["sex"]])) {
    expect_setequal(
      unique(dplyr::filter(completed, sex == stratum)[["event"]]),
      expected
    )
  }
})

test_that("complete_zeroes() honours `until` but never truncates the data", {
  x <- make_gappy_tbl_now()

  # An `until` earlier than the data must not remove event dates.
  shrunk <- complete_zeroes(x, until = as.Date("2020-01-02"))
  expect_equal(max(shrunk[["event"]]), max(x[["event"]]))
  expect_equal(
    sum(shrunk[[get_case_count(shrunk)]]),
    sum(x[[get_case_count(x)]])
  )
})

test_that("complete_zeroes() rejects a line list with actionable advice", {
  ll <- tbl_now(
    dplyr::tibble(
      event  = as.Date(c("2020-01-01", "2020-01-02")),
      report = as.Date(c("2020-01-02", "2020-01-03"))
    ),
    event_date = event, report_date = report,
    data_type = "linelist", verbose = FALSE
  )
  expect_error(complete_zeroes(ll), "count-incidence")
  expect_error(complete_zeroes(ll), "to_count")
})

test_that("complete_zeroes works on a grouped tbl_now", {
  df <- data.frame(
    onset = as.Date("2024-01-07") + 7 * rep(0:9, each = 2),
    reported = as.Date("2024-01-14") + 7 * rep(0:9, each = 2),
    sex = rep(c("F", "M"), 10),
    n = rep(c(3L, 5L), 10)
  )
  x <- tbl_now(df,
    event_date = onset, report_date = reported, case_count = n, strata = sex,
    data_type = "count-incidence", units = "weeks", verbose = FALSE
  )
  ungrouped <- suppressMessages(complete_zeroes(x))

  out <- suppressMessages(complete_zeroes(x |> dplyr::group_by(!!as.symbol("sex"))))

  expect_true(is_tbl_now(out))
  expect_equal(dplyr::group_vars(out), "sex")
  # The grid is a property of the object, not of how the caller grouped it.
  # Grouped, every bound was computed once PER GROUP and the date sequence was
  # handed a length-2 `from`.
  expect_equal(
    dplyr::as_tibble(ungroup(out)),
    dplyr::as_tibble(ungroup(ungrouped))
  )
  expect_equal(nrow(out), nrow(ungrouped))
})

test_that("complete_zeroes gives the same grid when grouped by a non-stratum", {
  df <- data.frame(
    onset = as.Date("2024-01-07") + 7 * rep(0:5, each = 2),
    reported = as.Date("2024-01-14") + 7 * rep(0:5, each = 2),
    sex = rep(c("F", "M"), 6),
    n = rep(c(2L, 4L), 6)
  )
  x <- tbl_now(df,
    event_date = onset, report_date = reported, case_count = n, strata = sex,
    data_type = "count-incidence", units = "weeks", verbose = FALSE
  )
  by_report <- suppressMessages(
    complete_zeroes(x |> dplyr::group_by(!!as.symbol("reported")))
  )
  expect_equal(
    dplyr::as_tibble(ungroup(by_report)),
    dplyr::as_tibble(ungroup(suppressMessages(complete_zeroes(x))))
  )
})

# ---- missing dates (#66) ----

make_na_report <- function() {
  tibble(
    event = as.Date(c(
      "2020-01-01", "2020-01-01", "2020-01-02", "2020-01-04", "2020-01-04"
    )),
    report = as.Date(c(
      "2020-01-01", "2020-01-02", NA, "2020-01-04", "2020-01-05"
    )),
    n = c(3L, 2L, 4L, 1L, 5L)
  ) |>
    tbl_now(
      event_date = event, report_date = report, case_count = n,
      data_type = "count-incidence", verbose = FALSE
    )
}

test_that("complete_zeroes works when a report date is missing (#66)", {
  x <- suppressWarnings(make_na_report())

  out <- suppressWarnings(complete_zeroes(x))

  expect_true(is_tbl_now(out))
  # The grid is built from the dates that exist, so it reaches the `now`.
  expect_equal(max(pull(out, "event"), na.rm = TRUE), get_now(x))
  # The NA-report row is a case, not a cell: carried through, never deleted.
  expect_equal(sum(is.na(pull(out, "report"))), 1L)
  expect_equal(sum(pull(out, "n"), na.rm = TRUE), sum(pull(x, "n")))
})

test_that("complete_zeroes works when an event date is missing (#66)", {
  df <- tibble(
    event = as.Date(c("2020-01-01", "2020-01-01", NA, "2020-01-04")),
    report = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04")),
    n = c(3L, 2L, 4L, 1L)
  )
  x <- suppressWarnings(tbl_now(df,
    event_date = event, report_date = report, case_count = n,
    data_type = "count-incidence", verbose = FALSE
  ))

  out <- suppressWarnings(complete_zeroes(x))

  expect_true(is_tbl_now(out))
  expect_equal(sum(is.na(pull(out, "event"))), 1L)
  expect_equal(sum(pull(out, "n"), na.rm = TRUE), sum(pull(x, "n")))
})

test_that("complete_zeroes keeps .event_num for rows that are off the grid", {
  # A negative delay has no cell either, so the join that used to supply
  # `.event_num` matched nothing and blanked a perfectly known event number.
  df <- tibble(
    event = as.Date(c("2020-01-02", "2020-01-03", "2020-01-04")),
    report = as.Date(c("2020-01-01", "2020-01-04", "2020-01-05")),
    n = c(2L, 4L, 1L)
  )
  x <- suppressWarnings(tbl_now(df,
    event_date = event, report_date = report, case_count = n,
    data_type = "count-incidence", verbose = FALSE
  ))

  out <- suppressWarnings(complete_zeroes(x))

  expect_false(any(is.na(pull(out, ".event_num"))))
  expect_equal(sum(pull(out, "n"), na.rm = TRUE), sum(pull(x, "n")))
})

test_that("complete_zeroes refuses an object with no usable date pair", {
  df <- tibble(
    event = as.Date(c("2020-01-01", "2020-01-02")),
    report = as.Date(c(NA, NA)),
    n = c(3L, 2L)
  )
  x <- suppressWarnings(tbl_now(df,
    event_date = event, report_date = report, case_count = n,
    data_type = "count-incidence", units = "days", now = as.Date("2020-01-05"),
    verbose = FALSE
  ))

  expect_error(complete_zeroes(x), "no usable")
})

test_that("complete_zeroes works on a grouped tbl_now with missing dates (#66)", {
  df <- tibble(
    event = as.Date(rep(c("2020-01-01", "2020-01-02", "2020-01-04"), each = 2)),
    report = as.Date(c(
      "2020-01-01", NA, "2020-01-03", "2020-01-02", "2020-01-04", "2020-01-05"
    )),
    sex = rep(c("F", "M"), 3),
    n = c(3L, 2L, 4L, 1L, 5L, 2L)
  )
  x <- suppressWarnings(tbl_now(df,
    event_date = event, report_date = report, case_count = n, strata = sex,
    data_type = "count-incidence", verbose = FALSE
  ))

  out <- suppressWarnings(complete_zeroes(suppressWarnings(
    group_by(x, !!as.symbol("sex"))
  )))

  expect_true(is_tbl_now(out))
  expect_equal(group_vars(out), "sex")
  expect_equal(
    suppressWarnings(as_tibble(ungroup(out))),
    suppressWarnings(as_tibble(ungroup(complete_zeroes(x))))
  )
})
