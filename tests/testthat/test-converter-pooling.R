# Converters must not make the caller aggregate first.
#
# `covid_colombia` is the motivating shape: it carries `sex`, so an object built
# without `strata = sex` has TWO rows per (event, report) cell. A reporting
# triangle, a tsibble key and an epinowcast observation table each have exactly
# one slot per cell, so the extra dimension has to be pooled away.
#
# The assertion that matters everywhere below is on the CASE TOTAL, not on the
# absence of an error: pooling that drops or double-counts a row still "works".

pooled_fixture <- function(n_sex = 2, cases_per_row = 10L) {
  data <- tidyr::expand_grid(
    event_date = as.Date("2020-01-06") + 0:19,
    delay = 0:3,
    sex = head(c("F", "M", "X"), n_sex)
  ) |>
    dplyr::mutate(
      report_date = .data$event_date + .data$delay,
      n = cases_per_row
    ) |>
    dplyr::select("event_date", "report_date", "sex", "n")

  suppressWarnings(tbl_now(data,
    event_date = "event_date", report_date = "report_date",
    case_count = "n", data_type = "count-incidence",
    event_units = "days", report_units = "days", verbose = FALSE
  ))
}

test_that("an undeclared column is reported, and named, at construction", {
  expect_warning(
    x <- tbl_now(
      data.frame(
        event_date = as.Date("2020-01-06") + c(0, 0, 1, 1),
        report_date = as.Date("2020-01-06") + c(0, 0, 1, 1),
        sex = c("F", "M", "F", "M"), n = 1L
      ),
      event_date = "event_date", report_date = "report_date",
      case_count = "n", data_type = "count-incidence", verbose = FALSE
    ),
    "sex"
  )

  # The old message recommended `distinct()`, which cannot fix this -- those rows
  # ARE distinct, they differ in `sex` -- and on data with real repeats it
  # deletes cases. Recommending it here is what sent a user in a circle.
  warning <- tryCatch(
    tbl_now(
      data.frame(
        event_date = as.Date("2020-01-06") + c(0, 0, 1, 1),
        report_date = as.Date("2020-01-06") + c(0, 0, 1, 1),
        sex = c("F", "M", "F", "M"), n = 1L
      ),
      event_date = "event_date", report_date = "report_date",
      case_count = "n", data_type = "count-incidence", verbose = FALSE
    ),
    warning = function(w) conditionMessage(w)
  )
  expect_no_match(warning, "distinct")
  expect_match(warning, "strata")
})

test_that("genuine duplicates are still told to use distinct()", {
  duplicated_rows <- data.frame(
    event_date = as.Date("2020-01-06") + c(0, 0, 1),
    report_date = as.Date("2020-01-06") + c(0, 0, 1),
    n = c(1L, 1L, 2L)
  )
  warning <- tryCatch(
    tbl_now(duplicated_rows,
      event_date = "event_date", report_date = "report_date",
      case_count = "n", data_type = "count-incidence", verbose = FALSE
    ),
    warning = function(w) conditionMessage(w)
  )
  expect_match(warning, "duplicates")
  expect_match(warning, "distinct")
})

test_that("declaring the column removes the warning entirely", {
  expect_no_warning(
    tbl_now(
      data.frame(
        event_date = as.Date("2020-01-06") + c(0, 0, 1, 1),
        report_date = as.Date("2020-01-06") + c(0, 0, 1, 1),
        sex = c("F", "M", "F", "M"), n = 1L
      ),
      event_date = "event_date", report_date = "report_date",
      case_count = "n", strata = "sex", data_type = "count-incidence",
      verbose = FALSE
    )
  )
})

test_that("baselinenowcast pools an undeclared column, preserving the total", {
  skip_if_not_installed("baselinenowcast")

  x <- pooled_fixture()
  total <- sum(x$n)

  triangle <- suppressWarnings(
    tbl_now_to_baselinenowcast(x, verbose = FALSE)
  )

  # Before this change the converter aborted: "duplicate reference_date and
  # report_date combinations".
  expect_true(is.matrix(triangle) || inherits(triangle, "reporting_triangle"))
  expect_equal(sum(triangle, na.rm = TRUE), total)
})

test_that("tsibble pools an undeclared column, preserving the total", {
  skip_if_not_installed("tsibble")

  x <- pooled_fixture()
  total <- sum(x$n)

  # Before this change: "A valid tsibble must have distinct rows identified by
  # key and index".
  converted <- suppressWarnings(tbl_now_to_tsibble(x, verbose = FALSE))

  expect_equal(sum(converted$n), total)
})

test_that("three undeclared levels pool as readily as two", {
  skip_if_not_installed("baselinenowcast")

  x <- pooled_fixture(n_sex = 3)
  triangle <- suppressWarnings(tbl_now_to_baselinenowcast(x, verbose = FALSE))

  expect_equal(sum(triangle, na.rm = TRUE), sum(x$n))
})

test_that("a declared stratum is NOT pooled away", {
  skip_if_not_installed("baselinenowcast")

  x <- pooled_fixture() |> add_strata(sex)
  triangles <- suppressWarnings(
    tbl_now_to_baselinenowcast(x, format = "triangle_list", verbose = FALSE)
  )

  # Declaring the column is how you ask for it to be modelled separately; the
  # pooling helper must leave it alone.
  expect_named(triangles, c("F", "M"))
  expect_equal(
    sum(vapply(triangles, sum, numeric(1), na.rm = TRUE)), sum(x$n)
  )
})

test_that("line lists are left alone: one row is already one case", {
  linelist <- suppressWarnings(tbl_now(
    data.frame(
      event_date = as.Date("2020-01-06") + c(0, 0, 1, 1),
      report_date = as.Date("2020-01-06") + c(0, 1, 1, 2),
      sex = c("F", "M", "F", "M")
    ),
    event_date = "event_date", report_date = "report_date",
    data_type = "linelist", verbose = FALSE
  ))

  expect_equal(nrow(tbl.now:::.pool_undeclared(linelist, "test", verbose = FALSE)), 4L)
})

test_that("the pooling is announced under verbose and silent otherwise", {
  x <- pooled_fixture()

  expect_message(
    tbl.now:::.pool_undeclared(x, "tbl_now_to_baselinenowcast", verbose = TRUE),
    "sex"
  )
  expect_no_message(
    tbl.now:::.pool_undeclared(x, "tbl_now_to_baselinenowcast", verbose = FALSE)
  )
})

# max_delay -------------------------------------------------------------------

test_that("max_delay counts delay periods the way epinowcast does", {
  skip_if_not_installed("baselinenowcast")

  x <- pooled_fixture() # delays 0..3
  capped <- suppressWarnings(
    tbl_now_to_baselinenowcast(x, max_delay = 3, verbose = FALSE)
  )
  uncapped <- suppressWarnings(tbl_now_to_baselinenowcast(x, verbose = FALSE))

  # `max_delay = 3` means delays 0, 1, 2 -- three columns, matching
  # `enw_preprocess_data(max_delay = 3)`. An off-by-one between the two
  # converters would be invisible until two engines disagreed.
  expect_equal(ncol(capped), 3L)
  expect_equal(ncol(uncapped), 4L)

  # The dropped delay carried real cases, so the total must fall by exactly them
  dropped <- sum(x$n[x$.delay == 3])
  expect_gt(dropped, 0)
  expect_equal(sum(capped, na.rm = TRUE), sum(uncapped, na.rm = TRUE) - dropped)
})

test_that("max_delay is validated", {
  x <- pooled_fixture()

  expect_error(tbl.now:::.cap_max_delay(x, 0, "f"), "at least 1")
  expect_error(tbl.now:::.cap_max_delay(x, -1, "f"), "at least 1")
  expect_error(tbl.now:::.cap_max_delay(x, 2.5, "f"), "whole number")
  expect_error(tbl.now:::.cap_max_delay(x, c(1, 2), "f"), "single")
  expect_error(tbl.now:::.cap_max_delay(x, "30", "f"), "whole number")

  # NULL means no cap, and must not touch the object
  expect_identical(tbl.now:::.cap_max_delay(x, NULL, "f"), x)
})

test_that("a max_delay beyond the data is a no-op rather than an error", {
  x <- pooled_fixture()
  expect_equal(nrow(tbl.now:::.cap_max_delay(x, 500, "f", verbose = FALSE)), nrow(x))
})
