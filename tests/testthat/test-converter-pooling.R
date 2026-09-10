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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
  skip_if_not_installed("tsibble")

  x <- pooled_fixture()
  total <- sum(x$n)

  # Before this change: "A valid tsibble must have distinct rows identified by
  # key and index".
  converted <- suppressWarnings(tbl_now_to_tsibble(x, verbose = FALSE))

  expect_equal(sum(converted$n), total)
})

test_that("three undeclared levels pool as readily as two", {
  skip_on_cran()
  skip_if_not_installed("baselinenowcast")

  x <- pooled_fixture(n_sex = 3)
  triangle <- suppressWarnings(tbl_now_to_baselinenowcast(x, verbose = FALSE))

  expect_equal(sum(triangle, na.rm = TRUE), sum(x$n))
})

test_that("a declared stratum is NOT pooled away", {
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
  x <- pooled_fixture()

  expect_message_quietly(
    tbl.now:::.pool_undeclared(x, "tbl_now_to_baselinenowcast", verbose = TRUE),
    "sex"
  )
  expect_no_message(
    tbl.now:::.pool_undeclared(x, "tbl_now_to_baselinenowcast", verbose = FALSE)
  )
})

# --- the delay-distribution front ends ---------------------------------------
#
# epidist and `EpiNow2::estimate_dist()` are the two converters that keep the
# report-censoring flag, so they were also the two that never pooled. Neither
# carries an undeclared column onto its result, so the extra rows arrived as
# duplicates that no downstream code could tell apart: `covid_colombia` filtered
# to 90 days came back as 3456 epidist rows covering 1898 distinct delays.

test_that("epidist pools an undeclared column, preserving the total", {
  skip_on_cran()
  skip_if_not_installed("epidist")

  x <- pooled_fixture()
  converted <- suppressWarnings(suppressMessages(
    tbl_now_to_epidist(x, verbose = FALSE, quiet = TRUE)
  ))

  expect_equal(sum(converted$n), sum(x$n))
  expect_equal(nrow(converted), nrow(dplyr::distinct(dplyr::as_tibble(x),
    .data$event_date, .data$report_date
  )))
})

test_that("EpiNow2 estimate_dist pools an undeclared column too", {
  skip_on_cran()
  skip_if_not_installed("EpiNow2")

  x <- pooled_fixture()
  converted <- suppressWarnings(suppressMessages(
    tbl_now_to_EpiNow2(x, target = "estimate_dist", verbose = FALSE, quiet = TRUE)
  ))

  expect_equal(sum(converted$n), sum(x$n))
  expect_equal(nrow(converted), nrow(dplyr::distinct(dplyr::as_tibble(x),
    .data$event_date, .data$report_date
  )))
})

test_that("every epidist row is a distinct delay observation", {
  skip_on_cran()
  skip_if_not_installed("epidist")

  # Everything at once: an undeclared column, a revision axis (dropped, so its
  # columns cannot separate rows either) and a censored report inside its own
  # event period (whose window is widened onto the uncensored one's).
  data <- tidyr::expand_grid(
    event_date = as.Date("2020-01-06") + 0:9,
    delay = 0:2,
    sex = c("F", "M"),
    outcome = c("confirmed", "retracted")
  ) |>
    dplyr::mutate(
      report_date = .data$event_date + .data$delay,
      revision_date = .data$report_date + 2L,
      censored = .data$delay == 0 & .data$sex == "F",
      n = 1L
    )

  x <- suppressWarnings(tbl_now(data,
    event_date = "event_date", report_date = "report_date",
    revision_date = "revision_date", revision_type = "outcome",
    is_censored_report = "censored", case_count = "n",
    data_type = "count-incidence", units = "days", verbose = FALSE
  ))

  converted <- suppressWarnings(suppressMessages(
    tbl_now_to_epidist(x, verbose = FALSE, quiet = TRUE)
  ))

  # No case is lost, and no row is a duplicate of another.
  expect_equal(sum(converted$n), sum(data$n))
  expect_equal(
    nrow(dplyr::distinct(dplyr::as_tibble(converted))), nrow(converted)
  )
})

test_that("a declared stratum survives the epidist pooling as a covariate", {
  skip_on_cran()
  skip_if_not_installed("epidist")

  x <- pooled_fixture() |> add_strata(sex)
  converted <- suppressWarnings(suppressMessages(
    tbl_now_to_epidist(x, verbose = FALSE, quiet = TRUE)
  ))

  # Declaring the column is how you ask epidist to model it; it must stay.
  expect_true("sex" %in% names(converted))
  expect_equal(nrow(converted), nrow(x))
  expect_equal(sum(converted$n), sum(x$n))
})

test_that("an epidist line list is not pooled: one row is one case", {
  skip_on_cran()
  skip_if_not_installed("epidist")

  linelist <- suppressWarnings(tbl_now(
    data.frame(
      event_date = as.Date("2020-01-06") + c(0, 0, 1, 1),
      report_date = as.Date("2020-01-07") + c(0, 0, 1, 1),
      sex = c("F", "M", "F", "M")
    ),
    event_date = "event_date", report_date = "report_date",
    data_type = "linelist", verbose = FALSE
  ))

  converted <- suppressWarnings(suppressMessages(
    tbl_now_to_epidist(linelist, verbose = FALSE, quiet = TRUE)
  ))
  expect_equal(nrow(converted), 4L)
})

test_that("removing the revision process keeps the `now`", {
  skip_on_cran()
  # `remove_revision_date()` rebuilt without passing `now`, so the constructor
  # re-inferred it from the dates that were left -- and the revision axis is the
  # one that usually holds the latest of them. `tbl_now_to_epidist()` turns `now`
  # into `obs_date`, epidist's right-truncation clock, so the loss was silent
  # and material.
  x <- suppressWarnings(tbl_now(
    data.frame(
      event = as.Date("2024-01-01") + 0:3,
      report = as.Date("2024-01-04") + 0:3,
      revision = as.Date("2024-01-06") + 0:3,
      outcome = "confirmed",
      n = 1L
    ),
    event_date = "event", report_date = "report",
    revision_date = "revision", revision_type = "outcome",
    case_count = "n", data_type = "count-incidence", units = "days",
    now = as.Date("2024-02-01"), verbose = FALSE
  ))

  expect_equal(get_now(remove_revision_date(x)), as.Date("2024-02-01"))
})

# max_delay -------------------------------------------------------------------

test_that("max_delay counts delay periods the way epinowcast does", {
  skip_on_cran()
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

test_that("max_delay is revised", {
  skip_on_cran()
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
  skip_on_cran()
  x <- pooled_fixture()
  expect_equal(nrow(tbl.now:::.cap_max_delay(x, 500, "f", verbose = FALSE)), nrow(x))
})
