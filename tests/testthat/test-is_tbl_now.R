# `is_tbl_now()` is a question about the CLASS, not about the data (#62).
#
# It used to run `validate_tbl_now()` inside a `tryCatch()` that caught errors
# but not warnings, so a findings warning escaped from wherever the predicate
# happened to be called -- which is every `.assert_tbl_now()` in the package.

messy_fixture <- function() {
  suppressWarnings(tbl_now(
    data.frame(
      onset = as.Date("2020-01-01") + 0:3,
      reported = as.Date(c("2020-01-03", NA, "2222-02-22", "2020-01-06"))
    ),
    event_date = "onset", report_date = "reported",
    data_type = "linelist", units = "days", now = as.Date("2020-01-10"),
    verbose = FALSE
  ))
}

test_that("is_tbl_now() is silent on an object validate_tbl_now() complains about", {
  x <- messy_fixture()

  # The object is malformed data in a well-formed container, and the two
  # questions get two answers.
  expect_no_warning(expect_true(is_tbl_now(x)))
  # Two findings: the missing report date, and a `now` before the last report.
  expect_warning(
    expect_warning(validate_tbl_now(x), "NA values"), "seems to be in the past"
  )
})

test_that("is_tbl_now() does not run the findings engine at all", {
  x <- messy_fixture()

  # If the predicate still called it, this mock would abort.
  local_mocked_bindings(
    validate_tbl_now = function(...) {
      cli::cli_abort("`is_tbl_now()` must not run the validator.")
    }
  )
  expect_true(is_tbl_now(x))
  expect_false(is_tbl_now(data.frame(a = 1)))
})

test_that("is_tbl_now() checks the class, its attributes and their columns", {
  x <- tbl_now(
    data.frame(
      onset = as.Date("2021-01-04") + 0:3,
      visit = as.Date("2021-01-05") + 0:3,
      sex = c("F", "M", "F", "M")
    ),
    event_date = "onset", report_date = "visit", strata = "sex",
    data_type = "linelist", units = "days", verbose = FALSE
  )
  expect_true(is_tbl_now(x))

  # Not the class.
  expect_false(is_tbl_now(data.frame(x = 1:3)))
  expect_false(is_tbl_now(list(a = 1)))
  expect_false(is_tbl_now(dplyr::as_tibble(x)))

  # The class, but missing an attribute it cannot do without.
  for (attribute in c(
    "event_date", "report_date", "data_type", "now", "event_units",
    "report_units"
  )) {
    broken <- x
    attr(broken, attribute) <- NULL
    expect_false(is_tbl_now(broken), info = attribute)
  }

  # The class and the attributes, but an attribute naming a column that is
  # gone. This is the shape that hid a real bug: the getters do not check the
  # class, so a demoted object kept answering for itself. The attributes are
  # set directly here because the dplyr and base methods repair them, which is
  # exactly why the predicate has to check rather than assume.
  gone <- x
  attr(gone, "event_date") <- "not_a_column"
  expect_false(is_tbl_now(gone))

  no_strata <- x
  attr(no_strata, "strata") <- "not_a_column"
  expect_false(is_tbl_now(no_strata))

  # A generated column counts too: `.delay` is protected.
  no_delay <- x
  no_delay[[".delay"]] <- NULL
  expect_false(is_tbl_now(no_delay))
})

test_that("is_tbl_now() is TRUE for a grouped tbl_now", {
  x <- tbl_now(
    data.frame(
      onset = as.Date("2021-01-04") + 0:3,
      visit = as.Date("2021-01-05") + 0:3,
      sex = c("F", "M", "F", "M")
    ),
    event_date = "onset", report_date = "visit", strata = "sex",
    data_type = "linelist", units = "days", verbose = FALSE
  )

  grouped <- dplyr::group_by(x, sex)
  expect_s3_class(grouped, "grouped_tbl_now")
  expect_no_warning(expect_true(is_tbl_now(grouped)))
})

test_that("a verb that fixes a problem does not re-report it (#62)", {
  x <- messy_fixture()

  # `censor_reports()` asserts the class, rebuilds, and asserts again. Before
  # #62 each assertion re-emitted the object's findings, so fixing the missing
  # report date warned about the missing report date -- twice, after the fix.
  fixed <- suppressMessages(
    censor_reports(x, is.na(reported), to_report = as.Date("2020-01-10"))
  )
  expect_true(is_tbl_now(fixed))
  expect_false(anyNA(fixed[["reported"]]))
  expect_no_warning(is_tbl_now(fixed))
})

test_that("validate_tbl_now() still answers loudly", {
  # Nothing about #62 makes the validator quieter; it is only no longer run
  # from the predicate.
  expect_error(validate_tbl_now(data.frame(x = 1:3)))
  expect_warning(
    expect_warning(validate_tbl_now(messy_fixture()), "NA values"),
    "seems to be in the past"
  )
  expect_true(validate_tbl_now(tbl_now(
    data.frame(
      onset = as.Date("2021-01-04") + 0:3,
      visit = as.Date("2021-01-05") + 0:3
    ),
    event_date = "onset", report_date = "visit",
    data_type = "linelist", units = "days", verbose = FALSE
  )))
})
