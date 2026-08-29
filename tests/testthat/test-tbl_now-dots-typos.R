# `tbl_now()` keeps unmatched `...` names as metadata, so a misspelled argument
# name would otherwise be accepted in silence -- which is how `case_col` once sat
# in a shipped example, typing count data as a linelist.

make_data <- function() {
  data.frame(
    onset = as.Date("2020-01-01") + rep(0:9, each = 2),
    reported = as.Date("2020-01-01") + rep(0:9, each = 2) + c(1, 3),
    gender = rep(c("F", "M"), 10)
  )
}

build <- function(...) {
  tbl_now(make_data(),
    event_date = onset, report_date = reported, verbose = FALSE, ...
  )
}

test_that("a name that is nearly a real argument warns", {
  expect_warning(build(case_col = "n"), "case_count")
  expect_warning(build(stata = "gender"), "strata")
  expect_warning(build(reprot_date = "reported"), "report_date")
})

test_that("deliberate metadata does not warn", {
  expect_no_warning(build(data_source = "Ministry of Health"))
  expect_no_warning(build(citation = "doi:10.0000/example"))
  expect_no_warning(build(source = "surveillance system"))
  expect_no_warning(build(population = 1e6))
})

test_that("metadata is still stored, warning or not", {
  x <- suppressWarnings(build(case_col = "n"))
  expect_identical(attr(x, "case_col"), "n")

  y <- build(data_source = "Ministry of Health")
  expect_identical(attr(y, "data_source"), "Ministry of Health")
})

test_that("no dots means no warning", {
  expect_no_warning(build())
})
