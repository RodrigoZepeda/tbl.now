# The reported- and validated-cases getters, issues #61 and #64.
#
# Every expected number in this file was worked out by hand from the fixture
# below, which is small enough to hold in your head. It is written out here so
# that a failure tells you which cell moved rather than only that a total did.
#
#   row  onset  visit  result  outcome    sex  region  hospital   n
#   1    d0     d0     d0      confirmed  F    N       A           1
#   2    d0     d1     d1      retracted  F    N       B           2
#   3    d0     d0     d2      confirmed  M    S       B           4
#   4    d0     d2     NA      pending    M    S       B           8
#   5    d1     d1     d1      confirmed  F    S       A          16
#   6    d1     d3     d3      confirmed  M    N       B          32
#
# The counts are powers of two so that any subset sums to a number that names
# its own members: 12 is rows 3 and 4, and nothing else.

cases_frame <- function() {
  day <- function(offsets) as.Date("2021-01-04") + offsets
  data.frame(
    onset = day(c(0, 0, 0, 0, 1, 1)),
    visit = day(c(0, 1, 0, 2, 1, 3)),
    result = day(c(0, 1, 2, NA, 1, 3)),
    outcome = c(
      "confirmed", "retracted", "confirmed", "pending", "confirmed", "confirmed"
    ),
    sex = c("F", "F", "M", "M", "F", "M"),
    region = c("N", "N", "S", "S", "S", "N"),
    hospital = c("A", "B", "B", "B", "A", "B"),
    n = c(1L, 2L, 4L, 8L, 16L, 32L),
    stringsAsFactors = FALSE
  )
}

# Stratified by sex, with `region` as a covariate and `hospital` declared
# nowhere -- so a grouping is the only way to ask about it, which is the case
# issue #61 was filed for.
cases_fixture <- function(strata = "sex", covariates = "region",
                          validation = TRUE) {
  arguments <- list(
    cases_frame(),
    event_date = "onset", report_date = "visit",
    strata = strata, covariates = covariates,
    case_count = "n", data_type = "count-incidence", units = "days",
    verbose = FALSE, warn_non_uniqueness = FALSE
  )
  if (isTRUE(validation)) {
    arguments <- c(arguments, list(
      validation_date = "result", validation_type = "outcome"
    ))
  }
  do.call(tbl_now, arguments)
}

# The reporting axis ----------------------------------------------------------

test_that("the reporting getters count what the fixture says they should", {
  x <- cases_fixture()

  # Cells per (onset, sex, region), cumulated over the visit:
  #   (d0, F, N) 1 then 3   (d0, M, S) 4 then 12
  #   (d1, F, S) 16         (d1, M, N) 32
  latest <- get_latest_reported_cases(x)
  expect_equal(latest[["n"]], c(3, 12, 16, 32))
  expect_equal(latest[["sex"]], c("F", "M", "F", "M"))
  expect_equal(latest[["region"]], c("N", "S", "S", "N"))
  # Nothing is lost or invented: every case is somewhere in the answer.
  expect_equal(sum(latest[["n"]]), sum(cases_frame()[["n"]]))

  expect_equal(get_initial_reported_cases(x)[["n"]], c(1, 4, 16, 32))

  # `.delay` is 0, 1, 0, 2, 0, 2 for rows 1..6.
  expect_equal(get_nth_reported_cases(x, delay = 0)[["n"]], c(1, 4, 16))
  expect_equal(get_nth_reported_cases(x, delay = 1)[["n"]], c(3, 4, 16))
  expect_equal(
    get_nth_reported_cases(x, delay = Inf)[["n"]], latest[["n"]]
  )

  # An event date with nothing inside the delay is absent, not zero.
  within_one <- get_nth_reported_cases(x, delay = 1)
  expect_equal(nrow(within_one), 3L)
  expect_false(any(within_one[["sex"]] == "M" & within_one[["region"]] == "N"))
})

test_that("the reporting getters pool what they were not told about", {
  # No strata, no covariates: sex, region and hospital are all undeclared, so
  # every case for an event date lands in one cell.
  x <- cases_fixture(strata = NULL, covariates = NULL)

  latest <- get_latest_reported_cases(x)
  # d0: 1 + 4 at visit d0, then 2, then 8 -> 15. d1: 16 then 32 -> 48.
  expect_equal(latest[["n"]], c(15, 48))
  expect_equal(nrow(latest), 2L)
  expect_false("sex" %in% names(latest))
})

test_that("a covariate but no strata keys on the covariate alone", {
  x <- cases_fixture(strata = NULL, covariates = "region")

  latest <- get_latest_reported_cases(x)
  # (d0, N) 1 then 3; (d0, S) 4 then 12; (d1, N) 32; (d1, S) 16.
  expect_equal(latest[["region"]], c("N", "S", "N", "S"))
  expect_equal(latest[["n"]], c(3, 12, 32, 16))
})

test_that("the getters return a count-cumulative tbl_now", {
  x <- cases_fixture()
  latest <- get_latest_reported_cases(x)

  expect_true(is_tbl_now(latest))
  expect_equal(get_data_type(latest), "count-cumulative")
  expect_equal(get_case_count(latest), "n")
  expect_equal(get_strata(latest), "sex")
  expect_true(all(c(".event_num", ".report_num", ".delay") %in% names(latest)))
})

# Grouping (#61) --------------------------------------------------------------

test_that("the reporting getters keep a grouping and answer by it", {
  x <- cases_fixture()

  # Grouping by a column that is ALREADY a key changes nothing but the
  # grouping itself.
  by_stratum <- x |> dplyr::group_by(sex) |> get_latest_reported_cases()
  expect_true(is_tbl_now(by_stratum))
  expect_equal(dplyr::group_vars(by_stratum), "sex")
  expect_equal(
    dplyr::as_tibble(dplyr::ungroup(by_stratum)),
    dplyr::as_tibble(dplyr::ungroup(get_latest_reported_cases(x)))
  )

  # Grouping by two other columns -- one covariate, one declared nowhere --
  # splits the cells, which is the whole point: `hospital` is reachable no
  # other way.
  by_two <- x |>
    dplyr::group_by(region, hospital) |>
    get_latest_reported_cases()

  expect_true(is_tbl_now(by_two))
  expect_equal(dplyr::group_vars(by_two), c("region", "hospital"))
  expect_true("hospital" %in% names(by_two))
  # (d0,F,N,A) 1; (d0,F,N,B) 2; (d0,M,S,B) 4 then 12;
  # (d1,F,S,A) 16; (d1,M,N,B) 32.
  expect_equal(by_two[["n"]], c(1, 2, 12, 16, 32))
  expect_equal(by_two[["hospital"]], c("A", "B", "B", "A", "B"))
  # Still every case, just split more finely.
  expect_equal(sum(by_two[["n"]]), sum(cases_frame()[["n"]]))

  # And it is genuinely a different answer from the ungrouped one, which is
  # what #61 asked for.
  expect_false(identical(
    by_two[["n"]], get_latest_reported_cases(x)[["n"]]
  ))
})

test_that("grouping is kept by the initial and nth getters too", {
  x <- cases_fixture() |> dplyr::group_by(region, hospital)

  for (out in list(
    get_initial_reported_cases(x),
    get_nth_reported_cases(x, delay = 2),
    get_latest_validated_cases(x),
    get_initial_validated_cases(x),
    get_nth_validated_cases(x, delay = 2)
  )) {
    expect_true(is_tbl_now(out))
    expect_equal(dplyr::group_vars(out), c("region", "hospital"))
  }
})

test_that("a grouping survives an object with no validation process", {
  x <- cases_fixture(validation = FALSE)
  expect_false(has_validation(x))

  out <- x |> dplyr::group_by(hospital) |> get_latest_reported_cases()
  expect_true(is_tbl_now(out))
  expect_equal(dplyr::group_vars(out), "hospital")
  # (d0,F,N,A) 1; (d0,F,N,B) 2; (d0,M,S,B) 12; (d1,F,S,A) 16; (d1,M,N,B) 32.
  expect_equal(out[["n"]], c(1, 2, 12, 16, 32))
})

test_that("to_count() warns that it is dropping the grouping (#61)", {
  x <- cases_fixture()

  expect_warning(
    pooled <- x |> dplyr::group_by(hospital) |> to_count("count-incidence"),
    "dropped the grouping"
  )
  expect_equal(dplyr::group_vars(pooled), character(0))
  expect_true(is_tbl_now(pooled))
  # It really did pool: `hospital` was summed away, so the six rows become the
  # four (onset, visit) cells the declared columns describe.
  expect_equal(sum(pooled[["n"]]), sum(cases_frame()[["n"]]))

  # Ungrouped, it says nothing.
  expect_no_warning(to_count(x, "count-incidence"))
})

test_that("to_count() does not warn about a grouping it set itself", {
  # The linelist -> cumulative path recurses through `to_count()` twice, on an
  # object this function had just grouped by the cell key.
  linelist <- tbl_now(
    cases_frame()[rep(seq_len(6), cases_frame()[["n"]]), c("onset", "visit")],
    event_date = "onset", report_date = "visit",
    data_type = "linelist", units = "days",
    verbose = FALSE, warn_non_uniqueness = FALSE
  )
  expect_no_warning(to_count(linelist, "count-cumulative"))
  expect_no_warning(to_count(linelist, "count-incidence"))
})

# The validation axis (#64) ---------------------------------------------------

test_that("the validated getters count arrivals on the third date", {
  x <- cases_fixture()

  # Row 4 is pending, so it never appears: cells per (onset, sex, region),
  # cumulated over the RESULT date.
  #   (d0, F, N) 1 at d0 then 3 at d1     (d0, M, S) 4 at d2
  #   (d1, F, S) 16 at d1                 (d1, M, N) 32 at d3
  latest <- get_latest_validated_cases(x)
  expect_equal(latest[["n"]], c(3, 4, 16, 32))
  expect_equal(
    latest[["result"]],
    as.Date("2021-01-04") + c(1, 2, 1, 3)
  )
  # The eight pending cases are exactly what is missing from the reported total.
  expect_equal(
    sum(get_latest_reported_cases(x)[["n"]]) - sum(latest[["n"]]), 8
  )

  expect_equal(get_initial_validated_cases(x)[["n"]], c(1, 4, 16, 32))

  # The delay counted is from the EVENT: 0, 1, 2, -, 0, 2 for rows 1..6.
  expect_equal(get_nth_validated_cases(x, delay = 0)[["n"]], c(1, 16))
  expect_equal(get_nth_validated_cases(x, delay = 1)[["n"]], c(3, 16))
  expect_equal(get_nth_validated_cases(x, delay = 2)[["n"]], c(3, 4, 16, 32))
})

test_that("the validated getters return the full three-date object", {
  x <- cases_fixture()
  latest <- get_latest_validated_cases(x)

  expect_true(is_tbl_now(latest))
  expect_true(has_validation(latest))
  expect_equal(get_validation_date(latest), "result")
  expect_equal(get_data_type(latest), "count-cumulative")
  expect_true(all(
    c(".validation_num", ".validation_delay") %in% names(latest)
  ))
  # `"total"` pools outcomes, so the aggregate row has none rather than one of
  # the cases' own.
  expect_true(all(is.na(latest[["outcome"]])))
})

test_that("type = filters the outcome on both axes", {
  x <- cases_fixture()

  # Confirmed: rows 1, 3, 5, 6 -- one per cell, so both axes agree.
  expect_equal(
    get_latest_reported_cases(x, type = "confirmed")[["n"]], c(1, 4, 16, 32)
  )
  expect_equal(
    get_latest_validated_cases(x, type = "confirmed")[["n"]], c(1, 4, 16, 32)
  )

  # Retracted: row 2 alone.
  retracted <- get_latest_validated_cases(x, type = "retracted")
  expect_equal(nrow(retracted), 1L)
  expect_equal(retracted[["n"]], 2)
  expect_equal(retracted[["outcome"]], "retracted")

  # Pending: row 4 alone, and only the reporting axis can answer it.
  pending <- get_latest_reported_cases(x, type = "pending")
  expect_equal(nrow(pending), 1L)
  expect_equal(pending[["n"]], 8)
  expect_error(
    get_latest_validated_cases(x, type = "pending"), "has no validation date"
  )

  # Net: +1 confirmed, -1 retracted. (d0, F, N) is 1 then 1 - 2 = -1.
  expect_equal(
    get_latest_validated_cases(x, type = "net")[["n"]], c(-1, 4, 16, 32)
  )
})

test_that("type = 'by_type' reports every outcome side by side", {
  x <- cases_fixture()

  # The reporting axis keeps all four rows of event d0, pending included.
  reported <- get_latest_reported_cases(x, type = "by_type")
  expect_equal(reported[["n"]], c(1, 2, 4, 8, 16, 32))
  expect_equal(
    reported[["outcome"]],
    c("confirmed", "retracted", "confirmed", "pending", "confirmed", "confirmed")
  )
  # The outcome column is not something you nowcast BY, but it must survive an
  # aggregation, so it comes back declared as a covariate.
  expect_true("outcome" %in% get_covariates(reported))
  expect_equal(sum(reported[["n"]]), sum(cases_frame()[["n"]]))

  # The validation axis drops the pending row: it has not arrived there.
  validated <- get_latest_validated_cases(x, type = "by_type")
  expect_equal(validated[["n"]], c(1, 2, 4, 16, 32))
  expect_false("pending" %in% validated[["outcome"]])
  expect_equal(get_validation_type(validated), "outcome")
})

test_that("type = respects a grouping too", {
  x <- cases_fixture() |> dplyr::group_by(hospital)

  confirmed <- get_latest_validated_cases(x, type = "confirmed")
  expect_equal(dplyr::group_vars(confirmed), "hospital")
  # Rows 1 (A), 3 (B), 5 (A), 6 (B) -- already one per cell, so splitting by
  # hospital changes nothing but the key.
  expect_equal(confirmed[["n"]], c(1, 4, 16, 32))
  expect_equal(confirmed[["hospital"]], c("A", "B", "A", "B"))
})

# No validation process, and a validation that is all NA ----------------------

test_that("the validated getters refuse an object with no validation", {
  x <- cases_fixture(validation = FALSE)

  expect_error(get_latest_validated_cases(x), "needs a validation process")
  expect_error(get_initial_validated_cases(x), "needs a validation process")
  expect_error(get_nth_validated_cases(x, 1), "needs a validation process")
})

test_that("type = on an object with no validation warns and pools", {
  x <- cases_fixture(validation = FALSE)

  expect_warning(
    pooled <- get_latest_reported_cases(x, type = "confirmed"),
    "no validation process"
  )
  expect_equal(pooled[["n"]], get_latest_reported_cases(x)[["n"]])

  # `"total"` is the default and asks nothing of the outcome, so it is silent.
  expect_no_warning(get_latest_reported_cases(x, type = "total"))
})

test_that("an all-NA validation date is an error that says so", {
  frame <- cases_frame()
  frame$result <- as.Date(NA)
  frame$outcome <- "pending"
  x <- tbl_now(frame,
    event_date = "onset", report_date = "visit",
    validation_date = "result", validation_type = "outcome",
    strata = "sex", covariates = "region", case_count = "n",
    data_type = "count-incidence", units = "days",
    verbose = FALSE, warn_non_uniqueness = FALSE
  )

  expect_true(has_validation(x))
  expect_error(get_latest_validated_cases(x), "nothing has been validated yet")
  expect_error(get_initial_validated_cases(x), "selected no cases")

  # The reporting axis still works, and it is where a pending case is counted.
  expect_equal(sum(get_latest_reported_cases(x)[["n"]]), 63)
  expect_equal(
    sum(get_latest_reported_cases(x, type = "pending")[["n"]]), 63
  )

  # An outcome nothing has is an empty selection, and it says which.
  expect_error(
    get_latest_reported_cases(x, type = "confirmed"), "No case has"
  )
})

test_that("an empty nth selection names the delay rather than failing later", {
  # Every report is a day late, so nothing at all arrives at delay 0. Without
  # the guard this dies inside `tbl_now()` with "`data` is an empty
  # data.frame", which says nothing about the question that was asked.
  slow <- tbl_now(
    data.frame(
      onset = as.Date("2021-01-04") + 0:2,
      visit = as.Date("2021-01-05") + 0:2,
      n = c(1L, 2L, 4L)
    ),
    event_date = "onset", report_date = "visit", case_count = "n",
    data_type = "count-incidence", units = "days", verbose = FALSE
  )

  expect_equal(get_nth_reported_cases(slow, delay = 1)[["n"]], c(1, 2, 4))
  expect_error(
    get_nth_reported_cases(slow, delay = 0),
    "No case arrived within a delay"
  )
})

test_that("the getters check their arguments", {
  x <- cases_fixture()

  expect_error(get_nth_reported_cases(x, "two"), "non-negative number")
  expect_error(get_nth_reported_cases(x, -1), "non-negative number")
  expect_error(get_nth_validated_cases(x, NA), "non-negative number")
  expect_error(get_latest_reported_cases(x, type = "nonsense"), "must be one of")
  expect_error(get_latest_reported_cases(x, type = c("a", "b")), "single string")
})
