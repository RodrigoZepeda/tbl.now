# Targets one member has and another does not.
#
# Both combination rules group by the prediction target and average what they
# find there. A target with only one member present therefore comes out as that
# member's own value, at full weight, labelled as the ensemble -- a silent
# single-member point in the middle of a combined series.
#
# This is not hypothetical. `EpiNow2::estimate_infections()` forecasts one period
# past the end of the data, so on a weekly object it returns an extra week beyond
# `now`. That lands at the `now` edge, which is the part of a nowcast picture
# people actually read.

one_member <- function(dates, value, method, draws = FALSE) {
  predictions <- tidyr::expand_grid(
    event_date = dates, .quantile_level = c(0.025, 0.5, 0.975)
  ) |>
    dplyr::mutate(.value = value)
  drawn <- if (draws) {
    tidyr::expand_grid(event_date = dates, .draw = 1:50) |>
      dplyr::mutate(.value = value)
  } else {
    NULL
  }
  tbl_nowcast(
    predictions = predictions, draws = drawn,
    method = method, event_date = "event_date"
  )
}

WEEKS <- as.Date("2010-09-06") + c(0, 7, 14, 21)

test_that("a target only one member covers is dropped, not averaged", {
  a <- one_member(WEEKS, 100, "A")
  b <- one_member(c(WEEKS, max(WEEKS) + 7), 10, "B")

  expect_warning(
    ens <- suppressMessages(
      nowcast_ensemble(list(A = a, B = b), weights = c(A = 0.5, B = 0.5))
    ),
    "not every member covers"
  )

  out <- dplyr::filter(as_tibble(ens), .data$.quantile_level == 0.5)
  # The extra week is gone rather than reported as 10 -- B's own value, which is
  # what it read before this was fixed.
  expect_false((max(WEEKS) + 7) %in% out$event_date)
  expect_setequal(out$event_date, WEEKS)
  expect_true(all(out$.value == 55))
})

test_that("the linear pool drops uncovered targets too", {
  a <- one_member(WEEKS, 100, "A", draws = TRUE)
  b <- one_member(c(WEEKS, max(WEEKS) + 7), 10, "B", draws = TRUE)

  expect_warning(
    ens <- suppressMessages(nowcast_ensemble(
      list(A = a, B = b), weights = c(A = 0.5, B = 0.5), type = "linear_pool"
    )),
    "not every member covers"
  )
  expect_setequal(unique(as_tibble(ens)$event_date), WEEKS)
  expect_setequal(unique(as_tibble(ens, type = "draws")$event_date), WEEKS)
})

test_that("members that agree on their targets are untouched and silent", {
  a <- one_member(WEEKS, 100, "A")
  b <- one_member(WEEKS, 10, "B")

  expect_no_warning(
    ens <- suppressMessages(
      nowcast_ensemble(list(A = a, B = b), weights = c(A = 0.5, B = 0.5))
    )
  )
  expect_setequal(unique(as_tibble(ens)$event_date), WEEKS)
})

test_that("strata are part of the target, not just the date", {
  make <- function(levels, method) {
    predictions <- tidyr::expand_grid(
      event_date = WEEKS, sex = levels, .quantile_level = 0.5
    ) |>
      dplyr::mutate(.value = 1)
    tbl_nowcast(
      predictions = predictions, method = method,
      event_date = "event_date", strata = "sex"
    )
  }
  # Same dates, but B carries a stratum A never fitted.
  expect_warning(
    ens <- suppressMessages(nowcast_ensemble(
      list(A = make("F", "A"), B = make(c("F", "M"), "B")),
      weights = c(A = 0.5, B = 0.5)
    )),
    "not every member covers"
  )
  expect_setequal(unique(as_tibble(ens)$sex), "F")
})
