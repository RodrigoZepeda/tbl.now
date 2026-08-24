# Ensembling is pure arithmetic on the tidy predictions, so these tests build
# `tbl_nowcast` objects directly rather than fitting anything.

fake_nowcast <- function(method, values, levels = c(0.1, 0.25, 0.5, 0.75, 0.9),
                         dates = as.Date("2020-01-06") + c(0, 7),
                         draws = NULL) {
  predictions <- tidyr::expand_grid(event_date = dates, .quantile_level = levels) |>
    dplyr::mutate(.value = rep(values, times = length(dates)))

  tbl_nowcast(
    predictions = predictions, draws = draws, method = method,
    event_date = "event_date", now = as.Date("2020-01-13")
  )
}

fake_draws_nowcast <- function(method, centre, n = 500,
                               dates = as.Date("2020-01-06") + c(0, 7)) {
  set.seed(1)
  draws <- tidyr::expand_grid(event_date = dates, .draw = seq_len(n)) |>
    dplyr::mutate(.value = centre + stats::qnorm(.data$.draw / (n + 1)))

  predictions <- tbl.now:::.draws_to_quantiles(
    draws, "event_date", nowcast_quantile_levels()
  )

  tbl_nowcast(
    predictions = predictions, draws = draws, method = method,
    event_date = "event_date", now = as.Date("2020-01-13")
  )
}

test_that("the quantile ensemble averages the members level by level", {
  a <- fake_nowcast("a", c(1, 2, 3, 4, 5))
  b <- fake_nowcast("b", c(3, 4, 5, 6, 7))

  ensemble <- nowcast_ensemble(a, b, verbose = FALSE)

  expect_true(is_tbl_nowcast(ensemble))
  expect_equal(ensemble@method, "ensemble")
  expect_equal(
    ensemble@predictions$.value,
    rep(c(2, 3, 4, 5, 6), times = 2)
  )
  expect_equal(unname(ensemble@metadata$weights), c(0.5, 0.5))
})

test_that("unequal weights shift the ensemble towards the heavier member", {
  a <- fake_nowcast("a", c(1, 2, 3, 4, 5))
  b <- fake_nowcast("b", c(3, 4, 5, 6, 7))

  ensemble <- nowcast_ensemble(a, b, weights = c(a = 0.75, b = 0.25), verbose = FALSE)
  expect_equal(ensemble@predictions$.value, rep(c(1.5, 2.5, 3.5, 4.5, 5.5), times = 2))

  # Weights are normalised, so their scale does not matter
  scaled <- nowcast_ensemble(a, b, weights = c(a = 3, b = 1), verbose = FALSE)
  expect_equal(scaled@predictions$.value, ensemble@predictions$.value)

  # Unnamed weights follow the order the members were given in
  positional <- nowcast_ensemble(a, b, weights = c(0.75, 0.25), verbose = FALSE)
  expect_equal(positional@predictions$.value, ensemble@predictions$.value)
})

test_that("weights are validated", {
  a <- fake_nowcast("a", c(1, 2, 3, 4, 5))
  b <- fake_nowcast("b", c(3, 4, 5, 6, 7))

  expect_error(nowcast_ensemble(a, b, weights = c(0.5), verbose = FALSE), "length")
  expect_error(nowcast_ensemble(a, b, weights = c(a = 1, c = 1), verbose = FALSE), "no entry")
  expect_error(nowcast_ensemble(a, b, weights = c(-1, 2), verbose = FALSE), "non-negative")
  expect_error(nowcast_ensemble(a, b, weights = c(0, 0), verbose = FALSE), "sum to zero")
  expect_error(
    nowcast_ensemble(a, b, weights = "inverse_score", verbose = FALSE),
    "needs a"
  )
})

test_that("an ensemble needs at least two compatible members", {
  a <- fake_nowcast("a", c(1, 2, 3, 4, 5))
  expect_error(nowcast_ensemble(a, verbose = FALSE), "at least two")
  expect_error(nowcast_ensemble(a, mtcars, verbose = FALSE), "tbl_nowcast")

  other_column <- fake_nowcast("b", c(1, 2, 3, 4, 5))
  other_column <- tbl_nowcast(
    predictions = dplyr::rename(other_column@predictions, onset = "event_date"),
    method = "b", event_date = "onset"
  )
  expect_error(nowcast_ensemble(a, other_column, verbose = FALSE), "different event-date")
})

test_that("members can be passed as a single list, and are named uniquely", {
  a <- fake_nowcast("a", c(1, 2, 3, 4, 5))
  b <- fake_nowcast("a", c(3, 4, 5, 6, 7))

  ensemble <- nowcast_ensemble(list(a, b), verbose = FALSE)
  # Both members report method "a"; the second is disambiguated
  expect_equal(ensemble@metadata$members, c("a", "a_1"))
})

test_that("mismatched quantile levels fall back to the shared ones", {
  a <- fake_nowcast("a", c(1, 2, 3), levels = c(0.25, 0.5, 0.75))
  b <- fake_nowcast("b", c(1, 2, 3, 4, 5), levels = c(0.1, 0.25, 0.5, 0.75, 0.9))

  expect_warning(
    ensemble <- nowcast_ensemble(a, b, verbose = FALSE),
    "different quantile levels"
  )
  expect_equal(sort(unique(ensemble@predictions$.quantile_level)), c(0.25, 0.5, 0.75))

  expect_error(
    nowcast_ensemble(a, b, quantile_levels = 0.9, verbose = FALSE),
    "not available in every nowcast"
  )
})

test_that("the linear pool needs draws and produces them", {
  a <- fake_nowcast("a", c(1, 2, 3, 4, 5))
  b <- fake_draws_nowcast("b", 10)

  expect_error(
    nowcast_ensemble(a, b, type = "linear_pool", verbose = FALSE),
    "needs draws from every member"
  )

  c_member <- fake_draws_nowcast("c", 20)
  pooled <- nowcast_ensemble(b, c_member, type = "linear_pool", n_draws = 400, verbose = FALSE)

  expect_false(is.null(pooled@draws))
  expect_equal(length(unique(pooled@draws$.draw)), 400)
  # A 50/50 mixture of two well separated components straddles both
  median <- pooled@predictions |> dplyr::filter(.data$.quantile_level == 0.5)
  expect_true(all(median$.value > 10 & median$.value < 20))
})

test_that("the linear pool splits draws according to the weights", {
  b <- fake_draws_nowcast("b", 10)
  c_member <- fake_draws_nowcast("c", 20)

  pooled <- nowcast_ensemble(
    b, c_member,
    type = "linear_pool", weights = c(b = 0.9, c = 0.1),
    n_draws = 1000, verbose = FALSE
  )

  # 90% of the pooled draws come from the component centred at 10
  first_date <- pooled@draws |>
    dplyr::filter(.data$event_date == min(.data$event_date))
  expect_equal(mean(first_date$.value < 15), 0.9, tolerance = 0.02)
})

test_that(".allocate_draws() splits exactly n_draws", {
  weights <- c(a = 1 / 3, b = 1 / 3, c = 1 / 3)
  allocated <- tbl.now:::.allocate_draws(weights, 100)

  expect_equal(sum(allocated), 100)
  expect_named(allocated, c("a", "b", "c"))
  expect_true(all(abs(allocated - 100 / 3) < 1))
})

test_that("differing now dates warn but still combine", {
  a <- fake_nowcast("a", c(1, 2, 3, 4, 5))
  b <- fake_nowcast("b", c(3, 4, 5, 6, 7))
  b@now <- as.Date("2020-01-20")

  expect_warning(nowcast_ensemble(a, b, verbose = FALSE), "different")
})
