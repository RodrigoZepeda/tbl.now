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

# The invariants ------------------------------------------------------------
#
# These are the properties an ensemble must have whatever the arithmetic inside
# is. Each of them fails loudly for one specific way of getting the combination
# wrong: crossing quantiles for a bad interpolation, a result outside the
# members' range for a normalisation slip, a shifted result for a weight applied
# to the wrong member, and idempotence for essentially every averaging bug.

quantile_values <- function(nowcast, level) {
  nowcast@predictions |>
    dplyr::filter(abs(.data$.quantile_level - level) < 1e-8) |>
    dplyr::pull(".value")
}

test_that("the combined quantiles are monotone in the quantile level", {
  a <- fake_nowcast("a", c(1, 2, 3, 4, 5))
  b <- fake_nowcast("b", c(0, 10, 20, 30, 40))

  monotone <- function(nowcast) {
    nowcast@predictions |>
      dplyr::group_by(.data$event_date) |>
      dplyr::summarise(
        ok = !is.unsorted(.data$.value[order(.data$.quantile_level)]),
        .groups = "drop"
      )
  }

  expect_true(all(monotone(nowcast_ensemble(a, b, verbose = FALSE))$ok))
  expect_true(all(
    monotone(nowcast_ensemble(a, b, weights = c(0.9, 0.1), verbose = FALSE))$ok
  ))

  pooled <- nowcast_ensemble(
    fake_draws_nowcast("b", 10), fake_draws_nowcast("c", 40),
    type = "linear_pool", n_draws = 1000, verbose = FALSE
  )
  expect_true(all(monotone(pooled)$ok))
})

test_that("an equally weighted ensemble sits between its members at every level", {
  a <- fake_nowcast("a", c(1, 2, 3, 4, 5))
  b <- fake_nowcast("b", c(0, 10, 20, 30, 40))

  ensemble <- nowcast_ensemble(a, b, verbose = FALSE)

  for (level in c(0.1, 0.25, 0.5, 0.75, 0.9)) {
    lower <- pmin(quantile_values(a, level), quantile_values(b, level))
    upper <- pmax(quantile_values(a, level), quantile_values(b, level))
    combined <- quantile_values(ensemble, level)

    expect_true(all(combined >= lower & combined <= upper),
      info = paste("level", level)
    )
  }
})

test_that("increasing a member's weight moves the ensemble towards it, monotonically", {
  a <- fake_nowcast("a", c(1, 2, 3, 4, 5))
  b <- fake_nowcast("b", c(0, 10, 20, 30, 40))

  shares <- seq(0, 1, by = 0.25)
  medians <- vapply(shares, function(w) {
    ensemble <- nowcast_ensemble(a, b, weights = c(a = w, b = 1 - w), verbose = FALSE)
    quantile_values(ensemble, 0.5)[[1]]
  }, numeric(1))

  # `a`'s median (3) is below `b`'s (20), so weighting `a` more must move the
  # ensemble DOWN, without ever turning back.
  expect_true(all(diff(medians) < 0))
  expect_equal(medians[[1]], quantile_values(b, 0.5)[[1]])
  expect_equal(medians[[length(medians)]], quantile_values(a, 0.5)[[1]])
})

test_that("combining a nowcast with itself returns that nowcast", {
  a <- fake_nowcast("a", c(1, 2, 3, 4, 5))
  copy <- fake_nowcast("a_copy", c(1, 2, 3, 4, 5))

  idempotent <- nowcast_ensemble(a, copy, verbose = FALSE)
  expect_equal(idempotent@predictions$.value, a@predictions$.value)

  # Unequal weights cannot change an average of identical numbers either
  lopsided <- nowcast_ensemble(a, copy, weights = c(0.99, 0.01), verbose = FALSE)
  expect_equal(lopsided@predictions$.value, a@predictions$.value)

  # The linear pool re-samples, so it agrees only up to sampling noise -- but it
  # must still be centred on the member rather than shifted or widened.
  set.seed(20260824)
  draws_member <- fake_draws_nowcast("d", 10)
  draws_copy <- fake_draws_nowcast("d_copy", 10)
  pooled <- nowcast_ensemble(
    draws_member, draws_copy,
    type = "linear_pool", n_draws = 4000, verbose = FALSE
  )
  expect_equal(
    quantile_values(pooled, 0.5), quantile_values(draws_member, 0.5),
    tolerance = 0.05
  )
  expect_equal(
    quantile_values(pooled, 0.9), quantile_values(draws_member, 0.9),
    tolerance = 0.05
  )
})

test_that("no member is silently dropped when the levels differ", {
  a <- fake_nowcast("a", c(1, 2, 3), levels = c(0.25, 0.5, 0.75))
  b <- fake_nowcast("b", c(1, 2, 3, 4, 5), levels = c(0.1, 0.25, 0.5, 0.75, 0.9))

  expect_warning(
    ensemble <- nowcast_ensemble(a, b, verbose = FALSE),
    "different quantile levels"
  )

  # Both members must still be counted and weighted: dropping the one with the
  # narrower level set would silently turn the ensemble into a single model.
  expect_equal(ensemble@metadata$members, c("a", "b"))
  expect_equal(unname(ensemble@metadata$weights), c(0.5, 0.5))
  expect_equal(
    quantile_values(ensemble, 0.5),
    (quantile_values(a, 0.5) + quantile_values(b, 0.5)) / 2
  )
})

test_that("the quantile ensemble is narrower than the linear pool", {
  # The textbook contrast: averaging quantiles pulls the tails in, pooling draws
  # turns between-model disagreement into extra spread.
  set.seed(20260824)
  b <- fake_draws_nowcast("b", 10)
  c_member <- fake_draws_nowcast("c", 20)

  averaged <- nowcast_ensemble(b, c_member, type = "quantile", verbose = FALSE)
  pooled <- nowcast_ensemble(
    b, c_member,
    type = "linear_pool", n_draws = 4000, verbose = FALSE
  )

  width <- function(nowcast) {
    quantile_values(nowcast, 0.95) - quantile_values(nowcast, 0.05)
  }
  expect_true(all(width(pooled) > width(averaged)))
})
