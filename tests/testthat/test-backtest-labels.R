# One package, two models.
#
# `nowcast_ensemble()` takes a NAMED list, so a member can be "the same package,
# a different model" -- two `diseasenowcasting` fits with different epidemic
# processes, say. `nowcast_backtest()` took bare method names, so it could not
# tell those apart: both scored under the package's name, and `nowcast_weights()`
# learned one weight for what were two different models. An engine carries a
# `label`, so two configurations of one package are two members.

backtest_tbl_now <- function() {
  d <- data.frame(
    ev = rep(seq(as.Date("2024-01-01"), by = "1 day", length.out = 40), each = 2),
    rp = rep(seq(as.Date("2024-01-01"), by = "1 day", length.out = 40), each = 2)
  )
  d$rp <- d$ev + rep(c(0, 2), nrow(d) / 2)
  d$n <- 5
  tbl_now(d, event_date = "ev", report_date = "rp", case_count = "n",
          data_type = "count-incidence", verbose = FALSE)
}

test_that("engine labels reach the scores, the predictions and the weights", {
  skip_on_cran()
  skip_if_not_installed("baselinenowcast")
  x <- backtest_tbl_now()
  dates <- get_now(x) - c(2, 1)

  bt <- suppressWarnings(suppressMessages(nowcast_backtest(
    x,
    engine_baselinenowcast(draws = 100, label = "few"),
    engine_baselinenowcast(draws = 400, label = "many"),
    now_dates = dates, seed = 1, verbose = FALSE
  )))

  # Scores and predictions must agree on the label. They did not before: scores
  # took `@method` (the package) while predictions took the label, so the two
  # tables could not be joined on `.method` at all.
  expect_setequal(unique(bt$scores$.method), c("few", "many"))
  expect_setequal(unique(bt$predictions$.method), c("few", "many"))
  expect_setequal(bt$methods, c("few", "many"))

  weights <- nowcast_weights(bt)
  expect_setequal(names(weights), c("few", "many"))
  expect_equal(sum(weights), 1, tolerance = 1e-8)
})

test_that("an argument name overrides the engine's own label", {
  skip_on_cran()
  skip_if_not_installed("baselinenowcast")
  x <- backtest_tbl_now()

  bt <- suppressWarnings(suppressMessages(nowcast_backtest(
    x,
    renamed = engine_baselinenowcast(draws = 100, label = "few"),
    now_dates = get_now(x) - 1, seed = 1, verbose = FALSE
  )))
  expect_equal(bt$methods, "renamed")
})

test_that("an unlabelled engine is labelled by its own method", {
  skip_on_cran()
  skip_if_not_installed("baselinenowcast")
  x <- backtest_tbl_now()

  bt <- suppressWarnings(suppressMessages(nowcast_backtest(
    x, engine_baselinenowcast(draws = 100), now_dates = get_now(x) - 1,
    seed = 1, verbose = FALSE
  )))
  expect_equal(unique(bt$scores$.method), "baselinenowcast")
  expect_equal(bt$methods, "baselinenowcast")
})

test_that("a list of engines is accepted as well as loose arguments", {
  skip_on_cran()
  skip_if_not_installed("baselinenowcast")
  x <- backtest_tbl_now()

  # `lapply()` over a set of configurations produces a list; refusing it would
  # send the caller to `do.call()` for no reason.
  bt <- suppressWarnings(suppressMessages(nowcast_backtest(
    x,
    list(a = engine_baselinenowcast(draws = 100),
         b = engine_baselinenowcast(draws = 200)),
    now_dates = get_now(x) - 1, seed = 1, verbose = FALSE
  )))
  expect_setequal(bt$methods, c("a", "b"))
})

test_that("duplicate labels abort rather than collapsing two models into one", {
  skip_on_cran()
  skip_if_not_installed("baselinenowcast")
  x <- backtest_tbl_now()

  # Same package twice with no labels: both would be "baselinenowcast", so the
  # result would carry two rows per date under one name and the weights would be
  # meaningless.
  expect_error(
    nowcast_backtest(x, engine_baselinenowcast(), engine_baselinenowcast(),
                     now_dates = get_now(x) - 1, verbose = FALSE),
    "[Dd]uplicate engine label"
  )
  # And the error says how to fix it.
  expect_error(
    nowcast_backtest(x, engine_baselinenowcast(), engine_baselinenowcast(),
                     now_dates = get_now(x) - 1, verbose = FALSE),
    "label ="
  )
})

test_that("engines reporting different quantile levels are refused", {
  x <- backtest_tbl_now()

  # The WIS averages over the levels reported, so two engines summarised at
  # different levels are not scoring the same quantity -- and NobBS cannot be
  # re-summarised after the fit, so this cannot be patched up downstream.
  expect_error(
    nowcast_backtest(
      x,
      a = engine("testtoy"),
      b = engine("testtoy", quantile_levels = c(0.1, 0.5, 0.9)),
      now_dates = get_now(x) - 1, verbose = FALSE
    ),
    "different .*quantile_levels"
  )
})

test_that("nowcast_backtest() refuses a bare method name", {
  skip_on_cran()
  x <- backtest_tbl_now()
  expect_error(
    nowcast_backtest(x, "baselinenowcast", now_dates = get_now(x) - 1),
    "engine_baselinenowcast"
  )
  expect_error(nowcast_backtest(x, now_dates = get_now(x) - 1), "at least one engine")
})

test_that("run_nowcast() records the engine label, so `inverse_score` can find it", {
  x <- backtest_tbl_now()

  # `example_engine()` needs no modelling package and is deterministic, so this
  # is about the labelling and nothing else.
  tight <- example_engine(spread = 0.1, label = "tight")
  wide <- example_engine(spread = 0.8, label = "wide")

  fit_tight <- run_nowcast(x, tight, verbose = FALSE)
  fit_wide <- run_nowcast(x, wide, verbose = FALSE)

  # The regression: the fit recorded the PACKAGE while the backtest scored the
  # LABEL, so two configurations of one backend were indistinguishable here and
  # `nowcast_ensemble()` aborted with "the backtest has no scores for method".
  expect_equal(fit_tight@method, "tight")
  expect_equal(fit_wide@method, "wide")

  bt <- nowcast_backtest(
    x, tight, wide, now_dates = get_now(x) - c(2, 1), verbose = FALSE
  )
  ensemble <- nowcast_ensemble(
    fit_tight, fit_wide, weights = "inverse_score", backtest = bt
  )
  expect_true(is_tbl_nowcast(ensemble))
  expect_setequal(names(ensemble@metadata$weights), c("tight", "wide"))
})

test_that("an unlabelled engine still records its package as the method", {
  x <- backtest_tbl_now()
  expect_equal(run_nowcast(x, example_engine(), verbose = FALSE)@method, "example")
})
