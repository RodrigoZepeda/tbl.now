# `tidy()` for a `tbl_nowcast`. Every object here is built with the constructor
# rather than fitted: the method is pure reshaping, and the failures that bite
# it are reshaping failures (strata pooled under one label, a `level` assumed
# rather than read, quantiles handed to the wrong stratum).

nowcast_at <- function(levels, values, method = "toy",
                       dates = as.Date("2020-01-05") + c(0, 7),
                       strata = character(0), strata_values = NULL,
                       draws = NULL) {
  predictions <- tidyr::expand_grid(event_date = dates, .quantile_level = levels)
  predictions$.value <- rep(values, times = length(dates))
  if (!is.null(strata_values)) {
    predictions <- tidyr::expand_grid(predictions, !!!strata_values)
  }
  tbl_nowcast(
    predictions = predictions, draws = draws, method = method,
    event_date = "event_date", strata = strata,
    now = as.Date("2020-01-12")
  )
}

test_that("tidy() returns the package's standard nowcast frame", {
  nowcast <- nowcast_at(c(0.025, 0.5, 0.975), c(5, 10, 18))

  tidied <- tidy(nowcast)

  expect_s3_class(tidied, "tbl_df")
  expect_named(
    tidied,
    c("event_date", "stratum", "estimate", "conf.low", "conf.high", "level", "engine")
  )
  expect_s3_class(tidied$event_date, "Date")
  expect_type(tidied$stratum, "character")
  expect_type(tidied$estimate, "double")
  expect_equal(nrow(tidied), 2)
  expect_equal(tidied$estimate, c(10, 10))
  expect_equal(tidied$conf.low, c(5, 5))
  expect_equal(tidied$conf.high, c(18, 18))
})

test_that("`engine` is the method, including the ensemble's own name", {
  a <- nowcast_at(c(0.1, 0.5, 0.9), c(1, 2, 3), method = "a")
  b <- nowcast_at(c(0.1, 0.5, 0.9), c(3, 4, 5), method = "b")

  expect_equal(unique(tidy(a)$engine), "a")

  ensemble <- nowcast_ensemble(a, b, verbose = FALSE)
  expect_equal(unique(tidy(ensemble)$engine), "ensemble")

  named <- nowcast_ensemble(a, b, name = "hub", verbose = FALSE)
  expect_equal(unique(tidy(named)$engine), "hub")
})

test_that("`level` is read off the quantiles stored, never assumed", {
  # The default hub levels give a 95% band...
  default <- nowcast_at(nowcast_quantile_levels(), seq_len(9))
  expect_equal(unique(tidy(default)$level), 0.95)

  # ...but a fit summarised at 10/50/90 has an 80% one, and reporting 0.95 here
  # would put a wrong number in the column that exists to stop widths being
  # compared blindly.
  eighty <- nowcast_at(c(0.1, 0.5, 0.9), c(4, 10, 16))
  tidied <- tidy(eighty)
  expect_equal(unique(tidied$level), 0.8)
  expect_equal(tidied$conf.low, c(4, 4))
  expect_equal(tidied$conf.high, c(16, 16))

  # The WIDEST symmetric pair wins when several are present.
  nested <- nowcast_at(c(0.05, 0.25, 0.5, 0.75, 0.95), c(2, 7, 10, 13, 20))
  expect_equal(unique(tidy(nested)$level), 0.9)
})

test_that("an asymmetric set of levels gives NA bounds and an NA level", {
  lopsided <- nowcast_at(c(0.25, 0.5, 0.9), c(7, 10, 16))

  tidied <- tidy(lopsided)

  expect_true(all(is.na(tidied$level)))
  expect_true(all(is.na(tidied$conf.low)))
  expect_true(all(is.na(tidied$conf.high)))
  # The median is still there: only the interval is unavailable.
  expect_equal(tidied$estimate, c(10, 10))
})

test_that("`estimate` is NA when the nowcast carries no median", {
  no_median <- nowcast_at(c(0.05, 0.95), c(2, 20))

  tidied <- tidy(no_median)

  expect_true(all(is.na(tidied$estimate)))
  expect_equal(unique(tidied$level), 0.9)
  expect_equal(tidied$conf.low, c(2, 2))
})

test_that("stratum is 'all' only when the nowcast declares no strata", {
  expect_equal(unique(tidy(nowcast_at(c(0.1, 0.5, 0.9), 1:3))$stratum), "all")
})

test_that("strata are labelled and paired with their own values", {
  # The two strata differ by two orders of magnitude, so a mispairing is
  # arithmetically unmistakable rather than a plausible-looking table.
  dates <- as.Date("2020-01-05") + c(0, 7)
  predictions <- tidyr::expand_grid(
    event_date = dates, sex = c("F", "M"), .quantile_level = c(0.1, 0.5, 0.9)
  ) |>
    dplyr::mutate(
      .value = ifelse(.data$sex == "F", 10, 1000) + .data$.quantile_level
    )
  nowcast <- tbl_nowcast(
    predictions = predictions, method = "toy",
    event_date = "event_date", strata = "sex"
  )

  tidied <- tidy(nowcast)

  expect_setequal(tidied$stratum, c("F", "M"))
  expect_equal(nrow(tidied), 4)
  expect_true(all(tidied$estimate[tidied$stratum == "F"] < 100))
  expect_true(all(tidied$estimate[tidied$stratum == "M"] > 100))
  # (stratum, event_date) is the promised unique key
  expect_equal(nrow(dplyr::distinct(tidied, stratum, event_date)), 4)
})

test_that("several strata columns are pasted ' | '-separated", {
  predictions <- tidyr::expand_grid(
    event_date = as.Date("2020-01-05"),
    sex = c("F", "M"), region = c("north", "south"),
    .quantile_level = c(0.1, 0.5, 0.9)
  ) |>
    dplyr::mutate(.value = seq_len(dplyr::n()))
  nowcast <- tbl_nowcast(
    predictions = predictions, method = "toy",
    event_date = "event_date", strata = c("sex", "region")
  )

  tidied <- tidy(nowcast)

  expect_setequal(
    tidied$stratum,
    c("F | north", "F | south", "M | north", "M | south")
  )
  expect_equal(nrow(dplyr::distinct(tidied, stratum, event_date)), 4)
})

test_that("probs are honoured from draws, and per stratum", {
  set.seed(20260824)
  dates <- as.Date("2020-01-05") + c(0, 7)
  draws <- tidyr::expand_grid(
    event_date = dates, sex = c("F", "M"), .draw = seq_len(400)
  ) |>
    dplyr::mutate(
      .value = ifelse(.data$sex == "F", 10, 1000) +
        stats::qnorm(.data$.draw / 401)
    )
  predictions <- tbl.now:::.draws_to_quantiles(
    draws, c("event_date", "sex"), nowcast_quantile_levels()
  )
  nowcast <- tbl_nowcast(
    predictions = predictions, draws = draws, method = "toy",
    event_date = "event_date", strata = "sex"
  )

  tidied <- tidy(nowcast, probs = c(0.1, 0.9))

  expect_true(all(c("q10", "q90") %in% colnames(tidied)))
  expect_true(all(tidied$q10 < tidied$q90))
  # Each stratum's quantiles must come from ITS OWN draws
  expect_true(all(tidied$q90[tidied$stratum == "F"] < 100))
  expect_true(all(tidied$q10[tidied$stratum == "M"] > 100))
})

test_that("probs are refused when the nowcast keeps no draws", {
  quantiles_only <- nowcast_at(nowcast_quantile_levels(), seq_len(9))

  expect_error(tidy(quantiles_only, probs = 0.3), "does not keep posterior draws")
  expect_error(tidy(quantiles_only, probs = c(0.1, 0.9)), "probs")
})

test_that("tidy() of an empty nowcast is an empty standard frame", {
  empty <- tbl_nowcast(method = "toy", event_date = "event_date")

  tidied <- tidy(empty)

  expect_equal(nrow(tidied), 0)
  expect_named(
    tidied,
    c("event_date", "stratum", "estimate", "conf.low", "conf.high", "level", "engine")
  )
})

test_that("tidy() of a backtest gives one row per method, now date and target", {
  register_scoretoy()
  x <- score_tbl_now()
  dates <- as.Date(c("2020-06-01", "2020-06-08"))

  backtest <- nowcast_backtest(
    x,
    engine("scoretoy", bias = 0),
    engine("scoretoy2", bias = 25),
    now_dates = dates, verbose = FALSE
  )

  tidied <- tidy(backtest)

  expect_named(
    tidied,
    c(
      "method", "now", "event_date", "stratum", "observed", "estimate",
      "conf.low", "conf.high", "level", "wis", "ae_median", "coverage_50",
      "coverage_90"
    )
  )
  expect_setequal(unique(tidied$method), c("scoretoy", "scoretoy2"))
  expect_setequal(unique(tidied$now), dates)
  expect_equal(unique(tidied$stratum), "all")
  expect_equal(nrow(tidied), nrow(backtest$scores))
  # (method, now, stratum, event_date) is a unique key
  expect_equal(
    nrow(dplyr::distinct(tidied, method, now, stratum, event_date)),
    nrow(tidied)
  )
  # The biased model earns the larger WIS
  mean_wis <- tapply(tidied$wis, tidied$method, mean, na.rm = TRUE)
  expect_gt(mean_wis[["scoretoy2"]], mean_wis[["scoretoy"]])
})

test_that("tidy() of a backtest carries the predictions, not just the truth (#70)", {
  register_scoretoy()
  x <- score_tbl_now()
  dates <- as.Date(c("2020-06-01", "2020-06-08"))

  backtest <- nowcast_backtest(
    x,
    engine("scoretoy", bias = 0),
    engine("scoretoy2", bias = 25),
    now_dates = dates, verbose = FALSE
  )

  tidied <- tidy(backtest)

  # The three prediction columns are the quantiles the scores were computed
  # from, so they must equal the corresponding rows of `$predictions` -- not
  # merely be present and finite.
  at <- function(level) {
    backtest$predictions |>
      dplyr::filter(abs(.data$.quantile_level - level) < 1e-8) |>
      dplyr::transmute(
        method = .data$.method, now = .data$.now,
        event_date = .data$event_date, .value = .data$.value
      ) |>
      dplyr::arrange(.data$method, .data$now, .data$event_date) |>
      dplyr::pull(".value")
  }

  expect_equal(tidied$estimate, at(0.5))
  expect_equal(tidied$conf.low, at(0.025))
  expect_equal(tidied$conf.high, at(0.975))
  expect_equal(unique(tidied$level), 0.95)

  # The biased engine's median sits 25 above the unbiased one's, which is the
  # whole difference between the two members.
  medians <- tapply(tidied$estimate, tidied$method, mean)
  expect_equal(
    unname(medians[["scoretoy2"]] - medians[["scoretoy"]]), 25
  )

  # Adding columns must not add rows.
  expect_equal(nrow(tidied), nrow(backtest$scores))
})

test_that("tidy() of a backtest keys the predictions on the strata, not the date", {
  x <- score_tbl_now_strata()

  backtest <- nowcast_backtest(
    x, example_engine(label = "toy"),
    now_dates = as.Date("2020-06-01"), verbose = FALSE
  )

  tidied <- tidy(backtest)

  expect_setequal(unique(tidied$stratum), c("a", "b"))
  expect_equal(nrow(tidied), nrow(backtest$scores))

  # `b` is built at four times `a`, so joining on the event date alone -- the
  # bug this keys against -- would hand both strata whichever sorted first and
  # the two would come out identical.
  wide <- tidied |>
    dplyr::select("event_date", "stratum", "estimate") |>
    tidyr::pivot_wider(names_from = "stratum", values_from = "estimate")
  expect_false(isTRUE(all.equal(wide$a, wide$b)))
  expect_true(all(wide$b > wide$a))
})

test_that("tidy() of a backtest reports NA where the levels cannot support a bound", {
  register_scoretoy()
  x <- score_tbl_now()

  backtest <- nowcast_backtest(
    x, engine("scoretoy", quantile_levels = c(0.25, 0.4, 0.75)),
    now_dates = as.Date("2020-06-01"), verbose = FALSE
  )

  tidied <- tidy(backtest)

  # No symmetric pair among (0.25, 0.4, 0.75) -- 0.25/0.75 is symmetric, so
  # bounds exist; the median is what is absent here.
  expect_true(all(is.na(tidied$estimate)))
  expect_equal(unique(tidied$level), 0.5)
  expect_false(anyNA(tidied$conf.low))

  # And a set with no symmetric pair at all gives NA bounds and an NA level.
  backtest2 <- nowcast_backtest(
    x, engine("scoretoy", quantile_levels = c(0.25, 0.5, 0.9)),
    now_dates = as.Date("2020-06-01"), verbose = FALSE
  )
  tidied2 <- tidy(backtest2)

  expect_true(all(is.na(tidied2$conf.low)))
  expect_true(all(is.na(tidied2$conf.high)))
  expect_true(all(is.na(tidied2$level)))
  expect_false(anyNA(tidied2$estimate))
})
