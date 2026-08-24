# Scoring, backtesting and weight fitting. The toy backend from
# `test-run_nowcast.R` is re-registered here so the file can run standalone.

nowcast_fit.scoretoy <- function(method, x, ..., bias = 0, spread = 1,
                                 quantile_levels = nowcast_quantile_levels(),
                                 verbose = TRUE) {
  list(
    observed = nowcast_truth(x),
    bias = bias, spread = spread, event_col = get_event_date(x)
  )
}

nowcast_tidy.scoretoy <- function(method, fit, x, ..., quantile_levels) {
  predictions <- fit$observed |>
    dplyr::reframe(
      !!fit$event_col := rep(.data[[fit$event_col]], each = length(quantile_levels)),
      .quantile_level = rep(quantile_levels, times = dplyr::n()),
      .value = rep(.data$.observed + fit$bias, each = length(quantile_levels)) +
        fit$spread * stats::qnorm(rep(quantile_levels, times = dplyr::n()))
    )
  list(predictions = predictions, draws = NULL)
}

#' Register the toy backend twice, so that a two-model ensemble can be built
#' from a single implementation with different `bias` arguments.
register_scoretoy <- function() {
  for (name in c("scoretoy", "scoretoy2")) {
    registerS3method("nowcast_fit", name, nowcast_fit.scoretoy,
      envir = asNamespace("tbl.now")
    )
    registerS3method("nowcast_tidy", name, nowcast_tidy.scoretoy,
      envir = asNamespace("tbl.now")
    )
  }
}

score_tbl_now <- function() {
  dates <- as.Date("2020-01-06") + seq(0, 7 * 29, by = 7)
  data <- tidyr::expand_grid(event_date = dates, delay = 0:3) |>
    dplyr::mutate(
      report_date = .data$event_date + 7 * .data$delay,
      n = as.integer(pmax(1, 40 - 8 * .data$delay))
    ) |>
    dplyr::select("event_date", "report_date", "n")

  tbl_now(data,
    event_date = "event_date", report_date = "report_date",
    case_count = "n", data_type = "count-incidence", verbose = FALSE
  )
}

test_that(".wis() is zero for a perfect point prediction and grows with error", {
  levels <- c(0.25, 0.5, 0.75)

  expect_equal(tbl.now:::.wis(10, levels, rep(10, 3)), 0)
  expect_gt(tbl.now:::.wis(10, levels, rep(15, 3)), 0)
  # Being further away is worse
  expect_gt(
    tbl.now:::.wis(10, levels, rep(20, 3)),
    tbl.now:::.wis(10, levels, rep(15, 3))
  )
  # Symmetric over- and under-prediction cost the same at the median
  expect_equal(
    tbl.now:::.wis(10, 0.5, 15),
    tbl.now:::.wis(10, 0.5, 5)
  )
  expect_true(is.na(tbl.now:::.wis(NA_real_, levels, rep(10, 3))))
})

test_that(".covered() checks the right pair of quantiles", {
  levels <- c(0.05, 0.25, 0.5, 0.75, 0.95)
  predicted <- c(1, 4, 5, 6, 9)

  expect_true(tbl.now:::.covered(5, levels, predicted, 0.5))
  expect_false(tbl.now:::.covered(8, levels, predicted, 0.5))
  expect_true(tbl.now:::.covered(8, levels, predicted, 0.9))
  # 80% intervals were not reported
  expect_true(is.na(tbl.now:::.covered(5, levels, predicted, 0.8)))
})

test_that("score_nowcast() returns one row per target", {
  predictions <- tidyr::expand_grid(
    event_date = as.Date("2020-01-06") + c(0, 7),
    .quantile_level = c(0.25, 0.5, 0.75)
  ) |>
    dplyr::mutate(.value = rep(c(8, 10, 13), times = 2))

  nowcast <- tbl_nowcast(
    predictions = predictions, method = "toy", event_date = "event_date"
  )
  truth <- data.frame(
    event_date = as.Date("2020-01-06") + c(0, 7), .observed = c(10, 20)
  )

  scores <- score_nowcast(nowcast, truth = truth)

  expect_equal(nrow(scores), 2)
  expect_named(
    scores,
    c(
      ".method", "event_date", ".observed", "wis", "ae_median",
      "coverage_50", "coverage_90"
    )
  )
  expect_equal(scores$ae_median, c(0, 10))
  expect_equal(scores$coverage_50, c(TRUE, FALSE))
  # No 5%/95% quantiles were supplied
  expect_true(all(is.na(scores$coverage_90)))
})

test_that("score_nowcast() needs a truth it can find", {
  nowcast <- tbl_nowcast(
    predictions = data.frame(
      event_date = as.Date("2020-01-06"), .quantile_level = 0.5, .value = 1
    ),
    method = "toy", event_date = "event_date"
  )
  expect_error(score_nowcast(nowcast), "does not carry its source data")
  expect_error(
    score_nowcast(nowcast, truth = data.frame(event_date = as.Date("2020-01-06"), y = 1),
      observed_col = "nope"
    ),
    "was not found"
  )
})

test_that("a snapshot only keeps the reports available at that date", {
  x <- score_tbl_now()
  cutoff <- as.Date("2020-03-02")
  snapshot <- tbl.now:::.nowcast_snapshot(x, cutoff)

  expect_equal(get_now(snapshot), cutoff)
  expect_true(all(snapshot$report_date <= cutoff))
  expect_lt(nrow(snapshot), nrow(x))
})

test_that("nowcast_backtest() scores every method at every date", {
  register_scoretoy()
  x <- score_tbl_now()
  dates <- as.Date(c("2020-06-01", "2020-06-08"))

  backtest <- nowcast_backtest(
    x,
    methods = "scoretoy", now_dates = dates, verbose = FALSE
  )

  expect_s3_class(backtest, "nowcast_backtest")
  expect_equal(backtest$methods, "scoretoy")
  expect_equal(backtest$now_dates, dates)
  expect_setequal(unique(backtest$scores$.now), dates)
  expect_true(all(c(".method", ".now", "wis") %in% colnames(backtest$scores)))
})

test_that("nowcast_backtest() validates its inputs", {
  x <- score_tbl_now()
  expect_error(nowcast_backtest(mtcars, methods = "scoretoy"), "tbl_now")
  expect_error(nowcast_backtest(x, methods = character(0)), "at least one")
})

test_that("a failing method is skipped with a warning, or aborts on request", {
  register_scoretoy()
  registerS3method("nowcast_fit", "brokentoy",
    function(method, x, ..., quantile_levels, verbose = TRUE) stop("nope"),
    envir = asNamespace("tbl.now")
  )
  x <- score_tbl_now()
  dates <- as.Date("2020-06-01")

  expect_warning(
    backtest <- nowcast_backtest(
      x,
      methods = c("scoretoy", "brokentoy"), now_dates = dates, verbose = FALSE
    ),
    "failed"
  )
  expect_equal(backtest$methods, "scoretoy")

  expect_error(
    nowcast_backtest(x,
      methods = "brokentoy", now_dates = dates,
      on_error = "abort", verbose = FALSE
    ),
    "failed"
  )
})

test_that("weights reward the better model", {
  register_scoretoy()
  x <- score_tbl_now()
  dates <- as.Date(c("2020-06-01", "2020-06-08", "2020-06-15"))

  backtest <- nowcast_backtest(
    x,
    methods = c("scoretoy", "scoretoy2"),
    now_dates = dates, verbose = FALSE,
    method_args = list(scoretoy = list(bias = 0), scoretoy2 = list(bias = 25))
  )
  expect_equal(sort(backtest$methods), c("scoretoy", "scoretoy2"))

  equal <- nowcast_weights(backtest, "equal")
  expect_equal(unname(equal), c(0.5, 0.5))

  inverse <- nowcast_weights(backtest, "inverse_score")
  expect_equal(sum(inverse), 1)
  # The unbiased model must get the larger share
  expect_gt(inverse[["scoretoy"]], inverse[["scoretoy2"]])

  optimised <- nowcast_weights(backtest, "optim")
  expect_equal(sum(optimised), 1)
  expect_gt(optimised[["scoretoy"]], optimised[["scoretoy2"]])
})

test_that("nowcast_weights() rejects anything that is not a backtest", {
  expect_error(nowcast_weights(mtcars), "nowcast_backtest")
})

test_that("performance weights flow into nowcast_ensemble()", {
  register_scoretoy()
  x <- score_tbl_now()
  dates <- as.Date(c("2020-06-01", "2020-06-08"))

  backtest <- nowcast_backtest(
    x,
    methods = c("scoretoy", "scoretoy2"), now_dates = dates, verbose = FALSE,
    method_args = list(scoretoy = list(bias = 0), scoretoy2 = list(bias = 25))
  )

  good <- run_nowcast(x, "scoretoy", bias = 0, verbose = FALSE)
  bad <- run_nowcast(x, "scoretoy2", bias = 25, verbose = FALSE)

  ensemble <- nowcast_ensemble(
    good, bad,
    weights = "inverse_score", backtest = backtest, verbose = FALSE
  )
  weights <- ensemble@metadata$weights

  expect_equal(sum(weights), 1)
  expect_gt(weights[["scoretoy"]], weights[["scoretoy2"]])
})

test_that("as_scoringutils() produces the expected column names", {
  predictions <- data.frame(
    event_date = as.Date("2020-01-06"),
    .quantile_level = c(0.25, 0.5, 0.75), .value = c(8, 10, 13)
  )
  nowcast <- tbl_nowcast(
    predictions = predictions, method = "toy", event_date = "event_date"
  )
  truth <- data.frame(event_date = as.Date("2020-01-06"), .observed = 11)

  exported <- as_scoringutils(nowcast, truth = truth)

  expect_true(all(
    c("observed", "predicted", "quantile_level", "model") %in% colnames(exported)
  ))
  expect_equal(nrow(exported), 3)

  skip_if_not_installed("scoringutils")
  expect_no_error(scoringutils::as_forecast_quantile(as.data.frame(exported)))
})
