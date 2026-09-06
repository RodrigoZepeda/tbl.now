# Scoring, backtesting and weight fitting. The `scoretoy` backend and the data
# it is fitted to live in `helper-nowcast.R`, so `test-tidy-nowcast.R` can build
# a backtest from the same pieces rather than growing a second copy.

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
  truth <- truth_tbl_now(as.Date("2020-01-06") + c(0, 7), c(10, 20), units = "weeks")

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

  # A bare data frame is refused: the count column is read off the `tbl_now`
  # with `get_case_count()`, so there is nothing for the caller to name and
  # nothing to guess at.
  expect_error(
    score_nowcast(nowcast, truth = data.frame(event_date = as.Date("2020-01-06"), y = 1)),
    "must be a <tbl_now>"
  )
  expect_error(
    as_scoringutils(nowcast, truth = data.frame(event_date = as.Date("2020-01-06"), y = 1)),
    "must be a <tbl_now>"
  )
})

test_that("the truth's observed column is read off the object, line list included", {
  # The point of dropping `observed_col`: a line list has NO count column, and a
  # count object names its own. Both must score identically, because they are
  # the same data.
  dates <- as.Date("2020-01-06") + c(0, 7)
  counts <- truth_tbl_now(dates, c(3, 2))
  linelist <- tbl_now(
    data.frame(
      event_date = rep(dates, times = c(3, 2)),
      rp = rep(dates, times = c(3, 2))
    ),
    event_date = "event_date", report_date = "rp", verbose = FALSE
  )
  expect_null(get_case_count(linelist))

  predictions <- tidyr::expand_grid(
    event_date = dates, .quantile_level = c(0.25, 0.5, 0.75)
  ) |>
    dplyr::mutate(.value = rep(c(2, 3, 4), times = 2))
  nowcast <- tbl_nowcast(
    predictions = predictions, method = "toy", event_date = "event_date"
  )

  expect_equal(
    score_nowcast(nowcast, truth = counts)$.observed,
    score_nowcast(nowcast, truth = linelist)$.observed
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
    x, engine("scoretoy"), now_dates = dates, verbose = FALSE
  )

  expect_s3_class(backtest, "nowcast_backtest")
  expect_equal(backtest$methods, "scoretoy")
  expect_equal(backtest$now_dates, dates)
  expect_setequal(unique(backtest$scores$.now), dates)
  expect_true(all(c(".method", ".now", "wis") %in% colnames(backtest$scores)))
})

test_that("nowcast_backtest() validates its inputs", {
  x <- score_tbl_now()
  expect_error(nowcast_backtest(mtcars, engine("scoretoy")), "tbl_now")
  expect_error(nowcast_backtest(x), "at least one engine")
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
      x, engine("scoretoy"), engine("brokentoy"),
      now_dates = dates, verbose = FALSE
    ),
    "failed"
  )
  expect_equal(backtest$methods, "scoretoy")

  expect_error(
    nowcast_backtest(x, engine("brokentoy"),
      now_dates = dates, on_error = "abort", verbose = FALSE
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
    engine("scoretoy", bias = 0),
    engine("scoretoy2", bias = 25),
    now_dates = dates, verbose = FALSE
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
    engine("scoretoy", bias = 0),
    engine("scoretoy2", bias = 25),
    now_dates = dates, verbose = FALSE
  )

  good <- run_nowcast(x, engine("scoretoy", bias = 0), verbose = FALSE)
  bad <- run_nowcast(x, engine("scoretoy2", bias = 25), verbose = FALSE)

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
  truth <- truth_tbl_now(as.Date("2020-01-06"), 11)

  exported <- as_scoringutils(nowcast, truth = truth)

  expect_true(all(
    c("observed", "predicted", "quantile_level", "model") %in% colnames(exported)
  ))
  expect_equal(nrow(exported), 3)

  skip_if_not_installed("scoringutils")
  expect_no_error(scoringutils::as_forecast_quantile(as.data.frame(exported)))
})

test_that("as_scoringutils() ignores grouping on a tbl_now truth", {
  predictions <- data.frame(
    event_date = as.Date("2020-01-06"),
    .quantile_level = c(0.25, 0.5, 0.75), .value = c(8, 10, 13)
  )
  nowcast <- tbl_nowcast(
    predictions = predictions, method = "toy", event_date = "event_date"
  )
  truth <- truth_tbl_now(as.Date("2020-01-06"), 11)

  expect_equal(
    as_scoringutils(nowcast, truth = dplyr::group_by(truth, rp)),
    as_scoringutils(nowcast, truth = truth)
  )
})

test_that("as_scoringutils() converts a backtest with its stored truth", {
  register_scoretoy()
  x <- score_tbl_now()
  dates <- as.Date(c("2020-06-01", "2020-06-08"))
  backtest <- nowcast_backtest(
    x,
    engine("scoretoy", bias = 0),
    engine("scoretoy2", bias = 10),
    now_dates = dates, verbose = FALSE
  )

  exported <- as_scoringutils(backtest)

  expect_true(all(
    c("observed", "predicted", "quantile_level", "model", "now") %in%
      colnames(exported)
  ))
  expect_setequal(unique(exported$model), backtest$methods)
  expect_setequal(unique(exported$now), dates)
  expect_equal(nrow(exported), nrow(backtest$predictions))
})

test_that("scoringutils directly coerces nowcasts, ensembles and backtests", {
  skip_if_not_installed("scoringutils")
  register_scoretoy()
  truth <- score_tbl_now()
  first <- run_nowcast(
    truth, engine("scoretoy", bias = 0), verbose = FALSE
  )
  second <- run_nowcast(
    truth, engine("scoretoy2", bias = 5), verbose = FALSE
  )
  ensemble <- nowcast_ensemble(first, second, verbose = FALSE)
  backtest <- nowcast_backtest(
    truth, engine("scoretoy"), engine("scoretoy2", bias = 5),
    now_dates = as.Date("2020-06-01"), verbose = FALSE
  )

  for (object in list(first, ensemble)) {
    converted <- scoringutils::as_forecast_quantile(object, truth = truth)
    expect_s3_class(converted, "forecast_quantile")
  }
  converted_backtest <- scoringutils::as_forecast_quantile(backtest)
  expect_s3_class(converted_backtest, "forecast_quantile")
  expect_true("now" %in% scoringutils::get_forecast_unit(converted_backtest))
  expect_no_error(suppressWarnings(
    scoringutils::add_relative_skill(scoringutils::score(converted_backtest))
  ))
})

test_that("scoringutils directly coerces draw-based nowcasts and ensembles", {
  skip_if_not_installed("scoringutils", minimum_version = "2.0.0")
  register_sampletoy()
  truth <- score_tbl_now()
  first <- run_nowcast(
    truth, engine("sampletoy", bias = 0), verbose = FALSE
  )
  second <- run_nowcast(
    truth, engine("sampletoy2", bias = 5), verbose = FALSE
  )

  converted <- scoringutils::as_forecast_sample(first, truth = truth)
  expect_s3_class(converted, "forecast_sample")
  expect_true(all(
    c("model", "event_date") %in% scoringutils::get_forecast_unit(converted)
  ))

  pooled <- nowcast_ensemble(
    first, second, type = "linear_pool", n_draws = 80, verbose = FALSE
  )
  converted_pool <- scoringutils::as_forecast_sample(pooled, truth = truth)
  expect_s3_class(converted_pool, "forecast_sample")
  expect_equal(length(unique(converted_pool$sample_id)), 80)

  quantile_ensemble <- nowcast_ensemble(first, second, verbose = FALSE)
  expect_error(
    scoringutils::as_forecast_sample(quantile_ensemble, truth = truth),
    "does not carry draws"
  )
})

test_that("sample coercion of a backtest requires retained draws from every fit", {
  skip_if_not_installed("scoringutils", minimum_version = "2.0.0")
  register_scoretoy()
  register_sampletoy()
  truth <- score_tbl_now()
  date <- as.Date("2020-06-01")

  ordinary <- nowcast_backtest(
    truth, engine("sampletoy"), now_dates = date, verbose = FALSE
  )
  expect_null(ordinary$draws)
  expect_error(
    scoringutils::as_forecast_sample(ordinary),
    "keep_draws = TRUE"
  )

  retained <- nowcast_backtest(
    truth,
    engine("sampletoy"), engine("sampletoy2", bias = 5),
    now_dates = date, keep_draws = TRUE, verbose = FALSE
  )
  expect_false(is.null(retained$draws))
  converted <- scoringutils::as_forecast_sample(retained)
  expect_s3_class(converted, "forecast_sample")
  expect_true("now" %in% scoringutils::get_forecast_unit(converted))
  expect_no_error(scoringutils::score(converted))

  mixed <- nowcast_backtest(
    truth,
    engine("sampletoy"), engine("scoretoy"),
    now_dates = date, keep_draws = TRUE, verbose = FALSE
  )
  expect_error(
    scoringutils::as_forecast_sample(mixed),
    "no draws for 1 successful fit"
  )
})

# Cross-checks against scoringutils -------------------------------------------
#
# `.wis()` is hand-rolled so that weighting an ensemble never needs an extra
# package. That is only defensible if it agrees with the reference
# implementation, so here it is checked against it on the same numbers. Two
# implementations agreeing is worth more than either alone.

test_that(".wis(), ae_median and coverage agree with scoringutils", {
  skip_if_not_installed("scoringutils")

  set.seed(20260824)
  levels <- nowcast_quantile_levels()
  observed <- c(10, 55, 3, 128, 0)
  # Deliberately mixed: biased high, biased low, sharp, wide, and spot on.
  centres <- observed + c(4, -9, 0, 30, 2)
  spreads <- c(6, 6, 1, 40, 3)

  predicted <- lapply(seq_along(observed), function(i) {
    stats::qnorm(levels, mean = centres[i], sd = spreads[i])
  })

  reference <- do.call(rbind, lapply(seq_along(observed), function(i) {
    data.frame(
      target = i, observed = observed[i], quantile_level = levels,
      predicted = predicted[[i]], model = "m"
    )
  })) |>
    scoringutils::as_forecast_quantile(forecast_unit = c("target", "model")) |>
    scoringutils::score() |>
    as.data.frame()
  reference <- reference[order(reference$target), ]

  ours <- vapply(
    seq_along(observed),
    function(i) .wis(observed[i], levels, predicted[[i]]), numeric(1)
  )
  expect_equal(ours, reference$wis)

  covered_90 <- vapply(
    seq_along(observed),
    function(i) .covered(observed[i], levels, predicted[[i]], 0.9), logical(1)
  )
  expect_equal(covered_90, as.logical(reference$interval_coverage_90))

  covered_50 <- vapply(
    seq_along(observed),
    function(i) .covered(observed[i], levels, predicted[[i]], 0.5), logical(1)
  )
  expect_equal(covered_50, as.logical(reference$interval_coverage_50))
})

test_that("score_nowcast() agrees with scoringutils end to end", {
  skip_if_not_installed("scoringutils")

  set.seed(20260824)
  levels <- nowcast_quantile_levels()
  dates <- as.Date("2020-01-06") + 7 * (0:9)
  means <- seq(20, 200, length.out = length(dates))

  predictions <- tidyr::expand_grid(event_date = dates, .quantile_level = levels)
  predictions$.value <- stats::qnorm(
    predictions$.quantile_level,
    mean = rep(means, each = length(levels)), sd = 15
  )
  nowcast <- tbl_nowcast(
    predictions = predictions, method = "toy", event_date = "event_date"
  )
  truth <- truth_tbl_now(dates, stats::rpois(length(dates), means))

  ours <- score_nowcast(nowcast, truth = truth)
  theirs <- as_scoringutils(nowcast, truth = truth) |>
    as.data.frame() |>
    scoringutils::as_forecast_quantile(
      forecast_unit = c("event_date", "model")
    ) |>
    scoringutils::score() |>
    as.data.frame()
  theirs <- theirs[order(theirs$event_date), ]

  expect_equal(ours$wis, theirs$wis)
  expect_equal(ours$ae_median, theirs$ae_median)
  expect_equal(mean(ours$coverage_90), mean(theirs$interval_coverage_90))
})

# Calibration -----------------------------------------------------------------
#
# An ensemble whose intervals are wrong is worse than no ensemble, so the thing
# actually worth asserting is empirical coverage: over many targets, the share of
# truths inside the nominal q% interval should be near q. Simulated data, because
# only there is the data-generating distribution known -- and counts large enough
# that Poisson discreteness does not make a well-specified 50% interval read as a
# 59% one.

simulate_calibration <- function(seed = 20260824, n = 400) {
  set.seed(seed)
  levels <- nowcast_quantile_levels()
  dates <- as.Date("2020-01-01") + seq_len(n) - 1
  mu <- rep(c(80, 200, 500, 1200), length.out = n)

  member <- function(scale, method) {
    predictions <- tidyr::expand_grid(event_date = dates, .quantile_level = levels)
    predictions$.value <- stats::qpois(
      predictions$.quantile_level, rep(mu * scale, each = length(levels))
    )
    draws <- tidyr::expand_grid(event_date = dates, .draw = seq_len(600))
    draws$.value <- stats::rpois(nrow(draws), rep(mu * scale, each = 600))
    tbl_nowcast(
      predictions = predictions, draws = draws, method = method,
      event_date = "event_date"
    )
  }

  list(
    truth = truth_tbl_now(dates, stats::rpois(n, mu)),
    member = member
  )
}

test_that("a well-specified nowcast has close to nominal empirical coverage", {
  sim <- simulate_calibration()
  scores <- score_nowcast(sim$member(1, "truth_model"), truth = sim$truth)

  # The predictive distribution IS the data-generating one, so anything far from
  # nominal means the scoring or the quantile pipeline is wrong, not the model.
  expect_equal(mean(scores$coverage_50), 0.5, tolerance = 0.1)
  expect_equal(mean(scores$coverage_90), 0.9, tolerance = 0.05)
})

test_that("the ensemble restores the calibration its members have lost", {
  sim <- simulate_calibration()
  low <- sim$member(0.9, "low")
  high <- sim$member(1.1, "high")

  coverage <- function(nowcast) {
    scores <- score_nowcast(nowcast, truth = sim$truth)
    c(
      cov90 = mean(scores$coverage_90),
      wis = mean(scores$wis)
    )
  }

  low_scores <- coverage(low)
  high_scores <- coverage(high)
  # Each member is biased, and its intervals miss far too often.
  expect_lt(low_scores[["cov90"]], 0.7)
  expect_lt(high_scores[["cov90"]], 0.7)

  averaged <- coverage(nowcast_ensemble(low, high, verbose = FALSE))
  expect_equal(averaged[["cov90"]], 0.9, tolerance = 0.05)
  # ...and averaging two opposite biases beats both members outright
  expect_lt(averaged[["wis"]], min(low_scores[["wis"]], high_scores[["wis"]]))

  # The linear pool propagates the disagreement instead of cancelling it, so it
  # comes out WIDER than nominal. That is the documented trade-off, not a bug.
  pooled <- coverage(nowcast_ensemble(
    low, high,
    type = "linear_pool", n_draws = 2000, verbose = FALSE
  ))
  expect_gt(pooled[["cov90"]], averaged[["cov90"]])
})

# Weight properties -----------------------------------------------------------

test_that("every weighting rule returns non-negative weights summing to 1", {
  register_scoretoy()
  x <- score_tbl_now()
  backtest <- nowcast_backtest(
    x,
    engine("scoretoy", bias = 0),
    engine("scoretoy2", bias = 25),
    now_dates = as.Date(c("2020-06-01", "2020-06-08")), verbose = FALSE
  )

  for (type in c("equal", "inverse_score", "optim")) {
    weights <- nowcast_weights(backtest, type = type)
    expect_equal(sum(weights), 1, info = type)
    expect_true(all(weights >= 0), info = type)
    expect_named(weights, backtest$methods, info = type)
  }
})

test_that("the optim weights are reproducible", {
  register_scoretoy()
  x <- score_tbl_now()
  backtest <- nowcast_backtest(
    x,
    engine("scoretoy", bias = 0),
    engine("scoretoy2", bias = 25),
    now_dates = as.Date(c("2020-06-01", "2020-06-08")), verbose = FALSE
  )

  set.seed(1)
  first <- nowcast_weights(backtest, "optim")
  set.seed(99)
  second <- nowcast_weights(backtest, "optim")

  expect_equal(first, second)
})

test_that("an optimiser that fails falls back to equal weights, not NA", {
  register_scoretoy()
  x <- score_tbl_now()
  backtest <- nowcast_backtest(
    x,
    engine("scoretoy", bias = 0),
    engine("scoretoy2", bias = 25),
    now_dates = as.Date("2020-06-01"), verbose = FALSE
  )

  # A vector of NA weights does not fail here: it fails much later, inside
  # `nowcast_ensemble()`, as an all-NA nowcast that reads like a modelling
  # problem rather than an optimisation one.
  local_mocked_bindings(
    optim = function(...) list(par = c(NA_real_, NA_real_), value = NA_real_),
    .package = "stats"
  )

  expect_warning(weights <- nowcast_weights(backtest, "optim"), "did not converge")
  expect_false(anyNA(weights))
  expect_equal(unname(weights), c(0.5, 0.5))
})

test_that("a method with WIS = 0 does not produce infinite weights", {
  # Built by hand rather than fitted: a backend only ever sees the SNAPSHOT, so
  # no toy model scored against the eventual truth can reach a WIS of exactly
  # zero -- and zero is the whole point, because 1/0 is what the guard exists
  # for.
  backtest <- structure(
    list(
      scores = dplyr::tibble(
        .method = rep(c("perfect", "ordinary"), each = 3),
        .now = as.Date("2020-06-01"),
        event_date = rep(as.Date("2020-05-04") + 7 * (0:2), times = 2),
        .observed = 100,
        wis = c(0, 0, 0, 4, 6, 5),
        ae_median = c(0, 0, 0, 4, 6, 5),
        coverage_50 = TRUE, coverage_90 = TRUE
      ),
      methods = c("perfect", "ordinary"),
      now_dates = as.Date("2020-06-01"),
      event_date = "event_date",
      strata = character(0)
    ),
    class = "nowcast_backtest"
  )

  weights <- nowcast_weights(backtest, "inverse_score")

  expect_false(anyNA(weights))
  expect_true(all(is.finite(weights)))
  expect_equal(sum(weights), 1)
  # The perfect model takes everything, rather than an `Inf` that propagates as
  # `NaN` through the normalisation and out into the ensemble.
  expect_equal(weights[["perfect"]], 1)
  expect_equal(weights[["ordinary"]], 0)
})

test_that("a backtest whose scores are all missing is refused, not averaged", {
  backtest <- structure(
    list(
      scores = dplyr::tibble(
        .method = "broken", .now = as.Date("2020-06-01"),
        event_date = as.Date("2020-05-04"), .observed = NA_real_,
        wis = NA_real_, ae_median = NA_real_,
        coverage_50 = NA, coverage_90 = NA
      ),
      methods = "broken", now_dates = as.Date("2020-06-01"),
      event_date = "event_date", strata = character(0)
    ),
    class = "nowcast_backtest"
  )

  expect_error(
    suppressWarnings(nowcast_weights(backtest, "inverse_score")),
    "missing score"
  )
})
