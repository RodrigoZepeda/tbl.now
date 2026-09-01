# The framework is tested with a toy backend so that the tests never depend on
# a modelling package being installed (or on an MCMC run finishing). The real
# backends are exercised in `test-nowcast_backends.R`.

# A deterministic backend: predict the counts reported so far, with a fixed
# multiplicative spread.
nowcast_fit.testtoy <- function(engine, x, ..., spread = 1.2,
                                quantile_levels = nowcast_quantile_levels(),
                                verbose = TRUE) {
  observed <- tbl.now:::.eventual_counts(x)
  list(observed = observed, spread = spread, event_col = get_event_date(x))
}

nowcast_tidy.testtoy <- function(engine, fit, x, ..., quantile_levels) {
  n_draws <- 200
  draws <- fit$observed |>
    dplyr::reframe(
      !!fit$event_col := rep(.data[[fit$event_col]], each = n_draws),
      .draw = rep(seq_len(n_draws), times = dplyr::n()),
      .value = rep(.data$.observed, each = n_draws) *
        stats::qunif(seq(0.001, 0.999, length.out = n_draws), 1 / fit$spread, fit$spread)
    )
  list(predictions = NULL, draws = draws)
}

toy_tbl_now <- function() {
  set.seed(42)
  dates <- as.Date("2020-01-06") + seq(0, 7 * 19, by = 7)
  data <- tidyr::expand_grid(event_date = dates, delay = 0:3) |>
    dplyr::mutate(
      report_date = .data$event_date + 7 * .data$delay,
      n = as.integer(30 - 5 * .data$delay + seq_len(dplyr::n()) %% 4)
    ) |>
    dplyr::select("event_date", "report_date", "n")

  tbl_now(data,
    event_date = "event_date", report_date = "report_date",
    case_count = "n", data_type = "count-incidence", verbose = FALSE
  )
}

test_that("engine() canonicalises the built-in names", {
  expect_equal(engine("nobbs")$name, "NobBS")
  expect_equal(engine("NOBBS")$name, "NobBS")
  expect_equal(engine("epinowcast")$name, "epinowcast")
  # Unknown names are passed through so third-party backends keep working
  expect_equal(engine("my_model")$name, "my_model")

  expect_s3_class(engine("testtoy"), "nowcast_engine")
  expect_error(engine(c("a", "b")), "single")
  expect_error(engine(1), "single")
})

test_that("engine() validates its arguments at CONSTRUCTION time", {
  # The whole point of the engine: a mistake is caught here, where the call is,
  # rather than at fit time or -- worse -- not at all.
  expect_error(engine("testtoy", 1), "must be named")
  expect_error(engine("testtoy", quantile_levels = c(0, 0.5)), "between")
  expect_error(engine("testtoy", quantile_levels = 1.5), "between")
  expect_error(engine("testtoy", min_date = -1), "positive")
  expect_error(engine("testtoy", min_date = c(1, 2)), "single")
  expect_error(engine("testtoy", label = c("a", "b")), "single")

  # An argument left NULL must not reach the backend at all: several of these
  # packages treat an explicit NULL as an error rather than as a default.
  expect_named(engine_nobbs()$args, character(0))
  expect_named(engine_nobbs(max_D = 3)$args, "max_D")
})

test_that("run_nowcast() refuses a bare method name, and says what to write", {
  x <- toy_tbl_now()
  expect_error(run_nowcast(x, "testtoy"), "engine")
  # The message has to name the constructor, or the reader is left guessing
  # where the arguments the string used to carry are supposed to go.
  expect_error(run_nowcast(x, "nobbs"), "engine_nobbs")
})

test_that("an unregistered method gives an actionable error", {
  x <- toy_tbl_now()
  expect_error(
    run_nowcast(x, engine("definitely_not_a_method"), verbose = FALSE),
    "No nowcasting method"
  )
})

test_that("a backend with no nowcast_tidy() method is reported as such", {
  x <- toy_tbl_now()
  # Register only half a backend, in this test's environment
  local({
    nowcast_fit.halfdone <- function(engine, x, ..., quantile_levels, verbose = TRUE) 1
    registerS3method("nowcast_fit", "halfdone", nowcast_fit.halfdone,
      envir = asNamespace("tbl.now")
    )
  })
  expect_error(run_nowcast(x, engine("halfdone"), verbose = FALSE), "nowcast_tidy")
})

test_that("run_nowcast() returns a well formed tbl_nowcast", {
  x <- toy_tbl_now()
  registerS3method("nowcast_fit", "testtoy", nowcast_fit.testtoy, envir = asNamespace("tbl.now"))
  registerS3method("nowcast_tidy", "testtoy", nowcast_tidy.testtoy, envir = asNamespace("tbl.now"))

  nowcast <- run_nowcast(x, engine("testtoy"), verbose = FALSE)

  expect_true(is_tbl_nowcast(nowcast))
  expect_equal(nowcast@method, "testtoy")
  expect_equal(nowcast@event_date, "event_date")
  expect_equal(nowcast@now, get_now(x))
  expect_named(
    nowcast@predictions,
    c("event_date", ".quantile_level", ".value")
  )
  # The quantiles were derived from the draws the backend returned
  expect_equal(
    sort(unique(nowcast@predictions$.quantile_level)),
    nowcast_quantile_levels()
  )
  expect_false(is.null(nowcast@draws))
})

test_that("the engine's quantile_levels reach the result", {
  x <- toy_tbl_now()
  nowcast <- run_nowcast(
    x, engine("testtoy", quantile_levels = c(0.1, 0.5, 0.9)), verbose = FALSE
  )
  expect_equal(sort(unique(nowcast@predictions$.quantile_level)), c(0.1, 0.5, 0.9))
})

test_that("the engine's min_date trims the series it is fitted on", {
  x <- toy_tbl_now()
  now <- get_now(x)

  # A Date is a fixed cut...
  cut <- as.Date("2020-03-02")
  by_date <- run_nowcast(x, engine("testtoy", min_date = cut), verbose = FALSE)
  expect_true(min(by_date@predictions$event_date) >= cut)

  # ...and a number is that many PERIODS before `now`, in the object's own
  # units, which on this weekly fixture is weeks and not days. Measured from
  # `now`, NOT from the last event date: `now` is the as-of date of the whole
  # nowcasting problem, and it is what a backtest moves, so a window anchored to
  # it stays the same length at every retrospective date. Here `now` sits three
  # weeks past the last event, so a five-week window holds two event dates.
  kept <- unique(x[[get_event_date(x)]])
  kept <- kept[as.numeric(now - kept) / 7 < 5]
  expect_length(kept, 2L)

  by_periods <- run_nowcast(x, engine("testtoy", min_date = 5), verbose = FALSE)
  expect_setequal(unique(by_periods@predictions$event_date), kept)

  # The trimmed object is what the result carries, so everything downstream --
  # scoring, `autoplot()`'s reported counts -- describes the data the model saw.
  expect_equal(min(by_periods@data[[get_event_date(x)]]), min(kept))
  expect_equal(get_now(by_periods@data), now)

  expect_error(
    run_nowcast(x, engine("testtoy", min_date = as.Date("2099-01-01")), verbose = FALSE),
    "leaves no data"
  )
})

test_that("run_nowcast() rejects anything that is not a tbl_now", {
  expect_error(run_nowcast(mtcars, engine("testtoy")), "tbl_now")
})

test_that("the predicted quantiles are monotone in the quantile level", {
  x <- toy_tbl_now()
  nowcast <- run_nowcast(x, engine("testtoy"), verbose = FALSE)

  monotone <- nowcast@predictions |>
    dplyr::group_by(.data$event_date) |>
    dplyr::summarise(ok = !is.unsorted(.data$.value[order(.data$.quantile_level)]))

  expect_true(all(monotone$ok))
})

test_that("list_nowcast_methods() finds registered backends", {
  methods <- list_nowcast_methods(installed_only = FALSE)
  expect_true("testtoy" %in% methods)
  expect_false("default" %in% methods)
})

test_that("tbl_nowcast validates its inputs", {
  expect_error(
    tbl_nowcast(
      predictions = data.frame(a = 1, .quantile_level = 0.5, .value = 1),
      method = "toy", event_date = "event_date"
    ),
    "missing the column"
  )

  expect_error(
    tbl_nowcast(
      predictions = data.frame(event_date = 1, .quantile_level = 1.5, .value = 1),
      method = "toy", event_date = "event_date"
    ),
    "between 0 and 1"
  )

  expect_error(
    tbl_nowcast(
      predictions = data.frame(event_date = 1, .quantile_level = 0.5, .value = 1),
      draws = data.frame(event_date = 1, .value = 1),
      method = "toy", event_date = "event_date"
    ),
    "`draws` is missing"
  )
})

test_that("as_tibble() gives quantiles by default and draws on request", {
  x <- toy_tbl_now()
  nowcast <- run_nowcast(x, engine("testtoy"), verbose = FALSE)

  expect_true(".quantile_level" %in% colnames(tibble::as_tibble(nowcast)))
  expect_true(".draw" %in% colnames(tibble::as_tibble(nowcast, type = "draws")))

  quantiles_only <- tbl_nowcast(
    predictions = data.frame(event_date = 1, .quantile_level = 0.5, .value = 1),
    method = "toy", event_date = "event_date"
  )
  expect_error(tibble::as_tibble(quantiles_only, type = "draws"), "did not return")
})

test_that("autoplot() draws a fan", {
  skip_if_not_installed("ggplot2")
  x <- toy_tbl_now()
  nowcast <- run_nowcast(x, engine("testtoy"), verbose = FALSE)

  expect_s3_class(autoplot(nowcast), "ggplot")
  expect_s3_class(autoplot(nowcast, levels = 0.9), "ggplot")
  expect_error(autoplot(nowcast, levels = 0.42), "not available")
})

# Printing ---------------------------------------------------------------------

test_that("printing a nowcast leads with the value at the now edge", {
  predictions <- dplyr::tibble(
    onset_week = rep(as.Date("2020-01-05") + c(0, 7), each = 3),
    .quantile_level = rep(c(0.025, 0.5, 0.975), times = 2),
    .value = c(5, 10, 20, 8, 14, 31)
  )
  nowcast <- tbl_nowcast(
    predictions = predictions, method = "toy", event_date = "onset_week",
    now = as.Date("2020-01-12")
  )

  # `cli_*` writes to the MESSAGE stream, which `capture.output()` does not see;
  # a print method has to reach stdout.
  printed <- capture.output(print(nowcast))

  # The LAST event date, not `now`: on a weekly grid they are the same date only
  # by accident, and printing `now` over a number belonging to the week before
  # it would be a lie about which period was estimated.
  expect_true(any(grepl("Nowcast at \"2020-01-12\"", printed)))
  expect_true(any(grepl("14 [8, 31]", printed, fixed = TRUE)))
})

test_that("the value at the now edge is reported per stratum", {
  predictions <- dplyr::tibble(
    onset_week = as.Date("2020-01-05"),
    gender = rep(c("Female", "Male"), each = 3),
    .quantile_level = rep(c(0.05, 0.5, 0.95), times = 2),
    .value = c(1, 2, 3, 40, 50, 60)
  )
  nowcast <- tbl_nowcast(
    predictions = predictions, method = "toy", event_date = "onset_week",
    strata = "gender", now = as.Date("2020-01-05")
  )

  printed <- capture.output(print(nowcast))

  expect_true(any(grepl("Female: 2 [1, 3]", printed, fixed = TRUE)))
  expect_true(any(grepl("Male: 50 [40, 60]", printed, fixed = TRUE)))
  expect_true(any(grepl("5-95% interval", printed)))
})

test_that("a single quantile level prints a point estimate and no interval", {
  nowcast <- tbl_nowcast(
    predictions = dplyr::tibble(
      onset_week = as.Date("2020-01-05"), .quantile_level = 0.5, .value = 7
    ),
    method = "toy", event_date = "onset_week"
  )

  printed <- capture.output(print(nowcast))

  expect_true(any(grepl("(q50)", printed, fixed = TRUE)))
  expect_false(any(grepl("interval", printed)))
})
