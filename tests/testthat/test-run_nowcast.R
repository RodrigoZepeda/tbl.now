# The framework is tested with a toy backend so that the tests never depend on
# a modelling package being installed (or on an MCMC run finishing). The real
# backends are exercised in `test-nowcast_backends.R`.

# A deterministic backend: predict the counts reported so far, with a fixed
# multiplicative spread.
nowcast_fit.testtoy <- function(method, x, ..., spread = 1.2,
                                quantile_levels = nowcast_quantile_levels(),
                                verbose = TRUE) {
  observed <- tbl.now:::nowcast_truth(x)
  list(observed = observed, spread = spread, event_col = get_event_date(x))
}

nowcast_tidy.testtoy <- function(method, fit, x, ..., quantile_levels) {
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

test_that("nowcast_method() canonicalises the built-in names", {
  expect_equal(nowcast_method("nobbs")$name, "NobBS")
  expect_equal(nowcast_method("NOBBS")$name, "NobBS")
  expect_equal(nowcast_method("epinowcast")$name, "epinowcast")
  # Unknown names are passed through so third-party backends keep working
  expect_equal(nowcast_method("my_model")$name, "my_model")

  expect_s3_class(nowcast_method("testtoy"), "nowcast_method")
  expect_error(nowcast_method(c("a", "b")), "single")
  expect_error(nowcast_method(1), "single")
})

test_that("an unregistered method gives an actionable error", {
  x <- toy_tbl_now()
  expect_error(
    run_nowcast(x, method = "definitely_not_a_method", verbose = FALSE),
    "No nowcasting method"
  )
})

test_that("a backend with no nowcast_tidy() method is reported as such", {
  x <- toy_tbl_now()
  # Register only half a backend, in this test's environment
  local({
    nowcast_fit.halfdone <- function(method, x, ..., quantile_levels, verbose = TRUE) 1
    registerS3method("nowcast_fit", "halfdone", nowcast_fit.halfdone,
      envir = asNamespace("tbl.now")
    )
  })
  expect_error(run_nowcast(x, "halfdone", verbose = FALSE), "nowcast_tidy")
})

test_that("run_nowcast() returns a well formed tbl_nowcast", {
  x <- toy_tbl_now()
  registerS3method("nowcast_fit", "testtoy", nowcast_fit.testtoy, envir = asNamespace("tbl.now"))
  registerS3method("nowcast_tidy", "testtoy", nowcast_tidy.testtoy, envir = asNamespace("tbl.now"))

  nowcast <- run_nowcast(x, "testtoy", verbose = FALSE)

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

test_that("run_nowcast() honours quantile_levels and validates them", {
  x <- toy_tbl_now()
  nowcast <- run_nowcast(x, "testtoy", quantile_levels = c(0.1, 0.5, 0.9), verbose = FALSE)
  expect_equal(sort(unique(nowcast@predictions$.quantile_level)), c(0.1, 0.5, 0.9))

  expect_error(run_nowcast(x, "testtoy", quantile_levels = c(0, 0.5), verbose = FALSE), "between")
  expect_error(run_nowcast(x, "testtoy", quantile_levels = 1.5, verbose = FALSE), "between")
})

test_that("run_nowcast() rejects anything that is not a tbl_now", {
  expect_error(run_nowcast(mtcars, "testtoy"), "tbl_now")
})

test_that("the predicted quantiles are monotone in the quantile level", {
  x <- toy_tbl_now()
  nowcast <- run_nowcast(x, "testtoy", verbose = FALSE)

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
  nowcast <- run_nowcast(x, "testtoy", verbose = FALSE)

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
  nowcast <- run_nowcast(x, "testtoy", verbose = FALSE)

  expect_s3_class(autoplot(nowcast), "ggplot")
  expect_s3_class(autoplot(nowcast, levels = 0.9), "ggplot")
  expect_error(autoplot(nowcast, levels = 0.42), "not available")
})
