# `nowcast_backtest(parallel = TRUE)` runs each (engine, date) fit as a future
# task. It must return the same object, in the same order, as the sequential
# loop -- and the sequential loop must be untouched when `parallel = FALSE`.

parallel_data <- function(strata = FALSE) {
  df <- data.frame(
    onset = as.Date("2024-01-07") + rep(7 * (0:9), each = 6),
    report = as.Date("2024-01-07") + rep(7 * (0:9), each = 6) + rep(c(0, 7, 14), 2),
    gender = rep(c("F", "M"), each = 3)
  )
  tbl_now(df,
    event_date = onset, report_date = report,
    strata = if (strata) "gender" else NULL,
    data_type = "linelist", verbose = FALSE
  )
}

parallel_dates <- as.Date(c("2024-02-18", "2024-03-03"))

run_both <- function(x, ...) {
  sequential <- nowcast_backtest(x, ..., now_dates = parallel_dates,
                                 verbose = FALSE)
  parallel <- nowcast_backtest(x, ..., now_dates = parallel_dates,
                               verbose = FALSE, parallel = TRUE)
  list(sequential = sequential, parallel = parallel)
}

expect_same_backtest <- function(a, b) {
  expect_equal(a$scores, b$scores)
  expect_equal(a$predictions, b$predictions)
  expect_equal(a$draws, b$draws)
  expect_equal(a$truth, b$truth)
  expect_equal(a$methods, b$methods)
  expect_equal(a$now_dates, b$now_dates)
  expect_equal(
    dplyr::select(a$timings, -"elapsed_seconds"),
    dplyr::select(b$timings, -"elapsed_seconds")
  )
}

test_that("`parallel` must be TRUE or FALSE", {
  x <- parallel_data()
  expect_error(
    nowcast_backtest(x, example_engine(), now_dates = parallel_dates,
                     verbose = FALSE, parallel = "yes"),
    "parallel"
  )
  expect_error(
    nowcast_backtest(x, example_engine(), now_dates = parallel_dates,
                     verbose = FALSE, parallel = NA),
    "parallel"
  )
})

test_that("a parallel backtest equals the sequential one, in order", {
  skip_if_not_installed("doFuture")
  skip_if_not_installed("foreach")
  skip_if_not_installed("future")
  future::plan(future::sequential)

  both <- run_both(parallel_data(),
    example_engine(spread = 0.2, label = "narrow"),
    example_engine(spread = 0.5, label = "wide")
  )
  expect_s3_class(both$parallel, "nowcast_backtest")
  expect_same_backtest(both$sequential, both$parallel)
  # Dates outer, engines inner -- as the sequential loop has always done.
  expect_equal(both$parallel$timings$.method, c("narrow", "wide", "narrow", "wide"))
})

test_that("a parallel backtest works with strata and a grouped tbl_now", {
  skip_if_not_installed("doFuture")
  skip_if_not_installed("foreach")
  skip_if_not_installed("future")
  future::plan(future::sequential)

  x <- parallel_data(strata = TRUE)
  both <- run_both(x, example_engine(label = "toy"))
  expect_same_backtest(both$sequential, both$parallel)
  expect_true("gender" %in% colnames(both$parallel$predictions))

  grouped <- dplyr::group_by(x, gender)
  grouped_bt <- nowcast_backtest(grouped, example_engine(label = "toy"),
                                 now_dates = parallel_dates, verbose = FALSE,
                                 parallel = TRUE)
  expect_same_backtest(both$sequential, grouped_bt)
})

test_that("a supplied seed makes stochastic fits agree across modes", {
  skip_on_cran()
  skip_if_not_installed("doFuture")
  skip_if_not_installed("foreach")
  skip_if_not_installed("future")
  skip_if_not_installed("baselinenowcast")
  future::plan(future::sequential)

  x <- parallel_data()
  seq_bt <- suppressWarnings(suppressMessages(nowcast_backtest(
    x, engine_baselinenowcast(draws = 100, label = "bnc"),
    now_dates = parallel_dates, seed = 7, keep_draws = TRUE, verbose = FALSE
  )))
  par_bt <- suppressWarnings(suppressMessages(nowcast_backtest(
    x, engine_baselinenowcast(draws = 100, label = "bnc"),
    now_dates = parallel_dates, seed = 7, keep_draws = TRUE, verbose = FALSE,
    parallel = TRUE
  )))
  expect_same_backtest(seq_bt, par_bt)
})

test_that("failures are relayed from the workers as in the sequential loop", {
  skip_if_not_installed("doFuture")
  skip_if_not_installed("foreach")
  skip_if_not_installed("future")
  future::plan(future::sequential)
  registerS3method("nowcast_fit", "brokentoy",
    function(method, x, ..., quantile_levels, verbose = TRUE) stop("nope"),
    envir = asNamespace("tbl.now")
  )
  x <- parallel_data()

  expect_warning(
    bt <- nowcast_backtest(
      x, example_engine(label = "ok"), engine("brokentoy"),
      now_dates = parallel_dates[[2]], verbose = FALSE, parallel = TRUE
    ),
    "failed"
  )
  expect_equal(bt$methods, "ok")
  expect_equal(bt$timings$success, c(TRUE, FALSE))
  expect_match(bt$timings$error[[2L]], "nope")

  # doFuture adds its own "Canceling all iterations" warning to the error.
  expect_error(
    suppressWarnings(nowcast_backtest(x, engine("brokentoy"),
      now_dates = parallel_dates, on_error = "abort", verbose = FALSE,
      parallel = TRUE
    )),
    "failed"
  )
})

test_that("a parallel backtest runs on real multisession workers", {
  skip_on_cran()
  skip_on_covr()
  skip_if_not_installed("doFuture")
  skip_if_not_installed("foreach")
  skip_if_not_installed("future")
  future::plan(future::multisession, workers = 2)
  withr::defer(future::plan(future::sequential))
  # Workers load the INSTALLED tbl.now; skip unless it is the code under test.
  under_test <- deparse(body(nowcast_backtest))
  installed <- tryCatch(
    future::value(future::future(
      deparse(body(asNamespace("tbl.now")$nowcast_backtest))
    )),
    error = function(e) NULL
  )
  skip_if_not(identical(installed, under_test),
              "the installed tbl.now is not the version under test")

  both <- run_both(parallel_data(),
    example_engine(spread = 0.2, label = "narrow"),
    example_engine(spread = 0.5, label = "wide")
  )
  expect_same_backtest(both$sequential, both$parallel)
})
