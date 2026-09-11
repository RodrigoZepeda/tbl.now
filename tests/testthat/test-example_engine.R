# `example_engine()` exists so the documentation has something to run. If it
# stops working, every fitting and scoring example goes quiet.

make_data <- function(strata = FALSE) {
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

test_that("example_engine() builds an engine run_nowcast() accepts", {
  e <- example_engine()
  expect_true(is_nowcast_engine(e))
  expect_identical(e$name, "example")

  nc <- run_nowcast(make_data(), e, verbose = FALSE)
  expect_true(is_tbl_nowcast(nc))
  expect_identical(nowcast_quantile_levels(), e$quantile_levels)
})

test_that("it is deterministic and leaves the RNG stream alone", {
  skip_on_cran()
  x <- make_data()
  set.seed(1)
  before <- .Random.seed
  a <- tibble::as_tibble(run_nowcast(x, example_engine(), verbose = FALSE))
  expect_identical(before, .Random.seed)

  b <- tibble::as_tibble(run_nowcast(x, example_engine(), verbose = FALSE))
  expect_identical(a, b)
})

test_that("`spread` widens the interval, and 0 collapses it", {
  skip_on_cran()
  x <- make_data()
  wide <- tibble::as_tibble(run_nowcast(x, example_engine(spread = 0.5), verbose = FALSE))
  narrow <- tibble::as_tibble(run_nowcast(x, example_engine(spread = 0.1), verbose = FALSE))
  rng <- function(d) diff(range(d$.value[d[[1]] == d[[1]][1]]))
  expect_gt(rng(wide), rng(narrow))

  flat <- tibble::as_tibble(run_nowcast(x, example_engine(spread = 0), verbose = FALSE))
  expect_length(unique(flat$.value[flat[[1]] == flat[[1]][1]]), 1L)
})

test_that("it carries strata through", {
  skip_on_cran()
  nc <- run_nowcast(make_data(strata = TRUE), example_engine(), verbose = FALSE)
  out <- tibble::as_tibble(nc)
  expect_true("gender" %in% names(out))
  expect_setequal(unique(out$gender), c("F", "M"))
})

test_that("predictions are non-negative and ordered by quantile level", {
  skip_on_cran()
  out <- tibble::as_tibble(run_nowcast(make_data(), example_engine(), verbose = FALSE))
  expect_true(all(out$.value >= 0))
  one <- out[out[[1]] == out[[1]][1], ]
  one <- one[order(one$.quantile_level), ]
  expect_false(is.unsorted(one$.value))
})

test_that("it works in a backtest, and the labels come through", {
  skip_on_cran()
  bt <- nowcast_backtest(make_data(),
    example_engine(spread = 0.2, label = "narrow"),
    example_engine(spread = 0.5, label = "wide"),
    now_dates = as.Date(c("2024-02-18", "2024-03-03")),
    verbose = FALSE
  )
  expect_setequal(bt$methods, c("narrow", "wide"))

  w <- nowcast_weights(bt)
  expect_equal(sum(w), 1, tolerance = 1e-8)
  expect_setequal(names(w), c("narrow", "wide"))
})

test_that("a bad `spread` is refused", {
  skip_on_cran()
  expect_error(example_engine(spread = -1), "spread")
  expect_error(example_engine(spread = c(1, 2)), "spread")
  expect_error(example_engine(spread = "wide"), "spread")
})

test_that("a covariate is pooled, not left as a repeated prediction key", {
  skip_on_cran()
  # `get_latest_reported_cases()` answers by the covariate too, but a
  # `tbl_nowcast` is keyed by event date and stratum alone -- so the counts have
  # to come back down to that key rather than arrive twice per date.
  df <- data.frame(
    onset = as.Date("2024-01-07") + c(0, 0, 7, 7),
    report = as.Date("2024-01-07") + c(0, 7, 7, 14),
    gender = c("F", "F", "M", "M"),
    hospital = c("A", "B", "A", "B")
  )
  x <- tbl_now(df,
    event_date = onset, report_date = report, strata = "gender",
    covariates = "hospital", data_type = "linelist", verbose = FALSE
  )

  nc <- run_nowcast(x, example_engine(), verbose = FALSE)
  keys <- as.data.frame(nc@predictions)[c("onset", "gender", ".quantile_level")]
  expect_false(anyDuplicated(keys) > 0)

  # Both hospitals' cases are counted, not one of them.
  medians <- tidy(nc)
  expect_equal(medians$estimate, c(2, 2))
})

test_that("censored and uncensored arrivals in one cell give one prediction row", {
  skip_on_cran()
  # `get_latest_reported_cases()` answers by the censoring flag as well, so a
  # cell holding both an exact and a bounded arrival arrives as two rows. They
  # are one nowcast target, and passing both on put duplicate keys into
  # `predictions` -- which surfaced as a recycling error in `tidy()` and a
  # list-column crash in `autoplot()`.
  df <- data.frame(
    onset = as.Date("2024-01-07") + c(0, 0, 7),
    report = as.Date("2024-01-07") + c(0, 14, 14),
    gender = c("F", "F", "F")
  )
  x <- tbl_now(df,
    event_date = onset, report_date = report, strata = "gender",
    data_type = "linelist", units = "weeks", verbose = FALSE
  )
  x <- suppressMessages(censor_reports(x, report == as.Date("2024-01-21")))
  # The fixture is only meaningful if the getter really does split that cell.
  expect_equal(nrow(suppressWarnings(get_latest_reported_cases(x))), 3L)

  nc <- run_nowcast(x, example_engine(), verbose = FALSE)
  keys <- as.data.frame(nc@predictions)[c("onset", "gender", ".quantile_level")]
  expect_false(anyDuplicated(keys) > 0)

  # Both arrivals are counted, not one of them.
  expect_equal(tidy(nc)$estimate, c(2, 1))
  expect_no_error(ggplot2::ggplot_build(autoplot(nc)))
})
