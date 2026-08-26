# One package, two models.
#
# `nowcast_ensemble()` takes a NAMED list, so a member can be "the same package,
# a different model" -- two `diseasenowcasting` fits with different epidemic
# processes, say. `nowcast_backtest()` took bare method names, so it could not
# tell those apart: both scored under the package's name, and `nowcast_weights()`
# learned one weight for what were two different models.

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

test_that("named methods become the labels, and reach the weights", {
  skip_if_not_installed("baselinenowcast")
  x <- backtest_tbl_now()
  dates <- get_now(x) - c(2, 1)

  bt <- suppressWarnings(suppressMessages(nowcast_backtest(
    x,
    methods = c(few = "baselinenowcast", many = "baselinenowcast"),
    method_args = list(few = list(draws = 100), many = list(draws = 400)),
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

test_that("unnamed methods are labelled by their own name, as before", {
  skip_if_not_installed("baselinenowcast")
  x <- backtest_tbl_now()

  bt <- suppressWarnings(suppressMessages(nowcast_backtest(
    x, methods = "baselinenowcast", now_dates = get_now(x) - 1,
    seed = 1, verbose = FALSE
  )))
  expect_equal(unique(bt$scores$.method), "baselinenowcast")
  expect_equal(bt$methods, "baselinenowcast")
})

test_that("method_args are keyed by label, falling back to the method name", {
  skip_if_not_installed("baselinenowcast")
  x <- backtest_tbl_now()

  # Keyed by the method name with NO names on `methods`: the old form, which
  # must keep working.
  bt <- suppressWarnings(suppressMessages(nowcast_backtest(
    x, methods = "baselinenowcast",
    method_args = list(baselinenowcast = list(draws = 100)),
    now_dates = get_now(x) - 1, seed = 1, verbose = FALSE
  )))
  expect_equal(unique(bt$scores$.method), "baselinenowcast")
})

test_that("duplicate labels abort rather than collapsing two models into one", {
  skip_if_not_installed("baselinenowcast")
  x <- backtest_tbl_now()

  # Same package twice with no names: both would be labelled "baselinenowcast",
  # so the result would carry two rows per date under one name and the weights
  # would be meaningless.
  expect_error(
    nowcast_backtest(x, methods = c("baselinenowcast", "baselinenowcast"),
                     now_dates = get_now(x) - 1, verbose = FALSE),
    "duplicate label"
  )
  # And the error says how to fix it.
  expect_error(
    nowcast_backtest(x, methods = c("baselinenowcast", "baselinenowcast"),
                     now_dates = get_now(x) - 1, verbose = FALSE),
    "diseasenowcasting"
  )
})
