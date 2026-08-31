# tidy() must return the SAME table whatever engine produced the fit. That is the
# entire point of the method, so the contract is asserted directly.

TIDY_COLUMNS <- c(
  "event_date", "stratum", "estimate", "conf.low", "conf.high", "level", "engine"
)

expect_tidy_contract <- function(out, engine) {
  expect_s3_class(out, "tbl_df")
  expect_equal(names(out)[seq_along(TIDY_COLUMNS)], TIDY_COLUMNS)
  expect_s3_class(out$event_date, "Date")
  expect_type(out$stratum, "character")
  expect_type(out$estimate, "double")
  expect_type(out$conf.low, "double")
  expect_type(out$conf.high, "double")
  expect_type(out$level, "double")
  expect_equal(unique(out$engine), engine)
  expect_gt(nrow(out), 0L)
}

tidy_test_tbl_now <- function() {
  data(denguedat, envir = environment())
  denguedat |>
    dplyr::filter(
      onset_week  < as.Date("2002-07-15"),
      report_week < as.Date("2002-07-15"),
      onset_week >= as.Date("2000-01-01")
    ) |>
    tbl_now(
      event_date = onset_week, report_date = report_week,
      data_type = "linelist", verbose = FALSE
    )
}

test_that("tidy() on a baselinenowcast fit meets the contract", {
  skip_if_not_installed("baselinenowcast")
  x <- tidy_test_tbl_now()
  triangle <- suppressWarnings(tbl_now_to_baselinenowcast(x, verbose = FALSE))
  fit <- suppressWarnings(suppressMessages(
    baselinenowcast::baselinenowcast(triangle, output_type = "samples", draws = 100)
  ))
  expect_tidy_contract(tidy(fit), "baselinenowcast")
})

test_that("tidy() adds a q* column per requested probability", {
  skip_if_not_installed("baselinenowcast")
  x <- tidy_test_tbl_now()
  triangle <- suppressWarnings(tbl_now_to_baselinenowcast(x, verbose = FALSE))
  fit <- suppressWarnings(suppressMessages(
    baselinenowcast::baselinenowcast(triangle, output_type = "samples", draws = 100)
  ))
  out <- tidy(fit, probs = c(0.05, 0.5, 0.95))

  expect_true(all(c("q5", "q50", "q95") %in% names(out)))
  # A non-integer probability keeps its decimal in the name.
  expect_true("q2.5" %in% names(tidy(fit, probs = 0.025)))
  # Quantiles must be ordered.
  expect_true(all(out$q5 <= out$q95))
})

test_that("tidy() refuses a `probs` the NobBS fit was never asked for", {
  skip_if_not_installed("NobBS")
  # A bare list carrying NobBS's shape, so no model has to be fitted. No
  # `specs$quantiles` was set, so there are no `q_*` columns at all.
  fake_nobbs <- list(estimates = data.frame(
    onset_date = as.Date(c("2002-07-01", "2002-07-08")),
    estimate = c(10, 8), lower = c(6, 2), upper = c(15, 19)
  ))
  expect_tidy_contract(tidy(fake_nobbs), "NobBS")
  expect_error(tidy(fake_nobbs, probs = 0.5), "did not compute")
  # The message has to say how to get it, which is at FIT time.
  expect_error(tidy(fake_nobbs, probs = 0.5), "specs")
})

test_that("tidy() returns the quantiles a NobBS fit WAS asked for", {
  skip_if_not_installed("NobBS")
  # `specs = list(quantiles = c(0.1, 0.5, 0.9))` puts these columns in
  # `estimates`. Reading them back is a lookup, not an approximation, so it must
  # not be refused -- the whole point of NobBS's `specs$quantiles` argument.
  fake_nobbs <- list(estimates = data.frame(
    onset_date = as.Date("2020-01-01") + 0:1,
    estimate = c(10, 8), lower = c(6, 2), upper = c(15, 19),
    q_0.1 = c(7, 3), q_0.5 = c(10, 8), q_0.9 = c(14, 17)
  ))

  tidied <- tidy(fake_nobbs, probs = c(0.1, 0.5, 0.9))
  expect_true(all(c("q10", "q50", "q90") %in% names(tidied)))
  # The values are NobBS's own, not something recomputed.
  expect_equal(tidied$q10, c(7, 3))
  expect_equal(tidied$q90, c(14, 17))

  # A level it did not compute is still refused, even though others are present.
  expect_error(tidy(fake_nobbs, probs = c(0.1, 0.33)), "did not compute")
  # And the advice lists what to ask for, including what is already there.
  expect_error(tidy(fake_nobbs, probs = 0.33), "0\\.1")
})

test_that("tidy() recognises a NobBS fit by structure", {
  fake_nobbs <- list(estimates = data.frame(
    onset_date = as.Date("2020-01-01") + 0:2,
    estimate = c(3, 4, 5), lower = c(2, 3, 4), upper = c(4, 5, 6)
  ))
  expect_equal(unique(tidy(fake_nobbs)$engine), "NobBS")
  expect_equal(unique(tidy(fake_nobbs, engine = "NobBS")$engine), "NobBS")
})

test_that("tidy() records the interval width each engine actually returns", {
  # epinowcast's default band is q5-q95, i.e. 90% -- not the 95% the others use.
  # Recording it is what stops a 90% band being compared with a 95% one.
  #
  # BEHAVIOUR CHANGE (0.16.0): NobBS used to be reported as 0.95, its `conf`
  # default. `NobBS()` does not return `specs`, so the width is unrecoverable
  # from the fit and is now `NA` unless the caller supplies it -- a guess in this
  # column defeats the column's whole purpose. See `test-tidy-strata.R` for the
  # `level` argument.
  fake_nobbs <- list(estimates = data.frame(
    onset_date = as.Date("2020-01-01"), estimate = 10, lower = 6, upper = 15
  ))
  expect_true(is.na(unique(tidy(fake_nobbs)$level)))
  expect_equal(unique(tidy(fake_nobbs, level = 0.95)$level), 0.95)
})

# --- surveillance -------------------------------------------------------------
# `nowcast()` already reports a prediction interval in the object's `pi` slot,
# so `tidy()` must not blank it out (it used to return NA bounds and NA level).

# Fitted ONCE and shared by the three tests below. `nowcast()` runs an MCMC, and
# paying for it per test cost 18 seconds -- 5% of the CRAN suite -- for three
# assertions about the same `pi` slot. The fit is read-only here: the test that
# empties `pi` assigns into its own local binding, which R copies on
# modification, so the cache cannot be reached through it.
surveillance_fit <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) cached <<- .surveillance_fit()
    cached
  }
})

.surveillance_fit <- function() {
  data(denguedat, envir = environment())
  cut <- as.Date("2002-07-22")
  dengue <- denguedat |>
    dplyr::filter(onset_week < cut, report_week < cut)
  x <- tbl_now(
    dengue,
    event_date = "onset_week", report_date = "report_week",
    data_type = "linelist", verbose = FALSE
  )
  linelist <- suppressWarnings(suppressMessages(
    tbl_now_to_surveillance(x, verbose = FALSE)
  ))
  now <- get_now(x)

  suppressWarnings(suppressMessages(surveillance::nowcast(
    now = now,
    when = seq(now - 7 * 5, now, by = "1 week"),
    data = linelist,
    dEventCol = "dHospital", dReportCol = "dReport",
    aggregate.by = "1 week", D = 10L,
    method = "bayes.notrunc.bnb",
    control = list(
      dRange = seq(min(linelist$dHospital), now, by = "1 week"),
      N.tInf.max = 500, nSamples = 500
    )
  )))
}

test_that("tidy() reads surveillance's prediction interval", {
  skip_if_not_installed("surveillance")
  tidied <- generics::tidy(surveillance_fit())

  expect_s3_class(tidied, "tbl_df")
  expect_gt(nrow(tidied), 0L)
  expect_false(anyNA(tidied$conf.low))
  expect_false(anyNA(tidied$conf.high))
  expect_true(all(tidied$conf.low <= tidied$estimate))
  expect_true(all(tidied$estimate <= tidied$conf.high))
})

test_that("tidy() reports surveillance's interval width from control$alpha", {
  skip_if_not_installed("surveillance")
  tidied <- generics::tidy(surveillance_fit())

  # `alpha = 0.05` is nowcast()'s default, i.e. a 95% band.
  expect_equal(unique(tidied$level), 0.95)
  expect_equal(unique(tidied$engine), "surveillance")
})

test_that("tidy() falls back to NA bounds when there is no pi slot", {
  skip_if_not_installed("surveillance")
  fit <- surveillance_fit()

  # `lawless` and `unif` can leave the slot empty; emulate that rather than
  # paying for a second fit.
  methods::slot(fit, "pi")[] <- NA_real_
  tidied <- generics::tidy(fit)

  expect_true(all(is.na(tidied$conf.low)))
  expect_true(all(is.na(tidied$conf.high)))
  expect_true(all(is.na(tidied$level)))
  expect_false(anyNA(tidied$estimate))
})

# --- epidist ------------------------------------------------------------------
# epidist is the one supported package that does NOT nowcast: it estimates the
# reporting-delay distribution, so `tidy()` returns a delay-shaped table (one row
# per parameter) rather than the per-event-date nowcast frame.
#
# The bindings are mocked so the test does not have to compile a Stan model; the
# method was verified against a real fit separately.

fake_delay_draws <- function() {
  set.seed(1)
  data.frame(
    draw  = seq_len(400),
    index = rep(1:4, each = 100),
    mu    = stats::rnorm(400, 1.6, 0.05),
    sigma = stats::rnorm(400, 0.61, 0.02)
  )
}

with_mocked_epidist <- function(code) {
  testthat::local_mocked_bindings(
    predict_delay_parameters = function(fit, newdata = NULL, ...) {
      fake_delay_draws()
    },
    add_mean_sd = function(data, ...) {
      data$mean <- exp(data$mu + data$sigma^2 / 2)
      data$sd <- data$mean * sqrt(exp(data$sigma^2) - 1)
      data
    },
    .package = "epidist"
  )
  force(code)
}

fake_epidist_fit <- function() structure(list(), class = c("brmsfit", "epidist_fit"))

test_that("tidy() on an epidist fit returns the delay distribution", {
  skip_if_not_installed("epidist")

  with_mocked_epidist({
    out <- generics::tidy(fake_epidist_fit())

    expect_s3_class(out, "tbl_df")
    # One row per distribution parameter, NOT per event date.
    expect_named(
      out, c("term", "estimate", "conf.low", "conf.high", "level", "engine")
    )
    expect_setequal(out$term, c("mu", "sigma", "mean", "sd"))
    expect_equal(unique(out$engine), "epidist")
    expect_equal(unique(out$level), 0.95)

    # None of the nowcast columns belong here.
    expect_false(any(c("event_date", "stratum") %in% names(out)))

    expect_true(all(out$conf.low <= out$estimate))
    expect_true(all(out$estimate <= out$conf.high))
  })
})

test_that("tidy() drops epidist's bookkeeping columns", {
  skip_if_not_installed("epidist")

  with_mocked_epidist({
    out <- generics::tidy(fake_epidist_fit())
    # `draw` and `index` identify the draw, they are not delay parameters.
    expect_false(any(c("draw", "index") %in% out$term))
  })
})

test_that("tidy() honours probs and level on an epidist fit", {
  skip_if_not_installed("epidist")

  with_mocked_epidist({
    out <- generics::tidy(fake_epidist_fit(), probs = c(0.05, 0.95))
    expect_true(all(c("q5", "q95") %in% names(out)))
    expect_true(all(out$q5 <= out$q95))

    narrow <- generics::tidy(fake_epidist_fit(), level = 0.5)
    wide <- generics::tidy(fake_epidist_fit(), level = 0.95)
    expect_equal(unique(narrow$level), 0.5)
    # A 50% interval must sit inside the 95% one.
    expect_true(all(narrow$conf.low >= wide$conf.low))
    expect_true(all(narrow$conf.high <= wide$conf.high))
  })
})

test_that("tidy() rejects an impossible level for an epidist fit", {
  skip_if_not_installed("epidist")

  with_mocked_epidist({
    expect_error(generics::tidy(fake_epidist_fit(), level = 1), "strictly between")
    expect_error(generics::tidy(fake_epidist_fit(), level = 0), "strictly between")
    expect_error(
      generics::tidy(fake_epidist_fit(), level = c(0.5, 0.9)), "strictly between"
    )
  })
})
