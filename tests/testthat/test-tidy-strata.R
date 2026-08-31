# `tidy()` makes two promises that `test-tidy.R` never puts under strain:
#
#   1. `(stratum, event_date)` is a unique key -- `"all"` means the fit was
#      unstratified, not "several strata pooled under one label";
#   2. `level` is the width the engine ACTUALLY produced, and `NA` when it
#      produced none.
#
# Both only bite on a stratified fit, or on an engine that returns a point
# estimate. Every fixture in `test-tidy.R` is single-stratum and interval-valued,
# so the two failure modes lived there undetected: `tidy.epinowcast()` and the
# NobBS branch of `tidy.list()` both stacked every stratum under `"all"`, and
# `tidy.baselinenowcast_df()` quoted a zero-width band as 95%.
#
# The fits are MOCKED, not fitted: an epinowcast fit needs cmdstan and a
# `NobBS.strat()` fit needs JAGS. Each fixture below is the shape of a real fit,
# checked once by hand against `germany_covid19_hosp` (two age groups) and
# against `NobBS.strat()` on `denguedat` split in two.

expect_unique_stratum_key <- function(out) {
  expect_equal(sum(duplicated(out[, c("stratum", "event_date")])), 0L)
}

# --- epinowcast: one block per `by` group -------------------------------------

# `summary(fit, type = "nowcast")` stacks one block per `.group`, ordered by
# `.group` then `reference_date`, and carries the grouping column beside the
# index. `location` is deliberately present and NOT in `by`: only the `by`
# columns may become the stratum label.
enw_summary_fixture <- function() {
  data.frame(
    reference_date = rep(as.Date("2021-08-13") + 0:2, times = 2),
    report_date    = as.Date("2021-08-22"),
    .group         = rep(1:2, each = 3),
    location       = "DE",
    age_group      = rep(c("00+", "80+"), each = 3),
    median         = c(204, 193, 132, 16, 25, 11),
    q5             = c(204, 190, 127, 16, 25, 10),
    q95            = c(204, 200, 142, 16, 27, 13),
    stringsAsFactors = FALSE
  )
}

# Draws for the same six cells. The two strata are an order of magnitude apart,
# so a quantile handed to the wrong stratum cannot pass unnoticed.
enw_samples_fixture <- function() {
  cells <- enw_summary_fixture()
  do.call(rbind, lapply(seq_len(nrow(cells)), function(i) {
    data.frame(
      reference_date = cells$reference_date[i],
      .group         = cells$.group[i],
      age_group      = cells$age_group[i],
      sample         = cells$median[i] + seq(-4, 5),
      stringsAsFactors = FALSE
    )
  }))
}

local_mock_epinowcast <- function(by = list("age_group"), frame = parent.frame()) {
  testthat::local_mocked_s3_method(
    "summary", "epinowcast",
    function(object, type = "nowcast", ...) {
      switch(type,
        nowcast         = enw_summary_fixture(),
        nowcast_samples = enw_samples_fixture(),
        stop("unexpected type")
      )
    },
    frame = frame
  )
  structure(list(by = by), class = c("epinowcast", "list"))
}

test_that("tidy() reports one block per epinowcast `by` group", {
  skip_if_not_installed("epinowcast")
  fit <- local_mock_epinowcast()
  out <- tidy(fit)

  expect_setequal(unique(out$stratum), c("00+", "80+"))
  expect_unique_stratum_key(out)
  expect_equal(nrow(out), 6L)

  # The estimate must stay with its own stratum, not with whichever row sorted
  # first: 80+ is roughly a tenth of 00+.
  expect_equal(out$estimate[out$stratum == "00+"], c(204, 193, 132))
  expect_equal(out$estimate[out$stratum == "80+"], c(16, 25, 11))
  # epinowcast's default band is q5-q95.
  expect_equal(unique(out$level), 0.90)
})

test_that("tidy() keeps `stratum` = 'all' when epinowcast has no grouping", {
  skip_if_not_installed("epinowcast")
  # `by` is `list(NULL)` on an ungrouped fit, which is what `enw_example()`
  # carries; the grouping columns present in the summary must be ignored.
  fit <- local_mock_epinowcast(by = list(NULL))
  out <- tidy(fit)

  expect_equal(unique(out$stratum), "all")
  # Without strata the six rows are two per date, so the key is NOT unique --
  # that is the fit's shape, and `tidy()` must not invent a label to fix it.
  expect_equal(nrow(out), 6L)
})

test_that("epinowcast `probs` quantiles stay with their own stratum", {
  skip_if_not_installed("epinowcast")
  fit <- local_mock_epinowcast()
  out <- tidy(fit, probs = c(0.05, 0.5, 0.95))

  expect_true(all(c("q5", "q50", "q95") %in% names(out)))
  expect_true(all(out$q5 <= out$q95))
  # The draws were built as median + (-4:5), so the median of the draws is the
  # summary's own median plus 0.5. Checking that per row is what catches a
  # quantile column pasted on in `split()` order instead of row order.
  expect_equal(out$q50, out$estimate + 0.5)
})

# --- NobBS: `NobBS.strat()` names its strata in a column ----------------------

# `NobBS()` returns `estimate`/`lower`/`upper`/`onset_date`/`n.reported`.
# `NobBS.strat()` returns the same plus a `stratum` column (and, because it
# `cbind()`s the quantile block, a duplicate `stratum.1` -- which must not be
# mistaken for the label).
nobbs_strat_fixture <- function() {
  dates <- as.Date("1990-01-01") + 7 * (0:2)
  est <- data.frame(
    estimate   = c(31, 25, 23, 30, 24, 22),
    lower      = c(31, 25, 22, 30, 24, 21),
    upper      = c(31, 28, 25, 30, 27, 24),
    stratum    = rep(c("odd", "even"), each = 3),
    q_0.025    = c(31, 25, 22, 30, 24, 21),
    q_0.975    = c(31, 28, 25, 30, 27, 24),
    stratum.1  = rep(c("odd", "even"), each = 3),
    onset_date = rep(dates, times = 2),
    n.reported = c(31, 25, 22, 30, 24, 21),
    stringsAsFactors = FALSE
  )
  list(estimates = est, estimates.inflated = est)
}

test_that("tidy() reports one block per NobBS.strat() stratum", {
  out <- tidy(nobbs_strat_fixture())

  expect_setequal(unique(out$stratum), c("odd", "even"))
  expect_unique_stratum_key(out)
  expect_equal(nrow(out), 6L)
  expect_equal(out$estimate[out$stratum == "odd"], c(31, 25, 23))
  expect_equal(out$estimate[out$stratum == "even"], c(30, 24, 22))
  expect_equal(unique(out$engine), "NobBS")
})

test_that("an unstratified NobBS fit still reports 'all'", {
  plain <- list(estimates = data.frame(
    estimate   = c(10, 8),
    lower      = c(6, 2),
    upper      = c(15, 19),
    onset_date = as.Date(c("2002-07-01", "2002-07-08")),
    n.reported = c(9, 4)
  ))
  out <- tidy(plain)

  expect_equal(unique(out$stratum), "all")
  expect_unique_stratum_key(out)
})

# --- baselinenowcast: a point fit has no interval ----------------------------

test_that("tidy() returns NA bounds for a baselinenowcast point fit", {
  skip_if_not_installed("baselinenowcast")
  skip_on_cran()
  fit <- suppressWarnings(suppressMessages(baselinenowcast::baselinenowcast(
    baselinenowcast::example_reporting_triangle,
    output_type = "point"
  )))

  out <- tidy(fit)
  expect_true(all(is.na(out$conf.low)))
  expect_true(all(is.na(out$conf.high)))
  expect_true(all(is.na(out$level)))
  # The point estimate itself is real and must survive.
  expect_false(anyNA(out$estimate))
  expect_equal(unique(out$engine), "baselinenowcast")
})

test_that("tidy() refuses `probs` on a baselinenowcast point fit", {
  skip_if_not_installed("baselinenowcast")
  skip_on_cran()
  fit <- suppressWarnings(suppressMessages(baselinenowcast::baselinenowcast(
    baselinenowcast::example_reporting_triangle,
    output_type = "point"
  )))
  # One value per reference date means every quantile of it is that value; an
  # approximation dressed up as a quantile is exactly what `probs` must not do.
  expect_error(tidy(fit, probs = 0.5), "output_type")
})

test_that("a baselinenowcast SAMPLES fit still reports a 95% band", {
  skip_if_not_installed("baselinenowcast")
  skip_on_cran()
  fit <- suppressWarnings(suppressMessages(baselinenowcast::baselinenowcast(
    baselinenowcast::example_reporting_triangle,
    output_type = "samples", draws = 50
  )))

  out <- tidy(fit)
  expect_equal(unique(out$level), 0.95)
  expect_false(anyNA(out$conf.low))
  expect_true(all(out$conf.low <= out$estimate))
  expect_true(all(out$estimate <= out$conf.high))
})

# --- a per-stratum LIST of fits is not tidyable ------------------------------

# --- a per-stratum LIST of baselinenowcast fits ------------------------------

# `?tbl_now_triangle_list` tells the reader to run
# `lapply(triangles, baselinenowcast::baselinenowcast)`, which yields a bare list
# of `baselinenowcast_df`s. `tidy()` used to refuse it (and point at NobBS).
fake_bnc_fit <- function(base) {
  structure(
    data.frame(
      pred_count     = rep(base + c(-2, 0, 2), each = 1),
      reference_date = rep(as.Date("2024-01-01") + 0:1, each = 3),
      draw           = rep(1:3, times = 2),
      output_type    = "samples"
    ),
    class = c("baselinenowcast_df", "data.frame")
  )
}

test_that("tidy() tidies a list of per-stratum baselinenowcast fits", {
  fits <- list(alpha = fake_bnc_fit(10), zulu = fake_bnc_fit(100))
  out <- tidy(fits)

  expect_setequal(unique(out$stratum), c("alpha", "zulu"))
  expect_unique_stratum_key(out)
  expect_equal(nrow(out), 4L)
  expect_equal(unique(out$engine), "baselinenowcast")
  # The estimate must stay with its own list element.
  expect_equal(unique(out$estimate[out$stratum == "alpha"]), 10)
  expect_equal(unique(out$estimate[out$stratum == "zulu"]), 100)
})

test_that("a per-stratum list passes `probs` through and falls back on names", {
  fits <- list(fake_bnc_fit(10), fake_bnc_fit(100))
  out <- tidy(fits, probs = c(0.05, 0.95))

  expect_true(all(c("q5", "q95") %in% names(out)))
  expect_true(all(out$q5 <= out$q95))
  # An unnamed list is labelled by position rather than collapsed to "all".
  expect_setequal(unique(out$stratum), c("1", "2"))
})

test_that("a list that is neither shape still errors helpfully", {
  expect_error(tidy(list(a = 1, b = 2)), "Don't know how to")
  # A list with only SOME baselinenowcast_df elements is not a per-stratum list.
  expect_error(tidy(list(fake_bnc_fit(10), 42)), "Don't know how to")
})

# --- NobBS: the width it does not report -------------------------------------

test_that("tidy() reports NA level for NobBS and lets the caller set it", {
  # `NobBS()` returns `list(estimates, estimates.inflated, nowcast.post.samps,
  # params.post)` -- no `specs`, so `specs$conf` (which produced lower/upper) is
  # unrecoverable. Guessing its 0.95 default would put a number in the one column
  # that exists to stop widths being compared blindly.
  fit <- nobbs_strat_fixture()

  expect_true(all(is.na(tidy(fit)$level)))
  expect_equal(unique(tidy(fit, level = 0.5)$level), 0.5)
  # The bounds themselves are untouched by `level`.
  expect_equal(tidy(fit)$conf.low, tidy(fit, level = 0.5)$conf.low)

  expect_error(tidy(fit, level = 0), "strictly between 0 and 1")
  expect_error(tidy(fit, level = 1.5), "strictly between 0 and 1")
  expect_error(tidy(fit, level = c(0.5, 0.9)), "strictly between 0 and 1")
})

# --- diseasenowcasting: the method tbl.now is taking over ---------------------

test_that("tbl.now's diseasenowcasting tidy matches the one it replaces", {
  skip_on_cran()
  skip_if_not_installed("diseasenowcasting")
  # diseasenowcasting 2.1.0 still owns the registered method, so `tidy()`
  # dispatches to theirs; 2.2.0 removes it and `.onLoad()` then registers this
  # one. Call ours directly and check the handover is not a behaviour change.
  data(denguedat, envir = environment())
  d <- denguedat |>
    dplyr::filter(
      onset_week  >= as.Date("2009-06-01"),
      onset_week  <  as.Date("2010-01-01"),
      report_week <  as.Date("2010-01-01")
    )
  d$sex <- ifelse(seq_len(nrow(d)) %% 3 == 0, "F", "M")
  x <- tbl_now(
    d, event_date = "onset_week", report_date = "report_week",
    strata = "sex", data_type = "linelist", verbose = FALSE
  )
  fit <- suppressWarnings(suppressMessages(
    diseasenowcasting::nowcast(x, seed = 42L)
  ))
  prediction <- suppressWarnings(suppressMessages(stats::predict(fit)))

  ours <- tbl.now:::tidy_nowcast_prediction(prediction)
  expect_equal(
    names(ours),
    c("event_date", "stratum", "estimate", "conf.low", "conf.high", "level",
      "engine")
  )
  expect_s3_class(ours$event_date, "Date")
  expect_equal(unique(ours$engine), "diseasenowcasting")
  # A stratified fit reports one block per stratum, never one pooled "all".
  expect_setequal(unique(ours$stratum), c("F", "M"))
  expect_unique_stratum_key(ours)

  theirs <- suppressWarnings(suppressMessages(generics::tidy(prediction)))
  expect_equal(dim(ours), dim(theirs))
  expect_equal(ours$estimate, theirs$estimate)
  expect_equal(ours$conf.low, theirs$conf.low)
  expect_setequal(unique(ours$stratum), unique(theirs$stratum))
})

test_that("the diseasenowcasting tidy takes `level`, and refuses `conf.level`", {
  skip_on_cran()
  skip_if_not_installed("diseasenowcasting")
  data(denguedat, envir = environment())
  d <- denguedat |>
    dplyr::filter(
      onset_week  >= as.Date("2009-09-01"),
      onset_week  <  as.Date("2010-01-01"),
      report_week <  as.Date("2010-01-01")
    )
  x <- tbl_now(
    d, event_date = "onset_week", report_date = "report_week",
    data_type = "linelist", verbose = FALSE
  )
  prediction <- suppressWarnings(suppressMessages(
    stats::predict(diseasenowcasting::nowcast(x, seed = 42L))
  ))

  expect_equal(
    unique(tbl.now:::tidy_nowcast_prediction(prediction, level = 0.5)$level), 0.5
  )
  # diseasenowcasting called it `conf.level`. Letting that fall into `...` would
  # silently ignore it and report a 95% interval, so it is an error instead.
  expect_error(
    tbl.now:::tidy_nowcast_prediction(prediction, conf.level = 0.9),
    "not an argument"
  )
  expect_true(all(
    c("q5", "q95") %in%
      names(tbl.now:::tidy_nowcast_prediction(prediction, probs = c(0.05, 0.95)))
  ))
})
