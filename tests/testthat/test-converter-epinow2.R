# EpiNow2 takes four different input shapes, one per entry point, and the
# conversion is wrong in a different way for each. The dimensions that matter:
#
#   * EpiNow2 models a DAILY process and (as of 1.9.0) has no `timestep`, so a
#     weekly series passed as one row per week is read as one row per day -- no
#     error, just an epidemic seven times too fast. `accumulate` is the fix, and
#     the tests below check the grid it produces, not merely that one exists.
#   * `estimate_truncation()` is the only EpiNow2 model that uses the report
#     dimension, so it is the only one with an inverse. That inverse must be
#     exact, and `EpiNow2::example_truncated` is the fixture to prove it on --
#     neither end of that check is ours.
#   * `estimate_dist()` (new in 1.9.0) takes the epidist schema, so it shares a
#     builder with `tbl_now_to_epidist()`; the two must not drift apart.
#
# No fit is run here: EpiNow2 needs cmdstan. `tidy()` is tested against mocked
# summaries built from the shape of real ones.

q <- function(expr) suppressWarnings(suppressMessages(expr))

epinow2_weekly <- function(strata = FALSE) {
  data(denguedat, envir = environment())
  d <- denguedat |>
    dplyr::filter(
      onset_week  >= as.Date("1990-01-01"),
      onset_week  <  as.Date("1990-08-01"),
      report_week <  as.Date("1990-08-01")
    )
  if (strata) {
    # Two strata in a 2:1 ratio, so a mispaired label is arithmetically obvious.
    d$grp <- ifelse(seq_len(nrow(d)) %% 3 == 0, "b", "a")
  }
  tbl_now(
    d,
    event_date = "onset_week", report_date = "report_week",
    strata = if (strata) "grp" else NULL,
    data_type = "linelist", verbose = FALSE
  )
}

# --- the daily grid ----------------------------------------------------------

test_that("a weekly series is laid on EpiNow2's daily grid with `accumulate`", {
  skip_if_not_installed("EpiNow2")
  x <- epinow2_weekly()
  out <- q(tbl_now_to_EpiNow2(x, verbose = FALSE, quiet = TRUE))

  expect_true(all(c("date", "confirm", "accumulate") %in% names(out)))
  # Dense daily grid: EpiNow2 has no timestep, so any gap is read as a real day.
  expect_true(all(diff(out$date) == 1))
  expect_type(out$accumulate, "logical")
  # One carrying row per event week, and -- the part that was wrong before --
  # each observation stays on the date it was GIVEN. `fill_missing()` back-fills
  # the days BEFORE it; an earlier version put it on the week's last day instead,
  # shifting every weekly fit six days late.
  expect_equal(sum(!out$accumulate), length(unique(x$onset_week)))
  expect_setequal(out$date[!out$accumulate], unique(x$onset_week))
})

test_that("the grid is exactly what EpiNow2::fill_missing() would build", {
  skip_if_not_installed("EpiNow2")
  # The converter delegates to `fill_missing()` rather than laying the grid out
  # by hand. This pins that: reimplementing it is how the six-day shift got in.
  weekly <- data.frame(
    date = as.Date("2024-01-01") + 7 * (0:3), confirm = c(10, 20, 30, 40)
  )
  theirs <- as.data.frame(q(EpiNow2::fill_missing(
    weekly, missing_dates = "accumulate", obs_column = "confirm"
  )))
  ours <- tbl.now:::.epinow2_grid(weekly, "weeks")

  expect_equal(ours$date, theirs$date)
  expect_equal(ours$confirm, theirs$confirm)
  expect_equal(ours$accumulate, theirs$accumulate)
})

test_that("the daily expansion preserves the case total exactly", {
  skip_if_not_installed("EpiNow2")
  x <- epinow2_weekly()
  out <- q(tbl_now_to_EpiNow2(x, verbose = FALSE, quiet = TRUE))

  latest <- q(get_latest_reported_cases(x))
  expect_equal(
    sum(out$confirm, na.rm = TRUE), sum(latest[[get_case_count(latest)]])
  )
  # Filler days carry NA, not 0 -- "no observation for this day", which is what
  # `fill_missing()` writes and is the honest encoding. A 0 there would be an
  # observed zero.
  expect_true(all(is.na(out$confirm[out$accumulate])))
})

test_that("daily data passes through with no `accumulate` column", {
  skip_if_not_installed("EpiNow2")
  daily <- tbl_now(
    data.frame(
      ev = as.Date("2024-01-01") + c(0, 0, 1, 2),
      rp = as.Date("2024-01-01") + c(0, 1, 1, 2)
    ),
    event_date = "ev", report_date = "rp",
    data_type = "linelist", verbose = FALSE
  )
  out <- q(tbl_now_to_EpiNow2(daily, verbose = FALSE, quiet = TRUE))

  expect_named(out, c("date", "confirm"))
  expect_false("accumulate" %in% names(out))
})

test_that("EpiNow2 refuses grids it cannot lay on a daily axis", {
  skip_if_not_installed("EpiNow2")
  numeric_grid <- tbl_now(
    data.frame(
      ev = c(1L, 1L, 2L, 3L), rp = c(1L, 2L, 2L, 3L), n = c(2, 1, 3, 5)
    ),
    event_date = "ev", report_date = "rp", case_count = "n",
    data_type = "count-incidence", verbose = FALSE
  )
  # The error must name the units AND `accumulate`, so the reader knows both what
  # is wrong and what the mechanism is.
  expect_error(
    q(tbl_now_to_EpiNow2(numeric_grid, verbose = FALSE, quiet = TRUE)),
    "numeric"
  )
  expect_error(
    q(tbl_now_to_EpiNow2(numeric_grid, verbose = FALSE, quiet = TRUE)),
    "accumulate"
  )
})

# --- regional_epinow ---------------------------------------------------------

test_that("strata become a `region` column, each keeping its own cases", {
  skip_if_not_installed("EpiNow2")
  x <- epinow2_weekly(strata = TRUE)
  out <- q(tbl_now_to_EpiNow2(
    x, target = "regional_epinow", verbose = FALSE, quiet = TRUE
  ))

  expect_true(all(c("date", "confirm", "region") %in% names(out)))
  totals <- tapply(out$confirm, out$region, sum, na.rm = TRUE)
  truth <- table(as.character(dplyr::as_tibble(x)$grp))
  expect_setequal(names(totals), names(truth))
  # The decisive check: each region's total must match ITS OWN label, not merely
  # be one of the two totals.
  expect_equal(as.numeric(totals[names(truth)]), as.numeric(truth))
})

test_that("no region loses its first period to the accumulate grid", {
  skip_if_not_installed("EpiNow2")
  # Regression, and the kind that only shows up on real data. `fill_missing()`
  # infers `initial_accumulate` when it is not given, and with `by` that
  # inference DROPS each group's first observation: a 336/167 two-region weekly
  # series came back as 295/147, with the grid starting the day after the first
  # report. The converter passes `initial_accumulate` explicitly.
  #
  # A regular series does not reproduce it, so this uses the real one.
  x <- epinow2_weekly(strata = TRUE)
  out <- q(tbl_now_to_EpiNow2(
    x, target = "regional_epinow", verbose = FALSE, quiet = TRUE
  ))

  first_period <- min(dplyr::as_tibble(x)$onset_week)
  # The first event period must survive as a real observation in every region,
  # not be swallowed by the initial accumulation window.
  carrying <- out[!out$accumulate, ]
  for (region in unique(out$region)) {
    expect_true(
      first_period %in% carrying$date[carrying$region == region],
      info = region
    )
  }
  # And the grid must reach back far enough that the first period is a full
  # window, not a one-day one.
  expect_lte(min(out$date), first_period)
})

test_that("the single-series targets pool strata with a warning", {
  skip_if_not_installed("EpiNow2")
  x <- epinow2_weekly(strata = TRUE)
  expect_warning(
    out <- suppressMessages(
      tbl_now_to_EpiNow2(x, verbose = FALSE, quiet = TRUE)
    ),
    "pooling over strata"
  )
  expect_false("region" %in% names(out))
  expect_equal(sum(out$confirm, na.rm = TRUE), nrow(dplyr::as_tibble(x)))
})

# --- estimate_truncation snapshots -------------------------------------------

test_that("snapshots are one per report date, shortest first, non-decreasing", {
  skip_if_not_installed("EpiNow2")
  x <- epinow2_weekly()
  snaps <- q(tbl_now_to_EpiNow2(
    x, target = "estimate_truncation", verbose = FALSE, quiet = TRUE
  ))

  expect_s3_class(snaps, "tbl_now_epinow2_snapshots")
  expect_true(is.list(snaps))          # thin class: still a plain list
  expect_length(snaps, 5L)             # EpiNow2's own example ships five
  expect_length(attr(snaps, "report_dates"), 5L)

  # `get_predictions.estimate_truncation()` reorders by nrow, so emit them
  # shortest-to-longest or the caller's indices stop matching the fit's.
  expect_false(is.unsorted(vapply(snaps, nrow, integer(1))))
  # A snapshot can only gain reports.
  totals <- vapply(snaps, function(s) sum(s$confirm, na.rm = TRUE), numeric(1))
  expect_false(is.unsorted(totals))
})

test_that("`snapshots` caps how many are emitted", {
  skip_if_not_installed("EpiNow2")
  x <- epinow2_weekly()
  snaps <- q(tbl_now_to_EpiNow2(
    x, target = "estimate_truncation", snapshots = 3,
    verbose = FALSE, quiet = TRUE
  ))
  # One per report date would be dozens here, and a fit that never finishes.
  expect_length(snaps, 3L)
})

test_that("each snapshot is the series as known at its own report date", {
  skip_if_not_installed("EpiNow2")
  x <- epinow2_weekly()
  snaps <- q(tbl_now_to_EpiNow2(
    x, target = "estimate_truncation", verbose = FALSE, quiet = TRUE
  ))
  report_dates <- attr(snaps, "report_dates")

  for (k in seq_along(snaps)) {
    as_of <- report_dates[k]
    known <- sum(dplyr::as_tibble(x)$report_week <= as_of)
    expect_equal(
      sum(snaps[[k]]$confirm, na.rm = TRUE), known, info = as.character(as_of)
    )
  }
})

# --- the inverse -------------------------------------------------------------

test_that("snapshots round-trip back into a tbl_now with every case", {
  skip_if_not_installed("EpiNow2")
  x <- epinow2_weekly()
  snaps <- q(tbl_now_to_EpiNow2(
    x, target = "estimate_truncation", verbose = FALSE, quiet = TRUE
  ))
  back <- q(as_tbl_now(snaps))

  expect_true(is_tbl_now(back))
  expect_equal(get_data_type(back), "count-incidence")
  # Differencing consecutive snapshots recovers the increments, so the total is
  # the latest snapshot's total.
  expect_equal(
    sum(back[[get_case_count(back)]], na.rm = TRUE),
    sum(snaps[[length(snaps)]]$confirm, na.rm = TRUE)
  )
  # The daily expansion puts a week's count on the week's LAST day; the inverse
  # must map it back to the week start, or every event date lands six days late
  # and the object fills with spurious negative delays.
  expect_true(all(back$.delay >= 0))
  expect_equal(get_now(back), attr(snaps, "now"))
})

test_that("EpiNow2's own example_truncated round-trips", {
  skip_if_not_installed("EpiNow2")
  # The strongest available check: neither end of it is ours.
  snapshots <- EpiNow2::example_truncated
  report_dates <- as.Date(vapply(
    snapshots, function(s) as.character(max(s$date)), character(1)
  ))
  back <- q(tbl_now_from_EpiNow2(
    snapshots, report_dates = report_dates, verbose = FALSE
  ))

  expect_true(is_tbl_now(back))
  largest <- max(vapply(snapshots, function(s) sum(s$confirm), numeric(1)))
  expect_equal(sum(back[[get_case_count(back)]], na.rm = TRUE), largest)
  expect_true(all(back$.delay >= 0))
})

test_that("the inverse refuses what it genuinely cannot invert", {
  skip_if_not_installed("EpiNow2")
  # A single series has no report dimension to recover.
  expect_error(
    tbl_now_from_EpiNow2(
      data.frame(date = as.Date("2024-01-01"), confirm = 1), verbose = FALSE
    ),
    "list of"
  )
  # A bare list does not record WHEN each snapshot was taken.
  expect_error(
    tbl_now_from_EpiNow2(EpiNow2::example_truncated, verbose = FALSE),
    "report_dates"
  )
  expect_error(
    tbl_now_from_EpiNow2(
      EpiNow2::example_truncated,
      report_dates = as.Date("2020-01-01"), verbose = FALSE
    ),
    "snapshot"
  )
})

# --- estimate_dist -----------------------------------------------------------

test_that("estimate_dist gets the schema EpiNow2 documents", {
  skip_if_not_installed("EpiNow2")
  x <- epinow2_weekly()
  out <- q(tbl_now_to_EpiNow2(
    x, target = "estimate_dist", verbose = FALSE, quiet = TRUE
  ))

  expect_true(all(
    c("pdate_lwr", "pdate_upr", "sdate_lwr", "sdate_upr", "obs_date")
      %in% names(out)
  ))
  # A weekly object gets a 7-day censoring window: estimate_dist works in
  # windows, not on a grid, so it is exempt from the daily expansion.
  expect_equal(unique(as.numeric(out$pdate_upr - out$pdate_lwr)), 7)
  # `obs_date` is when observation STOPPED. The `now` LABELS a period -- for
  # weekly data the start of the last observed week -- so the instant observation
  # stopped is the END of it, `now + 7`. That is what makes
  # `estimate_dist()`'s `obs_date >= sdate_upr` assertion hold on every row
  # without moving any report into an earlier week.
  expect_equal(unique(out$obs_date), get_now(x) + 7)
  expect_true(all(out$obs_date >= out$sdate_upr))
  # A line list is one row per case.
  expect_equal(nrow(out), nrow(dplyr::as_tibble(x)))
})

test_that("estimate_dist and tbl_now_to_epidist build identical windows", {
  skip_if_not_installed("EpiNow2")
  skip_if_not_installed("epidist")
  # They share `.delay_censoring_windows()`. This is the regression guard on that
  # factoring: EpiNow2 1.9.0's estimate_dist() documents the epidist schema
  # exactly, so the two must not drift apart.
  x <- epinow2_weekly()
  dates <- c("pdate_lwr", "pdate_upr", "sdate_lwr", "sdate_upr")

  from_epinow2 <- q(tbl_now_to_EpiNow2(
    x, target = "estimate_dist", verbose = FALSE, quiet = TRUE
  ))
  from_epidist <- q(tbl_now_to_epidist(x, verbose = FALSE))

  expect_equal(
    as.data.frame(from_epinow2[, dates]),
    as.data.frame(from_epidist[, dates])
  )
})

test_that("count data reaches estimate_dist as `n` weights, with no empty cells", {
  skip_if_not_installed("EpiNow2")
  x <- epinow2_weekly()
  counts <- q(to_count(x, to = "count-incidence"))
  out <- q(tbl_now_to_EpiNow2(
    counts, target = "estimate_dist", verbose = FALSE, quiet = TRUE
  ))

  expect_true("n" %in% names(out))
  # Cases are conserved even though the row count collapses.
  expect_equal(sum(out$n), nrow(dplyr::as_tibble(x)))
  expect_lt(nrow(out), nrow(dplyr::as_tibble(x)))
  # `estimate_dist()` asserts `n >= 1` with the identical message epidist uses,
  # so the same filter has to run.
  expect_true(all(out$n >= 1))
})

test_that("estimate_dist warns that it pools strata", {
  skip_if_not_installed("EpiNow2")
  # `estimate_dist()` has no grouping argument (`args$n` is the observation
  # weight, not a stratum count), so it fits ONE distribution to everything.
  # Every other pooling path in the package says so; this must too.
  x <- epinow2_weekly(strata = TRUE)
  expect_warning(
    out <- suppressMessages(tbl_now_to_EpiNow2(
      x, target = "estimate_dist", verbose = FALSE, quiet = TRUE
    )),
    "pooling over strata"
  )
  # The columns are kept, so a caller can split and fit per stratum.
  expect_true("grp" %in% names(out))
})

test_that("a large share of zero delays warns before the fit misbehaves", {
  skip_if_not_installed("EpiNow2")
  # A lognormal has zero density at zero, so a point mass there does not error --
  # it inflates the variance. `?tbl_now_epidist` records a case where that gave
  # an implied mean delay of 1.5e73 days.
  same_day <- tbl_now(
    data.frame(
      ev = as.Date("2024-01-01") + c(0, 1, 2, 3, 4),
      rp = as.Date("2024-01-01") + c(0, 1, 2, 3, 5)
    ),
    event_date = "ev", report_date = "rp",
    data_type = "linelist", verbose = FALSE
  )
  expect_warning(
    suppressMessages(tbl_now_to_EpiNow2(
      same_day, target = "estimate_dist", verbose = FALSE, quiet = TRUE
    )),
    "delays are exactly zero"
  )

  # A series with few zero delays must stay quiet.
  spread <- tbl_now(
    data.frame(
      ev = as.Date("2024-01-01") + 0:4,
      rp = as.Date("2024-01-01") + c(3, 4, 6, 7, 9)
    ),
    event_date = "ev", report_date = "rp",
    data_type = "linelist", verbose = FALSE
  )
  zero_warnings <- function(expr) {
    seen <- character(0)
    withCallingHandlers(
      suppressMessages(expr),
      warning = function(cnd) {
        if (grepl("exactly zero", conditionMessage(cnd))) {
          seen <<- c(seen, conditionMessage(cnd))
        }
        invokeRestart("muffleWarning")
      }
    )
    seen
  }
  expect_length(
    zero_warnings(tbl_now_to_EpiNow2(
      spread, target = "estimate_dist", verbose = FALSE, quiet = TRUE
    )),
    0L
  )
})

test_that("estimate_dist keeps the censoring flag the others must drop", {
  skip_if_not_installed("EpiNow2")
  # Estimating a delay distribution is the one job that can USE a per-case
  # censoring flag, so this target does not collapse it -- exactly the carve-out
  # `tbl_now_to_epidist()` has.
  x <- epinow2_weekly()
  censored <- suppressMessages(censor_delays_above(x, max_delay = 3))

  censoring_warnings <- function(expr) {
    seen <- character(0)
    withCallingHandlers(
      suppressMessages(expr),
      warning = function(cnd) {
        if (grepl("censored delays", conditionMessage(cnd))) {
          seen <<- c(seen, conditionMessage(cnd))
        }
        invokeRestart("muffleWarning")
      }
    )
    seen
  }

  expect_length(
    censoring_warnings(tbl_now_to_EpiNow2(
      censored, target = "estimate_dist", verbose = FALSE, quiet = TRUE
    )),
    0L
  )
  # The date-keyed targets index by date, so they must collapse it.
  expect_length(
    censoring_warnings(tbl_now_to_EpiNow2(
      censored, verbose = FALSE, quiet = TRUE
    )),
    1L
  )
})

# --- line list vs counts ------------------------------------------------------

test_that("a line list and its own aggregate give the same series", {
  skip_if_not_installed("EpiNow2")
  x <- epinow2_weekly()
  counts <- q(to_count(x, to = "count-incidence"))

  expect_equal(
    q(tbl_now_to_EpiNow2(x, verbose = FALSE, quiet = TRUE)),
    q(tbl_now_to_EpiNow2(counts, verbose = FALSE, quiet = TRUE))
  )
})

# --- tidy() -------------------------------------------------------------------
#
# Every EpiNow2 summary goes through `calc_summary_measures()`: `median`, `mean`,
# `sd`, and one `lower_<pct>`/`upper_<pct>` pair per requested credible interval.
# `CrIs` is a USER argument, so the widths present depend on how the fit was made
# and `level` has to be read off the column names. The shapes below were copied
# from a real `estimate_dist()` fit on `denguedat`.

test_that(".epinow2_interval reads the width off the widest column pair", {
  default_cris <- data.frame(
    median = 1, mean = 1, sd = 0.1,
    lower_90 = 0.5, lower_50 = 0.8, lower_20 = 0.9,
    upper_20 = 1.1, upper_50 = 1.2, upper_90 = 1.5
  )
  out <- tbl.now:::.epinow2_interval(default_cris)
  expect_equal(out$level, 0.90)
  expect_equal(out$conf.low, 0.5)
  expect_equal(out$conf.high, 1.5)

  # A fit made with CrIs = c(0.5, 0.95) has no lower_90 at all. Hard-coding 0.90
  # would report a width the fit never produced.
  custom <- data.frame(
    median = 1, mean = 1, sd = 0.1,
    lower_95 = 0.3, lower_50 = 0.8, upper_50 = 1.2, upper_95 = 1.7
  )
  out <- tbl.now:::.epinow2_interval(custom)
  expect_equal(out$level, 0.95)
  expect_equal(out$conf.low, 0.3)
  expect_equal(out$conf.high, 1.7)

  # No interval columns at all -> NA, never a guess.
  bare <- data.frame(median = c(1, 2), mean = c(1, 2))
  out <- tbl.now:::.epinow2_interval(bare)
  expect_true(all(is.na(out$conf.low)))
  expect_true(is.na(out$level))
})

test_that("tidy() on an estimate_dist fit returns the DELAY schema", {
  skip_if_not_installed("EpiNow2")
  # Draws of `meanlog`/`sdlog` plus the derived delay moments -- the shape
  # `.epinow2_delay_draws()` returns, checked once against a real cmdstan fit.
  set.seed(1)
  fake_draws <- cbind(
    meanlog = rnorm(500, 2.29, 0.03),
    sdlog   = rnorm(500, 0.71, 0.03),
    mean    = rnorm(500, 12.49, 0.40),
    sd      = rnorm(500, 9.35, 0.45)
  )
  testthat::local_mocked_bindings(
    .epinow2_delay_draws = function(x) fake_draws
  )
  fit <- structure(list(), class = c("estimate_dist", "epinowfit", "list"))
  out <- tidy(fit)

  # Delay-shaped, not nowcast-shaped: `term`, never `event_date` or `stratum`.
  expect_named(
    out, c("term", "estimate", "conf.low", "conf.high", "level", "engine")
  )
  # The family's own parameters PLUS the derived moments, which is what makes
  # this comparable with `tidy.epidist_fit()`.
  expect_equal(out$term, c("meanlog", "sdlog", "mean", "sd"))
  expect_equal(unique(out$level), 0.95)
  expect_equal(unique(out$engine), "EpiNow2")
  expect_true(all(out$conf.low <= out$estimate))
  expect_true(all(out$estimate <= out$conf.high))
})

test_that("the draws columns line up with the family's parameter names", {
  skip_if_not_installed("EpiNow2")
  # `.epinow2_delay_draws()` labels the draws matrix with
  # `names(fix_parameters(spec)$parameters)`, relying on that order matching the
  # `delay_params` columns. For gamma (`shape`, `rate`) and weibull
  # (`shape`, `scale`) the family order is NOT alphabetical, so if anything
  # canonicalised the names the columns would transpose silently -- meanlog
  # reported as sdlog, and no test would notice.
  families <- list(
    lognormal = c("meanlog", "sdlog"),
    gamma     = c("shape", "rate"),
    normal    = c("mean", "sd"),
    exp       = "rate",
    weibull   = c("shape", "scale")
  )
  for (family in names(families)) {
    expected <- families[[family]]
    spec <- EpiNow2::new_dist_spec(
      params = stats::setNames(
        lapply(seq_along(expected), function(i) EpiNow2::Normal(i, 0.1)), expected
      ),
      max = 40, distribution = family
    )
    fixed <- EpiNow2::fix_parameters(spec)
    expect_equal(names(fixed$parameters), expected, info = family)
    # And the values stay with their own names.
    expect_equal(
      unname(unlist(fixed$parameters)), seq_along(expected),
      tolerance = 0.5, info = family
    )
  }
})

test_that("delay moments come from the distribution, not the family's algebra", {
  skip_if_not_installed("EpiNow2")
  # The moments are computed by discretising the fitted `dist_spec`, so a family
  # EpiNow2 adds later works without touching this package. Check the arithmetic
  # against the closed forms: the mean should be exact, the sd about 1% high
  # (the variance a discrete grid adds).
  moments <- function(spec) {
    pmf <- EpiNow2::discretise(spec)$pmf
    delay <- seq_along(pmf) - 1
    m <- sum(delay * pmf) / sum(pmf)
    c(mean = m, sd = sqrt(sum(delay^2 * pmf) / sum(pmf) - m^2))
  }

  lognormal <- moments(EpiNow2::LogNormal(meanlog = 1.5, sdlog = 0.5, max = 60))
  expect_equal(unname(lognormal["mean"]), exp(1.5 + 0.125), tolerance = 1e-3)
  expect_equal(
    unname(lognormal["sd"]), exp(1.5 + 0.125) * sqrt(exp(0.25) - 1),
    tolerance = 0.02
  )

  gamma_fit <- moments(EpiNow2::Gamma(shape = 2, rate = 0.5, max = 60))
  expect_equal(unname(gamma_fit["mean"]), 4, tolerance = 1e-3)
  expect_equal(unname(gamma_fit["sd"]), sqrt(2) / 0.5, tolerance = 0.02)

  weibull <- moments(EpiNow2::Weibull(shape = 1.5, scale = 5, max = 60))
  expect_equal(unname(weibull["mean"]), 5 * gamma(1 + 1 / 1.5), tolerance = 1e-3)
})

test_that("tidy() on estimate_dist honours `level` and `probs`", {
  skip_if_not_installed("EpiNow2")
  set.seed(2)
  fake_draws <- cbind(meanlog = rnorm(1000, 2, 0.1), sdlog = rnorm(1000, 0.7, 0.05))
  testthat::local_mocked_bindings(
    .epinow2_delay_draws = function(x) fake_draws
  )
  fit <- structure(list(), class = c("estimate_dist", "epinowfit", "list"))

  # The fit exposes draws, so `probs` is real rather than an approximation --
  # DEVELOPMENT_SKILL section 7's rule, and what `tidy.epidist_fit()` does.
  out <- tidy(fit, probs = c(0.05, 0.5, 0.95))
  expect_true(all(c("q5", "q50", "q95") %in% names(out)))
  expect_true(all(out$q5 <= out$q95))
  expect_equal(out$q50, out$estimate, tolerance = 1e-8)

  # `level` is the interval asked for, not whichever CrIs the fit happened to
  # use -- again matching `tidy.epidist_fit()`.
  narrow <- tidy(fit, level = 0.5)
  wide   <- tidy(fit, level = 0.99)
  expect_equal(unique(narrow$level), 0.5)
  expect_true(all(narrow$conf.low >= wide$conf.low))
  expect_true(all(narrow$conf.high <= wide$conf.high))

  expect_error(tidy(fit, level = 1.5), "strictly between 0 and 1")
})

test_that("tidy() on an estimate_infections fit meets the nowcast contract", {
  skip_if_not_installed("EpiNow2")
  fake_predictions <- data.frame(
    date     = as.Date("2020-03-01") + 0:2,
    median   = c(100, 120, 140),
    mean     = c(101, 121, 141),
    sd       = c(10, 11, 12),
    lower_90 = c(80, 96, 112),
    lower_50 = c(92, 110, 129),
    upper_50 = c(108, 130, 151),
    upper_90 = c(120, 144, 168)
  )
  # `registerS3method()` looks the generic up from the calling frame, and
  # EpiNow2 is not attached during tests, so bring it into scope first.
  get_predictions <- EpiNow2::get_predictions
  testthat::local_mocked_s3_method(
    "get_predictions", "estimate_infections",
    function(object, format = "summary", ...) fake_predictions
  )
  fit <- structure(list(), class = c("estimate_infections", "list"))
  out <- tidy(fit)

  expect_s3_class(out$event_date, "Date")
  expect_equal(out$estimate, c(100, 120, 140))
  expect_equal(out$conf.low, c(80, 96, 112))
  expect_equal(out$conf.high, c(120, 144, 168))
  expect_equal(unique(out$level), 0.90)
  expect_equal(unique(out$engine), "EpiNow2")
  expect_equal(unique(out$stratum), "all")
})

test_that("tidy() gives regional_epinow one block per region", {
  skip_if_not_installed("EpiNow2")
  # `regional_epinow()` returns a plain nested list, so `tidy.list()` has to
  # recognise it by structure. The two regions are an order of magnitude apart,
  # so a mispaired label is arithmetically unmistakable.
  make_predictions <- function(scale) {
    data.frame(
      date = as.Date("2020-03-01") + 0:1,
      median = c(10, 20) * scale,
      lower_90 = c(8, 16) * scale,
      upper_90 = c(12, 24) * scale
    )
  }
  predictions <- list(north = make_predictions(1), south = make_predictions(100))
  get_predictions <- EpiNow2::get_predictions
  testthat::local_mocked_s3_method(
    "get_predictions", "estimate_infections",
    function(object, format = "summary", ...) predictions[[object$region]]
  )
  fit <- list(regional = list(
    north = structure(list(region = "north"), class = c("estimate_infections", "list")),
    south = structure(list(region = "south"), class = c("estimate_infections", "list"))
  ))

  out <- tidy(fit)
  expect_setequal(unique(out$stratum), c("north", "south"))
  expect_equal(sum(duplicated(out[, c("stratum", "event_date")])), 0L)
  expect_equal(out$estimate[out$stratum == "north"], c(10, 20))
  expect_equal(out$estimate[out$stratum == "south"], c(1000, 2000))
})
