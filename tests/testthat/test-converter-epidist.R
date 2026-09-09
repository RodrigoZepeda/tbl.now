# Regression tests for the epidist converter.
#
# These lock the three failure modes the audit surfaced:
#
# 1. `obs_date` (right truncation): the converter must not let epidist default
#    `obs_date` to `max(sdate_upr)`, which collapses the truncation clock to
#    the last observed report and stops the correction at exactly the tail a
#    nowcast is about. `tbl_now_to_epidist()` sets `obs_date = now + w`; the
#    parallel EpiNow2 branch has always done so (R/converters.R,
#    `tbl_now_to_EpiNow2(target = "estimate_dist")`).
#
# 2. Round trip: `tbl_now_from_epidist()` must recover `now` from `obs_date`
#    on the auto path, so `to_epidist() |> from_epidist()` preserves `now`.
#
# 3. Revision axis: epidist models one delay. When the caller declared a
#    revision process, the converter warns once that it is dropped, rather than
#    silently ignoring it (DEVELOPMENT_SKILL Definition of Done).
#
# The tests are structural. They call the epidist constructors and inspect the
# columns rather than fitting a model.

skip_if_not_installed("epidist")

# --- fixtures ---------------------------------------------------------------

# A count-incidence tbl_now whose LAST event period is silent -- exactly the
# case that surfaces the obs_date blocker (max(report_date) < now).
silent_tail_now <- function() {
  events  <- as.Date("2024-01-01") + 0:20
  reports <- events + 3L
  df <- data.frame(
    event  = rep(events, times = 1L),
    report = rep(reports, times = 1L),
    n      = rep(1L, length(events))
  )
  # Drop the last 5 event dates' reports -- so max(report_date) is well before
  # the true `now`.
  df <- df[df$event <= as.Date("2024-01-15"), ]
  tbl_now(df,
    event_date = "event", report_date = "report", case_count = "n",
    data_type = "count-incidence", units = "days",
    now = as.Date("2024-01-25"), verbose = FALSE
  )
}

# A tbl_now with a revision process, for the revision-drop warning.
revision_now <- function() {
  df <- data.frame(
    event    = as.Date("2024-01-01") + rep(0:4, each = 2),
    report   = as.Date("2024-01-03") + rep(0:4, each = 2),
    revision = as.Date("2024-01-05") + rep(0:4, each = 2),
    outcome  = rep(c("confirmed", "retracted"), 5),
    n        = 1L
  )
  tbl_now(df,
    event_date = "event", report_date = "report",
    revision_date = "revision", revision_type = "outcome",
    case_count = "n", data_type = "count-incidence", units = "days",
    verbose = FALSE
  )
}

quiet_epidist <- function(expr) suppressWarnings(suppressMessages(force(expr)))

# --- 1. obs_date is written and equals get_now(x) + w ------------------------

test_that("tbl_now_to_epidist() sets obs_date to get_now(x) + w by default", {
  x <- silent_tail_now()
  out <- quiet_epidist(tbl_now_to_epidist(x, verbose = FALSE, quiet = TRUE))

  expect_true("obs_date" %in% names(out))
  expected <- get_now(x) + 1L # daily => w = 1
  expect_true(all(out$obs_date == expected))

  # And the truncation clock ends AFTER the last report, not at it.
  expect_true(all(out$obs_date >= out$sdate_upr))
  expect_gt(as.numeric(unique(out$obs_date) - max(out$sdate_upr)), 0)
})

test_that("tbl_now_to_epidist() honours a user-supplied obs_date", {
  x <- silent_tail_now()
  chosen <- as.Date("2024-02-01")
  out <- quiet_epidist(
    tbl_now_to_epidist(x, obs_date = chosen, verbose = FALSE, quiet = TRUE)
  )
  expect_true(all(out$obs_date == chosen))
})

test_that("tbl_now_to_epidist() sets obs_date on the aggregate path", {
  x <- silent_tail_now()
  out <- quiet_epidist(
    tbl_now_to_epidist(x, format = "aggregate", verbose = FALSE, quiet = TRUE)
  )
  expect_true(epidist::is_epidist_aggregate_data(out))
  expect_true("obs_date" %in% names(out))
  expect_true(all(out$obs_date == get_now(x) + 1L))
})

test_that("tbl_now_to_epidist() sets obs_date on the linelist path too", {
  # Line-list source: one row per case, no `n` column.
  df <- data.frame(
    event  = as.Date("2024-01-01") + rep(0:5, each = 2),
    report = as.Date("2024-01-04") + rep(0:5, each = 2)
  )
  x <- tbl_now(df, event_date = "event", report_date = "report",
    data_type = "linelist", units = "days",
    now = as.Date("2024-01-20"), verbose = FALSE
  )
  out <- quiet_epidist(tbl_now_to_epidist(x, verbose = FALSE, quiet = TRUE))
  expect_true(epidist::is_epidist_linelist_data(out))
  expect_true(all(out$obs_date == as.Date("2024-01-20") + 1L))
})

test_that("tbl_now_to_epidist() sets obs_date on the interval path", {
  # Interval source: user-carried upper-bound covariate columns.
  df <- data.frame(
    event   = as.Date("2024-01-01") + 0:4,
    report  = as.Date("2024-01-04") + 0:4,
    pupper  = as.Date("2024-01-02") + 0:4,
    supper  = as.Date("2024-01-05") + 0:4
  )
  x <- tbl_now(df, event_date = "event", report_date = "report",
    covariates = c("pupper", "supper"),
    data_type = "linelist", units = "days",
    now = as.Date("2024-01-20"), verbose = FALSE
  )
  out <- quiet_epidist(tbl_now_to_epidist(x,
    format = "interval", primary_upper = "pupper", secondary_upper = "supper",
    verbose = FALSE, quiet = TRUE
  ))
  expect_true("obs_date" %in% names(out))
  expect_true(all(out$obs_date == as.Date("2024-01-20") + 1L))
})

test_that("obs_date scales with the censoring window on weekly data", {
  df <- data.frame(
    event  = as.Date("2024-01-07") + 7 * (0:4),
    report = as.Date("2024-01-14") + 7 * (0:4),
    n      = 1L
  )
  x <- tbl_now(df, event_date = "event", report_date = "report",
    case_count = "n", data_type = "count-incidence", units = "weeks",
    now = as.Date("2024-02-11"), verbose = FALSE
  )
  out <- quiet_epidist(tbl_now_to_epidist(x, verbose = FALSE, quiet = TRUE))
  # weekly => w = 7 days
  expect_true(all(out$obs_date == as.Date("2024-02-11") + 7L))
})

test_that("obs_date length mismatch is a clear error", {
  x <- silent_tail_now()
  expect_error(
    tbl_now_to_epidist(x, obs_date = as.Date(c("2024-02-01", "2024-02-02")),
      verbose = FALSE, quiet = TRUE
    ),
    "must have length 1"
  )
})

# --- 2. round trip preserves `now` ------------------------------------------

test_that("tbl_now_from_epidist() recovers `now` from obs_date", {
  x <- silent_tail_now()
  out  <- quiet_epidist(tbl_now_to_epidist(x, verbose = FALSE, quiet = TRUE))
  back <- quiet_epidist(tbl_now_from_epidist(out, verbose = FALSE))

  expect_true(is_tbl_now(back))
  expect_equal(get_now(back), get_now(x))
})

test_that("round-tripped `now` differs from max(report_date)", {
  # The whole point: without the fix, from_epidist() would read `now` as
  # max(report_date). Assert the recovered `now` is strictly later.
  x <- silent_tail_now()
  out  <- quiet_epidist(tbl_now_to_epidist(x, verbose = FALSE, quiet = TRUE))
  back <- quiet_epidist(tbl_now_from_epidist(out, verbose = FALSE))
  max_rep <- max(back[[get_report_date(back)]], na.rm = TRUE)

  expect_gt(as.numeric(get_now(back) - max_rep), 0)
})

test_that("explicit `now` argument to tbl_now_from_epidist() wins over recovery", {
  x <- silent_tail_now()
  out <- quiet_epidist(tbl_now_to_epidist(x, verbose = FALSE, quiet = TRUE))
  chosen <- as.Date("2024-02-15")
  back <- quiet_epidist(
    tbl_now_from_epidist(out, now = chosen, verbose = FALSE)
  )
  expect_equal(get_now(back), chosen)
})

# --- 3. revision axis warning -----------------------------------------------

test_that("tbl_now_to_epidist() warns once when a revision process is dropped", {
  x <- revision_now()
  expect_warning(
    suppressMessages(tbl_now_to_epidist(x, verbose = FALSE)),
    "revision process is dropped"
  )
})

test_that("quiet = TRUE silences the revision-dropped warning", {
  x <- revision_now()
  # No revision warning at all under quiet = TRUE.
  seen <- character(0)
  withCallingHandlers(
    suppressMessages(tbl_now_to_epidist(x, verbose = FALSE, quiet = TRUE)),
    warning = function(w) {
      if (grepl("revision", conditionMessage(w))) {
        seen <<- c(seen, conditionMessage(w))
      }
      invokeRestart("muffleWarning")
    }
  )
  expect_length(seen, 0L)
})

test_that("no revision warning fires on a plain tbl_now", {
  x <- silent_tail_now()
  seen <- character(0)
  withCallingHandlers(
    suppressMessages(tbl_now_to_epidist(x, verbose = FALSE)),
    warning = function(w) {
      if (grepl("revision", conditionMessage(w))) {
        seen <<- c(seen, conditionMessage(w))
      }
      invokeRestart("muffleWarning")
    }
  )
  expect_length(seen, 0L)
})

# --- 4. tidy.epidist_fit() bookkeeping filter -------------------------------

test_that("tidy.epidist_fit() strips epidist's marginal-model bookkeeping columns", {
  # Simulate the shape `predict_delay_parameters()` returns for an aggregate/
  # marginal model: a `weight` (or `n`) column plus `.observation`/`.row`. Neither
  # is a delay parameter and each is numeric, so a blocklist that misses them
  # would leak them into the tidy output as bogus "parameters".
  fake_draws <- data.frame(
    draw    = rep(1:5, each = 2),
    .draw   = rep(1:5, each = 2),
    index   = rep(1:2, times = 5),
    .observation = rep(1:2, times = 5),
    .row    = rep(1:2, times = 5),
    obs     = rep(1:2, times = 5),
    n       = 42L,
    weight  = 42L,
    mu      = rnorm(10, 1.5, 0.1),
    sigma   = runif(10, 0.4, 0.6)
  )
  testthat::local_mocked_bindings(
    predict_delay_parameters = function(fit, newdata = NULL, ...) fake_draws,
    add_mean_sd = function(data, ...) {
      data$mean <- exp(data$mu + data$sigma^2 / 2)
      data$sd   <- data$mean * sqrt(exp(data$sigma^2) - 1)
      data
    },
    .package = "epidist"
  )

  fake_fit <- structure(list(), class = c("brmsfit", "epidist_fit"))
  out <- suppressWarnings(generics::tidy(fake_fit))

  # Only real distribution parameters land in `term`.
  expect_setequal(out$term, c("mu", "sigma", "mean", "sd"))
  expect_false(any(c("n", "weight", ".observation", ".row", "obs") %in% out$term))
})

# --- 5. grouped input keeps working ----------------------------------------

test_that("obs_date fix does not break a grouped tbl_now", {
  df <- data.frame(
    onset    = as.Date("2024-01-01") + rep(0:9, each = 2),
    reported = as.Date("2024-01-03") + rep(0:9, each = 2),
    sex      = rep(c("F", "M"), 10),
    n        = 1L
  )
  x <- tbl_now(df,
    event_date = "onset", report_date = "reported", case_count = "n",
    strata = "sex", data_type = "count-incidence", units = "days",
    now = as.Date("2024-02-01"), verbose = FALSE
  )
  a <- quiet_epidist(tbl_now_to_epidist(x, verbose = FALSE, quiet = TRUE))
  b <- quiet_epidist(
    tbl_now_to_epidist(dplyr::group_by(x, .data$sex),
      verbose = FALSE, quiet = TRUE
    )
  )
  expect_equal(a, b)
})
