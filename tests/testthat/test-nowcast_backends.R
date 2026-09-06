# The real backends. Only `baselinenowcast` is fast enough to run routinely; the
# others need a Stan, JAGS or CmdStan toolchain and minutes per fit, so what is
# checked here is the part tbl.now actually owns: that each backend feeds its
# modelling package through the matching `tbl_now_to_*()` converter, with the
# arguments derived from the object. The modelling call itself is mocked.

backend_tbl_now <- function(strata = FALSE) {
  data(denguedat, package = "tbl.now", envir = environment())
  recent <- denguedat[denguedat$onset_week >= as.Date("2009-01-01"), ]

  tbl_now(recent,
    event_date = "onset_week", report_date = "report_week",
    strata = if (strata) "gender" else NULL,
    verbose = FALSE
  )
}

# Count data carrying far more cases than it has rows: the shape that makes a
# row-counting engine silently wrong.
counts_tbl_now <- function(strata = FALSE) {
  data <- tidyr::expand_grid(
    event_date = as.Date("2020-01-06") + 7 * (0:9),
    delay = 0:2,
    sex = c("F", "M")
  ) |>
    dplyr::mutate(
      report_date = .data$event_date + 7 * .data$delay,
      n = as.integer(100 - 20 * .data$delay)
    ) |>
    dplyr::select("event_date", "report_date", "sex", "n")

  if (!strata) {
    data <- data |>
      dplyr::group_by(.data$event_date, .data$report_date) |>
      dplyr::summarise(n = sum(.data$n), .groups = "drop")
  }

  tbl_now(data,
    event_date = "event_date", report_date = "report_date",
    case_count = "n", data_type = "count-incidence",
    strata = if (strata) "sex" else NULL, verbose = FALSE
  )
}

test_that("baselinenowcast produces draws on the object's own event dates", {
  skip_on_cran()
  skip_if_not_installed("baselinenowcast")

  x <- backend_tbl_now()
  set.seed(20260824)
  nowcast <- suppressWarnings(
    run_nowcast(x, engine_baselinenowcast(draws = 100), verbose = FALSE)
  )

  expect_true(is_tbl_nowcast(nowcast))
  expect_equal(nowcast@method, "baselinenowcast")
  expect_false(is.null(nowcast@draws))
  expect_equal(length(unique(nowcast@draws$.draw)), 100)

  # The predictions run on the object's own weekly grid and REACH the `now`.
  # They are not confined to the weeks that carry rows: a line list cannot
  # express a zero week, so the newest weeks may have none at all -- and those
  # are exactly the weeks a nowcast exists to estimate.
  dates <- sort(unique(nowcast@predictions$onset_week))
  expect_s3_class(dates, "Date")
  expect_gte(min(dates), min(x$onset_week))
  expect_equal(max(dates), get_now(x))
  expect_true(all(as.integer(diff(dates)) == 7L))

  # Scoring the object against the data it was fitted on must line up
  scores <- score_nowcast(nowcast)
  expect_gt(nrow(scores), 0)
  expect_false(any(is.na(scores$wis)))

  # ...and so must `tidy()`, which is the user's way into the same numbers
  tidied <- tidy(nowcast)
  expect_equal(unique(tidied$engine), "baselinenowcast")
  expect_equal(unique(tidied$level), 0.95)
  expect_equal(nrow(tidied), length(unique(nowcast@predictions$onset_week)))
})

test_that("baselinenowcast nowcasts each stratum separately", {
  skip_on_cran()
  skip_if_not_installed("baselinenowcast")

  x <- backend_tbl_now(strata = TRUE)
  set.seed(20260824)
  nowcast <- suppressWarnings(
    run_nowcast(x, engine_baselinenowcast(draws = 50), verbose = FALSE)
  )

  expect_equal(nowcast@strata, "gender")
  expect_setequal(unique(nowcast@predictions$gender), unique(x$gender))
  expect_true("gender" %in% colnames(nowcast@draws))

  # The strata survive into `tidy()` as labels, one block each
  expect_setequal(unique(tidy(nowcast)$stratum), as.character(unique(x$gender)))
})

test_that("two baselinenowcast fits can be ensembled", {
  skip_on_cran()
  skip_if_not_installed("baselinenowcast")

  x <- backend_tbl_now()
  set.seed(20260824)
  members <- lapply(1:2, function(i) {
    suppressWarnings(run_nowcast(x, engine_baselinenowcast(draws = 100), verbose = FALSE))
  })

  ensemble <- nowcast_ensemble(a = members[[1]], b = members[[2]], verbose = FALSE)
  pooled <- nowcast_ensemble(
    a = members[[1]], b = members[[2]],
    type = "linear_pool", n_draws = 200, verbose = FALSE
  )

  expect_true(is_tbl_nowcast(ensemble))
  expect_equal(nrow(ensemble@predictions), nrow(members[[1]]@predictions))
  expect_equal(length(unique(pooled@draws$.draw)), 200)
})

# NobBS -----------------------------------------------------------------------

test_that("NobBS is handed one row per CASE, not one row per count row", {
  skip_if_not_installed("NobBS")

  x <- counts_tbl_now()
  total_cases <- sum(x$n)
  expect_lt(nrow(x), total_cases) # the setup only means anything if this holds

  seen <- NULL
  local_mocked_bindings(
    NobBS = function(data, ...) {
      seen <<- data
      list(estimates = data.frame(
        onset_date = unique(data$onset_date), estimate = 1,
        q_0.5 = 1, lower = 0, upper = 2
      ))
    },
    .package = "NobBS"
  )

  run_nowcast(x, engine_nobbs(), verbose = FALSE)

  # THE failure this converter exists to prevent: 30 rows carrying 1,800 cases
  # nowcast as 30 cases, with no error and an answer 60 times too small.
  expect_equal(nrow(seen), total_cases)
  expect_true(all(c("onset_date", "report_date") %in% colnames(seen)))
  expect_s3_class(seen$onset_date, "Date")
})

test_that("NobBS is told the quantile levels that were asked for", {
  skip_if_not_installed("NobBS")

  x <- counts_tbl_now()
  seen <- NULL
  local_mocked_bindings(
    NobBS = function(data, ..., specs) {
      seen <<- specs
      list(estimates = data.frame(
        onset_date = unique(data$onset_date), estimate = 1, q_0.5 = 1
      ))
    },
    .package = "NobBS"
  )

  # NobBS reports whichever quantiles it is given at FIT time, so the levels have
  # to reach `specs` -- they cannot be recovered afterwards.
  run_nowcast(x, engine_nobbs(quantile_levels = c(0.1, 0.5, 0.9)), verbose = FALSE)
  expect_equal(seen$quantiles, c(0.1, 0.5, 0.9))

  # An explicit `specs$quantiles` still wins
  run_nowcast(
    x,
    engine_nobbs(
      specs = list(quantiles = c(0.25, 0.75)),
      quantile_levels = c(0.1, 0.5, 0.9)
    ),
    verbose = FALSE
  )
  expect_equal(seen$quantiles, c(0.25, 0.75))
})

test_that("a single stratum routes NobBS through NobBS.strat()", {
  skip_if_not_installed("NobBS")

  x <- counts_tbl_now(strata = TRUE)
  seen <- NULL
  local_mocked_bindings(
    NobBS.strat = function(data, ..., strata) {
      seen <<- list(data = data, strata = strata)
      dates <- unique(data$onset_date)
      list(estimates = data.frame(
        onset_date = rep(dates, 2),
        stratum = rep(c("F", "M"), each = length(dates)),
        estimate = 1, q_0.5 = 1
      ))
    },
    .package = "NobBS"
  )

  nowcast <- run_nowcast(x, engine_nobbs(), verbose = FALSE)

  # `tbl_now_to_nobbs()` builds the single column `NobBS.strat()` takes, and the
  # engine hands over that column whether there is one declared stratum or six.
  # Passing "sex" directly (as this did before 0.22.0) worked only in the
  # one-stratum case, and left the engine carrying its own copy of the pasting
  # logic for every other case.
  expect_equal(seen$strata, "strata")
  expect_equal(seen$data$strata, as.character(seen$data$sex))
  expect_equal(nrow(seen$data), sum(x$n))
  # NobBS.strat() calls the column `stratum`; the nowcast must report it under
  # the name the `tbl_now` declared, or the strata cannot be joined to anything.
  expect_equal(nowcast@strata, "sex")
  expect_true("sex" %in% colnames(nowcast@predictions))
  expect_setequal(unique(nowcast@predictions$sex), c("F", "M"))
})

test_that("NobBS refuses a grid it cannot model", {
  skip_if_not_installed("NobBS")

  monthly <- tbl_now(
    data.frame(
      event_date = as.Date(c("2020-01-01", "2020-02-01")),
      report_date = as.Date(c("2020-02-01", "2020-03-01"))
    ),
    event_date = "event_date", report_date = "report_date",
    event_units = "months", report_units = "months",
    data_type = "linelist", verbose = FALSE
  )

  expect_error(run_nowcast(monthly, engine_nobbs(), verbose = FALSE), "event units")
})

# surveillance ----------------------------------------------------------------

test_that("surveillance gets a line list and a grid that reaches `now`", {
  skip_if_not_installed("surveillance")

  x <- counts_tbl_now()
  seen <- NULL
  local_mocked_bindings(
    nowcast = function(now, when, data, ..., D, control) {
      seen <<- list(now = now, when = when, data = data, D = D, control = control)
      structure(list(), class = "stsNC")
    },
    .package = "surveillance"
  )

  nowcast_fit(engine_surveillance(), x, verbose = FALSE)

  expect_equal(nrow(seen$data), sum(x$n))
  expect_true(all(c("dHospital", "dReport") %in% colnames(seen$data)))
  expect_equal(seen$now, get_now(x))
  # A line list cannot express a zero week -- no rows -- so `dRange` has to be
  # handed over explicitly or `surveillance` stops at the last week that
  # happened to carry a report.
  expect_equal(max(seen$control$dRange), get_now(x))
  expect_equal(seen$D, as.integer(ceiling(max(x$.delay))))
  expect_length(seen$when, seen$D + 1L)
})

test_that("surveillance is fitted once per stratum", {
  skip_if_not_installed("surveillance")

  x <- counts_tbl_now(strata = TRUE)
  seen <- list()
  local_mocked_bindings(
    nowcast = function(now, when, data, ...) {
      seen[[length(seen) + 1L]] <<- data
      structure(list(), class = "stsNC")
    },
    .package = "surveillance"
  )

  fit <- nowcast_fit(engine_surveillance(), x, verbose = FALSE)

  # `surveillance::nowcast()` models ONE series and has no strata argument, so
  # two strata must mean two fits, each carrying only its own cases.
  expect_length(seen, 2)
  expect_s3_class(fit, "surveillance_strata")
  expect_named(fit, c("F", "M"))
  expect_equal(sum(vapply(seen, nrow, integer(1))), sum(x$n))
})

test_that("surveillance's predictions come from tidy(), keeping its own width", {
  skip_if_not_installed("surveillance")

  x <- counts_tbl_now(strata = TRUE)
  dates <- sort(unique(x$event_date))

  # `tidy.stsNC()` reads the interval width off the fit's `pi` slot; mocking
  # `tidy()` here checks that the backend USES that answer rather than
  # re-deriving one, which is where the audited bugs came from.
  local_mocked_bindings(
    tidy = function(x, ...) {
      dplyr::tibble(
        event_date = dates, stratum = "all",
        estimate = 100, conf.low = 60, conf.high = 150,
        level = 0.8, engine = "surveillance"
      )
    },
    .package = "tbl.now"
  )

  fit <- structure(
    list(F = structure(list(), class = "stsNC"), M = structure(list(), class = "stsNC")),
    class = "surveillance_strata"
  )
  tidied <- suppressWarnings(
    nowcast_tidy(engine_surveillance(), fit, x,
      quantile_levels = nowcast_quantile_levels()
    )
  )

  # An 80% interval means the 0.1 and 0.9 quantiles -- not the 0.025/0.975 the
  # default levels ask for, which this engine simply does not have.
  expect_setequal(unique(tidied$predictions$.quantile_level), c(0.1, 0.5, 0.9))
  expect_null(tidied$draws)
  expect_setequal(unique(tidied$predictions$sex), c("F", "M"))
})

test_that("a backend that reports one interval says what it cannot report", {
  skip_if_not_installed("surveillance")

  x <- counts_tbl_now()
  tidied <- dplyr::tibble(
    event_date = sort(unique(x$event_date)), stratum = "all",
    estimate = 10, conf.low = 5, conf.high = 20, level = 0.95,
    engine = "surveillance"
  )

  expect_warning(
    result <- .tidy_to_predictions(
      tidied, x, nowcast_quantile_levels(), "surveillance"
    ),
    "cannot report"
  )
  expect_setequal(
    unique(result$predictions$.quantile_level), c(0.025, 0.5, 0.975)
  )

  # Asking only for what it has is silent
  expect_silent(
    .tidy_to_predictions(tidied, x, c(0.025, 0.5, 0.975), "surveillance")
  )

  # No interval at all: the median survives, nothing is invented around it
  no_interval <- dplyr::mutate(
    tidied, conf.low = NA_real_, conf.high = NA_real_, level = NA_real_
  )
  bare <- suppressWarnings(
    .tidy_to_predictions(no_interval, x, nowcast_quantile_levels(), "surveillance")
  )
  expect_equal(unique(bare$predictions$.quantile_level), 0.5)
})

# EpiNow2 ---------------------------------------------------------------------

test_that("EpiNow2 uses estimate_infections(), and regional_epinow() for strata", {
  skip_if_not_installed("EpiNow2")

  x <- counts_tbl_now()
  seen <- NULL
  local_mocked_bindings(
    estimate_infections = function(data, ...) {
      seen <<- data
      structure(list(), class = "estimate_infections")
    },
    .package = "EpiNow2"
  )

  fit <- nowcast_fit(engine_epinow2(), x, verbose = FALSE)

  expect_s3_class(fit, "estimate_infections")
  # `tbl_now_to_EpiNow2()`'s `estimate_infections` target: a date/confirm series
  # laid on EpiNow2's daily grid, because it has no `timestep` argument.
  expect_true(all(c("date", "confirm") %in% colnames(seen)))

  regional_seen <- NULL
  local_mocked_bindings(
    regional_epinow = function(data, ...) {
      regional_seen <<- data
      list(regional = list())
    },
    .package = "EpiNow2"
  )

  stratified <- nowcast_fit(
    engine_epinow2(), counts_tbl_now(strata = TRUE), verbose = FALSE
  )

  expect_true("region" %in% colnames(regional_seen))
  expect_setequal(unique(regional_seen$region), c("F", "M"))
  expect_named(stratified, "regional")
})

test_that("EpiNow2 engine maps supported report temporal effects to obs_opts", {
  skip_if_not_installed("EpiNow2")

  x <- counts_tbl_now() |>
    add_temporal_effects(
      temporal_effects(day_of_week = TRUE), date_type = "report_date"
    )
  seen_args <- NULL
  local_mocked_bindings(
    estimate_infections = function(data, ...) {
      seen_args <<- list(...)
      structure(list(), class = "estimate_infections")
    },
    .package = "EpiNow2"
  )

  suppressWarnings(nowcast_fit(engine_epinow2(), x, verbose = FALSE))

  expect_s3_class(seen_args$obs, "obs_opts")
  expect_true(seen_args$obs$week_effect)
})

test_that("EpiNow2 engine does not overwrite an explicit obs option", {
  skip_if_not_installed("EpiNow2")

  x <- counts_tbl_now() |>
    add_temporal_effects(
      temporal_effects(day_of_week = TRUE), date_type = "report_date"
    )
  explicit_obs <- EpiNow2::obs_opts(week_effect = FALSE)
  seen_args <- NULL
  local_mocked_bindings(
    estimate_infections = function(data, ...) {
      seen_args <<- list(...)
      structure(list(), class = "estimate_infections")
    },
    .package = "EpiNow2"
  )

  suppressWarnings(
    nowcast_fit(engine_epinow2(obs = explicit_obs), x, verbose = FALSE)
  )

  expect_identical(seen_args$obs, explicit_obs)
})

test_that("EpiNow2 engine warns for unavailable temporal effects", {
  skip_if_not_installed("EpiNow2")

  x <- counts_tbl_now() |>
    add_temporal_effects(temporal_effects(week_of_year = TRUE))
  local_mocked_bindings(
    estimate_infections = function(data, ...) {
      structure(list(), class = "estimate_infections")
    },
    .package = "EpiNow2"
  )

  warnings <- character()
  withCallingHandlers(
    suppressMessages(nowcast_fit(engine_epinow2(), x, verbose = FALSE)),
    warning = function(cnd) {
      warnings <<- c(warnings, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("Unavailable", warnings)))
  expect_true(any(grepl("temporal effects", warnings)))
})

test_that("EpiNow2's predictions come from tidy(), keeping its own width", {
  skip_if_not_installed("EpiNow2")

  x <- counts_tbl_now()
  dates <- sort(unique(x$event_date))

  # EpiNow2's `CrIs` is a user argument, so a default fit reports a 90% band and
  # one built with `CrIs = 0.95` reports 95%. `tidy()` reads which; the backend
  # must not overrule it.
  local_mocked_bindings(
    tidy = function(x, ...) {
      dplyr::tibble(
        event_date = dates, stratum = "all",
        estimate = 42, conf.low = 30, conf.high = 55,
        level = 0.9, engine = "EpiNow2"
      )
    },
    .package = "tbl.now"
  )

  tidied <- suppressWarnings(
    nowcast_tidy(
      engine_epinow2(), structure(list(), class = "estimate_infections"),
      x, quantile_levels = nowcast_quantile_levels()
    )
  )

  expect_setequal(unique(tidied$predictions$.quantile_level), c(0.05, 0.5, 0.95))
  expect_equal(
    tidied$predictions$.value[tidied$predictions$.quantile_level == 0.05],
    rep(30, length(dates))
  )
})

# The catalogue ---------------------------------------------------------------

test_that("every built-in method has both extension methods registered", {
  builtin <- c(
    "diseasenowcasting", "baselinenowcast", "epinowcast", "surveillance",
    "EpiNow2", "NobBS"
  )
  registered_fit <- sub("^nowcast_fit\\.", "", as.character(utils::methods("nowcast_fit")))
  registered_tidy <- sub("^nowcast_tidy\\.", "", as.character(utils::methods("nowcast_tidy")))

  expect_true(all(builtin %in% registered_fit))
  expect_true(all(builtin %in% registered_tidy))

  # `nowcaster`'s converters went in 0.16.0 and its backend in 0.18.0. A leftover
  # backend would abort at fit time on `tbl_now_to_nowcaster()` and
  # `get_nowcaster_strata()`, which no longer exist.
  expect_false("nowcaster" %in% registered_fit)
  expect_false("nowcaster" %in% registered_tidy)
})

test_that("built-in method names are matched case-insensitively", {
  expect_equal(engine("epinow2")$name, "EpiNow2")
  expect_equal(engine("EPINOW2")$name, "EpiNow2")
  expect_equal(engine("Surveillance")$name, "surveillance")
})

# A nowcast estimates what has already happened -----------------------------
#
# `EpiNow2::estimate_infections()` also forecasts: it returns one period past the
# end of the data, so a weekly object comes back with an extra week after `now`.
# Left in, it reaches `nowcast_ensemble()` as a target no other member predicted.

test_that("predictions after `now` are dropped, with a message naming them", {
  x <- counts_tbl_now()
  now <- get_now(x)
  event_col <- get_event_date(x)

  # A tidied frame in the shape every engine hands to `.tidy_to_predictions()`,
  # carrying one row past `now`.
  tidied <- dplyr::tibble(
    event_date = c(now - 7, now, now + 7),
    stratum    = "all",
    estimate   = c(10, 20, 30),
    conf.low   = c(5, 15, 25),
    conf.high  = c(15, 25, 35),
    level      = 0.95,
    engine     = "EpiNow2"
  )

  # The fixture reports one interval, so the "cannot report levels" warning is
  # expected noise here; the message about `now` is what is under test.
  expect_message(
    out <- suppressWarnings(tbl.now:::.tidy_to_predictions(
      tidied, x, nowcast_quantile_levels(), "EpiNow2"
    )),
    "after"
  )
  expect_equal(max(out$predictions[[event_col]]), now)
  expect_false((now + 7) %in% out$predictions[[event_col]])
  # Everything at or before `now` survives untouched.
  expect_setequal(unique(out$predictions[[event_col]]), c(now - 7, now))
})

test_that("an engine that stops at `now` is left alone and stays silent", {
  x <- counts_tbl_now()
  now <- get_now(x)

  tidied <- dplyr::tibble(
    event_date = c(now - 7, now), stratum = "all",
    estimate = c(10, 20), conf.low = c(5, 15), conf.high = c(15, 25),
    level = 0.95, engine = "baselinenowcast"
  )

  expect_no_message(
    out <- suppressWarnings(tbl.now:::.tidy_to_predictions(
      tidied, x, nowcast_quantile_levels(), "baselinenowcast"
    ))
  )
  expect_equal(nrow(dplyr::distinct(out$predictions[get_event_date(x)])), 2L)
})
