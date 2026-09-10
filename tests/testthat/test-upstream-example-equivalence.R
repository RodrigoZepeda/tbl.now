# Equivalence against the modelling packages' own shipped examples.
#
# These are deliberately integration tests.  The converter assertion is the
# first half of every example; the second half runs the package entry point by
# hand and through `run_nowcast()` with the same arguments and RNG state.  Stan
# objects contain run-specific paths and timings, so their posterior samples --
# the stochastic result -- are compared exactly rather than their wrappers.

skip_on_cran()

quietly <- function(expr) suppressWarnings(suppressMessages(force(expr)))

skip_without_cmdstan <- function() {
  skip_if_not_installed("cmdstanr")
  version <- tryCatch(
    cmdstanr::cmdstan_version(error_on_NA = FALSE),
    error = function(e) NULL
  )
  skip_if(is.null(version), "CmdStan is not installed")
}

epinowcast_example_data <- function() {
  data <- data.table::copy(epinowcast::germany_covid19_hosp)
  data <- data[location == "DE"][age_group == "00+"]
  data <- epinowcast::enw_filter_report_dates(
    data, latest_date = "2021-10-01"
  )
  data <- epinowcast::enw_complete_dates(
    data, by = c("location", "age_group")
  )
  data <- epinowcast::enw_filter_report_dates(data, remove_days = 40)
  data <- epinowcast::enw_filter_reference_dates(data, include_days = 40)

  observations <- data[, .(reference_date, report_date, confirm)]
  list(
    raw = data,
    observations = observations,
    tbl = suppressWarnings(tbl_now(
      observations,
      event_date = "reference_date", report_date = "report_date",
      case_count = "confirm", data_type = "count-cumulative",
      units = "days", verbose = FALSE
    ))
  )
}

test_that("epinowcast's Germany preprocessing is reproduced exactly", {
  skip_if_not_installed("epinowcast")
  skip_if_not_installed("data.table")
  example <- epinowcast_example_data()

  expect_gt(sum(is.na(example$tbl$reference_date)), 0L)
  native <- epinowcast::enw_preprocess_data(
    example$observations, max_delay = 20
  )
  converted <- quietly(tbl_now_to_epinowcast(
    example$tbl, max_delay = 20, verbose = FALSE, quiet = TRUE
  ))

  # In particular, `max_confirm` must still use observations after delay 20.
  # Completing with the model cap before preprocessing used to change it.
  expect_identical(converted, native)
})

test_that("epinowcast's Germany fit matches run_nowcast()", {
  skip_if_not_installed("epinowcast")
  skip_if_not_installed("data.table")
  skip_without_cmdstan()
  example <- epinowcast_example_data()
  native_data <- epinowcast::enw_preprocess_data(
    example$observations, max_delay = 20
  )
  priors <- data.frame(variable = "refp_mean_int", mean = 2, sd = 0.5)
  fit_options <- epinowcast::enw_fit_opts(
    save_warmup = FALSE, pp = TRUE,
    chains = 1, iter_warmup = 100, iter_sampling = 100,
    seed = 20260910, refresh = 0, show_messages = FALSE
  )

  set.seed(20260910)
  native <- quietly(epinowcast::epinowcast(
    native_data, priors = priors, fit = fit_options
  ))
  set.seed(20260910)
  wrapped <- quietly(run_nowcast(
    example$tbl,
    engine_epinowcast(
      preprocess_args = list(max_delay = 20),
      priors = priors, fit = fit_options
    ),
    verbose = FALSE
  ))

  expect_s3_class(wrapped@fit, "epinowcast")
  expect_identical(
    summary(wrapped@fit, type = "nowcast_samples"),
    summary(native, type = "nowcast_samples")
  )
})

test_that("NobBS's dengue line list is reproduced exactly", {
  skip_if_not_installed("NobBS")
  data <- NobBS::denguedat
  now <- as.Date("1990-04-09")
  x <- suppressWarnings(tbl_now(
    data,
    event_date = "onset_week", report_date = "report_week",
    data_type = "linelist", units = "weeks", now = now,
    verbose = FALSE
  ))

  expected <- data.frame(
    onset_date = data$onset_week,
    report_date = data$report_week
  )
  expect_identical(tbl_now_to_nobbs(x, verbose = FALSE), expected)
})

test_that("NobBS's dengue fit matches run_nowcast()", {
  skip_if_not_installed("NobBS")
  data <- NobBS::denguedat
  now <- as.Date("1990-04-09")
  x <- suppressWarnings(tbl_now(
    data,
    event_date = "onset_week", report_date = "report_week",
    data_type = "linelist", units = "weeks", now = now,
    verbose = FALSE
  ))
  converted <- tbl_now_to_nobbs(x, verbose = FALSE)
  specs <- list(
    quantiles = nowcast_quantile_levels(),
    nAdapt = 100, nBurnin = 100, nSamp = 200
  )

  native <- quietly(NobBS::NobBS(
    converted, now = now, units = "1 week",
    onset_date = "onset_date", report_date = "report_date",
    specs = specs
  ))
  wrapped <- quietly(run_nowcast(
    x, engine_nobbs(specs = specs), verbose = FALSE
  ))

  # NobBS fixes its JAGS RNG internally, so the complete native result is exact.
  expect_identical(wrapped@fit, native)
})

surveillance_example_data <- function() {
  environment <- new.env(parent = emptyenv())
  utils::data("husO104Hosp", package = "surveillance", envir = environment)
  data <- environment$husO104Hosp
  now <- as.Date("2011-06-10")
  list(
    raw = data,
    now = now,
    tbl = suppressWarnings(tbl_now(
      data,
      event_date = "dHosp", report_date = "dReport",
      data_type = "linelist", units = "days", now = now,
      verbose = FALSE
    ))
  )
}

test_that("surveillance's HUS line list is reproduced exactly", {
  skip_if_not_installed("surveillance")
  example <- surveillance_example_data()
  expected <- example$raw
  names(expected) <- c("dHospital", "dReport")

  expect_identical(
    tbl_now_to_surveillance(example$tbl, verbose = FALSE), expected
  )
})

test_that("surveillance's HUS fit matches run_nowcast()", {
  skip_if_not_installed("surveillance")
  example <- surveillance_example_data()
  when <- seq(example$now - 3, length.out = 10, by = "-1 day")
  control <- list(
    dRange = get_surveillance_range(example$tbl),
    N.tInf.max = 300L,
    N.tInf.prior = structure(
      "poisgamma", mean.lambda = 50, var.lambda = 3000
    ),
    nSamples = 50
  )

  set.seed(20260910)
  native <- quietly(surveillance::nowcast(
    now = example$now, when = when,
    data = tbl_now_to_surveillance(example$tbl, verbose = FALSE),
    dEventCol = "dHospital", dReportCol = "dReport",
    aggregate.by = "1 day", D = 15, method = "bayes.trunc",
    control = control
  ))
  set.seed(20260910)
  wrapped <- quietly(run_nowcast(
    example$tbl,
    engine_surveillance(
      when = when, D = 15, fit_method = "bayes.trunc", control = control
    ),
    verbose = FALSE
  ))

  expect_equal(wrapped@fit, native)
  expect_identical(
    surveillance::reportingTriangle(wrapped@fit),
    surveillance::reportingTriangle(native)
  )
})

epinow2_example_data <- function() {
  data <- as.data.frame(EpiNow2::example_confirmed[1:40])
  input <- data
  input$report_date <- input$date
  list(
    raw = data,
    tbl = tbl_now(
      input,
      event_date = "date", report_date = "report_date",
      case_count = "confirm", data_type = "count-incidence",
      units = "days", now = max(input$date), verbose = FALSE
    )
  )
}

test_that("EpiNow2's example series is reproduced exactly", {
  skip_if_not_installed("EpiNow2")
  example <- epinow2_example_data()
  converted <- quietly(tbl_now_to_EpiNow2(
    example$tbl, verbose = FALSE, quiet = TRUE
  ))

  expect_identical(converted, example$raw)
})

test_that("EpiNow2's example fit matches run_nowcast()", {
  skip_if_not_installed("EpiNow2")
  example <- epinow2_example_data()
  series <- quietly(tbl_now_to_EpiNow2(
    example$tbl, verbose = FALSE, quiet = TRUE
  ))
  generation_time <- EpiNow2::gt_opts(EpiNow2::example_generation_time)
  delays <- EpiNow2::delay_opts(
    EpiNow2::example_incubation_period + EpiNow2::example_reporting_delay
  )
  rt <- EpiNow2::rt_opts(
    prior = EpiNow2::LogNormal(mean = 2, sd = 0.1)
  )
  stan <- EpiNow2::stan_opts(
    samples = 25, warmup = 50, chains = 1, seed = 20260910
  )

  set.seed(20260910)
  native <- quietly(EpiNow2::estimate_infections(
    series,
    generation_time = generation_time, delays = delays, rt = rt, stan = stan
  ))
  set.seed(20260910)
  wrapped <- quietly(run_nowcast(
    example$tbl,
    engine_epinow2(
      generation_time = generation_time, delays = delays, rt = rt, stan = stan
    ),
    verbose = FALSE
  ))

  # EpiNow2's wrapper retains run times, but a fixed R seed plus Stan seed makes
  # every posterior reported-case sample identical.
  expect_identical(
    quietly(EpiNow2::get_predictions(wrapped@fit, format = "sample")),
    quietly(EpiNow2::get_predictions(native, format = "sample"))
  )
})

epidist_example_data <- function() {
  data <- epidist::sierra_leone_ebola_data
  list(
    raw = data,
    tbl = tbl_now(
      data,
      event_date = "date_of_symptom_onset",
      report_date = "date_of_sample_tested",
      strata = c("id", "age", "sex", "district", "chiefdom"),
      data_type = "linelist", units = "days", verbose = FALSE
    )
  )
}

test_that("epidist's Ebola transformation is reproduced exactly", {
  skip_if_not_installed("epidist")
  example <- epidist_example_data()
  native <- quietly(epidist::as_epidist_linelist_data(
    example$raw,
    pdate_lwr = "date_of_symptom_onset",
    sdate_lwr = "date_of_sample_tested"
  ))
  converted <- quietly(tbl_now_to_epidist(
    example$tbl, verbose = FALSE, quiet = TRUE
  ))

  # The two constructors order passthrough columns differently; after restoring
  # the upstream order, the class, attributes, dates and data are byte-identical.
  expect_identical(converted[, names(native)], native)
  native_model <- epidist::as_epidist_marginal_model(native)
  converted_model <- epidist::as_epidist_marginal_model(converted)
  expect_identical(converted_model[, names(native_model)], native_model)
})

test_that("epidist's Ebola delay fit is unchanged after conversion", {
  skip_if_not_installed("epidist")
  skip_without_cmdstan()
  example <- epidist_example_data()
  # The vignette's full 8,358-row fit is unnecessarily expensive for a plumbing
  # assertion.  Keep its first 300 real observations and its exact pipeline.
  source <- example$raw[1:300, ]
  x <- tbl_now(
    source,
    event_date = "date_of_symptom_onset",
    report_date = "date_of_sample_tested",
    strata = c("id", "age", "sex", "district", "chiefdom"),
    data_type = "linelist", units = "days", verbose = FALSE
  )
  native_data <- quietly(epidist::as_epidist_linelist_data(
    source,
    pdate_lwr = "date_of_symptom_onset",
    sdate_lwr = "date_of_sample_tested"
  )) |>
    epidist::as_epidist_aggregate_data() |>
    epidist::as_epidist_marginal_model()
  converted_data <- quietly(tbl_now_to_epidist(
    x, verbose = FALSE, quiet = TRUE
  )) |>
    epidist::as_epidist_aggregate_data() |>
    epidist::as_epidist_marginal_model()

  set.seed(20260910)
  native <- quietly(epidist::epidist(
    native_data,
    chains = 1, cores = 1, iter = 60, warmup = 30,
    backend = "cmdstanr", seed = 20260910, refresh = 0
  ))
  set.seed(20260910)
  converted <- quietly(epidist::epidist(
    converted_data,
    chains = 1, cores = 1, iter = 60, warmup = 30,
    backend = "cmdstanr", seed = 20260910, refresh = 0
  ))

  expect_identical(as.data.frame(converted), as.data.frame(native))
})
