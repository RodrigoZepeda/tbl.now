# Regression guard: every converter, against every dataset the package ships.
#
# This is the testthat counterpart of `data-raw/converter_matrix.R`. The matrix
# in the article is documentation; this is the thing that FAILS if a converter
# change breaks a dataset shape. The expectations record what works TODAY, so a
# regression shows up as a test failure rather than as a surprised user.

dataset_tbl_now <- function(name) {
  switch(name,
    denguedat = {
      data(denguedat, envir = environment())
      denguedat |>
        dplyr::filter(onset_week >= as.Date("2009-01-01")) |>
        tbl_now(event_date = onset_week, report_date = report_week,
                strata = gender, data_type = "linelist", verbose = FALSE)
    },
    hai_bucaramanga = {
      data(hai_bucaramanga, envir = environment())
      hai_bucaramanga |>
        dplyr::distinct() |>
        dplyr::filter(!is.na(specimen_date), !is.na(report_date),
                      report_date >= specimen_date) |>
        dplyr::mutate(
          ew = lubridate::floor_date(specimen_date, "week", week_start = 1),
          rw = lubridate::floor_date(report_date, "week", week_start = 1)
        ) |>
        tbl_now(event_date = ew, report_date = rw, strata = icu_type,
                data_type = "linelist", verbose = FALSE)
    },
    covid_colombia = {
      data(covid_colombia, envir = environment())
      covid_colombia |>
        dplyr::filter(notification_date >= as.Date("2021-01-01")) |>
        tbl_now(event_date = notification_date, report_date = diagnosis_date,
                case_count = n, strata = sex, data_type = "count-incidence",
                verbose = FALSE)
    },
    covidat = {
      data(covidat, envir = environment())
      covidat |>
        # Some rows are registered BEFORE onset; a negative delay is not a delay.
        dplyr::filter(date_of_registry >= date_of_symptom_onset) |>
        dplyr::filter(date_of_symptom_onset >= as.Date("2021-01-01")) |>
        tbl_now(event_date = date_of_symptom_onset, report_date = date_of_registry,
                case_count = n, strata = sex, data_type = "count-incidence",
                verbose = FALSE)
    },
    covid_us = {
      data(covid_us, envir = environment())
      covid_us |>
        dplyr::filter(cdc_case_earliest_dt >= as.Date("2021-06-01")) |>
        tbl_now(event_date = cdc_case_earliest_dt, report_date = cdc_report_dt,
                case_count = n, data_type = "count-incidence", verbose = FALSE)
    },
    mpoxdat = {
      data(mpoxdat, envir = environment())
      # `race` MUST be declared: undeclared, (event, report) is not unique and
      # every target needing a unique cell rejects it.
      mpoxdat |>
        tbl_now(event_date = dx_date, report_date = dx_report_date,
                case_count = n, strata = race, data_type = "count-incidence",
                verbose = FALSE)
    },
    flusight = {
      data(flusight, envir = environment())
      flusight |>
        dplyr::filter(location_name == "California", !is.na(observation),
                      target_end_date >= as.Date("2024-01-01")) |>
        tbl_now(event_date = target_end_date, report_date = as_of,
                case_count = observation, data_type = "count-cumulative",
                verbose = FALSE)
    }
  )
}

CONVERTERS <- list(
  baselinenowcast = function(x) tbl_now_to_baselinenowcast(x, verbose = FALSE),
  epinowcast      = function(x) tbl_now_to_epinowcast(x, verbose = FALSE, quiet = TRUE),
  epidist         = function(x) tbl_now_to_epidist(x, verbose = FALSE),
  surveillance    = function(x) tbl_now_to_surveillance(x, verbose = FALSE),
  data.table      = function(x) tbl_now_to_data_table(x, verbose = FALSE),
  tsibble         = function(x) tbl_now_to_tsibble(x, verbose = FALSE)
)

# Datasets whose shape every converter handles.
WORKING_DATASETS <- c(
  "denguedat", "hai_bucaramanga", "covid_colombia",
  "covidat", "covid_us", "mpoxdat"
)

for (dataset in WORKING_DATASETS) {
  local({
    this_dataset <- dataset
    test_that(paste("every converter handles", this_dataset), {
      skip_on_cran()
      x <- dataset_tbl_now(this_dataset)
      expect_true(is_tbl_now(x))
      for (nm in names(CONVERTERS)) {
        skip_if_not_installed(nm)
        expect_no_error(
          suppressWarnings(suppressMessages(CONVERTERS[[nm]](x))),
          message = paste0(nm, " broke on ", this_dataset)
        )
      }
    })
  })
}

test_that("count-cumulative converts once the delays are whole periods", {
  skip_on_cran()
  skip_if_not_installed("baselinenowcast")

  # FluSight reports `as_of` on Saturdays AND Wednesdays while `target_end_date`
  # is always Saturday, so raw delays are fractional (0.571 weeks) and no
  # reporting triangle can be built. That is an alignment problem, NOT a
  # cumulative-data problem: `align_weeks = TRUE` fixes it.
  data(flusight, envir = environment())
  aligned <- flusight |>
    dplyr::filter(location_name == "California", !is.na(observation),
                  target_end_date >= as.Date("2024-01-01")) |>
    tbl_now(event_date = target_end_date, report_date = as_of,
            case_count = observation, data_type = "count-cumulative",
            align_weeks = TRUE, verbose = FALSE)
  expect_true(all(aligned$.delay == round(aligned$.delay)))

  triangle <- suppressWarnings(
    tbl_now_to_baselinenowcast(aligned, verbose = FALSE)
  )
  expect_s3_class(triangle, "reporting_triangle")

  # De-accumulating a cumulative series produces NEGATIVE increments wherever a
  # total was revised downward. They must be absorbed, not passed through.
  expect_equal(sum(triangle < 0, na.rm = TRUE), 0L)
})

test_that("`negatives = 'error'` refuses cumulative input outright", {
  skip_on_cran()
  skip_if_not_installed("baselinenowcast")
  flusight_now <- dataset_tbl_now("flusight")
  expect_equal(get_data_type(flusight_now), "count-cumulative")
  expect_error(
    suppressWarnings(
      tbl_now_to_baselinenowcast(flusight_now, negatives = "error", verbose = FALSE)
    ),
    "count-cumulative"
  )
})

test_that("count-cumulative converts for the other targets too", {
  skip_on_cran()
  flusight_now <- dataset_tbl_now("flusight")
  for (nm in c("epinowcast", "surveillance", "data.table", "tsibble")) {
    skip_if_not_installed(nm)
    expect_no_error(
      suppressWarnings(suppressMessages(CONVERTERS[[nm]](flusight_now))),
      message = paste0(nm, " broke on cumulative flusight")
    )
  }
})
