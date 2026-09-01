# The shipped datasets, each shaped into a `tbl_now` once.
#
# Lives in a helper rather than in one test file because two files need it:
# `test-converter-datasets.R` runs every CONVERTER against these shapes, and
# `test-engines-datasets.R` runs every ENGINE against them. Those are different
# questions -- a converter can produce a perfectly good reporting triangle that
# the engine behind it then refuses -- and that gap is what let the `flusight`
# failure reach a user.

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
    covid_us = {
      data(covid_us, envir = environment())
      # `sex` MUST be declared: undeclared, the (event, report) pair is not
      # unique and every target that needs one slot per cell rejects it.
      covid_us |>
        dplyr::filter(onset_dt >= as.Date("2020-10-01")) |>
        dplyr::summarise(
          n = sum(.data$n),
          .by = c("onset_dt", "pos_spec_dt", "sex")
        ) |>
        tbl_now(event_date = onset_dt, report_date = pos_spec_dt,
                case_count = n, strata = sex, data_type = "count-incidence",
                verbose = FALSE)
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
    },
    # FluSight reports `as_of` on Saturdays AND Wednesdays while
    # `target_end_date` is always a Saturday, so the raw delays are fractional
    # (0.571 weeks) and no reporting triangle can be built from them at all.
    # That is an alignment problem, not a cumulative-data one, and it is why
    # anything that has to REACH an engine uses this variant rather than the one
    # above.
    flusight_aligned = {
      data(flusight, envir = environment())
      flusight |>
        dplyr::filter(location_name == "California", !is.na(observation),
                      target_end_date >= as.Date("2024-01-01")) |>
        tbl_now(event_date = target_end_date, report_date = as_of,
                case_count = observation, data_type = "count-cumulative",
                align_weeks = TRUE, verbose = FALSE)
    }
  )
}

