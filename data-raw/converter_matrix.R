# converter_matrix --------------------------------------------------------------
#
# Runs every `tbl_now_to_*()` converter, and one representative nowcast, against
# every dataset the package ships. The point is not to nowcast well -- it is to
# find out, honestly, which combinations WORK and which BREAK, and why.
#
# Run with:  source("data-raw/converter_matrix.R")
# Output:    vignettes/articles/converter-matrix.rds

library(dplyr)
devtools::load_all(".", quiet = TRUE)

TIME_LIMIT <- 180  # seconds allowed per conversion/fit

# -- build one tbl_now per dataset ----------------------------------------------
#
# Each is cut down so the matrix runs in minutes, not hours; the shapes (linelist
# vs count-incidence vs count-cumulative) are what the test is about.

build <- list(
  denguedat = function() {
    data(denguedat, envir = environment())
    denguedat |>
      filter(onset_week >= as.Date("2008-01-01")) |>
      tbl_now(event_date = onset_week, report_date = report_week,
              strata = gender, data_type = "linelist", verbose = FALSE)
  },
  hai_bucaramanga = function() {
    data(hai_bucaramanga, envir = environment())
    hai_bucaramanga |>
      distinct() |>
      filter(!is.na(specimen_date), !is.na(report_date),
             report_date >= specimen_date) |>
      mutate(ew = lubridate::floor_date(specimen_date, "week", week_start = 1),
             rw = lubridate::floor_date(report_date,  "week", week_start = 1)) |>
      tbl_now(event_date = ew, report_date = rw, strata = icu_type,
              data_type = "linelist", verbose = FALSE)
  },
  covid_colombia = function() {
    data(covid_colombia, envir = environment())
    covid_colombia |>
      filter(notification_date >= as.Date("2021-01-01")) |>
      tbl_now(event_date = notification_date, report_date = diagnosis_date,
              case_count = n, strata = sex, data_type = "count-incidence",
              verbose = FALSE)
  },
  covidat = function() {
    data(covidat, envir = environment())
    covidat |>
      # Some rows are registered BEFORE symptom onset. A negative delay is not
      # a delay, and epidist rejects it outright, so drop them first.
      filter(date_of_registry >= date_of_symptom_onset) |>
      filter(date_of_symptom_onset >= as.Date("2021-01-01")) |>
      tbl_now(event_date = date_of_symptom_onset, report_date = date_of_registry,
              case_count = n, strata = sex, data_type = "count-incidence",
              verbose = FALSE)
  },
  covid_us = function() {
    data(covid_us, envir = environment())
    covid_us |>
      filter(cdc_case_earliest_dt >= as.Date("2021-06-01")) |>
      tbl_now(event_date = cdc_case_earliest_dt, report_date = cdc_report_dt,
              case_count = n, data_type = "count-incidence", verbose = FALSE)
  },
  mpoxdat = function() {
    data(mpoxdat, envir = environment())
    # `race` MUST be declared. Left as an undeclared extra column the
    # (event, report) pair is not unique, and every target that needs a unique
    # cell -- a reporting triangle, a tsibble index/key -- rejects it.
    mpoxdat |>
      tbl_now(event_date = dx_date, report_date = dx_report_date,
              case_count = n, strata = race, data_type = "count-incidence",
              verbose = FALSE)
  },
  flusight = function() {
    data(flusight, envir = environment())
    flusight |>
      filter(location_name == "California", !is.na(observation),
             target_end_date >= as.Date("2024-01-01")) |>
      # `as_of` lands on Saturdays AND Wednesdays while `target_end_date` is
      # always Saturday, so raw delays are fractional (0.571 weeks) and no
      # reporting triangle can be built. That is an ALIGNMENT problem, not a
      # cumulative-data one.
      tbl_now(event_date = target_end_date, report_date = as_of,
              case_count = observation, data_type = "count-cumulative",
              align_weeks = TRUE, verbose = FALSE)
  }
)

# -- the things we try on each ---------------------------------------------------

attempts <- list(
  "to_count(incidence)"  = function(x) to_count(x, to = "count-incidence"),
  "baselinenowcast"      = function(x) tbl_now_to_baselinenowcast(x, verbose = FALSE),
  "epinowcast"           = function(x) tbl_now_to_epinowcast(x, verbose = FALSE, quiet = TRUE),
  "epidist"              = function(x) tbl_now_to_epidist(x, verbose = FALSE),
  "surveillance"         = function(x) tbl_now_to_surveillance(x, verbose = FALSE),
  "NobBS"                = function(x) tbl_now_to_nobbs(x, verbose = FALSE),
  "data.table"           = function(x) tbl_now_to_data_table(x, verbose = FALSE),
  "tsibble"              = function(x) tbl_now_to_tsibble(x, verbose = FALSE),
  # EpiNow2 has four input shapes; the two that stress the converter are the
  # daily-grid expansion (estimate_infections) and the report dimension
  # (estimate_truncation). estimate_dist shares its schema with epidist.
  "EpiNow2 (infections)" = function(x) {
    tbl_now_to_EpiNow2(x, verbose = FALSE, quiet = TRUE)
  },
  "EpiNow2 (truncation)" = function(x) {
    tbl_now_to_EpiNow2(x, target = "estimate_truncation",
                       verbose = FALSE, quiet = TRUE)
  },
  "EpiNow2 (dist)"       = function(x) {
    tbl_now_to_EpiNow2(x, target = "estimate_dist", verbose = FALSE, quiet = TRUE)
  },
  # A FIXED seed: `nowcast()` defaults to `seed = sample.int(...)`, so without
  # this the recorded outcome is not reproducible. (The flusight failure below is
  # deterministic and not seed-related -- see devel/diseasenowcasting-issue.md --
  # but the matrix should not depend on that being true.)
  "nowcast (dnc)"        = function(x) diseasenowcasting::nowcast(x, seed = 20260824L)
)

# -- run --------------------------------------------------------------------------

rows <- list()
for (dataset in names(build)) {
  message("== ", dataset, " ==")
  obj <- tryCatch(build[[dataset]](), error = function(e) e)
  if (inherits(obj, "error")) {
    rows[[length(rows) + 1L]] <- tibble(
      dataset = dataset, data_type = NA_character_, step = "tbl_now()",
      status = "error", message = conditionMessage(obj)
    )
    next
  }
  dtype <- get_data_type(obj)
  for (what in names(attempts)) {
    started <- Sys.time()
    # Cap each attempt. Without this the matrix is not reproducible: fitting
    # `diseasenowcasting` to the cumulative FluSight series ran for over 25
    # minutes without finishing, which is a result worth recording rather than
    # something to wait out.
    #
    # NB the cap does not actually bite on that cell: `setTimeLimit()` cannot
    # interrupt Stan's compiled code, so the 180s limit was overrun to 2163s. The
    # limit still protects the R-level steps.
    res <- tryCatch(
      {
        setTimeLimit(elapsed = TIME_LIMIT, transient = TRUE)
        on.exit(setTimeLimit(elapsed = Inf, transient = TRUE), add = TRUE)
        suppressWarnings(suppressMessages(attempts[[what]](obj)))
        list(status = "ok", message = NA_character_)
      },
      error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("reached elapsed time limit|reached CPU time limit", msg)) {
          list(status = "timeout",
               message = paste0("did not finish within ", TIME_LIMIT, "s"))
        } else {
          list(status = "error", message = msg)
        }
      }
    )
    setTimeLimit(elapsed = Inf, transient = TRUE)
    secs <- round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1)
    message(sprintf("  %-22s %s (%ss)", what, res$status, secs))
    rows[[length(rows) + 1L]] <- tibble(
      dataset = dataset, data_type = dtype, step = what,
      status = res$status,
      message = if (is.na(res$message)) NA_character_ else
        gsub("\\s+", " ", substr(res$message, 1, 300)),
      seconds = secs
    )
  }
}

matrix_results <- bind_rows(rows)
saveRDS(matrix_results, "vignettes/articles/converter-matrix.rds")

message("\n=== summary ===")
print(as.data.frame(
  matrix_results |> count(step, status) |>
    tidyr::pivot_wider(names_from = status, values_from = n, values_fill = 0L)
))
message("\n=== failures and timeouts ===")
print(as.data.frame(matrix_results |> filter(status != "ok") |>
  select(dataset, data_type, step, status, message)))
