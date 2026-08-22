# nowcast_comparison -----------------------------------------------------------
#
# Fits every engine covered by vignettes/articles/nowcasting-models.Rmd and saves
# a tidy table of their nowcasts, the data each engine actually saw, and the
# counts those weeks eventually reached.
#
# Run with:  source("data-raw/nowcast_comparison.R")
# Output:    vignettes/articles/nowcast-comparison.rds
#
# IMPORTANT -- keep this file and the article in step. The calls below are the
# calls the article shows. The article displays them with `eval = FALSE` and
# reads the results from the saved file, because the Stan / JAGS / INLA fits are
# far too slow to run on every build. If you change a call here, change it there.
#
# WHY THIS WINDOW. `denguedat` runs to the end of 2010. Cutting BOTH dates at
# 2002-07-22 gives a `tbl_now` whose `now` is 2002-07-15 and whose recent weeks
# are genuinely incomplete -- a real nowcasting problem -- while the full
# `denguedat` still records what those weeks eventually reached. That is the
# "truth" line. Mid-year is deliberate: a cut at the turn of the year lands on
# the holiday reporting slump, which says more about December than about the
# models.
#
# The exact week matters, because of nowcaster. `nowcasting_inla()` takes its
# Tmax from the last **onset** in the data:
#
#     Tmax <- max(pull(data_w, date_onset))
#     Y <- ifelse(Time + delay > Tmax.id, NA, Y)
#
# so every cell whose report lands after the last onset week is masked as
# unobservable -- even when that report is right there in the data. If the final
# onset week has no same-week report, `max(onset) < max(report)` and nowcaster
# silently discards a whole diagonal of real reports, which is what made it look
# as though it nowcast below the observed counts.
#
# At 2002-07-22 the last onset week (2002-07-15) does contain a same-week report,
# so max(onset) == max(report) and nowcaster sees exactly what every other engine
# sees: 105 of 105 weeks match, and it never estimates below the observed count.
# At 2002-07-15 it lost one diagonal and 6 weeks came out low.
#
# TRIMMING. `diseasenowcasting` and `baselinenowcast` are given the WHOLE series
# (677 weeks); they cope. The others are trimmed through their own arguments,
# never by subsetting the data first: `moving_window` (NobBS), `wdw` (nowcaster),
# `when` + `control$dRange` (surveillance). `epinowcast` has no such argument, so
# it is trimmed before preprocessing.

library(dplyr)
devtools::load_all(".", quiet = TRUE)

set.seed(20260820)

# -- the window ----------------------------------------------------------------

CUT       <- as.Date("2002-07-22")  # both dates strictly before this
MAX_DELAY <- 10L                    # weeks of delay every engine is given
TRIM      <- 104L                   # weeks kept by the engines that need trimming
N_HORIZON <- 20L                    # weeks shown in the article's figures

data(denguedat)

# Exactly the filter the article applies, on BOTH dates.
dengue <- denguedat |>
  filter(onset_week < CUT, report_week < CUT)

NOW  <- max(dengue$report_week)
grid <- sort(unique(dengue$onset_week))
horizon <- utils::tail(grid, N_HORIZON)

# -- truth and what was observed at `now` --------------------------------------

truth_for <- function(full, seen, label) {
  full |>
    count(onset_week, name = "truth") |>
    left_join(seen |> count(onset_week, name = "observed"), by = "onset_week") |>
    mutate(observed = coalesce(observed, 0L), stratum = label) |>
    rename(event_date = onset_week)
}

full_window <- denguedat |> filter(onset_week < CUT)  # all reports, incl. post-cut

truth <- bind_rows(
  truth_for(full_window, dengue, "Total"),
  lapply(split(full_window, full_window$gender), function(g) {
    lvl <- as.character(g$gender[1])
    truth_for(g, dengue |> filter(gender == lvl), lvl)
  }) |> bind_rows()
)

# -- the objects the article builds --------------------------------------------

make_now <- function(data) {
  tbl_now(
    data,
    event_date = onset_week, report_date = report_week,
    data_type = "linelist", verbose = FALSE
  )
}

objects <- list(Total = make_now(dengue))
for (lvl in levels(factor(dengue$gender))) {
  objects[[lvl]] <- make_now(dengue |> filter(gender == lvl))
}

# Engines that build their grid from the rows they get need the zero weeks.
complete_to_now <- function(x) {
  suppressWarnings(complete_zeroes(to_count(x, to = "count-incidence")))
}

# -- engine adapters -----------------------------------------------------------
#
# Each returns tibble(event_date, estimate, lower, upper). `lower`/`upper` are
# NA when the engine gives no interval.

fit_diseasenowcasting <- function(x) {
  # NO trimming, and the package's own defaults.
  fit <- diseasenowcasting::nowcast(x)
  prediction <- stats::predict(fit)
  draws <- S7::prop(prediction, "draws")
  out <- tibble(
    event_date = S7::prop(prediction, "event_dates"),
    estimate   = apply(draws, 2, stats::median),
    lower      = apply(draws, 2, stats::quantile, probs = 0.025),
    upper      = apply(draws, 2, stats::quantile, probs = 0.975)
  )
  # The article prints `dnc_fit` itself, so carry the fit along; `prediction` is
  # what tidy() dispatches on.
  attr(out, "fit")     <- fit
  attr(out, "tidy_on") <- prediction
  out
}

fit_baselinenowcast <- function(x) {
  # NO trimming: the whole series.
  #
  # Completing the zero weeks out to `now` is what lets the triangle reach the
  # final week -- but baselinenowcast refuses a triangle whose most recent
  # reference times are all zero ("the method to iteratively complete the
  # reporting triangle and estimate a delay PMF will be invalid"). A thin
  # stratum can hit exactly that: here the female series has no case in the last
  # week even though the pooled series does. Fall back to completing only as far
  # as the last week that actually holds a case, which costs that stratum one
  # week rather than the whole nowcast.
  # Drop trailing reference times that are FORECASTS, not nowcasts.
  #
  # baselinenowcast divides an observed row by the share of the delay
  # distribution that should have arrived by now. Here P(delay = 0) is only
  # about 0.05 -- almost nothing is reported in the same week -- so the newest
  # week, seen only at delay 0, gets divided by 0.05 and explodes: a single case
  # became an estimate of 257 with an upper bound of 1584, against a truth of 15.
  # baselinenowcast's own error text recommends the remedy ("truncating to an
  # earlier reference time to ensure a nowcast, not a forecast, is being
  # produced"), so drop rows whose expected observed share is below
  # MIN_OBSERVED_SHARE.
  MIN_OBSERVED_SHARE <- 0.10

  trim_forecast_rows <- function(triangle) {
    pmf <- as.numeric(baselinenowcast::estimate_delay(triangle))
    repeat {
      n_rows <- nrow(triangle)
      if (n_rows < 10) break
      # delays already observable for the last row
      observed_delays <- sum(!is.na(triangle[n_rows, ]))
      share <- sum(utils::head(pmf, max(observed_delays, 1L)))
      if (share >= MIN_OBSERVED_SHARE) break
      triangle <- triangle[seq_len(n_rows - 1L), , drop = FALSE]
    }
    triangle
  }

  draw_from <- function(triangle) {
    baselinenowcast::baselinenowcast(
      trim_forecast_rows(triangle), output_type = "samples", draws = 1000
    )
  }
  # The converter aggregates and completes a line list on its own now.
  samples <- tryCatch(
    draw_from(tbl_now_to_baselinenowcast(x, verbose = FALSE)),
    error = function(e) NULL
  )
  if (is.null(samples)) {
    counts <- to_count(x, to = "count-incidence")
    last_seen <- max(counts[[get_event_date(counts)]], na.rm = TRUE)
    message("      (baselinenowcast: completing only to ", format(last_seen), ")")
    samples <- draw_from(tbl_now_to_baselinenowcast(
      suppressWarnings(complete_zeroes(counts, until = last_seen)),
      verbose = FALSE
    ))
  }
  out <- as_tibble(samples) |>
    group_by(event_date = as.Date(.data$reference_date)) |>
    summarise(
      estimate = stats::median(.data$pred_count),
      lower    = stats::quantile(.data$pred_count, 0.025),
      upper    = stats::quantile(.data$pred_count, 0.975),
      .groups  = "drop"
    )
  attr(out, "tidy_on") <- samples
  out
}

fit_epinowcast <- function(x) {
  # Trimmed BEFORE preprocessing: epinowcast has no argument for it.
  observations <- tbl_now_to_epinowcast(
    x, preprocess = FALSE, verbose = FALSE, quiet = TRUE
  )
  recent <- epinowcast::enw_filter_reference_dates(
    observations, include_days = TRIM * 7
  )
  preprocessed <- epinowcast::enw_preprocess_data(
    recent, max_delay = MAX_DELAY, timestep = "week"
  )
  fit <- epinowcast::epinowcast(
    preprocessed,
    fit = epinowcast::enw_fit_opts(
      pp = TRUE, chains = 2, iter_sampling = 500, iter_warmup = 500,
      show_messages = FALSE, refresh = 0
    )
  )
  nowcast <- summary(fit, type = "nowcast")
  out <- tibble(
    event_date = as.Date(nowcast$reference_date),
    estimate   = nowcast$median,
    lower      = nowcast$q5,
    upper      = nowcast$q95
  )
  attr(out, "fit")     <- fit
  attr(out, "tidy_on") <- fit
  out
}

fit_nobbs <- function(x) {
  # Trimmed with NobBS's own `moving_window`.
  fit <- NobBS::NobBS(
    data          = as.data.frame(x),
    now           = get_now(x),
    units         = "1 week",
    onset_date    = get_event_date(x),
    report_date   = get_report_date(x),
    max_D         = MAX_DELAY,
    moving_window = TRIM
  )
  est <- fit$estimates
  out <- tibble(
    event_date = as.Date(est$onset_date),
    estimate   = est$estimate,
    lower      = est$lower,
    upper      = est$upper
  )
  attr(out, "fit")     <- fit
  attr(out, "tidy_on") <- fit
  out
}

fit_surveillance <- function(x) {
  # Trimmed with `when`; `control$dRange` extends the grid to `now`, which a
  # line list cannot express on its own (a zero week has no rows).
  linelist <- tbl_now_to_surveillance(x, verbose = FALSE)
  when   <- seq(get_now(x) - 7 * N_HORIZON, get_now(x), by = "1 week")
  drange <- seq(min(linelist$dHospital), get_now(x), by = "1 week")
  fit <- surveillance::nowcast(
    now = get_now(x), when = when, data = linelist,
    dEventCol = "dHospital", dReportCol = "dReport",
    aggregate.by = "1 week", D = MAX_DELAY,
    method = "bayes.notrunc.bnb",
    control = list(dRange = drange, N.tInf.max = 1000, nSamples = 1000)
  )
  # `nowcast()` stores a 95% prediction interval in the object's `pi` slot
  # (`control$alpha = 0.05`), so surveillance does not need the JAGS-backed
  # `bayes.trunc*` methods to report uncertainty. `tidy()` reads that slot.
  tidied <- generics::tidy(fit)
  out <- tibble(
    event_date = tidied$event_date,
    estimate   = tidied$estimate,
    lower      = tidied$conf.low,
    upper      = tidied$conf.high
  )
  attr(out, "fit")     <- fit
  attr(out, "tidy_on") <- fit
  out
}

# nowcaster is the one engine fitted ONCE for all three panels: it stratifies
# natively through `age_col`, so a single fit returns the pooled `$total` and the
# per-stratum `$age`. `tbl_now_to_nowcaster()` supplies the numeric code column
# and the matching breaks it needs (its `age_col` must be numeric, despite the
# help calling it a "stratum column").
#
# Note `Dmax` is nowcaster's NOWCAST HORIZON -- how many past weeks it estimates
# -- not a maximum delay. `wdw` is the fitting window.
fit_nowcaster_all <- function(x_stratified) {
  linelist <- tbl_now_to_nowcaster(x_stratified, verbose = FALSE)
  levels_map <- attr(linelist, "nowcaster_levels")

  fit <- nowcaster::nowcasting_inla(
    dataset      = linelist,
    date_onset   = date_onset,
    date_report  = date_report,
    age_col      = stratum_code,
    bins_age     = attr(linelist, "nowcaster_bins"),
    data.by.week = TRUE,
    use.epiweek  = FALSE,
    Dmax         = N_HORIZON,
    wdw          = TRIM,
    silent       = TRUE
  )

  pooled <- tibble(
    event_date = as.Date(fit$total$dt_event),
    estimate   = fit$total$Median,
    lower      = fit$total$LI,
    upper      = fit$total$LS,
    stratum    = "Total"
  )
  by_stratum <- as.data.frame(fit$age)
  per <- tibble(
    event_date = as.Date(by_stratum$dt_event),
    estimate   = by_stratum$Median,
    lower      = by_stratum$LI,
    upper      = by_stratum$LS,
    stratum    = levels_map[as.integer(as.character(by_stratum$fx_etaria))]
  )
  out <- dplyr::bind_rows(pooled, per)
  attr(out, "fit")     <- fit
  attr(out, "tidy_on") <- fit
  out
}

ENGINES <- list(
  diseasenowcasting = fit_diseasenowcasting,
  baselinenowcast   = fit_baselinenowcast,
  epinowcast        = fit_epinowcast,
  NobBS             = fit_nobbs,
  surveillance      = fit_surveillance
)

# Engines report on their own week starts; snap onto the data's grid. CEILING,
# not floor -- see the article's caveats: nowcaster labels a week by its Tuesday,
# so its label holds the cases `denguedat` files under the following Monday.
snap_to_grid <- function(dates, grid) {
  dates <- as.Date(dates)
  idx <- vapply(dates, function(d) {
    hit <- which(grid >= d)
    if (length(hit)) hit[1L] else NA_integer_
  }, integer(1))
  out <- rep(as.Date(NA), length(idx))
  out[!is.na(idx)] <- grid[idx[!is.na(idx)]]
  out
}

# The article shows `dnc_fit` and `tidy(dnc_fit)` and must display their real
# output, but the fits are far too slow to run on every build. Rather than ship
# the fitted objects (Stan and INLA fits are large and version-fragile), capture
# exactly what the article prints: the fit's own print output, and its tidy()
# table. `display` below is what the article reads back.
display <- list()

capture_display <- function(fit, tidy_on) {
  # Capture BOTH streams. Several packages' print methods emit their output as
  # *messages* rather than to stdout (diseasenowcasting is one), so capturing
  # stdout alone silently returns nothing.
  printed <- if (is.null(fit)) NULL else tryCatch({
    from_message <- NULL
    from_stdout <- utils::capture.output(
      from_message <- utils::capture.output(print(fit), type = "message")
    )
    if (length(from_stdout)) c(from_stdout, from_message) else from_message
  }, error = function(e) paste("<print failed:", conditionMessage(e), ">"))
  tidied <- if (is.null(tidy_on)) NULL else tryCatch(
    suppressWarnings(suppressMessages(tbl.now::tidy(tidy_on))),
    error = function(e) NULL
  )
  list(printed = printed, tidy = tidied)
}

run_engine <- function(name, fun, x, stratum) {
  message("  - ", name, " [", stratum, "] ...", appendLF = FALSE)
  started <- Sys.time()
  out <- tryCatch(
    suppressWarnings(suppressMessages(fun(x))),
    error = function(e) {
      message(" FAILED: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(out)) return(NULL)
  secs <- round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1)
  message(" ok (", secs, "s)")

  # Only the pooled fit is displayed in the article.
  if (identical(stratum, "Total")) {
    display[[name]] <<- capture_display(attr(out, "fit"), attr(out, "tidy_on"))
  }

  out |>
    mutate(
      event_date = snap_to_grid(.data$event_date, grid),
      package    = name,
      stratum    = stratum,
      seconds    = secs
    ) |>
    filter(!is.na(.data$event_date))
}

results <- list()
for (stratum in names(objects)) {
  message("== stratum: ", stratum, " ==")
  for (name in names(ENGINES)) {
    results[[length(results) + 1L]] <-
      run_engine(name, ENGINES[[name]], objects[[stratum]], stratum)
  }
}

# nowcaster: one stratified fit covers all three panels.
message("== nowcaster (single stratified fit) ==")
stratified <- tbl_now(
  dengue,
  event_date = onset_week, report_date = report_week,
  strata = gender, data_type = "linelist", verbose = FALSE
)
nowcaster_rows <- tryCatch(
  suppressWarnings(suppressMessages(fit_nowcaster_all(stratified))),
  error = function(e) {
    message("  - nowcaster FAILED: ", conditionMessage(e))
    NULL
  }
)
if (!is.null(nowcaster_rows)) {
  message("  - nowcaster ok (", nrow(nowcaster_rows), " rows)")
  display[["nowcaster"]] <- capture_display(
    attr(nowcaster_rows, "fit"), attr(nowcaster_rows, "tidy_on")
  )
  results[[length(results) + 1L]] <- nowcaster_rows |>
    mutate(
      event_date = snap_to_grid(.data$event_date, grid),
      package    = "nowcaster",
      seconds    = NA_real_
    ) |>
    filter(!is.na(.data$event_date))
}

nowcasts <- bind_rows(results) |>
  select(package, stratum, event_date, estimate, lower, upper, seconds) |>
  arrange(package, stratum, event_date)

# The article also shows `nowcast(dengue_seasonal)`, so capture that fit too.
message("== diseasenowcasting (seasonal, for display) ==")
seasonal_obj <- objects[["Total"]] |>
  add_strata(gender) |>
  add_temporal_effects(temporal_effects(week_of_year = TRUE, seasons = 52))
seasonal_fit <- tryCatch(
  suppressWarnings(suppressMessages(diseasenowcasting::nowcast(seasonal_obj))),
  error = function(e) {
    message("  FAILED: ", conditionMessage(e))
    NULL
  }
)
if (!is.null(seasonal_fit)) {
  display[["diseasenowcasting_seasonal"]] <- capture_display(
    seasonal_fit, stats::predict(seasonal_fit)
  )
  message("  ok")
}

comparison <- list(
  nowcasts = nowcasts,
  truth    = truth,
  display  = display,
  meta = list(
    now           = NOW,
    cut           = CUT,
    window_start  = min(grid),
    horizon       = horizon,
    max_delay     = MAX_DELAY,
    trim          = TRIM,
    n_weeks       = length(grid),
    n_cases       = nrow(dengue),
    built_on      = Sys.Date(),
    package_order = names(ENGINES)
  )
)

saveRDS(comparison, "vignettes/articles/nowcast-comparison.rds")

message(
  "\nSaved ", nrow(nowcasts), " rows from ",
  dplyr::n_distinct(nowcasts$package), " engine(s) across ",
  dplyr::n_distinct(nowcasts$stratum), " strata."
)
print(nowcasts |> count(package, stratum) |> tidyr::pivot_wider(
  names_from = stratum, values_from = n, values_fill = 0L
))

# -- sanity check --------------------------------------------------------------
#
# A nowcast estimates what a week will EVENTUALLY reach, so it should not sit
# below what had already been reported by `now`. A whole engine landing under the
# observed counts signals a week-grid misalignment rather than a bad model.
alignment <- nowcasts |>
  inner_join(truth, by = c("stratum", "event_date")) |>
  filter(event_date %in% horizon) |>
  group_by(package) |>
  summarise(
    weeks          = dplyr::n(),
    below_observed = sum(estimate < observed - 0.5, na.rm = TRUE),
    worst_gap      = min(estimate - observed, na.rm = TRUE),
    .groups        = "drop"
  )
message("\nAlignment check (estimate vs. what was already reported):")
print(as.data.frame(alignment))
