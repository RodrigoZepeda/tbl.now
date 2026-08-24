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
# WHY THIS WINDOW. `covid_colombia` records notifications to 2023-03-03 but
# diagnoses to 2023-05-26. Cutting BOTH dates at 2023-03-03 gives a `tbl_now`
# whose recent days are genuinely incomplete -- a real nowcasting problem --
# while the full dataset still records what those days eventually reached. That
# is the "truth" line. The cut sits in a low-incidence stretch, which keeps the
# line-list engines affordable: expanding counts to one row per case costs
# ~50,000 rows over the trailing 180 days, against 2.5M at the 2021 peak.
#
# TRIMMING, measured rather than assumed (timings on the article's own data):
#
#   diseasenowcasting  full 1,094-day series, no trim ......  20s (pooled)
#   baselinenowcast    full series, delays capped at 30d ....  50s  (331 delay
#                      columns cost 314s; 99% of delays land within 15 days)
#   surveillance       180-day window ........................   2s
#   NobBS              180-day window ........................  37s
#   epinowcast         180-day window, max_delay = 30 ........ 351s
#
# `covid_colombia` is DAILY and count-incidence, which removes the artifacts the
# weekly line list forced on this script: there are no negative delays, and
# P(delay = 0) is 0.279 rather than 0.047, so baselinenowcast no longer needs
# trailing rows dropped to stop the newest period exploding.
#
# The source file is stratified by sex, so a pooled object must aggregate OVER
# sex first: an undeclared column leaves two rows per (event, report) cell, and
# a reporting triangle has one slot per cell.

library(dplyr)
devtools::load_all(".", quiet = TRUE)

set.seed(20260820)

# -- the window ----------------------------------------------------------------

CUT       <- as.Date("2023-03-03")  # both dates strictly before this
MAX_DELAY <- 30L                    # days of delay every engine is given
TRIM      <- 180L                   # days kept by the engines that need trimming
N_HORIZON <- 30L                    # days shown in the article's figures

data(covid_colombia)

# Exactly the filter the article applies, on BOTH dates.
covid <- covid_colombia |>
  filter(notification_date < CUT, diagnosis_date < CUT)

NOW  <- max(covid$diagnosis_date)
grid <- sort(unique(covid$notification_date))
horizon <- utils::tail(grid, N_HORIZON)

# -- truth and what was observed at `now` --------------------------------------

truth_for <- function(full, seen, label) {
  full |>
    group_by(notification_date) |>
    summarise(truth = sum(n), .groups = "drop") |>
    left_join(
      seen |> group_by(notification_date) |> summarise(observed = sum(n), .groups = "drop"),
      by = "notification_date"
    ) |>
    mutate(observed = coalesce(observed, 0L), stratum = label) |>
    rename(event_date = notification_date)
}

# All reports, including those that arrived after the cut -- the eventual truth.
full_window <- covid_colombia |> filter(notification_date < CUT)

truth <- bind_rows(
  truth_for(full_window, covid, "Total"),
  lapply(split(full_window, full_window$sex), function(g) {
    lvl <- as.character(g$sex[1])
    truth_for(g, covid |> filter(sex == lvl), lvl)
  }) |> bind_rows()
)

# -- the objects the article builds --------------------------------------------

make_now <- function(data) {
  # Pool over any column that is not declared, so each (event, report) cell is
  # unique; `sex` is a stratum only where the article asks for one.
  pooled <- data |>
    group_by(notification_date, diagnosis_date) |>
    summarise(n = sum(n), .groups = "drop")
  tbl_now(
    pooled,
    event_date = notification_date, report_date = diagnosis_date,
    case_count = n, data_type = "count-incidence", verbose = FALSE
  )
}

objects <- list(Total = make_now(covid))
for (lvl in levels(factor(covid$sex))) {
  objects[[lvl]] <- make_now(covid |> filter(sex == lvl))
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
  # The article prints `dnc_fit` and then calls `tidy(dnc_fit)`, so tidy the FIT
  # -- that is the call on the page. From diseasenowcasting 2.1.0 the fit has its
  # own method, which delegates to `predict()`; the resulting table is the same
  # one `tidy(predict(fit))` gives, bar the sampling noise in `predict()`.
  attr(out, "fit")     <- fit
  attr(out, "tidy_on") <- fit
  out
}

fit_baselinenowcast <- function(x) {
  # NO trimming of rows, and no zero-completion workaround: on daily COVID data
  # P(delay = 0) is 0.279, so the newest reference date is divided by a sensible
  # share rather than by 0.047, and it does not explode. (On the weekly dengue
  # line list this function had to drop trailing rows to keep a single case from
  # becoming an estimate of 257.)
  #
  # Delays ARE capped, which is a modelling choice rather than a workaround: a
  # lone 330-day straggler makes the triangle 1094 x 331 and the fit take 314s,
  # against 50s at 1094 x 31, and 99% of delays arrive within 15 days.
  capped <- x |> filter(.data$.delay <= MAX_DELAY)
  triangle <- tbl_now_to_baselinenowcast(capped, verbose = FALSE)

  samples <- baselinenowcast::baselinenowcast(
    triangle, output_type = "samples", draws = 1000
  )
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
    observations, include_days = TRIM
  )
  preprocessed <- epinowcast::enw_preprocess_data(
    recent, max_delay = MAX_DELAY, timestep = "day"
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

# NobBS and surveillance both count ROWS, so count-incidence input has to be
# expanded to one row per case. Their converters do that; all this has to do is
# TRIM FIRST, because the whole series is 6.1M cases and each engine's own
# window argument limits only what it fits, not what it is handed.
recent <- function(x) {
  x |> filter(.data[[get_event_date(x)]] >= get_now(x) - TRIM)
}

fit_nobbs <- function(x) {
  linelist <- tbl_now_to_nobbs(recent(x), verbose = FALSE)
  fit <- NobBS::NobBS(
    data          = linelist,
    now           = get_now(x),
    units         = "1 day",
    onset_date    = "onset_date",
    report_date   = "report_date",
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
  linelist <- tbl_now_to_surveillance(recent(x), verbose = FALSE)
  now    <- get_now(x)
  when   <- seq(now - N_HORIZON, now, by = "1 day")
  drange <- seq(min(linelist$dHospital), now, by = "1 day")
  fit <- surveillance::nowcast(
    now = now, when = when, data = linelist,
    dEventCol = "dHospital", dReportCol = "dReport",
    aggregate.by = "1 day", D = MAX_DELAY,
    method = "bayes.notrunc.bnb",
    control = list(dRange = drange, N.tInf.max = 100000, nSamples = 1000)
  )
  # `nowcast()` stores a 95% prediction interval in the object's `pi` slot
  # (`control$alpha = 0.05`); `tidy()` reads that slot.
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

ENGINES <- list(
  diseasenowcasting = fit_diseasenowcasting,
  baselinenowcast   = fit_baselinenowcast,
  epinowcast        = fit_epinowcast,
  NobBS             = fit_nobbs,
  surveillance      = fit_surveillance
)

# Engines report on their own grids; snap onto the data's daily grid. CEILING,
# not floor: a label carries the cases that fall on or before it.
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

nowcasts <- bind_rows(results) |>
  select(package, stratum, event_date, estimate, lower, upper, seconds) |>
  arrange(package, stratum, event_date)

# The article also shows `nowcast(covid_seasonal)`, so capture that fit too.
message("== diseasenowcasting (seasonal, for display) ==")
seasonal_obj <- tbl_now(
  covid,
  event_date = notification_date, report_date = diagnosis_date,
  case_count = n, strata = sex, data_type = "count-incidence", verbose = FALSE
) |>
  add_temporal_effects(temporal_effects(day_of_week = TRUE, seasons = 365))
seasonal_fit <- tryCatch(
  suppressWarnings(suppressMessages(diseasenowcasting::nowcast(seasonal_obj))),
  error = function(e) {
    message("  FAILED: ", conditionMessage(e))
    NULL
  }
)
if (!is.null(seasonal_fit)) {
  display[["diseasenowcasting_seasonal"]] <- capture_display(
    seasonal_fit, seasonal_fit
  )
  message("  ok")
}

# -- NobBS with user-chosen quantiles ------------------------------------------
# `tidy(probs =)` is refused for NobBS because the fit keeps no draws to
# re-summarise. NobBS can compute any quantiles you like, but they have to be
# ASKED FOR at fit time, via `specs$quantiles`; the article shows how.
message("== NobBS (custom quantiles, for display) ==")
nobbs_q <- tryCatch(
  suppressWarnings(suppressMessages(NobBS::NobBS(
    data          = tbl_now_to_nobbs(recent(objects[["Total"]]), verbose = FALSE),
    now           = get_now(objects[["Total"]]),
    units         = "1 day",
    onset_date    = "onset_date",
    report_date   = "report_date",
    max_D         = MAX_DELAY,
    moving_window = TRIM,
    specs         = list(quantiles = c(0.1, 0.5, 0.9))
  ))),
  error = function(e) {
    message("  FAILED: ", conditionMessage(e))
    NULL
  }
)
if (!is.null(nobbs_q)) {
  display[["NobBS_quantiles"]] <- list(
    printed   = NULL,
    tidy      = suppressWarnings(suppressMessages(tbl.now::tidy(nobbs_q))),
    estimates = utils::tail(nobbs_q$estimates, 5)
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
    n_days        = length(grid),
    n_cases       = sum(covid$n),
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
