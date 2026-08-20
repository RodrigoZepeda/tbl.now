# nowcast_comparison -----------------------------------------------------------
#
# Fits every nowcasting engine covered by vignettes/articles/nowcasting-models.Rmd
# to one common dengue window, and saves a tidy table of their nowcasts plus the
# ground truth. The article reads the saved file and only draws the plots, so
# editing the prose never re-runs Stan / JAGS / INLA.
#
# Run with:  source("data-raw/nowcast_comparison.R")
# Output:    vignettes/articles/nowcast-comparison.rds
#
# WHY THIS WINDOW. The article's main `dengue_now` (onset >= 2009-01-01) runs to
# the very end of `denguedat`, so its most recent weeks are still incomplete and
# there is no ground truth to compare against. Here we use an EARLIER window of
# comparable length (104 weeks in 2002-2003). Reports are truncated at `now`, so
# each engine faces a genuine nowcasting problem, while the full `denguedat`
# still holds the counts those weeks eventually reached -- the "truth" line.
#
# Engines: diseasenowcasting, baselinenowcast, epinowcast, NobBS, surveillance,
# nowcaster. `epidist` is deliberately absent: it estimates the reporting-delay
# distribution, not a case-count nowcast, so it has nothing to draw on this axis.

library(dplyr)
devtools::load_all(".", quiet = TRUE)

set.seed(20260819)

# -- the comparison window -----------------------------------------------------

WINDOW_START <- as.Date("2002-01-01")
WINDOW_END   <- as.Date("2003-12-31")
MAX_DELAY    <- 10L   # weeks; the delay window every engine is given
N_HORIZON    <- 10L   # weeks back from `now` that we plot

data(denguedat)

window_data <- denguedat |>
  filter(onset_week >= WINDOW_START, onset_week <= WINDOW_END)

NOW <- max(window_data$onset_week)

# What a nowcaster would actually have seen on `NOW`.
observed_data <- window_data |> filter(report_week <= NOW)

# The common weekly grid every engine's output is snapped onto.
grid <- sort(unique(window_data$onset_week))
horizon <- utils::tail(grid, N_HORIZON + 1L)

# Engines aggregate onto their own week starts, which can sit a day or two off
# `denguedat`'s Mondays, so every engine's dates are snapped onto the common
# grid before anything is joined or plotted.
#
# The rule is CEILING -- the first grid week on or after the returned date --
# not floor, and the difference matters. `denguedat` labels a week by its
# MONDAY. \pkg{nowcaster} re-bins the line list onto its own TUESDAY-start
# weeks, so its label 2003-12-02 covers Dec 2-8 and therefore holds the cases
# `denguedat` files under 2003-12-08. Flooring would map it to 2003-12-01 and
# shift that engine's whole curve a week early, making it look as though it
# nowcast *below* the data it was given. Every other engine returns dates
# already on the grid, and ceiling leaves those untouched.
snap_to_grid <- function(dates, grid) {
  dates <- as.Date(dates)
  idx <- vapply(
    dates,
    function(d) {
      hit <- which(grid >= d)
      if (length(hit)) hit[1L] else NA_integer_
    },
    integer(1)
  )
  out <- rep(as.Date(NA), length(idx))
  out[!is.na(idx)] <- grid[idx[!is.na(idx)]]
  out
}

# -- ground truth --------------------------------------------------------------

truth_for <- function(full, seen, label) {
  final <- full |> count(onset_week, name = "truth")
  seen_n <- seen |> count(onset_week, name = "observed")
  final |>
    left_join(seen_n, by = "onset_week") |>
    mutate(
      observed = coalesce(observed, 0L),
      stratum  = label
    ) |>
    rename(event_date = onset_week)
}

truth <- bind_rows(
  truth_for(window_data, observed_data, "Total"),
  window_data |>
    group_split(gender) |>
    lapply(function(g) {
      lvl <- as.character(g$gender[1])
      truth_for(g, observed_data |> filter(gender == lvl), lvl)
    }) |>
    bind_rows()
)

# -- one tbl_now per stratum ---------------------------------------------------

make_tbl_now <- function(data) {
  tbl_now(
    data,
    event_date  = onset_week,
    report_date = report_week,
    data_type   = "linelist",
    verbose     = FALSE
  )
}

objects <- list(Total = make_tbl_now(observed_data))
for (lvl in levels(factor(observed_data$gender))) {
  objects[[lvl]] <- make_tbl_now(observed_data |> filter(gender == lvl))
}

# -- engine adapters -----------------------------------------------------------
#
# Each returns a tibble(event_date, estimate, lower, upper) or NULL. They are
# called through `run_engine()`, which times them and turns a failure into a
# warning rather than losing the whole run.

fit_diseasenowcasting <- function(x) {
  fit <- diseasenowcasting::nowcast(x, type = "one_stage", n_draws = 2000L)
  # Use predict(), NOT summary(fit)$latent. `latent` is the smoothed LATENT
  # epidemic curve, which legitimately sits below the reported counts and so is
  # not comparable with the other engines' nowcasts. predict() returns the
  # posterior draws of the eventual case counts, plus the event dates already on
  # the data's own weekly grid.
  prediction <- stats::predict(fit)
  draws <- S7::prop(prediction, "draws")           # draws x event times
  tibble(
    event_date = S7::prop(prediction, "event_dates"),
    estimate   = apply(draws, 2, stats::median),
    lower      = apply(draws, 2, stats::quantile, probs = 0.025),
    upper      = apply(draws, 2, stats::quantile, probs = 0.975)
  )
}

fit_baselinenowcast <- function(x) {
  triangle <- tbl_now_to_baselinenowcast(x, verbose = FALSE)
  samples <- baselinenowcast::baselinenowcast(
    triangle, output_type = "samples", draws = 1000
  )
  as_tibble(samples) |>
    group_by(event_date = as.Date(.data$reference_date)) |>
    summarise(
      estimate = stats::median(.data$pred_count),
      lower    = stats::quantile(.data$pred_count, 0.025),
      upper    = stats::quantile(.data$pred_count, 0.975),
      .groups  = "drop"
    )
}

fit_epinowcast <- function(x) {
  enw <- tbl_now_to_epinowcast(x, verbose = FALSE, quiet = TRUE)
  fit <- epinowcast::epinowcast(
    enw,
    fit = epinowcast::enw_fit_opts(
      pp = TRUE, chains = 2, iter_sampling = 500, iter_warmup = 500,
      show_messages = FALSE, refresh = 0
    )
  )
  nowcast <- summary(fit, type = "nowcast")
  tibble(
    event_date = as.Date(nowcast$reference_date),
    estimate   = nowcast$median,
    lower      = nowcast$q5,
    upper      = nowcast$q95
  )
}

fit_nobbs <- function(x) {
  est <- NobBS::NobBS(
    data        = as.data.frame(x),
    now         = get_now(x),
    units       = "1 week",
    onset_date  = get_event_date(x),
    report_date = get_report_date(x),
    max_D       = MAX_DELAY,
    moving_window = 52
  )$estimates
  tibble(
    event_date = as.Date(est$onset_date),
    estimate   = est$estimate,
    lower      = est$lower,
    upper      = est$upper
  )
}

fit_surveillance <- function(x) {
  linelist <- tbl_now_to_surveillance(x, verbose = FALSE)
  when <- seq(get_now(x) - 7 * N_HORIZON, get_now(x), by = "1 week")
  fit <- surveillance::nowcast(
    now = get_now(x), when = when, data = linelist,
    dEventCol = "dHospital", dReportCol = "dReport",
    aggregate.by = "1 week", D = MAX_DELAY,
    method = "bayes.notrunc.bnb",
    control = list(N.tInf.max = 1000, nSamples = 1000)
  )
  tibble(
    event_date = as.Date(surveillance::epoch(fit)),
    estimate   = as.numeric(surveillance::upperbound(fit)),
    lower      = NA_real_,
    upper      = NA_real_
  ) |>
    filter(!is.na(.data$estimate))
}

fit_nowcaster <- function(x) {
  linelist <- tbl_now_to_nowcaster(x, verbose = FALSE)
  fit <- nowcaster::nowcasting_inla(
    dataset = linelist, date_onset = date_onset, date_report = date_report,
    data.by.week = TRUE, Dmax = MAX_DELAY, wdw = 30, silent = TRUE
  )
  tibble(
    event_date = as.Date(fit$total$dt_event),
    estimate   = fit$total$Median,
    lower      = fit$total$LI,
    upper      = fit$total$LS
  )
}

ENGINES <- list(
  diseasenowcasting = fit_diseasenowcasting,
  baselinenowcast   = fit_baselinenowcast,
  epinowcast        = fit_epinowcast,
  NobBS             = fit_nobbs,
  surveillance      = fit_surveillance,
  nowcaster         = fit_nowcaster
)

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
  out |>
    mutate(
      event_date = snap_to_grid(.data$event_date, grid),
      package    = name,
      stratum    = stratum
    ) |>
    filter(!is.na(.data$event_date))
}

# -- run everything ------------------------------------------------------------

results <- list()
for (stratum in names(objects)) {
  message("== stratum: ", stratum, " ==")
  for (name in names(ENGINES)) {
    results[[length(results) + 1L]] <-
      run_engine(name, ENGINES[[name]], objects[[stratum]], stratum)
  }
}

nowcasts <- bind_rows(results) |>
  select(package, stratum, event_date, estimate, lower, upper) |>
  arrange(package, stratum, event_date)

comparison <- list(
  nowcasts = nowcasts,
  truth    = truth,
  meta = list(
    now          = NOW,
    window_start = WINDOW_START,
    window_end   = WINDOW_END,
    horizon      = horizon,
    max_delay    = MAX_DELAY,
    built_on     = Sys.Date(),
    package_order = names(ENGINES)
  )
)

saveRDS(comparison, "vignettes/articles/nowcast-comparison.rds")

message(
  "\nSaved ", nrow(nowcasts), " nowcast rows from ",
  dplyr::n_distinct(nowcasts$package), " engine(s) across ",
  dplyr::n_distinct(nowcasts$stratum), " strata."
)
print(nowcasts |> count(package, stratum) |> tidyr::pivot_wider(
  names_from = stratum, values_from = n, values_fill = 0L
))

# -- alignment sanity check ----------------------------------------------------
#
# A nowcast estimates what a week will EVENTUALLY reach, so it should not sit
# below what had already been reported by `now`. A whole engine landing under
# the observed counts is the signature of a week-grid misalignment (see
# `snap_to_grid()` above), not of a bad model, so flag it loudly here rather
# than letting a misleading figure reach the article.
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

suspect <- alignment |> filter(below_observed > weeks / 2)
if (nrow(suspect)) {
  warning(
    "These engines nowcast below the observed data in most weeks, which \
     usually means their dates were snapped onto the wrong grid week: ",
    paste(suspect$package, collapse = ", "),
    call. = FALSE
  )
}
