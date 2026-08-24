# ensemble_comparison ----------------------------------------------------------
#
# Fits every member of the ensemble article on three very different epidemics,
# builds the ensembles, and saves everything the article displays.
#
# Run with:  Rscript data-raw/ensemble_comparison.R           # all three
#            Rscript data-raw/ensemble_comparison.R dengue    # just one, merged
#            source("data-raw/ensemble_comparison.R")         # all three
# Output:    vignettes/articles/ensemble-comparison.rds
#
# Naming epidemics on the command line refits ONLY those and merges them into the
# existing file, leaving the others' rows and timings alone.
#
# IMPORTANT -- keep this file and the article in step. The calls below are the
# calls the article shows. The article displays them with `eval = FALSE` and
# reads the results from this file, because the Stan and JAGS fits are far too
# slow to run on every build. Change a call here, change it there.
#
# WHY THREE EPIDEMICS. The article's claim is that an ensemble buys insurance
# against picking the wrong model, and that claim is only testable across
# outbreaks that differ. These three do: dengue is a weekly line list with a
# short delay tail, mpox is daily counts with a very long one (reports arrive
# for seven months after the last case), covid is daily counts at a scale three
# orders of magnitude larger.
#
# WHY THESE MEMBERS. baselinenowcast, diseasenowcasting and NobBS all nowcast on
# the object's OWN event-date grid, so their predictions line up with the truth
# date for date. epinowcast and EpiNow2 would fit here too, but EpiNow2 in
# particular re-bins onto its own daily grid and models an infection process
# rather than a reporting one, which makes a date-for-date WIS comparison
# misleading rather than merely slow.

library(dplyr)
devtools::load_all(".", quiet = TRUE)

OUT <- "vignettes/articles/ensemble-comparison.rds"
QUANTILES <- nowcast_quantile_levels()

# One `set.seed()` at the top of a script is not reproducibility: it only pins
# anything if every engine consumes the same random numbers in the same order,
# so refitting a SUBSET lands somewhere else entirely. Everything below is
# seeded per (epidemic, model) immediately before the fit, and the backtests get
# the same treatment through `nowcast_backtest(seed = )`.
BASE_SEED <- 20260824L

fit_seed <- function(...) BASE_SEED + sum(utf8ToInt(paste(..., sep = "|")))

# -- the three epidemics -------------------------------------------------------
#
# `now` is chosen so that later reports still exist to score against, and
# `horizon` is how many periods back from `now` the comparison covers: older
# dates are complete by then, so scoring them measures nothing.

data(denguedat)
data(mpoxdat)
data(covid_colombia)

pooled_tbl_now <- function(data, event, report, count = NULL, units) {
  # Pool over every undeclared column first: a column that is neither declared
  # nor protected leaves two rows per (event, report) cell, and a reporting
  # triangle has one slot per cell.
  if (is.null(count)) {
    data <- data |>
      count(.data[[event]], .data[[report]], name = ".n")
    count <- ".n"
  } else {
    data <- data |>
      group_by(.data[[event]], .data[[report]]) |>
      summarise(.n = sum(.data[[count]]), .groups = "drop")
    count <- ".n"
  }
  # Bare strings, not `all_of()`: `tbl_now()` takes tidy-select, and `all_of()`
  # outside a selecting context has been deprecated since tidyselect 1.2.0.
  tbl_now(data,
    event_date = event, report_date = report, case_count = count,
    data_type = "count-incidence",
    event_units = units, report_units = units, verbose = FALSE
  )
}

EPIDEMICS <- list(
  dengue = list(
    units = "weeks",
    horizon = 8L,
    max_delay = 10L,
    # Three years is plenty of history and keeps the Bayesian fits affordable.
    build = function() {
      pooled_tbl_now(
        denguedat |> filter(onset_week >= as.Date("2008-01-01")),
        "onset_week", "report_week", NULL, "weeks"
      )
    },
    now = as.Date("2010-10-04")   # a Monday: denguedat's weeks start on Mondays
  ),
  mpox = list(
    units = "days",
    horizon = 21L,
    max_delay = 30L,
    build = function() {
      pooled_tbl_now(mpoxdat, "dx_date", "dx_report_date", "n", "days")
    },
    now = as.Date("2022-09-15")
  ),
  covid = list(
    units = "days",
    horizon = 21L,
    max_delay = 30L,
    # A four-month window in a low-incidence stretch. The full series is 6.1M
    # cases, and NobBS counts ROWS: expanding all of it to one row per case is
    # neither affordable nor necessary to compare machinery.
    build = function() {
      pooled_tbl_now(
        covid_colombia |>
          filter(notification_date >= as.Date("2022-11-01"),
                 notification_date <= as.Date("2023-03-03")),
        "notification_date", "diagnosis_date", "n", "days"
      )
    },
    now = as.Date("2023-02-10")
  )
)

# -- the members ---------------------------------------------------------------
#
# Each is a `run_nowcast()` call with the arguments that epidemic needs. They are
# the calls the article shows.

MEMBERS <- c("baselinenowcast", "diseasenowcasting", "NobBS")

member_args <- function(name, config, x) {
  # `moving_window` counts EVENT PERIODS, and NobBS pads its grid backwards to
  # fill a window longer than the data it was handed -- returning ZERO for every
  # date, with no error. That reads as a catastrophic score rather than as the
  # misconfiguration it is, so the window is capped at the history available.
  periods <- length(unique(x[[get_event_date(x)]]))
  switch(name,
    # Delays are capped rather than trimmed: one long straggler gives the
    # reporting triangle a column per day of it, at no modelling benefit.
    baselinenowcast = list(draws = 1000),
    diseasenowcasting = list(),
    NobBS = list(
      max_D = config$max_delay,
      moving_window = min(8L * config$horizon, periods)
    ),
    list()
  )
}

# Cap the delay axis for every engine, so they are all shown the same data.
capped <- function(x, config) {
  filter(x, .data$.delay <= config$max_delay)
}

fit_member <- function(name, snapshot, config, epidemic, label = "now") {
  message("  - ", name, " [", epidemic, "/", label, "] ...", appendLF = FALSE)
  started <- Sys.time()
  set.seed(fit_seed(epidemic, name, label))
  out <- tryCatch(
    suppressWarnings(suppressMessages(do.call(
      run_nowcast,
      c(
        list(capped(snapshot, config), name,
             quantile_levels = QUANTILES, verbose = FALSE),
        member_args(name, config, capped(snapshot, config))
      )
    ))),
    error = function(e) {
      message(" FAILED: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(out)) return(NULL)
  message(" ok (", round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1), "s)")
  out
}

# -- plausibility --------------------------------------------------------------
#
# A Stan or JAGS fit that does not converge does not error -- it returns numbers.
# Everything above suppresses warnings, so a broken fit records as `ok`. Read the
# numbers instead: refuse anything whose scale is nowhere near the data.
IMPLAUSIBLE_FACTOR <- 100

check_plausible <- function(nowcast, observed, name, epidemic) {
  if (is.null(nowcast)) return(NULL)
  ceiling_ <- IMPLAUSIBLE_FACTOR * max(observed, na.rm = TRUE)
  worst <- max(nowcast@predictions$.value, na.rm = TRUE)
  if (worst > ceiling_) {
    stop(sprintf(
      paste0("%s [%s] did not converge: largest predicted quantile is %s against ",
             "an observed maximum of %s (>%dx). Refusing to cache it."),
      name, epidemic, format(round(worst), big.mark = ","),
      format(max(observed, na.rm = TRUE), big.mark = ","), IMPLAUSIBLE_FACTOR
    ))
  }
  nowcast
}

# -- one epidemic --------------------------------------------------------------

run_epidemic <- function(epidemic) {
  config <- EPIDEMICS[[epidemic]]
  message("== ", epidemic, " ==")

  full <- config$build()
  now <- config$now
  truth <- nowcast_truth(full)

  snapshot <- full |>
    filter(.data[[get_report_date(full)]] <= now) |>
    change_now(now = now)

  # The comparison window: the `horizon` most recent periods at `now`. Older
  # dates were already complete, so scoring them measures nothing about
  # nowcasting.
  step <- if (identical(config$units, "weeks")) 7L else 1L
  grid <- sort(unique(full[[get_event_date(full)]]))
  if (!now %in% grid) {
    stop("`now` for ", epidemic, " (", format(now), ") is not on the data's ",
         "event-date grid; the comparison window would select nothing")
  }
  window <- utils::tail(grid[grid <= now], config$horizon)

  fits <- lapply(MEMBERS, function(name) {
    check_plausible(
      fit_member(name, snapshot, config, epidemic),
      truth$.observed, name, epidemic
    )
  })
  names(fits) <- MEMBERS
  fits <- fits[!vapply(fits, is.null, logical(1))]
  if (length(fits) < 2) {
    stop("fewer than two members fitted for ", epidemic, "; nothing to ensemble")
  }

  # -- the ensembles -----------------------------------------------------------

  ensembles <- list(
    `ensemble (quantile)` = nowcast_ensemble(
      fits, type = "quantile", name = "ensemble (quantile)", verbose = FALSE
    )
  )

  # The linear pool needs draws from EVERY member, and NobBS returns none: its
  # `nowcast.post.samps` cover the `now` date only, not every event date. So the
  # pool is built from the members that do have draws, and the article says so
  # rather than quietly dropping a model from a differently named ensemble.
  with_draws <- fits[!vapply(fits, function(f) is.null(f@draws), logical(1))]
  if (length(with_draws) >= 2) {
    set.seed(fit_seed(epidemic, "linear_pool"))
    ensembles[["ensemble (linear pool)"]] <- nowcast_ensemble(
      with_draws, type = "linear_pool", n_draws = 4000,
      name = "ensemble (linear pool)", verbose = FALSE
    )
  }

  # -- performance weights -----------------------------------------------------
  #
  # Three retrospective dates, one horizon apart, all far enough back that the
  # truth had settled. This is `length(methods) x length(now_dates)` extra fits
  # and is the expensive part of the script.
  backtest_dates <- now - step * config$horizon * (3:1)
  message("  backtest at ", paste(format(backtest_dates), collapse = ", "))

  backtest <- tryCatch(
    suppressWarnings(suppressMessages(nowcast_backtest(
      capped(full, config),
      methods = names(fits),
      now_dates = backtest_dates,
      quantile_levels = QUANTILES,
      method_args = lapply(
        stats::setNames(names(fits), names(fits)), member_args,
        config = config, x = capped(full, config)
      ),
      seed = BASE_SEED, verbose = FALSE
    ))),
    error = function(e) {
      message("  backtest FAILED: ", conditionMessage(e))
      NULL
    }
  )

  weights <- NULL
  if (!is.null(backtest)) {
    weights <- nowcast_weights(backtest, type = "inverse_score")
    ensembles[["ensemble (weighted)"]] <- nowcast_ensemble(
      fits[names(weights)], type = "quantile", weights = weights,
      name = "ensemble (weighted)", verbose = FALSE
    )
  }

  # -- score everything, on one footing ----------------------------------------

  scored <- c(fits, ensembles) |>
    lapply(function(nowcast) {
      as_scoringutils(nowcast, truth = truth, observed_col = ".observed") |>
        rename(target_date = all_of(nowcast@event_date))
    }) |>
    bind_rows(.id = "model") |>
    filter(.data$target_date %in% window) |>
    mutate(epidemic = epidemic, units = config$units) |>
    select("target_date", "quantile_level", "predicted", "observed",
           "model", "epidemic", "units")

  # Two guards, both for failures that actually happened while building this.
  #
  # A `now` off the data's grid, or an engine that reports on a grid of its own,
  # leaves NOTHING to score -- and an empty block simply vanishes from the
  # article's table rather than announcing itself.
  if (nrow(scored) == 0) {
    stop(epidemic, " scored zero rows: no model's predictions landed on the ",
         "comparison window")
  }
  missing_models <- setdiff(c(names(fits), names(ensembles)), unique(scored$model))
  if (length(missing_models)) {
    stop(epidemic, ": no scored rows for ", paste(missing_models, collapse = ", "),
         " -- they predict on a different grid from the truth")
  }

  # An engine asked for a longer window than it was given data for pads its grid
  # and returns ZERO everywhere, with no error. Against a non-zero truth that
  # reads as a catastrophic score rather than as a broken call.
  flat <- scored |>
    filter(.data$quantile_level == 0.5) |>
    summarise(all_zero = all(.data$predicted == 0), .by = "model") |>
    filter(.data$all_zero)
  if (nrow(flat) && any(scored$observed > 0)) {
    stop(epidemic, ": ", paste(flat$model, collapse = ", "),
         " predicted zero for every date in the window against a non-zero truth.",
         " That is a misconfigured call, not a bad model. Refusing to cache it.")
  }

  list(
    forecasts = scored,
    weights = weights,
    backtest = if (is.null(backtest)) NULL else tidy(backtest),
    now = now,
    window = range(window),
    n_members = length(fits)
  )
}

# -- what the article prints ---------------------------------------------------
#
# The article shows `run_nowcast()`, `tidy()` and `autoplot()` on ONE fit. Rather
# than ship the fitted objects (Stan fits are large and version-fragile), capture
# exactly what the article displays and replay it there.

capture_display <- function(epidemic) {
  config <- EPIDEMICS[[epidemic]]
  full <- config$build()
  snapshot <- full |>
    filter(.data[[get_report_date(full)]] <= config$now) |>
    change_now(now = config$now)

  # The stratified case, on the same data. `denguedat` carries `gender`, which
  # the pooled object above summed away, so it is rebuilt here declaring it.
  stratified_full <- denguedat |>
    filter(onset_week >= as.Date("2008-01-01")) |>
    count(.data$onset_week, .data$report_week, .data$gender, name = ".n") |>
    tbl_now(
      event_date = "onset_week", report_date = "report_week", case_count = ".n",
      strata = "gender", data_type = "count-incidence",
      event_units = "weeks", report_units = "weeks", verbose = FALSE
    )
  stratified_snapshot <- stratified_full |>
    filter(.data$report_week <= config$now) |>
    change_now(now = config$now)
  set.seed(fit_seed(epidemic, "display_strata"))
  stratified <- suppressWarnings(suppressMessages(run_nowcast(
    capped(stratified_snapshot, config), "baselinenowcast",
    draws = 1000, quantile_levels = QUANTILES, verbose = FALSE
  )))

  baseline <- fit_member("baselinenowcast", snapshot, config, epidemic, "display")
  set.seed(fit_seed(epidemic, "display_dnc"))
  dnc <- fit_member("diseasenowcasting", snapshot, config, epidemic, "display")

  ensemble <- nowcast_ensemble(
    baselinenowcast = baseline, diseasenowcasting = dnc,
    type = "quantile", verbose = FALSE
  )

  printed <- function(object) {
    from_message <- NULL
    from_stdout <- utils::capture.output(
      from_message <- utils::capture.output(print(object), type = "message")
    )
    if (length(from_stdout)) c(from_stdout, from_message) else from_message
  }

  list(
    stratified_print = printed(stratified),
    stratified_tidy = tidy(stratified),
    stratified_quantiles = tibble::as_tibble(stratified),
    baseline_print = printed(baseline),
    baseline_tidy = tidy(baseline),
    baseline_quantiles = tibble::as_tibble(baseline),
    baseline_draws = tibble::as_tibble(baseline, type = "draws"),
    ensemble_print = printed(ensemble),
    ensemble_tidy = tidy(ensemble),
    truth = nowcast_truth(full),
    now = config$now,
    # The fan chart is redrawn in the article from these, so the plot on the page
    # is the plot of the cached fit rather than a picture of a different one.
    ensemble_predictions = ensemble@predictions,
    event_date = ensemble@event_date
  )
}

# -- driver --------------------------------------------------------------------

selected <- commandArgs(trailingOnly = TRUE)
selected <- selected[nzchar(selected)]
if (!length(selected)) selected <- names(EPIDEMICS)
unknown <- setdiff(selected, names(EPIDEMICS))
if (length(unknown)) {
  stop("unknown epidemic(s): ", paste(unknown, collapse = ", "),
       "\nknown: ", paste(names(EPIDEMICS), collapse = ", "))
}

incremental <- !identical(sort(selected), sort(names(EPIDEMICS)))
previous <- if (incremental && file.exists(OUT)) readRDS(OUT) else NULL
if (incremental && is.null(previous)) {
  stop("cannot merge ", paste(selected, collapse = ", "), " into a file that ",
       "does not exist yet -- run every epidemic once first")
}

results <- lapply(stats::setNames(selected, selected), run_epidemic)

forecasts <- bind_rows(lapply(results, `[[`, "forecasts"))
weights <- lapply(results, `[[`, "weights")
backtests <- bind_rows(lapply(results, `[[`, "backtest"), .id = "epidemic")
metadata <- lapply(results, function(r) r[c("now", "window", "n_members")])

if (incremental) {
  keep <- setdiff(unique(previous$forecasts$epidemic), selected)
  forecasts <- bind_rows(filter(previous$forecasts, .data$epidemic %in% keep), forecasts)
  backtests <- bind_rows(filter(previous$backtests, .data$epidemic %in% keep), backtests)
  weights <- utils::modifyList(previous$weights, weights)
  metadata <- utils::modifyList(previous$metadata, metadata)
  display <- previous$display
} else {
  display <- capture_display("dengue")
}

saveRDS(
  list(
    forecasts = forecasts,
    weights = weights,
    backtests = backtests,
    metadata = metadata,
    display = display,
    members = MEMBERS,
    built = Sys.Date()
  ),
  OUT
)

message("wrote ", OUT, " (", nrow(forecasts), " rows)")
