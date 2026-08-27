# ensemble_comparison ----------------------------------------------------------
#
# Fits every member of the ensemble article on three very different epidemics,
# builds the ensembles, and saves everything the article displays.
#
# Run with:  Rscript data-raw/ensemble_comparison.R
# Output:    vignettes/articles/ensemble-comparison.rds
#
# IMPORTANT -- keep this file and the article in step. The calls below are the
# calls the article shows. The article displays them with `eval = FALSE` and
# reads the results from this file, because the Stan and JAGS fits are far too
# slow to run on every build. Change a call here, change it there.
#
# WHAT THIS BUILDS, AND ONLY THIS. Everything below feeds one of the article's
# chunks. The script used to fit THREE epidemics (dengue, mpox, covid) and score
# every member on each, but no chunk in the article ever read that table -- it
# was roughly two thirds of the run time producing numbers nobody saw. If you
# want the cross-epidemic comparison back, it belongs in an article that shows
# it; a precompute with no reader is a precompute that silently rots.
#
# APPROXIMATE INFERENCE, ON PURPOSE. See `member_engine()` below.

library(dplyr)
devtools::load_all(".", quiet = TRUE)

OUT <- "vignettes/articles/ensemble-comparison.rds"
QUANTILES <- nowcast_quantile_levels()

# One `set.seed()` at the top of a script is not reproducibility: it only pins
# anything if every engine consumes the same random numbers in the same order,
# so refitting a SUBSET lands somewhere else entirely. Everything below is
# seeded per (model, label) immediately before the fit, and the backtests get
# the same treatment through `nowcast_backtest(seed = )`.
BASE_SEED <- 20260824L

fit_seed <- function(...) BASE_SEED + sum(utf8ToInt(paste(..., sep = "|")))

# -- the three epidemics -------------------------------------------------------
#
# `now` is chosen so that later reports still exist to score against, and
# `horizon` is how many periods back from `now` the comparison covers: older
# dates are complete by then, so scoring them measures nothing.

data(denguedat)

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

# The article's data: dengue in Puerto Rico, a weekly line list, nowcast as of a
# date far enough in the past that later reports exist to score against.
CONFIG <- list(
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
)

# -- the members ---------------------------------------------------------------
#
# Each is an `engine()` carrying the arguments this data needs. They are the
# engines the article shows.
#
# APPROXIMATE INFERENCE ON PURPOSE. Both Stan back-ends run a fast approximation
# rather than full MCMC: `epinowcast` through `enw_pathfinder()`, `EpiNow2`
# through `stan_opts(method = "pathfinder")`. This article is about the ENSEMBLE
# machinery -- how members combine, how weights are learned, how the parts
# compare with the whole -- and none of that is a statement about any one
# model's posterior. Full sampling turned this script into an overnight job for
# numbers nobody reads to three significant figures. The article says so, so a
# reader does not mistake these fits for tuned ones.

MEMBERS <- c("baselinenowcast", "diseasenowcasting", "NobBS")

member_engine <- function(name, config, x, label = NULL) {
  # `moving_window` counts EVENT PERIODS, and NobBS pads its grid backwards to
  # fill a window longer than the data it was handed -- returning ZERO for every
  # date, with no error. That reads as a catastrophic score rather than as the
  # misconfiguration it is, so the window is capped at the history available.
  periods <- length(unique(x[[get_event_date(x)]]))
  common <- list(quantile_levels = QUANTILES, label = label)

  switch(name,
    # Delays are capped rather than trimmed: one long straggler gives the
    # reporting triangle a column per day of it, at no modelling benefit.
    baselinenowcast = do.call(engine_baselinenowcast, c(list(draws = 1000), common)),
    diseasenowcasting = do.call(engine_diseasenowcasting, common),
    NobBS = do.call(engine_nobbs, c(list(
      max_D = config$max_delay,
      moving_window = min(8L * config$horizon, periods)
    ), common)),
    epinowcast = do.call(engine_epinowcast, c(list(
      preprocess_args = list(max_delay = config$max_delay),
      # Pathfinder rather than `enw_sample()`: minutes instead of hours.
      fit = epinowcast::enw_fit_opts(
        sampler = epinowcast::enw_pathfinder, draws = 1000, seed = BASE_SEED
      ),
      # The slowest engine here, and it scales with the number of REFERENCE
      # dates, so it gets a window where the others get the whole series. This
      # is exactly what `min_date` on an engine is for.
      min_date = 12L * config$horizon
    ), common)),
    surveillance = do.call(engine_surveillance, c(list(
      D = config$max_delay,
      control = list(N.tInf.max = 100000, nSamples = 500)
    ), common)),
    EpiNow2 = do.call(engine_epinow2, c(list(
      generation_time = EpiNow2::gt_opts(EpiNow2::example_generation_time),
      delays = EpiNow2::delay_opts(EpiNow2::example_incubation_period),
      # Laplace rather than sampling: same reason as epinowcast above.
      stan = EpiNow2::stan_opts(method = "pathfinder", backend = "cmdstanr", samples = 500),
      min_date = 12L * config$horizon
    ), common)),
    engine(name, quantile_levels = QUANTILES, label = label)
  )
}

# Cap the delay axis for every engine, so they are all shown the same data.
capped <- function(x, config) {
  filter(x, .data$.delay <= config$max_delay)
}

fit_member <- function(name, snapshot, config, label = "now") {
  message("  - ", name, " [", label, "] ...", appendLF = FALSE)
  started <- Sys.time()
  set.seed(fit_seed(name, label))
  data <- capped(snapshot, config)
  out <- tryCatch(
    suppressWarnings(suppressMessages(
      run_nowcast(data, member_engine(name, config, data), verbose = FALSE)
    )),
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

check_plausible <- function(nowcast, observed, name) {
  if (is.null(nowcast)) return(NULL)
  ceiling_ <- IMPLAUSIBLE_FACTOR * max(observed, na.rm = TRUE)
  worst <- max(nowcast@predictions$.value, na.rm = TRUE)
  if (worst > ceiling_) {
    stop(sprintf(
      paste0("%s did not converge: largest predicted quantile is %s against ",
             "an observed maximum of %s (>%dx). Refusing to cache it."),
      name, format(round(worst), big.mark = ","),
      format(max(observed, na.rm = TRUE), big.mark = ","), IMPLAUSIBLE_FACTOR
    ))
  }
  nowcast
}

# -- the backtest and the weights it learns ------------------------------------
#
# The ONLY thing this stage produces is the retrospective scores the article
# shows and the weights derived from them. It used to also score every member on
# a comparison window and cache the result as `forecasts`; nothing read it.

run_backtest <- function(config) {
  message("== backtest ==")
  full <- config$build()
  now <- config$now

  # Three retrospective dates, one horizon apart, all far enough back that the
  # truth had settled. This is `length(engines) x length(now_dates)` model fits
  # and is the expensive part of the script.
  step <- if (identical(config$units, "weeks")) 7L else 1L
  backtest_dates <- now - step * config$horizon * (3:1)
  message("  at ", paste(format(backtest_dates), collapse = ", "))

  data <- capped(full, config)
  backtest <- tryCatch(
    suppressWarnings(suppressMessages(nowcast_backtest(
      data,
      lapply(stats::setNames(MEMBERS, MEMBERS), member_engine,
             config = config, x = data),
      now_dates = backtest_dates,
      seed = BASE_SEED, verbose = FALSE
    ))),
    error = function(e) {
      message("  backtest FAILED: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(backtest)) {
    return(list(backtest = NULL, weights = NULL))
  }

  # An engine asked for a longer window than it was given data for pads its grid
  # and returns ZERO everywhere, with no error. Against a non-zero truth that
  # reads as a catastrophic score rather than as a broken call.
  flat <- backtest$predictions |>
    filter(.data$.quantile_level == 0.5) |>
    summarise(all_zero = all(.data$.value == 0), .by = ".method") |>
    filter(.data$all_zero)
  if (nrow(flat)) {
    stop(paste(flat$.method, collapse = ", "),
         " predicted zero for every date and quantile in the backtest.",
         " That is a misconfigured call, not a bad model. Refusing to cache it.")
  }

  list(backtest = tidy(backtest), weights = nowcast_weights(backtest, "inverse_score"))
}

# -- what the article prints ---------------------------------------------------
#
# The article shows `run_nowcast()`, `tidy()` and `autoplot()` on ONE fit. Rather
# than ship the fitted objects (Stan fits are large and version-fragile), capture
# exactly what the article displays and replay it there.

capture_display <- function(config) {
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
  set.seed(fit_seed("display_strata"))
  stratified <- suppressWarnings(suppressMessages(run_nowcast(
    capped(stratified_snapshot, config),
    engine_baselinenowcast(draws = 1000, quantile_levels = QUANTILES),
    verbose = FALSE
  )))

  # -- the engine cards --------------------------------------------------------
  #
  # ONE fit per engine, and the SAME fits are the ensemble's members, so the
  # article's "the members below are the fits from the cards above" is true by
  # construction rather than by a comment asking someone to keep it true.
  card_engines <- c(
    "baselinenowcast", "diseasenowcasting", "epinowcast",
    "NobBS", "surveillance", "EpiNow2"
  )
  cards <- lapply(stats::setNames(card_engines, card_engines), function(name) {
    fit_member(name, snapshot, config, "display")
  })
  cards <- cards[!vapply(cards, is.null, logical(1))]
  if (length(cards) < 2) {
    stop("fewer than two display fits succeeded; nothing to ensemble")
  }

  # The SAME package, a different epidemic process -- the article's example of
  # why an ensemble is not only a hedge across packages.
  if ("diseasenowcasting" %in% names(cards)) {
    set.seed(fit_seed("display_dnc_ar1"))
    ar1 <- tryCatch(
      suppressWarnings(suppressMessages(run_nowcast(
        capped(snapshot, config),
        engine_diseasenowcasting(
          model = diseasenowcasting::model(
            epidemic = diseasenowcasting::ar1_epidemic()
          ),
          quantile_levels = QUANTILES, label = "dnc_ar1"
        ),
        verbose = FALSE
      ))),
      error = function(e) {
        message("  dnc_ar1 FAILED: ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(ar1)) cards[["dnc_ar1"]] <- ar1
  }

  # `surveillance` is a CARD but not an ENSEMBLE MEMBER, and the reason is
  # arithmetic rather than taste. It reports a fixed 95% interval
  # ({0.025, 0.5, 0.975}) while EpiNow2 reports a fixed 90% one
  # ({0.05, 0.5, 0.95}); neither can be re-summarised after the fit, so an
  # ensemble holding both shares only the MEDIAN and collapses to a single
  # quantile level with no fan to draw. Dropping it leaves {0.05, 0.5, 0.95},
  # which every other member carries. The article says the same thing in words.
  ensemble_members <- cards[setdiff(names(cards), "surveillance")]
  ensemble <- nowcast_ensemble(ensemble_members, type = "quantile", verbose = FALSE)

  shared_levels <- sort(unique(ensemble@predictions$.quantile_level))
  if (length(shared_levels) < 3) {
    stop("the ensemble shares only ", length(shared_levels), " quantile level(s) (",
         paste(shared_levels, collapse = ", "), "): there is no symmetric pair ",
         "left to draw a fan from. Check which members report a FIXED interval ",
         "width -- they cannot be re-summarised, so two different fixed widths ",
         "intersect at the median alone.")
  }

  printed <- function(object) {
    from_message <- NULL
    from_stdout <- utils::capture.output(
      from_message <- utils::capture.output(print(object), type = "message")
    )
    if (length(from_stdout)) c(from_stdout, from_message) else from_message
  }

  # Every member's predictions, plus the ensemble's, in ONE long frame keyed by
  # `model`. This is what the article's "the ensemble against its parts" figure
  # draws, and caching the predictions rather than the plot means the figure is
  # of these fits and not of a differently configured run.
  member_predictions <- bind_rows(
    lapply(ensemble_members, function(nowcast) nowcast@predictions),
    .id = "model"
  )
  ensemble_predictions <- ensemble@predictions

  list(
    stratified_print = printed(stratified),
    stratified_tidy = tidy(stratified),
    stratified_quantiles = tibble::as_tibble(stratified),
    baseline_print = printed(cards[["baselinenowcast"]]),
    baseline_tidy = tidy(cards[["baselinenowcast"]]),
    baseline_quantiles = tibble::as_tibble(cards[["baselinenowcast"]]),
    baseline_draws = tibble::as_tibble(cards[["baselinenowcast"]], type = "draws"),
    baseline_predictions = cards[["baselinenowcast"]]@predictions,
    baseline_score = score_nowcast(cards[["baselinenowcast"]], truth = full),
    card_dnc = if (!is.null(cards$diseasenowcasting)) printed(cards$diseasenowcasting),
    card_enw = if (!is.null(cards$epinowcast)) printed(cards$epinowcast),
    card_nobbs = if (!is.null(cards$NobBS)) printed(cards$NobBS),
    card_sur = if (!is.null(cards$surveillance)) printed(cards$surveillance),
    card_en2 = if (!is.null(cards$EpiNow2)) printed(cards$EpiNow2),
    ensemble_print = printed(ensemble),
    ensemble_tidy = tidy(ensemble),
    members = names(cards),
    ensemble_members = names(ensemble_members),
    truth = tbl.now:::.eventual_counts(full),
    now = config$now,
    # The figures are redrawn in the article from these, so the plots on the page
    # are plots of the cached fits rather than pictures of different ones.
    member_predictions = member_predictions,
    ensemble_predictions = ensemble_predictions,
    event_date = ensemble@event_date
  )
}

# -- driver --------------------------------------------------------------------

display <- capture_display(CONFIG)
scored <- run_backtest(CONFIG)

backtest_tidy <- scored$backtest
backtest_summary <- if (is.null(backtest_tidy)) {
  NULL
} else {
  backtest_tidy |>
    summarise(
      mean_wis = round(mean(.data$wis, na.rm = TRUE), 2),
      mean_ae_median = round(mean(.data$ae_median, na.rm = TRUE), 2),
      coverage_90 = round(mean(.data$coverage_90, na.rm = TRUE), 2),
      .by = "method"
    ) |>
    arrange(.data$mean_wis)
}

# Every key here is read by a chunk of the article. Adding one that nothing reads
# is how this file and the article drifted apart once already: the script was
# rewritten and kept writing `forecasts`, while the article went on reading
# `backtest_tidy`, which nothing wrote any more.
saveRDS(
  list(
    display = display,
    backtest_tidy = backtest_tidy,
    backtest_summary = backtest_summary,
    article_weights = scored$weights,
    members = MEMBERS,
    built = Sys.Date(),
    source = "vignettes/articles/ensemble-nowcasting.Rmd"
  ),
  OUT
)

message("wrote ", OUT, " (", format(file.size(OUT) / 1024^2, digits = 3), " MB)")
