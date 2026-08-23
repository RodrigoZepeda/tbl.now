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
  # The article prints `dnc_fit` and then calls `tidy(dnc_fit)`, so tidy the FIT
  # -- that is the call on the page. From diseasenowcasting 2.1.0 the fit has its
  # own method, which delegates to `predict()`; the resulting table is the same
  # one `tidy(predict(fit))` gives, bar the sampling noise in `predict()`.
  attr(out, "fit")     <- fit
  attr(out, "tidy_on") <- fit
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

  # The article's nowcaster section shows an UNSTRATIFIED fit and then prints
  # `tidy(nowcaster_fit)`. Displaying the stratified fit's tidy there would show
  # a table the reader's own code cannot produce (one row per stratum, labelled
  # with the numeric codes the converter assigned), so the display comes from a
  # second, unstratified fit matching the code on the page. The stratified fit
  # above still supplies the per-stratum panels.
  message("  - nowcaster (unstratified, for display) ...", appendLF = FALSE)
  nowcaster_plain <- tryCatch(
    suppressWarnings(suppressMessages(nowcaster::nowcasting_inla(
      dataset      = tbl_now_to_nowcaster(objects[["Total"]], verbose = FALSE),
      date_onset   = date_onset,
      date_report  = date_report,
      data.by.week = TRUE,
      use.epiweek  = FALSE,
      Dmax         = N_HORIZON,
      wdw          = TRIM,
      silent       = TRUE
    ))),
    error = function(e) {
      message(" FAILED: ", conditionMessage(e))
      NULL
    }
  )
  if (!is.null(nowcaster_plain)) {
    message(" ok")
    display[["nowcaster"]] <- capture_display(nowcaster_plain, nowcaster_plain)
  }
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
    seasonal_fit, seasonal_fit
  )
  message("  ok")
}

# -- epidist: the delay distribution, fitted vs observed ------------------------
# epidist does not nowcast, so there is no per-date estimate to compare. What it
# DOES produce is a reporting-delay distribution, and that can be checked against
# the delays actually seen in the data.
#
# The LATENT model is used, not the marginal one: with epidist 0.4.0 and
# primarycensored 1.5.1 the marginal model fails to compile (its generated Stan
# code calls `primarycensored_lpmf` with 8 arguments against a 9-argument
# signature -- the `L` lower-truncation argument is missing). The latent model
# has no such problem and estimates the same quantity, so the article uses it.
#
# The latent model carries one latent variable per observation, so it is trimmed
# to the last `TRIM` weeks; a delay distribution does not need twelve years of
# data to be characterised.
EPIDIST_ITER <- 1000L
EPIDIST_MAX_WEEKS <- 8L

epidist_window <- dengue |> filter(onset_week >= CUT - 7 * TRIM)

fit_epidist_latent <- function(data, formula) {
  obj <- make_now(data)
  if (!is.null(formula) && "gender" %in% names(data)) {
    obj <- add_strata(obj, "gender")
  }
  converted <- suppressWarnings(suppressMessages(
    tbl_now_to_epidist(obj, verbose = FALSE)
  ))
  fit <- suppressWarnings(suppressMessages(epidist::epidist(
    epidist::as_epidist_latent_model(converted),
    formula = formula %||% (mu ~ 1),
    chains = 2, iter = EPIDIST_ITER, refresh = 0, silent = 2, seed = 1
  )))

  # brms swallows a Stan data-initialisation failure and hands back a fit with
  # no draws, so an unchecked call reports "ok" for a model that never ran.
  if (length(brms::as_draws(fit)) == 0L || brms::ndraws(fit) == 0L) {
    stop("epidist fit produced no posterior draws", call. = FALSE)
  }
  fit
}

# Fitted P(delay falls in week w), integrating the lognormal over each week.
# Weekly data means a delay of "0 weeks" covers [0, 7) days, so the comparison
# has to be made on bins, not on a continuous density.
#
# `predict_delay_parameters()` needs `newdata` to carry every model variable,
# the response included -- a bare `data.frame(gender = "Male")` is rejected with
# "Response variables must be specified in 'newdata'". Passing representative
# ROWS OF THE FIT'S OWN DATA satisfies that; the returned `index` column says
# which row each draw belongs to.
epidist_representative_rows <- function(fit, by = NULL) {
  model_data <- fit$data
  if (is.null(by)) {
    return(model_data[1L, , drop = FALSE])
  }
  model_data[!duplicated(model_data[[by]]), , drop = FALSE]
}

# The observed quantity is a WEEK-INDEX DIFFERENCE, week(report) - week(onset),
# not the delay in days binned into weeks. Those are different things: the onset
# time is uniform inside its week, so a case whose true delay is D lands in week
# `floor((p + D) / 7)` with `p ~ U(0, 7)`. Comparing the fitted lognormal
# directly against the week counts overstates week 0 badly (0.24 against an
# observed 0.03) -- it credits week 0 with every delay under seven days, when
# only those from a case whose onset fell early in the week can land there.
#
# So push the fitted distribution through that same mapping before comparing,
# integrating over the onset position rather than sampling it.
epidist_week_probs <- function(mu, sigma, weeks, n_grid = 101L) {
  onset_grid <- seq(0, 7, length.out = n_grid)
  vapply(weeks, function(w) {
    per_position <- vapply(onset_grid, function(p) {
      upper <- 7 * (w + 1) - p
      lower <- 7 * w - p
      hi <- ifelse(upper > 0, stats::plnorm(upper, mu, sigma), 0)
      lo <- ifelse(lower > 0, stats::plnorm(lower, mu, sigma), 0)
      mean(hi - lo)
    }, numeric(1))
    mean(per_position)
  }, numeric(1))
}

epidist_fitted_bins <- function(fit, rows, labels) {
  draws <- epidist::predict_delay_parameters(fit, newdata = rows)
  weeks <- 0:EPIDIST_MAX_WEEKS

  per_stratum <- lapply(seq_along(labels), function(i) {
    dd <- draws[draws$index == i, , drop = FALSE]

    # One row per posterior draw, so the interval reflects the posterior rather
    # than the Monte Carlo noise of a single simulation.
    by_draw <- vapply(
      seq_len(nrow(dd)),
      function(j) epidist_week_probs(dd$mu[j], dd$sigma[j], weeks),
      numeric(length(weeks))
    )

    tibble(
      stratum = labels[i],
      delay   = weeks,
      fitted  = apply(by_draw, 1, stats::median),
      lower   = apply(by_draw, 1, stats::quantile, probs = 0.025),
      upper   = apply(by_draw, 1, stats::quantile, probs = 0.975)
    )
  })
  bind_rows(per_stratum)
}

# The delays actually observed, on the same bins.
epidist_observed_bins <- function(data, label) {
  delays <- as.numeric(difftime(data$report_week, data$onset_week, units = "days")) / 7
  delays <- delays[delays >= 0 & delays <= EPIDIST_MAX_WEEKS]
  counts <- table(factor(round(delays), levels = 0:EPIDIST_MAX_WEEKS))
  tibble(
    stratum  = label,
    delay    = 0:EPIDIST_MAX_WEEKS,
    observed = as.numeric(counts) / sum(counts)
  )
}

message("== epidist (delay distribution) ==")
epidist_result <- tryCatch({
  message("  - total ...", appendLF = FALSE)
  fit_total <- fit_epidist_latent(epidist_window, NULL)
  message(" ok")

  message("  - by gender ...", appendLF = FALSE)
  fit_gender <- fit_epidist_latent(epidist_window, mu ~ 1 + gender)
  message(" ok")

  gender_rows <- epidist_representative_rows(fit_gender, by = "gender")
  levels_gender <- as.character(gender_rows$gender)

  fitted <- bind_rows(
    epidist_fitted_bins(
      fit_total, epidist_representative_rows(fit_total), "Total"
    ),
    epidist_fitted_bins(fit_gender, gender_rows, levels_gender)
  )
  observed <- bind_rows(
    epidist_observed_bins(epidist_window, "Total"),
    bind_rows(lapply(levels_gender, function(g) {
      epidist_observed_bins(epidist_window |> filter(gender == g), g)
    }))
  )

  stopifnot(setequal(unique(fitted$stratum), unique(observed$stratum)))

  list(
    tidy_total  = suppressWarnings(suppressMessages(tbl.now::tidy(fit_total))),
    tidy_gender = suppressWarnings(suppressMessages(tbl.now::tidy(fit_gender))),
    fitted      = fitted,
    observed    = observed,
    n_cases     = nrow(epidist_window),
    trim_weeks  = TRIM
  )
}, error = function(e) {
  message("  FAILED: ", conditionMessage(e))
  NULL
})
if (!is.null(epidist_result)) {
  display[["epidist"]] <- epidist_result
  message("  ok")
}

# -- NobBS with user-chosen quantiles ------------------------------------------
# `tidy(probs =)` is refused for NobBS because the fit keeps no draws to
# re-summarise. NobBS can compute any quantiles you like, but they have to be
# ASKED FOR at fit time, via `specs$quantiles`; the article shows how.
message("== NobBS (custom quantiles, for display) ==")
nobbs_q <- tryCatch(
  suppressWarnings(suppressMessages(NobBS::NobBS(
    data          = as.data.frame(objects[["Total"]]),
    now           = get_now(objects[["Total"]]),
    units         = "1 week",
    onset_date    = get_event_date(objects[["Total"]]),
    report_date   = get_report_date(objects[["Total"]]),
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
