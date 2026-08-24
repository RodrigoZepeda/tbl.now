# tidy() for fitted nowcasts ----------------------------------------------------
#
# One shape of answer, whatever engine produced it. The converters normalise what
# goes INTO a nowcasting package; these methods normalise what comes OUT.

#' Tidy a fitted nowcast into one standard table
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Every nowcasting package returns its answer in its own shape -- a matrix of
#' posterior draws, an `stsNC` object, a Stan fit, an INLA summary, a bare list.
#' `tidy()` turns any of them into the **same** table, so downstream code
#' (plotting, scoring, comparison) does not care which engine produced it.
#'
#' @section Value:
#'
#' A [tibble][tibble::tibble] with one row per event date (per stratum, where the
#' fit carries strata) and these columns:
#'
#' \describe{
#'   \item{`event_date`}{`Date`. The event/reference date, **on the engine's own
#'     grid**. `tidy()` deliberately does not re-grid: some packages bin onto
#'     week starts of their own choosing, and silently snapping them would hide a
#'     real difference. Align afterwards if you need to.}
#'   \item{`stratum`}{`character`. One label per stratum the fit reports, and
#'     `"all"` when the fit is unstratified. Several stratifying columns are
#'     pasted `" | "`-separated, matching the `triangle_list` naming of
#'     [tbl_now_to_baselinenowcast()]. `(stratum, event_date)` is therefore a
#'     unique key.}
#'   \item{`estimate`}{`numeric`. The point nowcast -- the posterior median where
#'     the engine provides draws or a median, otherwise its point estimate.}
#'   \item{`conf.low`, `conf.high`}{`numeric`. Interval bounds, following
#'     \pkg{broom}'s naming. `NA` when the engine returns no interval.}
#'   \item{`level`}{`numeric`. The width the interval actually has, e.g. `0.95`.
#'     Engines differ -- \pkg{epinowcast} reports a 90% band by default while
#'     others report 95% -- and without this column those get compared as if they
#'     were the same thing. `NA` whenever the width cannot be established:
#'     because the engine returned no interval (a \pkg{baselinenowcast} fit made
#'     with `output_type = "point"`), or because it returned one without saying
#'     how wide it is. \pkg{NobBS} is the latter case -- its `lower`/`upper`
#'     come from `specs$conf`, and `NobBS()` does not return `specs` -- so pass
#'     `level` yourself if you need it filled in. A guessed default is worse than
#'     `NA` in the one column that exists to stop widths being compared blindly.}
#'   \item{`engine`}{`character`. Which package produced the fit.}
#' }
#'
#' When `probs` is supplied, one extra column per requested quantile is appended,
#' named `q5`, `q50`, `q95` and so on (the probability times 100, so `0.025`
#' becomes `q2.5`).
#'
#' @section Which engines can honour `probs`:
#'
#' Only the engines that expose **draws** can compute an arbitrary quantile:
#' \pkg{diseasenowcasting}, \pkg{baselinenowcast} and \pkg{epinowcast}. The
#' others report a fixed set of summaries and nothing else, so asking them for a
#' quantile they did not compute is an error rather than a silent approximation.
#'
#' @param x A fitted nowcast. See *Supported objects*.
#' @param probs Optional numeric vector of probabilities in `[0, 1]`. Adds a
#'   `q*` column per probability. Only available for engines that expose draws.
#' @param engine Optional string naming the engine. Needed only for the shapes
#'   that arrive as an **unclassed list** -- a \pkg{NobBS} fit, or a list of
#'   per-stratum \pkg{baselinenowcast} fits -- which are otherwise recognised by
#'   their structure.
#' @param level Interval width to report for an engine that does not say what it
#'   produced. Only used by the \pkg{NobBS} branch (see `level` under *Value*);
#'   `NULL`, the default, reports `NA`.
#' @param ... Passed to methods.
#'
#' @section Supported objects:
#'
#' * `nowcast_prediction` (S7) from `diseasenowcasting::predict()`
#' * `baselinenowcast_df` from `baselinenowcast::baselinenowcast()`
#' * `epinowcast` fits
#' * `stsNC` from `surveillance::nowcast()`
#' * the list returned by `NobBS::NobBS()` or by `NobBS::NobBS.strat()` (the
#'   stratified variant is recognised by its `stratum` column)
#' * a **list of `baselinenowcast_df` fits**, one per stratum -- what
#'   `lapply()`-ing over a [tbl_now_triangle_list] produces. Each element is
#'   tidied and labelled with its list name, giving the same
#'   one-block-per-stratum table the natively stratified engines return.
#'
#' @return A tibble, as described above.
#'
#' @examplesIf requireNamespace("baselinenowcast", quietly = TRUE)
#' data(denguedat)
#' # A few years of data and a small number of draws, to keep the example quick.
#' dengue <- tbl_now(denguedat[1:10000, ],
#'   event_date = "onset_week", report_date = "report_week", verbose = FALSE
#' )
#' triangle <- suppressWarnings(
#'   tbl_now_to_baselinenowcast(dengue, verbose = FALSE)
#' )
#' fit <- baselinenowcast::baselinenowcast(
#'   triangle, output_type = "samples", draws = 25
#' )
#' tidy(fit)
#'
#' @name tidy.nowcast
NULL

#' @rdname tidy.nowcast
#' @importFrom generics tidy
#' @export
generics::tidy

# -- shared helpers -------------------------------------------------------------

#' Assemble the standard tidy-nowcast table
#'
#' @param event_date,estimate,conf.low,conf.high Vectors of equal length.
#' @param level Interval width actually returned.
#' @param engine Name of the producing package.
#' @param stratum Stratum label.
#' @param quantiles Optional named list of quantile vectors.
#'
#' @return A tibble in the documented column order.
#'
#' @keywords internal
#' @noRd
.tidy_nowcast_frame <- function(event_date, estimate, conf.low, conf.high,
                                level, engine, stratum = "all",
                                quantiles = NULL) {
  out <- tibble::tibble(
    event_date = as.Date(event_date),
    stratum    = as.character(stratum),
    estimate   = as.numeric(estimate),
    conf.low   = as.numeric(conf.low),
    conf.high  = as.numeric(conf.high),
    level      = as.numeric(level),
    engine     = as.character(engine)
  )
  if (!is.null(quantiles)) out <- dplyr::bind_cols(out, tibble::as_tibble(quantiles))
  out[order(out$stratum, out$event_date), ]
}

#' Name a quantile column the way the documentation promises
#'
#' `0.05` becomes `"q5"`, `0.025` becomes `"q2.5"`.
#'
#' @param probs Numeric vector of probabilities.
#'
#' @return Character vector of column names.
#'
#' @keywords internal
#' @noRd
.tidy_quantile_names <- function(probs) {
  paste0("q", format(probs * 100, trim = TRUE, scientific = FALSE, drop0trailing = TRUE))
}

#' Compute requested quantiles from a draws matrix
#'
#' @param draws Numeric matrix, draws x event times.
#' @param probs Numeric vector of probabilities, or `NULL`.
#'
#' @return A named list of numeric vectors, or `NULL`.
#'
#' @keywords internal
#' @noRd
.tidy_quantiles_from_draws <- function(draws, probs) {
  if (is.null(probs) || length(probs) == 0L) return(NULL)
  .assert_probs(probs)
  values <- lapply(probs, function(p) unname(apply(draws, 2, stats::quantile, probs = p)))
  stats::setNames(values, .tidy_quantile_names(probs))
}

#' Reject probabilities an engine cannot compute
#'
#' @param probs Numeric vector.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
.assert_probs <- function(probs) {
  if (!is.numeric(probs) || anyNA(probs) || any(probs < 0 | probs > 1)) {
    cli::cli_abort("{.arg probs} must be numbers between 0 and 1.")
  }
  invisible(NULL)
}

#' Abort when `probs` is asked of an engine that keeps no draws
#'
#' @param probs Numeric vector or `NULL`.
#' @param engine Engine name, for the message.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
.reject_probs <- function(probs, engine) {
  if (!is.null(probs) && length(probs) > 0L) {
    cli::cli_abort(c(
      "{.pkg {engine}} does not keep posterior draws, so {.arg probs} cannot be \\
       honoured.",
      "i" = "It reports a fixed set of summaries; returning anything else would \\
             be an approximation dressed up as a quantile.",
      "i" = "Draws are available from {.pkg diseasenowcasting}, \\
             {.pkg baselinenowcast} and {.pkg epinowcast}."
    ))
  }
  invisible(NULL)
}

# -- methods --------------------------------------------------------------------

#' @rdname tidy.nowcast
#' @exportS3Method generics::tidy
tidy.baselinenowcast_df <- function(x, probs = NULL, ...) {
  draws_df <- as.data.frame(x)

  # `new_baselinenowcast_df()` always stamps `output_type`, asserted to be
  # "samples" or "point". A point fit holds ONE value per reference date, so
  # every quantile of it is that same value: quoting it as a 95% interval would
  # dress a point estimate up as a band that happens to have zero width.
  is_point <- identical(unique(draws_df$output_type), "point")
  if (is_point && !is.null(probs) && length(probs) > 0L) {
    cli::cli_abort(c(
      "This {.pkg baselinenowcast} fit was produced with \\
       {.code output_type = \"point\"}, so {.arg probs} cannot be honoured.",
      "i" = "It carries a single value per reference date; any quantile of \\
             that is the value itself.",
      "i" = "Refit with {.code output_type = \"samples\"} for draws."
    ))
  }
  .assert_probs(probs %||% 0.5)

  grouped <- split(draws_df$pred_count, as.Date(draws_df$reference_date))
  dates <- as.Date(names(grouped))

  if (is_point) {
    return(.tidy_nowcast_frame(
      event_date = dates,
      estimate   = vapply(grouped, stats::median, numeric(1)),
      conf.low   = NA_real_,
      conf.high  = NA_real_,
      level      = NA_real_,
      engine     = "baselinenowcast"
    ))
  }

  quantiles <- if (!is.null(probs)) {
    stats::setNames(
      lapply(probs, function(p) vapply(grouped, stats::quantile, numeric(1), probs = p)),
      .tidy_quantile_names(probs)
    )
  }

  .tidy_nowcast_frame(
    event_date = dates,
    estimate   = vapply(grouped, stats::median, numeric(1)),
    conf.low   = vapply(grouped, stats::quantile, numeric(1), probs = 0.025),
    conf.high  = vapply(grouped, stats::quantile, numeric(1), probs = 0.975),
    level      = 0.95,
    engine     = "baselinenowcast",
    quantiles  = quantiles
  )
}

#' Stratum labels for the rows of an \pkg{epinowcast} summary table
#'
#' A fit built with `by = "age_group"` reports one block per group, and
#' `summary()` carries both the `.group` index and the grouping columns
#' themselves. The *values* are the label a reader wants, so they are pasted --
#' `" | "` separated for several columns, matching
#' [tbl_now_to_baselinenowcast()]'s `triangle_list` naming.
#'
#' @param summary_table A `summary(fit, type = "nowcast"/"nowcast_samples")`
#'   table.
#' @param fit The `epinowcast` fit, whose `by` element names the grouping
#'   columns.
#'
#' @return A character vector, one label per row of `summary_table`; all
#'   `"all"` when the fit is unstratified.
#'
#' @keywords internal
#' @noRd
.epinowcast_stratum <- function(summary_table, fit) {
  by_cols <- intersect(unlist(fit$by), names(summary_table))
  if (length(by_cols) == 0L) {
    return(rep("all", nrow(summary_table)))
  }
  do.call(
    paste, c(unname(as.list(summary_table[by_cols])), sep = " | ")
  )
}

#' @rdname tidy.nowcast
#' @exportS3Method generics::tidy
tidy.epinowcast <- function(x, probs = NULL, ...) {
  .need_pkg("epinowcast")
  nowcast <- as.data.frame(summary(x, type = "nowcast"))
  stratum <- .epinowcast_stratum(nowcast, x)

  quantiles <- NULL
  if (!is.null(probs)) {
    .assert_probs(probs)
    samples <- as.data.frame(summary(x, type = "nowcast_samples"))
    # Index the split by the nowcast's OWN (stratum, reference_date) key. A
    # stratified fit repeats every reference date once per stratum, so keying on
    # the date alone would hand each date's quantiles to whichever stratum
    # `split()` happened to sort first.
    draws_by_cell <- split(
      samples$sample,
      paste(
        .epinowcast_stratum(samples, x), as.Date(samples$reference_date)
      )
    )
    key <- paste(stratum, as.Date(nowcast$reference_date))
    quantiles <- stats::setNames(
      lapply(probs, function(p) {
        vapply(
          draws_by_cell[key], stats::quantile, numeric(1),
          probs = p, USE.NAMES = FALSE
        )
      }),
      .tidy_quantile_names(probs)
    )
  }

  .tidy_nowcast_frame(
    event_date = as.Date(nowcast$reference_date),
    estimate   = nowcast$median,
    conf.low   = nowcast$q5,
    conf.high  = nowcast$q95,
    level      = 0.90,   # epinowcast's default band is q5-q95, NOT 95%
    engine     = "epinowcast",
    stratum    = stratum,
    quantiles  = quantiles
  )
}

#' @rdname tidy.nowcast
#' @exportS3Method generics::tidy
tidy.stsNC <- function(x, probs = NULL, ...) {
  .need_pkg("surveillance")
  .reject_probs(probs, "surveillance")
  estimate <- as.numeric(surveillance::upperbound(x))
  keep <- !is.na(estimate)

  # An `stsNC` carries its prediction interval in the `pi` slot: a
  # (time x unit x 2) array of lower/upper bounds. The Bayesian methods fill it,
  # `lawless` and `unif` may not, so fall back to NA rather than assume.
  bounds <- .stsNC_interval(x, keep)

  .tidy_nowcast_frame(
    event_date = as.Date(surveillance::epoch(x))[keep],
    estimate   = estimate[keep],
    conf.low   = bounds$conf.low,
    conf.high  = bounds$conf.high,
    level      = bounds$level,
    engine     = "surveillance"
  )
}

#' Pull the prediction interval off an `stsNC` object
#'
#' The width is whatever `nowcast()` was given as `control$alpha` (0.05 by
#' default, i.e. a 95% interval); the slot's third dimension is named with the
#' two quantiles, so it can be recovered from there when `alpha` is missing.
#'
#' @param x An `stsNC` object.
#' @param keep Logical vector selecting the rows with a non-`NA` estimate.
#'
#' @returns A list with `conf.low`, `conf.high` and `level`, all `NA` when the
#'   method produced no interval.
#'
#' @noRd
.stsNC_interval <- function(x, keep) {
  empty <- list(
    conf.low  = rep(NA_real_, sum(keep)),
    conf.high = rep(NA_real_, sum(keep)),
    level     = NA_real_
  )

  pi <- methods::slot(x, "pi")
  if (is.null(pi) || length(dim(pi)) != 3L || all(is.na(pi))) {
    return(empty)
  }

  alpha <- x@control$alpha
  level <- if (is.numeric(alpha) && length(alpha) == 1L) {
    1 - alpha
  } else {
    # e.g. c("2.5%", "97.5%") -> 0.95.
    tails <- suppressWarnings(
      as.numeric(sub("%$", "", dimnames(pi)[[3]])) / 100
    )
    if (length(tails) == 2L && !anyNA(tails)) diff(tails) else NA_real_
  }

  list(
    conf.low  = as.numeric(pi[keep, 1L, 1L]),
    conf.high = as.numeric(pi[keep, 1L, 2L]),
    level     = level
  )
}

#' Tidy a diseasenowcasting prediction
#'
#' Registered dynamically in `.onLoad()`, not with `@exportS3Method`: the object
#' is an S7 class whose `class()` is `"diseasenowcasting::nowcast_prediction"`,
#' and a `::` cannot appear in an S3 method NAME, so the usual
#' `tidy.<class>` convention cannot express it.
#'
#' @inheritParams tidy.nowcast
#'
#' @return A tibble, as for the other `tidy()` methods.
#'
#' @keywords internal
#' @noRd
tidy_nowcast_prediction <- function(x, probs = NULL, ...) {
  event_dates   <- S7::prop(x, "event_dates")
  strata_draws  <- S7::prop(x, "strata_draws")
  strata_levels <- S7::prop(x, "strata_levels")

  summarise_draws <- function(draws, stratum) {
    .tidy_nowcast_frame(
      event_date = event_dates,
      estimate   = apply(draws, 2, stats::median),
      conf.low   = apply(draws, 2, stats::quantile, probs = 0.025),
      conf.high  = apply(draws, 2, stats::quantile, probs = 0.975),
      level      = 0.95,
      engine     = "diseasenowcasting",
      stratum    = stratum,
      quantiles  = .tidy_quantiles_from_draws(draws, probs)
    )
  }

  # A stratified fit carries `strata_draws`: draws x event times x stratum.
  # Report one block per stratum, matching what the other engines do; the
  # pooled `draws` slot is only used when there are no strata.
  if (!is.null(strata_draws) && length(strata_levels) > 0L) {
    per_stratum <- lapply(seq_along(strata_levels), function(k) {
      summarise_draws(strata_draws[, , k, drop = TRUE], strata_levels[k])
    })
    return(dplyr::bind_rows(per_stratum))
  }

  summarise_draws(S7::prop(x, "draws"), "all")
}


# -- EpiNow2 --------------------------------------------------------------------
#
# Every EpiNow2 summary goes through `calc_summary_measures()`, which emits
# `median`, `mean`, `sd` and one `lower_<pct>` / `upper_<pct>` pair per requested
# credible interval. `CrIs` is a USER argument, so the widths present depend on
# how the fit was made -- reading them off the column names is the only way to
# report `level` honestly.

#' Pull the widest credible interval out of an \pkg{EpiNow2} summary
#'
#' @param summary_table A `calc_summary_measures()` result.
#'
#' @return A list with `conf.low`, `conf.high` and `level`; all `NA` when the
#'   summary carries no interval columns at all.
#'
#' @keywords internal
#' @noRd
.epinow2_interval <- function(summary_table) {
  n <- nrow(summary_table)
  lower <- grep("^lower_", names(summary_table), value = TRUE)
  upper <- grep("^upper_", names(summary_table), value = TRUE)
  widths <- suppressWarnings(as.numeric(sub("^lower_", "", lower)))
  ok <- !is.na(widths) & sub("^lower_", "upper_", lower) %in% upper
  if (!any(ok)) {
    return(list(
      conf.low = rep(NA_real_, n), conf.high = rep(NA_real_, n),
      level = NA_real_
    ))
  }
  widest <- lower[ok][which.max(widths[ok])]
  list(
    conf.low  = summary_table[[widest]],
    conf.high = summary_table[[sub("^lower_", "upper_", widest)]],
    # `lower_90` is a 90% interval, so the column name is the width in percent.
    level     = max(widths[ok]) / 100
  )
}

#' Tidy any \pkg{EpiNow2} fit that predicts cases by date
#'
#' @param x An `estimate_infections`, `epinow` or `estimate_truncation` fit.
#' @param probs Optional numeric vector of probabilities.
#' @param stratum Stratum label.
#'
#' @return A tibble in the standard tidy-nowcast shape.
#'
#' @keywords internal
#' @noRd
.tidy_epinow2_predictions <- function(x, probs = NULL, stratum = "all") {
  .need_pkg("EpiNow2")
  summarised <- as.data.frame(EpiNow2::get_predictions(x, format = "summary"))
  bounds <- .epinow2_interval(summarised)

  quantiles <- NULL
  if (!is.null(probs) && length(probs) > 0L) {
    .assert_probs(probs)
    samples <- as.data.frame(EpiNow2::get_predictions(x, format = "sample"))
    grouped <- split(samples$predicted, as.Date(samples$date))
    key <- as.character(as.Date(summarised$date))
    quantiles <- stats::setNames(
      lapply(probs, function(p) {
        vapply(
          grouped[key], stats::quantile, numeric(1),
          probs = p, na.rm = TRUE, USE.NAMES = FALSE
        )
      }),
      .tidy_quantile_names(probs)
    )
  }

  .tidy_nowcast_frame(
    event_date = as.Date(summarised$date),
    estimate   = summarised$median,
    conf.low   = bounds$conf.low,
    conf.high  = bounds$conf.high,
    level      = bounds$level,
    engine     = "EpiNow2",
    stratum    = stratum,
    quantiles  = quantiles
  )
}

#' @rdname tidy.nowcast
#' @exportS3Method generics::tidy
tidy.estimate_infections <- function(x, probs = NULL, ...) {
  .tidy_epinow2_predictions(x, probs = probs)
}

#' @rdname tidy.nowcast
#' @exportS3Method generics::tidy
tidy.epinow <- function(x, probs = NULL, ...) {
  # `epinow()` wraps `estimate_infections()` and keeps the fit in `$estimates`.
  fit <- x$estimates %||% x
  .tidy_epinow2_predictions(fit, probs = probs)
}

#' @rdname tidy.nowcast
#' @exportS3Method generics::tidy
tidy.estimate_truncation <- function(x, probs = NULL, ...) {
  .tidy_epinow2_predictions(x, probs = probs)
}

#' Moments of a fitted \pkg{EpiNow2} delay distribution, per posterior draw
#'
#' The mean and sd are what a reader actually wants off a delay fit, but
#' \pkg{EpiNow2} exposes only the distribution's own parameters (`meanlog`,
#' `sdlog`, ...). Deriving the moments with the family's algebra would mean a
#' `switch()` over lognormal / gamma / normal / exp / weibull in *this* package --
#' which silently returns nothing the day \pkg{EpiNow2} gains a sixth family.
#'
#' So they are computed from the **distribution**, not from the parameters:
#' [EpiNow2::discretise()] turns a fixed `dist_spec` into a PMF whatever family
#' it is, and the moments follow by summation. The parameter *names* come off the
#' object too, so nothing here names a family. A new family works the day
#' `discretise()` supports it.
#'
#' Accuracy against the closed forms, checked on lognormal, gamma and Weibull:
#' the mean is exact, the sd is high by roughly 1% -- the variance a discrete
#' grid adds. These are the moments of the discretised delay, which is the
#' distribution \pkg{EpiNow2} itself convolves with downstream.
#'
#' @param fit An `estimate_dist` fit.
#' @param draws The `delay_params` draws matrix.
#'
#' @return A two-column matrix (`mean`, `sd`), one row per draw; `NULL` when the
#'   distribution cannot be discretised.
#'
#' @keywords internal
#' @noRd
.epinow2_delay_moments <- function(fit, draws) {
  template <- tryCatch(
    EpiNow2::fix_parameters(EpiNow2::get_parameters(fit)$delay),
    error = function(e) NULL
  )
  if (is.null(template)) return(NULL)

  moments <- tryCatch(
    t(apply(draws, 1, function(row) {
      spec <- template
      # Overwrite by position: `.extract_to_dist_spec()` builds the parameters in
      # the same order as the draws columns, and a fixed `dist_spec` holds bare
      # numerics, so this needs no knowledge of what the family is.
      spec$parameters[] <- as.list(row)
      pmf <- EpiNow2::discretise(spec)$pmf
      delay <- seq_along(pmf) - 1
      total <- sum(pmf)
      mean_delay <- sum(delay * pmf) / total
      c(
        mean = mean_delay,
        sd   = sqrt(sum(delay^2 * pmf) / total - mean_delay^2)
      )
    })),
    error = function(e) NULL
  )
  moments
}

#' Posterior draws of an \pkg{EpiNow2} delay fit, parameters and moments
#'
#' Split out from [tidy.estimate_dist()] so the summarising can be tested without
#' a Stan fit: mock this and the rest is arithmetic.
#'
#' @param x An `estimate_dist` fit.
#'
#' @return A numeric matrix, draws x terms, with the family's own parameter names
#'   plus `mean` and `sd` where the distribution could be discretised.
#'
#' @keywords internal
#' @noRd
.epinow2_delay_draws <- function(x) {
  draws <- EpiNow2::extract_samples(x$fit, pars = "delay_params")$delay_params
  # The parameter names come off the fitted object, in the same order as the
  # draws columns, so this works for whatever family was fitted.
  spec <- EpiNow2::get_parameters(x)$delay
  colnames(draws) <- names(EpiNow2::fix_parameters(spec)$parameters)

  moments <- .epinow2_delay_moments(x, draws)
  if (!is.null(moments)) draws <- cbind(draws, moments)
  draws
}

#' Tidy the delay distribution from an \pkg{EpiNow2} `estimate_dist()` fit
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [EpiNow2::estimate_dist()] (new in \pkg{EpiNow2} 1.9.0) estimates a
#' **reporting-delay distribution**, not a nowcast, so -- like
#' [tidy.epidist_fit()] -- this returns a *delay-shaped* table: one row per
#' distribution parameter, with `term` rather than `event_date`.
#'
#' @section Value:
#'
#' One row per parameter of the fitted distribution -- whichever `dist` was
#' fitted, named as \pkg{EpiNow2} names them -- plus the derived **`mean`** and
#' **`sd`** of the delay, which are the numbers most people want and which make
#' the result directly comparable with [tidy.epidist_fit()].
#'
#' Everything is summarised from the posterior draws, so `level` is the interval
#' you asked for rather than whichever `CrIs` the fit happened to use, and
#' `probs` can add any quantile.
#'
#' @section How `mean` and `sd` are obtained:
#'
#' Not from the family's algebra. \pkg{EpiNow2} can fit five families today and
#' may add more, and a `switch()` in this package would quietly stop reporting
#' anything the day it does. Instead each draw's parameters are put back into the
#' fit's own `dist_spec` and discretised with [EpiNow2::discretise()], which
#' knows the families; the moments are then a summation over the PMF. Nothing
#' here names a distribution, so a new family works as soon as `discretise()`
#' supports it.
#'
#' The trade-off is that these are the moments of the **discretised** delay --
#' the distribution \pkg{EpiNow2} convolves with downstream. Against the closed
#' forms the mean is exact and the sd runs about 1% high, that being the variance
#' a discrete grid adds. \pkg{epidist} reports continuous-distribution moments
#' via `epidist::add_mean_sd()`, so expect a difference of that order when
#' comparing the two.
#'
#' @section A name collision worth knowing about:
#'
#' `summary()` on an `estimate_dist` fit has `mean` and `sd` **columns**, and
#' those are the posterior mean and sd **of the parameter** on that row -- not of
#' the delay. The `mean` and `sd` this method reports are **rows**, and are the
#' delay distribution's own moments, matching [tidy.epidist_fit()]. Same words,
#' different quantities.
#'
#' @param x A fit from [EpiNow2::estimate_dist()].
#' @param probs Optional numeric vector of probabilities in `[0, 1]`, adding one
#'   `q*` column each.
#' @param level Width of the reported interval. Defaults to `0.95`, matching
#'   [tidy.epidist_fit()].
#' @param ... Unused, for generic consistency.
#'
#' @returns A tibble with `term`, `estimate`, `conf.low`, `conf.high`, `level`
#'   and `engine`.
#'
#' @seealso [tidy.epidist_fit()] for the \pkg{epidist} equivalent,
#'   [tbl_now_to_EpiNow2()] for the conversion.
#'
#' @rdname tidy.estimate_dist
#' @exportS3Method generics::tidy
tidy.estimate_dist <- function(x, probs = NULL, level = 0.95, ...) {
  .need_pkg("EpiNow2")
  if (!is.numeric(level) || length(level) != 1L || is.na(level) ||
        level <= 0 || level >= 1) {
    cli::cli_abort("{.arg level} must be a single number strictly between 0 and 1.")
  }

  draws <- .epinow2_delay_draws(x)

  tail_probs <- c((1 - level) / 2, 1 - (1 - level) / 2)
  if (!is.null(probs) && length(probs) > 0L) .assert_probs(probs)

  # `apply()` carries the column names through onto the result; unname so the
  # tibble's columns are bare vectors rather than named ones.
  by_term <- function(f, ...) unname(apply(draws, 2, f, ...))

  out <- tibble::tibble(
    term      = colnames(draws),
    estimate  = by_term(stats::median, na.rm = TRUE),
    conf.low  = by_term(stats::quantile, probs = tail_probs[1], na.rm = TRUE),
    conf.high = by_term(stats::quantile, probs = tail_probs[2], na.rm = TRUE),
    level     = level,
    engine    = "EpiNow2"
  )
  if (!is.null(probs) && length(probs) > 0L) {
    quantiles <- lapply(probs, function(p) {
      unname(apply(draws, 2, stats::quantile, probs = p, na.rm = TRUE))
    })
    out <- dplyr::bind_cols(
      out, tibble::as_tibble(stats::setNames(quantiles, .tidy_quantile_names(probs)))
    )
  }
  out
}

#' @rdname tidy.nowcast
#' @exportS3Method generics::tidy
tidy.list <- function(x, probs = NULL, engine = NULL, level = NULL, ...) {
  # Two different things arrive here as a bare `list`, so S3 dispatch cannot tell
  # them apart: a NobBS fit, and the per-stratum list of fits produced by
  # `lapply(tbl_now_to_baselinenowcast(x, format = "triangle_list"), ...)`.
  # Identify by structure, and let the caller override.
  engine <- engine %||% .tidy_detect_engine(x)

  if (identical(engine, "NobBS")) {
    .reject_probs(probs, "NobBS")
    est <- as.data.frame(x$estimates)
    # `NobBS.strat()` stacks one block per stratum and names it in a `stratum`
    # column; plain `NobBS()` has no such column and is a single pooled series.
    # Reading only the first would drop strata, and labelling them all "all"
    # would make (stratum, event_date) non-unique.
    stratum <- if ("stratum" %in% names(est)) {
      as.character(est$stratum)
    } else {
      "all"
    }
    # `lower`/`upper` are the `specs$conf` interval, and `NobBS()` does not
    # return `specs` -- its value is `list(estimates, estimates.inflated,
    # nowcast.post.samps, params.post)`. The width is therefore genuinely
    # unrecoverable from the fit, so it is reported as NA unless the caller says
    # what they asked for. Guessing the 0.95 default would put a number in the
    # one column that exists to stop widths being compared blindly.
    return(.tidy_nowcast_frame(
      event_date = as.Date(est$onset_date),
      estimate   = est$estimate,
      conf.low   = est$lower,
      conf.high  = est$upper,
      level      = .tidy_level(level),
      engine     = "NobBS",
      stratum    = stratum
    ))
  }

  # `regional_epinow()` returns a plain nested list, one block per region under
  # `$regional`. Same treatment as the per-stratum list below: one block per
  # region, labelled with the region name.
  if (identical(engine, "EpiNow2")) {
    regional <- x$regional
    return(dplyr::bind_rows(lapply(names(regional), function(region) {
      .tidy_epinow2_predictions(regional[[region]], probs = probs, stratum = region)
    })))
  }

  # A per-stratum list of `baselinenowcast_df` fits: tidy each and label it with
  # its list name, so the result is the same one-block-per-stratum table the
  # natively stratified engines produce.
  if (identical(engine, "baselinenowcast")) {
    labels <- names(x)
    if (is.null(labels) || any(!nzchar(labels))) {
      labels <- as.character(seq_along(x))
    }
    return(dplyr::bind_rows(lapply(seq_along(x), function(i) {
      one <- tidy(x[[i]], probs = probs, ...)
      one$stratum <- labels[i]
      one
    })))
  }

  cli::cli_abort(c(
    "Don't know how to {.fn tidy} this list.",
    "i" = "Recognised shapes are a {.pkg NobBS} fit and a list of \\
           {.cls baselinenowcast_df} fits, one per stratum.",
    "i" = "Supply {.arg engine} explicitly, e.g. \\
           {.code tidy(x, engine = \"NobBS\")}."
  ))
}

#' Validate a caller-supplied interval width
#'
#' `NULL` means "the engine did not say", which is reported as `NA` rather than
#' guessed.
#'
#' @param level A single number strictly between 0 and 1, or `NULL`.
#'
#' @return `NA_real_` or `level`.
#'
#' @keywords internal
#' @noRd
.tidy_level <- function(level) {
  if (is.null(level)) {
    return(NA_real_)
  }
  if (!is.numeric(level) || length(level) != 1L || is.na(level) ||
        level <= 0 || level >= 1) {
    cli::cli_abort(
      "{.arg level} must be a single number strictly between 0 and 1."
    )
  }
  level
}

#' Work out which package produced an unclassed list
#'
#' @param x A list.
#'
#' @return `"NobBS"` or `NULL`.
#'
#' @keywords internal
#' @noRd
.tidy_detect_engine <- function(x) {
  if (!is.list(x)) {
    return(NULL)
  }
  if ("estimates" %in% names(x) &&
        "onset_date" %in% names(as.data.frame(x$estimates))) {
    return("NobBS")
  }
  # `regional_epinow()` nests its per-region fits under `$regional`.
  if ("regional" %in% names(x) && is.list(x$regional) &&
        length(x$regional) > 0L) {
    return("EpiNow2")
  }
  # `?tbl_now_triangle_list` recommends
  # `lapply(triangles, baselinenowcast::baselinenowcast)`, which yields a plain
  # list of classed fits -- one per stratum.
  if (length(x) > 0L &&
        all(vapply(x, inherits, logical(1), "baselinenowcast_df"))) {
    return("baselinenowcast")
  }
  NULL
}

#' Tidy the delay distribution from an \pkg{epidist} fit
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' \pkg{epidist} is the one supported package that does **not** produce a
#' nowcast. It estimates the **reporting-delay distribution**, so there are no
#' per-event-date case estimates to tidy and the columns
#' [tidy.nowcast()] promises (`event_date`, `stratum`, ...) would all be
#' meaningless. This method therefore returns a different, delay-shaped table --
#' one row per distribution parameter rather than one row per date.
#'
#' @section Value:
#'
#' A [tibble][tibble::tibble] with one row per parameter of the fitted delay
#' distribution and these columns:
#'
#' \describe{
#'   \item{`term`}{`character`. The parameter: the distribution's own parameters
#'     (`mu`, `sigma`, ... -- whichever the `family` has) plus the derived
#'     `mean` and `sd`, which are the numbers most people actually want.}
#'   \item{`estimate`}{`numeric`. Posterior median.}
#'   \item{`conf.low`, `conf.high`}{`numeric`. Interval bounds, following
#'     \pkg{broom}'s naming.}
#'   \item{`level`}{`numeric`. The width of that interval.}
#'   \item{`engine`}{`character`. Always `"epidist"`.}
#' }
#'
#' `probs` appends one `q*` column per requested probability, exactly as it does
#' for the nowcast methods -- the fit exposes draws, so any quantile is real
#' rather than an approximation.
#'
#' @section Dispatch:
#'
#' `epidist()` returns an object of class `c("brmsfit", "epidist_fit")`, in that
#' order, so if \pkg{broom.mixed} is loaded its `tidy.brmsfit()` method matches
#' **first** and you get raw \pkg{brms} parameters instead of this table. Call
#' `tidy.epidist_fit(fit)` explicitly when you want the delay distribution and
#' cannot be sure which method will win.
#'
#' @param x A fit from [epidist::epidist()].
#' @param probs Optional numeric vector of probabilities in `[0, 1]`, adding one
#'   `q*` column each.
#' @param level Width of the reported interval. Defaults to `0.95`.
#' @param newdata Optional data frame passed to
#'   [epidist::predict_delay_parameters()], for a fit with covariates in the
#'   delay model (`formula = mu ~ 1 + gender`, say). `NULL` uses the fit's own
#'   data.
#' @param ... Unused, for generic consistency.
#'
#' @returns A tibble, as described in *Value*.
#'
#' @seealso [tidy.nowcast()] for the case-count nowcast engines,
#'   [tbl_now_to_epidist()] for the conversion.
#'
#' @examplesIf FALSE
#' # Fitting needs Stan, so this is not run.
#' data(denguedat)
#' nowobj <- tbl_now(denguedat,
#'   event_date = "onset_week", report_date = "report_week", verbose = FALSE
#' )
#' fit <- tbl_now_to_epidist(nowobj) |>
#'   epidist::as_epidist_marginal_model() |>
#'   epidist::epidist()
#'
#' tidy(fit)
#' tidy(fit, probs = c(0.05, 0.95))
#'
#' @rdname tidy.epidist_fit
#' @exportS3Method generics::tidy
tidy.epidist_fit <- function(x, probs = NULL, level = 0.95, newdata = NULL,
                             ...) {
  .need_pkg("epidist")
  if (!is.numeric(level) || length(level) != 1L || level <= 0 || level >= 1) {
    cli::cli_abort("{.arg level} must be a single number strictly between 0 and 1.")
  }

  # Draws of the delay parameters, one row per draw per observation.
  # `add_mean_sd()` appends the summaries of the distribution those parameters
  # imply, which is what a reader actually wants to read off a delay fit.
  draws <- epidist::predict_delay_parameters(x, newdata = newdata)
  draws <- epidist::add_mean_sd(draws)

  # Everything except the bookkeeping columns is a parameter worth reporting.
  bookkeeping <- c("draw", "index", ".draw", ".chain", ".iteration", "obs")
  terms <- setdiff(names(draws), bookkeeping)
  terms <- terms[vapply(draws[terms], is.numeric, logical(1))]

  # `predict_delay_parameters()` returns one row per draw x OBSERVATION, and the
  # quantiles below pool over both. For an intercept-only delay model every
  # observation shares the draw's value, so that pooling is exactly the posterior
  # interval. With covariates in the delay model (`formula = mu ~ 1 + gender`)
  # the parameter genuinely differs between observations, and pooling reports a
  # mixture ACROSS covariate levels rather than a posterior interval for any one
  # of them. Say so rather than let the number be read as the latter.
  .warn_epidist_mixture(draws, terms)

  tail_probs <- c((1 - level) / 2, 1 - (1 - level) / 2)

  summaries <- lapply(terms, function(term) {
    values <- draws[[term]]
    bounds <- stats::quantile(values, probs = tail_probs, na.rm = TRUE)
    row <- tibble::tibble(
      term      = term,
      estimate  = stats::median(values, na.rm = TRUE),
      conf.low  = unname(bounds[1]),
      conf.high = unname(bounds[2]),
      level     = level,
      engine    = "epidist"
    )
    if (!is.null(probs) && length(probs) > 0L) {
      .assert_probs(probs)
      extra <- lapply(probs, function(p) {
        unname(stats::quantile(values, probs = p, na.rm = TRUE))
      })
      row <- dplyr::bind_cols(
        row, tibble::as_tibble(stats::setNames(extra, .tidy_quantile_names(probs)))
      )
    }
    row
  })

  dplyr::bind_rows(summaries)
}

#' Warn when an epidist delay model varies by observation
#'
#' Detected on a single draw: if a parameter takes more than one value within one
#' draw, it is a function of the data and the pooled quantile is a mixture.
#' Checking one draw is enough, and keeps this O(n_obs) rather than O(n_draws x
#' n_obs).
#'
#' @param draws The data frame from [epidist::predict_delay_parameters()].
#' @param terms Character vector of the parameter columns being summarised.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
.warn_epidist_mixture <- function(draws, terms) {
  draw_id <- if ("draw" %in% names(draws)) draws$draw else draws$.draw
  if (is.null(draw_id) || length(unique(draw_id)) == nrow(draws)) {
    return(invisible(NULL))
  }

  first <- draws[draw_id == draw_id[1], , drop = FALSE]
  if (nrow(first) < 2L) {
    return(invisible(NULL))
  }
  varying <- terms[vapply(
    terms,
    function(term) length(unique(first[[term]])) > 1L,
    logical(1)
  )]
  if (length(varying) == 0L) {
    return(invisible(NULL))
  }

  cli::cli_warn(c(
    "This delay model has covariates: {.field {varying}} take{?s/} a \\
     different value for different observations.",
    "i" = "{.arg estimate} and the interval pool over draws {.emph and} \\
           observations, so they describe the mixture across covariate \\
           levels, not one level's posterior.",
    "i" = "Pass {.arg newdata} with the covariate combination you want, e.g. \\
           {.code tidy(fit, newdata = data.frame(gender = \"female\"))}."
  ))
  invisible(NULL)
}
