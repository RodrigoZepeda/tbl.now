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
#'   \item{`stratum`}{`character`. `"all"` when the fit is unstratified.}
#'   \item{`estimate`}{`numeric`. The point nowcast -- the posterior median where
#'     the engine provides draws or a median, otherwise its point estimate.}
#'   \item{`conf.low`, `conf.high`}{`numeric`. Interval bounds, following
#'     \pkg{broom}'s naming. `NA` when the engine returns no interval.}
#'   \item{`level`}{`numeric`. The width the interval actually has, e.g. `0.95`.
#'     Engines differ -- \pkg{epinowcast} reports a 90% band by default while
#'     others report 95% -- and without this column those get compared as if they
#'     were the same thing.}
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
#' @param engine Optional string naming the engine. Needed only for the two
#'   packages that return an **unclassed list** (\pkg{NobBS} and
#'   \pkg{nowcaster}), which are otherwise told apart by their structure.
#' @param strata_levels Optional character vector of stratum labels, in code
#'   order. Only used for a stratified \pkg{nowcaster} fit, whose `$age`
#'   component reports the numeric code the converter assigned rather than the
#'   label; pass the `"nowcaster_levels"` attribute that
#'   [tbl_now_to_nowcaster()] leaves on the line list.
#' @param ... Passed to methods.
#'
#' @section Supported objects:
#'
#' * `nowcast_prediction` (S7) from `diseasenowcasting::predict()`
#' * `baselinenowcast_df` from `baselinenowcast::baselinenowcast()`
#' * `epinowcast` fits
#' * `stsNC` from `surveillance::nowcast()`
#' * the list returned by `NobBS::NobBS()`
#' * the list returned by `nowcaster::nowcasting_inla()`
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
  .assert_probs(probs %||% 0.5)
  draws_df <- as.data.frame(x)
  grouped <- split(draws_df$pred_count, as.Date(draws_df$reference_date))
  dates <- as.Date(names(grouped))
  draws <- do.call(cbind, lapply(grouped, identity))

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

#' @rdname tidy.nowcast
#' @exportS3Method generics::tidy
tidy.epinowcast <- function(x, probs = NULL, ...) {
  .need_pkg("epinowcast")
  nowcast <- summary(x, type = "nowcast")

  quantiles <- NULL
  if (!is.null(probs)) {
    .assert_probs(probs)
    samples <- summary(x, type = "nowcast_samples")
    grouped <- split(samples$sample, as.Date(samples$reference_date))
    quantiles <- stats::setNames(
      lapply(probs, function(p) vapply(grouped, stats::quantile, numeric(1), probs = p)),
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

#' @rdname tidy.nowcast
#' @exportS3Method generics::tidy
tidy.list <- function(x, probs = NULL, engine = NULL, strata_levels = NULL, ...) {
  # NobBS and nowcaster both return a bare `list`, so S3 dispatch cannot tell
  # them apart. Identify them by structure, and let the caller override.
  engine <- engine %||% .tidy_detect_engine(x)

  if (identical(engine, "NobBS")) {
    .reject_probs(probs, "NobBS")
    est <- as.data.frame(x$estimates)
    return(.tidy_nowcast_frame(
      event_date = as.Date(est$onset_date),
      estimate   = est$estimate,
      conf.low   = est$lower,
      conf.high  = est$upper,
      level      = 0.95,
      engine     = "NobBS"
    ))
  }

  if (identical(engine, "nowcaster")) {
    .reject_probs(probs, "nowcaster")

    # A stratified fit carries an extra `$age` component -- one row per stratum
    # per week -- whose `fx_etaria` holds the numeric code the converter
    # assigned. Map it back with `strata_levels` (the "nowcaster_levels"
    # attribute that `tbl_now_to_nowcaster()` puts on the line list).
    if (!is.null(x$age) && nrow(as.data.frame(x$age)) > 0L) {
      by_stratum <- as.data.frame(x$age)
      codes <- as.integer(as.character(by_stratum$fx_etaria))
      stratum <- if (!is.null(strata_levels)) {
        strata_levels[codes]
      } else {
        as.character(by_stratum$fx_etaria)
      }
      return(.tidy_nowcast_frame(
        event_date = as.Date(by_stratum$dt_event),
        estimate   = by_stratum$Median,
        conf.low   = by_stratum$LI,
        conf.high  = by_stratum$LS,
        level      = 0.95,
        engine     = "nowcaster",
        stratum    = stratum
      ))
    }

    total <- as.data.frame(x$total)
    return(.tidy_nowcast_frame(
      event_date = as.Date(total$dt_event),
      estimate   = total$Median,
      conf.low   = total$LI,
      conf.high  = total$LS,
      level      = 0.95,
      engine     = "nowcaster"
    ))
  }

  cli::cli_abort(c(
    "Don't know how to {.fn tidy} this list.",
    "i" = "Supply {.arg engine} explicitly, e.g. \\
           {.code tidy(x, engine = \"NobBS\")}."
  ))
}

#' Work out which package produced an unclassed list
#'
#' @param x A list.
#'
#' @return `"NobBS"`, `"nowcaster"` or `NULL`.
#'
#' @keywords internal
#' @noRd
.tidy_detect_engine <- function(x) {
  if (is.list(x) && "estimates" %in% names(x) &&
        "onset_date" %in% names(as.data.frame(x$estimates))) {
    return("NobBS")
  }
  if (is.list(x) && "total" %in% names(x) &&
        "dt_event" %in% names(as.data.frame(x$total))) {
    return("nowcaster")
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
