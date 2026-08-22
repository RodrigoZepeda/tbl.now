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
  .tidy_nowcast_frame(
    event_date = as.Date(surveillance::epoch(x))[keep],
    estimate   = estimate[keep],
    conf.low   = NA_real_,
    conf.high  = NA_real_,
    level      = NA_real_,
    engine     = "surveillance"
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
  draws <- S7::prop(x, "draws")
  .tidy_nowcast_frame(
    event_date = S7::prop(x, "event_dates"),
    estimate   = apply(draws, 2, stats::median),
    conf.low   = apply(draws, 2, stats::quantile, probs = 0.025),
    conf.high  = apply(draws, 2, stats::quantile, probs = 0.975),
    level      = 0.95,
    engine     = "diseasenowcasting",
    quantiles  = .tidy_quantiles_from_draws(draws, probs)
  )
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
