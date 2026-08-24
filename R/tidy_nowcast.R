# tidy() for the objects `run_nowcast()` and `nowcast_backtest()` return.
#
# `nowcast_tidy()` (R/run_nowcast.R) is the BACKEND AUTHOR's hook -- it turns a
# Stan fit into the tidy quantile format. This file is the USER's verb: the same
# `tidy()` that already works on every raw engine fit, so a `tbl_nowcast` and an
# ensemble land in exactly the standard frame documented at [tidy.nowcast()].

#' The widest central interval a set of quantile levels can express
#'
#' A `tbl_nowcast` carries whatever levels it was summarised at, which need not
#' be symmetric: a fit reported at `c(0.1, 0.5, 0.9)` has a 80% interval, one at
#' `c(0.25, 0.5, 0.9)` has none at all. Guessing 0.95 in the column that exists
#' to stop widths being compared blindly is worse than reporting `NA`, so an
#' unmatched set gets `NA` bounds and an `NA` level.
#'
#' @param levels Numeric vector of quantile levels present in the predictions.
#' @param tolerance Numeric tolerance for the symmetry match.
#'
#' @return A list with `lower`, `upper` and `level`, all `NA` when no symmetric
#'   pair exists.
#'
#' @keywords internal
#' @noRd
.widest_symmetric_pair <- function(levels, tolerance = 1e-8) {
  none <- list(lower = NA_real_, upper = NA_real_, level = NA_real_)
  levels <- sort(unique(levels[!is.na(levels)]))
  candidates <- levels[levels < 0.5 - tolerance]

  for (lower in candidates) {
    hit <- levels[abs(levels - (1 - lower)) < tolerance]
    if (length(hit) > 0) {
      # `candidates` is ascending, so the first match is the widest interval.
      return(list(lower = lower, upper = hit[[1]], level = 1 - 2 * lower))
    }
  }
  none
}

#' Tidy a nowcast produced by `run_nowcast()` or `nowcast_ensemble()`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Turns a [tbl_nowcast] into the same table every other `tidy()` method in this
#' package returns, so a nowcast produced through [run_nowcast()] and one
#' produced by calling a modelling package by hand are read the same way.
#'
#' @section Value:
#'
#' A [tibble][tibble::tibble] with the columns documented at [tidy.nowcast()]:
#' `event_date`, `stratum`, `estimate`, `conf.low`, `conf.high`, `level` and
#' `engine`, plus one `q*` column per element of `probs`.
#'
#' Two of those columns are read off the object rather than assumed:
#'
#' \describe{
#'   \item{`level`}{A `tbl_nowcast` holds whatever quantile levels it was
#'     summarised at, and those need not be symmetric. `level` is the width of
#'     the **widest symmetric pair actually present** -- `0.95` for the default
#'     [nowcast_quantile_levels()], `0.8` for a fit summarised at
#'     `c(0.1, 0.5, 0.9)`. When no symmetric pair exists, `level`, `conf.low`
#'     and `conf.high` are all `NA`: a guessed width defeats the point of the
#'     column.}
#'   \item{`engine`}{The nowcast's `method`, so `"baselinenowcast"` for a single
#'     fit and `"ensemble"` (or whatever `name` [nowcast_ensemble()] was given)
#'     for a combined one.}
#' }
#'
#' `stratum` is `"all"` only when the nowcast declares no strata. Several strata
#' columns are pasted `" | "`-separated, matching the rest of the package, so
#' `(stratum, event_date)` is a unique key.
#'
#' `estimate` is the `0.5` quantile, and `NA` when the nowcast was summarised at
#' levels that do not include the median.
#'
#' @param x A [tbl_nowcast].
#' @param probs Optional numeric vector of probabilities in `[0, 1]`, adding one
#'   `q*` column each. Only available when the nowcast carries **draws**: a
#'   quantile-only nowcast cannot produce a level it was not summarised at, so
#'   asking for one is an error rather than an interpolation dressed up as a
#'   quantile.
#' @param ... Unused, for generic consistency.
#'
#' @return A tibble, as described in *Value*.
#'
#' @seealso [tidy.nowcast()] for the same table off a raw engine fit,
#'   [run_nowcast()], [nowcast_ensemble()].
#'
#' @examples
#' predictions <- tidyr::expand_grid(
#'   onset_week = as.Date("2020-01-05") + c(0, 7),
#'   .quantile_level = c(0.025, 0.5, 0.975)
#' )
#' predictions$.value <- c(5, 10, 18, 6, 12, 21)
#' nc <- tbl_nowcast(
#'   predictions = predictions, method = "toy", event_date = "onset_week"
#' )
#'
#' tidy(nc)
#'
#' @name tidy.tbl_nowcast
#' @usage NULL
tidy_tbl_nowcast <- function(x, probs = NULL, ...) {
  .assert_tbl_nowcast(x)

  event_col <- x@event_date
  strata <- x@strata
  predictions <- dplyr::as_tibble(x@predictions)

  if (nrow(predictions) == 0) {
    return(.tidy_nowcast_frame(
      event_date = as.Date(character(0)), estimate = numeric(0),
      conf.low = numeric(0), conf.high = numeric(0),
      level = numeric(0), engine = character(0), stratum = character(0)
    ))
  }

  # `.epinow2_region()` is the package's one place for the " | " convention;
  # reusing it keeps this label identical to the one every other tidy() method
  # and `triangle_list` produces.
  predictions$.stratum <- .epinow2_region(predictions, strata)

  bounds <- .widest_symmetric_pair(predictions$.quantile_level)

  # One row per (stratum, event date), in the order the wide columns below are
  # built from, so nothing has to be re-joined.
  targets <- predictions |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(".stratum", event_col)))) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(".stratum", event_col))))

  at_level <- function(level) {
    if (is.na(level)) {
      return(rep(NA_real_, nrow(targets)))
    }
    wanted <- predictions |>
      dplyr::filter(abs(.data$.quantile_level - level) < 1e-8) |>
      dplyr::select(dplyr::all_of(c(".stratum", event_col, ".value")))
    # Keyed on (stratum, event date), never on the date alone: a stratified
    # nowcast repeats every date once per stratum, and joining on the date would
    # hand each date's values to whichever stratum sorted first.
    targets |>
      dplyr::left_join(wanted, by = c(".stratum", event_col)) |>
      dplyr::pull(".value")
  }

  quantiles <- .nowcast_probs(x, targets, probs, event_col)

  .tidy_nowcast_frame(
    event_date = targets[[event_col]],
    estimate   = at_level(0.5),
    conf.low   = at_level(bounds$lower),
    conf.high  = at_level(bounds$upper),
    level      = bounds$level,
    engine     = x@method,
    stratum    = targets$.stratum,
    quantiles  = quantiles
  )
}

#' Quantiles of a `tbl_nowcast`'s draws, aligned with its targets
#'
#' @param x A [tbl_nowcast].
#' @param targets A tibble of `.stratum` and the event-date column, one row per
#'   prediction target, in output order.
#' @param probs Numeric vector of probabilities, or `NULL`.
#' @param event_col Name of the event-date column.
#'
#' @return A named list of numeric vectors, or `NULL`.
#'
#' @keywords internal
#' @noRd
.nowcast_probs <- function(x, targets, probs, event_col) {
  if (is.null(probs) || length(probs) == 0L) {
    return(NULL)
  }
  if (is.null(x@draws)) {
    .reject_probs(probs, x@method)
  }
  .assert_probs(probs)

  draws <- dplyr::as_tibble(x@draws)
  draws$.stratum <- .epinow2_region(draws, x@strata)
  by_cell <- split(
    draws$.value, paste(draws$.stratum, as.character(draws[[event_col]]))
  )
  key <- paste(targets$.stratum, as.character(targets[[event_col]]))

  stats::setNames(
    lapply(probs, function(p) {
      vapply(
        by_cell[key], stats::quantile, numeric(1),
        probs = p, na.rm = TRUE, USE.NAMES = FALSE
      )
    }),
    .tidy_quantile_names(probs)
  )
}

#' Tidy the scores of a `nowcast_backtest()`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' One row per (method, `now` date, target) with the scores that target earned,
#' with the dot-prefixed internal column names traded for ordinary ones so the
#' result goes straight into \pkg{dplyr} or \pkg{ggplot2}.
#'
#' @param x A `nowcast_backtest` object.
#' @param ... Unused, for generic consistency.
#'
#' @return A [tibble][tibble::tibble] with the columns `method`, `now`,
#'   `event_date`, `stratum`, `observed`, `wis`, `ae_median`, `coverage_50` and
#'   `coverage_90`. `stratum` is `"all"` for an unstratified backtest and the
#'   `" | "`-pasted strata otherwise, so `(method, now, stratum, event_date)` is
#'   a unique key.
#'
#' @seealso [nowcast_backtest()], [nowcast_weights()]
#'
#' @examplesIf FALSE
#' # A backtest refits every method once per date, so this is not run.
#' bt <- nowcast_backtest(dengue, methods = "baselinenowcast")
#' tidy(bt)
#'
#' @exportS3Method generics::tidy
tidy.nowcast_backtest <- function(x, ...) {
  scores <- dplyr::as_tibble(x$scores)
  event_col <- x$event_date
  strata <- x$strata %||% character(0)

  dplyr::tibble(
    method     = as.character(scores$.method),
    now        = as.Date(scores$.now),
    event_date = as.Date(scores[[event_col]]),
    stratum    = .epinow2_region(scores, strata),
    observed   = as.numeric(scores$.observed),
    wis        = as.numeric(scores$wis),
    ae_median  = as.numeric(scores$ae_median),
    coverage_50 = as.logical(scores$coverage_50),
    coverage_90 = as.logical(scores$coverage_90)
  ) |>
    dplyr::arrange(
      .data$method, .data$now, .data$stratum, .data$event_date
    )
}
