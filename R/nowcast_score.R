# Scoring nowcasts, and the retrospective backtest that feeds the
# performance-weighted ensemble.
#
# The scores are computed here rather than delegated to scoringutils so that
# weighting an ensemble never depends on an extra package. `as_scoringutils()`
# is provided for users who want its full score suite.

#' Weighted interval score of a set of quantile predictions
#'
#' Uses the quantile (pinball) loss formulation
#' \eqn{\mathrm{WIS} = \frac{1}{K}\sum_k 2\,(\mathbb{1}\{y \le q_k\} - \tau_k)(q_k - y)},
#' which coincides with the interval-score formulation of Bracher et al. (2021)
#' when the quantile levels are symmetric around, and include, the median.
#'
#' @param observed A single observed value.
#' @param quantile_levels Numeric vector of probabilities.
#' @param predicted Numeric vector of predicted quantiles, aligned with
#'   `quantile_levels`.
#'
#' @return A single number (`NA_real_` when `observed` is missing).
#'
#' @keywords internal
#' @noRd
.wis <- function(observed, quantile_levels, predicted) {
  keep <- !is.na(predicted) & !is.na(quantile_levels)
  if (is.na(observed) || !any(keep)) {
    return(NA_real_)
  }
  quantile_levels <- quantile_levels[keep]
  predicted <- predicted[keep]

  pinball <- 2 * ((observed <= predicted) - quantile_levels) * (predicted - observed)
  mean(pinball)
}

#' Interval coverage of a set of quantile predictions
#'
#' @param observed A single observed value.
#' @param quantile_levels Numeric vector of probabilities.
#' @param predicted Numeric vector of predicted quantiles.
#' @param level The nominal central interval, e.g. `0.5` or `0.9`.
#'
#' @return `TRUE`/`FALSE`, or `NA` when the required quantiles are absent.
#'
#' @keywords internal
#' @noRd
.covered <- function(observed, quantile_levels, predicted, level) {
  lower_level <- (1 - level) / 2
  upper_level <- 1 - lower_level
  lower <- predicted[which.min(abs(quantile_levels - lower_level))]
  upper <- predicted[which.min(abs(quantile_levels - upper_level))]
  tolerance <- 1e-8
  if (length(lower) == 0 || length(upper) == 0 || is.na(observed) ||
    min(abs(quantile_levels - lower_level)) > tolerance ||
    min(abs(quantile_levels - upper_level)) > tolerance) {
    return(NA)
  }
  observed >= lower && observed <= upper
}

#' The final observed counts of a `tbl_now`
#'
#' The "truth" a nowcast is scored against: everything that was eventually
#' reported for each event date.
#'
#' This is [get_latest_reported_cases()] reshaped for scoring -- the values are
#' identical. What it adds is the bookkeeping the scoring code wants and the
#' getter deliberately does not do: the `tbl_now` class and the report-date
#' columns are dropped, any column that is neither the event date nor a
#' requested stratum is summed away, and the count is renamed `.observed` so
#' downstream code does not have to know what the source called it.
#'
#' Internal since 0.19.0. It was exported, but a second public name for
#' `get_latest_reported_cases()` is a second thing to learn for no gain --
#' [score_nowcast()] and [as_scoringutils()] now take the `tbl_now` itself as
#' `truth` and call this for you.
#'
#' Pass the *full* object here, not the truncated snapshot the nowcast was
#' fitted on: the point of the comparison is that the truth contains reports the
#' model had not seen.
#'
#' @param x A `tbl_now` object.
#' @param strata Character vector of strata columns to keep. Defaults to the
#'   object's declared strata.
#'
#' @return A `tibble` with the event-date column, the strata columns and
#'   `.observed`.
#'
#' @keywords internal
#' @noRd
nowcast_truth <- function(x, strata = get_strata(x)) {
  .assert_tbl_now(x, "nowcast_truth")

  event_col <- get_event_date(x)
  strata <- intersect(strata %||% character(0), colnames(x))

  observed <- get_latest_reported_cases(x)
  # `.reported_cases_at()` names the count after the object's `case_count`, or
  # `n` when the source was a line list.
  count_col <- get_case_count(observed) %||% "n"

  .declass_tbl_now(observed) |>
    dplyr::as_tibble() |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(event_col, strata)))) |>
    dplyr::summarise(.observed = sum(.data[[count_col]], na.rm = TRUE), .groups = "drop")
}

#' Resolve the `truth` argument of the scoring functions
#'
#' Accepts the full `tbl_now` (the common case -- everything eventually reported
#' for each event date), a data frame of observed counts, or `NULL` to fall back
#' to the nowcast's own source data.
#'
#' @param truth The user's `truth` argument.
#' @param observed_col The user's `observed_col` argument.
#' @param x The `tbl_nowcast` being scored.
#' @param arg Calling function name, for messages.
#'
#' @return A list with `truth` (a tibble) and `observed_col`.
#'
#' @keywords internal
#' @noRd
.resolve_truth <- function(truth, observed_col, x, arg) {
  if (is.null(truth)) {
    if (is.null(x@data)) {
      cli::cli_abort(c(
        "{.arg truth} is required: the nowcast does not carry its source data.",
        "i" = "Pass the full {.cls tbl_now} the nowcast was made from."
      ))
    }
    return(list(
      truth = nowcast_truth(x@data, strata = x@strata), observed_col = ".observed"
    ))
  }

  # A `tbl_now` is the natural thing to hand over -- it IS the eventual truth --
  # so collapse it here rather than making the caller do it.
  if (is_tbl_now(truth)) {
    return(list(
      truth = nowcast_truth(truth, strata = x@strata), observed_col = ".observed"
    ))
  }

  if (!is.data.frame(truth)) {
    cli::cli_abort(
      "{.arg truth} must be a {.cls tbl_now}, a data frame, or {.code NULL}."
    )
  }
  list(truth = dplyr::as_tibble(truth), observed_col = observed_col)
}

#' Score a nowcast against observed data
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Computes the weighted interval score (WIS), the absolute error of the median
#' and the 50%/90% interval coverage of a [tbl_nowcast], one row per event date
#' (and stratum).
#'
#' @param x A [tbl_nowcast] object.
#' @param truth What the nowcast is scored against. One of:
#'
#'   * a **`tbl_now`** -- normally the *full* object, still holding the reports
#'     that arrived after the nowcast's `now`. Its eventual counts per event
#'     date are worked out for you (this is [get_latest_reported_cases()],
#'     aggregated over anything that is not a stratum).
#'   * a **data frame** with the event-date column, the strata columns and a
#'     column of observed counts, named by `observed_col`.
#'   * **`NULL`** (default) -- use the `tbl_now` the nowcast was built from,
#'     which is only meaningful when that object still holds the later reports,
#'     as it does inside [nowcast_backtest()].
#' @param observed_col Name of the observed-count column in `truth`. Defaults to
#'   the last column that is neither the event date nor a stratum.
#'
#' @return A `tibble` with the event-date column, the strata columns and the
#'   columns `.observed`, `wis`, `ae_median`, `coverage_50` and `coverage_90`.
#'
#' @references
#' Bracher, J., Ray, E. L., Gneiting, T., & Reich, N. G. (2021). Evaluating
#' epidemic forecasts in an interval format. *PLoS Computational Biology*,
#' 17(2), e1008618.
#'
#' @seealso [nowcast_backtest()], [as_scoringutils()]
#'
#' @examples
#' predictions <- data.frame(
#'   onset_week = as.Date("2020-01-05"),
#'   .quantile_level = c(0.25, 0.5, 0.75),
#'   .value = c(8, 10, 13)
#' )
#' nc <- tbl_nowcast(predictions = predictions, method = "toy", event_date = "onset_week")
#' score_nowcast(nc, truth = data.frame(onset_week = as.Date("2020-01-05"), .observed = 11))
#'
#' @export
score_nowcast <- function(x, truth = NULL, observed_col = NULL) {
  .assert_tbl_nowcast(x)

  event_col <- x@event_date
  strata <- x@strata
  key <- c(event_col, strata)

  resolved <- .resolve_truth(truth, observed_col, x, "score_nowcast")
  truth <- resolved$truth
  observed_col <- resolved$observed_col

  if (is.null(observed_col)) {
    observed_col <- utils::tail(setdiff(colnames(truth), key), 1)
  }
  if (!observed_col %in% colnames(truth)) {
    cli::cli_abort("Column {.val {observed_col}} was not found in {.arg truth}.")
  }
  truth <- truth |>
    dplyr::select(dplyr::all_of(c(key, observed_col))) |>
    dplyr::rename(.observed = dplyr::all_of(observed_col))

  x@predictions |>
    dplyr::inner_join(truth, by = key) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(key))) |>
    dplyr::summarise(
      .observed = dplyr::first(.data$.observed),
      wis = .wis(dplyr::first(.data$.observed), .data$.quantile_level, .data$.value),
      ae_median = abs(
        dplyr::first(.data$.observed) -
          .data$.value[which.min(abs(.data$.quantile_level - 0.5))]
      ),
      coverage_50 = .covered(
        dplyr::first(.data$.observed), .data$.quantile_level, .data$.value, 0.5
      ),
      coverage_90 = .covered(
        dplyr::first(.data$.observed), .data$.quantile_level, .data$.value, 0.9
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(.method = x@method, .before = 1)
}

#' Restrict a `tbl_now` to the data available at a past date
#'
#' @param x A `tbl_now` object.
#' @param now_date A single Date.
#'
#' @return A `tbl_now` holding only the rows reported up to `now_date`, with its
#'   `now` set to `now_date`.
#'
#' @keywords internal
#' @noRd
.nowcast_snapshot <- function(x, now_date) {
  report_col <- get_report_date(x)
  snapshot <- x |>
    dplyr::filter(.data[[report_col]] <= now_date)
  change_now(snapshot, now = now_date)
}

#' Refit several methods at past `now` dates and score them
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Walks back through time: for every date in `now_dates`, the `tbl_now` is
#' truncated to the reports that were available then, each method is refitted on
#' that snapshot, and the resulting nowcast is scored against what was
#' eventually observed. This is what turns a set of models into ensemble weights
#' (see [nowcast_weights()] and [nowcast_ensemble()]).
#'
#' Be aware that this refits every model once per date: with Bayesian backends
#' and a long `now_dates` it is genuinely expensive.
#'
#' @param x A `tbl_now` object holding the *full* data (the later reports are
#'   what the retrospective nowcasts are scored against).
#' @param methods Character vector of method names, as in [run_nowcast()].
#' @param now_dates Vector of Dates to nowcast at. Defaults to the four most
#'   recent event dates that are at least `horizon` units before the object's
#'   `now`, so that some later reports exist to score against.
#' @param ... Passed to [run_nowcast()] for every method. Use `method_args` for
#'   arguments that differ between methods.
#' @param method_args A named list of lists: `method_args$epinowcast` is passed
#'   only to the `"epinowcast"` method. Overrides `...` on name clashes.
#' @param horizon Number of time units of hindsight required when `now_dates` is
#'   chosen automatically. Default `4`.
#' @param quantile_levels Quantile levels to score at, see [run_nowcast()].
#' @param seed Optional integer. When given, the RNG is seeded **immediately
#'   before each fit**, from `seed` and the method and date that fit is for. One
#'   `set.seed()` before the whole backtest is not enough: it only pins anything
#'   if every method consumes the same random numbers in the same order, so
#'   dropping a method, or refitting one date, silently moves every other fit.
#'   Seeding per (method, date) makes a fit depend only on which fit it is.
#' @param on_error Either `"warn"` (default) to skip a model/date that fails
#'   with a warning, or `"abort"` to stop.
#' @param verbose Logical. Whether to report progress.
#'
#' @return An object of class `nowcast_backtest`: a list with
#'
#'   \describe{
#'     \item{scores}{A `tibble` of per-date scores with an extra `.now` column.}
#'     \item{predictions}{A `tibble` of every retrospective quantile prediction.}
#'     \item{truth}{The observed counts used for scoring.}
#'     \item{methods}{The methods that produced at least one nowcast.}
#'     \item{now_dates}{The dates that were nowcast.}
#'   }
#'
#' @seealso [nowcast_weights()], [nowcast_ensemble()], [score_nowcast()]
#'
#' @examples
#' \donttest{
#' data(denguedat)
#'
#' # A short recent window keeps the example quick.
#' recent <- subset(denguedat, onset_week >= as.Date("2010-06-01"))
#' dengue <- tbl_now(recent,
#'   event_date = onset_week, report_date = report_week, verbose = FALSE
#' )
#'
#' if (requireNamespace("baselinenowcast", quietly = TRUE)) {
#'   bt <- nowcast_backtest(dengue,
#'     methods = "baselinenowcast", now_dates = as.Date("2010-11-15"),
#'     draws = 100, verbose = FALSE
#'   )
#'   bt$scores
#' }
#' }
#'
#' @export
nowcast_backtest <- function(x, methods, now_dates = NULL, ...,
                             method_args = list(), horizon = 4,
                             quantile_levels = nowcast_quantile_levels(),
                             seed = NULL,
                             on_error = c("warn", "abort"), verbose = TRUE) {
  .assert_tbl_now(x, "nowcast_backtest")
  on_error <- match.arg(on_error)

  if (missing(methods) || length(methods) == 0) {
    cli::cli_abort("{.arg methods} must name at least one nowcasting method.")
  }
  methods <- vapply(methods, .canonical_nowcast_method, character(1), USE.NAMES = FALSE)

  if (is.null(now_dates)) {
    now_dates <- .default_backtest_dates(x, horizon = horizon)
  }
  now_dates <- sort(unique(now_dates))
  if (length(now_dates) == 0) {
    cli::cli_abort("No usable {.arg now_dates}: the object has too little history to backtest.")
  }

  truth <- nowcast_truth(x)
  dots <- list(...)

  results <- list()
  for (now_date in as.list(now_dates)) {
    snapshot <- .nowcast_snapshot(x, now_date)

    for (method in methods) {
      if (isTRUE(verbose)) {
        cli::cli_alert_info("Backtesting {.val {method}} at {.val {now_date}}.")
      }

      arguments <- utils::modifyList(dots, method_args[[method]] %||% list())
      arguments <- c(
        list(x = snapshot, method = method, quantile_levels = quantile_levels, verbose = FALSE),
        arguments
      )

      if (!is.null(seed)) {
        set.seed(.backtest_seed(seed, method, now_date))
      }

      nowcast <- tryCatch(
        do.call(run_nowcast, arguments),
        error = function(e) {
          message <- c(
            "Method {.val {method}} failed at {.val {now_date}}.",
            "x" = conditionMessage(e)
          )
          if (on_error == "abort") cli::cli_abort(message) else cli::cli_warn(message)
          NULL
        }
      )
      if (is.null(nowcast)) next

      scores <- score_nowcast(nowcast, truth = truth, observed_col = ".observed")
      predictions <- nowcast@predictions |>
        dplyr::mutate(.method = method, .now = now_date, .before = 1)

      results[[length(results) + 1]] <- list(scores = scores, predictions = predictions)
    }
  }

  if (length(results) == 0) {
    cli::cli_abort("Every method failed at every date; nothing to score.")
  }

  scores <- dplyr::bind_rows(lapply(results, `[[`, "scores"))
  predictions <- dplyr::bind_rows(lapply(results, `[[`, "predictions"))
  # `.now` is carried on the predictions; put it on the scores too.
  scores <- scores |>
    dplyr::mutate(.now = rep(
      vapply(results, function(r) as.character(r$predictions$.now[1]), character(1)),
      vapply(results, function(r) nrow(r$scores), integer(1))
    ) |> as.Date(), .after = ".method")

  structure(
    list(
      scores = scores,
      predictions = predictions,
      truth = truth,
      methods = unique(scores$.method),
      now_dates = now_dates,
      event_date = get_event_date(x),
      strata = intersect(get_strata(x) %||% character(0), colnames(predictions))
    ),
    class = "nowcast_backtest"
  )
}

#' A seed that depends only on which fit it is
#'
#' @param seed The user's base seed.
#' @param method Method name.
#' @param now_date The retrospective date.
#'
#' @return A single integer.
#'
#' @keywords internal
#' @noRd
.backtest_seed <- function(seed, method, now_date) {
  as.integer(seed) + sum(utf8ToInt(paste0(method, "|", as.character(now_date))))
}

#' Pick sensible retrospective `now` dates
#'
#' @param x A `tbl_now` object.
#' @param horizon How many time units of hindsight to require.
#' @param n How many dates to return.
#'
#' @return A vector of Dates.
#'
#' @keywords internal
#' @noRd
.default_backtest_dates <- function(x, horizon = 4, n = 4) {
  report_col <- get_report_date(x)
  candidates <- sort(unique(dplyr::pull(dplyr::as_tibble(x), report_col)))
  cutoff <- max(candidates, na.rm = TRUE)

  step <- switch(get_report_units(x),
    weeks = 7, months = 30, years = 365, 1
  )
  latest <- cutoff - horizon * step
  candidates <- candidates[candidates <= latest]
  utils::tail(candidates, n)
}

#' @exportS3Method base::print
print.nowcast_backtest <- function(x, ...) {
  # `cat_*()` rather than `cli_*()`: the latter writes to the MESSAGE stream, so
  # a print method built on it vanishes under `message = FALSE`, `sink()` or
  # `capture.output()`.
  cli::cat_rule(left = cli::format_inline("A {.cls nowcast_backtest}"))
  cli::cat_bullet(c(
    cli::format_inline("methods: {.val {x$methods}}"),
    cli::format_inline("now dates: {.val {as.character(x$now_dates)}}")
  ))

  summary <- x$scores |>
    dplyr::group_by(.data$.method) |>
    dplyr::summarise(
      mean_wis = mean(.data$wis, na.rm = TRUE),
      mean_ae_median = mean(.data$ae_median, na.rm = TRUE),
      coverage_90 = mean(.data$coverage_90, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$mean_wis)
  # `format()` gives the tibble's printed lines, so the table goes to stdout
  # without a bare `print()` call in package code.
  cli::cat_line(format(summary))

  invisible(x)
}

#' Ensemble weights from a backtest
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Turns the retrospective scores of a [nowcast_backtest()] into a vector of
#' weights for [nowcast_ensemble()].
#'
#' @param backtest A `nowcast_backtest` object.
#' @param type How to derive the weights:
#'
#'   \describe{
#'     \item{`"inverse_score"` (default)}{\eqn{w_i \propto 1/\overline{WIS}_i}.
#'       Cheap, robust, and never puts all the mass on one model.}
#'     \item{`"optim"`}{The weights on the simplex that minimise the WIS of the
#'       quantile-averaged ensemble over the training window. Better in
#'       principle, but prone to overfitting when the window is short.}
#'     \item{`"equal"`}{\eqn{w_i = 1/M}. Included so that the same code path can
#'       produce the unweighted ensemble.}
#'   }
#'
#' @param ... Unused.
#'
#' @return A named numeric vector of weights summing to 1.
#'
#' @seealso [nowcast_backtest()], [nowcast_ensemble()]
#'
#' @examples
#' \donttest{
#' data(denguedat)
#'
#' # A short recent window keeps the example quick.
#' recent <- subset(denguedat, onset_week >= as.Date("2010-06-01"))
#' dengue <- tbl_now(recent,
#'   event_date = onset_week, report_date = report_week, verbose = FALSE
#' )
#'
#' if (requireNamespace("baselinenowcast", quietly = TRUE)) {
#'   bt <- nowcast_backtest(dengue,
#'     methods = "baselinenowcast", now_dates = as.Date("2010-11-15"),
#'     draws = 100, verbose = FALSE
#'   )
#'   nowcast_weights(bt)
#' }
#' }
#'
#' @export
nowcast_weights <- function(backtest, type = c("inverse_score", "optim", "equal"), ...) {
  if (!inherits(backtest, "nowcast_backtest")) {
    cli::cli_abort("{.arg backtest} must be a {.cls nowcast_backtest} (see {.fn nowcast_backtest}).")
  }
  type <- match.arg(type)

  methods <- backtest$methods
  if (type == "equal") {
    return(stats::setNames(rep(1 / length(methods), length(methods)), methods))
  }

  if (type == "inverse_score") {
    mean_wis <- backtest$scores |>
      dplyr::group_by(.data$.method) |>
      dplyr::summarise(wis = mean(.data$wis, na.rm = TRUE), .groups = "drop")

    weights <- 1 / mean_wis$wis
    # A perfect model (WIS = 0) would give an infinite weight; hand it everything.
    if (any(is.infinite(weights))) {
      weights <- as.numeric(is.infinite(weights))
    }
    weights[is.na(weights)] <- 0
    if (sum(weights) == 0) {
      cli::cli_abort("Could not derive weights: every method has a missing score.")
    }
    return(stats::setNames(weights / sum(weights), mean_wis$.method))
  }

  .optimise_nowcast_weights(backtest)
}

#' Weights minimising the training-window WIS of the quantile-averaged ensemble
#'
#' Parametrised through a softmax so that the optimiser is unconstrained while
#' the weights stay on the simplex.
#'
#' @param backtest A `nowcast_backtest` object.
#'
#' @return A named numeric vector of weights summing to 1.
#'
#' @keywords internal
#' @noRd
.optimise_nowcast_weights <- function(backtest) {
  methods <- backtest$methods
  if (length(methods) == 1) {
    return(stats::setNames(1, methods))
  }

  key <- c(".now", backtest$event_date, backtest$strata, ".quantile_level")

  wide <- backtest$predictions |>
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(key),
      names_from = ".method", values_from = ".value"
    ) |>
    dplyr::inner_join(
      backtest$truth,
      by = c(backtest$event_date, backtest$strata)
    ) |>
    tidyr::drop_na(dplyr::all_of(methods))

  if (nrow(wide) == 0) {
    cli::cli_abort(c(
      "Cannot optimise the weights: the methods have no predictions in common.",
      "i" = "Use {.code type = \"inverse_score\"} instead."
    ))
  }

  predicted <- as.matrix(wide[, methods, drop = FALSE])
  observed <- wide$.observed
  levels <- wide$.quantile_level
  # Every row is one (date, stratum, quantile level) triple, so the mean pinball
  # loss over rows is proportional to the mean WIS over targets.
  objective <- function(parameters) {
    weights <- exp(parameters - max(parameters))
    weights <- weights / sum(weights)
    ensemble <- as.numeric(predicted %*% weights)
    mean(2 * ((observed <= ensemble) - levels) * (ensemble - observed))
  }

  optimum <- tryCatch(
    stats::optim(
      par = rep(0, length(methods)), fn = objective,
      method = "Nelder-Mead", control = list(maxit = 2000)
    ),
    error = function(e) NULL
  )

  weights <- if (is.null(optimum)) NULL else exp(optimum$par - max(optimum$par))

  # An optimiser that did not converge on a usable point must fall back to equal
  # weights, not hand back `NA`s. A vector of `NA` weights does not fail here --
  # it fails much later, inside `nowcast_ensemble()`, as an all-`NA` nowcast that
  # looks like a modelling problem rather than an optimisation one.
  usable <- !is.null(weights) && !anyNA(weights) && all(is.finite(weights)) &&
    sum(weights) > 0
  if (!usable) {
    cli::cli_warn(c(
      "The weight optimiser did not converge; falling back to equal weights.",
      "i" = "Use {.code type = \"inverse_score\"} for a weighting that cannot fail \\
             this way."
    ))
    return(stats::setNames(rep(1 / length(methods), length(methods)), methods))
  }

  stats::setNames(weights / sum(weights), methods)
}

#' Export a nowcast in `scoringutils` format
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Reshapes a [tbl_nowcast] (and its observed counts) into the long
#' quantile format `scoringutils::as_forecast_quantile()` expects, so that the
#' full \pkg{scoringutils} score suite can be applied.
#'
#' @param x A [tbl_nowcast] object.
#' @param truth Passed to [score_nowcast()]; see there.
#' @param observed_col Passed to [score_nowcast()]; see there.
#'
#' @return A `tibble` with the columns `observed`, `predicted`, `quantile_level`
#'   and `model`, plus the event date and strata as forecast units.
#'
#' @seealso [score_nowcast()]
#'
#' @examples
#' predictions <- data.frame(
#'   onset_week = as.Date("2020-01-05"),
#'   .quantile_level = c(0.25, 0.5, 0.75), .value = c(8, 10, 13)
#' )
#' nc <- tbl_nowcast(predictions = predictions, method = "toy", event_date = "onset_week")
#' as_scoringutils(nc, truth = data.frame(onset_week = as.Date("2020-01-05"), .observed = 11))
#'
#' @export
as_scoringutils <- function(x, truth = NULL, observed_col = NULL) {
  .assert_tbl_nowcast(x)

  key <- c(x@event_date, x@strata)

  resolved <- .resolve_truth(truth, observed_col, x, "as_scoringutils")
  truth <- resolved$truth
  observed_col <- resolved$observed_col

  if (is.null(observed_col)) {
    observed_col <- utils::tail(setdiff(colnames(truth), key), 1)
  }

  x@predictions |>
    dplyr::inner_join(
      truth |> dplyr::select(dplyr::all_of(c(key, observed_col))),
      by = key
    ) |>
    dplyr::rename(
      predicted = ".value",
      quantile_level = ".quantile_level",
      observed = dplyr::all_of(observed_col)
    ) |>
    dplyr::mutate(model = x@method)
}
