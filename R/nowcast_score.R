# Scoring nowcasts, and the retrospective backtest that feeds the
# performance-weighted ensemble.
#
# The scores are computed here rather than delegated to scoringutils so that
# weighting an ensemble never depends on an extra package. The scoringutils
# coercion helpers are provided for users who want its full score suite.

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

#' Everything eventually reported for each event date
#'
#' The "truth" a nowcast is scored against, and the reason the scoring functions
#' need no `observed_col`: the count column is read off the object rather than
#' named by the caller.
#'
#' This is [get_latest_reported_cases()] with the bookkeeping the scoring code
#' needs and the getter deliberately does not do -- the `tbl_now` class and the
#' report-date columns are dropped, any column that is neither the event date nor
#' a requested stratum is summed away, and the count is renamed `.observed`.
#'
#' A **line list** has no count column at all. `get_latest_reported_cases()`
#' aggregates it first (that is `to_count()`, applied for you), and the result
#' carries its count under `n` -- which is why the fallback below is `"n"` rather
#' than an error.
#'
#' It is **not** a second public way of asking the same question -- use
#' [get_latest_reported_cases()] for that. `score_nowcast()` and the
#' scoringutils coercion methods take the `tbl_now` itself as `truth` and call
#' this.
#'
#' @param x A `tbl_now` object holding the *full* data, including the reports
#'   that arrived after the nowcast's `now`.
#' @param strata Character vector of strata columns to keep.
#' @param truth_axis Which process defines the observed counts: `"report"` for
#'   counts eventually reported, or `"revision"` for counts eventually
#'   resolved on the revision axis.
#' @param truth_type Which case type to score. See [get_latest_reported_cases()]
#'   and [get_latest_revised_cases()] for the accepted values.
#' @param complete_grid Logical. Whether missing event/stratum cells inside
#'   `x`'s surveillance grid should be filled with zero.
#'
#' @return A `tibble` with the event-date column, the strata columns and
#'   `.observed`.
#'
#' @keywords internal
#' @noRd
.eventual_counts <- function(x, strata = get_strata(x),
                             truth_axis = c("report", "revision"),
                             truth_type = "total", complete_grid = FALSE) {
  .assert_tbl_now(x, "truth")
  truth_axis <- match.arg(truth_axis)
  .check_truth_type(truth_type)

  event_col <- get_event_date(x)
  strata <- intersect(strata %||% character(0), colnames(x))

  observed <- if (identical(truth_axis, "report")) {
    get_latest_reported_cases(ungroup(x), type = truth_type)
  } else {
    get_latest_revised_cases(ungroup(x), type = truth_type)
  }
  # `.cases_at()` names the count after the object's `case_count`, or
  # `n` when the source was a line list (which it has just aggregated).
  count_col <- get_case_count(observed) %||% "n"

  counts <- .declass_tbl_now(observed) |>
    dplyr::as_tibble() |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(event_col, strata)))) |>
    dplyr::summarise(.observed = sum(.data[[count_col]], na.rm = TRUE), .groups = "drop")

  if (isTRUE(complete_grid)) {
    counts <- .complete_truth_grid(x, counts, strata)
  }
  counts
}

#' Check scoring truth type
#'
#' `by_type` deliberately returns more than one row per event/stratum, while the
#' scoring table has exactly one observed value per target.
#'
#' @param truth_type User-supplied truth type.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
.check_truth_type <- function(truth_type) {
  if (!is.character(truth_type) || length(truth_type) != 1L || is.na(truth_type)) {
    cli::cli_abort("{.arg truth_type} must be a single string.")
  }
  rlang::arg_match0(truth_type, .case_types(), arg_nm = "truth_type")
  if (identical(truth_type, "by_type")) {
    cli::cli_abort(c(
      "{.code truth_type = \"by_type\"} cannot be scored as one observed value.",
      "i" = "Use one of {.val {setdiff(.case_types(), 'by_type')}}."
    ))
  }
  invisible(NULL)
}

#' Complete missing truth cells that are inside the surveillance grid
#'
#' A `tbl_now` does not have to store zero-count rows. If the prediction target
#' is a date/stratum combination inside the truth object's own grid, an absent
#' observed row means zero. Targets outside this completed grid remain missing
#' and are handled by `.warn_missing_truth_targets()`.
#'
#' @param x The `tbl_now` used as truth.
#' @param counts Observed counts by event date and strata.
#' @param strata Character vector of strata columns to keep.
#'
#' @return `counts`, completed over event date and observed stratum
#'   combinations, with missing `.observed` values set to zero.
#'
#' @keywords internal
#' @noRd
.complete_truth_grid <- function(x, counts, strata) {
  event_col <- get_event_date(x)
  event_values <- dplyr::pull(dplyr::as_tibble(x), dplyr::all_of(event_col))
  event_dates <- .tbl_now_date_seq(
    min(event_values, na.rm = TRUE), get_now(x), get_event_units(x)
  )

  if (length(strata) == 0L) {
    grid <- tibble::tibble(!!event_col := event_dates)
  } else {
    stratum_grid <- x |>
      dplyr::as_tibble() |>
      dplyr::distinct(dplyr::across(dplyr::all_of(strata)))
    grid <- tidyr::expand_grid(
      tibble::tibble(!!event_col := event_dates), stratum_grid
    )
  }

  grid |>
    dplyr::left_join(counts, by = c(event_col, strata)) |>
    dplyr::mutate(.observed = dplyr::coalesce(.data$.observed, 0))
}

#' Resolve the `truth` argument of the scoring functions
#'
#' The full `tbl_now` -- everything eventually reported for each event date -- or
#' `NULL` to fall back to the nowcast's own source data.
#'
#' Before 0.27.0 a plain data frame was also accepted, together with an
#' `observed_col` naming its count column. That put the burden on the caller to
#' say which column held the truth, when the `tbl_now` already knows: it is
#' `get_case_count()`, or the count `to_count()` produces from a line list.
#' Guessing (the old default was "the last column that is neither the event date
#' nor a stratum") is exactly the kind of silent mis-scoring worth removing.
#'
#' @param truth The user's `truth` argument.
#' @param x The `tbl_nowcast` being scored.
#'
#' @return A tibble with the event date, the strata and `.observed`.
#'
#' @keywords internal
#' @noRd
.resolve_truth <- function(truth, x, truth_axis = c("report", "revision"),
                           truth_type = "total") {
  truth_axis <- match.arg(truth_axis)
  if (is.null(truth)) {
    if (is.null(x@data)) {
      cli::cli_abort(c(
        "{.arg truth} is required: the nowcast does not carry its source data.",
        "i" = "Pass the full {.cls tbl_now} the nowcast was made from."
      ))
    }
    return(.eventual_counts(
      x@data, strata = x@strata,
      truth_axis = truth_axis, truth_type = truth_type,
      complete_grid = TRUE
    ))
  }

  if (!is_tbl_now(truth)) {
    cli::cli_abort(c(
      "{.arg truth} must be a {.cls tbl_now}, or {.code NULL} to reuse the \\
       nowcast's own source data.",
      "i" = "The observed counts are read off the object with \\
             {.fn get_case_count} (a line list is aggregated first), so there \\
             is no column to name."
    ))
  }
  .eventual_counts(
    truth, strata = x@strata,
    truth_axis = truth_axis, truth_type = truth_type,
    complete_grid = TRUE
  )
}

#' Score predictions against a resolved truth table
#'
#' The engine behind [score_nowcast()], split out so [nowcast_backtest()] can
#' compute the truth once and score every fit against the same table rather than
#' re-collapsing the `tbl_now` for each of `methods x dates` fits.
#'
#' @param x A `tbl_nowcast`.
#' @param truth A tibble with the event date, the strata and `.observed`.
#'
#' @return A scored tibble, as [score_nowcast()] documents.
#'
#' @keywords internal
#' @noRd
.score_against <- function(x, truth) {
  key <- c(x@event_date, x@strata)

  missing_key <- setdiff(key, colnames(truth))
  if (length(missing_key) > 0) {
    cli::cli_abort(c(
      "{.arg truth} has no column{?s} {.val {missing_key}}.",
      "i" = "The nowcast is keyed by {.val {key}}; the truth must carry the same \\
             columns."
    ))
  }

  .warn_missing_truth_targets(x@predictions, truth, key)

  x@predictions |>
    dplyr::inner_join(
      dplyr::select(truth, dplyr::all_of(c(key, ".observed"))),
      by = key
    ) |>
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

#' Warn about prediction targets that have no truth row
#'
#' `.eventual_counts()` fills zeros for missing observations inside a
#' `tbl_now`'s own surveillance grid. Anything still absent at this point is
#' outside the truth the user supplied and cannot be scored.
#'
#' @param predictions Prediction table.
#' @param truth Resolved truth table.
#' @param key Event-date and stratum columns.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
.warn_missing_truth_targets <- function(predictions, truth, key) {
  missing <- predictions |>
    dplyr::distinct(dplyr::across(dplyr::all_of(key))) |>
    dplyr::anti_join(
      dplyr::distinct(truth, dplyr::across(dplyr::all_of(key))),
      by = key
    )

  if (nrow(missing) == 0L) {
    return(invisible(NULL))
  }

  sample <- .format_target_sample(missing)
  cli::cli_warn(c(
    "{nrow(missing)} prediction target{?s} {?is/are} absent from {.arg truth} \\
     and cannot be scored.",
    "i" = "Missing observations inside the {.cls tbl_now} truth grid are scored \\
           as zero; the dropped targets are outside that grid or have unmatched \\
           strata.",
    "i" = "Sample dropped targets: {.val {sample}}."
  ))
  invisible(NULL)
}

#' Format a few target rows for a warning
#'
#' @param targets A data frame of target key columns.
#' @param n Maximum number of rows to format.
#'
#' @return A character vector.
#'
#' @keywords internal
#' @noRd
.format_target_sample <- function(targets, n = 3L) {
  sample <- utils::head(dplyr::as_tibble(targets), n)
  vapply(seq_len(nrow(sample)), function(i) {
    values <- vapply(sample, function(column) as.character(column[[i]]), character(1))
    paste(paste(names(values), values, sep = " = "), collapse = ", ")
  }, character(1))
}

#' Score a nowcast against observed data
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' A nowcast is a claim about numbers that are not in yet. Once the late reports
#' arrive you can ask how good the claim was.
#'
#' * `score_nowcast()` scores it here: the **weighted interval score** (WIS,
#'   lower is better), the absolute error of the median, and whether the truth
#'   fell inside the 50% and 90% intervals -- one row per event date and stratum.
#' * `as_forecast_point()` hands the median prediction and the same truth to
#'   \pkg{scoringutils}, so you can use its point-score functions and plots.
#' * `scoringutils::as_forecast_quantile()` and
#'   `scoringutils::as_forecast_sample()` also accept these objects directly
#'   when \pkg{scoringutils} is installed.
#'
#' In each case `truth` is a `tbl_now` seen *later*, after the information the
#' nowcast was predicting has arrived. The observed counts are computed from
#' `truth_axis` and `truth_type`: by default this is
#' [get_latest_reported_cases()][get_latest_first] with `type = "total"`, while
#' `truth_axis = "revision"` uses [get_latest_revised_cases()]. There is no
#' observed column to name; the count column is read from the `tbl_now`.
#'
#' @param x For `score_nowcast()`, a [tbl_nowcast]. For `as_forecast_point()`, a
#'   [tbl_nowcast] (including an ensemble) or a [nowcast_backtest()].
#' @param truth The `tbl_now` the nowcast is scored against -- normally the
#'   *full* object, still holding the reports or revisions that arrived after
#'   the nowcast's `now`. Its observed counts per event date are worked out from
#'   `truth_axis` and `truth_type`, aggregated over anything that is not a
#'   stratum, with the count column read off the object ([get_case_count()]). A
#'   **line list** is aggregated first, so it needs no special handling.
#'
#'   For a single nowcast, `NULL` (default) uses the `tbl_now` it was built from,
#'   which is only meaningful when that object still holds the later reports.
#'   A backtest instead uses the truth table it already stores.
#' @param truth_axis Which process defines the observed counts. `"report"`
#'   (default) scores counts eventually reported. `"revision"` scores counts
#'   eventually resolved on the revision axis and requires a revision-aware
#'   `truth`.
#' @param truth_type Which case type to score. Defaults to `"total"`. Revision
#'   types such as `"confirmed"`, `"retracted"`, `"pending"`, `"unknown"` and
#'   `"net"` follow the same meanings as [get_latest_reported_cases()] and
#'   [get_latest_revised_cases()]. `"by_type"` is refused because scoring needs
#'   one observed value per event-date/stratum target.
#'
#' @return
#' `score_nowcast()` returns a `tibble` with the event-date column, the strata
#' columns, and the columns `.observed`, `wis`, `ae_median`, `coverage_50` and
#' `coverage_90` -- one row per event date and stratum.
#'
#' `as_forecast_point()` accepts either a single [tbl_nowcast] (including one
#' returned by [nowcast_ensemble()]) or a [nowcast_backtest()]. It returns a
#' `forecast_point` object from \pkg{scoringutils}, using the nowcast's median
#' quantile as `predicted` and the resolved truth as `observed`.
#'
#' The `scoringutils::as_forecast_*()` methods return the corresponding
#' `forecast_quantile`, `forecast_sample` or `forecast_point` object from
#' \pkg{scoringutils}.
#'
#' When \pkg{scoringutils} is installed, calling its coercion generic directly
#' is equivalent: `scoringutils::as_forecast_quantile(x, truth = truth)` and
#' `scoringutils::as_forecast_point(x, truth = truth)` work for a
#' [tbl_nowcast], an ensemble, and a [nowcast_backtest()]. A backtest already
#' carries the truth it was scored against, so its `truth` can normally be
#' omitted.
#'
#' [scoringutils::as_forecast_sample()] also accepts those objects when they
#' carry posterior draws. Draws are retained by a `linear_pool` ensemble, but
#' not by a quantile ensemble. A backtest retains them only when run with
#' `keep_draws = TRUE`; every engine in the backtest must return draws.
#'
#' @references
#' Bracher, J., Ray, E. L., Gneiting, T., & Reich, N. G. (2021). Evaluating
#' epidemic forecasts in an interval format. *PLoS Computational Biology*,
#' 17(2), e1008618.
#'
#' @seealso
#' [nowcast_backtest()] to score many nowcasts at many `now` dates at once;
#' [nowcast_weights()] to turn those scores into ensemble weights;
#' [get_latest_reported_cases()][get_latest_first], which is how the truth is
#' read off `truth`; [nowcast_quantile_levels()] for the levels being scored.
#'
#' @examples
#' # A nowcast and the truth it should be judged against. Both are built by
#' # hand here so that the example needs no modelling package; in practice `nc`
#' ## comes from run_nowcast() and `truth` is the same data seen later, once the
#' # late reports have arrived.
#' truth_df <- data.frame(
#'   onset  = rep(as.Date("2024-03-04") + 7 * (0:3), each = 3),
#'   report = rep(as.Date("2024-03-04") + 7 * (0:3), each = 3) + c(0, 7, 14),
#'   n      = c(5, 3, 2, 8, 4, 1, 6, 5, 3, 9, 2, 2)
#' )
#' truth <- tbl_now(truth_df,
#'   event_date = onset, report_date = report, case_count = n,
#'   data_type = "count-incidence", verbose = FALSE
#' )
#'
#' # What eventually turned out to be true for each week.
#' get_latest_reported_cases(truth)
#'
#' # A nowcast that predicted 8 / 10 / 13 for every week.
#' levels <- c(0.25, 0.5, 0.75)
#' preds <- tidyr::expand_grid(
#'   onset = unique(truth_df$onset), .quantile_level = levels
#' )
#' preds$.value <- rep(c(8, 10, 13), times = 4)
#' nc <- tbl_nowcast(predictions = preds, method = "toy", event_date = "onset")
#'
#' # Lower `wis` is better. `coverage_50` says whether the truth fell inside
#' # the 50% interval, which it should about half the time.
#' score_nowcast(nc, truth = truth)
#'
#' # The same comparison handed to scoringutils as a point forecast.
#' if (requireNamespace("scoringutils", quietly = TRUE)) {
#'   as_forecast_point(nc, truth = truth)
#' }
#'
#' # With a real model, `truth` is the full object and the nowcast is fitted to
#' # a snapshot of it taken at an earlier `now`.
#' data(denguedat)
#'
#' recent <- subset(denguedat, onset_week >= as.Date("2010-06-01"))
#' dengue <- tbl_now(recent,
#'   event_date = onset_week, report_date = report_week, verbose = FALSE
#' )
#' snapshot <- change_now(
#'   dplyr::filter(dengue, report_week <= as.Date("2010-10-04")),
#'   now = as.Date("2010-10-04")
#' )
#'
#' if (requireNamespace("baselinenowcast", quietly = TRUE)) {
#'   nc <- run_nowcast(snapshot, engine_baselinenowcast(draws = 100), verbose = FALSE)
#'   # The FULL object is the truth: it still holds the reports that arrived
#'   # after the snapshot's `now`.
#'   score_nowcast(nc, truth = dengue)
#' }
#'
#' @export
score_nowcast <- function(x, truth = NULL, truth_axis = c("report", "revision"),
                          truth_type = "total") {
  .assert_tbl_nowcast(x)
  truth_axis <- match.arg(truth_axis)
  .score_against(
    x,
    .resolve_truth(
      truth, x, truth_axis = truth_axis, truth_type = truth_type
    )
  )
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
#' that snapshot, and the resulting nowcast is scored against the resolved truth
#' defined by `truth_axis` and `truth_type` (reported totals by default). This is
#' what turns a set of models into ensemble weights (see [nowcast_weights()] and
#' [nowcast_ensemble()]).
#'
#' Be aware that this refits every model once per date: with Bayesian backends
#' and a long `now_dates` it is genuinely expensive.
#'
#' @param x A `tbl_now` object holding the *full* data (the later reports or
#'   revisions are what the retrospective nowcasts are scored against).
#'   Covariates on each retrospective snapshot should mean values available as
#'   of that snapshot's `now`; do not attach future realized covariate values
#'   unless they are an explicit forecast input for the engine.
#' @param ... The [engine()] objects to backtest, one per model. Each carries its
#'   own arguments, so there is no keyed side-table of per-method options to get
#'   wrong.
#'
#'   Give an engine a `label` (or name the argument) when the same package
#'   appears twice: `engine_diseasenowcasting(label = "ar1", model = ...)` and a
#'   plain `engine_diseasenowcasting()` are backtested separately, so
#'   [nowcast_weights()] can learn a weight for each -- matching how
#'   [nowcast_ensemble()] takes a named list of members. An engine with no label
#'   is labelled by its method.
#' @param now_dates Vector of retrospective nowcast origins. Defaults to the
#'   `n_dates` most recent report-axis dates that are at least `horizon` units
#'   before the object's `now`; these dates are used as as-of origins, not as a
#'   filter on target event dates.
#' @param n_dates Number of automatic retrospective origins. Default `4`.
#'   Ignored when `now_dates` is supplied explicitly.
#' @param horizon Number of time units of hindsight required when `now_dates` is
#'   chosen automatically. Default `4`.
#' @param seed Optional integer. When given, the RNG is seeded **immediately
#'   before each fit**, from `seed` and the label and date that fit is for. One
#'   `set.seed()` before the whole backtest is not enough: it only pins anything
#'   if every method consumes the same random numbers in the same order, so
#'   dropping a method, or refitting one date, silently moves every other fit.
#'   Seeding per (label, date) makes a fit depend only on which fit it is.
#' @param keep_draws Logical. Whether to retain every posterior draw from every
#'   successful fit. Default `FALSE`, because this can make a backtest much
#'   larger. Set it to `TRUE` when the backtest should be passed directly to
#'   [scoringutils::as_forecast_sample()]. Engines that return only quantiles
#'   still cannot be converted to samples.
#' @param on_error Either `"warn"` (default) to skip a model/date that fails
#'   with a warning, or `"abort"` to stop.
#' @param verbose Logical. Whether to report progress.
#' @inheritParams score_nowcast
#'
#' @return An object of class `nowcast_backtest`: a list with
#'
#'   \describe{
#'     \item{scores}{A `tibble` of per-date scores with an extra `.now` column.}
#'     \item{predictions}{A `tibble` of every retrospective quantile prediction.}
#'     \item{draws}{When `keep_draws = TRUE`, a `tibble` of the retained draws;
#'       otherwise `NULL`.}
#'     \item{timings}{A `tibble` with one row per attempted engine/date fit,
#'       its elapsed time in seconds, whether it succeeded, and any error text.}
#'     \item{truth}{The observed counts used for scoring.}
#'     \item{methods}{The labels that produced at least one nowcast.}
#'     \item{now_dates}{The dates that were nowcast.}
#'   }
#'
#' @section Use the result directly with scoringutils:
#'
#' A `nowcast_backtest` has methods for all three standard forecast formats, so
#' no manual reshaping is needed:
#'
#' ```r
#' quantile_forecast <- scoringutils::as_forecast_quantile(bt)
#' point_forecast <- scoringutils::as_forecast_point(bt)
#' ```
#'
#' For sample forecasts, create the backtest with `keep_draws = TRUE` and
#' use `scoringutils::as_forecast_sample(bt)`. The returned forecast objects can
#' be passed to any compatible scoringutils workflow. For example, relative WIS
#' is obtained with:
#'
#' ```r
#' relative_scores <- quantile_forecast |>
#'   scoringutils::score() |>
#'   scoringutils::add_relative_skill(metric = "wis")
#' ```
#'
#' `model`, `now`, the event-date column, and declared strata are retained as
#' forecast units, allowing scores to be extended, regrouped, or summarised
#' without returning to the internal `tbl.now` representation.
#'
#' @section Every engine must report the same quantile levels:
#'
#' A backtest exists to compare models, and two models summarised at different
#' levels are not comparable: the weighted interval score is an average over the
#' levels reported, so a model asked for three of them and one asked for nine are
#' scoring different quantities. Mismatched engines are therefore an **error**
#' rather than a warning.
#'
#' This matters most for the engines where the levels are a *fit-time* argument.
#' \pkg{NobBS} computes exactly the quantiles it is handed and keeps no draws, so
#' a level it was never asked for cannot be recovered afterwards -- and an
#' ensemble weighted from such a backtest would silently fall back to whatever
#' levels its members happened to share.
#'
#' @seealso
#' [engine()] to specify each model being compared, and its `min_date` argument,
#' which matters here because `now` moves between fits;
#' [score_nowcast()] for the scores computed at each `now`;
#' [nowcast_weights()] to turn the result into ensemble weights, and
#' [nowcast_ensemble()] to use them. The
#' [*One call, many models* article](https://rodrigozepeda.github.io/tbl.now/articles/ensemble-nowcasting.html)
#' compares several packages this way.
#'
#' @examples
#' data(denguedat)
#'
#' # A short recent window keeps the example quick.
#' recent <- subset(denguedat, onset_week >= as.Date("2010-06-01"))
#' dengue <- tbl_now(recent,
#'   event_date = onset_week, report_date = report_week, verbose = FALSE
#' )
#'
#' ## `example_engine()` is a toy that ignores the reporting delay entirely; it
#' # is used here only so the example runs without a modelling package.
#' ## Swap in a real one -- `engine_baselinenowcast()`, `engine_epinowcast()`,
#' ## `engine_nobbs()` -- for anything you intend to act on.
#'
#' # Refit at two past `now` dates and score each against what is known now.
#' bt <- nowcast_backtest(dengue,
#'   example_engine(label = "carry forward"),
#'   now_dates = as.Date(c("2010-10-04", "2010-11-15")),
#'   verbose = FALSE
#' )
#' head(bt$scores)
#'
#' # Naming several engines compares them on identical data and dates.
#' bt$methods
#'
#' # With a real model the call is the same, with a real engine.
#' if (requireNamespace("baselinenowcast", quietly = TRUE)) {
#'   nowcast_backtest(dengue,
#'     engine_baselinenowcast(draws = 100),
#'     now_dates = as.Date("2010-11-15"), verbose = FALSE
#'   )$scores
#' }
#'
#' @export
nowcast_backtest <- function(x, ..., now_dates = NULL, horizon = 4,
                             n_dates = 4L,
                             seed = NULL, keep_draws = FALSE,
                             on_error = c("warn", "abort"), verbose = TRUE,
                             truth_axis = c("report", "revision"),
                             truth_type = "total") {
  .assert_tbl_now(x, "nowcast_backtest")
  on_error <- match.arg(on_error)
  truth_axis <- match.arg(truth_axis)

  engines <- .collect_engines(...)
  labels <- names(engines)

  if (is.null(now_dates)) {
    if (!is.numeric(n_dates) || length(n_dates) != 1L || is.na(n_dates) ||
        n_dates < 1 || n_dates != as.integer(n_dates)) {
      cli::cli_abort("{.arg n_dates} must be one positive whole number.")
    }
    now_dates <- .default_backtest_dates(
      x, horizon = horizon, n = as.integer(n_dates)
    )
  }
  if (length(now_dates) == 0) {
    cli::cli_abort("No usable {.arg now_dates}: the object has too little history to backtest.")
  }
  now_dates <- .validate_backtest_now_dates(x, now_dates)

  # Computed ONCE, not per fit: it is the same table for every (engine, date),
  # and collapsing the full object is not free on a long series.
  truth <- .eventual_counts(
    x, truth_axis = truth_axis, truth_type = truth_type,
    complete_grid = TRUE
  )

  results <- list()
  timings <- list()
  for (now_date in as.list(now_dates)) {
    snapshot <- .nowcast_snapshot(x, now_date)

    for (i in seq_along(engines)) {
      this_engine <- engines[[i]]
      label <- labels[[i]]
      if (isTRUE(verbose)) {
        cli::cli_alert_info("Backtesting {.val {label}} at {.val {now_date}}.")
      }

      if (!is.null(seed)) {
        # Seeded from the LABEL, so the two models of one package do not draw
        # the same numbers -- which would make them look more alike than they are.
        set.seed(.backtest_seed(seed, label, now_date))
      }

      fit_error <- NULL
      started <- proc.time()[["elapsed"]]
      nowcast <- tryCatch(
        run_nowcast(snapshot, this_engine, verbose = FALSE),
        error = function(e) {
          fit_error <<- conditionMessage(e)
          message <- c(
            "Engine {.val {label}} failed at {.val {now_date}}.",
            "x" = conditionMessage(e)
          )
          if (on_error == "abort") cli::cli_abort(message) else cli::cli_warn(message)
          NULL
        }
      )
      timings[[length(timings) + 1L]] <- dplyr::tibble(
        .method = label,
        .now = now_date,
        elapsed_seconds = unname(proc.time()[["elapsed"]] - started),
        success = !is.null(nowcast),
        error = fit_error %||% NA_character_
      )
      if (is.null(nowcast)) next

      # `.score_against()` labels with the fit's own `@method` -- the PACKAGE.
      # Overwrite with the backtest's label, or two models of one package would
      # score under one name here while their predictions carried two, and
      # `nowcast_weights()` would learn a single weight for both.
      scores <- .score_against(nowcast, truth) |>
        dplyr::mutate(.method = label)
      predictions <- nowcast@predictions |>
        dplyr::mutate(.method = label, .now = now_date, .before = 1)
      draws <- if (isTRUE(keep_draws) && !is.null(nowcast@draws)) {
        nowcast@draws |>
          dplyr::mutate(.method = label, .now = now_date, .before = 1)
      } else {
        NULL
      }

      results[[length(results) + 1]] <- list(
        scores = scores, predictions = predictions, draws = draws
      )
    }
  }

  if (length(results) == 0) {
    cli::cli_abort("Every method failed at every date; nothing to score.")
  }

  scores <- dplyr::bind_rows(lapply(results, `[[`, "scores"))
  predictions <- dplyr::bind_rows(lapply(results, `[[`, "predictions"))
  draw_tables <- lapply(results, `[[`, "draws")
  draw_tables <- draw_tables[!vapply(draw_tables, is.null, logical(1))]
  draws <- if (length(draw_tables) == 0L) {
    NULL
  } else {
    dplyr::bind_rows(draw_tables)
  }
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
      draws = draws,
      timings = dplyr::bind_rows(timings),
      truth = truth,
      methods = unique(scores$.method),
      now_dates = now_dates,
      keep_draws = isTRUE(keep_draws),
      event_date = get_event_date(x),
      strata = intersect(get_strata(x) %||% character(0), colnames(predictions)),
      truth_axis = truth_axis,
      truth_type = truth_type
    ),
    class = "nowcast_backtest"
  )
}

#' Validate retrospective nowcast origins
#'
#' The backtest walks through as-of dates on the report axis. They must be
#' inside the observed surveillance window; `get_now(x)` itself is allowed with
#' a warning because it is often today and therefore incomplete.
#'
#' @param x A `tbl_now`.
#' @param now_dates User-supplied or default nowcast origins.
#'
#' @return A sorted unique Date vector.
#'
#' @keywords internal
#' @noRd
.validate_backtest_now_dates <- function(x, now_dates) {
  if (!lubridate::is.Date(now_dates)) {
    cli::cli_abort("{.arg now_dates} must be a Date vector.")
  }
  if (length(now_dates) < 1L || any(is.na(now_dates))) {
    cli::cli_abort("{.arg now_dates} must contain at least one non-missing Date.")
  }

  now_dates <- sort(unique(now_dates))
  earliest <- .backtest_earliest_now_date(x)
  before_data <- now_dates < earliest
  if (any(before_data)) {
    cli::cli_abort(c(
      "{.arg now_dates} must be on or after {.val {as.character(earliest)}}.",
      "i" = "That is the latest of the first event, report, and revision dates \\
             present in {.arg x}."
    ))
  }

  object_now <- get_now(x)
  future <- now_dates > object_now
  if (any(future)) {
    cli::cli_abort(c(
      "{.arg now_dates} must not be after the object's {.field now} \\
       ({.val {as.character(object_now)}}).",
      "i" = "Backtests are retrospective; for future origins, fit a normal \\
             {.fn run_nowcast} instead."
    ))
  }
  if (any(now_dates == object_now)) {
    cli::cli_warn(c(
      "{.arg now_dates} includes the object's current {.field now} \\
       ({.val {as.character(object_now)}}).",
      "i" = "If this is today, the data may still be incomplete."
    ))
  }

  now_dates
}

#' Earliest valid backtest origin
#'
#' A retrospective origin before any declared time axis exists cannot represent
#' data availability. Revision dates join the lower bound when present.
#'
#' @param x A `tbl_now`.
#'
#' @return A single Date.
#'
#' @keywords internal
#' @noRd
.backtest_earliest_now_date <- function(x) {
  candidates <- c(
    .min_present_date(x, get_event_date(x)),
    .min_present_date(x, get_report_date(x)),
    .min_present_date(x, get_revision_date(x))
  )
  max(candidates[!is.na(candidates)])
}

#' Minimum non-missing date in one column
#'
#' @param x A data frame.
#' @param column Column name, or `NULL`.
#'
#' @return A Date or `NA`.
#'
#' @keywords internal
#' @noRd
.min_present_date <- function(x, column) {
  if (is.null(column) || !column %in% colnames(x)) {
    return(as.Date(NA))
  }
  values <- dplyr::pull(dplyr::as_tibble(x), dplyr::all_of(column))
  values <- values[!is.na(values)]
  if (length(values) == 0L) {
    return(as.Date(NA))
  }
  min(values)
}

#' Collect and label the engines passed to `nowcast_backtest()`
#'
#' Accepts loose [engine()] arguments or a single list of them, labels each one,
#' and refuses the two ways a set of engines cannot be backtested: duplicate
#' labels, and disagreeing quantile levels.
#'
#' @param ... The `...` of [nowcast_backtest()].
#'
#' @return A named list of engines; the names are the labels.
#'
#' @keywords internal
#' @noRd
.collect_engines <- function(...) {
  engines <- list(...)
  # A list of engines is what `lapply()` over a set of configurations produces,
  # and refusing it would send the caller to `do.call()` for no reason.
  if (length(engines) == 1 && is.list(engines[[1]]) && !is_nowcast_engine(engines[[1]])) {
    engines <- engines[[1]]
  }

  if (length(engines) == 0) {
    cli::cli_abort(c(
      "{.fn nowcast_backtest} needs at least one engine.",
      "i" = "e.g. {.code nowcast_backtest(x, engine_baselinenowcast(), \\
             engine_nobbs(max_D = 10))}."
    ))
  }
  for (i in seq_along(engines)) {
    .assert_engine(engines[[i]], arg = paste0("engine ", i))
  }

  # An argument name wins over the engine's own `label`, so a call can rename a
  # member without rebuilding it.
  given <- names(engines) %||% rep("", length(engines))
  own <- vapply(engines, function(e) e$label, character(1))
  given[!nzchar(given)] <- own[!nzchar(given)]

  if (anyDuplicated(given)) {
    duplicated_labels <- unique(given[duplicated(given)])
    cli::cli_abort(c(
      "Duplicate engine label{?s} {.val {duplicated_labels}}.",
      "i" = "Each engine becomes one row per date in the result, so the labels \\
             have to be unique.",
      "i" = "Set one with {.code engine_diseasenowcasting(label = \"ar1\")}, or \\
             name the argument."
    ))
  }

  # See the "Every engine must report the same quantile levels" section: the WIS
  # averages over the levels reported, so engines summarised differently are not
  # measuring the same thing, and NobBS cannot be re-summarised afterwards.
  levels <- unique(lapply(engines, function(e) e$quantile_levels))
  if (length(levels) > 1) {
    cli::cli_abort(c(
      "The engines report different {.arg quantile_levels}.",
      "i" = "A backtest compares them, and the weighted interval score averages \\
             over the levels reported, so they must match.",
      "i" = "Levels found: {.val {lapply(levels, function(l) paste(l, collapse = ', '))}}."
    ))
  }

  stats::setNames(engines, given)
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
#' @param now Optional Date vector of nowcast origins to exclude from the
#'   weight-training window when `include_now = FALSE`. [nowcast_ensemble()]
#'   passes its members' own `now` values here when deriving performance
#'   weights.
#' @param include_now Logical. Should rows at `now` be allowed into the
#'   weight-training window? Default `FALSE`; set `TRUE` for an in-sample
#'   diagnostic.
#'
#' @return A named numeric vector of weights summing to 1.
#'
#' @seealso
#' [nowcast_backtest()], which produces the scores these weights come from;
#' [nowcast_ensemble()], which consumes them;
#' [engine()]'s `label` argument, which is what tells two configurations of the
#' same package apart in the result.
#'
#' @examples
#' data(denguedat)
#'
#' # A short recent window keeps the example quick.
#' recent <- subset(denguedat, onset_week >= as.Date("2010-06-01"))
#' dengue <- tbl_now(recent,
#'   event_date = onset_week, report_date = report_week, verbose = FALSE
#' )
#'
#' ## `example_engine()` is a toy that ignores the reporting delay entirely; it
#' # is used here only so the example runs without a modelling package.
#' ## Swap in a real one -- `engine_baselinenowcast()`, `engine_epinowcast()`,
#' ## `engine_nobbs()` -- for anything you intend to act on.
#'
#' # Two engines that differ in how wide they claim their intervals are.
#' bt <- nowcast_backtest(dengue,
#'   example_engine(spread = 0.2, label = "narrow"),
#'   example_engine(spread = 0.5, label = "wide"),
#'   now_dates = as.Date(c("2010-10-04", "2010-11-15")),
#'   verbose = FALSE
#' )
#'
#' # Weights sum to one, and the better-scoring engine takes the larger share.
#' nowcast_weights(bt)
#' sum(nowcast_weights(bt))
#'
#' ## Hand them to nowcast_ensemble() to pool the nowcasts they came from.
#'
#' @export
nowcast_weights <- function(backtest, type = c("inverse_score", "optim", "equal"),
                            now = NULL, include_now = FALSE, ...) {
  if (!inherits(backtest, "nowcast_backtest")) {
    cli::cli_abort("{.arg backtest} must be a {.cls nowcast_backtest} (see {.fn nowcast_backtest}).")
  }
  type <- match.arg(type)
  backtest <- .weight_training_window(backtest, now, include_now)

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

#' Restrict backtest rows used to train performance weights
#'
#' By default, an ensemble weighted from a backtest should not learn from the
#' same origin as the nowcasts it is combining. `nowcast_ensemble()` supplies
#' its members' `now` values here; direct calls can pass `now` explicitly.
#'
#' @param backtest A `nowcast_backtest`.
#' @param now Origin dates to exclude unless `include_now` is `TRUE`.
#' @param include_now Logical. Whether rows at `now` can train weights.
#'
#' @return A possibly filtered `nowcast_backtest`.
#'
#' @keywords internal
#' @noRd
.weight_training_window <- function(backtest, now, include_now) {
  if (!is.logical(include_now) || length(include_now) != 1L || is.na(include_now)) {
    cli::cli_abort("{.arg include_now} must be `TRUE` or `FALSE`.")
  }
  if (isTRUE(include_now) || is.null(now)) {
    return(backtest)
  }
  if (!lubridate::is.Date(now) || any(is.na(now))) {
    cli::cli_abort("{.arg now} must be a non-missing Date vector.")
  }

  now <- unique(now)
  old_n <- nrow(backtest$scores)
  backtest$scores <- backtest$scores |>
    dplyr::filter(!.data$.now %in% now)
  removed <- old_n - nrow(backtest$scores)

  if (removed == 0L) {
    return(backtest)
  }

  backtest$predictions <- backtest$predictions |>
    dplyr::filter(!.data$.now %in% now)
  if (!is.null(backtest$draws)) {
    backtest$draws <- backtest$draws |>
      dplyr::filter(!.data$.now %in% now)
  }

  cli::cli_warn(c(
    "Excluded {removed} backtest score row{?s} at the nowcast origin{?s} \\
     {.val {as.character(now)}} while training ensemble weights.",
    "i" = "Set {.code include_now = TRUE} to estimate in-sample weights."
  ))

  if (nrow(backtest$scores) == 0L) {
    cli::cli_abort(c(
      "No backtest rows remain to train ensemble weights after excluding \\
       {.arg now}.",
      "i" = "Use earlier {.arg now_dates}, or set {.code include_now = TRUE} \\
             for an in-sample diagnostic."
    ))
  }

  backtest$methods <- intersect(backtest$methods, unique(backtest$scores$.method))
  backtest
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

#' @rdname score_nowcast
#' @export
as_forecast_point <- function(x, truth = NULL,
                              truth_axis = c("report", "revision"),
                              truth_type = "total",
                              ...) {
  .need_pkg("scoringutils")
  scoringutils::as_forecast_point(
    as.data.frame(.as_scoringutils_point(
      x, truth = truth, truth_axis = truth_axis, truth_type = truth_type
    )),
    ...
  )
}

#' Build the long quantile frame understood by scoringutils
#'
#' @param x A [tbl_nowcast] or [nowcast_backtest()].
#' @param truth A `tbl_now` or `NULL`.
#'
#' @return A long tibble in scoringutils' quantile-forecast format.
#'
#' @keywords internal
#' @noRd
.as_scoringutils <- function(x, truth = NULL,
                             truth_axis = c("report", "revision"),
                             truth_type = "total") {
  truth_axis <- match.arg(truth_axis)
  if (inherits(x, "nowcast_backtest")) {
    key <- c(x$event_date, x$strata %||% character(0))
    return(.as_scoringutils_frame(
      x$predictions,
      .resolve_backtest_truth(
        x, truth, truth_axis = truth_axis, truth_type = truth_type
      ),
      key
    ))
  }

  .assert_tbl_nowcast(x)

  key <- c(x@event_date, x@strata)
  predictions <- x@predictions |>
    dplyr::mutate(.method = x@method)

  .as_scoringutils_frame(
    predictions,
    .resolve_truth(truth, x, truth_axis = truth_axis, truth_type = truth_type),
    key
  )
}

#' Build the point frame understood by scoringutils
#'
#' This wraps the internal quantile-frame builder and keeps only the median
#' prediction, which is scoringutils' point-forecast convention for quantile
#' forecasts.
#'
#' @param x A [tbl_nowcast] or [nowcast_backtest()].
#' @param truth A `tbl_now` or `NULL`.
#'
#' @return A tibble in scoringutils' point-forecast format.
#'
#' @keywords internal
#' @noRd
.as_scoringutils_point <- function(x, truth = NULL,
                                   truth_axis = c("report", "revision"),
                                   truth_type = "total") {
  point <- .as_scoringutils(
    x, truth = truth, truth_axis = truth_axis, truth_type = truth_type
  ) |>
    dplyr::filter(.near(.data$quantile_level, 0.5)) |>
    dplyr::select(-dplyr::all_of("quantile_level"))

  if (nrow(point) == 0L) {
    cli::cli_abort(
      "{.arg x} does not contain a median ({.code .quantile_level = 0.5}) prediction."
    )
  }

  point
}

#' Resolve the truth stored by, or supplied for, a backtest
#'
#' @param x A [nowcast_backtest()].
#' @param truth A `tbl_now` or `NULL`.
#'
#' @return A resolved truth table carrying `.observed`.
#'
#' @keywords internal
#' @noRd
.resolve_backtest_truth <- function(x, truth, truth_axis = c("report", "revision"),
                                    truth_type = "total") {
  truth_axis <- match.arg(truth_axis)
  resolved <- if (is.null(truth)) {
    x$truth
  } else {
    .eventual_counts(
      truth, strata = x$strata %||% character(0),
      truth_axis = truth_axis, truth_type = truth_type,
      complete_grid = TRUE
    )
  }
  if (is.null(resolved)) {
    cli::cli_abort(
      "{.arg truth} is required: the backtest does not carry its truth table."
    )
  }
  resolved
}

#' Build the long frame understood by scoringutils
#'
#' This is the one implementation behind `.as_scoringutils()` for a single
#' [tbl_nowcast], an ensemble and a [nowcast_backtest()]. Callers only normalise
#' where their predictions and truth live.
#'
#' @param predictions A long quantile table carrying `.method` and optionally
#'   `.now`.
#' @param truth A resolved truth table carrying `.observed`.
#' @param key Event-date and strata columns used to join the two.
#'
#' @return A long tibble in scoringutils' quantile-forecast format.
#'
#' @keywords internal
#' @noRd
.as_scoringutils_frame <- function(predictions, truth, key) {
  .warn_missing_truth_targets(predictions, truth, key)

  out <- predictions |>
    dplyr::inner_join(
      dplyr::select(truth, dplyr::all_of(c(key, ".observed"))),
      by = key
    ) |>
    dplyr::rename(
      predicted = ".value",
      quantile_level = ".quantile_level",
      observed = ".observed",
      model = ".method"
    )

  if (".now" %in% colnames(out)) {
    out <- dplyr::rename(out, now = ".now")
  }
  out
}

#' Coerce tbl.now nowcasts to scoringutils quantile forecasts
#'
#' These methods let [scoringutils::as_forecast_quantile()] consume the result
#' of [run_nowcast()], [nowcast_ensemble()] or [nowcast_backtest()] directly.
#' They first use `.as_scoringutils()` to attach the observed values, then let
#' \pkg{scoringutils} validate and construct its `forecast_quantile` class.
#'
#' @param data A [tbl_nowcast] or [nowcast_backtest()].
#' @param ... Passed to [scoringutils::as_forecast_quantile()], most commonly
#'   `forecast_unit`.
#' @param truth Optional full `tbl_now` used as truth. A backtest reuses its
#'   stored truth by default.
#'
#' @return A `forecast_quantile` object from \pkg{scoringutils}.
#'
#' @keywords internal
#' @noRd
as_forecast_quantile_tbl_nowcast <- function(data, ..., truth = NULL,
                                             truth_axis = c("report", "revision"),
                                             truth_type = "total") {
  scoringutils::as_forecast_quantile(
    as.data.frame(.as_scoringutils(
      data, truth = truth, truth_axis = truth_axis, truth_type = truth_type
    )),
    ...
  )
}

#' @rdname score_nowcast
#' @param data A [nowcast_backtest()].
#' @param ... Passed to the corresponding \pkg{scoringutils} coercion generic,
#'   most commonly `forecast_unit`.
#' @exportS3Method scoringutils::as_forecast_quantile
as_forecast_quantile.nowcast_backtest <- function(data, ..., truth = NULL,
                                                  truth_axis = c("report", "revision"),
                                                  truth_type = "total") {
  scoringutils::as_forecast_quantile(
    as.data.frame(.as_scoringutils(
      data, truth = truth, truth_axis = truth_axis, truth_type = truth_type
    )),
    ...
  )
}

#' Coerce tbl.now nowcasts to scoringutils point forecasts
#'
#' These methods let [scoringutils::as_forecast_point()] consume the result of
#' [run_nowcast()], [nowcast_ensemble()] or [nowcast_backtest()] directly. They
#' keep the median prediction and pass it to \pkg{scoringutils} with the
#' resolved observed value.
#'
#' @param data A [tbl_nowcast] or [nowcast_backtest()].
#' @param ... Passed to [scoringutils::as_forecast_point()], most commonly
#'   `forecast_unit`.
#' @param truth Optional full `tbl_now` used as truth. A backtest reuses its
#'   stored truth by default.
#'
#' @return A `forecast_point` object from \pkg{scoringutils}.
#'
#' @keywords internal
#' @noRd
as_forecast_point_tbl_nowcast <- function(data, ..., truth = NULL,
                                          truth_axis = c("report", "revision"),
                                          truth_type = "total") {
  scoringutils::as_forecast_point(
    as.data.frame(.as_scoringutils_point(
      data, truth = truth, truth_axis = truth_axis, truth_type = truth_type
    )),
    ...
  )
}

#' @rdname score_nowcast
#' @param data A [nowcast_backtest()].
#' @param ... Passed to the corresponding \pkg{scoringutils} coercion generic,
#'   most commonly `forecast_unit`.
#' @exportS3Method scoringutils::as_forecast_point
as_forecast_point.nowcast_backtest <- function(data, ..., truth = NULL,
                                               truth_axis = c("report", "revision"),
                                               truth_type = "total") {
  scoringutils::as_forecast_point(
    as.data.frame(.as_scoringutils_point(
      data, truth = truth, truth_axis = truth_axis, truth_type = truth_type
    )),
    ...
  )
}

#' Build the long sample frame understood by scoringutils
#'
#' @param draws A draw table carrying `.method` and optionally `.now`.
#' @param truth A resolved truth table carrying `.observed`.
#' @param key Event-date and strata columns used to join the two.
#'
#' @return A long tibble in scoringutils' sample-forecast format.
#'
#' @keywords internal
#' @noRd
.as_scoringutils_sample_frame <- function(draws, truth, key) {
  .warn_missing_truth_targets(draws, truth, key)

  out <- draws |>
    dplyr::inner_join(
      dplyr::select(truth, dplyr::all_of(c(key, ".observed"))),
      by = key
    ) |>
    dplyr::rename(
      predicted = ".value",
      sample_id = ".draw",
      observed = ".observed",
      model = ".method"
    )

  if (".now" %in% colnames(out)) {
    out <- dplyr::rename(out, now = ".now")
  }
  out
}

#' Coerce a tbl.now nowcast to a scoringutils sample forecast
#'
#' Unlike quantile coercion, this is available only when the nowcast carries
#' posterior draws. A quantile ensemble discards its members' draws; a
#' `linear_pool` ensemble retains the pooled draws and can be converted.
#'
#' @param data A [tbl_nowcast].
#' @param ... Passed to [scoringutils::as_forecast_sample()], most commonly
#'   `forecast_unit`.
#' @param truth Optional full `tbl_now` used as truth.
#'
#' @return A `forecast_sample` object from \pkg{scoringutils}.
#'
#' @keywords internal
#' @noRd
as_forecast_sample_tbl_nowcast <- function(data, ..., truth = NULL,
                                           truth_axis = c("report", "revision"),
                                           truth_type = "total") {
  .assert_tbl_nowcast(data, "data")
  if (is.null(data@draws)) {
    cli::cli_abort(c(
      "The {.cls tbl_nowcast} from {.val {data@method}} does not carry draws.",
      "i" = "Samples cannot be reconstructed from reported quantiles.",
      "i" = paste0(
        "Use {.fn scoringutils::as_forecast_quantile}, or refit with an ",
        "engine that returns draws."
      )
    ))
  }

  key <- c(data@event_date, data@strata)
  draws <- data@draws |>
    dplyr::mutate(.method = data@method)
  frame <- .as_scoringutils_sample_frame(
    draws,
    .resolve_truth(
      truth, data, truth_axis = truth_axis, truth_type = truth_type
    ),
    key
  )
  scoringutils::as_forecast_sample(as.data.frame(frame), ...)
}

#' Retained draws from every fit represented in a backtest
#'
#' @param x A [nowcast_backtest()].
#'
#' @return The backtest's draw table, after checking that every successful fit
#'   represented in its predictions has draws.
#'
#' @keywords internal
#' @noRd
.backtest_sample_draws <- function(x) {
  if (is.null(x$draws)) {
    hint <- if (isTRUE(x$keep_draws)) {
      "None of its engines returned posterior draws."
    } else {
      "Re-run {.fn nowcast_backtest} with {.code keep_draws = TRUE}."
    }
    cli::cli_abort(c(
      "The {.cls nowcast_backtest} does not carry draws.",
      "i" = hint,
      "i" = "Samples cannot be reconstructed from reported quantiles."
    ))
  }

  fit_key <- c(".method", ".now")
  prediction_fits <- x$predictions |>
    dplyr::distinct(dplyr::across(dplyr::all_of(fit_key)))
  draw_fits <- x$draws |>
    dplyr::distinct(dplyr::across(dplyr::all_of(fit_key)))
  missing <- dplyr::anti_join(prediction_fits, draw_fits, by = fit_key)
  if (nrow(missing) > 0L) {
    labels <- paste(missing$.method, "at", as.character(missing$.now))
    cli::cli_abort(c(
      "The backtest has no draws for {nrow(missing)} successful fit{?s}.",
      "i" = "Missing: {.val {labels}}.",
      "i" = paste0(
        "Remove quantile-only engines or use ",
        "{.fn scoringutils::as_forecast_quantile}."
      )
    ))
  }
  x$draws
}

#' @rdname score_nowcast
#' @param data A [nowcast_backtest()].
#' @param ... Passed to the corresponding \pkg{scoringutils} coercion generic,
#'   most commonly `forecast_unit`.
#' @exportS3Method scoringutils::as_forecast_sample
as_forecast_sample.nowcast_backtest <- function(data, ..., truth = NULL,
                                                truth_axis = c("report", "revision"),
                                                truth_type = "total") {
  key <- c(data$event_date, data$strata %||% character(0))
  frame <- .as_scoringutils_sample_frame(
    .backtest_sample_draws(data),
    .resolve_backtest_truth(
      data, truth, truth_axis = truth_axis, truth_type = truth_type
    ),
    key
  )
  scoringutils::as_forecast_sample(as.data.frame(frame), ...)
}
