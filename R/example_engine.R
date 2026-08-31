# A backend that exists only so the documentation has something to run.
#
# Every real engine needs its modelling package, so an example that fits with one
# only runs where that package happens to be installed -- and three of them are
# not on CRAN at all. This engine needs nothing, is deterministic, and returns in
# milliseconds, so the examples for `run_nowcast()`, `nowcast_backtest()`,
# `score_nowcast()` and the rest show real output everywhere.

#' A toy engine for examples
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' A deliberately naive nowcasting engine that needs no modelling package. It
#' exists so that the examples in this package can actually run: every real
#' engine depends on \pkg{epinowcast}, \pkg{NobBS}, \pkg{EpiNow2} or another
#' optional package, and an example that cannot run teaches nothing.
#'
#' **Do not nowcast with this.** It does not model the reporting delay at all --
#' it reports the counts that have arrived so far and puts a fixed percentage
#' band around them. Because late reports are exactly what it ignores, it
#' under-predicts recent dates by design, which is a useful thing to *see* and a
#' terrible thing to rely on. For real work use one of the
#' [engines][nowcast_engines] -- [engine_baselinenowcast()][nowcast_engines],
#' [engine_epinowcast()][nowcast_engines], [engine_nobbs()][nowcast_engines] and
#' the rest -- or write your own.
#'
#' @details
#' For each event date (and stratum) it takes the cumulative count reported by
#' `now`, from [get_latest_reported_cases()][get_latest_first], and reports that
#' as the median. The other quantile levels are that median scaled linearly by
#' `spread`, so the 2.5% and 97.5% levels sit at roughly `1 -/+ spread` times it.
#'
#' No random numbers are involved, so it gives the same answer every time and
#' does not disturb the RNG stream.
#'
#' @param ... Ignored. Present so the engine accepts the same shape of call as
#'   the real ones.
#' @param spread Non-negative number setting the width of the interval, as a
#'   fraction of the point estimate. `0` gives a point mass at the median.
#' @inheritParams engine
#' @inheritParams nowcast_fit
#' @inheritParams nowcast_tidy
#'
#' @return A `nowcast_engine` object, as [engine()] returns, that
#'   [run_nowcast()] and [nowcast_backtest()] accept.
#'
#' @seealso
#' [nowcast_engines] for the engines you would actually nowcast with;
#' [engine()] for the general constructor;
#' [nowcast_fit()] and [nowcast_tidy()], the two methods this implements -- read
#' its source for the shortest possible complete backend. The
#' [*Adding your own nowcasting model* article](https://rodrigozepeda.github.io/tbl.now/articles/custom-nowcast-models.html)
#' walks through writing a real one.
#'
#' @examples
#' data(denguedat)
#' recent <- subset(denguedat, onset_week >= as.Date("2010-06-01"))
#' dengue <- tbl_now(recent,
#'   event_date = onset_week, report_date = report_week, verbose = FALSE
#' )
#'
#' # It is an ordinary engine, so it goes where a real one goes.
#' example_engine()
#'
#' nc <- run_nowcast(dengue, example_engine(), verbose = FALSE)
#' nc
#'
#' # `spread` controls how wide the (made-up) interval is.
#' run_nowcast(dengue, example_engine(spread = 0.5), verbose = FALSE)
#'
#' @export
example_engine <- function(..., spread = 0.2, min_date = NULL,
                           quantile_levels = nowcast_quantile_levels(),
                           label = NULL) {
  if (!is.numeric(spread) || length(spread) != 1L || is.na(spread) || spread < 0) {
    cli::cli_abort("{.arg spread} must be a single non-negative number.")
  }
  .new_engine("example", list(spread = spread), min_date, quantile_levels, label)
}

#' @rdname example_engine
#' @export
nowcast_fit.example <- function(engine, x, ..., spread = 0.2,
                                quantile_levels = nowcast_quantile_levels(),
                                verbose = TRUE) {
  list(counts = get_latest_reported_cases(x), spread = spread)
}

#' @rdname example_engine
#' @export
nowcast_tidy.example <- function(engine, fit, x, ..., quantile_levels) {
  counts <- fit$counts
  event_col <- get_event_date(x)
  strata <- get_strata(x) %||% character(0)
  count_col <- get_case_count(counts) %||% "n"

  keep <- c(event_col, strata)
  predictions <- dplyr::as_tibble(as.data.frame(counts)[, c(keep, count_col), drop = FALSE])
  names(predictions)[names(predictions) == count_col] <- ".point"

  # A deterministic band around the point estimate: level 0.5 returns it
  # unchanged, and the outermost levels sit at (1 -/+ spread) times it.
  predictions <- predictions |>
    tidyr::expand_grid(.quantile_level = sort(quantile_levels)) |>
    dplyr::mutate(
      .value = pmax(
        0,
        round(.data$.point * (1 + fit$spread * 2 * (.data$.quantile_level - 0.5)))
      )
    ) |>
    dplyr::select(-".point")

  list(predictions = predictions)
}
