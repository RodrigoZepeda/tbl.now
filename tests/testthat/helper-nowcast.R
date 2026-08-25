# Shared fixtures for the nowcasting-framework tests.
#
# A `helper-*.R` file is sourced into the test environment before any test file
# runs, so anything defined here is visible to all of them. The toy backends
# exist so that testing `run_nowcast()`, scoring, backtesting and ensembling
# never needs Stan, JAGS or INLA -- and never needs an MCMC run to finish.

# A deliberately controllable backend: predict the eventual counts, offset by
# `bias` and spread by `spread`, so a "good" and a "bad" model differ by one
# argument.
nowcast_fit.scoretoy <- function(method, x, ..., bias = 0, spread = 1,
                                 quantile_levels = nowcast_quantile_levels(),
                                 verbose = TRUE) {
  list(
    observed = tbl.now:::nowcast_truth(x),
    bias = bias, spread = spread, event_col = get_event_date(x)
  )
}

nowcast_tidy.scoretoy <- function(method, fit, x, ..., quantile_levels) {
  predictions <- fit$observed |>
    dplyr::reframe(
      !!fit$event_col := rep(.data[[fit$event_col]], each = length(quantile_levels)),
      .quantile_level = rep(quantile_levels, times = dplyr::n()),
      .value = rep(.data$.observed + fit$bias, each = length(quantile_levels)) +
        fit$spread * stats::qnorm(rep(quantile_levels, times = dplyr::n()))
    )
  list(predictions = predictions, draws = NULL)
}

#' Register the toy backend twice, so that a two-model ensemble can be built
#' from a single implementation with different `bias` arguments.
register_scoretoy <- function() {
  for (name in c("scoretoy", "scoretoy2")) {
    registerS3method("nowcast_fit", name, nowcast_fit.scoretoy,
      envir = asNamespace("tbl.now")
    )
    registerS3method("nowcast_tidy", name, nowcast_tidy.scoretoy,
      envir = asNamespace("tbl.now")
    )
  }
}

score_tbl_now <- function() {
  dates <- as.Date("2020-01-06") + seq(0, 7 * 29, by = 7)
  data <- tidyr::expand_grid(event_date = dates, delay = 0:3) |>
    dplyr::mutate(
      report_date = .data$event_date + 7 * .data$delay,
      n = as.integer(pmax(1, 40 - 8 * .data$delay))
    ) |>
    dplyr::select("event_date", "report_date", "n")

  tbl_now(data,
    event_date = "event_date", report_date = "report_date",
    case_count = "n", data_type = "count-incidence", verbose = FALSE
  )
}

