#' @details
#' Surveillance data arrives late. A case that happened on Monday may not reach
#' the system until Thursday, so the most recent counts always look lower than
#' they will turn out to be. **Nowcasting** corrects that: it estimates how many
#' cases have already happened but have not been reported yet.
#'
#' `tbl.now` is the tidy scaffolding around that problem. You declare which
#' columns hold the event date, the report date and anything else that matters
#' once, and everything else -- describing, diagnosing, plotting, fitting,
#' scoring -- follows from that declaration.
#'
#' @section The workflow:
#'
#' 1. **Declare.** [tbl_now()] turns a `data.frame` into a `tbl_now`, or
#'    [as_tbl_now()] converts an object from another nowcasting package. The
#'    result is still a `tibble`, so `dplyr` keeps working.
#' 2. **Describe.** [summary()][tbl_now_summary] says what is in the data;
#'    [autoplot()][autoplot.tbl_now] draws it.
#' 3. **Diagnose.** [diagnose()] says what is *wrong* with it, and points at the
#'    statistical tests worth running -- [diagnose_drift()] for delays that are
#'    getting longer, [diagnose_batches()] for backlog releases,
#'    [diagnostic_plot()] for the reporting process as a picture.
#' 4. **Reshape.** [to_count()], [complete_zeroes()], [aggregate_time_units()],
#'    [align_weeks()] and [censor_reports()][censoring] put the data on the grid
#'    a model needs.
#' 5. **Fit.** [run_nowcast()] takes the data and an [engine()] -- one interface
#'    over \pkg{epinowcast}, \pkg{baselinenowcast}, \pkg{NobBS},
#'    \pkg{EpiNow2}, \pkg{surveillance} and \pkg{diseasenowcasting}. Write your
#'    own with [nowcast_fit()] and [nowcast_tidy()].
#' 6. **Check.** [score_nowcast()] and [nowcast_backtest()] say whether the
#'    nowcast was any good; [nowcast_ensemble()] combines several.
#'
#' `vignette("tbl.now")` walks through this end to end. The
#' [package website](https://rodrigozepeda.github.io/tbl.now/) carries longer
#' articles on the modelling packages, batch reporting, ensembles and writing
#' your own backend.
#'
#' @section Datasets:
#' Six surveillance datasets ship with the package for experimenting:
#' [denguedat], [mpoxdat], [flusight], [covid_colombia], [covid_us] and
#' [hai_bucaramanga] -- the last deliberately messy, for the diagnostics.
#'
#' @keywords internal
"_PACKAGE"

# enable usage of <S7_object>@name in package code
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL

## usethis namespace: start
#' @importFrom lifecycle deprecated
#' @importFrom rlang := %||%
#' @importFrom utils head
## usethis namespace: end
NULL
