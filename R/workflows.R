#' Native and cross-engine nowcasting workflows
#'
#' `tbl.now` owns the data declaration and common result grammar; modelling
#' packages own their statistical models and fit-specific diagnostics. Choose
#' the entry point according to which layer you need. Once a model returns a
#' [tbl_nowcast], the downstream workflow is shared.
#'
#' @section Use a modelling package's native entry point when:
#'
#' Use a package such as \pkg{diseasenowcasting} directly when you need its model
#' constructors, priors, fitting strategy, optimizer controls, or fit-specific
#' diagnostics. A compatible backend may already return a [tbl_nowcast], so
#' native fitting does not imply a separate result format. Its package-specific
#' methods can retain and unwrap the raw fit while generic result operations use
#' the common fields.
#'
#' @section Use the cross-engine entry point when:
#'
#' Use [engine()] plus [run_nowcast()] when you want the same fitting call across
#' packages, or [nowcast_backtest()] when you want models evaluated on the same
#' retrospective origins and truth. Use [autoplot()][autoplot.tbl_nowcast],
#' [tidy()][tidy.tbl_nowcast], [nowcast_ensemble()], and [score_nowcast()] on the
#' common result without converting it back to an engine-specific object.
#'
#' A [nowcast_backtest()] can be converted directly with
#' [scoringutils::as_forecast_quantile()],
#' [scoringutils::as_forecast_point()], or, when `keep_draws = TRUE`,
#' [scoringutils::as_forecast_sample()]. The resulting scoringutils object is
#' the extension point for additional metrics, summaries, pairwise comparisons,
#' and relative skill.
#'
#' @examples
#' \dontrun{
#' fit <- run_nowcast(data, engine_diseasenowcasting())
#' autoplot(fit)
#'
#' bt <- nowcast_backtest(
#'   data,
#'   engine_diseasenowcasting(label = "structural"),
#'   engine_epinowcast(label = "renewal")
#' )
#' relative <- bt |>
#'   scoringutils::as_forecast_quantile() |>
#'   scoringutils::score() |>
#'   scoringutils::add_relative_skill(metric = "wis")
#' }
#'
#' @name tbl_now_workflows
NULL
