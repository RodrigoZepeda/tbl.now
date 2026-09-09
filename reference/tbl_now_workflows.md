# Native and cross-engine nowcasting workflows

`tbl.now` owns the data declaration and common result grammar; modelling
packages own their statistical models and fit-specific diagnostics.
Choose the entry point according to which layer you need. Once a model
returns a
[tbl_nowcast](https://rodrigozepeda.github.io/tbl.now/reference/tbl_nowcast.md),
the downstream workflow is shared.

## Use a modelling package's native entry point when

Use a package such as diseasenowcasting directly when you need its model
constructors, priors, fitting strategy, optimizer controls, or
fit-specific diagnostics. A compatible backend may already return a
[tbl_nowcast](https://rodrigozepeda.github.io/tbl.now/reference/tbl_nowcast.md),
so native fitting does not imply a separate result format. Its
package-specific methods can retain and unwrap the raw fit while generic
result operations use the common fields.

## Use the cross-engine entry point when

Use
[`engine()`](https://rodrigozepeda.github.io/tbl.now/reference/engine.md)
plus
[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
when you want the same fitting call across packages, or
[`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md)
when you want models evaluated on the same retrospective origins and
truth. Use
[autoplot()](https://rodrigozepeda.github.io/tbl.now/reference/autoplot.tbl_nowcast.md),
[tidy()](https://rodrigozepeda.github.io/tbl.now/reference/tidy.tbl_nowcast.md),
[`nowcast_ensemble()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_ensemble.md),
and
[`score_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/score_nowcast.md)
on the common result without converting it back to an engine-specific
object.

A
[`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md)
can be converted directly with
[`scoringutils::as_forecast_quantile()`](https://epiforecasts.io/scoringutils/reference/as_forecast_quantile.html),
[`scoringutils::as_forecast_point()`](https://epiforecasts.io/scoringutils/reference/as_forecast_point.html),
or, when `keep_draws = TRUE`,
[`scoringutils::as_forecast_sample()`](https://epiforecasts.io/scoringutils/reference/as_forecast_sample.html).
The resulting scoringutils object is the extension point for additional
metrics, summaries, pairwise comparisons, and relative skill.

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- run_nowcast(data, engine_diseasenowcasting())
autoplot(fit)

bt <- nowcast_backtest(
  data,
  engine_diseasenowcasting(label = "structural"),
  engine_epinowcast(label = "renewal")
)
relative <- bt |>
  scoringutils::as_forecast_quantile() |>
  scoringutils::score() |>
  scoringutils::add_relative_skill(metric = "wis")
} # }
```
