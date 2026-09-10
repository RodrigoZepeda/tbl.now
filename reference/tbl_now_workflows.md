# Native and cross-engine nowcasting workflows

`tbl.now` owns the data declaration and common result grammar; modelling
packages own their statistical models and fit-specific diagnostics.
Choose the entry point according to which layer you need. Once a model
returns a
[tbl_nowcast](https://rodrigozepeda.github.io/tbl.now/reference/tbl_nowcast.md),
the downstream workflow is shared.

## Value

`NULL`, invisibly. This page documents workflow choices rather than a
callable object.

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
data(denguedat)
recent <- subset(denguedat, onset_week >= as.Date("2010-06-01"))
dengue <- tbl_now(recent,
  event_date = onset_week, report_date = report_week, verbose = FALSE
)
fit <- run_nowcast(dengue, example_engine())
#> ℹ Nowcasting with "example" as of 2010-12-20.
autoplot(fit)


bt <- nowcast_backtest(
  dengue,
  example_engine(label = "carry forward"),
  now_dates = as.Date(c("2010-10-04", "2010-11-15")),
  verbose = FALSE
)
head(bt$scores)
#> # A tibble: 6 × 8
#>   .method       .now       onset_week .observed   wis ae_median coverage_50
#>   <chr>         <date>     <date>         <dbl> <dbl>     <dbl> <lgl>      
#> 1 carry forward 2010-10-04 2010-06-07       157  3.84         0 TRUE       
#> 2 carry forward 2010-10-04 2010-06-14       210  5.13         0 TRUE       
#> 3 carry forward 2010-10-04 2010-06-21       193  4.68         0 TRUE       
#> 4 carry forward 2010-10-04 2010-06-28       193  4.68         0 TRUE       
#> 5 carry forward 2010-10-04 2010-07-05       258  6.28         0 TRUE       
#> 6 carry forward 2010-10-04 2010-07-12       315  7.6          0 TRUE       
#> # ℹ 1 more variable: coverage_90 <lgl>
```
