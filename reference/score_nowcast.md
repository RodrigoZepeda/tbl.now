# Score a nowcast against observed data

**\[stable\]**

A nowcast is a claim about numbers that are not in yet. Once the late
reports arrive you can ask how good the claim was.

- `score_nowcast()` scores it here: the **weighted interval score**
  (WIS, lower is better), the absolute error of the median, and whether
  the truth fell inside the 50% and 90% intervals – one row per event
  date and stratum.

- [`scoringutils::as_forecast_point()`](https://epiforecasts.io/scoringutils/reference/as_forecast_point.html)
  hands the median prediction and the same truth to scoringutils, so you
  can use its point-score functions and plots.

- [`scoringutils::as_forecast_quantile()`](https://epiforecasts.io/scoringutils/reference/as_forecast_quantile.html)
  and
  [`scoringutils::as_forecast_sample()`](https://epiforecasts.io/scoringutils/reference/as_forecast_sample.html)
  accept the same objects directly.

All three are scoringutils generics; this package only supplies the
methods, so call them qualified (or after
[`library(scoringutils)`](https://doi.org/10.48550/arXiv.2205.07090)).

In each case `truth` is a `tbl_now` seen *later*, after the information
the nowcast was predicting has arrived. The observed counts are computed
from `truth_axis` and `truth_type`: by default this is
[get_latest_reported_cases()](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.md)
with `type = "total"`, while `truth_axis = "revision"` uses
[`get_latest_revised_cases()`](https://rodrigozepeda.github.io/tbl.now/reference/revised_cases.md).
There is no observed column to name; the count column is read from the
`tbl_now`.

## Usage

``` r
score_nowcast(
  x,
  truth = NULL,
  truth_axis = c("report", "revision"),
  truth_type = "total"
)

# S3 method for class 'nowcast_backtest'
as_forecast_quantile(
  data,
  ...,
  truth = NULL,
  truth_axis = c("report", "revision"),
  truth_type = "total"
)

# S3 method for class 'nowcast_backtest'
as_forecast_point(
  data,
  ...,
  truth = NULL,
  truth_axis = c("report", "revision"),
  truth_type = "total"
)

# S3 method for class 'nowcast_backtest'
as_forecast_sample(
  data,
  ...,
  truth = NULL,
  truth_axis = c("report", "revision"),
  truth_type = "total"
)
```

## Arguments

- x:

  A
  [tbl_nowcast](https://rodrigozepeda.github.io/tbl.now/reference/tbl_nowcast.md).

- truth:

  The `tbl_now` the nowcast is scored against – normally the *full*
  object, still holding the reports or revisions that arrived after the
  nowcast's `now`. Its observed counts per event date are worked out
  from `truth_axis` and `truth_type`, aggregated over anything that is
  not a stratum, with the count column read off the object
  ([`get_case_count()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)).
  A **line list** is aggregated first, so it needs no special handling.

  For a single nowcast, `NULL` (default) uses the `tbl_now` it was built
  from, which is only meaningful when that object still holds the later
  reports. A backtest instead uses the truth table it already stores.

- truth_axis:

  Which process defines the observed counts. `"report"` (default) scores
  counts eventually reported. `"revision"` scores counts eventually
  resolved on the revision axis and requires a revision-aware `truth`.

- truth_type:

  Which case type to score. Defaults to `"total"`. Revision types such
  as `"confirmed"`, `"retracted"`, `"pending"`, `"unknown"` and `"net"`
  follow the same meanings as
  [`get_latest_reported_cases()`](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.md)
  and
  [`get_latest_revised_cases()`](https://rodrigozepeda.github.io/tbl.now/reference/revised_cases.md).
  `"by_type"` is refused because scoring needs one observed value per
  event-date/stratum target.

- data:

  A
  [`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md).

- ...:

  Passed to the corresponding scoringutils coercion generic, most
  commonly `forecast_unit`.

## Value

`score_nowcast()` returns a `tibble` with the event-date column, the
strata columns, and the columns `.observed`, `wis`, `ae_median`,
`coverage_50` and `coverage_90` – one row per event date and stratum.

The `scoringutils::as_forecast_*()` methods return the corresponding
`forecast_quantile`, `forecast_sample` or `forecast_point` object from
scoringutils. Each accepts a
[tbl_nowcast](https://rodrigozepeda.github.io/tbl.now/reference/tbl_nowcast.md),
an ensemble and a
[`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md);
[`scoringutils::as_forecast_point()`](https://epiforecasts.io/scoringutils/reference/as_forecast_point.html)
keeps the nowcast's median quantile as `predicted` and the resolved
truth as `observed`. A backtest already carries the truth it was scored
against, so its `truth` can normally be omitted.

[`scoringutils::as_forecast_sample()`](https://epiforecasts.io/scoringutils/reference/as_forecast_sample.html)
also accepts those objects when they carry posterior draws. Draws are
retained by a `linear_pool` ensemble, but not by a quantile ensemble. A
backtest retains them only when run with `keep_draws = TRUE`; every
engine in the backtest must return draws.

## References

Bracher, J., Ray, E. L., Gneiting, T., & Reich, N. G. (2021). Evaluating
epidemic forecasts in an interval format. *PLoS Computational Biology*,
17(2), e1008618.

## See also

[`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md)
to score many nowcasts at many `now` dates at once;
[`nowcast_weights()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_weights.md)
to turn those scores into ensemble weights;
[get_latest_reported_cases()](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.md),
which is how the truth is read off `truth`;
[`nowcast_quantile_levels()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_quantile_levels.md)
for the levels being scored.

## Examples

``` r
# A nowcast and the truth it should be judged against. Both are built by
# hand here so that the example needs no modelling package; in practice `nc`
## comes from run_nowcast() and `truth` is the same data seen later, once the
# late reports have arrived.
truth_df <- data.frame(
  onset  = rep(as.Date("2024-03-04") + 7 * (0:3), each = 3),
  report = rep(as.Date("2024-03-04") + 7 * (0:3), each = 3) + c(0, 7, 14),
  n      = c(5, 3, 2, 8, 4, 1, 6, 5, 3, 9, 2, 2)
)
truth <- tbl_now(truth_df,
  event_date = onset, report_date = report, case_count = n,
  data_type = "count-incidence", verbose = FALSE
)

# What eventually turned out to be true for each week.
get_latest_reported_cases(truth)
#> # A tibble:  4 × 6
#> # Data type: "count-cumulative"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>   onset        report        .event_num .report_num       n .delay
#>   <date>       <date>             <dbl>       <dbl>   <dbl>  <dbl>
#>   [event_date] [report_date]      [...]       [...] [cases]  [...]
#> 1 2024-03-04   2024-03-18             0           2      10      2
#> 2 2024-03-11   2024-03-25             1           3      13      2
#> 3 2024-03-18   2024-04-01             2           4      14      2
#> 4 2024-03-25   2024-04-08             3           5      13      2
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2024-04-08 | Event date: "onset" | Report date: "report"
#> # ────────────────────────────────────────────────────────────────────────────────

# A nowcast that predicted 8 / 10 / 13 for every week.
levels <- c(0.25, 0.5, 0.75)
preds <- tidyr::expand_grid(
  onset = unique(truth_df$onset), .quantile_level = levels
)
preds$.value <- rep(c(8, 10, 13), times = 4)
nc <- tbl_nowcast(predictions = preds, method = "toy", event_date = "onset")

# Lower `wis` is better. `coverage_50` says whether the truth fell inside
# the 50% interval, which it should about half the time.
score_nowcast(nc, truth = truth)
#> # A tibble: 4 × 7
#>   .method onset      .observed   wis ae_median coverage_50 coverage_90
#>   <chr>   <date>         <dbl> <dbl>     <dbl> <lgl>       <lgl>      
#> 1 toy     2024-03-04        10 0.833         0 TRUE        NA         
#> 2 toy     2024-03-11        13 1.83          3 TRUE        NA         
#> 3 toy     2024-03-18        14 2.83          4 FALSE       NA         
#> 4 toy     2024-03-25        13 1.83          3 TRUE        NA         

# The same comparison handed to scoringutils as a point forecast.
if (requireNamespace("scoringutils", quietly = TRUE)) {
  scoringutils::as_forecast_point(nc, truth = truth)
}
#> Forecast type: point
#> Forecast unit:
#> onset and model
#> 
#>         onset predicted  model observed
#>        <Date>     <num> <char>    <num>
#> 1: 2024-03-04        10    toy       10
#> 2: 2024-03-11        10    toy       13
#> 3: 2024-03-18        10    toy       14
#> 4: 2024-03-25        10    toy       13

# With a real model, `truth` is the full object and the nowcast is fitted to
# a snapshot of it taken at an earlier `now`.
data(denguedat)

recent <- subset(denguedat, onset_week >= as.Date("2010-06-01"))
dengue <- tbl_now(recent,
  event_date = onset_week, report_date = report_week, verbose = FALSE
)
snapshot <- change_now(
  dplyr::filter(dengue, report_week <= as.Date("2010-10-04")),
  now = as.Date("2010-10-04")
)

if (requireNamespace("baselinenowcast", quietly = TRUE)) {
  nc <- run_nowcast(snapshot, engine_baselinenowcast(draws = 100), verbose = FALSE)
  # The FULL object is the truth: it still holds the reports that arrived
  # after the snapshot's `now`.
  score_nowcast(nc, truth = dengue)
}
#> Warning: baselinenowcast expects incremental counts; converting `x` to "count-incidence"
#> with `to_count()`.
#> Warning: 18 reference times available and 27 are specified.
#> ℹ All 18 reference times will be used.
#> # A tibble: 18 × 7
#>    .method        onset_week .observed     wis ae_median coverage_50 coverage_90
#>    <chr>          <date>         <dbl>   <dbl>     <dbl> <lgl>       <lgl>      
#>  1 baselinenowca… 2010-06-07       157   0           0   TRUE        TRUE       
#>  2 baselinenowca… 2010-06-14       210   0           0   TRUE        TRUE       
#>  3 baselinenowca… 2010-06-21       193   0           0   TRUE        TRUE       
#>  4 baselinenowca… 2010-06-28       193   0           0   TRUE        TRUE       
#>  5 baselinenowca… 2010-07-05       258   0           0   TRUE        TRUE       
#>  6 baselinenowca… 2010-07-12       315   0           0   TRUE        TRUE       
#>  7 baselinenowca… 2010-07-19       338   0           0   TRUE        TRUE       
#>  8 baselinenowca… 2010-07-26       302   0           0   TRUE        TRUE       
#>  9 baselinenowca… 2010-08-02       329   1           1   FALSE       FALSE      
#> 10 baselinenowca… 2010-08-09       358   0           0   TRUE        TRUE       
#> 11 baselinenowca… 2010-08-16       355   0           0   TRUE        TRUE       
#> 12 baselinenowca… 2010-08-23       258   0           0   TRUE        TRUE       
#> 13 baselinenowca… 2010-08-30       287   0.273       1   TRUE        TRUE       
#> 14 baselinenowca… 2010-09-06       298   0.201       0   TRUE        TRUE       
#> 15 baselinenowca… 2010-09-13       275   0.464       0   TRUE        TRUE       
#> 16 baselinenowca… 2010-09-20       250   5.33       11   FALSE       FALSE      
#> 17 baselinenowca… 2010-09-27       201  19.5        47.5 FALSE       TRUE       
#> 18 baselinenowca… 2010-10-04       147 105.        264   FALSE       TRUE       
```
