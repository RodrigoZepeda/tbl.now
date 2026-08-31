# Score a nowcast against observed data

**\[experimental\]**

A nowcast is a claim about numbers that are not in yet. Once the late
reports arrive you can ask how good the claim was, and these two
functions are the two ways of asking.

- `score_nowcast()` scores it here: the **weighted interval score**
  (WIS, lower is better), the absolute error of the median, and whether
  the truth fell inside the 50% and 90% intervals – one row per event
  date and stratum.

- `as_scoringutils()` hands the same comparison to scoringutils, in the
  long format that package expects, so you can use its full battery of
  scores and its plots.

In both cases `truth` is a `tbl_now` seen *later*, once the reports the
nowcast was predicting have actually arrived. The observed counts are
read from it with
[get_latest_reported_cases()](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.md),
so there is no column to name.

## Usage

``` r
score_nowcast(x, truth = NULL)

as_scoringutils(x, truth = NULL)
```

## Arguments

- x:

  A
  [tbl_nowcast](https://rodrigozepeda.github.io/tbl.now/reference/tbl_nowcast.md)
  object.

- truth:

  The `tbl_now` the nowcast is scored against – normally the *full*
  object, still holding the reports that arrived after the nowcast's
  `now`. Its eventual counts per event date are worked out for you: this
  is
  [`get_latest_reported_cases()`](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.md),
  aggregated over anything that is not a stratum, with the count column
  read off the object
  ([`get_case_count()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)).
  A **line list** is aggregated first, so it needs no special handling.

  `NULL` (default) uses the `tbl_now` the nowcast was built from, which
  is only meaningful when that object still holds the later reports.

## Value

`score_nowcast()` returns a `tibble` with the event-date column, the
strata columns, and the columns `.observed`, `wis`, `ae_median`,
`coverage_50` and `coverage_90` – one row per event date and stratum.

`as_scoringutils()` returns a long `tibble` with the columns `observed`,
`predicted`, `quantile_level` and `model`, plus the event date and
strata as forecast units – one row per quantile, ready for
[`scoringutils::as_forecast_quantile()`](https://epiforecasts.io/scoringutils/reference/as_forecast_quantile.html).

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

# The same comparison handed to scoringutils instead, one row per quantile.
head(as_scoringutils(nc, truth = truth))
#> # A tibble: 6 × 5
#>   onset      quantile_level predicted observed model
#>   <date>              <dbl>     <dbl>    <dbl> <chr>
#> 1 2024-03-04           0.25         8       10 toy  
#> 2 2024-03-04           0.5         10       10 toy  
#> 3 2024-03-04           0.75        13       10 toy  
#> 4 2024-03-11           0.25         8       13 toy  
#> 5 2024-03-11           0.5         10       13 toy  
#> 6 2024-03-11           0.75        13       13 toy  

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
#>    <chr>          <date>         <int>   <dbl>     <dbl> <lgl>       <lgl>      
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
