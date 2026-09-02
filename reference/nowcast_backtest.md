# Refit several methods at past `now` dates and score them

**\[experimental\]**

Walks back through time: for every date in `now_dates`, the `tbl_now` is
truncated to the reports that were available then, each method is
refitted on that snapshot, and the resulting nowcast is scored against
what was eventually observed. This is what turns a set of models into
ensemble weights (see
[`nowcast_weights()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_weights.md)
and
[`nowcast_ensemble()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_ensemble.md)).

Be aware that this refits every model once per date: with Bayesian
backends and a long `now_dates` it is genuinely expensive.

## Usage

``` r
nowcast_backtest(
  x,
  ...,
  now_dates = NULL,
  horizon = 4,
  seed = NULL,
  on_error = c("warn", "abort"),
  verbose = TRUE
)
```

## Arguments

- x:

  A `tbl_now` object holding the *full* data (the later reports are what
  the retrospective nowcasts are scored against).

- ...:

  The
  [`engine()`](https://rodrigozepeda.github.io/tbl.now/reference/engine.md)
  objects to backtest, one per model. Each carries its own arguments, so
  there is no keyed side-table of per-method options to get wrong.

  Give an engine a `label` (or name the argument) when the same package
  appears twice: `engine_diseasenowcasting(label = "ar1", model = ...)`
  and a plain
  [`engine_diseasenowcasting()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md)
  are backtested separately, so
  [`nowcast_weights()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_weights.md)
  can learn a weight for each – matching how
  [`nowcast_ensemble()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_ensemble.md)
  takes a named list of members. An engine with no label is labelled by
  its method.

- now_dates:

  Vector of Dates to nowcast at. Defaults to the four most recent event
  dates that are at least `horizon` units before the object's `now`, so
  that some later reports exist to score against.

- horizon:

  Number of time units of hindsight required when `now_dates` is chosen
  automatically. Default `4`.

- seed:

  Optional integer. When given, the RNG is seeded **immediately before
  each fit**, from `seed` and the label and date that fit is for. One
  [`set.seed()`](https://rdrr.io/r/base/Random.html) before the whole
  backtest is not enough: it only pins anything if every method consumes
  the same random numbers in the same order, so dropping a method, or
  refitting one date, silently moves every other fit. Seeding per
  (label, date) makes a fit depend only on which fit it is.

- on_error:

  Either `"warn"` (default) to skip a model/date that fails with a
  warning, or `"abort"` to stop.

- verbose:

  Logical. Whether to report progress.

## Value

An object of class `nowcast_backtest`: a list with

- scores:

  A `tibble` of per-date scores with an extra `.now` column.

- predictions:

  A `tibble` of every retrospective quantile prediction.

- truth:

  The observed counts used for scoring.

- methods:

  The labels that produced at least one nowcast.

- now_dates:

  The dates that were nowcast.

## Every engine must report the same quantile levels

A backtest exists to compare models, and two models summarised at
different levels are not comparable: the weighted interval score is an
average over the levels reported, so a model asked for three of them and
one asked for nine are scoring different quantities. Mismatched engines
are therefore an **error** rather than a warning.

This matters most for the engines where the levels are a *fit-time*
argument. NobBS computes exactly the quantiles it is handed and keeps no
draws, so a level it was never asked for cannot be recovered afterwards
– and an ensemble weighted from such a backtest would silently fall back
to whatever levels its members happened to share.

## See also

[`engine()`](https://rodrigozepeda.github.io/tbl.now/reference/engine.md)
to specify each model being compared, and its `min_date` argument, which
matters here because `now` moves between fits;
[`score_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/score_nowcast.md)
for the scores computed at each `now`;
[`nowcast_weights()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_weights.md)
to turn the result into ensemble weights, and
[`nowcast_ensemble()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_ensemble.md)
to use them. The [*One call, many models*
article](https://rodrigozepeda.github.io/tbl.now/articles/ensemble-nowcasting.html)
compares several packages this way.

## Examples

``` r
data(denguedat)

# A short recent window keeps the example quick.
recent <- subset(denguedat, onset_week >= as.Date("2010-06-01"))
dengue <- tbl_now(recent,
  event_date = onset_week, report_date = report_week, verbose = FALSE
)

## `example_engine()` is a toy that ignores the reporting delay entirely; it
# is used here only so the example runs without a modelling package.
## Swap in a real one -- `engine_baselinenowcast()`, `engine_epinowcast()`,
## `engine_nobbs()` -- for anything you intend to act on.

# Refit at two past `now` dates and score each against what is known now.
bt <- nowcast_backtest(dengue,
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

# Naming several engines compares them on identical data and dates.
bt$methods
#> [1] "carry forward"

# With a real model the call is the same, with a real engine.
if (requireNamespace("baselinenowcast", quietly = TRUE)) {
  nowcast_backtest(dengue,
    engine_baselinenowcast(draws = 100),
    now_dates = as.Date("2010-11-15"), verbose = FALSE
  )$scores
}
#> Warning: baselinenowcast expects incremental counts; converting `x` to "count-incidence"
#> with `to_count()`.
#> Warning: 24 reference times available and 30 are specified.
#> ℹ All 24 reference times will be used.
#> # A tibble: 24 × 8
#>    .method         .now       onset_week .observed   wis ae_median coverage_50
#>    <chr>           <date>     <date>         <dbl> <dbl>     <dbl> <lgl>      
#>  1 baselinenowcast 2010-11-15 2010-06-07       157     0         0 TRUE       
#>  2 baselinenowcast 2010-11-15 2010-06-14       210     0         0 TRUE       
#>  3 baselinenowcast 2010-11-15 2010-06-21       193     0         0 TRUE       
#>  4 baselinenowcast 2010-11-15 2010-06-28       193     0         0 TRUE       
#>  5 baselinenowcast 2010-11-15 2010-07-05       258     0         0 TRUE       
#>  6 baselinenowcast 2010-11-15 2010-07-12       315     0         0 TRUE       
#>  7 baselinenowcast 2010-11-15 2010-07-19       338     0         0 TRUE       
#>  8 baselinenowcast 2010-11-15 2010-07-26       302     0         0 TRUE       
#>  9 baselinenowcast 2010-11-15 2010-08-02       329     0         0 TRUE       
#> 10 baselinenowcast 2010-11-15 2010-08-09       358     0         0 TRUE       
#> # ℹ 14 more rows
#> # ℹ 1 more variable: coverage_90 <lgl>
```
