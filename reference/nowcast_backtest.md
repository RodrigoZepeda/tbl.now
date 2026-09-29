# Refit several methods at past `now` dates and score them

**\[stable\]**

Walks back through time: for every date in `now_dates`, the `tbl_now` is
truncated to the reports that were available then, each method is
refitted on that snapshot, and the resulting nowcast is scored against
the resolved truth defined by `truth_axis` and `truth_type` (reported
totals by default). This is what turns a set of models into ensemble
weights (see
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
  n_dates = 4L,
  seed = NULL,
  keep_draws = FALSE,
  on_error = c("warn", "abort"),
  verbose = TRUE,
  truth_axis = c("report", "revision"),
  truth_type = "total",
  parallel = FALSE,
  checkpoint_file = NULL
)
```

## Arguments

- x:

  A `tbl_now` object holding the *full* data (the later reports or
  revisions are what the retrospective nowcasts are scored against).
  Covariates on each retrospective snapshot should mean values available
  as of that snapshot's `now`; do not attach future realized covariate
  values unless they are an explicit forecast input for the engine.

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

  Vector of retrospective nowcast origins. Defaults to the `n_dates`
  most recent report-axis dates that are at least `horizon` units before
  the object's `now`; these dates are used as as-of origins, not as a
  filter on target event dates.

- horizon:

  Number of time units of hindsight required when `now_dates` is chosen
  automatically. Default `4`.

- n_dates:

  Number of automatic retrospective origins. Default `4`. Ignored when
  `now_dates` is supplied explicitly.

- seed:

  Optional integer. When given, the RNG is seeded **immediately before
  each fit**, from `seed` and the label and date that fit is for. One
  [`set.seed()`](https://rdrr.io/r/base/Random.html) before the whole
  backtest is not enough: it only pins anything if every method consumes
  the same random numbers in the same order, so dropping a method, or
  refitting one date, silently moves every other fit. Seeding per
  (label, date) makes a fit depend only on which fit it is.

- keep_draws:

  Logical. Whether to retain every posterior draw from every successful
  fit. Default `FALSE`, because this can make a backtest much larger.
  Set it to `TRUE` when the backtest should be passed directly to
  [`scoringutils::as_forecast_sample()`](https://epiforecasts.io/scoringutils/reference/as_forecast_sample.html).
  Engines that return only quantiles still cannot be converted to
  samples.

- on_error:

  Either `"warn"` (default) to skip a model/date that fails with a
  warning, or `"abort"` to stop.

- verbose:

  Logical. Whether to report progress.

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

- parallel:

  **\[experimental\]** Logical. Whether to run the (engine, date) fits
  in parallel with future, through foreach and doFuture (both must be
  installed). Default `FALSE`. The workers are whatever
  [`future::plan()`](https://future.futureverse.org/reference/plan.html)
  you set before the call; under the default `plan(sequential)` nothing
  runs in parallel. **May not play well with Stan-based engines**; see
  the "Parallel backtests" section.

- checkpoint_file:

  Optional path to a file where finished fits are saved as the backtest
  runs, so that an interrupted backtest can resume. Run the same call
  again with the same `checkpoint_file` and the (engine, date) fits
  already in it are not refitted; only the missing ones run. See the
  "Checkpoints and resuming" section. Default `NULL`: nothing is
  written.

## Value

An object of class `nowcast_backtest`: a list with

- scores:

  A `tibble` of per-date scores with an extra `.now` column.

- predictions:

  A `tibble` of every retrospective quantile prediction.

- draws:

  When `keep_draws = TRUE`, a `tibble` of the retained draws; otherwise
  `NULL`.

- timings:

  A `tibble` with one row per attempted engine/date fit, its elapsed
  time in seconds, whether it succeeded, and any error text.

- truth:

  The observed counts used for scoring.

- methods:

  The labels that produced at least one nowcast.

- now_dates:

  The dates that were nowcast.

Printing it summarises each method's scores over the targets every
method scored, warning when a failed fit made them differ; use
`print(bt, common_dates = FALSE)` to average each over its own targets.
The same rule sets
[`nowcast_weights()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_weights.md).

## Use the result directly with scoringutils

A `nowcast_backtest` has methods for
[`scoringutils::as_forecast_quantile()`](https://epiforecasts.io/scoringutils/reference/as_forecast_quantile.html),
[`scoringutils::as_forecast_point()`](https://epiforecasts.io/scoringutils/reference/as_forecast_point.html),
and
[`scoringutils::as_forecast_sample()`](https://epiforecasts.io/scoringutils/reference/as_forecast_sample.html),
so no manual reshaping is needed:

    quantile_forecast <- scoringutils::as_forecast_quantile(bt)
    point_forecast <- scoringutils::as_forecast_point(bt)

For sample forecasts, create the backtest with `keep_draws = TRUE` and
use `scoringutils::as_forecast_sample(bt)`. The returned forecast
objects can be passed to any compatible scoringutils workflow. For
example, relative WIS is obtained with:

    relative_scores <- quantile_forecast |>
      scoringutils::score() |>
      scoringutils::add_relative_skill(metric = "wis")

`model`, `now`, the event-date column, and declared strata are retained
as forecast units, allowing scores to be extended, regrouped, or
summarised without returning to the internal `tbl.now` representation.

## Parallel backtests (experimental)

With `parallel = TRUE`, every (engine, date) fit becomes one future
task, run on the backend you choose with
[`future::plan()`](https://future.futureverse.org/reference/plan.html):

    future::plan(future::multisession, workers = 3)
    bt <- nowcast_backtest(x, engine_a, engine_b, n_dates = 3, parallel = TRUE)
    future::plan(future::sequential)

The result is the same object, in the same row order, as a sequential
run. When `seed` is given every fit is seeded from it exactly as in a
sequential run, so the two agree; without `seed`, each task gets its own
parallel-safe random stream and results will differ from a sequential
run.

This option is **experimental** and **may not play well with Stan-based
engines** such as
[`engine_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md)
and
[`engine_epinow2()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md).
Those engines can already run chains in parallel themselves, and nesting
that inside parallel R workers can oversubscribe the CPU, exhaust
memory, or make Stan compilation and model caching fail (several workers
compiling or reading the same model at once). If you use them in a
parallel backtest, set their chains to run sequentially
(`epinowcast::enw_fit_opts(parallel_chains = 1)`,
`EpiNow2::stan_opts(cores = 1)`), compile each model once before the
backtest, and keep the number of workers small. When in doubt, leave
`parallel = FALSE`: the sequential path is unchanged.

## Checkpoints and resuming

A long backtest that is interrupted loses every fit it had finished.
With `checkpoint_file`, each finished (engine, date) fit is saved to
that file, and running the same call again continues from what is there:

    bt <- nowcast_backtest(x, engine_a, engine_b, now_dates = my_dates,
                           checkpoint_file = "tmp/my_backtest.rds")

The file is a single R data (`.rds`) file, written exactly as given (its
folder is created if missing). A resumed call returns the same object as
an uninterrupted one; only the `elapsed_seconds` of the `timings`
differ.

- **Only fits that succeeded are skipped.** A fit that failed (with
  `on_error = "warn"`) is recorded but retried on the next run.

- **The file is checked against the call.** It records a fingerprint of
  the data, `seed`, `keep_draws`, `truth_axis`, `truth_type` and each
  engine (by label). If any of them differ the call aborts rather than
  mixing results from two different backtests. Adding a new engine, or
  new `now_dates`, is fine: they are simply run and appended. Use a new
  file, or delete the old one, to start from scratch. The engine
  fingerprint ignores the environment of a function passed as an
  argument, so changing only what such a function captures is not
  detected.

- **Only the main R session writes the file.** With `parallel = TRUE`
  the `future` workers only return their fits; they never touch the
  file, so any
  [`future::plan()`](https://future.futureverse.org/reference/plan.html)
  – `multisession`, `multicore`, `cluster` on other machines – is safe.
  The fits then run in waves of
  [`future::nbrOfWorkers()`](https://future.futureverse.org/reference/nbrOfWorkers.html)
  and the file is updated after each wave, so an interruption loses at
  most one wave. Without `parallel` it is updated after every fit. Each
  update writes a temporary file next to it and renames it over the old
  one, so an interruption during a write cannot leave a truncated
  checkpoint.

- **Do not point two simultaneous runs at one file.** They would
  overwrite each other's progress.

- Without a `seed`, the fits are not reproducible, so a resumed backtest
  mixes fits that used different random numbers. Set `seed` if that
  matters.

To add later dates to a backtest you already have, or to combine
backtests of different models, see
[`backtest_combine()`](https://rodrigozepeda.github.io/tbl.now/reference/backtest_combine.md).

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
to use them;
[`backtest_combine()`](https://rodrigozepeda.github.io/tbl.now/reference/backtest_combine.md)
to join backtests run separately;
[`scoringutils::score()`](https://epiforecasts.io/scoringutils/reference/score.html)
and
[`scoringutils::add_relative_skill()`](https://epiforecasts.io/scoringutils/reference/add_relative_skill.html)
for an extensible scoring workflow. The [*One call, many models*
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
