# Nowcast a `tbl_now` with any supported modelling package

**\[experimental\]**

Fits a nowcasting model to a `tbl_now` and returns the result in a
package-agnostic shape, so that models from different packages can be
compared, scored and combined
([`nowcast_ensemble()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_ensemble.md))
without any manual reshaping.

The function is named `run_nowcast()` rather than `nowcast()` because
diseasenowcasting already exports a `nowcast()` function; keeping the
names distinct means both packages can be attached at once.

## Usage

``` r
run_nowcast(x, engine = engine_diseasenowcasting(), verbose = TRUE)
```

## Arguments

- x:

  A `tbl_now` object.

- engine:

  A
  [`engine()`](https://rodrigozepeda.github.io/tbl.now/reference/engine.md)
  object: the modelling package **and every argument it takes**,
  including `min_date` and `quantile_levels`. Defaults to
  [`engine_diseasenowcasting()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md).

  The data and `verbose` are the only things that sit outside it. That
  is the point: an argument in an outer `...` had to be routed to the
  right backend by name, and one that missed simply vanished, leaving
  the model at its default with nothing to say so.

- verbose:

  Logical. Whether to report what is being done.

## Value

A
[tbl_nowcast](https://rodrigozepeda.github.io/tbl.now/reference/tbl_nowcast.md)
object.

## Engines

- [`engine_diseasenowcasting()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md):

  Bayesian structural time series. The `tbl_now` is passed in directly,
  so strata and temporal effects are picked up automatically.

- [`engine_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md):

  Fast, assumption-light baseline built from the reporting triangle.
  Stratified objects are nowcast one triangle per stratum.

- [`engine_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md):

  Bayesian model with separate delay and reference modules;
  `preprocess_args` controls
  [`tbl_now_to_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md).

- [`engine_surveillance()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md):

  Höhle & an der Heiden's nowcast, fed by
  [`tbl_now_to_surveillance()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_surveillance.md).
  The package models one series, so a stratified object is fitted one
  stratum at a time.

- [`engine_epinow2()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md):

  [`EpiNow2::estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.html),
  fed by
  [`tbl_now_to_EpiNow2()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_EpiNow2.md);
  [`EpiNow2::regional_epinow()`](https://epiforecasts.io/EpiNow2/reference/regional_epinow.html)
  when the object declares strata.

- [`engine_nobbs()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md):

  Nowcasting by Bayesian Smoothing, fed by
  [`tbl_now_to_nobbs()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_nobbs.md),
  which expands counts to the one row per case NobBS counts.

[`engine()`](https://rodrigozepeda.github.io/tbl.now/reference/engine.md)
covers any other registered method, including one you wrote yourself.
Every modelling package is an optional dependency: it is only needed
when you ask for its engine.

## What the object contributes, and what it does not

`run_nowcast()` reads the `tbl_now`'s declarations and hands each
package the shape it wants. Three of those declarations behave
differently enough to be worth stating plainly.

**Strata.** How many strata a backend can honour is a property of the
*package*, not of this one. Where a backend cannot, it warns and pools
rather than pretending:

|  |  |
|----|----|
| engine | how strata are modelled |
| `"baselinenowcast"` | one reporting triangle, and one fit, per stratum |
| `"surveillance"` | one fit per stratum; the package models a single series |
| `"EpiNow2"` | `regional_epinow()` instead of `estimate_infections()` |
| `"epinowcast"` | passed to the model as `by`, so they are fitted jointly |
| `"diseasenowcasting"` | fitted jointly; the package returns a `[draws x time x stratum]` array, one slice per combination |
| `"NobBS"` | `NobBS.strat()` instead of `NobBS()` |

**Every backend takes any number of strata.** The two that model one
series at a time (`"surveillance"`) or accept a single column
(`"NobBS.strat()"`) are given the *interaction* of the declared columns,
which is what nowcasting each combination separately means; the label is
split back into its columns on the way out.

The one thing that can go wrong is a stratum **value** that already
contains the `" | "` used to join them. That is an error rather than a
guess, because silently mis-assigning strata is worse than refusing.

Whatever strata columns come back are what the result reports as its
`strata`.

Columns you did **not** declare are summed away by the converters (see
[`tbl_now_to_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_baselinenowcast.md)).
A `tbl_now` built from `covid_colombia` without `strata = sex` is
nowcast as one pooled series, not silently split.

**Temporal effects.**
[`add_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.md)
specs are lazy; the converters materialise them into ordinary columns,
so they travel with the data. Whether the *model* then uses them is a
separate question, and mostly the answer is "only if you say so":

- `"diseasenowcasting"` receives the `tbl_now` itself and reads the
  effects off it, so they enter the model with no further work.

- `"epinowcast"` carries them as covariates you name in a module
  formula, e.g.
  `reference = epinowcast::enw_reference(~ 1 + day_of_week, data = ...)`.
  Without a formula referring to them they are inert.

- every other backend ignores them: the columns ride along so you can
  split on them, and nothing else happens.

**Censored delays.** A per-case censoring flag (see
[`add_is_censored_report()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md))
puts a censored and an uncensored row in the same
`(event date, report date)` cell, and a reporting triangle has one slot
per cell. Every backend that goes through a converter therefore
**collapses the flag with a warning** — counts are summed over it, line
lists drop the column. `"diseasenowcasting"` is the exception: it is
handed the object untouched, so the flag reaches the package intact. To
*estimate* a delay distribution from censored data, use
[`tbl_now_to_epidist()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epidist.md)
instead; nowcasting and delay estimation are different jobs.

## How each model is specified, and how to change it

`run_nowcast()` does not invent priors or model structure: it calls each
package with **that package's own defaults** and passes the engine's
arguments straight through. The defaults are not always the ones you
want, and two are worth knowing before you read the output.

**[`engine_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md)**
runs three modules, all at their package defaults. The expectation
module is `enw_expectation(r = ~ 0 + (1 | day:.group))` – a **random
effect per day** on the growth rate, which is a random walk on the log
expected counts in all but name. The reference module is
`enw_reference(parametric = ~ 1, distribution = "lognormal")` – a
**single lognormal reporting delay, constant over time**. The report
module is `enw_report(non_parametric = ~ 0)` – **no day-of-week
reporting effect**. Each is a named argument of the engine, and
`preprocess_args` carries
[`tbl_now_to_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md)'s
own arguments:

    run_nowcast(nowobj, engine_epinowcast(
      preprocess_args = list(max_delay = 30),
      report = epinowcast::enw_report(~ 1 + day_of_week, data = pobs),
      fit    = epinowcast::enw_fit_opts(chains = 4, iter_sampling = 1000)
    ))

**[`engine_epinow2()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md)**
– read this before trusting the output.
[`EpiNow2::estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.html)
defaults to `delays = delay_opts()`, which is `Fixed(0)`: **no reporting
delay at all**. Its `generation_time = gt_opts()` is `Fixed(1)`, a
one-day generation time. Those defaults describe a process with nothing
to nowcast, so supply the epidemiology yourself. EpiNow2 also models
\\R_t\\ with a **Gaussian process** by default
(`rt_opts(rw = 0, gp_on = "R_t-1")`) rather than a random walk:

    run_nowcast(nowobj, engine_epinow2(
      generation_time = EpiNow2::gt_opts(EpiNow2::example_generation_time),
      delays          = EpiNow2::delay_opts(EpiNow2::example_reporting_delay),
      rt              = EpiNow2::rt_opts(rw = 7)   # weekly random walk instead
    ))

**[`engine_diseasenowcasting()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md)**
uses the package's own defaults, reading strata, covariates and temporal
effects off the object. `model`, `type` and `n_draws` go to
`diseasenowcasting::nowcast()`.

**[`engine_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md)**
is not Bayesian and has no priors: the delay is estimated from the
reporting triangle and applied. `draws` sets the number of nowcast
samples, and `max_delay` caps the triangle's width.

**[`engine_nobbs()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md)**
has a fixed model; what you tune is `max_D` (maximum delay) and
`moving_window` (how much history is fitted). `moving_window` counts
**event periods and must not exceed the history you hand it** – ask for
more and NobBS pads its grid backwards and returns zero for every date,
with no error. `specs` takes its prior list.

**[`engine_surveillance()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md)**
takes `fit_method`, which is surveillance's own `method` argument
renamed so it cannot collide with the engine's method; it defaults to
`"bayes.notrunc.bnb"`. `D`, `when` and `control` are derived from the
object when you do not give them.

## See also

[`engine()`](https://rodrigozepeda.github.io/tbl.now/reference/engine.md)
and
[nowcast_engines](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md)
to specify which model to fit and how;
[autoplot()](https://rodrigozepeda.github.io/tbl.now/reference/autoplot.tbl_nowcast.md)
and
[tidy()](https://rodrigozepeda.github.io/tbl.now/reference/tidy.tbl_nowcast.md)
to look at the result;
[`nowcast_ensemble()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_ensemble.md)
to combine several nowcasts;
[`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md)
and
[`score_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/score_nowcast.md)
to find out whether they are any good;
[`nowcast_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_fit.md)
and
[`nowcast_tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_tidy.md)
to add a backend of your own, and
[`example_engine()`](https://rodrigozepeda.github.io/tbl.now/reference/example_engine.md)
for the shortest complete one. The [*One dataset, many nowcasts*
article](https://rodrigozepeda.github.io/tbl.now/articles/nowcasting-models.html)
fits the same data with every supported package.

## Examples

``` r
data(denguedat)

# A short recent window keeps the example quick.
recent <- subset(denguedat, onset_week >= as.Date("2010-06-01"))
dengue <- tbl_now(recent,
  event_date = onset_week, report_date = report_week, verbose = FALSE
)

# Every nowcast goes the same way: describe the model with an engine, then
## hand it and the data to `run_nowcast()`.
#
## `example_engine()` is a toy that ignores the reporting delay entirely; it is
# used here only so the example runs without a modelling package. Swap in a
## real one -- `engine_baselinenowcast()`, `engine_epinowcast()`,
## `engine_nobbs()` -- for anything you intend to act on.
nc <- run_nowcast(dengue, example_engine(), verbose = FALSE)
nc
#> ── A <tbl_nowcast> from method "example" ───────────────────────────────────────
#> • now: "2010-12-20"
#> • event dates: 26
#> • quantile levels: 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, and 0.975
#> • draws: none (quantiles only)
#> 
#> Nowcast at "2010-11-29" (q50, 2.5-97.5% interval):
#> • 35 [28, 42]
#> 
#> # A tibble: 6 × 3
#>   onset_week .quantile_level .value
#>   <date>               <dbl>  <dbl>
#> 1 2010-06-07           0.025    127
#> 2 2010-06-07           0.05     129
#> 3 2010-06-07           0.1      132
#> 4 2010-06-07           0.25     141
#> 5 2010-06-07           0.5      157
#> 6 2010-06-07           0.75     173
#> ℹ 228 more rows. Use `as_tibble()` for all of them.

# The result is a tbl_nowcast: one row per event date and quantile level.
head(tibble::as_tibble(nc))
#> # A tibble: 6 × 3
#>   onset_week .quantile_level .value
#>   <date>               <dbl>  <dbl>
#> 1 2010-06-07           0.025    127
#> 2 2010-06-07           0.05     129
#> 3 2010-06-07           0.1      132
#> 4 2010-06-07           0.25     141
#> 5 2010-06-07           0.5      157
#> 6 2010-06-07           0.75     173

# A real model is the same call with a real engine.
if (requireNamespace("baselinenowcast", quietly = TRUE)) {
  nc <- run_nowcast(dengue, engine_baselinenowcast(draws = 100), verbose = FALSE)
  nc
}
#> Warning: baselinenowcast expects incremental counts; converting `x` to "count-incidence"
#> with `to_count()`.
#> Warning: 29 reference times available and 36 are specified.
#> ℹ All 29 reference times will be used.
#> ── A <tbl_nowcast> from method "baselinenowcast" ───────────────────────────────
#> • now: "2010-12-20"
#> • event dates: 29
#> • quantile levels: 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, and 0.975
#> • draws: 100
#> 
#> Nowcast at "2010-12-20" (q50, 2.5-97.5% interval):
#> • 13 [0, 163.3]
#> 
#> # A tibble: 6 × 3
#>   onset_week .quantile_level .value
#>   <date>               <dbl>  <dbl>
#> 1 2010-06-07           0.025    157
#> 2 2010-06-07           0.05     157
#> 3 2010-06-07           0.1      157
#> 4 2010-06-07           0.25     157
#> 5 2010-06-07           0.5      157
#> 6 2010-06-07           0.75     157
#> ℹ 255 more rows. Use `as_tibble()` for all of them.
```
