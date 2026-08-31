# Engines for the built-in nowcasting packages

**\[experimental\]**

One constructor per supported modelling package. Each is
[`engine()`](https://rodrigozepeda.github.io/tbl.now/reference/engine.md)
with the arguments of that package's own entry point spelled out, so the
ones that matter are visible in the signature and a typo is an error
rather than a silently ignored extra.

The [*One dataset, many nowcasts*
article](https://rodrigozepeda.github.io/tbl.now/articles/nowcasting-models.html)
documents each package's own API; this page is about driving it through
[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md).

## Usage

``` r
engine_diseasenowcasting(
  ...,
  model = NULL,
  type = NULL,
  n_draws = NULL,
  min_date = NULL,
  quantile_levels = nowcast_quantile_levels(),
  label = NULL
)

engine_baselinenowcast(
  ...,
  draws = 1000,
  delays_unit = NULL,
  min_date = NULL,
  quantile_levels = nowcast_quantile_levels(),
  label = NULL
)

engine_epinowcast(
  ...,
  preprocess_args = list(),
  expectation = NULL,
  reference = NULL,
  report = NULL,
  fit = NULL,
  min_date = NULL,
  quantile_levels = nowcast_quantile_levels(),
  label = NULL
)

engine_nobbs(
  ...,
  max_D = NULL,
  moving_window = NULL,
  specs = NULL,
  min_date = NULL,
  quantile_levels = nowcast_quantile_levels(),
  label = NULL
)

engine_surveillance(
  ...,
  D = NULL,
  when = NULL,
  fit_method = NULL,
  control = NULL,
  min_date = NULL,
  quantile_levels = nowcast_quantile_levels(),
  label = NULL
)

engine_epinow2(
  ...,
  generation_time = NULL,
  delays = NULL,
  truncation = NULL,
  rt = NULL,
  stan = NULL,
  convert_args = list(),
  min_date = NULL,
  quantile_levels = nowcast_quantile_levels(),
  label = NULL
)
```

## Arguments

- ...:

  Further arguments for the modelling function, passed through
  untouched. In
  [`engine()`](https://rodrigozepeda.github.io/tbl.now/reference/engine.md)
  this is *every* argument; in the `engine_*()` functions it is whatever
  their named arguments do not already cover.

- model, type, n_draws:

  (`engine_diseasenowcasting()`) Arguments of
  `diseasenowcasting::nowcast()`. `model` is where the epidemic and
  confirmation processes are chosen, e.g.
  `diseasenowcasting::model(epidemic = diseasenowcasting::ar1_epidemic())`.
  On `count-cumulative` data that revises downwards you also want a
  `confirmation` process, or the negative increments have nowhere to go.

- min_date:

  How much history to fit on. One of

  - `NULL` (default) – the whole series;

  - a **`Date`** – keep event dates on or after it;

  - a **single number** – keep the last *n* periods before the object's
    `now`, counted in the object's event units.

  The number is usually what you want in a
  [`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md):
  `now` moves between fits, and a fixed calendar date would make the
  fitted window grow as the backtest walks forward, so the last fit
  would be trained on more data than the first. Trimming is per engine
  on purpose – `baselinenowcast` and `diseasenowcasting` take a long
  series in their stride, while `epinowcast` scales with the number of
  reference dates and is best given a window.

- quantile_levels:

  Numeric vector of probabilities to report the nowcast at. Defaults to
  [`nowcast_quantile_levels()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_quantile_levels.md).

  It lives on the engine because for some backends it is a **fit-time
  model argument**, not a way of summarising afterwards. NobBS computes
  exactly the quantiles it is handed in `specs$quantiles` and keeps no
  draws, so a level it was never asked for cannot be recovered, and
  surveillance reports a fixed set and warns rather than interpolating.
  The draw-keeping backends – `baselinenowcast`, `diseasenowcasting`,
  `epinowcast` and `EpiNow2` – answer any level after the fact.

- label:

  Name for this engine in a
  [`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md)
  and in the ensemble weights derived from one. Defaults to the method
  name. Give one when the same package appears twice with different
  settings, which is the whole reason two `diseasenowcasting` models can
  be weighted separately.

- draws, delays_unit:

  (`engine_baselinenowcast()`) Number of nowcast samples, and the unit
  of the reporting triangle's delay axis (inferred from the object's
  units when `NULL`).

- preprocess_args, expectation, reference, report, fit:

  (`engine_epinowcast()`) `preprocess_args` is a list for
  [`tbl_now_to_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md),
  e.g. `list(max_delay = 30)`; the other four are
  [`epinowcast::epinowcast()`](https://package.epinowcast.org/reference/epinowcast.html)'s
  module arguments. **`epinowcast` is unseeded unless you say so**:
  `enw_fit_opts()` passes `...` to the sampler, so
  `fit = epinowcast::enw_fit_opts(seed = 1)` is what makes a fit
  reproducible.

- max_D, moving_window, specs:

  (`engine_nobbs()`) Arguments of
  [`NobBS::NobBS()`](https://rdrr.io/pkg/NobBS/man/NobBS.html) /
  [`NobBS::NobBS.strat()`](https://rdrr.io/pkg/NobBS/man/NobBS.strat.html).
  `moving_window` counts **event periods and must not exceed the history
  you hand it** – ask for more and NobBS pads its grid backwards and
  returns zero for every date, with no error. `specs$quantiles` is
  filled from `quantile_levels` unless you set it.

- D, when, fit_method, control:

  (`engine_surveillance()`) Arguments of
  [`surveillance::nowcast()`](https://rdrr.io/pkg/surveillance/man/nowcast.html).
  `fit_method` is that function's own `method` argument, renamed so it
  cannot collide with the engine's method. `when` defaults to
  `get_surveillance_when(x, length = D + 1)` and `control$dRange` to
  [`get_surveillance_range()`](https://rodrigozepeda.github.io/tbl.now/reference/surveillance_grids.md)
  – both built from the whole object, so every stratum is fitted on the
  same time axis.

- generation_time, delays, truncation, rt, stan, convert_args:

  (`engine_epinow2()`) Arguments of
  [`EpiNow2::estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.html)
  /
  [`EpiNow2::regional_epinow()`](https://epiforecasts.io/EpiNow2/reference/regional_epinow.html),
  plus `convert_args` for
  [`tbl_now_to_EpiNow2()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_EpiNow2.md).
  **Read this before trusting the output**: EpiNow2 defaults to
  `delays = delay_opts()`, which is `Fixed(0)` – no reporting delay at
  all – and a one-day generation time. Those defaults describe a process
  with nothing to nowcast, so supply the epidemiology yourself.

## Value

A `nowcast_engine`, as
[`engine()`](https://rodrigozepeda.github.io/tbl.now/reference/engine.md)
returns.

## See also

[`engine()`](https://rodrigozepeda.github.io/tbl.now/reference/engine.md),
[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md),
[`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md)

## Examples

``` r
engine_baselinenowcast(draws = 500)
#> ── <nowcast_engine: "baselinenowcast"> ─────────────────────────────────────────
#> • quantile levels: 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, and 0.975
#> • arguments: draws
engine_nobbs(max_D = 10, moving_window = 64)
#> ── <nowcast_engine: "NobBS"> ───────────────────────────────────────────────────
#> • quantile levels: 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, and 0.975
#> • arguments: max_D and moving_window

# Fit epinowcast on the last 180 periods only; it scales with the number of
# reference dates, while the two engines below take the whole series.
engine_epinowcast(preprocess_args = list(max_delay = 30), min_date = 180)
#> ── epinowcast model output ───────────────────────────────────────────────────── 
#> Groups:  | Timestep:  | Max delay:  
#> Observations:  timepoints x  snapshots 
#> Max date: NULL 
engine_baselinenowcast()
#> ── <nowcast_engine: "baselinenowcast"> ─────────────────────────────────────────
#> • quantile levels: 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, and 0.975
#> • arguments: draws
engine_diseasenowcasting()
#> ── <nowcast_engine: "diseasenowcasting"> ───────────────────────────────────────
#> • quantile levels: 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, and 0.975
```
