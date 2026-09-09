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
  max_delay = NULL,
  strata_sharing = "none",
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
  obs = NULL,
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
  [`diseasenowcasting::nowcast()`](https://rodrigozepeda.github.io/diseasenowcasting/reference/nowcast.html).
  `model` is where the epidemic and revision processes are chosen, e.g.
  `diseasenowcasting::model(epidemic = diseasenowcasting::ar1_epidemic())`.
  On `count-cumulative` data, diseasenowcasting selects its cumulative
  model automatically unless you supply one with
  `diseasenowcasting::model(cumulative = diseasenowcasting::cumulative_process())`.

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

- draws, delays_unit, max_delay:

  (`engine_baselinenowcast()`) Number of nowcast samples, the unit of
  the reporting triangle's delay axis (inferred from the object's units
  when `NULL`), and how many delay periods to keep – `max_delay = 10`
  keeps delays 0-9, as in
  [`tbl_now_to_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_baselinenowcast.md).
  The last one is not only about speed: `baselinenowcast` needs more
  reference dates than delay columns, so a **snapshot ("as of") series**
  – which re-reports every past period in every snapshot, and therefore
  has a delay axis as long as the series itself – cannot be fitted at
  all until the axis is capped. The error says which number to use.

- strata_sharing:

  (`engine_baselinenowcast()`) Whether to share estimates across the
  object's strata. `"none"` (default) fits every stratum independently.
  `"delay"` estimates the delay PMF once on the pooled counts and
  applies it to each stratum; `"uncertainty"` shares the uncertainty
  parameters the same way; pass `c("delay", "uncertainty")` to share
  both. Passed straight to
  [`baselinenowcast::baselinenowcast()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.html)'s
  argument of the same name, and only meaningful when the object has
  strata.

- preprocess_args, expectation, reference, report, fit:

  (`engine_epinowcast()`) `preprocess_args` is a list for
  [`tbl_now_to_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md),
  e.g. `list(max_delay = 30)`; the other four are
  [`epinowcast::epinowcast()`](https://package.epinowcast.org/reference/epinowcast.html)'s
  module arguments. **`epinowcast` is unseeded unless you say so**:
  `enw_fit_opts()` has no `seed` argument of its own –
  `formals(epinowcast::enw_fit_opts)` on 0.7.0 lists `sampler`,
  `nowcast`, `pp`, `likelihood`, `likelihood_aggregation`,
  `threads_per_chain`, `debug`, `output_loglik`, `sparse_design`, `...`
  – but its `...` are forwarded to the `sampler` (`enw_sample()`, i.e.
  `cmdstanr::sample()`), which does. Pass
  `fit = epinowcast::enw_fit_opts(seed = 1)` and the seed rides through
  to the sampler; that is what makes a fit reproducible.

  Two epinowcast 0.7.0 knobs worth knowing about, both reachable through
  the same pass-through:

  - a **delay-only** fit – reporting-delay distribution conditional on
    per-reference-date totals, with the latent process disabled – via
    `obs = epinowcast::enw_obs(delay_only = TRUE, data = pobs)`. `obs`
    is not a named engine argument here, but `engine_epinowcast()`
    forwards `...` to
    [`epinowcast::epinowcast()`](https://package.epinowcast.org/reference/epinowcast.html),
    so passing it there works.

  - a **structural** reporting effect (e.g. a fixed day-of-week
    reporting hazard) via `report = enw_report(structural = ...)`. Build
    the metadata with `enw_dayofweek_structural_reporting()`; this is
    separate from a temporal-effect covariate that lands on `metareport`
    and is referenced through `non_parametric =`.

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

- generation_time, delays, truncation, rt, obs, stan, convert_args:

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
  `truncation = trunc_opts()` is likewise `Fixed(0)` – **without a
  fitted truncation, `estimate_infections()` is a smooth through the
  incomplete recent days, not a nowcast**. The vignette's *EpiNow2*
  section walks through the two-step recipe (`estimate_truncation()`
  first, then pass its `get_parameters(...)[["truncation"]]` as
  `truncation` here).

  **Reproducibility.**
  [`EpiNow2::stan_opts()`](https://epiforecasts.io/EpiNow2/reference/stan_opts.html)
  picks a fresh random seed on every call
  (`seed = as.integer(runif(1, 1e8))`), so an unseeded fit cannot be
  reproduced – and a pathological sample cannot be told apart from a bad
  model afterwards. Pin it with
  `stan = stan_opts(samples = ..., warmup = ..., chains = ..., seed = <n>)`;
  `stan_opts()` forwards `seed` through to
  [`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
  / `cmdstanr::sample()`.

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
#> • arguments: draws and strata_sharing
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
#> • arguments: draws and strata_sharing
engine_diseasenowcasting()
#> ── <nowcast_engine: "diseasenowcasting"> ───────────────────────────────────────
#> • quantile levels: 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, and 0.975
```
