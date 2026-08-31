# Specify a nowcasting model and its arguments

**\[experimental\]**

An **engine** is one modelling package plus every argument that package
needs. It is what
[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
and
[`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md)
take, and it is the object
[`nowcast_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_fit.md)
and
[`nowcast_tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_tidy.md)
dispatch on.

`engine()` is the general constructor and works for any registered
method, including one you wrote yourself. The `engine_*()` functions are
its package-specific counterparts: each **names** the arguments of the
modelling function it drives, so the ones that matter are visible in the
signature and at
[`?engine_nobbs`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md)
rather than buried in a `...` that silently swallows a typo. Anything a
named argument does not cover still goes through `...`.

## Usage

``` r
engine(
  method,
  ...,
  min_date = NULL,
  quantile_levels = nowcast_quantile_levels(),
  label = NULL
)
```

## Arguments

- method:

  A single string naming the method, e.g. `"epinowcast"`. Built-in names
  are matched case-insensitively. See
  [`list_nowcast_methods()`](https://rodrigozepeda.github.io/tbl.now/reference/list_nowcast_methods.md).

- ...:

  Further arguments for the modelling function, passed through
  untouched. In `engine()` this is *every* argument; in the `engine_*()`
  functions it is whatever their named arguments do not already cover.

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

## Value

An object of class `c(method, "nowcast_engine")`.

## See also

[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md),
[`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md),
[`list_nowcast_methods()`](https://rodrigozepeda.github.io/tbl.now/reference/list_nowcast_methods.md),
and the [*Adding your own nowcasting model*
article](https://rodrigozepeda.github.io/tbl.now/articles/custom-nowcast-models.html)
for writing a backend of your own.

## Examples

``` r
engine("baselinenowcast", draws = 500)
#> ── <nowcast_engine: "baselinenowcast"> ─────────────────────────────────────────
#> • quantile levels: 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, and 0.975
#> • arguments: draws

# Built-in names are matched case-insensitively
engine("nobbs", max_D = 10)
#> ── <nowcast_engine: "NobBS"> ───────────────────────────────────────────────────
#> • quantile levels: 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, and 0.975
#> • arguments: max_D

# The same package twice, told apart by `label`
engine("diseasenowcasting", label = "default")
#> ── <nowcast_engine: "diseasenowcasting"> ───────────────────────────────────────
#> • label: "default"
#> • quantile levels: 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, and 0.975
```
