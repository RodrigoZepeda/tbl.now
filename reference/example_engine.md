# A toy engine for examples

**\[experimental\]**

A deliberately naive nowcasting engine that needs no modelling package.
It exists so that the examples in this package can actually run: every
real engine depends on epinowcast, NobBS, EpiNow2 or another optional
package, and an example that cannot run teaches nothing.

**Do not nowcast with this.** It does not model the reporting delay at
all – it reports the counts that have arrived so far and puts a fixed
percentage band around them. Because late reports are exactly what it
ignores, it under-predicts recent dates by design, which is a useful
thing to *see* and a terrible thing to rely on. For real work use one of
the
[engines](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md)
–
[engine_baselinenowcast()](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md),
[engine_epinowcast()](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md),
[engine_nobbs()](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md)
and the rest – or write your own.

## Usage

``` r
example_engine(
  ...,
  spread = 0.2,
  min_date = NULL,
  quantile_levels = nowcast_quantile_levels(),
  label = NULL
)

# S3 method for class 'example'
nowcast_fit(
  engine,
  x,
  ...,
  spread = 0.2,
  quantile_levels = nowcast_quantile_levels(),
  verbose = TRUE
)

# S3 method for class 'example'
nowcast_tidy(engine, fit, x, ..., quantile_levels)
```

## Arguments

- ...:

  Ignored. Present so the engine accepts the same shape of call as the
  real ones.

- spread:

  Non-negative number setting the width of the interval, as a fraction
  of the point estimate. `0` gives a point mass at the median.

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

- engine:

  An
  [`engine()`](https://rodrigozepeda.github.io/tbl.now/reference/engine.md)
  object – the modelling package plus its arguments. S3 dispatch is on
  its class, so a backend for `"mypackage"` is a function called
  `nowcast_fit.mypackage()`. The whole engine arrives, not just its
  name, so `engine$args`, `engine$label` and the rest are available to a
  backend that wants them.

- x:

  A `tbl_now` object.

- verbose:

  Logical. Whether the backend (and the converters feeding it) should be
  chatty.

- fit:

  The object returned by
  [`nowcast_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_fit.md).

## Value

A `nowcast_engine` object, as
[`engine()`](https://rodrigozepeda.github.io/tbl.now/reference/engine.md)
returns, that
[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
and
[`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md)
accept.

## Details

For each event date (and stratum) it takes the cumulative count reported
by `now`, from
[get_latest_reported_cases()](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.md),
and reports that as the median. The other quantile levels are that
median scaled linearly by `spread`, so the 2.5% and 97.5% levels sit at
roughly `1 -/+ spread` times it.

No random numbers are involved, so it gives the same answer every time
and does not disturb the RNG stream.

## See also

[nowcast_engines](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md)
for the engines you would actually nowcast with;
[`engine()`](https://rodrigozepeda.github.io/tbl.now/reference/engine.md)
for the general constructor;
[`nowcast_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_fit.md)
and
[`nowcast_tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_tidy.md),
the two methods this implements – read its source for the shortest
possible complete backend. The [*Adding your own nowcasting model*
article](https://rodrigozepeda.github.io/tbl.now/articles/custom-nowcast-models.html)
walks through writing a real one.

## Examples

``` r
data(denguedat)
recent <- subset(denguedat, onset_week >= as.Date("2010-06-01"))
dengue <- tbl_now(recent,
  event_date = onset_week, report_date = report_week, verbose = FALSE
)

# It is an ordinary engine, so it goes where a real one goes.
example_engine()
#> ── <nowcast_engine: "example"> ─────────────────────────────────────────────────
#> • quantile levels: 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, and 0.975
#> • arguments: spread

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

## `spread` controls how wide the (made-up) interval is.
run_nowcast(dengue, example_engine(spread = 0.5), verbose = FALSE)
#> ── A <tbl_nowcast> from method "example" ───────────────────────────────────────
#> • now: "2010-12-20"
#> • event dates: 26
#> • quantile levels: 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, and 0.975
#> • draws: none (quantiles only)
#> 
#> Nowcast at "2010-11-29" (q50, 2.5-97.5% interval):
#> • 35 [18, 52]
#> 
#> # A tibble: 6 × 3
#>   onset_week .quantile_level .value
#>   <date>               <dbl>  <dbl>
#> 1 2010-06-07           0.025     82
#> 2 2010-06-07           0.05      86
#> 3 2010-06-07           0.1       94
#> 4 2010-06-07           0.25     118
#> 5 2010-06-07           0.5      157
#> 6 2010-06-07           0.75     196
#> ℹ 228 more rows. Use `as_tibble()` for all of them.
```
