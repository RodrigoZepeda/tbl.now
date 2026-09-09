# Convert between `tbl_now` and epidist

**\[experimental\]**

epidist models the delay between a *primary* event (e.g. symptom onset)
and a *secondary* event (e.g. report), storing each as an
interval-censored pair of date columns: `pdate_lwr`/`pdate_upr` for the
primary event and `sdate_lwr`/`sdate_upr` for the secondary event. It
comes in two shapes: a one-row-per-case `epidist_linelist_data`
([`epidist::as_epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_linelist_data.html))
and an `epidist_aggregate_data` that adds an `n` count column
([`epidist::as_epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/as_epidist_aggregate_data.html)).
epidist stores everything in **days** and requires every censoring
window to have a strictly positive width.

`tbl_now_from_epidist()` converts either shape into a `tbl_now`:

- `"auto"` (default): use the lower bounds — `primary` (`pdate_lwr`)
  becomes `event_date`, `secondary` (`sdate_lwr`) becomes `report_date`.
  An `epidist_aggregate_data` (or any input with an `n` column) becomes
  `data_type = "count-incidence"` with `case_count = "n"`; otherwise
  `data_type = "linelist"`. The `event_units`/`report_units` are
  inferred from the primary censoring-window width (a 7-day window -\>
  `"weeks"`), and a left-censored secondary window `[origin, report]` is
  decoded back to `is_censored_report = TRUE` with the report taken from
  `secondary_upper`.

- `"interval"`: instead attach the upper bounds `primary_upper`
  (`pdate_upr`) and `secondary_upper` (`sdate_upr`) as `covariates` (a
  warning is emitted).

`tbl_now_to_epidist()` performs the inverse. By default
(`format = "auto"`) it builds an `epidist_aggregate_data` when `x` holds
counts and an `epidist_linelist_data` otherwise, filling all four
interval columns:

- the primary event spans `[event_date, event_date + w]`, where the
  window `w` matches the `tbl_now` unit (`"days"` -\> 1 day, `"weeks"`
  -\> 7 days, ..., or `censoring_window` if supplied);

- the secondary event spans `[report_date, report_date + w]` normally,
  but for rows flagged by `is_censored_report` it is left-censored to
  `[event_date, report_date]` (the report is known only to have happened
  at or before its report date, and cannot precede the event, i.e.
  epidist time 0) — encoding the `tbl_now` convention that a censored
  report is known only to have happened at or before its report date, so
  the window is `[event_date, report_date]`.

The strata, the covariate columns and any materialised temporal-effect
columns (holidays, Fourier terms, calendar effects; see
[`compute_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.md))
are carried onto the epidist data unchanged, so the strata are available
as covariates in an epidist model formula (epidist has no separate
grouping argument).

## Usage

``` r
tbl_now_from_epidist(
  data,
  ...,
  format = c("auto", "interval"),
  primary = "pdate_lwr",
  secondary = "sdate_lwr",
  primary_upper = "pdate_upr",
  secondary_upper = "sdate_upr",
  verbose = TRUE
)

tbl_now_to_epidist(
  x,
  ...,
  format = c("auto", "linelist", "aggregate", "interval"),
  primary_upper = NULL,
  secondary_upper = NULL,
  censoring_window = NULL,
  obs_date = NULL,
  verbose = TRUE,
  quiet = FALSE
)
```

## Arguments

- data:

  A `data.frame`, `epidist_linelist_data` or `epidist_aggregate_data` of
  epidist delay data.

- ...:

  Forwarded to
  [`as_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md)
  (`from`) or to the relevant epidist constructor (`to`).

- format:

  For `from`: `"auto"` (default) or `"interval"`. For `to`: `"auto"`
  (default), `"linelist"`, `"aggregate"` or `"interval"`.

- primary, secondary:

  Column names of the primary / secondary event lower-bound dates.
  Default to epidist's `"pdate_lwr"` / `"sdate_lwr"`.

- primary_upper, secondary_upper:

  Column names of the upper-bound dates. Default to epidist's
  `"pdate_upr"` / `"sdate_upr"`. Used to infer units and decode
  censoring (`from`) or, with `format = "interval"`, taken from
  covariate columns (`to`).

- verbose:

  Logical. Print the choices that were made.

- x:

  A `tbl_now` object.

- censoring_window:

  (`to` only) Optional positive integer width, in days, of the censoring
  windows. If `NULL` (default) it is derived from the `tbl_now`
  `event_units`.

- obs_date:

  (`to` only) Optional `Date` of length one (or a vector of length
  `nrow(x)`) to use as epidist's `obs_date` column. If `NULL` (default)
  it is set to `get_now(x) + censoring_window` so the right-truncation
  clock ends at the object's own `now`.

- quiet:

  Logical. A *different* channel from `verbose`: `verbose` controls the
  informational summary of what the conversion did, while `quiet`
  suppresses the lossy-conversion **warning** emitted by
  `tbl_now_to_epidist()`.

## Value

A `tbl_now` (`from`) or an `epidist_linelist_data` /
`epidist_aggregate_data` object (`to`).

## Delays of zero, and the lognormal

A delay distribution with a **point mass at zero** cannot be fitted with
a lognormal (or a gamma, or a Weibull): all have zero density at zero.
If a large share of your cases are reported the same period they occur,
the fit does not fail loudly – it inflates the variance until the
density piles up near zero. On a daily COVID series where **57%** of
cases carried a delay of exactly 0, `epidist` returned `sigma = 17.9`
and an implied mean delay of `1.5e73` days.

Check before fitting:

    mean(as.numeric(x[[get_report_date(x)]] - x[[get_event_date(x)]]) == 0)

If that share is large, model the delay as **discrete**, use a
**zero-inflated**/hurdle form, or fit the continuous distribution to the
non-zero delays and report the zero share separately.

## Counts epidist cannot use

`epidist_aggregate_data` requires `n >= 1`, and so does
[`EpiNow2::estimate_dist()`](https://epiforecasts.io/EpiNow2/reference/estimate_dist.html)
– with the identical assertion message. Count data routinely holds rows
that violate it:

- **zeros** – an `(event, report)` cell where the report added nothing,
  which is most cells once
  [`complete_zeroes()`](https://rodrigozepeda.github.io/tbl.now/reference/complete_zeroes.md)
  has run, and which de-accumulating a `count-cumulative` series
  produces wherever a cumulative total was unchanged;

- **negatives** – a cumulative total revised *downward*, which
  de-accumulates to a negative increment.

Both are dropped before the epidist object is built. A zero contributes
no case to a delay distribution, so dropping it is lossless and is only
reported when `verbose = TRUE`. A negative is not a number of cases at
all, so dropping it discards the revision and **warns**. If nothing
usable is left the conversion aborts saying so, rather than letting
epidist's own `Assertion on 'data$n' failed` through.

## Model choice for count data

[`epidist::as_epidist_marginal_model()`](https://epidist.epinowcast.org/reference/as_epidist_marginal_model.html)
is the model built for aggregated counts: it works from the
`(delay, observation time)` cells the converter produces, so a month of
cases costs a few hundred *weights* rather than a few thousand rows. The
latent and naive models are alternatives that expand the counts back to
one row per case.

## The `now` and the observation window

epidist uses `obs_date` (an "observation stopped at" instant) to correct
for right truncation: any case with an event date near the end of the
series is under-observed, because there was less time for its report to
arrive. `tbl_now_to_epidist()` sets `obs_date <- get_now(x) + w` (the
end of the `now` period, widened by the censoring window `w`) so the
truncation clock ends at the object's own `now` rather than at the last
reported case. The two are usually the same on a fully-observed series
and can differ when the tail is silent or when
[`change_now()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
moves `now` forward for a backtest. Pass `obs_date` explicitly to
override.

`tbl_now_from_epidist()` reads the same column back on the `"auto"`
path: `now` on the returned `tbl_now` is `max(obs_date) - w`, so a round
trip preserves it (up to the censoring-window widening).

## Revision axis (not modelled)

epidist estimates one delay distribution – the primary-to-secondary
delay, which the converter maps to `event_date` -\> `report_date`. It
has no way to represent the revision axis, so `has_revision(x)`,
`revision_type`, `is_censored_revision` and the revision dates are
**dropped** from the epidist object. `tbl_now_to_epidist()` warns once
when it drops them, so a user who declared a revision process is told
the converter is not surfacing it.

## See also

[add](https://rodrigozepeda.github.io/tbl.now/reference/add.md) and
[revision_delay](https://rodrigozepeda.github.io/tbl.now/reference/revision_delay.md),
since epidist is about delay distributions and a `tbl_now` may carry two
of them;
[`censor_reporting_delays_above()`](https://rodrigozepeda.github.io/tbl.now/reference/censoring.md)
for the long delays that would otherwise dominate a fitted distribution;
[tidy()](https://rodrigozepeda.github.io/tbl.now/reference/tidy.delay_distribution.md)
for the fitted result.
[`as_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md)
for the generic that dispatches to the `*_from_*()` side;
[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md),
which does the conversion for you when you fit through an
[`engine()`](https://rodrigozepeda.github.io/tbl.now/reference/engine.md).
The [*One dataset, many nowcasts*
article](https://rodrigozepeda.github.io/tbl.now/articles/nowcasting-models.html)
fits the same data with every supported package.

## Examples

``` r
## --- Linelist epidist data (one row per case) ---
ll <- suppressMessages(epidist::as_epidist_linelist_data(
  data.frame(
    pdate_lwr = as.Date(c("2020-03-01", "2020-03-02", "2020-03-02")),
    sdate_lwr = as.Date(c("2020-03-05", "2020-03-04", "2020-03-06"))
  ),
  pdate_lwr = "pdate_lwr", sdate_lwr = "sdate_lwr"
))
# -> a linelist tbl_now ...
nowll <- tbl_now_from_epidist(ll, verbose = FALSE)
get_data_type(nowll)
#> [1] "linelist"
# ... and back to an epidist_linelist_data
tbl_now_to_epidist(nowll, verbose = FALSE, quiet = TRUE)
#> # A tibble: 3 × 10
#>   ptime_lwr ptime_upr stime_lwr stime_upr obs_time pdate_lwr  pdate_upr 
#>       <dbl>     <dbl>     <dbl>     <dbl>    <dbl> <date>     <date>    
#> 1         0         1         4         5        6 2020-03-01 2020-03-02
#> 2         1         2         3         4        6 2020-03-02 2020-03-03
#> 3         1         2         5         6        6 2020-03-02 2020-03-03
#> # ℹ 3 more variables: sdate_lwr <date>, sdate_upr <date>, obs_date <date>

## --- Aggregate epidist data (counts in an `n` column) ---
agg <- suppressMessages(epidist::as_epidist_aggregate_data(
  data.frame(
    pdate_lwr = as.Date(c("2020-03-01", "2020-03-02")),
    sdate_lwr = as.Date(c("2020-03-05", "2020-03-04")),
    n = c(7, 3)
  ),
  n = "n", pdate_lwr = "pdate_lwr", sdate_lwr = "sdate_lwr"
))
## -> a count-incidence tbl_now (case_count = "n") ...
nowagg <- tbl_now_from_epidist(agg, verbose = FALSE)
get_data_type(nowagg)
#> [1] "count-incidence"
## ... and back to an epidist_aggregate_data (auto-detected from the counts)
tbl_now_to_epidist(nowagg, verbose = FALSE, quiet = TRUE)
#> # A tibble: 2 × 11
#>   ptime_lwr ptime_upr stime_lwr stime_upr obs_time pdate_lwr  pdate_upr 
#>       <dbl>     <dbl>     <dbl>     <dbl>    <dbl> <date>     <date>    
#> 1         0         1         4         5        5 2020-03-01 2020-03-02
#> 2         1         2         3         4        5 2020-03-02 2020-03-03
#> # ℹ 4 more variables: sdate_lwr <date>, sdate_upr <date>, obs_date <date>,
#> #   n <dbl>
```
