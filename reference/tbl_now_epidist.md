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
  inferred from the primary censoring-window width (a 7-day window ⇒
  `"weeks"`), and a left-censored secondary window `[origin, report]` is
  decoded back to `is_censored = TRUE` with the report taken from
  `secondary_upper`.

- `"interval"`: instead attach the upper bounds `primary_upper`
  (`pdate_upr`) and `secondary_upper` (`sdate_upr`) as `covariates` (a
  warning is emitted).

`tbl_now_to_epidist()` performs the inverse. By default
(`format = "auto"`) it builds an `epidist_aggregate_data` when `x` holds
counts and an `epidist_linelist_data` otherwise, filling all four
interval columns:

- the primary event spans `[event_date, event_date + w]`, where the
  window `w` matches the `tbl_now` unit (`"days"` ⇒ 1 day, `"weeks"` ⇒ 7
  days, ..., or `censoring_window` if supplied);

- the secondary event spans `[report_date, report_date + w]` normally,
  but for rows flagged by `is_censored` it is left-censored to
  `[origin, report_date]` (with `origin` the earliest `event_date`, i.e.
  epidist time 0) — encoding the `tbl_now` convention that a censored
  report is only known to lie in `[0, report_date]`.

The strata, the covariate columns and any materialised temporal-effect
columns (holidays, Fourier terms, calendar effects; see
[`compute_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/compute_temporal_effects.md))
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

- quiet:

  Logical. If `TRUE`, suppress the lossy-conversion warning emitted by
  `tbl_now_to_epidist()`.

## Value

A `tbl_now` (`from`) or an `epidist_linelist_data` /
`epidist_aggregate_data` object (`to`).

## Examples

``` r
# --- Linelist epidist data (one row per case) ---
ll <- epidist::as_epidist_linelist_data(
  data.frame(
    pdate_lwr = as.Date(c("2020-03-01", "2020-03-02", "2020-03-02")),
    sdate_lwr = as.Date(c("2020-03-05", "2020-03-04", "2020-03-06"))
  ),
  pdate_lwr = "pdate_lwr", sdate_lwr = "sdate_lwr"
)
#> ℹ No primary event upper bound provided, using the primary event lower bound + 1 day as the assumed upper bound.
#> ℹ No secondary event upper bound provided, using the secondary event lower bound + 1 day as the assumed upper bound.
#> ℹ No observation time column provided, using 2020-03-07 as the observation date (the maximum of the secondary event upper bound).
# -> a linelist tbl_now ...
nowll <- tbl_now_from_epidist(ll)
#> 
#> ── Converted epidist <data> into a <tbl_now> 
#> • event_date: "pdate_lwr"
#> • report_date: "sdate_lwr"
#> • data_type: "linelist"
#> • now: "2020-03-06"
#> • event_units: "days"
#> • report_units: "days"
#> • format: linelist (lower bounds -> event/report dates)
get_data_type(nowll)
#> [1] "linelist"
# ... and back to an epidist_linelist_data
tbl_now_to_epidist(nowll)
#> 
#> ── Converting <tbl_now> into epidist linelist data 
#> • pdate_lwr <- "pdate_lwr", sdate_lwr <- "sdate_lwr"
#> • censoring window: 1 day (from "days")
#> • left-censored rows (is_censored): 0
#> ℹ No observation time column provided, using 2020-03-07 as the observation date (the maximum of the secondary event upper bound).
#> # A tibble: 3 × 10
#>   ptime_lwr ptime_upr stime_lwr stime_upr obs_time pdate_lwr  pdate_upr 
#>       <dbl>     <dbl>     <dbl>     <dbl>    <dbl> <date>     <date>    
#> 1         0         1         4         5        6 2020-03-01 2020-03-02
#> 2         1         2         3         4        6 2020-03-02 2020-03-03
#> 3         1         2         5         6        6 2020-03-02 2020-03-03
#> # ℹ 3 more variables: sdate_lwr <date>, sdate_upr <date>, obs_date <date>

# --- Aggregate epidist data (counts in an `n` column) ---
agg <- epidist::as_epidist_aggregate_data(
  data.frame(
    pdate_lwr = as.Date(c("2020-03-01", "2020-03-02")),
    sdate_lwr = as.Date(c("2020-03-05", "2020-03-04")),
    n = c(7, 3)
  ),
  n = "n", pdate_lwr = "pdate_lwr", sdate_lwr = "sdate_lwr"
)
#> ℹ No primary event upper bound provided, using the primary event lower bound + 1 day as the assumed upper bound.
#> ℹ No secondary event upper bound provided, using the secondary event lower bound + 1 day as the assumed upper bound.
#> ℹ No observation time column provided, using 2020-03-06 as the observation date (the maximum of the secondary event upper bound).
# -> a count-incidence tbl_now (case_count = "n") ...
nowagg <- tbl_now_from_epidist(agg)
#> 
#> ── Converted epidist <data> into a <tbl_now> 
#> • event_date: "pdate_lwr"
#> • report_date: "sdate_lwr"
#> • data_type: "count-incidence"
#> • now: "2020-03-05"
#> • event_units: "days"
#> • report_units: "days"
#> • case_count: "n"
#> • format: aggregate (lower bounds -> event/report dates, n -> case_count)
get_data_type(nowagg)
#> [1] "count-incidence"
# ... and back to an epidist_aggregate_data (auto-detected from the counts)
tbl_now_to_epidist(nowagg)
#> 
#> ── Converting <tbl_now> into epidist aggregate data 
#> • pdate_lwr <- "pdate_lwr", sdate_lwr <- "sdate_lwr"
#> • censoring window: 1 day (from "days")
#> • left-censored rows (is_censored): 0
#> • n <- "n"
#> ℹ No observation time column provided, using 2020-03-06 as the observation date (the maximum of the secondary event upper bound).
#> # A tibble: 2 × 11
#>   ptime_lwr ptime_upr stime_lwr stime_upr obs_time pdate_lwr  pdate_upr 
#>       <dbl>     <dbl>     <dbl>     <dbl>    <dbl> <date>     <date>    
#> 1         0         1         4         5        5 2020-03-01 2020-03-02
#> 2         1         2         3         4        5 2020-03-02 2020-03-03
#> # ℹ 4 more variables: sdate_lwr <date>, sdate_upr <date>, n <dbl>,
#> #   obs_date <date>
```
