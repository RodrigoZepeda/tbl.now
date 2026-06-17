# Convert between `tbl_now` and epidist

**\[experimental\]**

epidist models the delay between a *primary* event (e.g. symptom onset)
and a *secondary* event (e.g. report), storing each as an
interval-censored pair of date columns: `pdate_lwr`/`pdate_upr` for the
primary event and `sdate_lwr`/`sdate_upr` for the secondary event (see
[`epidist::as_epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_linelist_data.html)).

`tbl_now_from_epidist()` converts such data into a `tbl_now`:

- `"linelist"` (default): use the lower bounds only — `primary`
  (`pdate_lwr`) becomes `event_date` and `secondary` (`sdate_lwr`)
  becomes `report_date`. `data_type = "linelist"`.

- `"interval"`: additionally attach the upper bounds `primary_upper`
  (`pdate_upr`) and `secondary_upper` (`sdate_upr`) as `covariates` (a
  warning is emitted).

`tbl_now_to_epidist()` performs the inverse and builds an
`epidist_linelist_data` object via
[`epidist::as_epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_linelist_data.html).
For `format = "interval"` the upper bounds are taken from covariate
columns named in `primary_upper` / `secondary_upper`.

## Usage

``` r
tbl_now_from_epidist(
  data,
  ...,
  format = c("linelist", "interval"),
  primary = "pdate_lwr",
  secondary = "sdate_lwr",
  primary_upper = "pdate_upr",
  secondary_upper = "sdate_upr",
  verbose = TRUE
)

tbl_now_to_epidist(
  x,
  ...,
  format = c("linelist", "interval"),
  primary_upper = NULL,
  secondary_upper = NULL,
  verbose = TRUE
)
```

## Arguments

- data:

  A `data.frame` (or `epidist_linelist_data`) of epidist delay data.

- ...:

  Forwarded to
  [`as_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md)
  (`from`) or
  [`epidist::as_epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_linelist_data.html)
  (`to`).

- format:

  `"linelist"` (default) or `"interval"`.

- primary, secondary:

  Column names of the primary / secondary event lower-bound dates.
  Default to epidist's `"pdate_lwr"` / `"sdate_lwr"`.

- primary_upper, secondary_upper:

  Column names of the upper-bound dates (`format = "interval"`). Default
  to epidist's `"pdate_upr"` / `"sdate_upr"`.

- verbose:

  Logical. Print the choices that were made.

- x:

  A `tbl_now` object.

## Value

A `tbl_now` (`from`) or an `epidist_linelist_data` object (`to`).

## Examples

``` r
df <- data.frame(
  pdate_lwr = as.Date(c("2020-03-01", "2020-03-02")),
  sdate_lwr = as.Date(c("2020-03-05", "2020-03-04"))
)
tbl_now_from_epidist(df, event_units = "days", report_units = "days")
#> 
#> ── Converted epidist <data> into a <tbl_now> 
#> • event_date: "pdate_lwr"
#> • report_date: "sdate_lwr"
#> • data_type: "linelist"
#> • now: "2020-03-05"
#> • units: event = "days", report = "days"
#> • format: linelist (primary lower bound -> event_date, secondary lower bound ->
#> report_date)
#> # A tibble:  2 × 5
#> # Data type: "linelist"
#> # Frequency: Event: `days` | Report: `days`
#>   pdate_lwr    sdate_lwr     .event_num .report_num .delay
#>   <date>       <date>             <dbl>       <dbl>  <dbl>
#>   [event_date] [report_date]      [...]       [...]  [...]
#> 1 2020-03-01   2020-03-05             0           4      4
#> 2 2020-03-02   2020-03-04             1           3      2
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2020-03-05 | Event date: "pdate_lwr" | Report date: "sdate_lwr"
#> # ────────────────────────────────────────────────────────────────────────────────
```
