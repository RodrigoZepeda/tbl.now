# Convert between `tbl_now` and EpiNow2

**\[experimental\]**

EpiNow2 takes four different input shapes, one per entry point, so
`tbl_now_to_EpiNow2()` is told which one you want with `target` – named
after the EpiNow2 function the result is passed to, so it can be handed
over unchanged:

- `"estimate_infections"`:

  a `data.frame` of `date` / `confirm`, the series as known at
  [`get_now()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md).
  Also what
  [`EpiNow2::epinow()`](https://epiforecasts.io/EpiNow2/reference/epinow.html)
  takes.

- `"regional_epinow"`:

  the same, plus a `region` column built from the object's strata.

- `"estimate_truncation"`:

  a
  [tbl_now_epinow2_snapshots](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinow2_snapshots.md)
  list – one `date`/`confirm` snapshot per report date, which is the one
  EpiNow2 model that uses the report dimension a `tbl_now` exists to
  carry.

- `"estimate_dist"`:

  the interval-censored `pdate_lwr` / `pdate_upr` / `sdate_lwr` /
  `sdate_upr` / `obs_date` frame that
  [`EpiNow2::estimate_dist()`](https://epiforecasts.io/EpiNow2/reference/estimate_dist.html)
  fits a **delay distribution** to (new in EpiNow2 1.9.0). Count data
  rides along as the `n` weight column.

`tbl_now_from_EpiNow2()` inverts the snapshot form: snapshot *k* is the
series as known at report date *k*, so differencing consecutive
snapshots recovers `count-incidence` exactly. There is deliberately
**no** inverse for the other three: a single series has no report
dimension to recover, and a delay distribution is not case data.

## Usage

``` r
tbl_now_to_EpiNow2(
  x,
  ...,
  target = c("estimate_infections", "regional_epinow", "estimate_truncation",
    "estimate_dist"),
  snapshots = NULL,
  accumulate = "auto",
  verbose = TRUE,
  quiet = FALSE
)

tbl_now_from_EpiNow2(data, ..., report_dates = NULL, verbose = TRUE)
```

## Arguments

- x:

  A `tbl_now` object.

- ...:

  Forwarded to
  [`as_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md)
  (`from`); unused (`to`).

- target:

  Which EpiNow2 entry point the result is for. See above.

- snapshots:

  For `"estimate_truncation"`: how many snapshots to emit, taken from
  the **latest** report dates. `NULL` (default) uses 5, matching
  [`EpiNow2::example_truncated`](https://epiforecasts.io/EpiNow2/reference/example_truncated.html).
  One snapshot per distinct report date is usually far more than the
  model can fit.

- accumulate:

  How to handle non-daily data. `"auto"` (default) lays a weekly series
  on EpiNow2's daily grid with an `accumulate` column; `FALSE` passes
  the rows through unchanged, which is almost always wrong (see
  *Non-daily data*). Ignored for `"estimate_dist"`, which works in
  censoring windows rather than on a grid.

- verbose:

  Logical. Print the choices that were made.

- quiet:

  Logical. A *different* channel from `verbose`: `verbose` controls the
  informational summary of what the conversion did, while `quiet`
  suppresses the lossy-conversion warning. Set both to keep a conversion
  entirely silent.

- data:

  A
  [tbl_now_epinow2_snapshots](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinow2_snapshots.md),
  or a plain list of `date`/`confirm` data frames (e.g.
  [`EpiNow2::example_truncated`](https://epiforecasts.io/EpiNow2/reference/example_truncated.html)),
  in which case `report_dates` is required.

- report_dates:

  For `from`: a `Date` vector, one per snapshot, saying when each was
  taken. Read from the object's attribute when it has one.

## Value

For `to`, a `data.frame` or a
[tbl_now_epinow2_snapshots](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinow2_snapshots.md),
according to `target`. For `from`, a `tbl_now` of
`data_type = "count-incidence"`.

## Non-daily data

EpiNow2 models a **daily** process. As of 1.9.0 there is no `timestep`,
`interval` or `period` argument on any of its entry points, so a weekly
series passed as one row per week is read as one row per **day** and the
fit is silently wrong on the time axis – no error, just an epidemic
seven times too fast.

Its own answer is the `accumulate` column (see
[`EpiNow2::fill_missing()`](https://epiforecasts.io/EpiNow2/reference/fill_missing.html)):
the series is laid on a daily grid and the filler days are marked to be
added to the next real observation. `accumulate = "auto"` does this from
[`get_event_units()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md).
Units coarser than a week, and the `"numeric"` grid, are refused
outright rather than approximated.

## What EpiNow2 will not take

- [`EpiNow2::estimate_secondary()`](https://epiforecasts.io/EpiNow2/reference/estimate_secondary.html)
  models **two** data streams (cases and deaths, say) against each
  other. One `tbl_now` is one stream, so there is no honest mapping and
  no target for it.

- [`EpiNow2::estimate_delay()`](https://epiforecasts.io/EpiNow2/reference/estimate_delay.html)
  takes a bare vector of delays. Its own help now points at
  `estimate_dist()` as "the recommended replacement", and it throws away
  the censoring a `tbl_now` carries, so there is no target for it
  either. If you want it anyway, it is `x$.delay`.

## See also

[`tbl_now_to_epidist()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epidist.md),
which builds the same censoring windows as `target = "estimate_dist"` –
the two are different front ends onto one delay-distribution schema.

## Examples

``` r
data(denguedat)
nowobj <- tbl_now(denguedat[1:2000, ],
  event_date = "onset_week", report_date = "report_week", verbose = FALSE
)
## A single daily series for estimate_infections() -- the weekly data is laid
# on EpiNow2's daily grid.
head(tbl_now_to_EpiNow2(nowobj, verbose = FALSE, quiet = TRUE))
#>         date confirm accumulate
#> 1 1989-12-26      NA       TRUE
#> 2 1989-12-27      NA       TRUE
#> 3 1989-12-28      NA       TRUE
#> 4 1989-12-29      NA       TRUE
#> 5 1989-12-30      NA       TRUE
#> 6 1989-12-31      NA       TRUE

## Snapshots for estimate_truncation(), which uses the report dimension.
snaps <- tbl_now_to_EpiNow2(nowobj,
  target = "estimate_truncation", verbose = FALSE, quiet = TRUE
)
snaps
#> ── 5 reporting snapshots from a <tbl_now> ──────────────────────────────────────
#> • One per report date: "1991-01-21", "1991-01-28", "1991-02-11", "1991-02-25", and "1991-03-04"
#> • Rows each: 357, 357, 357, 357, and 357
#> • Now: "1991-03-04"
#> ℹ Pass this to `EpiNow2::estimate_truncation()`. `EpiNow2::estimate_secondary()` wants a single data frame of linked series instead -- not this.
```
