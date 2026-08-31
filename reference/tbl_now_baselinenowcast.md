# Convert between `tbl_now` and baselinenowcast

**\[experimental\]**

`tbl_now_from_baselinenowcast()` accepts either the long `data.frame`
(`reference_date`, `report_date`, `count`) or a `reporting_triangle`
matrix (rownames = reference dates, colnames = delays, incremental
counts) and converts it into a `tbl_now` of
`data_type = "count-incidence"`.

`tbl_now_to_baselinenowcast()` returns either a `reporting_triangle`
matrix (`format = "matrix"`, the default) via
[`baselinenowcast::as_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.html),
or the long `baselinenowcast`-style `data.frame` (`format = "long"`).
The long format also carries the **strata**, the covariates, the
censoring indicator and any materialised temporal-effect columns (see
[`compute_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/compute_temporal_effects.md));
the matrix holds only the three core columns. A single
reporting-triangle matrix has no strata dimension, so
`format = "matrix"` **pools** any strata (summing the counts) with a
warning; use `format = "triangle_list"` to get one triangle per stratum
instead.

## Usage

``` r
tbl_now_from_baselinenowcast(
  data,
  ...,
  reference_date = "reference_date",
  report_date = "report_date",
  count = "count",
  delays_unit = NULL,
  verbose = TRUE
)

tbl_now_to_baselinenowcast(
  x,
  ...,
  format = c("matrix", "long", "triangle_list"),
  delays_unit = NULL,
  complete = "auto",
  negatives = c("redistribute", "error"),
  verbose = TRUE
)
```

## Arguments

- data:

  A long `data.frame` or a `reporting_triangle` matrix.

- ...:

  Forwarded to
  [`as_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md)
  (`from`) or
  [`baselinenowcast::as_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.html)
  (`to`, triangle formats).

- reference_date, report_date, count:

  Column names (long format only).

- delays_unit:

  Unit of the delay axis of the reporting triangle, one of `"days"` or
  `"weeks"`. Both directions default to `NULL`, meaning it is worked out
  for you. For `tbl_now_from_baselinenowcast()` that means reading the
  input matrix's own `delays_unit` attribute (falling back to `"days"`
  when it has none); a supplied value always wins. For
  `tbl_now_to_baselinenowcast()` (triangle formats only) it is
  **inferred** from the object's time units when the event and report
  units agree and are `"days"` or `"weeks"`; otherwise you must supply
  it explicitly.

- verbose:

  Logical. Print the choices that were made.

- x:

  A `tbl_now` object.

- format:

  For `to`, one of:

  - `"matrix"` (default) – a single
    [`baselinenowcast::as_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.html)
    matrix. A triangle has no strata dimension, so any strata are
    **pooled** (with a warning).

  - `"long"` – a tidy data frame, which can also carry the strata,
    covariates, temporal-effect columns and the censoring indicator.

  - `"triangle_list"` – one reporting triangle **per stratum**, as a
    [tbl_now_triangle_list](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_triangle_list.md).
    Use this instead of pooling when you want a nowcast per stratum.
    With no strata attached the result is still a list, of length one
    and named `"all"`, so the return type never depends on whether
    strata happen to be present. Unlike splitting the long format
    yourself, the delay unit and the strata are taken from the object,
    and
    [`as_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md)
    can rebuild a `tbl_now` from the result.

- complete:

  For `to` with a triangle format: fill event periods that have no
  reports at all with zeroes, out to the object's
  [`get_now()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md),
  via
  [`complete_zeroes()`](https://rodrigozepeda.github.io/tbl.now/reference/complete_zeroes.md).
  `"auto"` (the default) does this for **line-list** input only, so you
  do not have to remember `to_count() |> complete_zeroes()` first. Count
  data is left exactly as supplied, because it *can* distinguish an
  observed zero from a cell that could not be observed yet (`NA`) and
  filling those would claim reporting was complete when it was not.
  `TRUE` / `FALSE` force either behaviour. Ignored for
  `format = "long"`.

- negatives:

  How to handle the negative increments that appear when
  `count-cumulative` data is de-accumulated (a downward revision).
  `"redistribute"` (default) absorbs each negative into earlier delays
  with
  [`baselinenowcast::preprocess_negative_values()`](https://baselinenowcast.epinowcast.org/reference/preprocess_negative_values.html),
  which is what that function exists for; `"error"` refuses cumulative
  input instead.

## Value

A `tbl_now` (`from`), or a `data.frame`, `reporting_triangle` or
[tbl_now_triangle_list](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_triangle_list.md)
(`to`), according to `format`.

## Round-trip

A `reporting_triangle` distinguishes **not-yet-observed** cells (`NA`)
from **observed zeros** (`0`). The `NA` cells split at the **last
observed report date** (the latest report with a non-`NA` count, taken
as the nowcast's `now`):

- cells with `report_date > now` are \* not-yet-observable\* future
  cells. They are **dropped** from the
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md).

- cells with `report_date <= now` *could* have been reported but were
  not. They are genuinely **missing** and kept as `count = NA` rows in
  the
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md).

On the way back,
[`baselinenowcast::as_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.html)
fills the in-triangle cells with `0` unless they are marked in the
tibble as `NA`.

## Sparse same-period reporting (weekly data especially)

`baselinenowcast` divides each observed row by the share of the delay
distribution that should have arrived by now. When almost nothing is
reported in the same period as the event, that share is tiny for the
most recent row and the estimate explodes: on a weekly line list where
`P(delay = 0)` is about **0.05**, a final row holding a single case
became an estimate of **257 with an upper bound of 1584** against a
truth of 15.

Completing the triangle to the `now` *always* leaves a final row
observable only at delay 0, so no choice of cut-off avoids it. Check the
delay PMF before trusting the newest rows:

    pmf <- baselinenowcast::estimate_delay(triangle)
    pmf[1]   # share expected to arrive in the same period

If it is small, follow `baselinenowcast`'s own advice and truncate "to
an earlier reference time to ensure a nowcast, not a forecast, is being
produced" – drop trailing rows whose expected observed share is below,
say, 10%. Daily data with substantial same-day reporting does not have
this problem.

## Capping the delay axis

The triangle gets one column per delay, so a single long straggler makes
it very wide and the fit very slow: capping delays at 30 days on a daily
series took a fit from **314s to 50s** for a tail carrying under 1% of
cases. Unlike
[`tbl_now_to_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md)
there is no `max_delay` argument here, so cap with a filter before
converting:

    x |> dplyr::filter(.delay <= 30) |> tbl_now_to_baselinenowcast()

## Negative delays

A reporting triangle is indexed by delay from **0**, so a report that
arrived *before* its event has no cell to go in.
[`baselinenowcast::as_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.html)
drops it, and the cell then reads `0` – indistinguishable from an
observed zero. Both triangle formats therefore **warn**, naming how many
rows and cases go, so the loss is not silent; `format = "long"` is a
tidy data frame with no delay axis and keeps them.
[`tbl_now_to_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md)
drops them the same way, and warns the same way.

Filter first if you want to decide what happens:

    x |> dplyr::filter(.delay >= 0) |> tbl_now_to_baselinenowcast()

## Censored delays

A censoring indicator that is a property of the **case** rather than of
the delay – an administrative "this date is only an upper bound" mark,
say – puts a censored and an uncensored row in the same
`(event_date, report_date)` cell. A reporting triangle has one slot per
cell, so the extra dimension has to go before the conversion. It is
removed automatically, with a warning either way:

- **count data**: the counts are summed over the flag, leaving case
  totals unchanged;

- **line lists**: the column is dropped, leaving one row per case.

[`tbl_now_to_epidist()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epidist.md)
is the exception and keeps the flag: estimating a delay distribution is
the one job that can use it.

## Examples

``` r
# Get a reporting triangle example
rt     <- baselinenowcast::example_reporting_triangle

# Convert to a tbl_now
nowobj <- tbl_now_from_baselinenowcast(rt)
#> 
#> ── Converted baselinenowcast <data> into a <tbl_now> 
#> • event_date: "reference_date"
#> • report_date: "report_date"
#> • data_type: "count-incidence"
#> • now: "2024-01-07"
#> • event_units: "days"
#> • report_units: "days"
#> • case_count: "count"
#> • expanded a reporting-triangle matrix to long counts

# The matrix round-trip is faithful (not-yet-observed `NA` cells are kept).
identical(rt, tbl_now_to_baselinenowcast(nowobj))
#> 
#> ── Converting <tbl_now> to baselinenowcast matrix 
#> • reference_date <- "reference_date"
#> • report_date <- "report_date"
#> • count <- "count"
#> • format: "matrix"
#> • delays_unit: "days"
#> ℹ Using max_delay = 3 from data
#> [1] TRUE
```
