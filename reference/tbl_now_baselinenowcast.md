# Convert between `tbl_now` and baselinenowcast

**\[experimental\]**

`tbl_now_from_baselinenowcast()` accepts either the long `data.frame`
(`reference_date`, `report_date`, `count`) or a `reporting_triangle`
matrix (rownames = reference dates, colnames = delays, incremental
counts) and converts it into a `tbl_now` of
`data_type = "count-incidence"`.

`tbl_now_to_baselinenowcast()` returns either the long
`baselinenowcast`-style `data.frame` (`format = "long"`, default) or a
`reporting_triangle` matrix (`format = "matrix"`) via
[`baselinenowcast::as_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.html).

## Usage

``` r
tbl_now_from_baselinenowcast(
  data,
  ...,
  reference_date = "reference_date",
  report_date = "report_date",
  count = "count",
  delays_unit = "days",
  verbose = TRUE
)

tbl_now_to_baselinenowcast(
  x,
  ...,
  format = c("matrix", "long"),
  delays_unit = "days",
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
  (`to`, matrix format).

- reference_date, report_date, count:

  Column names (long format only).

- delays_unit:

  Unit of the delay axis (passed to
  [`baselinenowcast::as_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.html)).
  Defaults to `"days"`.

- verbose:

  Logical. Print the choices that were made.

- x:

  A `tbl_now` object.

- format:

  For `to`: `"matrix"` (default) or `"long"`.

## Value

A `tbl_now` (`from`), or a `data.frame`/`reporting_triangle` (`to`).

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
#> ℹ Using max_delay = 3 from data
#> [1] TRUE
```
