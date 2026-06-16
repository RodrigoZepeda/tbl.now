# Convert between `tbl_now` and baselinenowcast

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
  format = c("long", "matrix"),
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

  For `to`: `"long"` (default) or `"matrix"`.

## Value

A `tbl_now` (`from`), or a `data.frame`/`reporting_triangle` (`to`).

## Details

**\[experimental\]**

## Examples

``` r
rt <- baselinenowcast::example_reporting_triangle
nowobj <- tbl_now_from_baselinenowcast(rt)
#> 
#> ── Converted baselinenowcast <data> into a <tbl_now> 
#> • event_date: "reference_date"
#> • report_date: "report_date"
#> • data_type: "count-incidence"
#> • now: "2024-01-07"
#> • units: event = "days", report = "days"
#> • case_count: "count"
#> • expanded a reporting-triangle matrix to long incremental counts
```
