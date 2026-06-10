# Convert between `tbl_now` and epinowcast

`tbl_now_from_epinowcast()` takes the long observation `data.frame` used
by epinowcast (with `reference_date`, `report_date` and a cumulative
`confirm` column, plus optional grouping columns) and converts it into a
`tbl_now` of `data_type = "count-cumulative"`.

`tbl_now_to_epinowcast()` takes a `tbl_now` and builds an
[`epinowcast::enw_preprocess_data()`](https://package.epinowcast.org/reference/enw_preprocess_data.html)
object (or, with `preprocess = FALSE`, the completed observation
`data.table`).

## Usage

``` r
tbl_now_from_epinowcast(
  data,
  ...,
  reference_date = "reference_date",
  report_date = "report_date",
  confirm = "confirm",
  strata = NULL,
  verbose = TRUE
)

tbl_now_to_epinowcast(
  x,
  ...,
  max_delay = NULL,
  preprocess = TRUE,
  verbose = TRUE
)
```

## Arguments

- data:

  A `data.frame`/`data.table` in epinowcast long format.

- ...:

  Additional arguments forwarded to
  [`as_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md)
  (for `from`) or to
  [`epinowcast::enw_preprocess_data()`](https://package.epinowcast.org/reference/enw_preprocess_data.html)
  (for `to`).

- reference_date, report_date, confirm:

  Column names in `data`.

- strata:

  Optional character vector of grouping columns. If `NULL` (default) any
  column other than `reference_date`, `report_date` and `confirm` is
  treated as a stratifying group.

- verbose:

  Logical. Print the choices that were made.

- x:

  A `tbl_now` object.

- max_delay:

  Maximum delay (in `timestep`s) to use when preprocessing. If `NULL` it
  is inferred from the data as `max(.delay) + 1`.

- preprocess:

  If `TRUE` (default) returns an `enw_preprocess_data` object; if
  `FALSE` returns the completed observation `data.table`.

## Value

`tbl_now_from_epinowcast()` returns a `tbl_now`.
`tbl_now_to_epinowcast()` returns an `enw_preprocess_data` object or a
`data.table`.

## Details

**\[experimental\]**

## Examples

``` r
obs <- epinowcast::germany_covid19_hosp
nowobj <- tbl_now_from_epinowcast(obs, strata = c("location", "age_group"))
#> 
#> ── Converted epinowcast <data> into a <tbl_now> 
#> • event_date: "reference_date"
#> • report_date: "report_date"
#> • data_type: "count-cumulative"
#> • now: "2021-10-20"
#> • units: event = "days", report = "days"
#> • strata: "location" and "age_group"
#> • case_count: "confirm"
```
