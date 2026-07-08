# Convert between `tbl_now` and data.table

**\[experimental\]**

`tbl_now_from_data_table()` converts a `data.table` into a `tbl_now`
(requires explicit `event_date` / `report_date` columns).
`tbl_now_to_data_table()` strips the `tbl_now` class and returns a
`data.table` keeping every column; any lazy temporal effects are
materialised first (see
[`compute_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/compute_temporal_effects.md))
so their columns are present.

## Usage

``` r
tbl_now_from_data_table(data, event_date, report_date, ..., verbose = TRUE)

tbl_now_to_data_table(x, ..., verbose = TRUE)
```

## Arguments

- data:

  A `data.table`.

- event_date, report_date:

  Column names (passed to
  [`as_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md)).

- ...:

  Forwarded to
  [`as_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md)
  (`from`) or
  [`data.table::as.data.table()`](https://rdrr.io/pkg/data.table/man/as.data.table.html)
  (`to`).

- verbose:

  Logical. Print the choices that were made.

- x:

  A `tbl_now` object.

## Value

A `tbl_now` (`from`) or a `data.table` (`to`).

## Examples

``` r
data(denguedat)
dt <- data.table::as.data.table(denguedat)
nowobj <- tbl_now_from_data_table(dt,
  event_date = "onset_week",
  report_date = "report_week", verbose = FALSE
)
```
