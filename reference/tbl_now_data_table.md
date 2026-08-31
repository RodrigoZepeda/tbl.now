# Convert between `tbl_now` and data.table

**\[experimental\]**

`tbl_now_from_data_table()` converts a `data.table` into a `tbl_now`
(requires explicit `event_date` / `report_date` columns).
`tbl_now_to_data_table()` strips the `tbl_now` class and returns a
`data.table` keeping every column; any lazy temporal effects are
materialised first (see
[`compute_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.md))
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

  The event- and report-date columns, as
  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  expressions: a bare column name or a string both work.

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

## See also

[as.data.table()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_coercion_methods.md),
the data.table method that calls this;
[as_tibble()](https://rodrigozepeda.github.io/tbl.now/reference/as_tibble.tbl_now.md)
and
[as_tsibble()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_coercion_methods.md)
for the other exits from the class;
[`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
to build one from the result.
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
data(denguedat)
dt <- data.table::as.data.table(denguedat)
nowobj <- tbl_now_from_data_table(dt,
  event_date = "onset_week",
  report_date = "report_week", verbose = FALSE
)
```
