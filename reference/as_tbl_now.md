# Transform an object into a `tbl_now`

**\[experimental\]**

Convert a supported object into a
[tbl_now](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md).
For a plain `data.frame` / `data.table` (or an existing `tbl_now`) you
supply the `event_date` and `report_date` columns yourself. For objects
produced by other packages the conversion is delegated to the matching
`tbl_now_from_*()` converter, which already knows how to map that format
– so those methods do **not** take `event_date` / `report_date`.

## Usage

``` r
as_tbl_now(object, ...)

# S3 method for class 'tbl_now'
as_tbl_now(object, event_date, report_date, ...)

# S3 method for class 'data.frame'
as_tbl_now(object, event_date, report_date, ...)

# S3 method for class 'enw_preprocess_data'
as_tbl_now(object, ...)

# S3 method for class 'reporting_triangle'
as_tbl_now(object, ...)

# S3 method for class 'epidist_linelist_data'
as_tbl_now(object, ...)

# S3 method for class 'epidist_aggregate_data'
as_tbl_now(object, ...)

# S3 method for class 'tbl_ts'
as_tbl_now(object, report_date, event_date = NULL, ...)

# S3 method for class 'data.table'
as_tbl_now(object, event_date, report_date, ...)

# S3 method for class 'tbl_now_epinow2_snapshots'
as_tbl_now(object, ...)

# S3 method for class 'tbl_now_triangle_list'
as_tbl_now(object, ...)

# S3 method for class 'tbl_now_surveillance_list'
as_tbl_now(object, ...)
```

## Arguments

- object:

  An object to convert to a `tbl_now`.

- ...:

  Additional arguments forwarded to the relevant `tbl_now_from_*()`
  converter (and therefore to
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)).

- event_date, report_date:

  The event- and report-date columns, as
  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  expressions – a bare column name or a string both work. Used for
  `data.frame`, `data.table`, `tbl_ts` (tsibble) and `tbl_now` inputs;
  for a tsibble, `event_date` defaults to the index. They are **not**
  arguments of the package-conversion methods (epinowcast,
  baselinenowcast, epidist), which carry their own date mapping.

## Value

A `tbl_now` object.

## Details

Package-specific inputs forward to a dedicated converter. **See that
converter** for the extra arguments it accepts (e.g. `strata`,
`max_delay`, `format`, `delays_unit`, ...), for which columns are
carried over, and for the transformation notes / round-trip caveats in
its *Round-trip* section:

- `enw_preprocess_data` (or a fitted `epinowcast` object) -\>
  [`tbl_now_from_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md)

- `reporting_triangle` (baselinenowcast) -\>
  [`tbl_now_from_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_baselinenowcast.md)

- `epidist_linelist_data` / `epidist_aggregate_data` (epidist) -\>
  [`tbl_now_from_epidist()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epidist.md)

- `tbl_ts` (tsibble) -\>
  [`tbl_now_from_tsibble()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_tsibble.md)

- `data.table` -\>
  [`tbl_now_from_data_table()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_data_table.md)

Anything passed through `...` is forwarded to the underlying converter
(and on to
[`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)),
so options such as `event_units`, `now` or `verbose` can be supplied
here too.

## See also

[`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
to build one from scratch; the converters this dispatches to –
[`tbl_now_from_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md),
[`tbl_now_from_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_baselinenowcast.md),
[`tbl_now_from_epidist()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epidist.md),
[`tbl_now_from_tsibble()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_tsibble.md),
[`tbl_now_from_data_table()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_data_table.md)
– and the `tbl_now_to_*()` functions that go the other way. The [*One
dataset, many nowcasts*
article](https://rodrigozepeda.github.io/tbl.now/articles/nowcasting-models.html)
shows the round trip against each modelling package.

## Examples

``` r
## For a plain data.frame this is a synonym for tbl_now(): you name the
# columns yourself.
data(denguedat)
as_tbl_now(denguedat, event_date = "onset_week", report_date = "report_week")
#> ℹ Identified data as <linelist-data> where each observation is a test.
#> # A tibble:  52,987 × 6
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week   gender .event_num .report_num .delay
#>    <date>       <date>        <chr>       <dbl>       <dbl>  <dbl>
#>    [event_date] [report_date] [...]       [...]       [...]  [...]
#>  1 1990-01-01   1990-01-01    Male            0           0      0
#>  2 1990-01-01   1990-01-01    Female          0           0      0
#>  3 1990-01-01   1990-01-01    Female          0           0      0
#>  4 1990-01-01   1990-01-08    Female          0           1      1
#>  5 1990-01-01   1990-01-08    Male            0           1      1
#>  6 1990-01-01   1990-01-15    Female          0           2      2
#>  7 1990-01-01   1990-01-15    Female          0           2      2
#>  8 1990-01-01   1990-01-15    Female          0           2      2
#>  9 1990-01-01   1990-01-22    Female          0           3      3
#> 10 1990-01-01   1990-01-08    Female          0           1      1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "onset_week" | Report date: "report_week"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows

# For an object built by another nowcasting package you often do not name them,
# because that format already fixes which column is the event date and which
# is the report date. Here we send a tbl_now out to tsibble and bring it back.
if (requireNamespace("tsibble", quietly = TRUE)) {
  ndata <- tbl_now(denguedat,
    event_date = onset_week, report_date = report_week, verbose = FALSE
  )
  ts <- suppressWarnings(tbl_now_to_tsibble(ndata, verbose = FALSE))

  # Bare names and strings both work.
  as_tbl_now(ts, event_date = onset_week, report_date = report_week)
}
#> 
#> ── Converted tsibble <data> into a <tbl_now> 
#> • event_date: "onset_week"
#> • report_date: "report_week"
#> • data_type: "linelist"
#> • now: "2010-12-20"
#> • event_units: "weeks"
#> • report_units: "weeks"
#> • event_date taken from the tsibble index: onset_week
#> # A tibble:  5,154 × 6
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week       n .event_num .report_num .delay
#>    <date>       <date>        <int>      <dbl>       <dbl>  <dbl>
#>    [event_date] [report_date] [...]      [...]       [...]  [...]
#>  1 1990-01-01   1990-01-01        3          0           0      0
#>  2 1990-01-01   1990-01-08       24          0           1      1
#>  3 1990-01-08   1990-01-08        2          1           1      0
#>  4 1990-01-01   1990-01-15       23          0           2      2
#>  5 1990-01-08   1990-01-15       33          1           2      1
#>  6 1990-01-15   1990-01-15        6          2           2      0
#>  7 1990-01-01   1990-01-22        8          0           3      3
#>  8 1990-01-08   1990-01-22        6          1           3      2
#>  9 1990-01-15   1990-01-22       19          2           3      1
#> 10 1990-01-22   1990-01-22        8          3           3      0
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "onset_week" | Report date: "report_week"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 5,144 more rows
```
