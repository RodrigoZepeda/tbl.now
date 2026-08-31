# Coerce a `tbl_now` with another package's generic

**\[experimental\]**

These S3 methods make each supported package's own coercion verb accept
a `tbl_now`. They are thin wrappers around the matching `tbl_now_to_*()`
converter and are quiet by default.

- `as_epidist_linelist_data()` (epidist) wraps
  [`tbl_now_to_epidist()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epidist.md).

- `as_epidist_aggregate_data()` (epidist) wraps
  [`tbl_now_to_epidist()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epidist.md)
  with `format = "aggregate"`.

- `as_reporting_triangle()` (baselinenowcast) wraps
  [`tbl_now_to_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_baselinenowcast.md)
  with `format = "matrix"`.

- `as_tsibble()` (tsibble) wraps
  [`tbl_now_to_tsibble()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_tsibble.md).

- `as.data.table()` (data.table) wraps
  [`tbl_now_to_data_table()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_data_table.md).

## Usage

``` r
# S3 method for class 'tbl_now'
as_epidist_linelist_data(data, ..., verbose = FALSE)

# S3 method for class 'tbl_now'
as_epidist_aggregate_data(data, ..., verbose = FALSE)

# S3 method for class 'tbl_now'
as_reporting_triangle(data, ..., verbose = FALSE)

# S3 method for class 'tbl_now'
as_tsibble(x, ..., verbose = FALSE)

# S3 method for class 'tbl_now'
as.data.table(x, ..., verbose = FALSE)
```

## Arguments

- data, x:

  A `tbl_now` object.

- ...:

  Additional arguments forwarded to the underlying converter.

- verbose:

  Logical; forwarded to the underlying converter. Defaults to `FALSE` so
  coercion is quiet.

## Value

The object produced by the corresponding `tbl_now_to_*()` converter.

## See also

The `tbl_now_to_*()` functions these delegate to, which take the
arguments:
[`tbl_now_to_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md),
[`tbl_now_to_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_baselinenowcast.md),
[`tbl_now_to_epidist()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epidist.md),
[`tbl_now_to_tsibble()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_tsibble.md),
[`tbl_now_to_data_table()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_data_table.md);
[`as_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md)
to come back the other way;
[as_tibble()](https://rodrigozepeda.github.io/tbl.now/reference/as_tibble.tbl_now.md)
to drop to a plain tibble.

## Examples

``` r
data(denguedat)
dengue <- tbl_now(denguedat[1:3000, ],
  event_date = onset_week, report_date = report_week, verbose = FALSE
)

# These are S3 methods, so the other package's own verb works directly on a
# `tbl_now` -- no explicit converter call needed.
if (requireNamespace("tsibble", quietly = TRUE)) {
  suppressWarnings(tsibble::as_tsibble(dengue))
}
#> # A tsibble: 463 x 3 [7D]
#> # Key:       report_week [92]
#>    onset_week report_week     n
#>    <date>     <date>      <int>
#>  1 1990-01-01 1990-01-01      3
#>  2 1990-01-01 1990-01-08     24
#>  3 1990-01-08 1990-01-08      2
#>  4 1990-01-01 1990-01-15     23
#>  5 1990-01-08 1990-01-15     33
#>  6 1990-01-15 1990-01-15      6
#>  7 1990-01-01 1990-01-22      8
#>  8 1990-01-08 1990-01-22      6
#>  9 1990-01-15 1990-01-22     19
#> 10 1990-01-22 1990-01-22      8
#> # ℹ 453 more rows

if (requireNamespace("data.table", quietly = TRUE)) {
  head(data.table::as.data.table(dengue))
}
#>    onset_week report_week gender .event_num .report_num .delay
#>        <Date>      <Date> <char>      <num>       <num>  <num>
#> 1: 1990-01-01  1990-01-01   Male          0           0      0
#> 2: 1990-01-01  1990-01-01 Female          0           0      0
#> 3: 1990-01-01  1990-01-01 Female          0           0      0
#> 4: 1990-01-01  1990-01-08 Female          0           1      1
#> 5: 1990-01-01  1990-01-08   Male          0           1      1
#> 6: 1990-01-01  1990-01-15 Female          0           2      2

## Use the `tbl_now_to_*()` function itself when you need its arguments; these
# methods take none beyond `verbose`.
```
