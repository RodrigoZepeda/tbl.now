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
