# Is this object a `tbl_nowcast`?

**\[experimental\]**

Tests whether an object is a fitted nowcast – the thing
[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
returns – rather than the data a nowcast is fitted to (for which see
[is_tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md)).

## Usage

``` r
is_tbl_nowcast(x)
```

## Arguments

- x:

  An object.

## Value

A single `TRUE` when `x` is a
[tbl_nowcast](https://rodrigozepeda.github.io/tbl.now/reference/tbl_nowcast.md),
`FALSE` otherwise.

## See also

[tbl_nowcast](https://rodrigozepeda.github.io/tbl.now/reference/tbl_nowcast.md)
for the class itself;
[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
which produces one;
[is_tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md)
for the input side.

## Examples

``` r
# A number is not a nowcast.
is_tbl_nowcast(1)
#> [1] FALSE

## The object run_nowcast() returns is.
predictions <- data.frame(
  onset_week = as.Date("2020-01-05"),
  .quantile_level = c(0.5, 0.9), .value = c(10, 14)
)
nc <- tbl_nowcast(
  predictions = predictions, method = "toy", event_date = "onset_week"
)
is_tbl_nowcast(nc)
#> [1] TRUE
```
