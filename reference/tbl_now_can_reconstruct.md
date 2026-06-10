# Checks whether the `tbl_now` object is valid

This is a wrapper for
[`validate_tbl_now`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md)
in a [`tryCatch()`](https://rdrr.io/r/base/conditions.html) in order to
not error if the input object is invalid and returns `TRUE` or `FALSE`
on if the object is valid. If the object is valid it can be
"reconstructed" and not downgraded to a `data.frame`.

## Usage

``` r
tbl_now_can_reconstruct(data)
```

## Arguments

- data:

  A `data.frame` or `tibble` to be converted.

## Value

A boolean logical (`TRUE` or `FALSE`)
