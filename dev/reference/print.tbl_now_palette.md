# Print a palette, one role per line

Registered on [`base::print`](https://rdrr.io/r/base/print.html) rather
than with a plain `@export`: the package namespace defines an S7 `print`
generic, which shadows
[`base::print`](https://rdrr.io/r/base/print.html) for an attached
session, so a plainly-exported `print.*` method never dispatches on
auto-print. (`print.diagnose_batches` and `print.transport_discriminant`
still have that bug.)

## Usage

``` r
# S3 method for class 'tbl_now_palette'
print(x, ...)
```

## Arguments

- x:

  A `tbl_now_palette`.

- ...:

  Unused.

## Value

`x`, invisibly.
