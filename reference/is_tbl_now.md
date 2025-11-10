# Check if an object is a tbl_now

Check if an object is a tbl_now

## Usage

``` r
is_tbl_now(x)
```

## Arguments

- x:

  any R object

## Value

(boolean) \`TRUE\` if object is a \`tbl_now\` \`FALSE\` if not.

## Examples

``` r
is_tbl_now(data.frame(x = 1:3))
#> [1] FALSE

xval <- data.frame(x = 1:3)
class(xval) <- c("tbl_now", "data.frame")
is_tbl_now(xval)
#> [1] FALSE
```
