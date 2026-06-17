# Subset function for `tbl_now`

**\[stable\]**

Accesors to `tbl_now` elements (rows and columns) as if it was a
data.frame

## Usage

``` r
# S3 method for class 'tbl_now'
x[...]

# S3 method for class 'grouped_tbl_now'
x[...]
```

## Arguments

- x:

  A `tbl_now` object

- ...:

  further arguments to be passed to or from other methods.

## Value

A `tbl_now` object or a `data.frame`

## Details

If the subsetting invalidates the class then a `data.frame` is returned.
