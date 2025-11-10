# Set names on \`tbl_now\` class

If the modifying the names invalidates the \`tbl_now\` object the
subsetting will return a data frame with the other attributes of the
class preserved.

## Usage

``` r
# S3 method for class 'tbl_now'
names(x) <- value
```

## Arguments

- x:

  an R object.

- value:

  a character vector of up to the same length as `x`, or `NULL`.

## Value

A \`tbl_now\` object or a \`data.frame\`
