# Subset function for \`tbl_now\` with downgrade-on-subsetting

IF the subsetting invalidates the class then a \`data.frame\` will be
returned.

## Usage

``` r
# S3 method for class 'tbl_now'
x[...]
```

## Arguments

- x:

  A \`tbl_now\` object

- ...:

  further arguments to be passed to or from other methods.

## Value

A \`tbl_now\` object or a \`data.frame\`
