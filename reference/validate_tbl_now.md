# Validate a tbl_now object

Checks that an object is a properly constructed \`tbl_now\` with all
required attributes and valid data.

## Usage

``` r
validate_tbl_now(x)
```

## Arguments

- x:

  An object to validate

## Value

Returns \`TRUE\` invisibly or throws an error. Called for its side
effects.

## Examples

``` r
if (FALSE) { # \dontrun{
data(denguedat)
ndata <- tbl_now(denguedat, event_date = "onset_week",
  report_date = "report_week")

# Validate without errors
validate_tbl_now(ndata)

# Validate with errors
validate_tbl_now(data.frame(x = 1:3))

} # }
```
