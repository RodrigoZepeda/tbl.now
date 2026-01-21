# Validate a tbl_now object

Checks that an object is a properly constructed \`tbl_now\` with all
required attributes and valid data.

## Usage

``` r
validate_tbl_now(x, warn_non_uniqueness = FALSE, warn_now = TRUE)
```

## Arguments

- x:

  An object to validate

- warn_non_uniqueness:

  (optional) Logical. Whether to throw a warning if data has multiple
  observations for same event and report date (conditional on covariates
  and strata)

- warn_now:

  Boolean. Whether to warn if now is before last report or too far in
  the future.

## Value

Returns \`TRUE\` invisibly or throws an error. Called for its side
effects.

## Examples

``` r
if (FALSE) { # \dontrun{
data(denguedat)
ndata <- tbl_now(denguedat, event_date = "onset_week",
  report_date = "report_week", verbose = FALSE)

# Validate without errors
validate_tbl_now(ndata)

# Validate with errors
validate_tbl_now(data.frame(x = 1:3))

} # }
```
