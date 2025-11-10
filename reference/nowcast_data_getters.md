# Getters for tbl_now attributes

These functions extract metadata attributes from a \`tbl_now\` object.
Each function returns a specific attribute (e.g. event date, strata,
covariates, etc.).

## Usage

``` r
get_event_date(x)

get_report_date(x)

get_strata(x)

get_num_strata(x)

get_covariates(x)

get_num_covariates(x)

get_now(x)

get_report_units(x)

get_event_units(x)

get_data_type(x)

get_is_batched(x)
```

## Arguments

- x:

  A \`tbl_now\` object.

## Value

The requested attribute (character, date, logical, etc.).
