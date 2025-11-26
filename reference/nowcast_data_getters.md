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

get_temporal_effects(x)

get_is_batched(x)

get_case_col(x)
```

## Arguments

- x:

  A \`tbl_now\` object.

## Value

The requested attribute (character, date, logical, etc.).

## Examples

``` r
data(denguedat)
ndata <- tbl_now(denguedat,
    event_date = "onset_week",
    report_date = "report_week",
    strata = "gender",
    t_effects = temporal_effects(month_of_year = TRUE),
    verbose = FALSE)

#Get the event date
get_event_date(ndata)
#> [1] "onset_week"

#Get the report date
get_report_date(ndata)
#> [1] "report_week"

#Get strata
get_strata(ndata)
#> [1] "gender"

#Get covariates
get_covariates(ndata)
#> NULL

#Get is batched
get_is_batched(ndata)
#> NULL

#Get the now
get_now(ndata)
#> [1] "2010-12-20"

#Get the report units
get_report_units(ndata)
#> [1] "weeks"

#Get the event units
get_event_units(ndata)
#> [1] "weeks"

#Get the data type
get_data_type(ndata)
#> [1] "linelist"

#Get the column with cases
get_case_col(ndata)
#> [1] "n"

#Get temporal effects
get_temporal_effects(ndata)
#> [1] ".event_month_of_year"
```
