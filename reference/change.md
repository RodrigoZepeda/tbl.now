# Change attributes of a \`tbl_now\` object

Functions to modify the attributes of a \`tbl_now\` object. All
functions validate the object after making changes.

## Usage

``` r
change_event_date(x, value)

change_report_date(x, value)

change_strata(x, value)

remove_strata(x, value)

add_strata(x, value)

remove_all_strata(x)

change_covariates(x, value)

remove_covariate(x, value)

add_covariate(x, value)

remove_all_covariates(x)

change_now(x, value)
```

## Arguments

- x:

  A \`tbl_now\` object

- value:

  The new value for the attribute

## Value

A \`tbl_now\` object with updated attributes

## Examples

``` r
if (FALSE) { # \dontrun{
data(denguedat)
ndata <- tbl_now(denguedat,
                          event_date = "onset_week",
                          report_date = "report_week")

# Change the event_date column to a different date column
ndata$new_onset_week <- ndata$onset_week - lubridate::days(1)
ndata <- change_event_date(ndata, "new_onset_week")
ndata

# Change the report_date column to a different column
ndata$new_report_week <- ndata$report_week - lubridate::days(1)
ndata <- change_report_date(ndata, "new_report_week")
ndata

# Change strata to different strata
ndata$age_group <- sample(c("< 18","20-60","60+"), nrow(ndata), replace = TRUE)
ndata <- change_strata(ndata, c("gender", "age_group"))
ndata

# Remove some strata
ndata <- remove_strata(ndata, "gender")
ndata

# Add strata
ndata <- add_strata(ndata, "gender")
ndata

# Change covariates
ndata$temperature <- rnorm(nrow(ndata), 25, 4)
ndata$humidity    <- rbeta(nrow(ndata), 0.6, 0.4)
ndata <- change_covariates(ndata, c("temperature", "humidity"))
ndata

# Remove some covariates
ndata <- remove_covariates(ndata, "temperature")
ndata

# Add covariates
ndata <- add_covariates(ndata, "temperature")
ndata

# Change now
ndata <- change_now(ndata, as.Date("2025-01-01"))
ndata
} # }
```
