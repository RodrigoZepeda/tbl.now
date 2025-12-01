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

replace_temporal_effects(x, value)

remove_temporal_effects(x)
```

## Arguments

- x:

  A \`tbl_now\` object

- value:

  The new value for the attribute

## Value

A \`tbl_now\` object with updated attributes

## See also

\[add_temporal_effects()\]

## Examples

``` r
data(denguedat)
ndata <- tbl_now(denguedat,
                 event_date = "onset_week",
                 report_date = "report_week",
                 verbose = FALSE)

# Change the event_date column to a different date column
ndata$new_onset_week <- ndata$onset_week - lubridate::days(1)
ndata <- change_event_date(ndata, "new_onset_week")
ndata
#> # A tibble:  52,987 × 7
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week report_week   gender .event_num .report_num .delay new_onset_week
#>    <date>     <date>        <chr>       <dbl>       <dbl>  <dbl> <date>        
#>    [...]      [report_date] [...]       [...]       [...]  [...] [event_date]  
#>  1 1990-01-01 1990-01-01    Male            0           0      0 1989-12-31    
#>  2 1990-01-01 1990-01-01    Female          0           0      0 1989-12-31    
#>  3 1990-01-01 1990-01-01    Female          0           0      0 1989-12-31    
#>  4 1990-01-01 1990-01-08    Female          0           1      1 1989-12-31    
#>  5 1990-01-01 1990-01-08    Male            0           1      1 1989-12-31    
#>  6 1990-01-01 1990-01-15    Female          0           2      2 1989-12-31    
#>  7 1990-01-01 1990-01-15    Female          0           2      2 1989-12-31    
#>  8 1990-01-01 1990-01-15    Female          0           2      2 1989-12-31    
#>  9 1990-01-01 1990-01-22    Female          0           3      3 1989-12-31    
#> 10 1990-01-01 1990-01-08    Female          0           1      1 1989-12-31    
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "new_onset_week" | Report date: "report_week"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows

# Change the report_date column to a different column
ndata$new_report_week <- ndata$report_week - lubridate::days(1)
ndata <- change_report_date(ndata, "new_report_week")
ndata
#> # A tibble:  52,987 × 8
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week report_week gender .event_num .report_num .delay new_onset_week
#>    <date>     <date>      <chr>       <dbl>       <dbl>  <dbl> <date>        
#>    [...]      [...]       [...]       [...]       [...]  [...] [event_date]  
#>  1 1990-01-01 1990-01-01  Male            0           0      0 1989-12-31    
#>  2 1990-01-01 1990-01-01  Female          0           0      0 1989-12-31    
#>  3 1990-01-01 1990-01-01  Female          0           0      0 1989-12-31    
#>  4 1990-01-01 1990-01-08  Female          0           1      1 1989-12-31    
#>  5 1990-01-01 1990-01-08  Male            0           1      1 1989-12-31    
#>  6 1990-01-01 1990-01-15  Female          0           2      2 1989-12-31    
#>  7 1990-01-01 1990-01-15  Female          0           2      2 1989-12-31    
#>  8 1990-01-01 1990-01-15  Female          0           2      2 1989-12-31    
#>  9 1990-01-01 1990-01-22  Female          0           3      3 1989-12-31    
#> 10 1990-01-01 1990-01-08  Female          0           1      1 1989-12-31    
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "new_onset_week" | Report date: "new_report_week"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows
#> # ℹ 1 more variable: new_report_week <date>

# Change strata to different strata
ndata$age_group <- sample(c("< 18","20-60","60+"), nrow(ndata), replace = TRUE)
ndata <- change_strata(ndata, c("gender", "age_group"))
ndata
#> # A tibble:  52,987 × 9
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week report_week gender   .event_num .report_num .delay new_onset_week
#>    <date>     <date>      <chr>         <dbl>       <dbl>  <dbl> <date>        
#>    [...]      [...]       [strata]      [...]       [...]  [...] [event_date]  
#>  1 1990-01-01 1990-01-01  Male              0           0      0 1989-12-31    
#>  2 1990-01-01 1990-01-01  Female            0           0      0 1989-12-31    
#>  3 1990-01-01 1990-01-01  Female            0           0      0 1989-12-31    
#>  4 1990-01-01 1990-01-08  Female            0           1      1 1989-12-31    
#>  5 1990-01-01 1990-01-08  Male              0           1      1 1989-12-31    
#>  6 1990-01-01 1990-01-15  Female            0           2      2 1989-12-31    
#>  7 1990-01-01 1990-01-15  Female            0           2      2 1989-12-31    
#>  8 1990-01-01 1990-01-15  Female            0           2      2 1989-12-31    
#>  9 1990-01-01 1990-01-22  Female            0           3      3 1989-12-31    
#> 10 1990-01-01 1990-01-08  Female            0           1      1 1989-12-31    
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "new_onset_week" | Report date: "new_report_week"
#> # Strata: "gender" and "age_group"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows
#> # ℹ 2 more variables: new_report_week <date>, age_group <chr>

# Remove some strata
ndata <- remove_strata(ndata, "gender")
ndata
#> # A tibble:  52,987 × 9
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week report_week gender .event_num .report_num .delay new_onset_week
#>    <date>     <date>      <chr>       <dbl>       <dbl>  <dbl> <date>        
#>    [...]      [...]       [...]       [...]       [...]  [...] [event_date]  
#>  1 1990-01-01 1990-01-01  Male            0           0      0 1989-12-31    
#>  2 1990-01-01 1990-01-01  Female          0           0      0 1989-12-31    
#>  3 1990-01-01 1990-01-01  Female          0           0      0 1989-12-31    
#>  4 1990-01-01 1990-01-08  Female          0           1      1 1989-12-31    
#>  5 1990-01-01 1990-01-08  Male            0           1      1 1989-12-31    
#>  6 1990-01-01 1990-01-15  Female          0           2      2 1989-12-31    
#>  7 1990-01-01 1990-01-15  Female          0           2      2 1989-12-31    
#>  8 1990-01-01 1990-01-15  Female          0           2      2 1989-12-31    
#>  9 1990-01-01 1990-01-22  Female          0           3      3 1989-12-31    
#> 10 1990-01-01 1990-01-08  Female          0           1      1 1989-12-31    
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "new_onset_week" | Report date: "new_report_week"
#> # Strata: "age_group"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows
#> # ℹ 2 more variables: new_report_week <date>, age_group <chr>

# Add strata
ndata <- add_strata(ndata, "gender")
ndata
#> # A tibble:  52,987 × 9
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week report_week gender   .event_num .report_num .delay new_onset_week
#>    <date>     <date>      <chr>         <dbl>       <dbl>  <dbl> <date>        
#>    [...]      [...]       [strata]      [...]       [...]  [...] [event_date]  
#>  1 1990-01-01 1990-01-01  Male              0           0      0 1989-12-31    
#>  2 1990-01-01 1990-01-01  Female            0           0      0 1989-12-31    
#>  3 1990-01-01 1990-01-01  Female            0           0      0 1989-12-31    
#>  4 1990-01-01 1990-01-08  Female            0           1      1 1989-12-31    
#>  5 1990-01-01 1990-01-08  Male              0           1      1 1989-12-31    
#>  6 1990-01-01 1990-01-15  Female            0           2      2 1989-12-31    
#>  7 1990-01-01 1990-01-15  Female            0           2      2 1989-12-31    
#>  8 1990-01-01 1990-01-15  Female            0           2      2 1989-12-31    
#>  9 1990-01-01 1990-01-22  Female            0           3      3 1989-12-31    
#> 10 1990-01-01 1990-01-08  Female            0           1      1 1989-12-31    
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "new_onset_week" | Report date: "new_report_week"
#> # Strata: "gender" and "age_group"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows
#> # ℹ 2 more variables: new_report_week <date>, age_group <chr>

# Change covariates
ndata$temperature <- rnorm(nrow(ndata), 25, 4)
ndata$humidity    <- rbeta(nrow(ndata), 0.6, 0.4)
ndata <- change_covariates(ndata, c("temperature", "humidity"))
ndata
#> # A tibble:  52,987 × 11
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week report_week gender   .event_num .report_num .delay new_onset_week
#>    <date>     <date>      <chr>         <dbl>       <dbl>  <dbl> <date>        
#>    [...]      [...]       [strata]      [...]       [...]  [...] [event_date]  
#>  1 1990-01-01 1990-01-01  Male              0           0      0 1989-12-31    
#>  2 1990-01-01 1990-01-01  Female            0           0      0 1989-12-31    
#>  3 1990-01-01 1990-01-01  Female            0           0      0 1989-12-31    
#>  4 1990-01-01 1990-01-08  Female            0           1      1 1989-12-31    
#>  5 1990-01-01 1990-01-08  Male              0           1      1 1989-12-31    
#>  6 1990-01-01 1990-01-15  Female            0           2      2 1989-12-31    
#>  7 1990-01-01 1990-01-15  Female            0           2      2 1989-12-31    
#>  8 1990-01-01 1990-01-15  Female            0           2      2 1989-12-31    
#>  9 1990-01-01 1990-01-22  Female            0           3      3 1989-12-31    
#> 10 1990-01-01 1990-01-08  Female            0           1      1 1989-12-31    
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "new_onset_week" | Report date: "new_report_week"
#> # Strata: "gender" and "age_group"
#> # Covariates: "temperature" and "humidity"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows
#> # ℹ 4 more variables: new_report_week <date>, age_group <chr>,
#> #   temperature <dbl>, humidity <dbl>

# Remove some covariates
ndata <- remove_covariate(ndata, "temperature")
ndata
#> # A tibble:  52,987 × 11
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week report_week gender   .event_num .report_num .delay new_onset_week
#>    <date>     <date>      <chr>         <dbl>       <dbl>  <dbl> <date>        
#>    [...]      [...]       [strata]      [...]       [...]  [...] [event_date]  
#>  1 1990-01-01 1990-01-01  Male              0           0      0 1989-12-31    
#>  2 1990-01-01 1990-01-01  Female            0           0      0 1989-12-31    
#>  3 1990-01-01 1990-01-01  Female            0           0      0 1989-12-31    
#>  4 1990-01-01 1990-01-08  Female            0           1      1 1989-12-31    
#>  5 1990-01-01 1990-01-08  Male              0           1      1 1989-12-31    
#>  6 1990-01-01 1990-01-15  Female            0           2      2 1989-12-31    
#>  7 1990-01-01 1990-01-15  Female            0           2      2 1989-12-31    
#>  8 1990-01-01 1990-01-15  Female            0           2      2 1989-12-31    
#>  9 1990-01-01 1990-01-22  Female            0           3      3 1989-12-31    
#> 10 1990-01-01 1990-01-08  Female            0           1      1 1989-12-31    
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "new_onset_week" | Report date: "new_report_week"
#> # Strata: "gender" and "age_group"
#> # Covariates: "humidity"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows
#> # ℹ 4 more variables: new_report_week <date>, age_group <chr>,
#> #   temperature <dbl>, humidity <dbl>

# Add covariates
ndata <- add_covariate(ndata, "temperature")
ndata
#> # A tibble:  52,987 × 11
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week report_week gender   .event_num .report_num .delay new_onset_week
#>    <date>     <date>      <chr>         <dbl>       <dbl>  <dbl> <date>        
#>    [...]      [...]       [strata]      [...]       [...]  [...] [event_date]  
#>  1 1990-01-01 1990-01-01  Male              0           0      0 1989-12-31    
#>  2 1990-01-01 1990-01-01  Female            0           0      0 1989-12-31    
#>  3 1990-01-01 1990-01-01  Female            0           0      0 1989-12-31    
#>  4 1990-01-01 1990-01-08  Female            0           1      1 1989-12-31    
#>  5 1990-01-01 1990-01-08  Male              0           1      1 1989-12-31    
#>  6 1990-01-01 1990-01-15  Female            0           2      2 1989-12-31    
#>  7 1990-01-01 1990-01-15  Female            0           2      2 1989-12-31    
#>  8 1990-01-01 1990-01-15  Female            0           2      2 1989-12-31    
#>  9 1990-01-01 1990-01-22  Female            0           3      3 1989-12-31    
#> 10 1990-01-01 1990-01-08  Female            0           1      1 1989-12-31    
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "new_onset_week" | Report date: "new_report_week"
#> # Strata: "gender" and "age_group"
#> # Covariates: "temperature" and "humidity"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows
#> # ℹ 4 more variables: new_report_week <date>, age_group <chr>,
#> #   temperature <dbl>, humidity <dbl>

# Change now
ndata <- change_now(ndata, as.Date("2025-01-01"))
ndata
#> # A tibble:  52,987 × 11
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week report_week gender   .event_num .report_num .delay new_onset_week
#>    <date>     <date>      <chr>         <dbl>       <dbl>  <dbl> <date>        
#>    [...]      [...]       [strata]      [...]       [...]  [...] [event_date]  
#>  1 1990-01-01 1990-01-01  Male              0           0      0 1989-12-31    
#>  2 1990-01-01 1990-01-01  Female            0           0      0 1989-12-31    
#>  3 1990-01-01 1990-01-01  Female            0           0      0 1989-12-31    
#>  4 1990-01-01 1990-01-08  Female            0           1      1 1989-12-31    
#>  5 1990-01-01 1990-01-08  Male              0           1      1 1989-12-31    
#>  6 1990-01-01 1990-01-15  Female            0           2      2 1989-12-31    
#>  7 1990-01-01 1990-01-15  Female            0           2      2 1989-12-31    
#>  8 1990-01-01 1990-01-15  Female            0           2      2 1989-12-31    
#>  9 1990-01-01 1990-01-22  Female            0           3      3 1989-12-31    
#> 10 1990-01-01 1990-01-08  Female            0           1      1 1989-12-31    
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2025-01-01 | Event date: "new_onset_week" | Report date: "new_report_week"
#> # Strata: "gender" and "age_group"
#> # Covariates: "temperature" and "humidity"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows
#> # ℹ 4 more variables: new_report_week <date>, age_group <chr>,
#> #   temperature <dbl>, humidity <dbl>
```
