# Change attributes of a \`tbl_now\` object

Functions to modify the attributes of a \`tbl_now\` object.

## Usage

``` r
change_now(x, now = NULL)

change_event_date(x, event_date)

change_report_date(x, report_date)

change_case_count(x, case_count)

change_is_batched(x, is_batched)

change_strata(x, ...)

remove_strata(x, ...)

add_strata(x, ...)

remove_all_strata(x)

change_covariates(x, ...)

remove_covariate(x, ...)

add_covariate(x, ...)

remove_all_covariates(x)

replace_temporal_effects(x, t_effects)

remove_temporal_effects(x)
```

## Arguments

- x:

  A \`tbl_now\` object

- now:

  (optional) Date or `NULL` (default). The date that is considered the
  `now` of the nowcast. If no `now` is given then the function
  automatically uses the last `event_date`.

- event_date:

  [`` <`tidy-select`> ``](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  name of the column containing the event date.

- report_date:

  [`` <`tidy-select`> ``](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  name of the column containing the report date.

- case_count:

  (optional)
  [`` <`tidy-select`> ``](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  or `NULL` Name of the column with the case counts if `data_type` is
  "count-incidence" or "count-cumulative".

- is_batched:

  (optional)
  [`` <`tidy-select`> ``](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  or `NULL` (default). The name of a column containing either `TRUE` or
  `FALSE` indicating whether the `report_date` is correctly specified or
  corresponds to a `batch` and thus is censored. In other words, if the
  `report_date` is accurately measured set `is_batched = FALSE` but if
  the `report_date` corresponds to an error and is only an upper bound
  of the real report date set `is_batched = TRUE`.

- ...:

  [`` <`tidy-select`> ``](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  with the columns for the attribute. In the case of \`covariates\` and
  \`strata\` argument \`...\` can refer to multiple columns.

- t_effects:

  (optional) Either `NULL` (default), a
  [`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md)
  object or a character vector with the names of the columns containing
  the temporal effects.

## Value

A \`tbl_now\` object with updated attributes

## Details

Variable selection can be used with the auxiliary dplyr verbs such as
\[dplyr::starts_with()\], \[dplyr::all_of()\], and \[dplyr::where()\].
See \[dplyr::select()\] for additional info.

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
ndata <- ndata %>% change_event_date(new_onset_week)
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
ndata <- ndata %>% change_report_date(new_report_week)
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
ndata <- ndata %>% change_strata(gender, age_group)
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
ndata <- remove_strata(ndata, gender)
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
ndata <- add_strata(ndata, dplyr::starts_with("gender"))
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
ndata <- ndata %>% change_covariates(temperature, humidity)
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
ndata <- remove_covariate(ndata, temperature)
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
ndata <- add_covariate(ndata, temperature)
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
ndata <- change_now(ndata, now = as.Date("2025-01-01"))
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

#Change case count column
count_data <- ndata %>%
  to_count(to = "count-incidence")

count_data %>%
  dplyr::mutate(n2 = 1.15*n) %>%
  change_case_count(n2)
#> # A tibble:  52,987 × 11
#> # Data type: "count-incidence"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    new_onset_week new_report_week .event_num .report_num gender   age_group
#>    <date>         <date>               <dbl>       <dbl> <chr>    <chr>    
#>    [event_date]   [report_date]        [...]       [...] [strata] [strata] 
#>  1 1989-12-31     1989-12-31               0           0 Female   < 18     
#>  2 1989-12-31     1989-12-31               0           0 Female   < 18     
#>  3 1989-12-31     1989-12-31               0           0 Male     20-60    
#>  4 1989-12-31     1990-01-07               0           1 Female   20-60    
#>  5 1989-12-31     1990-01-07               0           1 Female   20-60    
#>  6 1989-12-31     1990-01-07               0           1 Female   20-60    
#>  7 1989-12-31     1990-01-07               0           1 Female   20-60    
#>  8 1989-12-31     1990-01-07               0           1 Female   20-60    
#>  9 1989-12-31     1990-01-07               0           1 Female   20-60    
#> 10 1989-12-31     1990-01-07               0           1 Female   60+      
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2025-01-01 | Event date: "new_onset_week" | Report date: "new_report_week"
#> # Strata: "gender" and "age_group"
#> # Covariates: "temperature" and "humidity"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows
#> # ℹ 5 more variables: temperature <dbl>, humidity <dbl>, n <int>, .delay <dbl>,
#> #   n2 <dbl>

#Change is_batched
ndata$is_batched <- FALSE
ndata %>%
  change_is_batched(is_batched)
#> # A tibble:  52,987 × 12
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
#> # ℹ 5 more variables: new_report_week <date>, age_group <chr>,
#> #   temperature <dbl>, humidity <dbl>, is_batched <lgl>
```
