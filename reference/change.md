# Change/update the attributes of a `tbl_now` object

**\[stable\]**

Functions to change the attributes of a `tbl_now` object.

## Usage

``` r
change_now(x, now = NULL)

update_now(x)

change_event_date(x, event_date)

change_report_date(x, report_date)

change_case_count(x, case_count)

change_is_censored(x, is_censored)

change_strata(x, ..., warn_now = TRUE, warn_non_uniqueness = TRUE)

change_covariates(x, ..., warn_now = TRUE, warn_non_uniqueness = TRUE)
```

## Arguments

- x:

  A `tbl_now` object

- now:

  (optional) Date or `NULL` (default). The date that is considered the
  `now` of the nowcast. If no `now` is given then the function
  automatically uses the last `event_date`.

- event_date:

  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  name of the column containing the event date. Optional when `delay` is
  provided together with `report_date`; the event date will be computed
  as `report_date - delay`.

- report_date:

  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  name of the column containing the report date. Optional when `delay`
  is provided together with `event_date`; the report date will be
  computed as `event_date + delay`.

- case_count:

  (optional)
  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  or `NULL` Name of the column with the case counts if `data_type` is
  "count-incidence" or "count-cumulative".

- is_censored:

  (optional)
  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  or `NULL` (default). The name of a column containing either `TRUE` or
  `FALSE` indicating whether the `report_date` is correctly specified or
  corresponds to a `batch` and thus is censored. In other words, if the
  `report_date` is accurately measured set `is_censored = FALSE` but if
  the `report_date` corresponds to an error and is only an upper bound
  of the real report date set `is_censored = TRUE`.

- ...:

  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  with the columns for the attribute. In the case of `covariates` and
  `strata` argument `...` can refer to multiple columns.

- warn_now:

  Boolean. Whether to warn if now is before last report or too far in
  the future.

- warn_non_uniqueness:

  (optional) Logical. Whether to throw a warning if data has multiple
  observations for same event and report date (conditional on covariates
  and strata)

## Value

A `tbl_now` object with updated attributes

## Details

Variable selection is done via
[tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
and can be used with the auxiliary dplyr verbs such as
[`dplyr::starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
[`dplyr::all_of()`](https://tidyselect.r-lib.org/reference/all_of.html),
and
[`dplyr::where()`](https://tidyselect.r-lib.org/reference/where.html).
See
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
for additional information.

The `update_now()` function updates the now to the latest date in a
tibble. For example:

    #Get the data
    data(denguedat)
    ndata <- tbl_now(denguedat,
                     event_date = onset_week,
                     report_date = report_week,
                     verbose = FALSE)

    #The now is in 2010 because the data reaches all the way there
    get_now(ndata)
    #> [1] "2010-12-20"

    #We can filter the data
    ndata_1992 <- ndata |>
     dplyr::filter(onset_week <= as.Date("1992/01/01") &
                   report_week <= as.Date("1992/01/01"))

    #But the now will still be in 2010
    get_now(ndata_1992)
    #> [1] "2010-12-20"

    #The update now brings it to the closest date before the cut
    ndata_1992 <- ndata_1992 |> update_now()

    #Which is now correct and set to the latest date before the cut
    get_now(ndata_1992)
    #> [1] "1991-12-30"

## See also

[`add_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.md)
[add](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
[remove](https://rodrigozepeda.github.io/tbl.now/reference/remove.md)

## Examples

``` r
data(denguedat)
ndata <- tbl_now(denguedat,
  event_date = onset_week,
  report_date = report_week,
  verbose = FALSE
)

# Change the event_date column to a different date column
ndata$new_onset_week <- ndata$onset_week - lubridate::days(1)
ndata <- ndata |> change_event_date(new_onset_week)
ndata
#> # A tibble:  52,987 × 7
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week report_week   gender .event_num .report_num .delay new_onset_week
#>    <date>     <date>        <chr>       <dbl>       <dbl>  <dbl> <date>        
#>    [...]      [report_date] [...]       [...]       [...]  [...] [event_date]  
#>  1 1990-01-01 1990-01-01    Male            0       0.143  0.143 1989-12-31    
#>  2 1990-01-01 1990-01-01    Female          0       0.143  0.143 1989-12-31    
#>  3 1990-01-01 1990-01-01    Female          0       0.143  0.143 1989-12-31    
#>  4 1990-01-01 1990-01-08    Female          0       1.14   1.14  1989-12-31    
#>  5 1990-01-01 1990-01-08    Male            0       1.14   1.14  1989-12-31    
#>  6 1990-01-01 1990-01-15    Female          0       2.14   2.14  1989-12-31    
#>  7 1990-01-01 1990-01-15    Female          0       2.14   2.14  1989-12-31    
#>  8 1990-01-01 1990-01-15    Female          0       2.14   2.14  1989-12-31    
#>  9 1990-01-01 1990-01-22    Female          0       3.14   3.14  1989-12-31    
#> 10 1990-01-01 1990-01-08    Female          0       1.14   1.14  1989-12-31    
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "new_onset_week" | Report date: "report_week"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows

# Change the report_date column to a different column
ndata$new_report_week <- ndata$report_week - lubridate::days(1)
ndata <- ndata |> change_report_date(new_report_week)
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
ndata$age_group <- sample(c("< 18", "20-60", "60+"), nrow(ndata), replace = TRUE)
ndata <- ndata |> change_strata(gender, age_group)
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
ndata$humidity <- rbeta(nrow(ndata), 0.6, 0.4)
ndata <- ndata |> change_covariates(temperature, humidity)
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

# Bring `now` back to the latest date actually observed
ndata <- ndata |> update_now()
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
#> # Now: 2010-12-19 | Event date: "new_onset_week" | Report date: "new_report_week"
#> # Strata: "gender" and "age_group"
#> # Covariates: "temperature" and "humidity"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows
#> # ℹ 4 more variables: new_report_week <date>, age_group <chr>,
#> #   temperature <dbl>, humidity <dbl>

# Change case count column
count_data <- ndata |>
  to_count(to = "count-incidence")

count_data |>
  dplyr::mutate(n2 = 1.15 * n) |>
  change_case_count(n2)
#> # A tibble:  52,987 × 11
#> # Data type: "count-incidence"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    new_onset_week new_report_week .event_num .report_num gender   age_group
#>    <date>         <date>               <dbl>       <dbl> <chr>    <chr>    
#>    [event_date]   [report_date]        [...]       [...] [strata] [strata] 
#>  1 1989-12-31     1990-01-07               0           1 Female   20-60    
#>  2 1989-12-31     1990-01-14               0           2 Female   20-60    
#>  3 1989-12-31     1990-01-14               0           2 Female   20-60    
#>  4 1989-12-31     1990-01-14               0           2 Female   20-60    
#>  5 1989-12-31     1989-12-31               0           0 Female   20-60    
#>  6 1989-12-31     1989-12-31               0           0 Female   20-60    
#>  7 1989-12-31     1990-01-07               0           1 Female   20-60    
#>  8 1989-12-31     1990-01-21               0           3 Female   20-60    
#>  9 1989-12-31     1990-01-07               0           1 Female   20-60    
#> 10 1989-12-31     1990-01-07               0           1 Female   20-60    
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-19 | Event date: "new_onset_week" | Report date: "new_report_week"
#> # Strata: "gender" and "age_group"
#> # Covariates: "temperature" and "humidity"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows
#> # ℹ 5 more variables: temperature <dbl>, humidity <dbl>, n <int>, .delay <dbl>,
#> #   n2 <dbl>

# Change is_censored
ndata$is_censored <- FALSE
ndata <- ndata |>
  change_is_censored(is_censored)
ndata
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
#> # Now: 2010-12-19 | Event date: "new_onset_week" | Report date: "new_report_week"
#> # left-censored indicator: "is_censored"
#> # Strata: "gender" and "age_group"
#> # Covariates: "temperature" and "humidity"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows
#> # ℹ 5 more variables: new_report_week <date>, age_group <chr>,
#> #   temperature <dbl>, humidity <dbl>, is_censored <lgl>

# Change covariates
ndata$temperature <- rnorm(nrow(ndata), 25, 4)
ndata$humidity <- rbeta(nrow(ndata), 0.6, 0.4)
ndata <- ndata |> change_covariates(temperature, humidity)
ndata
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
#> # Now: 2010-12-19 | Event date: "new_onset_week" | Report date: "new_report_week"
#> # left-censored indicator: "is_censored"
#> # Strata: "gender" and "age_group"
#> # Covariates: "temperature" and "humidity"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows
#> # ℹ 5 more variables: new_report_week <date>, age_group <chr>,
#> #   temperature <dbl>, humidity <dbl>, is_censored <lgl>

# Add temporal effects, remove and replace them
ndata <- ndata |>
  add_temporal_effects(disease_data,
    t_effects = temporal_effects(week_of_year = TRUE, month_of_year = TRUE)
  )

# Use the compute to calculate them
ndata |> compute_temporal_effects()
#> # A tibble:  52,987 × 14
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
#> # Now: 2010-12-19 | Event date: "new_onset_week" | Report date: "new_report_week"
#> # left-censored indicator: "is_censored"
#> # Strata: "gender" and "age_group"
#> # Covariates: "temperature" and "humidity"
#> # T. effects: [event_date] month_of_year, week_of_year
#> # T. effect cols: ".event_month_of_year" and ".event_week_of_year"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows
#> # ℹ 7 more variables: new_report_week <date>, age_group <chr>,
#> #   temperature <dbl>, humidity <dbl>, is_censored <lgl>,
#> #   .event_month_of_year <int>, .event_week_of_year <int>

# Use replace to change them
ndata |> replace_temporal_effects(t_effects = temporal_effects(seasons = 52))
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
#> # Now: 2010-12-19 | Event date: "new_onset_week" | Report date: "new_report_week"
#> # left-censored indicator: "is_censored"
#> # Strata: "gender" and "age_group"
#> # Covariates: "temperature" and "humidity"
#> # T. effects (lazy): [event_date] season(52)
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows
#> # ℹ 5 more variables: new_report_week <date>, age_group <chr>,
#> #   temperature <dbl>, humidity <dbl>, is_censored <lgl>

# Use remove to delete them
ndata |> remove_temporal_effects()
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
#> # Now: 2010-12-19 | Event date: "new_onset_week" | Report date: "new_report_week"
#> # left-censored indicator: "is_censored"
#> # Strata: "gender" and "age_group"
#> # Covariates: "temperature" and "humidity"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows
#> # ℹ 5 more variables: new_report_week <date>, age_group <chr>,
#> #   temperature <dbl>, humidity <dbl>, is_censored <lgl>
```
