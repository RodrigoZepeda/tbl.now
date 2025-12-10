
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Tibble now (tbl.now)

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/RodrigoZepeda/tbl.now/graph/badge.svg)](https://app.codecov.io/gh/RodrigoZepeda/tbl.now)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/tbl.now)](https://CRAN.R-project.org/package=tbl.now)
[![R-CMD-check](https://github.com/RodrigoZepeda/tbl.now/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RodrigoZepeda/tbl.now/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`tbl.now` provides a lightweight but rigorous extension of the tibble
class for storing, validating, and manipulating epidemiological
nowcasting data. It standardizes the representation of event dates,
report dates, strata, temporal covariates, and data types (linelist,
incidence, and cumulative), ensuring that downstream models within the
[`diseasenowcasting`](https://rodrigozepeda.github.io/diseasenowcasting/)
ecosystem can rely on a consistent interface.

<!---
&#10;## Key features
&#10;`tbl.now` implements:
&#10;### 1. A validated tibble subclass
&#10;Each `tbl_now` object guarantees:
&#10;+ An event date column.
+ A report date column.
+ Internally computed:
    + numeric event index (.event_num)
    + numeric report index (.report_num)
    + delay (.delay).
+ Optional strata (e.g., state, age group).
+ Optional covariates (e.g., state, age group).
+ Optional batch (right-censored report) indicator.
+ Optional temporal covariates (day of week, week of year, holidays).
&#10;### 2. Automatic data-type detection
&#10;`tbl_now()` infers whether the input represents:
&#10;+ **Linelist data**: one row per individual event
+ **Count–incidence data**: counts newly reported at each (event, report) pair
+ **Count–cumulative data**: cumulative totals revised over time
&#10;This allows a wide range of surveillance systems to be ingested with minimal preprocessing.
&#10;### 3. Built-in handling of the “now”
&#10;Each object records the nowcast horizon (now), defined as the latest reporting date unless overridden. This enables backtesting, historical reconstruction, and model evaluation under realistic information constraints. 
&#10;### 4. Native compatibility with tidyverse workflows
&#10;`tbl_now` objects behave as regular tibbles. Standard operations (`filter`, `mutate`, `summarise`, `join`, etc.) preserve metadata whenever possible.
&#10;### 5. Temporal covariates in one step
&#10;Use `temporal_effects()` and `add_temporal_effects()` to generate event-date covariates such as: day of week, week of year, and user-specified holiday calendars (via almanac). This standardizes temporal structures used by nowcasting models.
&#10;--->

## Installation

Install the development version from [GitHub](https://github.com/):

``` r
# install.packages("remotes")
remotes::install_github("RodrigoZepeda/tbl.now")
```

Load the package:

``` r
library(dplyr)
library(lubridate)
library(tbl.now)
library(almanac)    #Suggested for holiday effects
```

## A minimal example

Suppose you have a dataset where n cases reported on report_date belong
to events occurring on event_date:

``` r
df <- tibble(
  event_date  = c(ymd("2023-12-25"), ymd("2023-12-26"),
                  ymd("2023-12-25"), ymd("2023-12-26")),
  report_date = c(ymd("2023-12-26"), ymd("2023-12-26"),
                  ymd("2023-12-27"), ymd("2023-12-27")),
  n = c(10, 2, 5, 11)
)
```

Convert it to a
[tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html):

``` r
df_now <- df %>% 
  tbl_now(event_date = event_date, report_date = report_date, case_count = n)
#> ℹ Identified data as <count-incidence> with counts in column "n".

df_now
#> # A tibble:  4 × 6
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>   event_date   report_date         n .event_num .report_num .delay
#>   <date>       <date>          <dbl>      <dbl>       <dbl>  <dbl>
#>   [event_date] [report_date] [cases]      [...]       [...]  [...]
#> 1 2023-12-25   2023-12-26         10          0           1      1
#> 2 2023-12-26   2023-12-26          2          1           1      0
#> 3 2023-12-25   2023-12-27          5          0           2      2
#> 4 2023-12-26   2023-12-27         11          1           2      1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2023-12-27 | Event date: "event_date" | Report date: "report_date"
#> # ────────────────────────────────────────────────────────────────────────────────
```

`tbl_now()` automatically:

- infers that the **data type** is linelist (one row per event)

- determines the **date units** (daily event and report frequencies)

- computes numerical versions of the dates: `.event_num`, `.report_num`,
  and `.delay`

- sets the **now** to the most recent reporting date

Use it like any tibble:

``` r
df_now %>% 
  filter(n > 5)
#> # A tibble:  2 × 6
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>   event_date   report_date         n .event_num .report_num .delay
#>   <date>       <date>          <dbl>      <dbl>       <dbl>  <dbl>
#>   [event_date] [report_date] [cases]      [...]       [...]  [...]
#> 1 2023-12-25   2023-12-26         10          0           1      1
#> 2 2023-12-26   2023-12-27         11          1           2      1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2023-12-27 | Event date: "event_date" | Report date: "report_date"
#> # ────────────────────────────────────────────────────────────────────────────────
```

## Adding strata

If strata was given, the
[tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html)
can easily tag the corresponding strata.

``` r
#Add the column using dplyr:
df_now <- df_now %>% 
  mutate(sex = c("M","M","F","M")) 

df_now
#> # A tibble:  4 × 7
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>   event_date   report_date         n .event_num .report_num .delay sex  
#>   <date>       <date>          <dbl>      <dbl>       <dbl>  <dbl> <chr>
#>   [event_date] [report_date] [cases]      [...]       [...]  [...] [...]
#> 1 2023-12-25   2023-12-26         10          0           1      1 M    
#> 2 2023-12-26   2023-12-26          2          1           1      0 M    
#> 3 2023-12-25   2023-12-27          5          0           2      2 F    
#> 4 2023-12-26   2023-12-27         11          1           2      1 M    
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2023-12-27 | Event date: "event_date" | Report date: "report_date"
#> # ────────────────────────────────────────────────────────────────────────────────
```

Use the `add_strata` to specify the new column is a stratum:

``` r
df_now %>% 
  add_strata("sex")
#> # A tibble:  4 × 7
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>   event_date   report_date         n .event_num .report_num .delay sex     
#>   <date>       <date>          <dbl>      <dbl>       <dbl>  <dbl> <chr>   
#>   [event_date] [report_date] [cases]      [...]       [...]  [...] [strata]
#> 1 2023-12-25   2023-12-26         10          0           1      1 M       
#> 2 2023-12-26   2023-12-26          2          1           1      0 M       
#> 3 2023-12-25   2023-12-27          5          0           2      2 F       
#> 4 2023-12-26   2023-12-27         11          1           2      1 M       
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2023-12-27 | Event date: "event_date" | Report date: "report_date"
#> # Strata: "sex"
#> # ────────────────────────────────────────────────────────────────────────────────
```

The object now records `"sex"` as a stratification variable, preserved
through downstream operations.

## Adding temporal effects

Temporal covariates help nowcasting models incorporate weekly
seasonality, holiday effects, etc. Define the effects:

``` r
t_eff <- temporal_effects(
  day_of_week  = TRUE,
  week_of_year = TRUE, 
  holidays     = cal_us_federal())

t_eff
#> 
#> ── Temporal Effects ────────────────────────────────────────────────────────────
#> The following effects are in place:
#> • "day_of_week"
#> • "week_of_year"
#> • "holidays":
#>   1. New Year's Day, US Martin Luther King Jr. Day, US Presidents' Day, US
#>   Memorial Day, US Juneteenth, US Independence Day, US Labor Day, US Indigenous
#>   Peoples' Day, US Veterans Day, US Thanksgiving, and Christmas
```

Attach them to the dataset:

``` r
df_now %>% 
  add_temporal_effects(t_eff)
#> # A tibble:  4 × 10
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>   event_date   report_date         n .event_num .report_num .delay sex  
#>   <date>       <date>          <dbl>      <dbl>       <dbl>  <dbl> <chr>
#>   [event_date] [report_date] [cases]      [...]       [...]  [...] [...]
#> 1 2023-12-25   2023-12-26         10          0           1      1 M    
#> 2 2023-12-26   2023-12-26          2          1           1      0 M    
#> 3 2023-12-25   2023-12-27          5          0           2      2 F    
#> 4 2023-12-26   2023-12-27         11          1           2      1 M    
#>   .event_day_of_week .event_week_of_year .event_holiday
#>                <int>               <int>          <int>
#>           [t_effect]          [t_effect]     [t_effect]
#> 1                  2                   1              1
#> 2                  3                   1              0
#> 3                  2                   1              1
#> 4                  3                   1              0
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2023-12-27 | Event date: "event_date" | Report date: "report_date"
#> # T. effects: ".event_day_of_week", ".event_week_of_year", and ".event_holiday"
#> # ────────────────────────────────────────────────────────────────────────────────
```

This expands the table with `.event_day_of_week`, `.event_week_of_year`,
and `.event_holiday` columns which are automatically aligned with event
dates.

You can also attach different effects to the report:

``` r
r_eff <- temporal_effects(day_of_week = TRUE)

df_now %>% 
  add_temporal_effects(r_eff, date_type = "report_date")
#> # A tibble:  4 × 8
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>   event_date   report_date         n .event_num .report_num .delay sex  
#>   <date>       <date>          <dbl>      <dbl>       <dbl>  <dbl> <chr>
#>   [event_date] [report_date] [cases]      [...]       [...]  [...] [...]
#> 1 2023-12-25   2023-12-26         10          0           1      1 M    
#> 2 2023-12-26   2023-12-26          2          1           1      0 M    
#> 3 2023-12-25   2023-12-27          5          0           2      2 F    
#> 4 2023-12-26   2023-12-27         11          1           2      1 M    
#>   .report_day_of_week
#>                 <int>
#>            [t_effect]
#> 1                   3
#> 2                   3
#> 3                   4
#> 4                   4
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2023-12-27 | Event date: "event_date" | Report date: "report_date"
#> # T. effects: ".report_day_of_week"
#> # ────────────────────────────────────────────────────────────────────────────────
```

## Working with the “now”

You may override the default now to perform historical evaluation:

``` r
df_pruned <- df_now %>%
  filter(report_date <= ymd("2023-12-26")) %>%
  change_now(ymd("2023-12-26"))
```

Retrieve the current active nowcast horizon:

``` r
get_now(df_pruned)
#> [1] "2023-12-26"
```

## Learning more

- Introduction vignette:
  <https://rodrigozepeda.github.io/tbl.now/articles/Introduction.html>
- Full walk-through with real CDC Flusight data:
  <https://rodrigozepeda.github.io/tbl.now/articles/Example.html>
- Package reference:
  <https://rodrigozepeda.github.io/tbl.now/reference/>
