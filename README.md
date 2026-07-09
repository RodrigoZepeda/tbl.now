
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Tibble now (tbl.now) <a href="https://rodrigozepeda.github.io/tbl.now/"><img src="man/figures/logo.png" align="right" height="139" alt="tbl.now website" /></a>

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/RodrigoZepeda/tbl.now/graph/badge.svg)](https://app.codecov.io/gh/RodrigoZepeda/tbl.now)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/tbl.now)](https://CRAN.R-project.org/package=tbl.now)
[![R-CMD-check](https://github.com/RodrigoZepeda/tbl.now/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RodrigoZepeda/tbl.now/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

[`tbl.now`](https://rodrigozepeda.github.io/tbl.now/) provides an
extension of the [`tibble()`](https://tibble.tidyverse.org/) for
storing, validating, and manipulating epidemiological nowcasting data.
It standardizes the representation of event dates, report dates, strata,
temporal covariates, and data types (linelist and cumulative), ensuring
that downstream models within the
[`diseasenowcasting`](https://rodrigozepeda.github.io/diseasenowcasting/)
ecosystem can rely on a consistent interface.

## Installation

Install the development version from [GitHub](https://github.com/):

``` r
# install.packages("pak") # <- uncomment if you do not have `pak`
pak::pkg_install("RodrigoZepeda/tbl.now")
```

Load the package:

``` r
library(dplyr, quietly = TRUE)
library(lubridate)
library(tbl.now)
library(almanac) # Suggested for holiday effects
```

## A minimal example

Suppose you have a dataset where n cases reported on report_date belong
to events occurring on event_date:

``` r
df <- tibble(
  event_date = c(
    ymd("2023-12-25"), ymd("2023-12-26"),
    ymd("2023-12-25"), ymd("2023-12-26")
  ),
  report_date = c(
    ymd("2023-12-26"), ymd("2023-12-26"),
    ymd("2023-12-27"), ymd("2023-12-27")
  ),
  n = c(10, 2, 5, 11)
)
```

Convert it to a
[tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html):

``` r
df_now <- df |>
  tbl_now(event_date = event_date, report_date = report_date, case_count = n)
#> i Identified data as <count-incidence> with counts in column "n".

df_now
#> # A tibble:  4 x 6
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>   event_date   report_date         n .event_num .report_num .delay
#>   <date>       <date>          <dbl>      <dbl>       <dbl>  <dbl>
#>   [event_date] [report_date] [cases]      [...]       [...]  [...]
#> 1 2023-12-25   2023-12-26         10          0           1      1
#> 2 2023-12-26   2023-12-26          2          1           1      0
#> 3 2023-12-25   2023-12-27          5          0           2      2
#> 4 2023-12-26   2023-12-27         11          1           2      1
#> # --------------------------------------------------------------------------------
#> # Now: 2023-12-27 | Event date: "event_date" | Report date: "report_date"
#> # --------------------------------------------------------------------------------
```

`tbl_now()` automatically:

- infers that the **data type** is linelist (one row per event)

- determines the **date units** (daily event and report frequencies)

- computes numerical versions of the dates: `.event_num`, `.report_num`,
  and `.delay`

- sets the **now** to the most recent reporting date

Use it like any tibble:

``` r
df_now |>
  filter(n > 5)
#> # A tibble:  2 x 6
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>   event_date   report_date         n .event_num .report_num .delay
#>   <date>       <date>          <dbl>      <dbl>       <dbl>  <dbl>
#>   [event_date] [report_date] [cases]      [...]       [...]  [...]
#> 1 2023-12-25   2023-12-26         10          0           1      1
#> 2 2023-12-26   2023-12-27         11          1           2      1
#> # --------------------------------------------------------------------------------
#> # Now: 2023-12-27 | Event date: "event_date" | Report date: "report_date"
#> # --------------------------------------------------------------------------------
```

> **Note** Linelist, count-incidence and count-cumulative data is
> available for a `tbl_now`. See the [data
> types](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html#data-types)
> section of `tbl_now()` for more information.

## Adding strata

If strata was given, the
[tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html)
can easily tag the corresponding strata.

``` r
# Add the column using dplyr:
df_now <- df_now |>
  mutate(sex = c("M", "M", "F", "M"))

df_now
#> # A tibble:  4 x 7
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>   event_date   report_date         n .event_num .report_num .delay sex  
#>   <date>       <date>          <dbl>      <dbl>       <dbl>  <dbl> <chr>
#>   [event_date] [report_date] [cases]      [...]       [...]  [...] [...]
#> 1 2023-12-25   2023-12-26         10          0           1      1 M    
#> 2 2023-12-26   2023-12-26          2          1           1      0 M    
#> 3 2023-12-25   2023-12-27          5          0           2      2 F    
#> 4 2023-12-26   2023-12-27         11          1           2      1 M    
#> # --------------------------------------------------------------------------------
#> # Now: 2023-12-27 | Event date: "event_date" | Report date: "report_date"
#> # --------------------------------------------------------------------------------
```

Use the `add_strata` to specify the new column is a stratum:

``` r
df_now |>
  add_strata("sex")
#> # A tibble:  4 x 7
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>   event_date   report_date         n .event_num .report_num .delay sex     
#>   <date>       <date>          <dbl>      <dbl>       <dbl>  <dbl> <chr>   
#>   [event_date] [report_date] [cases]      [...]       [...]  [...] [strata]
#> 1 2023-12-25   2023-12-26         10          0           1      1 M       
#> 2 2023-12-26   2023-12-26          2          1           1      0 M       
#> 3 2023-12-25   2023-12-27          5          0           2      2 F       
#> 4 2023-12-26   2023-12-27         11          1           2      1 M       
#> # --------------------------------------------------------------------------------
#> # Now: 2023-12-27 | Event date: "event_date" | Report date: "report_date"
#> # Strata: "sex"
#> # --------------------------------------------------------------------------------
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
  holidays     = cal_us_federal()
)

t_eff
#> 
#> -- Temporal Effects ------------------------------------------------------------
#> The following effects are in place:
#> * "day_of_week"
#> * "week_of_year"
#> * "holidays":
#>   1. New Year's Day, US Martin Luther King Jr. Day, US Presidents' Day, US
#>   Memorial Day, US Juneteenth, US Independence Day, US Labor Day, US Indigenous
#>   Peoples' Day, US Veterans Day, US Thanksgiving, and Christmas
```

Attach them to the dataset:

``` r
df_now <- df_now |>
  add_temporal_effects(t_eff)

df_now
#> # A tibble:  4 x 7
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>   event_date   report_date         n .event_num .report_num .delay sex  
#>   <date>       <date>          <dbl>      <dbl>       <dbl>  <dbl> <chr>
#>   [event_date] [report_date] [cases]      [...]       [...]  [...] [...]
#> 1 2023-12-25   2023-12-26         10          0           1      1 M    
#> 2 2023-12-26   2023-12-26          2          1           1      0 M    
#> 3 2023-12-25   2023-12-27          5          0           2      2 F    
#> 4 2023-12-26   2023-12-27         11          1           2      1 M    
#> # --------------------------------------------------------------------------------
#> # Now: 2023-12-27 | Event date: "event_date" | Report date: "report_date"
#> # T. effects (lazy): [event_date] day_of_week, week_of_year, holidays
#> # --------------------------------------------------------------------------------
```

This lazily adds to the table `day_of_week`, `week_of_year`, and
`holiday` related to `event_date`. However it does not compute. Use
`compute_temporal_effects()` to add them as columns:

``` r
df_now |>
  compute_temporal_effects()
#> # A tibble:  4 x 10
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
#> # --------------------------------------------------------------------------------
#> # Now: 2023-12-27 | Event date: "event_date" | Report date: "report_date"
#> # T. effects: [event_date] day_of_week, week_of_year, holidays
#> # T. effect cols: ".event_day_of_week", ".event_week_of_year", and
#> # ".event_holiday"
#> # --------------------------------------------------------------------------------
```

You can also attach effects related to the `report_date`:

``` r
r_eff <- temporal_effects(day_of_week = TRUE)

df_now |>
  add_temporal_effects(r_eff, date_type = "report_date")
#> # A tibble:  4 x 7
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>   event_date   report_date         n .event_num .report_num .delay sex  
#>   <date>       <date>          <dbl>      <dbl>       <dbl>  <dbl> <chr>
#>   [event_date] [report_date] [cases]      [...]       [...]  [...] [...]
#> 1 2023-12-25   2023-12-26         10          0           1      1 M    
#> 2 2023-12-26   2023-12-26          2          1           1      0 M    
#> 3 2023-12-25   2023-12-27          5          0           2      2 F    
#> 4 2023-12-26   2023-12-27         11          1           2      1 M    
#> # --------------------------------------------------------------------------------
#> # Now: 2023-12-27 | Event date: "event_date" | Report date: "report_date"
#> # T. effects (lazy): [event_date] day_of_week, week_of_year, holidays |
#> # [report_date] day_of_week
#> # --------------------------------------------------------------------------------
```

## Working with the “now”

You may override the default now to perform historical evaluation:

``` r
df_pruned <- df_now |>
  filter(report_date <= ymd("2023-12-26")) |>
  change_now(ymd("2023-12-26"))
```

Retrieve the current active nowcast horizon:

``` r
get_now(df_pruned)
#> [1] "2023-12-26"
```

## Visualizing a `tbl_now`

The
[autoplot()](https://rodrigozepeda.github.io/tbl.now/reference/autoplot.tbl_now.html)
method gives a quick diagnostic overview of a `tbl_now`.

``` r
library(ggplot2)
library(patchwork)
data("flusight")

flusight_now <- tbl_now(flusight,
  event_date  = target_end_date,
  report_date = as_of,
  case_count  = observation,
  verbose     = FALSE
)

autoplot(flusight_now, level = 1)
```

<img src="man/figures/README-autoplot-1.png" alt="" width="100%" />

## Diagnosing reporting problems

`tbl.now` also helps diagnose two common reporting artefacts directly
from the data.

**Does the reporting delay drift over time?** `plot_delay_drift()` draws
a rolling fan chart of the delay distribution, while
`test_delay_drift()` and `test_delay_changepoint()` test for a gradual
trend or an abrupt change (with an autocorrelation-robust test, since a
delay series is correlated with itself).

**Batch reporting.** Laboratories sometimes withhold results and then
release a whole backlog at once. Such a *batch* moves reports along the
report axis without creating them, so a window spanning the lull and the
spike has an unchanged total — unlike a genuine epidemic surge, which
adds cases. `batch_screen()` uses this to tell the two apart, per report
date:

``` r
data(denguedat)
dengue <- tbl_now(denguedat,
  event_date = onset_week, report_date = report_week, verbose = FALSE
)

batches <- batch_screen(dengue, lookback = 2)

batches |>
  filter(batch) |>
  select(report_date, reported, baseline, deficit, delta, classification)
#> # A tibble: 69 x 6
#>    report_date reported baseline deficit    delta classification 
#>    <date>         <dbl>    <dbl>   <dbl>    <dbl> <chr>          
#>  1 1990-12-03       124     67    -44    101      batch_and_surge
#>  2 1991-02-04        53     38.4   14.5    0.0625 batch          
#>  3 1991-08-12        61     50.2   37.3  -26.6    batch          
#>  4 1991-12-02       241    154     12     75      batch_and_surge
#>  5 1992-01-20       126     99.2   50.0  -23.2    batch          
#>  6 1992-06-22        54     41.1   18.8   -5.83   batch          
#>  7 1993-01-18       150     62.2   -2.80  90.6    batch_and_surge
#>  8 1993-03-22        46     31.4   20.4   -5.73   batch          
#>  9 1993-09-27        65     40.7   21.5    2.85   batch          
#> 10 1993-10-25        83     53.4   24.7    4.88   batch          
#> # i 59 more rows
```

A spike paid for by a preceding `deficit`, with `delta` near zero, is a
batch; a large `delta` would instead signal a real surge.
`batch_shape_test()` adds a complementary check on whether a flagged
date drew on unusually *old* event dates. These functions are
experimental — see the *Batch detection* article.

## Extreme delays

Extreme delays can be censored with the `censor_delays_above()` function
that assigns upper bounds to the delay. In the following data frame for
example, an extreme delay of 300 is marked as `censored` with that
function:

``` r
df <- data.frame(
  onset = as.Date("2020-01-01") + c(0, 0, 1, 2),
  reported = as.Date("2020-01-01") + c(1, 5, 2, 300)
)
tn <- tbl_now(df,
  event_date = onset, report_date = reported,
  data_type = "linelist", verbose = FALSE
)

# the 300-day report becomes censored (an upper bound on its delay)
censor_delays_above(tn, max_delay = 60)
#> i Marked 1 report with delay > 60 days as censored.
#> * This delay is now an upper bound (is_censored).
#> # A tibble:  4 x 6
#> # Data type: "linelist"
#> # Frequency: Event: `days` | Report: `days`
#>   onset        reported      .event_num .report_num .delay .is_censored 
#>   <date>       <date>             <dbl>       <dbl>  <dbl> <lgl>        
#>   [event_date] [report_date]      [...]       [...]  [...] [is_censored]
#> 1 2020-01-01   2020-01-02             0           1      1 FALSE        
#> 2 2020-01-01   2020-01-06             0           5      5 FALSE        
#> 3 2020-01-02   2020-01-03             1           2      1 FALSE        
#> 4 2020-01-03   2020-10-27             2         300    298 TRUE         
#> # --------------------------------------------------------------------------------
#> # Now: 2020-10-27 | Event date: "onset" | Report date: "reported"
#> # Right-censored indicator: ".is_censored"
#> # --------------------------------------------------------------------------------
```

## Learning more

- Introduction vignette:
  <https://rodrigozepeda.github.io/tbl.now/articles/tbl.now.html>
- Full walk-through with real CDC Flusight data:
  <https://rodrigozepeda.github.io/tbl.now/articles/Example.html>
- Package reference:
  <https://rodrigozepeda.github.io/tbl.now/reference/>
