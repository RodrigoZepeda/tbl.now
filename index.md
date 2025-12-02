# Tibble now (tbl.now)

The **tbl.now** package contains a `data.frame` class that extends the
[tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
(and, hence, the [tidyverse](https://tidyverse.org/)) to nowcasting
data. The main purpose of the package is to provide a unified input
within the
[`diseasenowcasting`](https://rodrigozepeda.github.io/diseasenowcasting/)
framework.

## Installation

You can install the development version of tbl.now from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("RodrigoZepeda/tbl.now")
```

And after installation:

``` r
library(dplyr)
library(lubridate)
library(tbl.now)
library(almanac)    #Suggested for holiday effects
```

## Introduction

Traditionally in epidemiological nowcasting scenarios we have two dates:

- `event_date`: When something happened (e.g. symptom onset or a test
  was taken).
- `report_date`: When it was reported (e.g. the patient went to the
  clinic or the test results were registered).

The nowcasting problem is to estimate the total number of events **now**
that have occurred at any past `event_date` given that not all of them
have been reported yet.

## Example

In the context of nowcasting, the **tbl_now** can be thought of as a
specific [tibble()](https://tibble.tidyverse.org/reference/tibble.html)
that **guarantees** an `event_date` and a `report_date`.

For example the following data.frame represents the number of cases `n`
reported at `report_date` that happened at `event_date`

``` r
df <- tibble(
  event_date  = c(ymd("2023/12/25"), ymd("2023/12/26"), ymd("2023/12/25"), ymd("2023/12/26")),
  report_date = c(ymd("2023/12/26"), ymd("2023/12/26"), ymd("2023/12/27"), ymd("2023/12/27")),
  n = c(10, 2, 5, 11)
)

df
#> # A tibble: 4 × 3
#>   event_date report_date     n
#>   <date>     <date>      <dbl>
#> 1 2023-12-25 2023-12-26     10
#> 2 2023-12-26 2023-12-26      2
#> 3 2023-12-25 2023-12-27      5
#> 4 2023-12-26 2023-12-27     11
```

To convert to a `tbl_now` you can use the
[tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html)
function:

``` r
df_now <- tbl_now(df, event_date = "event_date", report_date = "report_date")
#> ℹ Identified data as count-incidence with counts in column "n".
df_now
#> # A tibble:  4 × 6
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>   event_date   report_date       n .event_num .report_num .delay
#>   <date>       <date>        <dbl>      <dbl>       <dbl>  <dbl>
#>   [event_date] [report_date] [...]      [...]       [...]  [...]
#> 1 2023-12-25   2023-12-26       10          0           1      1
#> 2 2023-12-26   2023-12-26        2          1           1      0
#> 3 2023-12-25   2023-12-27        5          0           2      2
#> 4 2023-12-26   2023-12-27       11          1           2      1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2023-12-27 | Event date: "event_date" | Report date: "report_date"
#> # ────────────────────────────────────────────────────────────────────────────────
```

The
[tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html)
automatically detects whether the **data-type** corresponds to
*linelist*, *count-incidence* or *count-cumulative* data, the
**date-units** for the event and report dates (frequency), and the
**now** is given by the latest date (2023-12-27). Additionally it
transforms the `event_date` into numeric (`.event_num` column) as well
as the `report_date` (`.report_num` column) and calculates the delay
(`.delay` column).

The
[tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html)
is compatible with the usual [dplyr](https://dplyr.tidyverse.org/)
operations:

``` r
df_now %>% 
  filter(n > 5)
#> # A tibble:  2 × 6
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>   event_date   report_date       n .event_num .report_num .delay
#>   <date>       <date>        <dbl>      <dbl>       <dbl>  <dbl>
#>   [event_date] [report_date] [...]      [...]       [...]  [...]
#> 1 2023-12-25   2023-12-26       10          0           1      1
#> 2 2023-12-26   2023-12-27       11          1           2      1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2023-12-27 | Event date: "event_date" | Report date: "report_date"
#> # ────────────────────────────────────────────────────────────────────────────────
```

## Temporal effects

Temporal effects can be added as covariates of the
[tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html)
using the
[temporal_effects()](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.html).

For example we can specify it includes the day of the week, the week of
the year, and whether it is holiday in the US:

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

Such effects can be added to the
[tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html)
object with the
[add_temporal_effects()](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.html)
function:

``` r
df_now %>% 
  add_temporal_effects(t_eff)
#> # A tibble:  4 × 9
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>   event_date   report_date       n .event_num .report_num .delay
#>   <date>       <date>        <dbl>      <dbl>       <dbl>  <dbl>
#>   [event_date] [report_date] [...]      [...]       [...]  [...]
#> 1 2023-12-25   2023-12-26       10          0           1      1
#> 2 2023-12-26   2023-12-26        2          1           1      0
#> 3 2023-12-25   2023-12-27        5          0           2      2
#> 4 2023-12-26   2023-12-27       11          1           2      1
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

Note that Christmas (`2023-12-25`) is marked as an `.event_holiday`,
everything corresponds to the first epidemiological week of `2024`, and
the days of the week correspond to Monday (`event_day_of_week = 2`) and
Tuesday (`event_day_of_week = 3`). All of these effects can be used as
covariates in the models.
