# Example analysis

In this article we show how to use the `tbl.now` framework with real
data from the Center for Disease Control (CDC).

We start by loading the required packages:

``` r
library(dplyr)
library(lubridate)
library(tbl.now)
```

## Data

For the example we will use the Flusight data from the CDC which
contains **weekly** hospital admissions of patients with confirmed
influenza (see
[`?flusight`](https://rodrigozepeda.github.io/tbl.now/reference/flusight.md)).
The data is reported **weekly** starting one week after the
observations.

``` r
data(flusight)
flusight
#> # A tibble: 491,706 × 4
#>    as_of      target_end_date location_name        observation
#>    <date>     <date>          <chr>                      <dbl>
#>  1 2023-09-23 2022-02-12      Alabama                       10
#>  2 2023-09-23 2022-02-12      Alaska                         0
#>  3 2023-09-23 2022-02-12      Arizona                       64
#>  4 2023-09-23 2022-02-12      Arkansas                      29
#>  5 2023-09-23 2022-02-12      California                    36
#>  6 2023-09-23 2022-02-12      Colorado                      29
#>  7 2023-09-23 2022-02-12      Connecticut                    0
#>  8 2023-09-23 2022-02-12      Delaware                       2
#>  9 2023-09-23 2022-02-12      District of Columbia           0
#> 10 2023-09-23 2022-02-12      Florida                       68
#> # ℹ 491,696 more rows
```

The column `target_end_date` corresponds to the **week** of observation
while `as_of` corresponds to the **week** when information was updated.
The same observation week (`target_end_date`) has then multiple report
dates (`as_of`) corresponding to future weeks of update. Data is
available for several states and territories of the United States via
`location_name`.

Finally note that the data is reported as cumulative cases
(`case-cumulative`). That is, the column `observations` contains not the
cases that were updated in date `as_of` but the **overall** number of
cases that were believed to have happend in date `target_end_date` by
date `as_of`. You can see this with an example:

``` r
flusight %>% 
  filter(location_name == "Florida" & target_end_date == ymd("2024/04/06")) 
#> # A tibble: 40 × 4
#>    as_of      target_end_date location_name observation
#>    <date>     <date>          <chr>               <dbl>
#>  1 2024-04-06 2024-04-06      Florida               307
#>  2 2024-04-13 2024-04-06      Florida               314
#>  3 2024-04-20 2024-04-06      Florida               314
#>  4 2024-04-27 2024-04-06      Florida               314
#>  5 2024-11-16 2024-04-06      Florida               312
#>  6 2024-11-30 2024-04-06      Florida               312
#>  7 2024-12-14 2024-04-06      Florida               312
#>  8 2024-12-21 2024-04-06      Florida               312
#>  9 2024-12-28 2024-04-06      Florida               312
#> 10 2025-01-11 2024-04-06      Florida               312
#> # ℹ 30 more rows
```

We can try and setup a `tbl_now` by specifying the following:

- `event_date`: When cases happened
- `report_date`: When cases were reported.
- `case_col`: Column containing information about the number of cases
  for that observation.
- `strata`: A vector containing all of the columns considered strata

Creating the `tbl_now` object like this will throw several warnings:

``` r
df_wrong <- tbl_now(flusight, event_date = "target_end_date", report_date = "as_of", 
        case_col = "observation", strata = c("location_name"))
#> Warning: Cannot accurately infer the data-type when rows are repeated across event and
#> report dates
#> Warning: Some observations in the count column "observation"
#> contain missing values.
#> ℹ Identified data as <count-incidence> with counts in column "observation".
#> Warning: *Non-unique*: Data has multiple rows for the same event (target_end_date) and
#> report(as_of) dates. Consider using `to_count()` to aggregate the data
#> or`distinct()` to remove repeated observations.
```

The first and last warnings indicate, for example, that some
observations are repeated. You can see that rows `422146`, `422147`,
`422148`, `422149` have exactly the same values:

``` r
flusight[c(422146, 422147, 422148, 422149), ]
#> # A tibble: 4 × 4
#>   as_of      target_end_date location_name observation
#>   <date>     <date>          <chr>               <dbl>
#> 1 2025-09-03 2022-02-05      Alabama                 5
#> 2 2025-09-03 2022-02-05      Alabama                 5
#> 3 2025-09-03 2022-02-05      Alabama                 5
#> 4 2025-09-03 2022-02-05      Alabama                 5
```

Using [\`dplyr’s
distinct()](https://dplyr.tidyverse.org/reference/distinct.html) we can
clean the repeated rows and fix those warnings:

``` r
flusight <- flusight %>% distinct()
```

**Missing** values are warned by the `tbl_now` object to make sure the
user knows of their presence. We can either ignore the warning, remove
or substitute the values. For the purpose of this example, we will
filter them out:

``` r
flusight <- flusight %>% filter(!is.na(observation))
```

Then try again the `tbl_now` (spoiler, we still need to fix one last
thing):

``` r
df_still_wrong <- tbl_now(flusight, event_date = "target_end_date", report_date = "as_of", 
        case_col = "observation", strata = c("location_name"))
#> ℹ Identified data as <count-incidence> with counts in column "observation".
```

The data was incorrectly identified as `count-incidence` which means
that each event-report date combination corresponds to the number of
cases that where observed for the `event_date` and reported **exactly**
at `report_date`. That is, the total number of cases that happened at
`event_date` is given by the **sum** of all of the cases for each
`report_date`.

In our case we have `count-cumulative` data where the `report_date`
reports the latest estimate of the **total** number of cases. This can
be specified with the `data_type` option:

``` r
df_flu <- tbl_now(flusight, event_date = "target_end_date", report_date = "as_of", 
        case_col = "observation", strata = c("location_name"), data_type = "count-cumulative")
```

And this results in the correct `tbl_now` object:

``` r
df_flu
#> # A tibble:  451,415 × 7
#> # Data type: "count-cumulative"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    as_of        target_end_date location_name observation .event_num .report_num
#>    <date>       <date>          <chr>               <dbl>      <dbl>       <dbl>
#>    [report_dat… [event_date]    [strata]            [...]      [...]       [...]
#>  1 2023-09-23   2022-02-12      Alabama                10          1          85
#>  2 2023-09-23   2022-02-12      Alaska                  0          1          85
#>  3 2023-09-23   2022-02-12      Arizona                64          1          85
#>  4 2023-09-23   2022-02-12      Arkansas               29          1          85
#>  5 2023-09-23   2022-02-12      California             36          1          85
#>  6 2023-09-23   2022-02-12      Colorado               29          1          85
#>  7 2023-09-23   2022-02-12      Connecticut             0          1          85
#>  8 2023-09-23   2022-02-12      Delaware                2          1          85
#>  9 2023-09-23   2022-02-12      District of …           0          1          85
#> 10 2023-09-23   2022-02-12      Florida                68          1          85
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2025-11-12 | Event date: "target_end_date" | Report date: "as_of"
#> # Strata: "location_name"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 451,405 more rows
#> # ℹ 1 more variable: .delay <dbl>
```
