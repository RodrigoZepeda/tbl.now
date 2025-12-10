# tbl.now

``` r
library(dplyr)
library(lubridate)
library(tbl.now)
```

## Introduction

In `R`, one of the main frameworks for data analysis is the
[tidyverse](https://tidyverse.org/) ([Wickham et al.
2019](#ref-tidyverse)). Within it, data is arranged with **rows**
corresponding to single observations and **columns** representing
different variables. This is referred to as [tidy
data](https://tidyr.tidyverse.org/articles/tidy-data.html) ([Wickham
2014](#ref-wickham2014tidy)).

For time series, tidy extensions to the classical `data.frame` (or
[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)) have
previously been proposed with the
[tsibble](https://tsibble.tidyverts.org/), the
[tibbletime](https://business-science.github.io/tibbletime/), and the
[timetk](https://business-science.github.io/timetk/) packages ([E. Wang,
Cook, and Hyndman 2020](#ref-wang2020new); [Y. Wang
2019](#ref-wang2019tidy); [Dancho and Vaughan 2023](#ref-timetk)).
However, in the context of epidemiological nowcasting were two different
time-indexes are on the same table, the previous methods are lacking.
This is where `tbl.now` comes in.

The tibble now (`tbl.now`) is an extension fo the
[tibble()](https://tibble.tidyverse.org/) that tags specific variables
as indexes for nowcasting models in the context of
[diseasenowcasting](https://rodrigozepeda.github.io/diseasenowcasting/).
The main purpose of the `tbl.now` is to unify the data inputted to
[diseasenowcasting](https://rodrigozepeda.github.io/diseasenowcasting/)’s
models while allowing the user to keep a `tidy` structure on the data
and perform the usual [dplyr](https://dplyr.tidyverse.org/) data
cleaning operations. This allows the user to move easierly into one of
the classical modeling workflow frameworks such as ([Gelman et al.
2020](#ref-gelman2020bayesian); [Wickham, Çetinkaya-Rundel, and
Grolemund 2023](#ref-wickham2023r)):

    Data Cleaning -> Modeling -> New Data Cleaning -> New Modeling -> ...

## The `tbl.now` framework

In epidemiological nowcasting scenarios we have at least two dates:

- `event_date`: When something happened (*e.g.* symptom onset or a test
  was taken).

- `report_date`: When it was reported (*e.g.* the patient visited a
  physician or the test results were registered).

The nowcasting problem is:

> To estimate the total number of events **now** that have occurred at
> any **past** `event_date` given that not all of them have been
> reported yet (i.e. some `report_date`(s) are potentially in the
> future).

In the context of nowcasting, the
[tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html)
can be thought of as a specific
[`tibble()`](https://tibble.tidyverse.org/reference/tibble.html) that
guarantees an `event_date` and a `report_date.` by tagging them as
**attributes** and allows for additional operations such as delay
calculation.

### Example

The
[tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html)
works from a `data.frame` by specifying the `event_date` and
`report_date` columns:

``` r
df <- data.frame(
  symptom_onset = c(ymd("2023/12/25"), ymd("2023/12/26"), ymd("2023/12/25"), ymd("2023/12/26")),
  medical_visit = c(ymd("2023/12/26"), ymd("2023/12/26"), ymd("2023/12/27"), ymd("2023/12/27")),
  n = c(10, 2, 5, 11)
)

df
#>   symptom_onset medical_visit  n
#> 1    2023-12-25    2023-12-26 10
#> 2    2023-12-26    2023-12-26  2
#> 3    2023-12-25    2023-12-27  5
#> 4    2023-12-26    2023-12-27 11
```

In the previous `data.frame`, column **symptom_onset** represents the
`event` while **medical_visit** corresponds to the `report`. Finally `n`
corresponds to the number of cases which can be specified in the
`case_count` variable.

The previous options can be specified in the
[tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html):

``` r
df %>% 
  tbl_now(event_date = symptom_onset, report_date = medical_visit, case_count = n)
#> ℹ Identified data as <count-incidence> with counts in column "n".
#> # A tibble:  4 × 6
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>   symptom_onset medical_visit       n .event_num .report_num .delay
#>   <date>        <date>          <dbl>      <dbl>       <dbl>  <dbl>
#>   [event_date]  [report_date] [cases]      [...]       [...]  [...]
#> 1 2023-12-25    2023-12-26         10          0           1      1
#> 2 2023-12-26    2023-12-26          2          1           1      0
#> 3 2023-12-25    2023-12-27          5          0           2      2
#> 4 2023-12-26    2023-12-27         11          1           2      1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2023-12-27 | Event date: "symptom_onset" | Report date: "medical_visit"
#> # ────────────────────────────────────────────────────────────────────────────────
```

Notice that the
[tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html)
function does several things at once:

- It auto-detects the **data-type** as `count-incidence`. Other data
  types such as `linelist` are available and are discussed in section
  @ref(data-types).

- It auto-detects the **units (frequency)** of the event and report
  dates as ‘daily’ for both.

- It **tags** `symptom_onset` as an `event_date`, `medical_visit` as a
  `report_date` and `n` as `cases`.

- It generates the `.event_num` and `.report_num` columns which are
  numerical versions of the `event_date` and `report_date` columns.

- It generates the `.delay` column with the delay given by the
  difference between report and event:
  `.delay = report_date - event_date`.

- It automatically detects the **now** for the nowcasting.

The following sections explain each of these perks of the `tbl.now`
framework as well as additional functions that can be applied to a
`tbl.now`.

## Attributes of a [tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html)

A
[tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html)
contains saves several of its column names as
[`attributes()`](https://rdrr.io/r/base/attributes.html). They are the
following:

- **Now** (`now`): Refers to the **now** of the nowcast. That is, the
  date for which the prediction from the nowcast should be made. Ideally
  it refers to the most recent `report_date`.

- **Event date** (`event_date`): Name of the column with the dates when
  the event of interest happened. The nowcast will predict how many
  observations will eventually be observed in `event_date`

- **Report date** (`report_date`): Name of the column with the dates
  when the event of interest was reported. The nowcast will predict how
  many observations will eventually be observed for `event_date` across
  all `report_date`s.

- **Event units** (`event_units`): The units (e.g. “days”, “weeks”,
  “numeric”) in which the `event_date` is registered. This is used for
  computing the `delay`.

- **Report units** (`report_units`): The units (e.g. “days”, “weeks”,
  “numeric”) in which the `report_date` is registered. This is used for
  computing the `delay`.

- **Data type** (`data_type`): Either `linelist` if each row corresponds
  to a different observation or one of the following counts if data is
  aggregated. In `count-incidence`, each row corresponds to the number
  of cases observed exactly in the `report_date`. The `count-cumulative`
  represents how many cases have been observed *up until (and
  including)* the `report_date`. See the [data types](#data-types)
  section for more details.

- **Strata** (`strata`): \[*optional*\][¹](#fn1) Name of the columns
  marking the strata for the nowcast. The strata should be those
  variables for which the nowcast should compute separate results. For
  example, if strata is age-group, the nowcast should compute one model
  for each age-group.

- **Covariates** (`covariates`): \[*optional*\] Name of the columns
  marking the covariates for the nowcast. The covariates should be those
  variables that help improve the nowcast. For example, a covariate for
  a dengue nowcast can be precipitaion or humidity levels.

- **Batched indicator** (`is_batched`): \[*optional*\] Name of the
  columns for the nowcast indicating whether the `report_date` is
  right-censored. As an example of cases when this happens is when some
  data-sources have a technical error and all reports are not registered
  until very late. The `report_date` would come in a **batch** in such
  cases and not represent the actual dynamics of the process but a
  right-censored version.

- **Cases** (`case_count`): \[*optional*\] If the `data-type` is any
  type of **count** data (`count-incidence` or `count-prevalence`), the
  column where the counts of events is registered.

- **Temporal effects** (`temporal_effects`): \[*optional*\] Refer to the
  temporal effects in the data created by the
  [temporal_effects()](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.html)
  function. See the [temporal effects](#temporal-effects) section for
  more details.

Values for all attributes can be accessed via the `get_*` functions.
Examples include:
[`get_event_date()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md),
and
[`get_strata()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md).

### Data types

The following data-types are admitted at
[tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html)
objects:

- **Linelist**: Each row is an observation that was reported at
  `report_date` as happening at `event_date`.

| patient | event_date | report_date | .event_num | .report_num | .delay |
|--------:|:-----------|:------------|-----------:|------------:|-------:|
|       1 | 2020-09-12 | 2020-09-12  |          0 |           0 |      0 |
|       2 | 2020-09-12 | 2020-09-12  |          0 |           0 |      0 |
|       3 | 2020-09-12 | 2020-09-13  |          0 |           1 |      1 |
|       4 | 2020-09-13 | 2020-09-13  |          1 |           1 |      0 |
|       5 | 2020-09-13 | 2020-09-13  |          1 |           1 |      0 |
|       6 | 2020-09-13 | 2020-09-13  |          1 |           1 |      0 |

Linelist data

- **Count-incidence**: Each `report_date`-`event_date` combination
  contains the total number of cases observed *exactly* at `report_date`
  for `event_date`.

|   n | event_date | report_date | .event_num | .report_num | .delay |
|----:|:-----------|:------------|-----------:|------------:|-------:|
|   7 | 2020-09-12 | 2020-09-12  |          0 |           0 |      0 |
|   1 | 2020-09-12 | 2020-09-13  |          0 |           1 |      1 |
|   9 | 2020-09-12 | 2020-09-14  |          0 |           2 |      2 |
|   5 | 2020-09-13 | 2020-09-13  |          1 |           1 |      0 |
|   0 | 2020-09-13 | 2020-09-14  |          1 |           2 |      1 |
|   2 | 2020-09-13 | 2020-09-15  |          1 |           3 |      2 |

Count-incidence data

- **Count-cumulative** Each `report_date`-`event_date` combination
  contains the total number of cases observed up until `report_date` for
  `event_date`. The most recent `report_date` contains the best
  estimation of cases happening at `event_date`.

|   n | event_date | report_date | .event_num | .report_num | .delay |
|----:|:-----------|:------------|-----------:|------------:|-------:|
|   1 | 2020-09-12 | 2020-09-12  |          0 |           0 |      0 |
|   5 | 2020-09-12 | 2020-09-13  |          0 |           1 |      1 |
|   8 | 2020-09-12 | 2020-09-14  |          0 |           2 |      2 |
|   2 | 2020-09-13 | 2020-09-13  |          1 |           1 |      0 |
|   2 | 2020-09-13 | 2020-09-14  |          1 |           2 |      1 |
|   4 | 2020-09-13 | 2020-09-15  |          1 |           3 |      2 |

Count-cumulative data

The
[`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md)
function allows you to easily convert between different data-types.

### Transforming a [tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html) to count data

A
[tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html)
can do the following transformations via the
[`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md)
function between data types:

- **linelist to count-incidence**: Aggregates each observation in the
  linelist by report and event date. The `n` column contains how many
  individuals were specifically observed at that `report_date` for the
  `event_date` in question.

``` r
df_linelist %>% 
  to_count(to = "count-incidence")
#> # A tibble:  3 × 6
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>   event_date   report_date   .event_num .report_num       n .delay
#>   <date>       <date>             <dbl>       <dbl>   <int>  <dbl>
#>   [event_date] [report_date]      [...]       [...] [cases]  [...]
#> 1 2020-09-12   2020-09-12             0           0       2      0
#> 2 2020-09-12   2020-09-13             0           1       1      1
#> 3 2020-09-13   2020-09-13             1           1       3      0
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2020-09-13 | Event date: "event_date" | Report date: "report_date"
#> # ────────────────────────────────────────────────────────────────────────────────
```

- **linelist to count-cumulative**: Aggregates each observation in the
  linelist by report and event date. The `n` column contains how many
  individuals had been observed up untill that `report_date` (including
  the previous dates) for the `event_date` in question.

``` r
df_linelist %>% 
  to_count(to = "count-cumulative")
#> # A tibble:  3 × 6
#> # Data type: "count-cumulative"
#> # Frequency: Event: `days` | Report: `days`
#>   event_date   report_date   .event_num .report_num       n .delay
#>   <date>       <date>             <dbl>       <dbl>   <int>  <dbl>
#>   [event_date] [report_date]      [...]       [...] [cases]  [...]
#> 1 2020-09-12   2020-09-12             0           0       2      0
#> 2 2020-09-12   2020-09-13             0           1       3      1
#> 3 2020-09-13   2020-09-13             1           1       3      0
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2020-09-13 | Event date: "event_date" | Report date: "report_date"
#> # ────────────────────────────────────────────────────────────────────────────────
```

> **Note** In the previous example the `n` counts `3` as it is
> aggregating the `1` observed at `report_date = 2020-09-13` and the `2`
> observed at `report_date = 2020-09-12`. This is the difference between
> the **count-incidence** that specifies the ones observed **exactly**
> on that date and the **count-cumulative** that specifies the ones
> observed up **until and including** that date.

- **count-incidence to count-cumulative**: Aggregates each observation
  accumulating how many cases for that `event_date` had been observed up
  until that `report_date`:

``` r
df_count_inc %>% 
  to_count(to = "count-cumulative")
#> # A tibble:  6 × 6
#> # Data type: "count-cumulative"
#> # Frequency: Event: `days` | Report: `days`
#>   event_date   report_date   .event_num .report_num       n .delay
#>   <date>       <date>             <dbl>       <dbl>   <dbl>  <dbl>
#>   [event_date] [report_date]      [...]       [...] [cases]  [...]
#> 1 2020-09-12   2020-09-12             0           0       7      0
#> 2 2020-09-12   2020-09-13             0           1       8      1
#> 3 2020-09-12   2020-09-14             0           2      17      2
#> 4 2020-09-13   2020-09-13             1           1       5      0
#> 5 2020-09-13   2020-09-14             1           2       5      1
#> 6 2020-09-13   2020-09-15             1           3       7      2
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2020-09-15 | Event date: "event_date" | Report date: "report_date"
#> # ────────────────────────────────────────────────────────────────────────────────
```

- **Aggregate repeated count-\* events**: The `to_count` function can
  also be used to aggregate data from one form to the same form. As an
  example of such a case consider the following
  [tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html):

``` r
df_example <- data.frame(
  n           = c(8, 11, 0, 1, 1, 5, 2, 4, 1, 10, 9, 11, 3, 1),
  sex         = c(rep("M", 3), rep("F", 4), rep("M", 2), rep("F", 5)),
  event_date  = c(rep(ymd("2020/09/12"), 3),
                  rep(ymd("2020/09/12"), 4),
                  rep(ymd("2020/09/13"), 2),
                  rep(ymd("2020/09/13"), 5)),
  report_date = c(ymd("2020/09/12"), ymd("2020/09/13"), ymd("2020/09/14"),
                  ymd("2020/09/12"), ymd("2020/09/13"), ymd("2020/09/14"),
                  ymd("2020/09/15"), ymd("2020/09/13"), ymd("2020/09/14"),
                  ymd("2020/09/13"), ymd("2020/09/14"),
                  ymd("2020/09/15"), ymd("2020/09/16"), ymd("2020/09/17"))) 

tbl_example <- df_example %>% 
  tbl_now(event_date = event_date, report_date = report_date, 
          case_count = n, strata = sex, verbose = FALSE)

tbl_example
#> # A tibble:  14 × 7
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>          n sex      event_date   report_date   .event_num .report_num .delay
#>      <dbl> <chr>    <date>       <date>             <dbl>       <dbl>  <dbl>
#>    [cases] [strata] [event_date] [report_date]      [...]       [...]  [...]
#>  1       8 M        2020-09-12   2020-09-12             0           0      0
#>  2      11 M        2020-09-12   2020-09-13             0           1      1
#>  3       0 M        2020-09-12   2020-09-14             0           2      2
#>  4       1 F        2020-09-12   2020-09-12             0           0      0
#>  5       1 F        2020-09-12   2020-09-13             0           1      1
#>  6       5 F        2020-09-12   2020-09-14             0           2      2
#>  7       2 F        2020-09-12   2020-09-15             0           3      3
#>  8       4 M        2020-09-13   2020-09-13             1           1      0
#>  9       1 M        2020-09-13   2020-09-14             1           2      1
#> 10      10 F        2020-09-13   2020-09-13             1           1      0
#> 11       9 F        2020-09-13   2020-09-14             1           2      1
#> 12      11 F        2020-09-13   2020-09-15             1           3      2
#> 13       3 F        2020-09-13   2020-09-16             1           4      3
#> 14       1 F        2020-09-13   2020-09-17             1           5      4
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2020-09-17 | Event date: "event_date" | Report date: "report_date"
#> # Strata: "sex"
#> # ────────────────────────────────────────────────────────────────────────────────
```

Notice that the data is already in `count-incidence` format with `sex`
being a strata. If we wanted the **overall** number of cases (not by
strata) we can remove the strata and then use the `to_count` function to
aggregate all the races while keeping the `count-incidence` structure:

1.  Remove the strata

``` r
tbl_example <- tbl_example %>% 
  remove_all_strata()

#This removes the strata but doesn't aggregate everything
tbl_example
#> # A tibble:  14 × 7
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>          n sex   event_date   report_date   .event_num .report_num .delay
#>      <dbl> <chr> <date>       <date>             <dbl>       <dbl>  <dbl>
#>    [cases] [...] [event_date] [report_date]      [...]       [...]  [...]
#>  1       8 M     2020-09-12   2020-09-12             0           0      0
#>  2      11 M     2020-09-12   2020-09-13             0           1      1
#>  3       0 M     2020-09-12   2020-09-14             0           2      2
#>  4       1 F     2020-09-12   2020-09-12             0           0      0
#>  5       1 F     2020-09-12   2020-09-13             0           1      1
#>  6       5 F     2020-09-12   2020-09-14             0           2      2
#>  7       2 F     2020-09-12   2020-09-15             0           3      3
#>  8       4 M     2020-09-13   2020-09-13             1           1      0
#>  9       1 M     2020-09-13   2020-09-14             1           2      1
#> 10      10 F     2020-09-13   2020-09-13             1           1      0
#> 11       9 F     2020-09-13   2020-09-14             1           2      1
#> 12      11 F     2020-09-13   2020-09-15             1           3      2
#> 13       3 F     2020-09-13   2020-09-16             1           4      3
#> 14       1 F     2020-09-13   2020-09-17             1           5      4
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2020-09-17 | Event date: "event_date" | Report date: "report_date"
#> # ────────────────────────────────────────────────────────────────────────────────
```

2.  And then aggregate to a `count-incidence`:

``` r
tbl_example <- tbl_example %>% 
  to_count(to = "count-incidence")

#It summed all the `n` columns with one entry per observation 
tbl_example
#> # A tibble:  9 × 6
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>   event_date   report_date   .event_num .report_num       n .delay
#>   <date>       <date>             <dbl>       <dbl>   <dbl>  <dbl>
#>   [event_date] [report_date]      [...]       [...] [cases]  [...]
#> 1 2020-09-12   2020-09-12             0           0       9      0
#> 2 2020-09-12   2020-09-13             0           1      12      1
#> 3 2020-09-12   2020-09-14             0           2       5      2
#> 4 2020-09-12   2020-09-15             0           3       2      3
#> 5 2020-09-13   2020-09-13             1           1      14      0
#> 6 2020-09-13   2020-09-14             1           2      10      1
#> 7 2020-09-13   2020-09-15             1           3      11      2
#> 8 2020-09-13   2020-09-16             1           4       3      3
#> 9 2020-09-13   2020-09-17             1           5       1      4
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2020-09-17 | Event date: "event_date" | Report date: "report_date"
#> # ────────────────────────────────────────────────────────────────────────────────
```

### Temporal effects

Temporal effects can be added as a special type of covariate to the
[tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html)
using the
[temporal_effects()](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.html)
function.

For example, we can specify to include covariates for the day of the
week, the week of the year, and whether it is holiday in the US:

``` r
library(almanac)

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

Note that the holidays calendar is an
[rcalendar](https://davisvaughan.github.io/almanac/reference/rcalendar.html)
object from the
[almanac](https://davisvaughan.github.io/almanac/articles/almanac.html)
package.

Temporal effects can be added to the
[tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html)
object with the
[add_temporal_effects()](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.html)
function:

``` r
data("denguedat")

#Create a tbl_now
df_now <- denguedat %>% 
  tbl_now(event_date = onset_week, report_date = report_week,
          verbose = FALSE, strata = gender)

#Add temporal effects (see them as . columns)
df_now %>% 
  add_temporal_effects(t_eff) 
#> # A tibble:  52,987 × 9
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week   gender   .event_num .report_num .delay
#>    <date>       <date>        <chr>         <dbl>       <dbl>  <dbl>
#>    [event_date] [report_date] [strata]      [...]       [...]  [...]
#>  1 1990-01-01   1990-01-01    Male              0           0      0
#>  2 1990-01-01   1990-01-01    Female            0           0      0
#>  3 1990-01-01   1990-01-01    Female            0           0      0
#>  4 1990-01-01   1990-01-08    Female            0           1      1
#>  5 1990-01-01   1990-01-08    Male              0           1      1
#>  6 1990-01-01   1990-01-15    Female            0           2      2
#>  7 1990-01-01   1990-01-15    Female            0           2      2
#>  8 1990-01-01   1990-01-15    Female            0           2      2
#>  9 1990-01-01   1990-01-22    Female            0           3      3
#> 10 1990-01-01   1990-01-08    Female            0           1      1
#>    .event_day_of_week .event_week_of_year .event_holiday
#>                 <int>               <int>          <int>
#>            [t_effect]          [t_effect]     [t_effect]
#>  1                  2                   1              1
#>  2                  2                   1              1
#>  3                  2                   1              1
#>  4                  2                   1              1
#>  5                  2                   1              1
#>  6                  2                   1              1
#>  7                  2                   1              1
#>  8                  2                   1              1
#>  9                  2                   1              1
#> 10                  2                   1              1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "onset_week" | Report date: "report_week"
#> # Strata: "gender"
#> # T. effects: ".event_day_of_week", ".event_week_of_year", and ".event_holiday"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows
```

### Modifying the attributes of a [tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html)

A
[tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html)’s
attributes can be modified with the
[add\_\*](https://rodrigozepeda.github.io/tbl.now/reference/add.html),
[change\_\*](https://rodrigozepeda.github.io/tbl.now/reference/change.html)
or
[remove\_\*](https://rodrigozepeda.github.io/tbl.now/reference/remove.html)
functions. They follow sort of the same pattern.

Here is an example of creating a
[tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html)
and adding strata and temporal effects, changing the strata later and
removing the temporal effects.

``` r
data("mpoxdat")

df_now <- mpoxdat %>% 
  tbl_now(event_date = dx_date, report_date = dx_report_date,
          case_count = n, verbose = FALSE, strata = race)

df_now
#> # A tibble:  1,417 × 7
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>    dx_date      dx_report_date race              n .event_num .report_num .delay
#>    <date>       <date>         <chr>         <int>      <dbl>       <dbl>  <dbl>
#>    [event_date] [report_date]  [strata]      [cas…      [...]       [...]  [...]
#>  1 2022-07-08   2022-07-12     Asian             4          0           4      4
#>  2 2022-07-08   2022-07-12     Black             6          0           4      4
#>  3 2022-07-08   2022-07-12     Hispanic          6          0           4      4
#>  4 2022-07-08   2022-07-12     Non-Hispanic…     6          0           4      4
#>  5 2022-07-08   2022-07-13     Asian             2          0           5      5
#>  6 2022-07-08   2022-07-13     Black             3          0           5      5
#>  7 2022-07-08   2022-07-13     Hispanic          8          0           5      5
#>  8 2022-07-08   2022-07-13     Non-Hispanic…     5          0           5      5
#>  9 2022-07-08   2022-07-14     Black             1          0           6      6
#> 10 2022-07-08   2022-07-14     Hispanic          3          0           6      6
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2023-05-19 | Event date: "dx_date" | Report date: "dx_report_date"
#> # Strata: "race"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 1,407 more rows
```

We can `change` the strata to include a new `race` column which is in
upper case:

``` r
df_now <- df_now %>% 
  mutate(RACE_UPPER = toupper(race)) %>% 
  change_strata(RACE_UPPER)

df_now
#> # A tibble:  1,417 × 8
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>    dx_date      dx_report_date race              n .event_num .report_num .delay
#>    <date>       <date>         <chr>         <int>      <dbl>       <dbl>  <dbl>
#>    [event_date] [report_date]  [...]         [cas…      [...]       [...]  [...]
#>  1 2022-07-08   2022-07-12     Asian             4          0           4      4
#>  2 2022-07-08   2022-07-12     Black             6          0           4      4
#>  3 2022-07-08   2022-07-12     Hispanic          6          0           4      4
#>  4 2022-07-08   2022-07-12     Non-Hispanic…     6          0           4      4
#>  5 2022-07-08   2022-07-13     Asian             2          0           5      5
#>  6 2022-07-08   2022-07-13     Black             3          0           5      5
#>  7 2022-07-08   2022-07-13     Hispanic          8          0           5      5
#>  8 2022-07-08   2022-07-13     Non-Hispanic…     5          0           5      5
#>  9 2022-07-08   2022-07-14     Black             1          0           6      6
#> 10 2022-07-08   2022-07-14     Hispanic          3          0           6      6
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2023-05-19 | Event date: "dx_date" | Report date: "dx_report_date"
#> # Strata: "RACE_UPPER"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 1,407 more rows
#> # ℹ 1 more variable: RACE_UPPER <chr>
```

Temporal effects can be added using
[add_temporal_effects()](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.html):

``` r
df_now <- df_now %>% 
  add_temporal_effects(temporal_effects(week_of_year = TRUE))

df_now
#> # A tibble:  1,417 × 9
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>    dx_date      dx_report_date race              n .event_num .report_num .delay
#>    <date>       <date>         <chr>         <int>      <dbl>       <dbl>  <dbl>
#>    [event_date] [report_date]  [...]         [cas…      [...]       [...]  [...]
#>  1 2022-07-08   2022-07-12     Asian             4          0           4      4
#>  2 2022-07-08   2022-07-12     Black             6          0           4      4
#>  3 2022-07-08   2022-07-12     Hispanic          6          0           4      4
#>  4 2022-07-08   2022-07-12     Non-Hispanic…     6          0           4      4
#>  5 2022-07-08   2022-07-13     Asian             2          0           5      5
#>  6 2022-07-08   2022-07-13     Black             3          0           5      5
#>  7 2022-07-08   2022-07-13     Hispanic          8          0           5      5
#>  8 2022-07-08   2022-07-13     Non-Hispanic…     5          0           5      5
#>  9 2022-07-08   2022-07-14     Black             1          0           6      6
#> 10 2022-07-08   2022-07-14     Hispanic          3          0           6      6
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2023-05-19 | Event date: "dx_date" | Report date: "dx_report_date"
#> # Strata: "RACE_UPPER"
#> # T. effects: ".event_week_of_year"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 1,407 more rows
#> # ℹ 2 more variables: RACE_UPPER <chr>, .event_week_of_year <int>
```

The remove functions can be used in a similar way, for example, to
remove the effects and strata we just added:

``` r
df_now <- df_now %>% 
  remove_temporal_effects() %>% 
  remove_all_strata()

df_now
#> # A tibble:  1,417 × 8
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>    dx_date      dx_report_date race              n .event_num .report_num .delay
#>    <date>       <date>         <chr>         <int>      <dbl>       <dbl>  <dbl>
#>    [event_date] [report_date]  [...]         [cas…      [...]       [...]  [...]
#>  1 2022-07-08   2022-07-12     Asian             4          0           4      4
#>  2 2022-07-08   2022-07-12     Black             6          0           4      4
#>  3 2022-07-08   2022-07-12     Hispanic          6          0           4      4
#>  4 2022-07-08   2022-07-12     Non-Hispanic…     6          0           4      4
#>  5 2022-07-08   2022-07-13     Asian             2          0           5      5
#>  6 2022-07-08   2022-07-13     Black             3          0           5      5
#>  7 2022-07-08   2022-07-13     Hispanic          8          0           5      5
#>  8 2022-07-08   2022-07-13     Non-Hispanic…     5          0           5      5
#>  9 2022-07-08   2022-07-14     Black             1          0           6      6
#> 10 2022-07-08   2022-07-14     Hispanic          3          0           6      6
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2023-05-19 | Event date: "dx_date" | Report date: "dx_report_date"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 1,407 more rows
#> # ℹ 1 more variable: RACE_UPPER <chr>
```

### Modifying a [tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html) with `dplyr`

All
[tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html)
objects are extensions of the [tibble()](https://tibble.tidyverse.org/).
Hence the classical [dplyr](https://dplyr.tidyverse.org/) verbs can be
used to operate on the
[tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html)
while it tries to automatically accomodate for the changes. As an
example, renaming a strata also changes the strata attribute:

``` r
library(dplyr)
data(denguedat)
df_now <- tbl_now(denguedat, event_date = onset_week, 
                  report_date = report_week, strata = gender,
                  verbose = FALSE)
```

You can see that the strata is `gender`:

``` r
get_strata(df_now)
#> [1] "gender"
```

However, if we rename the column, the strata will also be renamed:

``` r
df_now <- df_now %>% 
  rename(male_or_female = gender)

get_strata(df_now)
#> [1] "male_or_female"
```

If an object cannot keep its
[tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html)
structure it will collapse into a
[tibble()](https://tibble.tidyverse.org/) which is what happens in this
example where summarise collapses all dates:

``` r
df_now %>% 
  summarise(male_prop = sum(male_or_female == "Male") / n())
#> Warning: Dropping `tbl_now` attributes and converting to `tibble`
#> # A tibble: 1 × 1
#>   male_prop
#>       <dbl>
#> 1     0.498
```

### Updating a [tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html)

A
[tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html)
can be updated with the `update` function from another `data.frame`,
[tibble()](https://tibble.tidyverse.org/) or
[tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html).
As long as they have the same columns it will by default copy the
strata, covariate and temporal effects from the first
[tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html)
and apply it to the update. It also updates the `now` estimation.

Consider the following `data.frame`

``` r
#Initial observations
df <- data.frame(
    patient     = 1:6,
    event_date  = c(rep(ymd("2020/09/12"), 3), rep(ymd("2020/09/13"), 3)),
    report_date = c(ymd("2020/09/12"), ymd("2020/09/13"), ymd("2020/09/14"),
                   ymd("2020/09/13"), ymd("2020/09/14"), ymd("2020/09/15")))

df_now <- tbl_now(df, event_date = event_date, 
                  report_date = report_date, verbose = FALSE)
```

and its updated version (new data):

``` r
#Initial observations
df_new <- data.frame(
    patient     = 7:13,
    event_date  = c(ymd("2020/09/13"), rep(ymd("2020/09/14"), 3), 
                    rep(ymd("2020/09/15"), 3)),
    report_date = c(ymd("2020/09/14"), ymd("2020/09/14"), ymd("2020/09/15"), 
                    ymd("2020/09/16"),
                    ymd("2020/09/15"), ymd("2020/09/16"), ymd("2020/09/17")))
```

The full
[tbl_now()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.html)
can be updated with the
[update()](https://rodrigozepeda.github.io/tbl.now/reference/update.tbl_now.html)
function:

``` r
df_updated <- update(df_now, new_data = df_new)
#> Warning: Attribute 'now' (2020-09-15) seems to be in the past (before maximum
#> report_date (2020-09-17))
#> Attribute 'now' (2020-09-15) seems to be in the past (before maximum
#> report_date (2020-09-17))

df_updated
#> # A tibble:  13 × 6
#> # Data type: "linelist"
#> # Frequency: Event: `days` | Report: `days`
#>    patient event_date   report_date   .event_num .report_num .delay
#>      <int> <date>       <date>             <dbl>       <dbl>  <dbl>
#>      [...] [event_date] [report_date]      [...]       [...]  [...]
#>  1       1 2020-09-12   2020-09-12             0           0      0
#>  2       2 2020-09-12   2020-09-13             0           1      1
#>  3       3 2020-09-12   2020-09-14             0           2      2
#>  4       4 2020-09-13   2020-09-13             1           1      0
#>  5       5 2020-09-13   2020-09-14             1           2      1
#>  6       6 2020-09-13   2020-09-15             1           3      2
#>  7       7 2020-09-13   2020-09-14             1           2      1
#>  8       8 2020-09-14   2020-09-14             2           2      0
#>  9       9 2020-09-14   2020-09-15             2           3      1
#> 10      10 2020-09-14   2020-09-16             2           4      2
#> 11      11 2020-09-15   2020-09-15             3           3      0
#> 12      12 2020-09-15   2020-09-16             3           4      1
#> 13      13 2020-09-15   2020-09-17             3           5      2
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2020-09-17 | Event date: "event_date" | Report date: "report_date"
#> # ────────────────────────────────────────────────────────────────────────────────
```

## Other functions (utilities)

### Convert epiweeks to dates

The
[week_2_date()](https://rodrigozepeda.github.io/tbl.now/reference/week_2_date.html)
function allows you to convert from a `data.frame` with epidemiological
weeks and years to specific dates:

``` r
df <- data.frame(
  epidemiological_week = 1:5,
  epidemiological_year = rep(2024, 5)
)

df %>%
  week_2_date(
    week_col = epidemiological_week,
    year_col = epidemiological_year
  )
#>   epidemiological_week epidemiological_year       date
#> 1                    1                 2024 2023-12-31
#> 2                    2                 2024 2024-01-07
#> 3                    3                 2024 2024-01-14
#> 4                    4                 2024 2024-01-21
#> 5                    5                 2024 2024-01-28
```

### Reports

The
[get\_\*\_reported_cases()](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.html)
functions get either the initial or the latest number of reported cases.
This functions are useful to compare the initial number of cases
believed to have happened against the latest number of cases. Consider
the following example where initially 10 cases were suspected to have
happened but in the latest reported number up to 15 (=10 + 1 + 1 + 3)
cases happened for the same date:

``` r
df_reports <- data.frame(
  n           = c(10, 1, 1, 0, 0, 3),
  event_date  = rep(ymd("2020/09/12"), 6),
  report_date = c(ymd("2020/09/12"),
                  ymd("2020/09/13"),
                  ymd("2020/09/14"),
                  ymd("2020/09/15"),
                  ymd("2020/09/16"),
                  ymd("2020/09/17"))) 

tbl_reports <- df_reports %>% 
  tbl_now(event_date = event_date, report_date = report_date, 
          verbose = FALSE, case_count = n, report_units = "days", 
          event_units = "days")

tbl_reports
#> # A tibble:  6 × 6
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>         n event_date   report_date   .event_num .report_num .delay
#>     <dbl> <date>       <date>             <dbl>       <dbl>  <dbl>
#>   [cases] [event_date] [report_date]      [...]       [...]  [...]
#> 1      10 2020-09-12   2020-09-12             0           0      0
#> 2       1 2020-09-12   2020-09-13             0           1      1
#> 3       1 2020-09-12   2020-09-14             0           2      2
#> 4       0 2020-09-12   2020-09-15             0           3      3
#> 5       0 2020-09-12   2020-09-16             0           4      4
#> 6       3 2020-09-12   2020-09-17             0           5      5
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2020-09-17 | Event date: "event_date" | Report date: "report_date"
#> # ────────────────────────────────────────────────────────────────────────────────
```

Initially in the first report this was the number of cases

``` r
get_initial_reported_cases(tbl_reports)
#> # A tibble:  1 × 6
#> # Data type: "count-cumulative"
#> # Frequency: Event: `days` | Report: `days`
#>   event_date   report_date   .event_num .report_num       n .delay
#>   <date>       <date>             <dbl>       <dbl>   <dbl>  <dbl>
#>   [event_date] [report_date]      [...]       [...] [cases]  [...]
#> 1 2020-09-12   2020-09-12             0           0      10      0
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2020-09-17 | Event date: "event_date" | Report date: "report_date"
#> # ────────────────────────────────────────────────────────────────────────────────
```

But in the end 15 cases were observed:

``` r
get_latest_reported_cases(tbl_reports)
#> # A tibble:  1 × 6
#> # Data type: "count-cumulative"
#> # Frequency: Event: `days` | Report: `days`
#>   event_date   report_date   .event_num .report_num       n .delay
#>   <date>       <date>             <dbl>       <dbl>   <dbl>  <dbl>
#>   [event_date] [report_date]      [...]       [...] [cases]  [...]
#> 1 2020-09-12   2020-09-17             0           5      15      5
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2020-09-17 | Event date: "event_date" | Report date: "report_date"
#> # ────────────────────────────────────────────────────────────────────────────────
```

### Week alignment

The
[align_week()](https://rodrigozepeda.github.io/tbl.now/reference/align_week.html)
function allows you to take dates from different days of the same
epidemiological week and set them all to the same date. This is useful
for computing time differences between weekly reports (to avoid decimal
times).

``` r
df <- data.frame(
  date = c(ymd("2022-10-31"), ymd("2022-11-07"), ymd("2022-11-13")),
  epiweek = c(44, 45, 46)
)

# Align to Sundays
df_aligned <- align_week(df, date_col = date)
df_aligned
#>         date epiweek date_aligned
#> 1 2022-10-31      44   2022-10-30
#> 2 2022-11-07      45   2022-11-06
#> 3 2022-11-13      46   2022-11-13
```

we can check they are actually Sundays with the
[wday()](https://lubridate.tidyverse.org/reference/day.html) function
from the [lubridate](https://lubridate.tidyverse.org/index.html)
package:

``` r
df_aligned %>% 
  mutate(day_label = wday(date_aligned, label = TRUE, abbr = FALSE))
#>         date epiweek date_aligned day_label
#> 1 2022-10-31      44   2022-10-30    Sunday
#> 2 2022-11-07      45   2022-11-06    Sunday
#> 3 2022-11-13      46   2022-11-13    Sunday
```

## References

Dancho, Matt, and Davis Vaughan. 2023. *Timetk: A Tool Kit for Working
with Time Series*. <https://doi.org/10.32614/CRAN.package.timetk>.

Gelman, Andrew, Aki Vehtari, Daniel Simpson, Charles C Margossian, Bob
Carpenter, Yuling Yao, Lauren Kennedy, Jonah Gabry, Paul-Christian
Bürkner, and Martin Modrák. 2020. “Bayesian Workflow.” *arXiv Preprint
arXiv:2011.01808*.

Wang, Earo, Dianne Cook, and Rob J Hyndman. 2020. “A New Tidy Data
Structure to Support Exploration and Modeling of Temporal Data.”
*Journal of Computational and Graphical Statistics* 29 (3): 466–78.

Wang, Yiru. 2019. “Tidy Tools for Supporting Fluent Workflow in Temporal
Data Analysis.” PhD thesis, Monash University.

Wickham, Hadley. 2014. “Tidy Data.” *Journal of Statistical Software*
59: 1–23.

Wickham, Hadley, Mara Averick, Jennifer Bryan, Winston Chang, Lucy
D’Agostino McGowan, Romain François, Garrett Grolemund, et al. 2019.
“Welcome to the tidyverse.” *Journal of Open Source Software* 4 (43):
1686. <https://doi.org/10.21105/joss.01686>.

Wickham, Hadley, Mine Çetinkaya-Rundel, and Garrett Grolemund. 2023. *R
for Data Science: Import, Tidy, Transform, Visualize, and Model Data*.
O’Reilly Media, Inc.

------------------------------------------------------------------------

1.  Optional attributes don’t exist unless they are set. Hence they are
    not accesible via the
    [`attributes()`](https://rdrr.io/r/base/attributes.html) function
    unless they exist.
