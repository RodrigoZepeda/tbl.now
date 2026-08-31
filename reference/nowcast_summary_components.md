# Individual blocks of a `tbl_now` summary

**\[experimental\]**

[summary()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_summary.md)
answers a dozen questions about a `tbl_now` at once. When you only want
one of them – for a report, a dashboard, or a check inside a script –
call that block directly instead of computing the rest and filtering it
away.

Every one of these returns the same schema as
[`summary()`](https://rdrr.io/r/base/summary.html) itself, so they can
be stacked with
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html),
compared across datasets, or used alone:

- `cases_per_date()` – case counts per date on one axis.

- `delay_summary()` – the case-weighted delay distribution.

- `zero_run_summary()` – lengths of the runs of consecutive zero dates.

- `prop_censored()` – proportion of cases flagged censored.

- `prop_confirmation_type()` – proportion of cases per confirmation
  outcome.

- `prop_strata()` – proportion of cases per stratum.

- `prop_covariate_levels()` – proportion of cases per level of each
  categorical covariate.

- `case_autocorrelation()` – lagged autocorrelation of the case series.

- `date_ranges()` – totals, date ranges and `now`.

- `triangle_occupancy()` – how full the reporting triangle is, and how
  stale the object is.

- `reporting_completeness()` – share of each event date's eventual total
  that had arrived by delay `d`.

- `cumulative_growth()` – ratio of one delay's running total to the
  previous one's.

## Usage

``` r
cases_per_date(
  x,
  axis = c("event", "report", "confirmation"),
  by_strata = NULL,
  strata = NULL
)

delay_summary(
  x,
  delay = c("event_to_report", "event_to_confirmation", "report_to_confirmation"),
  by_strata = NULL,
  strata = NULL
)

zero_run_summary(
  x,
  axis = c("event", "report", "confirmation"),
  by_strata = NULL,
  strata = NULL
)

prop_censored(x, by_strata = NULL, strata = NULL)

prop_confirmation_type(x, by_strata = NULL, strata = NULL)

prop_strata(x, strata = NULL)

prop_covariate_levels(x, by_strata = NULL, strata = NULL)

case_autocorrelation(
  x,
  lags = 1,
  axis = c("event", "report", "confirmation"),
  by_strata = NULL,
  strata = NULL
)

date_ranges(x, by_strata = NULL, strata = NULL)

triangle_occupancy(x, by_strata = NULL, strata = NULL)

reporting_completeness(
  x,
  delays = NULL,
  mature_only = TRUE,
  by_strata = NULL,
  strata = NULL
)

cumulative_growth(x, k = 7, by_strata = NULL, strata = NULL)
```

## Arguments

- x:

  A `tbl_now` object.

- axis:

  Which time axis to describe: `"event"`, `"report"` or
  `"confirmation"`.

- by_strata:

  Logical. Add one set of rows per stratum on top of the pooled
  (`"all"`) rows. Defaults to `TRUE` when the object has strata.

- strata:

  Character vector of columns to stratify by. Defaults to
  `get_strata(x)`.

- delay:

  Which delay to describe: `"event_to_report"` (the reporting delay),
  `"event_to_confirmation"` (the same span measured to the confirmation,
  so the two are comparable) or `"report_to_confirmation"` (the
  laboratory's turnaround, the `.confirmation_delay` column).

- lags:

  Integer vector of lags.

- delays:

  Integer vector of delays to report completeness at. Defaults to every
  observed delay.

- mature_only:

  Logical. Drop event dates too recent to have been fully reported. The
  cutoff is `now` minus the 95th percentile of the delay distribution –
  the same rule
  [`autoplot.tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/autoplot.tbl_now.md)
  uses.

- k:

  Number of delays for the growth ratios.

## Value

A tibble in the schema documented in
[tbl_now_summary](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_summary.md):
one row per quantity and stratum, with `component`, `quantity` and
`stratum` identifying the row and the remaining columns holding
whichever statistics apply.

## See also

[summary()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_summary.md),
which stacks all of these into one table and documents the schema;
[`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
for what is *wrong* with the data rather than what is in it;
[autoplot()](https://rodrigozepeda.github.io/tbl.now/reference/autoplot.tbl_now.md)
for the same information as pictures. The [*Describing and diagnosing a
tbl_now*
article](https://rodrigozepeda.github.io/tbl.now/articles/describing-and-diagnosing.html)
walks through them in order.

## Examples

``` r
data(denguedat)
ndata <- tbl_now(denguedat,
  event_date = "onset_week",
  report_date = "report_week",
  strata = "gender",
  verbose = FALSE
)

# How many cases per week of onset, and how long they took to be reported.
cases_per_date(ndata, axis = "event")
#> # A tibble: 3 × 16
#>   component quantity     stratum     n total  mean    sd   min   q25   q50   q75
#>   <chr>     <chr>        <chr>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 cases     per_event_d… all      1095 52987  48.4  53.3     0    14    30    64
#> 2 cases     per_event_d… Female   1095 26592  24.3  26.7     0     7    15    32
#> 3 cases     per_event_d… Male     1095 26395  24.1  27.0     0     7    15    31
#> # ℹ 5 more variables: q90 <dbl>, max <dbl>, prop_zero <dbl>, prop <dbl>,
#> #   value <dbl>
delay_summary(ndata)
#> # A tibble: 3 × 16
#>   component quantity     stratum     n total  mean    sd   min   q25   q50   q75
#>   <chr>     <chr>        <chr>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 delay     event_to_re… all      8265 52987  1.74  1.21     0     1     1     2
#> 2 delay     event_to_re… Female   4133 26592  1.74  1.20     0     1     1     2
#> 3 delay     event_to_re… Male     4132 26395  1.74  1.22     0     1     1     2
#> # ℹ 5 more variables: q90 <dbl>, max <dbl>, prop_zero <dbl>, prop <dbl>,
#> #   value <dbl>

# How sparse the series is, and how strongly one week predicts the next.
zero_run_summary(ndata, axis = "event")
#> # A tibble: 3 × 16
#>   component quantity   stratum     n total  mean    sd   min   q25   q50   q75
#>   <chr>     <chr>      <chr>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 zero_run  event_date all         2     4  2    1.41      1     1     1     3
#> 2 zero_run  event_date Female     10    13  1.3  0.675     1     1     1     1
#> 3 zero_run  event_date Male        8    13  1.62 0.916     1     1     1     2
#> # ℹ 5 more variables: q90 <dbl>, max <dbl>, prop_zero <dbl>, prop <dbl>,
#> #   value <dbl>
case_autocorrelation(ndata, lags = 1)
#> # A tibble: 3 × 16
#>   component     quantity stratum     n total  mean    sd   min   q25   q50   q75
#>   <chr>         <chr>    <chr>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 autocorrelat… per_eve… all      1094    NA    NA    NA    NA    NA    NA    NA
#> 2 autocorrelat… per_eve… Female   1094    NA    NA    NA    NA    NA    NA    NA
#> 3 autocorrelat… per_eve… Male     1094    NA    NA    NA    NA    NA    NA    NA
#> # ℹ 5 more variables: q90 <dbl>, max <dbl>, prop_zero <dbl>, prop <dbl>,
#> #   value <dbl>

# What the data is made of, and how far it reaches.
prop_strata(ndata)
#> # A tibble: 2 × 16
#>   component   quantity   stratum     n total  mean    sd   min   q25   q50   q75
#>   <chr>       <chr>      <chr>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 composition strata = … all      4133 26592    NA    NA    NA    NA    NA    NA
#> 2 composition strata = … all      4132 26395    NA    NA    NA    NA    NA    NA
#> # ℹ 5 more variables: q90 <dbl>, max <dbl>, prop_zero <dbl>, prop <dbl>,
#> #   value <dbl>
prop_censored(ndata)
#> # A tibble: 0 × 16
#> # ℹ 16 variables: component <chr>, quantity <chr>, stratum <chr>, n <int>,
#> #   total <dbl>, mean <dbl>, sd <dbl>, min <dbl>, q25 <dbl>, q50 <dbl>,
#> #   q75 <dbl>, q90 <dbl>, max <dbl>, prop_zero <dbl>, prop <dbl>, value <dbl>
date_ranges(ndata)
#> # A tibble: 11 × 18
#>    component quantity    stratum     n total  mean    sd   min   q25   q50   q75
#>    <chr>     <chr>       <chr>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 coverage  total_cases all      8265 52987    NA    NA    NA    NA    NA    NA
#>  2 coverage  event_date  all      1091 52987    NA    NA    NA    NA    NA    NA
#>  3 coverage  report_date all      1092 52987    NA    NA    NA    NA    NA    NA
#>  4 coverage  total_cases Female   4133 26592    NA    NA    NA    NA    NA    NA
#>  5 coverage  event_date  Female   1082 26592    NA    NA    NA    NA    NA    NA
#>  6 coverage  report_date Female   1078 26592    NA    NA    NA    NA    NA    NA
#>  7 coverage  total_cases Male     4132 26395    NA    NA    NA    NA    NA    NA
#>  8 coverage  event_date  Male     1082 26395    NA    NA    NA    NA    NA    NA
#>  9 coverage  report_date Male     1073 26395    NA    NA    NA    NA    NA    NA
#> 10 coverage  now         all        NA    NA    NA    NA    NA    NA    NA    NA
#> 11 coverage  unobserved… all         0    NA    NA    NA    NA    NA    NA    NA
#> # ℹ 7 more variables: q90 <dbl>, max <dbl>, prop_zero <dbl>, prop <dbl>,
#> #   value <dbl>, date_min <date>, date_max <date>
triangle_occupancy(ndata)
#> # A tibble: 18 × 16
#>    component quantity    stratum     n total  mean    sd   min   q25   q50   q75
#>    <chr>     <chr>       <chr>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 coverage  max_delay   all        NA    NA    NA    NA    NA    NA    NA    NA
#>  2 coverage  triangle_c… all      5154    NA    NA    NA    NA    NA    NA    NA
#>  3 coverage  triangle_c… all     29214    NA    NA    NA    NA    NA    NA    NA
#>  4 coverage  triangle_o… all        NA    NA    NA    NA    NA    NA    NA    NA
#>  5 coverage  now_gap_ev… all        NA    NA    NA    NA    NA    NA    NA    NA
#>  6 coverage  now_gap_re… all        NA    NA    NA    NA    NA    NA    NA    NA
#>  7 coverage  max_delay   Female     NA    NA    NA    NA    NA    NA    NA    NA
#>  8 coverage  triangle_c… Female   4133    NA    NA    NA    NA    NA    NA    NA
#>  9 coverage  triangle_c… Female  29214    NA    NA    NA    NA    NA    NA    NA
#> 10 coverage  triangle_o… Female     NA    NA    NA    NA    NA    NA    NA    NA
#> 11 coverage  now_gap_ev… Female     NA    NA    NA    NA    NA    NA    NA    NA
#> 12 coverage  now_gap_re… Female     NA    NA    NA    NA    NA    NA    NA    NA
#> 13 coverage  max_delay   Male       NA    NA    NA    NA    NA    NA    NA    NA
#> 14 coverage  triangle_c… Male     4132    NA    NA    NA    NA    NA    NA    NA
#> 15 coverage  triangle_c… Male    29214    NA    NA    NA    NA    NA    NA    NA
#> 16 coverage  triangle_o… Male       NA    NA    NA    NA    NA    NA    NA    NA
#> 17 coverage  now_gap_ev… Male       NA    NA    NA    NA    NA    NA    NA    NA
#> 18 coverage  now_gap_re… Male       NA    NA    NA    NA    NA    NA    NA    NA
#> # ℹ 5 more variables: q90 <dbl>, max <dbl>, prop_zero <dbl>, prop <dbl>,
#> #   value <dbl>

# The two that matter most for nowcasting: what share of a week's eventual
# total had arrived by delay d, and how fast the total is still growing.
reporting_completeness(ndata, delays = 0:3)
#> # A tibble: 12 × 16
#>    component    quantity   stratum     n total   mean     sd   min   q25    q50
#>    <chr>        <chr>      <chr>   <int> <dbl>  <dbl>  <dbl> <dbl> <dbl>  <dbl>
#>  1 completeness delay <= 0 all      1090  2099 0.0381 0.0533 0     0     0.0220
#>  2 completeness delay <= 1 all      1090 26595 0.510  0.175  0     0.410 0.510 
#>  3 completeness delay <= 2 all      1090 44988 0.844  0.130  0     0.781 0.867 
#>  4 completeness delay <= 3 all      1090 49837 0.931  0.0850 0.104 0.9   0.953 
#>  5 completeness delay <= 0 Female   1081  1039 0.0367 0.0670 0     0     0     
#>  6 completeness delay <= 1 Female   1081 13313 0.509  0.214  0     0.384 0.514 
#>  7 completeness delay <= 2 Female   1081 22582 0.849  0.152  0     0.783 0.879 
#>  8 completeness delay <= 3 Female   1081 25024 0.933  0.109  0     0.909 0.971 
#>  9 completeness delay <= 0 Male     1081  1060 0.0384 0.0717 0     0     0     
#> 10 completeness delay <= 1 Male     1081 13282 0.516  0.221  0     0.396 0.509 
#> 11 completeness delay <= 2 Male     1081 22406 0.839  0.160  0     0.769 0.867 
#> 12 completeness delay <= 3 Male     1081 24813 0.929  0.106  0     0.892 0.967 
#> # ℹ 6 more variables: q75 <dbl>, q90 <dbl>, max <dbl>, prop_zero <dbl>,
#> #   prop <dbl>, value <dbl>
cumulative_growth(ndata, k = 3)
#> # A tibble: 9 × 16
#>   component quantity stratum     n total  mean    sd   min   q25   q50   q75
#>   <chr>     <chr>    <chr>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 growth    delay 1  all       631  9752  9.93 9.16      1  5     7.6  12   
#> 2 growth    delay 2  all      1078  9282  1.93 1.30      1  1.36  1.6   2   
#> 3 growth    delay 3  all      1089  2607  1.16 0.336     1  1.02  1.09  1.19
#> 4 growth    delay 1  Female    471  8292 10.7  8.92      1  5     8    13   
#> 5 growth    delay 2  Female   1045  9219  1.95 1.57      1  1.33  1.61  2   
#> 6 growth    delay 3  Female   1076  2406  1.13 0.295     1  1     1.06  1.15
#> 7 growth    delay 1  Male      469  8251 10.5  9.95      1  5     8    12   
#> 8 growth    delay 2  Male     1040  9035  1.88 1.39      1  1.33  1.57  2   
#> 9 growth    delay 3  Male     1075  2379  1.14 0.357     1  1     1.06  1.17
#> # ℹ 5 more variables: q90 <dbl>, max <dbl>, prop_zero <dbl>, prop <dbl>,
#> #   value <dbl>

# Every block shares one schema, so they stack.
dplyr::bind_rows(
  date_ranges(ndata),
  delay_summary(ndata)
)
#> # A tibble: 14 × 18
#>    component quantity    stratum     n total  mean    sd   min   q25   q50   q75
#>    <chr>     <chr>       <chr>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 coverage  total_cases all      8265 52987 NA    NA       NA    NA    NA    NA
#>  2 coverage  event_date  all      1091 52987 NA    NA       NA    NA    NA    NA
#>  3 coverage  report_date all      1092 52987 NA    NA       NA    NA    NA    NA
#>  4 coverage  total_cases Female   4133 26592 NA    NA       NA    NA    NA    NA
#>  5 coverage  event_date  Female   1082 26592 NA    NA       NA    NA    NA    NA
#>  6 coverage  report_date Female   1078 26592 NA    NA       NA    NA    NA    NA
#>  7 coverage  total_cases Male     4132 26395 NA    NA       NA    NA    NA    NA
#>  8 coverage  event_date  Male     1082 26395 NA    NA       NA    NA    NA    NA
#>  9 coverage  report_date Male     1073 26395 NA    NA       NA    NA    NA    NA
#> 10 coverage  now         all        NA    NA NA    NA       NA    NA    NA    NA
#> 11 coverage  unobserved… all         0    NA NA    NA       NA    NA    NA    NA
#> 12 delay     event_to_r… all      8265 52987  1.74  1.21     0     1     1     2
#> 13 delay     event_to_r… Female   4133 26592  1.74  1.20     0     1     1     2
#> 14 delay     event_to_r… Male     4132 26395  1.74  1.22     0     1     1     2
#> # ℹ 7 more variables: q90 <dbl>, max <dbl>, prop_zero <dbl>, prop <dbl>,
#> #   value <dbl>, date_min <date>, date_max <date>
```
