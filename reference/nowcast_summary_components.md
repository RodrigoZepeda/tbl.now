# Individual blocks of a `tbl_now` summary

**\[stable\]**

[summary()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_summary.md)
answers a dozen questions about a `tbl_now` at once. When you only want
one of them – for a report, a dashboard, or a check inside a script –
call that block directly instead of computing the rest and filtering it
away.

Every one of these returns the same schema as
[`summary()`](https://rdrr.io/r/base/summary.html) itself, so they can
be stacked with
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html),
compared across datasets, or used alone.

- `cases_per_date()` – case counts per date on one axis.

- `delay_summary()` – the case-weighted delay distribution.

- `zero_run_summary()` – lengths of the runs of consecutive zero dates.

- `prop_censored()` – proportion of cases flagged censored.

- `prop_revision_type()` – proportion of cases per revision outcome.

- `prop_strata()` – proportion of cases per stratum.

- `prop_covariate_levels()` – proportion of cases per level of each
  categorical covariate.

- `date_ranges()` – totals, date ranges and `now`.

- `triangle_occupancy()` – how full the reporting triangle is, and how
  stale the object is.

- `cumulative_growth()` – ratio of one delay's running total to the
  previous one's.

## Usage

``` r
cases_per_date(
  x,
  axis = c("event", "report", "revision"),
  by_strata = NULL,
  strata = NULL
)

delay_summary(
  x,
  delay = c("event_to_report", "event_to_revision", "report_to_revision"),
  by_strata = NULL,
  strata = NULL
)

zero_run_summary(
  x,
  axis = c("event", "report", "revision"),
  by_strata = NULL,
  strata = NULL
)

prop_censored(x, by_strata = NULL, strata = NULL)

prop_revision_type(x, by_strata = NULL, strata = NULL)

prop_strata(x, strata = NULL)

prop_covariate_levels(x, by_strata = NULL, strata = NULL)

date_ranges(x, by_strata = NULL, strata = NULL)

triangle_occupancy(x, by_strata = NULL, strata = NULL)

cumulative_growth(x, k = 7, by_strata = NULL, strata = NULL)
```

## Arguments

- x:

  A `tbl_now` object.

- axis:

  Which time axis to describe: `"event"`, `"report"` or `"revision"`.

- by_strata:

  Logical. Add one set of rows per stratum on top of the pooled
  (`"all"`) rows. Defaults to `TRUE` when the object has strata.

- strata:

  Character vector of columns to stratify by. Defaults to
  `get_strata(x)`.

- delay:

  Which delay to describe: `"event_to_report"` (the reporting delay),
  `"event_to_revision"` (the same span measured to the revision, so the
  two are comparable) or `"report_to_revision"` (the laboratory's
  turnaround, the `.revision_delay` column).

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
for the same information as pictures. The [*Diagnosing a tbl_now*
article](https://rodrigozepeda.github.io/tbl.now/articles/diagnosing-a-tbl-now.html)
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
#> ── Summary of a <tbl_now> ──────────────────────────────────────────────────────
#> 3 rows in 1 component; strata: "Female" and "Male".
#> 
#> cases
#>   n = dates on the grid; total = cases
#>   quantity   stratum     n total  mean    sd   min   q25   q50   q75   q90   max
#>   <chr>      <chr>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 per_event… all      1095 52987  48.4  53.3     0    14    30    64   104   358
#> 2 per_event… Female   1095 26592  24.3  26.7     0     7    15    32    52   189
#> 3 per_event… Male     1095 26395  24.1  27.0     0     7    15    31    53   176
#> # ℹ 1 more variable: prop_zero <dbl>
#> 
#> ℹ Use `dplyr::filter()` or `tibble::as_tibble()` for the full schema.
delay_summary(ndata)
#> ── Summary of a <tbl_now> ──────────────────────────────────────────────────────
#> 3 rows in 1 component; strata: "Female" and "Male".
#> 
#> delay
#>   n = (event, report) cells; total = cases
#>   quantity   stratum     n total  mean    sd   min   q25   q50   q75   q90   max
#>   <chr>      <chr>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 event_to_… all      8265 52987  1.74  1.21     0     1     1     2     3    26
#> 2 event_to_… Female   4133 26592  1.74  1.20     0     1     1     2     3    15
#> 3 event_to_… Male     4132 26395  1.74  1.22     0     1     1     2     3    26
#> 
#> ℹ Use `dplyr::filter()` or `tibble::as_tibble()` for the full schema.

# How sparse the series is.
zero_run_summary(ndata, axis = "event")
#> ── Summary of a <tbl_now> ──────────────────────────────────────────────────────
#> 3 rows in 1 component; strata: "Female" and "Male".
#> 
#> zero_run
#>   n = runs of consecutive zero dates; total = zero dates in those runs
#>   quantity   stratum     n total  mean    sd   min   q25   q50   q75   q90   max
#>   <chr>      <chr>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 event_date all         2     4  2    1.41      1     1     1     3     3     3
#> 2 event_date Female     10    13  1.3  0.675     1     1     1     1     2     3
#> 3 event_date Male        8    13  1.62 0.916     1     1     1     2     3     3
#> 
#> ℹ Use `dplyr::filter()` or `tibble::as_tibble()` for the full schema.

# What the data is made of, and how far it reaches.
prop_strata(ndata)
#> ── Summary of a <tbl_now> ──────────────────────────────────────────────────────
#> 2 rows in 1 component.
#> 
#> composition
#>   n = (event, report) cells in the category; total = cases in the category
#>   quantity            n total  prop
#>   <chr>           <int> <dbl> <dbl>
#> 1 strata = Female  4133 26592 0.502
#> 2 strata = Male    4132 26395 0.498
#> 
#> ℹ Use `dplyr::filter()` or `tibble::as_tibble()` for the full schema.
prop_censored(ndata)
#> ── Summary of a <tbl_now> ──────────────────────────────────────────────────────
#> Nothing to summarise.
date_ranges(ndata)
#> ── Summary of a <tbl_now> ──────────────────────────────────────────────────────
#> 11 rows in 1 component; strata: "Female" and "Male".
#> 
#> coverage
#>   n = cells, or distinct dates on a date row; total = cases
#>    quantity    stratum     n total date_min   date_max  
#>    <chr>       <chr>   <int> <dbl> <date>     <date>    
#>  1 total_cases all      8265 52987 NA         NA        
#>  2 event_date  all      1091 52987 1990-01-01 2010-11-29
#>  3 report_date all      1092 52987 1990-01-01 2010-12-20
#>  4 total_cases Female   4133 26592 NA         NA        
#>  5 event_date  Female   1082 26592 1990-01-01 2010-11-29
#>  6 report_date Female   1078 26592 1990-01-01 2010-12-20
#>  7 total_cases Male     4132 26395 NA         NA        
#>  8 event_date  Male     1082 26395 1990-01-01 2010-11-29
#>  9 report_date Male     1073 26395 1990-01-01 2010-12-13
#> 10 now         all        NA    NA 2010-12-20 2010-12-20
#> ℹ 1 more row.
#> 
#> ℹ Use `dplyr::filter()` or `tibble::as_tibble()` for the full schema.
triangle_occupancy(ndata)
#> ── Summary of a <tbl_now> ──────────────────────────────────────────────────────
#> 18 rows in 1 component; strata: "Female" and "Male".
#> 
#> coverage
#>   n = cells, or distinct dates on a date row
#>    quantity                stratum     n  value
#>    <chr>                   <chr>   <int>  <dbl>
#>  1 max_delay               all        NA 26    
#>  2 triangle_cells_observed all      5154 NA    
#>  3 triangle_cells_possible all     29214 NA    
#>  4 triangle_occupancy      all        NA  0.176
#>  5 now_gap_event           all        NA  3    
#>  6 now_gap_report          all        NA  0    
#>  7 max_delay               Female     NA 15    
#>  8 triangle_cells_observed Female   4133 NA    
#>  9 triangle_cells_possible Female  29214 NA    
#> 10 triangle_occupancy      Female     NA  0.141
#> ℹ 8 more rows.
#> 
#> ℹ Use `dplyr::filter()` or `tibble::as_tibble()` for the full schema.

# How fast the running total is still growing. This is a distribution over
# event dates, so it fills `mean`/`q50` rather than the scalar `value`.
cumulative_growth(ndata, k = 3)
#> ── Summary of a <tbl_now> ──────────────────────────────────────────────────────
#> 9 rows in 1 component; strata: "Female" and "Male".
#> 
#> growth
#>   n = event dates; total = cases added
#>   quantity stratum     n total  mean    sd   min   q25   q50   q75   q90   max
#>   <chr>    <chr>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 delay 1  all       631  9752  9.93 9.16      1  5     7.6  12    18    104  
#> 2 delay 2  all      1078  9282  1.93 1.30      1  1.36  1.6   2     2.83  18  
#> 3 delay 3  all      1089  2607  1.16 0.336     1  1.02  1.09  1.19  1.33   8  
#> 4 delay 1  Female    471  8292 10.7  8.92      1  5     8    13    20     71  
#> 5 delay 2  Female   1045  9219  1.95 1.57      1  1.33  1.61  2     3     31  
#> 6 delay 3  Female   1076  2406  1.13 0.295     1  1     1.06  1.15  1.31   7.5
#> 7 delay 1  Male      469  8251 10.5  9.95      1  5     8    12    19    104  
#> 8 delay 2  Male     1040  9035  1.88 1.39      1  1.33  1.57  2     2.56  27  
#> 9 delay 3  Male     1075  2379  1.14 0.357     1  1     1.06  1.17  1.33   8  
#> 
#> ℹ Use `dplyr::filter()` or `tibble::as_tibble()` for the full schema.

# Every block shares one schema, so they stack.
dplyr::bind_rows(
  date_ranges(ndata),
  delay_summary(ndata)
)
#> ── Summary of a <tbl_now> ──────────────────────────────────────────────────────
#> 14 rows in 2 components; strata: "Female" and "Male".
#> 
#> coverage
#>   n = cells, or distinct dates on a date row; total = cases
#>    quantity    stratum     n total date_min   date_max  
#>    <chr>       <chr>   <int> <dbl> <date>     <date>    
#>  1 total_cases all      8265 52987 NA         NA        
#>  2 event_date  all      1091 52987 1990-01-01 2010-11-29
#>  3 report_date all      1092 52987 1990-01-01 2010-12-20
#>  4 total_cases Female   4133 26592 NA         NA        
#>  5 event_date  Female   1082 26592 1990-01-01 2010-11-29
#>  6 report_date Female   1078 26592 1990-01-01 2010-12-20
#>  7 total_cases Male     4132 26395 NA         NA        
#>  8 event_date  Male     1082 26395 1990-01-01 2010-11-29
#>  9 report_date Male     1073 26395 1990-01-01 2010-12-13
#> 10 now         all        NA    NA 2010-12-20 2010-12-20
#> ℹ 1 more row.
#> 
#> delay
#>   n = (event, report) cells; total = cases
#>   quantity   stratum     n total  mean    sd   min   q25   q50   q75   q90   max
#>   <chr>      <chr>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 event_to_… all      8265 52987  1.74  1.21     0     1     1     2     3    26
#> 2 event_to_… Female   4133 26592  1.74  1.20     0     1     1     2     3    15
#> 3 event_to_… Male     4132 26395  1.74  1.22     0     1     1     2     3    26
#> 
#> ℹ Use `dplyr::filter()` or `tibble::as_tibble()` for the full schema.
```
