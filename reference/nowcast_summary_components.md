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
# The last five years. The full twenty-year series gives the same shape
# of answer, it just takes longer to compute.
recent <- denguedat[denguedat$onset_week >= as.Date("2006-01-01"), ]
ndata <- tbl_now(recent,
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
#> 1 per_event… all       260 14135  54.4  73.2     0    11    25    65   139   358
#> 2 per_event… Female    260  6998  26.9  36.4     0     6    12    31    71   189
#> 3 per_event… Male      260  7137  27.4  37.2     0     5    13    32    71   176
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
#> 1 event_to_… all      1673 14135  1.81  1.06     0     1     2     2     3    26
#> 2 event_to_… Female    842  6998  1.82  1.07     0     1     2     2     3    15
#> 3 event_to_… Male      831  7137  1.80  1.06     0     1     2     2     3    26
#> 
#> ℹ Use `dplyr::filter()` or `tibble::as_tibble()` for the full schema.

# How sparse the series is.
zero_run_summary(ndata, axis = "event")
#> ── Summary of a <tbl_now> ──────────────────────────────────────────────────────
#> 3 rows in 1 component; strata: "Female" and "Male".
#> 
#> zero_run
#>   n = runs of consecutive zero dates; total = zero dates in those runs
#>   quantity  stratum     n total  mean     sd   min   q25   q50   q75   q90   max
#>   <chr>     <chr>   <int> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 event_da… all         1     3   3   NA         3     3     3     3     3     3
#> 2 event_da… Female      5     8   1.6  0.894     1     1     1     2     3     3
#> 3 event_da… Male        5     7   1.4  0.894     1     1     1     1     3     3
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
#> 1 strata = Female   842  6998 0.495
#> 2 strata = Male     831  7137 0.505
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
#>  1 total_cases all      1673 14135 NA         NA        
#>  2 event_date  all       257 14135 2006-01-02 2010-11-29
#>  3 report_date all       257 14135 2006-01-09 2010-12-20
#>  4 total_cases Female    842  6998 NA         NA        
#>  5 event_date  Female    252  6998 2006-01-02 2010-11-29
#>  6 report_date Female    250  6998 2006-01-09 2010-12-20
#>  7 total_cases Male      831  7137 NA         NA        
#>  8 event_date  Male      253  7137 2006-01-02 2010-11-29
#>  9 report_date Male      251  7137 2006-01-09 2010-12-13
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
#>  2 triangle_cells_observed all      1034 NA    
#>  3 triangle_cells_possible all      6669 NA    
#>  4 triangle_occupancy      all        NA  0.155
#>  5 now_gap_event           all        NA  3    
#>  6 now_gap_report          all        NA  0    
#>  7 max_delay               Female     NA 15    
#>  8 triangle_cells_observed Female    842 NA    
#>  9 triangle_cells_possible Female   6669 NA    
#> 10 triangle_occupancy      Female     NA  0.126
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
#>   quantity stratum     n total  mean     sd   min   q25   q50   q75   q90   max
#>   <chr>    <chr>   <int> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 delay 1  all       107  2067 13.5  15.7       1  5.5   9    15    31.3  104  
#> 2 delay 2  all       249  2908  2.45  2.24      1  1.4   1.71  2.5   4     18  
#> 3 delay 3  all       255   859  1.21  0.616     1  1     1.10  1.2   1.38   8  
#> 4 delay 1  Female     68  1530 14.2  14.6       1  5     9    19    30     71  
#> 5 delay 2  Female    238  2812  2.51  2.90      1  1.33  1.67  2.46  4.5   31  
#> 6 delay 3  Female    249   785  1.17  0.522     1  1     1.05  1.16  1.4    7.5
#> 7 delay 1  Male       77  1801 15.4  17.8       1  6    10    15    33    104  
#> 8 delay 2  Male      236  2826  2.37  2.50      1  1.29  1.65  2.35  4     27  
#> 9 delay 3  Male      251   825  1.20  0.621     1  1     1.06  1.2   1.4    8  
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
#>  1 total_cases all      1673 14135 NA         NA        
#>  2 event_date  all       257 14135 2006-01-02 2010-11-29
#>  3 report_date all       257 14135 2006-01-09 2010-12-20
#>  4 total_cases Female    842  6998 NA         NA        
#>  5 event_date  Female    252  6998 2006-01-02 2010-11-29
#>  6 report_date Female    250  6998 2006-01-09 2010-12-20
#>  7 total_cases Male      831  7137 NA         NA        
#>  8 event_date  Male      253  7137 2006-01-02 2010-11-29
#>  9 report_date Male      251  7137 2006-01-09 2010-12-13
#> 10 now         all        NA    NA 2010-12-20 2010-12-20
#> ℹ 1 more row.
#> 
#> delay
#>   n = (event, report) cells; total = cases
#>   quantity   stratum     n total  mean    sd   min   q25   q50   q75   q90   max
#>   <chr>      <chr>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 event_to_… all      1673 14135  1.81  1.06     0     1     2     2     3    26
#> 2 event_to_… Female    842  6998  1.82  1.07     0     1     2     2     3    15
#> 3 event_to_… Male      831  7137  1.80  1.06     0     1     2     2     3    26
#> 
#> ℹ Use `dplyr::filter()` or `tibble::as_tibble()` for the full schema.
```
