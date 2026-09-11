# Print a `tbl_now` summary

**\[stable\]**

Prints the table
[summary()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_summary.md)
returned one **component** at a time, dropping the columns that
component does not populate. The full schema is wide because it has to
hold every block's statistics at once; no single block fills more than a
handful of them, and a table that is mostly `NA` is hard to read for a
reason that has nothing to do with the data.

The object is an ordinary tibble underneath, so
`print(tibble::as_tibble(x))` gives the whole schema back and every
`dplyr` verb still works on it.

## Usage

``` r
# S3 method for class 'tbl_now_summary_table'
print(x, ..., n = 10)
```

## Arguments

- x:

  A summary tibble, from
  [summary()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_summary.md)
  or one of the
  [nowcast_summary_components](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md).

- ...:

  Unused.

- n:

  Maximum number of rows to show per component. `Inf` shows all of them.

## Value

`x`, invisibly.

## See also

[summary()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_summary.md),
[nowcast_summary_components](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md)

## Examples

``` r
data(denguedat)
# The last five years. The full twenty-year series gives the same shape
# of answer, it just takes longer to compute.
recent <- denguedat[denguedat$onset_week >= as.Date("2006-01-01"), ]
ndata <- tbl_now(recent,
  event_date = "onset_week", report_date = "report_week",
  strata = "gender", verbose = FALSE
)

summary(ndata)
#> ── Summary of a <tbl_now> ──────────────────────────────────────────────────────
#> 46 rows in 5 components; strata: "Female" and "Male".
#> 
#> cases
#>   n = dates on the grid; total = cases
#>   quantity   stratum     n total  mean    sd   min   q25   q50   q75   q90   max
#>   <chr>      <chr>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 per_event… all       260 14135  54.4  73.2     0    11    25    65   139   358
#> 2 per_event… Female    260  6998  26.9  36.4     0     6    12    31    71   189
#> 3 per_event… Male      260  7137  27.4  37.2     0     5    13    32    71   176
#> 4 per_repor… all       259 14135  54.6  75.0     0    10    25    65   142   420
#> 5 per_repor… Female    259  6998  27.0  37.3     0     5    12    33    73   217
#> 6 per_repor… Male      259  7137  27.6  38.0     0     6    14    33    70   203
#> # ℹ 1 more variable: prop_zero <dbl>
#> 
#> zero_run
#>   n = runs of consecutive zero dates; total = zero dates in those runs
#>   quantity  stratum     n total  mean     sd   min   q25   q50   q75   q90   max
#>   <chr>     <chr>   <int> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 event_da… all         1     3  3    NA         3     3     3     3     3     3
#> 2 event_da… Female      5     8  1.6   0.894     1     1     1     2     3     3
#> 3 event_da… Male        5     7  1.4   0.894     1     1     1     1     3     3
#> 4 report_d… all         2     2  1     0         1     1     1     1     1     1
#> 5 report_d… Female      8     9  1.12  0.354     1     1     1     1     2     2
#> 6 report_d… Male        7     8  1.14  0.378     1     1     1     1     2     2
#> 
#> composition
#>   n = (event, report) cells in the category; total = cases in the category
#>   quantity            n total  prop
#>   <chr>           <int> <dbl> <dbl>
#> 1 strata = Female   842  6998 0.495
#> 2 strata = Male     831  7137 0.505
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
#> ℹ 19 more rows.
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

# One block on its own prints the same way.
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

# Still a tibble.
print(tibble::as_tibble(summary(ndata)))
#> # A tibble: 46 × 18
#>    component quantity   stratum     n total  mean     sd   min   q25   q50   q75
#>    <chr>     <chr>      <chr>   <int> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 cases     per_event… all       260 14135  54.4 73.2       0    11    25    65
#>  2 cases     per_event… Female    260  6998  26.9 36.4       0     6    12    31
#>  3 cases     per_event… Male      260  7137  27.4 37.2       0     5    13    32
#>  4 cases     per_repor… all       259 14135  54.6 75.0       0    10    25    65
#>  5 cases     per_repor… Female    259  6998  27.0 37.3       0     5    12    33
#>  6 cases     per_repor… Male      259  7137  27.6 38.0       0     6    14    33
#>  7 zero_run  event_date all         1     3   3   NA         3     3     3     3
#>  8 zero_run  event_date Female      5     8   1.6  0.894     1     1     1     2
#>  9 zero_run  event_date Male        5     7   1.4  0.894     1     1     1     1
#> 10 zero_run  report_da… all         2     2   1    0         1     1     1     1
#> # ℹ 36 more rows
#> # ℹ 7 more variables: q90 <dbl>, max <dbl>, prop_zero <dbl>, prop <dbl>,
#> #   value <dbl>, date_min <date>, date_max <date>
```
