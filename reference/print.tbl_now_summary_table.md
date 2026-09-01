# Print a `tbl_now` summary

**\[experimental\]**

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
ndata <- tbl_now(denguedat,
  event_date = "onset_week", report_date = "report_week",
  strata = "gender", verbose = FALSE
)

summary(ndata)
#> ── Summary of a <tbl_now> ──────────────────────────────────────────────────────
#> 76 rows in 7 components; strata: "Female" and "Male".
#> 
#> cases
#>   quantity   stratum     n total  mean    sd   min   q25   q50   q75   q90   max
#>   <chr>      <chr>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 per_event… all      1095 52987  48.4  53.3     0    14    30    64   104   358
#> 2 per_event… Female   1095 26592  24.3  26.7     0     7    15    32    52   189
#> 3 per_event… Male     1095 26395  24.1  27.0     0     7    15    31    53   176
#> 4 per_repor… all      1095 52987  48.4  54.3     0    14    29    64   111   420
#> 5 per_repor… Female   1095 26592  24.3  27.3     0     7    15    32    57   217
#> 6 per_repor… Male     1095 26395  24.1  27.5     0     7    15    32    54   203
#> # ℹ 1 more variable: prop_zero <dbl>
#> 
#> zero_run
#>   quantity   stratum     n total  mean    sd   min   q25   q50   q75   q90   max
#>   <chr>      <chr>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 event_date all         2     4  2    1.41      1     1     1     3     3     3
#> 2 event_date Female     10    13  1.3  0.675     1     1     1     1     2     3
#> 3 event_date Male        8    13  1.62 0.916     1     1     1     2     3     3
#> 4 report_da… all         3     3  1    0         1     1     1     1     1     1
#> 5 report_da… Female     15    17  1.13 0.352     1     1     1     1     2     2
#> 6 report_da… Male       19    22  1.16 0.501     1     1     1     1     2     3
#> 
#> autocorrelation
#>   quantity              stratum     n value
#>   <chr>                 <chr>   <int> <dbl>
#> 1 per_event_date lag 1  all      1094 0.958
#> 2 per_event_date lag 1  Female   1094 0.944
#> 3 per_event_date lag 1  Male     1094 0.941
#> 4 per_report_date lag 1 all      1094 0.885
#> 5 per_report_date lag 1 Female   1094 0.867
#> 6 per_report_date lag 1 Male     1094 0.878
#> 
#> composition
#>   quantity            n total  prop
#>   <chr>           <int> <dbl> <dbl>
#> 1 strata = Female  4133 26592 0.502
#> 2 strata = Male    4132 26395 0.498
#> 
#> coverage
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
#> ℹ 19 more rows.
#> 
#> completeness
#>    quantity   stratum     n total   mean     sd   min   q25    q50    q75   q90
#>    <chr>      <chr>   <int> <dbl>  <dbl>  <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl>
#>  1 delay <= 0 all      1090  2099 0.0381 0.0533 0     0     0.0220 0.0594 0.1  
#>  2 delay <= 1 all      1090 26595 0.510  0.175  0     0.410 0.510  0.618  0.710
#>  3 delay <= 2 all      1090 44988 0.844  0.130  0     0.781 0.867  0.930  1    
#>  4 delay <= 3 all      1090 49837 0.931  0.0850 0.104 0.9   0.953  1      1    
#>  5 delay <= 4 all      1090 51451 0.963  0.0597 0.5   0.949 0.984  1      1    
#>  6 delay <= 5 all      1090 52126 0.978  0.0449 0.5   0.972 1      1      1    
#>  7 delay <= 6 all      1090 52505 0.988  0.0330 0.5   0.990 1      1      1    
#>  8 delay <= 7 all      1090 52668 0.992  0.0275 0.5   1     1      1      1    
#>  9 delay <= 0 Female   1081  1039 0.0367 0.0670 0     0     0      0.0556 0.111
#> 10 delay <= 1 Female   1081 13313 0.509  0.214  0     0.384 0.514  0.635  0.75 
#> # ℹ 2 more variables: max <dbl>, prop <dbl>
#> ℹ 14 more rows.
#> 
#> delay
#>   quantity   stratum     n total  mean    sd   min   q25   q50   q75   q90   max
#>   <chr>      <chr>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 event_to_… all      8265 52987  1.74  1.21     0     1     1     2     3    26
#> 2 event_to_… Female   4133 26592  1.74  1.20     0     1     1     2     3    15
#> 3 event_to_… Male     4132 26395  1.74  1.22     0     1     1     2     3    26
#> 
#> ℹ Use `dplyr::filter()` or `tibble::as_tibble()` for the full schema.

# One block on its own prints the same way.
delay_summary(ndata)
#> ── Summary of a <tbl_now> ──────────────────────────────────────────────────────
#> 3 rows in 1 component; strata: "Female" and "Male".
#> 
#> delay
#>   quantity   stratum     n total  mean    sd   min   q25   q50   q75   q90   max
#>   <chr>      <chr>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 event_to_… all      8265 52987  1.74  1.21     0     1     1     2     3    26
#> 2 event_to_… Female   4133 26592  1.74  1.20     0     1     1     2     3    15
#> 3 event_to_… Male     4132 26395  1.74  1.22     0     1     1     2     3    26
#> 
#> ℹ Use `dplyr::filter()` or `tibble::as_tibble()` for the full schema.

# Still a tibble.
print(tibble::as_tibble(summary(ndata)))
#> # A tibble: 76 × 18
#>    component quantity   stratum     n total  mean     sd   min   q25   q50   q75
#>    <chr>     <chr>      <chr>   <int> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 cases     per_event… all      1095 52987 48.4  53.3       0    14    30    64
#>  2 cases     per_event… Female   1095 26592 24.3  26.7       0     7    15    32
#>  3 cases     per_event… Male     1095 26395 24.1  27.0       0     7    15    31
#>  4 cases     per_repor… all      1095 52987 48.4  54.3       0    14    29    64
#>  5 cases     per_repor… Female   1095 26592 24.3  27.3       0     7    15    32
#>  6 cases     per_repor… Male     1095 26395 24.1  27.5       0     7    15    32
#>  7 zero_run  event_date all         2     4  2     1.41      1     1     1     3
#>  8 zero_run  event_date Female     10    13  1.3   0.675     1     1     1     1
#>  9 zero_run  event_date Male        8    13  1.62  0.916     1     1     1     2
#> 10 zero_run  report_da… all         3     3  1     0         1     1     1     1
#> # ℹ 66 more rows
#> # ℹ 7 more variables: q90 <dbl>, max <dbl>, prop_zero <dbl>, prop <dbl>,
#> #   value <dbl>, date_min <date>, date_max <date>
```
