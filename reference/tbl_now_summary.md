# Summarise a `tbl_now`

**\[experimental\]**

[`summary()`](https://rdrr.io/r/base/summary.html) describes a `tbl_now`
the way a nowcaster needs it described: how many cases arrive on each of
the object's time axes, how long they take to get there, how sparse the
series is, what fraction of the data is censored or still pending, and
how far the object reaches.

Every block of the summary is also available on its own – see
[nowcast_summary_components](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md)
– and [`summary()`](https://rdrr.io/r/base/summary.html) is exactly the
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
of those pieces.

## Usage

``` r
# S3 method for class 'tbl_now'
summary(
  object,
  ...,
  by_strata = NULL,
  strata = NULL,
  lags = 1,
  completeness_delays = NULL,
  growth_k = 7,
  mature_only = TRUE
)
```

## Arguments

- object:

  A `tbl_now` object.

- ...:

  Unused, for compatibility with the
  [`summary()`](https://rdrr.io/r/base/summary.html) generic.

- by_strata:

  Logical. Add one set of rows per stratum on top of the pooled
  (`"all"`) rows. Defaults to `TRUE` when the object has strata.

- strata:

  Character vector of columns to stratify by. Defaults to
  `get_strata(object)`.

- lags:

  Integer vector of lags for the autocorrelation rows.

- completeness_delays:

  Integer vector of delays for the reporting completeness rows. Defaults
  to `0:7`, trimmed to the observed delays.

- growth_k:

  Number of delays for the cumulative growth rows.

- mature_only:

  Logical. Restrict the completeness rows to event dates old enough to
  have been fully reported (see
  [`reporting_completeness()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md)).

## Value

A tibble with the columns described above.

## Details

**The date grids.** "Cases per event date" is a statement about a
*calendar*, not about the rows present in the data, so each axis is
completed to a full grid running from the earliest observed date on that
axis to
[`get_now()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md),
stepping by that axis's units. Dates with no rows count as zeros. This
is what makes `prop_zero` and the zero-run lengths meaningful, and it is
why a line list – which cannot represent a zero – is summarised
correctly here.

**Not-yet-observed cells are dropped.** An `NA` count means the cell has
not been observed yet, unlike a `0`, which was observed and was zero.
Such rows carry no cases, so they are excluded rather than allowed to
turn every total they touch into `NA`. How many were dropped is reported
as the `"unobserved_cells"` coverage row.

The grid is **global**: when `by_strata = TRUE` every stratum is
summarised on the same grid, so a stratum whose cases start late
genuinely shows the leading zeros. Otherwise the strata would not be
comparable.

**Count-cumulative data gets no delay rows.** A cumulative total is not
additive across delays, so a case-weighted delay distribution would be
meaningless. The `"growth"` rows take their place, describing how each
event date's total grows from one delay to the next. Call
`to_count(x, to = "count-incidence")` first if you want the delay
distribution – and note that de-accumulating can produce negative
increments.

## Note

**Quantiles are inverse-ECDF (type 1), not
[`stats::quantile()`](https://rdrr.io/r/stats/quantile.html)'s
default.** `q50` is the smallest value whose cumulative weight reaches
`0.5`, which for an even number of observations is the upper of the two
middle values rather than their average. This is deliberate: it is the
same estimator
[`autoplot.tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/autoplot.tbl_now.md)
and
[`diagnose_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_drift.md)
use for the delay quantiles they draw, so the numbers in this table
match the numbers in the plots. It also always returns a value that was
actually observed, which a half-case delay is not.

## The columns

Every function in this family returns the same schema, so results can be
stacked with
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
and filtered with
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html).

- `component`:

  Which block the row belongs to: `"cases"`, `"delay"`, `"zero_run"`,
  `"composition"`, `"autocorrelation"`, `"completeness"`, `"growth"` or
  `"coverage"`.

- `quantity`:

  What the row describes, including the category for the compositional
  rows (`"validation_type = confirmed"`).

- `stratum`:

  Which subset of the data the row describes: `"all"` for the pooled
  rows, or the stratum label otherwise.

- `n`:

  Number of observations behind the row – dates for `"cases"`, runs for
  `"zero_run"`, data rows for `"delay"` and `"composition"`.

- `total`:

  Number of **cases** behind the row.

- `mean`, `sd`:

  Mean and standard deviation. For the case-weighted rows these are the
  weighted versions, equal to what you would get by expanding the counts
  to one row per case.

- `min`, `q25`, `q50`, `q75`, `q90`, `max`:

  Quantiles. See the note below on which estimator is used.

- `prop_zero`:

  Proportion of dates on the grid that are exactly zero.

- `prop`:

  Proportion of cases in this category (compositional rows), or the
  pooled share that had arrived by delay `d` (`"completeness"` rows).

- `value`:

  A single scalar that is not a distribution: an autocorrelation, a gap,
  an occupancy. The `"completeness"` and `"growth"` rows are
  distributions over event dates, so they populate `mean`/`sd`/the
  quantiles (and `prop`) instead and leave `value` empty.

- `date_min`, `date_max`:

  Date range. Present only when the result contains `"coverage"` rows.

- `unobserved_cells`:

  A `"coverage"` row counting the `NA`-count rows excluded as not yet
  observed.

## See also

[nowcast_summary_components](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md)
for the individual blocks.

## Examples

``` r
data(denguedat)
ndata <- tbl_now(denguedat,
  event_date = "onset_week",
  report_date = "report_week",
  strata = "gender",
  verbose = FALSE
)

# The whole summary: one row per quantity, per stratum.
overview <- summary(ndata)
overview
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

# It is an ordinary tibble, so pick out the block you want.
overview |> dplyr::filter(component == "delay")
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

# How much of each week's eventual total had arrived by delay d? This is the
# reporting-delay problem, in one table. Completeness is a distribution over
# event dates, so it lives in `mean`/`q50` (the typical event date) and
# `prop` (the pooled share), not in the scalar `value` column.
overview |>
  dplyr::filter(component == "completeness", stratum == "all") |>
  dplyr::select(quantity, n, mean, q50, prop)
#> # A tibble: 8 × 5
#>   quantity       n   mean    q50   prop
#>   <chr>      <int>  <dbl>  <dbl>  <dbl>
#> 1 delay <= 0  1090 0.0381 0.0220 0.0396
#> 2 delay <= 1  1090 0.510  0.510  0.502 
#> 3 delay <= 2  1090 0.844  0.867  0.850 
#> 4 delay <= 3  1090 0.931  0.953  0.941 
#> 5 delay <= 4  1090 0.963  0.984  0.972 
#> 6 delay <= 5  1090 0.978  1      0.984 
#> 7 delay <= 6  1090 0.988  1      0.992 
#> 8 delay <= 7  1090 0.992  1      0.995 

# Pooled rows only, ignoring the strata.
summary(ndata, by_strata = FALSE)
#> ── Summary of a <tbl_now> ──────────────────────────────────────────────────────
#> 26 rows in 6 components.
#> 
#> cases
#>   quantity     n total  mean    sd   min   q25   q50   q75   q90   max prop_zero
#>   <chr>    <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>     <dbl>
#> 1 per_eve…  1095 52987  48.4  53.3     0    14    30    64   104   358   0.00365
#> 2 per_rep…  1095 52987  48.4  54.3     0    14    29    64   111   420   0.00274
#> 
#> zero_run
#>   quantity        n total  mean    sd   min   q25   q50   q75   q90   max
#>   <chr>       <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 event_date      2     4     2  1.41     1     1     1     3     3     3
#> 2 report_date     3     3     1  0        1     1     1     1     1     1
#> 
#> autocorrelation
#>   quantity                  n value
#>   <chr>                 <int> <dbl>
#> 1 per_event_date lag 1   1094 0.958
#> 2 per_report_date lag 1  1094 0.885
#> 
#> coverage
#>    quantity                    n total  value date_min   date_max  
#>    <chr>                   <int> <dbl>  <dbl> <date>     <date>    
#>  1 total_cases              8265 52987 NA     NA         NA        
#>  2 event_date               1091 52987 NA     1990-01-01 2010-11-29
#>  3 report_date              1092 52987 NA     1990-01-01 2010-12-20
#>  4 now                        NA    NA NA     2010-12-20 2010-12-20
#>  5 unobserved_cells            0    NA NA     NA         NA        
#>  6 max_delay                  NA    NA 26     NA         NA        
#>  7 triangle_cells_observed  5154    NA NA     NA         NA        
#>  8 triangle_cells_possible 29214    NA NA     NA         NA        
#>  9 triangle_occupancy         NA    NA  0.176 NA         NA        
#> 10 now_gap_event              NA    NA  3     NA         NA        
#> ℹ 1 more row.
#> 
#> completeness
#>   quantity       n total   mean     sd   min   q25    q50    q75   q90   max
#>   <chr>      <int> <dbl>  <dbl>  <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl>
#> 1 delay <= 0  1090  2099 0.0381 0.0533 0     0     0.0220 0.0594 0.1     0.5
#> 2 delay <= 1  1090 26595 0.510  0.175  0     0.410 0.510  0.618  0.710   1  
#> 3 delay <= 2  1090 44988 0.844  0.130  0     0.781 0.867  0.930  1       1  
#> 4 delay <= 3  1090 49837 0.931  0.0850 0.104 0.9   0.953  1      1       1  
#> 5 delay <= 4  1090 51451 0.963  0.0597 0.5   0.949 0.984  1      1       1  
#> 6 delay <= 5  1090 52126 0.978  0.0449 0.5   0.972 1      1      1       1  
#> 7 delay <= 6  1090 52505 0.988  0.0330 0.5   0.990 1      1      1       1  
#> 8 delay <= 7  1090 52668 0.992  0.0275 0.5   1     1      1      1       1  
#> # ℹ 1 more variable: prop <dbl>
#> 
#> delay
#>   quantity            n total  mean    sd   min   q25   q50   q75   q90   max
#>   <chr>           <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 event_to_report  8265 52987  1.74  1.21     0     1     1     2     3    26
#> 
#> ℹ Use `dplyr::filter()` or `tibble::as_tibble()` for the full schema.
```
