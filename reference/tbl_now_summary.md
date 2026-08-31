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
  rows (`"confirmation_type = confirmed"`).

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

  Proportion of cases in this category (compositional rows).

- `value`:

  A single scalar that is not a distribution: an autocorrelation, a gap,
  an occupancy.

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

# It is an ordinary tibble, so pick out the block you want.
overview |> dplyr::filter(component == "delay")
#> # A tibble: 3 × 18
#>   component quantity     stratum     n total  mean    sd   min   q25   q50   q75
#>   <chr>     <chr>        <chr>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 delay     event_to_re… all      8265 52987  1.74  1.21     0     1     1     2
#> 2 delay     event_to_re… Female   4133 26592  1.74  1.20     0     1     1     2
#> 3 delay     event_to_re… Male     4132 26395  1.74  1.22     0     1     1     2
#> # ℹ 7 more variables: q90 <dbl>, max <dbl>, prop_zero <dbl>, prop <dbl>,
#> #   value <dbl>, date_min <date>, date_max <date>

# How much of each week's eventual total had arrived by delay d? This is the
# reporting-delay problem, in one table.
overview |>
  dplyr::filter(component == "completeness", stratum == "all") |>
  dplyr::select(quantity, value)
#> # A tibble: 8 × 2
#>   quantity   value
#>   <chr>      <dbl>
#> 1 delay <= 0    NA
#> 2 delay <= 1    NA
#> 3 delay <= 2    NA
#> 4 delay <= 3    NA
#> 5 delay <= 4    NA
#> 6 delay <= 5    NA
#> 7 delay <= 6    NA
#> 8 delay <= 7    NA

# Pooled rows only, ignoring the strata.
summary(ndata, by_strata = FALSE)
#> # A tibble: 26 × 18
#>    component    quantity stratum     n total  mean    sd   min   q25   q50   q75
#>    <chr>        <chr>    <chr>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 cases        per_eve… all      1095 52987  48.4 53.3      0    14    30    64
#>  2 cases        per_rep… all      1095 52987  48.4 54.3      0    14    29    64
#>  3 zero_run     event_d… all         2     4   2    1.41     1     1     1     3
#>  4 zero_run     report_… all         3     3   1    0        1     1     1     1
#>  5 autocorrela… per_eve… all      1094    NA  NA   NA       NA    NA    NA    NA
#>  6 autocorrela… per_rep… all      1094    NA  NA   NA       NA    NA    NA    NA
#>  7 coverage     total_c… all      8265 52987  NA   NA       NA    NA    NA    NA
#>  8 coverage     event_d… all      1091 52987  NA   NA       NA    NA    NA    NA
#>  9 coverage     report_… all      1092 52987  NA   NA       NA    NA    NA    NA
#> 10 coverage     now      all        NA    NA  NA   NA       NA    NA    NA    NA
#> # ℹ 16 more rows
#> # ℹ 7 more variables: q90 <dbl>, max <dbl>, prop_zero <dbl>, prop <dbl>,
#> #   value <dbl>, date_min <date>, date_max <date>
```
