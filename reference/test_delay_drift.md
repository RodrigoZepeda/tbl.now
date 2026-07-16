# Test whether the reporting-delay distribution drifts over time

**\[experimental\]**

## Usage

``` r
test_delay_drift(
  x,
  ...,
  stat = c("median", "spread"),
  method = c("hamed-rao", "yue-pilon", "block-bootstrap"),
  by_strata = FALSE,
  strata = NULL,
  mature_only = TRUE,
  level = 0.95,
  alpha = 0.05
)
```

## Arguments

- x:

  A `tbl_now` object.

- ...:

  Passed to the underlying modifiedmk function (e.g. `nsim` for
  `"block-bootstrap"`).

- stat:

  Which delay summaries to test: any of `"median"`, `"mean"`, `"iqr"`
  (q75 - q25) and `"spread"` (q90 - q10). Defaults to median + spread,
  i.e. one *location* and one *dispersion* statistic.

- method:

  Trend test: `"hamed-rao"` (default; Hamed-Rao variance correction,
  [`modifiedmk::mmkh()`](https://rdrr.io/pkg/modifiedmk/man/mmkh.html)),
  `"yue-pilon"` (Yue-Pilon,
  [`modifiedmk::mmky()`](https://rdrr.io/pkg/modifiedmk/man/mmky.html))
  or `"block-bootstrap"` (block-bootstrap MK,
  [`modifiedmk::bbsmk()`](https://rdrr.io/pkg/modifiedmk/man/bbsmk.html)).

- by_strata:

  Logical (default `FALSE`). When `TRUE`, the test is run separately per
  stratum.

- strata:

  Character vector of columns to group on when `by_strata = TRUE`.
  `NULL` (default) uses the object's `strata`.

- mature_only:

  Logical (default `TRUE`). Drop event dates after the `level`
  incompleteness cutoff before testing.

- level:

  Completeness level for the maturity cutoff (default `0.95`).

- alpha:

  Significance level for the `drift` verdict column (default `0.05`).

## Value

A tibble with one row per `stat` x stratum: `strata`, `stat`, `n`
(series length), `tau`, `sens_slope` (change in the statistic per
period), `statistic`, `p_value`, `method` and a logical `drift`
(`p_value < alpha`).

## Details

Runs an **autocorrelation-robust monotonic-trend test** on the
per-period, count-weighted delay summaries, to answer "do delays drift
over time?" in a way that respects the fact that a delay series is
correlated with itself.

For each requested `stat` (and each stratum) it builds the
per-event-date series of that statistic and tests it for a monotonic
trend with the modifiedmk package, which corrects the Mann-Kendall
variance for serial autocorrelation. A plain Mann-Kendall (or an OLS
slope) would be anti-conservative here, because positive autocorrelation
shrinks the effective sample size.

By default the test uses only **mature** event dates (those on or before
the `level` incompleteness cutoff), because the recent,
not-yet-fully-reported dates would otherwise inject a spurious downward
trend.

## See also

[`plot_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_drift.md)

## Examples

``` r
data(denguedat)
dengue <- tbl_now(denguedat,
  event_date = "onset_week", report_date = "report_week", verbose = FALSE
)
test_delay_drift(dengue)
#> Warning: ! `test_delay_drift()` is experimental: results are not guaranteed and the
#>   interface may change.
#> ℹ Interpret a significant result as a potential trend change, not a confirmed
#>   one.
#> This warning is displayed once every 8 hours.
#> # A tibble: 2 × 9
#>   strata stat       n      tau sens_slope statistic p_value method    drift
#>   <chr>  <chr>  <int>    <dbl>      <dbl>     <dbl>   <dbl> <chr>     <lgl>
#> 1 all    median  1090 -0.00918          0    -0.243  0.808  hamed-rao FALSE
#> 2 all    spread  1090 -0.178            0    -2.32   0.0203 hamed-rao TRUE 
```
