# Detect an abrupt change point in the reporting-delay distribution

**\[experimental\]**

## Usage

``` r
test_delay_changepoint(
  x,
  ...,
  stat = c("median", "spread"),
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
(series length), `changepoint` (the event date of the estimated change,
`NA` when none is found), `statistic` (Pettitt's `K`), `p_value`,
`before` / `after` (the mean of the statistic on each side of the
change), `shift` (`after - before`) and a logical `changepoint_detected`
(`p_value < alpha`).

## Details

Complements
[`test_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/test_delay_drift.md).
Where that tests for a *gradual* monotonic trend, this tests for a
**single abrupt shift** (e.g. a reporting-system change on some date) in
the per-period delay summaries, using **Pettitt's** nonparametric
change-point test. As with
[`test_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/test_delay_drift.md)
it works on both a location statistic (median / mean) and a dispersion
statistic (IQR / 10-90 spread), on mature data only, and — being
rank-based — it is robust to the skew and serial dependence of a delay
series.

## See also

[`test_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/test_delay_drift.md),
[`plot_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_drift.md)

## Examples

``` r
data(denguedat)
dengue <- tbl_now(denguedat,
  event_date = "onset_week", report_date = "report_week", verbose = FALSE
)
test_delay_changepoint(dengue)
#> Warning: ! `test_delay_changepoint()` is experimental: results are not guaranteed and
#>   the interface may change.
#> ℹ Treat a detected change as a potential change point, not a confirmed one.
#> This warning is displayed once every 8 hours.
#> # A tibble: 2 × 10
#>   strata stat       n changepoint statistic  p_value before after  shift
#>   <chr>  <chr>  <int> <date>          <dbl>    <dbl>  <dbl> <dbl>  <dbl>
#> 1 all    median  1090 1998-01-19      38402 2.17e- 3   1.53  1.39 -0.136
#> 2 all    spread  1090 1997-09-01      93409 5.77e-18   2.55  1.91 -0.639
#> # ℹ 1 more variable: changepoint_detected <lgl>
```
