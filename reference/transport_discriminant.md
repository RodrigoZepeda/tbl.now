# The transport discriminant of a reporting series

**\[experimental\]**

## Usage

``` r
transport_discriminant(
  data,
  lookback = 7L,
  baseline_window = NULL,
  period = NULL,
  alpha = 0.05
)
```

## Arguments

- data:

  A
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
  object.

- lookback:

  Integer window half-width `k` (report-grid steps) over which the
  deficit is accumulated. Default `7` (a week of daily reporting).

- baseline_window, period:

  Baseline controls, passed through to the same machinery as
  [`batch_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_test.md).
  `period` (e.g. `7`) absorbs a scheduled weekly reporting cadence.

- alpha:

  Level for the `classification` labels. Default `0.05`.

## Value

A tibble of class `transport_discriminant`, one row per (report date,
stratum), with columns `report_date`, `stratum`, `reported`, `baseline`,
`window_total`, `spike` (reported minus baseline), `deficit`, `delta`,
`transport_z`, `creation_z`, `classification` and `batch`.

## Details

Computes, for every report date, the two coordinates of
[`batch_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_test.md)'s
conservation law – the **deficit** (the *transport* axis: how many
reports the preceding window is missing) and the window **discriminant**
(the *creation* axis: the window total relative to its baseline) –
together with their robust standardised versions `transport_z` and
`creation_z`.

A batch *moves* reports later without creating them, so it leaves a
positive deficit while conserving the window total (`transport_z` large,
`creation_z` near 0). A genuine surge *creates* reports, lifting the
window total without a deficit (`creation_z` large, `transport_z` near
0). Reading the two together separates a backlog release from an
epidemic surge: a point sits in the **batch** corner when its transport
score is large and its creation score is not. A negative `creation_z`
with no transport is a hold in progress (the window is depleted and
nothing has been released yet). The `classification` column applies
these labels at level `alpha`, exactly as in
[`batch_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_test.md).

## See also

[`batch_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_test.md)
for the hypothesis test,
[`diagnostic_plot()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnostic_plot.md)
to plot this plane.

## Examples

``` r
data(denguedat)
dn <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)
td <- transport_discriminant(dn)
td[td$batch, ]
#> # A tibble: 3 × 14
#>   report_date stratum reported baseline window_total spike deficit delta
#>   <date>      <chr>      <dbl>    <dbl>        <dbl> <dbl>   <dbl> <dbl>
#> 1 1996-02-12  all           46     29.4          236  16.6    60.7 -44.1
#> 2 1997-09-15  all           93     55.7          330  37.3    92.0 -54.7
#> 3 2006-07-31  all           27     13.9           67  13.1    40.5 -27.4
#> # ℹ 6 more variables: transport_z <dbl>, creation_z <dbl>, p_transport <dbl>,
#> #   p_creation <dbl>, classification <chr>, batch <lgl>
```
