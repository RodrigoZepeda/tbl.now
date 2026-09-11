# The transport discriminant of a reporting series

**\[experimental\]**

Computes, for every report date, the two coordinates of
[`diagnose_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches.md)'s
conservation law – the **deficit** (the *transport* axis: how many
reports the preceding window is missing) and the window **discriminant**
(the *creation* axis: the window total relative to its baseline) –
together with their robust standardised versions `transport_z` and
`creation_z`.

## Usage

``` r
transport_discriminant(
  x,
  lookback = 7L,
  baseline_window = NULL,
  period = NULL,
  alpha = 0.05,
  axis = c("report", "revision"),
  drop_censored = TRUE
)
```

## Arguments

- x:

  A
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
  object.

- lookback:

  Integer window half-width `k` (report-grid steps) over which the
  deficit is accumulated. Default `7` (a week of daily reporting).

- baseline_window, period:

  Baseline controls, passed through to the same machinery as
  [`diagnose_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches.md).
  `period` (e.g. `7`) absorbs a scheduled weekly reporting cadence.

- alpha:

  Level for the `classification` labels. Default `0.05`.

- axis:

  Which time axis to scan for arrivals: `"report"` (default) or
  `"revision"`. Needs a revision process (see
  [`add_revision_date()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md));
  cases still `"pending"` are left out.

- drop_censored:

  Logical. Ignore the rows whose date on `axis` is flagged censored
  (`is_censored_report`, or `is_censored_revision` on the revision
  axis). Default `TRUE`: a censored date is a *bound*, not the date the
  record arrived, so those rows would pile up on the censoring date and
  be rediscovered as the very batch the censoring already recorded.

## Value

A tibble of class `transport_discriminant`, one row per (report date,
stratum), with columns `report_date`, `stratum`, `reported`, `baseline`,
`window_total`, `spike` (reported minus baseline), `deficit`, `delta`,
`transport_z`, `creation_z`, `classification` and `batch`.

## Details

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
[`diagnose_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches.md).

## See also

[`diagnose_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches.md)
for the hypothesis test,
[`diagnostic_plot()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnostic_plot.md)
to plot this plane.

## Examples

``` r
data(denguedat)
# The two and a half years around the 1996 and 1997 backlog dumps. The whole
# twenty-year series works the same way, it just takes longer to scan.
window <- denguedat[
  denguedat$onset_week >= as.Date("1995-06-01") &
    denguedat$onset_week <= as.Date("1998-01-01"),
]
dn <- tbl_now(window, onset_week, report_week, verbose = FALSE)
td <- transport_discriminant(dn)
td[td$batch, ]
#> <transport_discriminant>: 2 report dates, look-back 7, 2 batches and 0 surges at alpha = 0.05.
#> # A tibble: 2 × 14
#>   report_date stratum reported baseline window_total spike deficit delta
#>   <date>      <chr>      <dbl>    <dbl>        <dbl> <dbl>   <dbl> <dbl>
#> 1 1996-02-12  all           46     29.4          236  16.6    60.7 -44.1
#> 2 1997-09-15  all           93     55.7          330  37.3    92   -54.7
#> # ℹ 6 more variables: transport_z <dbl>, creation_z <dbl>, p_transport <dbl>,
#> #   p_creation <dbl>, classification <chr>, batch <lgl>
```
