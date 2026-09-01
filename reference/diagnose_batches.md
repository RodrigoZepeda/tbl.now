# Screen the report axis for batched reporting

**\[experimental\]**

Detects **batches**: report dates at which a stalled reporting system
releases a backlog. A batch *moves* reports along the report axis
without creating them, so it shows up as a spike preceded by a deficit,
while the total over a window spanning both is unchanged.
`diagnose_batches()` is completely **model-free** – it needs only a
[`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md),
not a fitted model – which makes it the right tool for exploratory data
analysis before any nowcasting model is chosen.

## Usage

``` r
diagnose_batches(
  x,
  lookback = 7L,
  baseline_window = NULL,
  period = NULL,
  null_model = c("auto", "poisson", "robust"),
  axis = c("report", "validation"),
  alpha = 0.05
)
```

## Arguments

- x:

  A
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
  object of any `data_type`.

- lookback:

  Integer `k`: how many report dates before `r` the window reaches back.
  Should comfortably cover the longest plausible stall. Default `7` (a
  week of daily reporting).

- baseline_window:

  Odd integer width of the smoother used to estimate the baseline (a
  robust local line, Siegel's repeated median). Must satisfy
  `baseline_window >= 2 * lookback + 3` so that a clean date is never
  outvoted by a batch episode. Defaults to the smallest admissible odd
  value (adjusted upward to a multiple of `period` plus one when
  `period` is supplied).

- period:

  Optional integer cycle length of a *scheduled* reporting pattern (e.g.
  `7` for a weekly cycle on daily data). `NULL` (default) means the
  cycle is taken from the object's temporal effects if present
  (day-of-week -\> `7`, week-of-year -\> `52`), and otherwise no
  calendar correction is applied. A value passed here always wins.

- null_model:

  `"auto"` (default) picks the null from the data. The exact
  Poisson/Binomial null assumes Poisson counts *and* a baseline that
  captures the mean; real surveillance counts are overdispersed, so on
  non-negative counts `auto` uses the exact null only when no
  overdispersion is detected (dispersion at most 1.5) and otherwise
  falls back to the dispersion-corrected robust normal approximation.
  Signed (count-cumulative) increments always use the robust null.
  `"poisson"` and `"robust"` force the choice; note that `"poisson"` is
  anti-conservative (over-flags) on overdispersed counts.

- axis:

  Which time axis to scan for arrivals: `"report"` (default) or
  `"validation"`. The question is the same either way – did an unusual
  number of records land on this date? – so a laboratory clearing its
  backlog is found exactly as a surveillance system clearing its inbox
  is. `"validation"` needs a validation process (see
  [`add_validation_date()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md))
  and ignores cases that are still `"pending"`, which have no validation
  date to arrive on.

- alpha:

  Significance level for the Benjamini-Hochberg `batch` flag. Default
  `0.05`.

## Value

A tibble of class `diagnose_batches`, one row per (report date,
stratum), with a [`print()`](https://rdrr.io/r/base/print.html) method
that summarises the flagged dates. Columns:

- `report_date`:

  The report (registration) date the row describes.

- `stratum`:

  The stratum label, or `"all"` when the data is unstratified.

- `reported`:

  Reports recorded on `report_date` (a signed increment for
  `"count-cumulative"` data, so it can be negative).

- `baseline`:

  The robust expected number of reports on `report_date` under "no
  batch", from the leave-window-out local line.

- `deficit`:

  How many reports the `lookback` days *before* `report_date` were
  missing relative to baseline – the **transport** signal. Large and
  positive when a stall preceded a spike.

- `delta`:

  The window total minus its baseline mean – the **creation** signal.
  Near zero for a pure batch (mass only moved), large for a surge.

- `p_transport`:

  One-sided p-value that the deficit is larger than noise (the raw,
  per-point transport test).

- `p_transport_bh`:

  `p_transport` after a Benjamini-Hochberg correction across all rows;
  the flag below thresholds this.

- `batch`:

  Logical verdict: `TRUE` when `p_transport_bh < alpha` and the window
  is not still depleted (a hold). This is the column to trust.

## Details

The idea is that a batch **moves** reports along the report axis without
creating them. Over a window of report dates that spans both the lull
and the release, the *total* is therefore unchanged – every report you
would have seen in the window you still see, just on a different day. A
genuine surge instead **adds** reports and inflates the window total. So
two quantities separate the two cases: the **deficit** (how many reports
the days just before the spike were missing) picks up transport, and the
window total (relative to a baseline) picks up creation. A batch has a
large deficit but a conserved window total; a surge has an inflated
window total but no deficit.

The baseline for each candidate window is refit from report dates lying
strictly *outside* that window, using a robust local line (Siegel's
repeated median). Smoothing through the episode instead would let the
deficit drag the baseline down and the batch would mask itself as a
surge.

The `batch` flag is the trustworthy verdict: it compares dates *within*
one window (insensitive to the overall level) and is
**Benjamini-Hochberg corrected** across every (report date, stratum)
pair, so it controls the false discovery rate rather than firing on
every point that crosses a raw threshold. A per-point creation ("surge")
label is deliberately *not* returned: it only compares the window total
against the baseline, so on a steeply curved epidemic curve it fires on
ordinary growth. If you need genuine surges, fit a model. For
`"count-cumulative"` data the increments are signed and `reported` can
be negative (a down-revision).

A reporting system that is always closed at weekends produces every
batch symptom, every week, so `diagnose_batches()` needs the length of
any scheduled cycle. It reads that from the object's temporal effects
when it can: a **day-of-week** effect sets `period = 7`, a
**week-of-year** effect `period = 52` (see
[`add_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.md)).
Pass `period` yourself to override; if the data is daily and carries no
temporal effect, the function suggests `period = 7`. With a period set,
the baseline is corrected by per-phase medians across cycles, so an
irregular batch reads as an excursion relative to the schedule.

## See also

[`diagnose_batch_shape()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batch_shape.md)
for the complementary test on *which* event dates a flagged report date
drew from;
[`transport_discriminant()`](https://rodrigozepeda.github.io/tbl.now/reference/transport_discriminant.md)
for the two coordinates behind the test, without the hypothesis test on
top;
[`simulate_batch()`](https://rodrigozepeda.github.io/tbl.now/reference/simulate_batch.md)
to plant a known batch and check the screen finds it;
[`plot_reporting_process()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_epidemic_process.md)
and
[`plot_reporting_triangle()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_reporting_triangle.md)
to see it;
[add_is_censored_report()](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
to record a batch once you believe it. The [*Diagnosing a tbl_now*
article](https://rodrigozepeda.github.io/tbl.now/articles/diagnosing-a-tbl-now.html)
works through a real example.

## Examples

``` r
data(denguedat)

dengue_tbl <- tbl_now(
  denguedat,
  event_date  = onset_week,
  report_date = report_week,
  data_type   = "linelist",
  verbose     = FALSE
)

# Scan every report date for an unusually large arrival.
screened <- diagnose_batches(dengue_tbl, lookback = 2)
#> Warning: ! `diagnose_batches()` is experimental: results are not guaranteed and the
#>   interface may change.
#> ℹ Treat a flagged report date as a potential batch, not a confirmed one.
#> This warning is displayed once every 8 hours.
head(screened)
#> # A tibble: 6 × 9
#>   report_date stratum reported baseline deficit  delta p_transport
#>   <date>      <chr>      <dbl>    <dbl>   <dbl>  <dbl>       <dbl>
#> 1 1990-01-01  all            3     NA     NA     NA    NA         
#> 2 1990-01-08  all           26     NA     NA     NA    NA         
#> 3 1990-01-15  all           62     43.6   65    -46.6   0.00000103
#> 4 1990-01-22  all           41     41.2    2.38  -2.62  0.430     
#> 5 1990-01-29  all           40     29.8  -38     48.2   1.000     
#> 6 1990-02-05  all           36     29    -17     24     0.934     
#> # ℹ 2 more variables: p_transport_bh <dbl>, batch <lgl>

# The dates it flagged, strongest evidence first. Treat these as candidates to
# look into, not as confirmed backlog releases. `reported` against `baseline`
# says how much bigger the arrival was than the surrounding days led you to
# expect.
flagged <- screened[screened$batch, ]
nrow(flagged)
#> [1] 4
flagged[order(flagged$p_transport_bh), c("report_date", "reported", "baseline")]
#> # A tibble: 4 × 3
#>   report_date reported baseline
#>   <date>         <dbl>    <dbl>
#> 1 2010-09-13       383    272. 
#> 2 2009-11-16       127     83.5
#> 3 2007-11-26       152     86.5
#> 4 1991-08-12        61     50.2
```
