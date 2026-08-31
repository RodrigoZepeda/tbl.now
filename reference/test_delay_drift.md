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
  See the *Choosing a method* section.

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

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with
**one row per requested `stat` per stratum**, and the following columns:

- `strata`:

  `character`. The stratum the row refers to. When `by_strata = FALSE`
  (the default) there is a single stratum labelled `"all"`; otherwise
  one level per observed combination of `strata`.

- `stat`:

  `character`. Which delay summary was tested — one of `"median"`,
  `"mean"`, `"iqr"` or `"spread"`. `"median"`/`"mean"` are *location*
  statistics (are delays getting longer?); `"iqr"`/`"spread"` are
  *dispersion* statistics (are delays getting more erratic?).

- `n`:

  `integer`. **Length of the tested series**, i.e. the number of event
  dates contributing a non-missing value after the `mature_only` filter.
  This is a count of *periods*, not a count of cases. Series with
  `n < 10` (or with zero variance) are not tested and return `NA` for
  every test column.

- `tau`:

  `numeric` in `[-1, 1]`. Kendall's rank correlation between the
  statistic and time — the *effect size*. Positive means delays are
  growing, negative means they are shrinking. Roughly, `|tau|` below 0.1
  is a negligible trend even when `p_value` is small.

- `sens_slope`:

  `numeric`. Sen's slope: the median pairwise rate of change, expressed
  **in delay units per period** — so for weekly data with delays
  measured in weeks, "weeks of delay gained per week elapsed". Multiply
  by `n` for the total drift implied across the series. Unlike an OLS
  slope this is robust to outlying periods.

- `statistic`:

  `numeric`. The autocorrelation-corrected Mann-Kendall `Z` score. Under
  the null it is standard normal, so `|Z| > 1.96` corresponds to
  `p_value < 0.05`.

- `p_value`:

  `numeric`. Two-sided p-value for the null hypothesis of *no monotonic
  trend*, after the serial-correlation correction implied by `method`.
  `NA` when the series was too short or constant.

- `method`:

  `character`. The `method` actually used, echoed back so the result is
  self-documenting when several runs are bound together.

- `drift`:

  `logical`. The verdict: `TRUE` when `p_value < alpha`. `NA` p-values
  give `FALSE`, so a `FALSE` means "no drift detected" and not
  necessarily "no drift".

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

## Interpreting the result

Read `tau` and `sens_slope` *before* `p_value`. On long surveillance
series a tiny, operationally irrelevant trend will still be highly
significant, so `drift = TRUE` on its own is not a reason to act. The
question to ask is whether `sens_slope * n` — the total drift implied
over the observed window — is large relative to the delays themselves.

The location and dispersion statistics answer different questions and
can disagree, which is informative rather than contradictory:

- `median` drifting up, `spread` flat: reporting is uniformly slower.

- `median` flat, `spread` drifting up: the typical case is unaffected
  but the tail is getting worse — often a subset of reporting sites
  degrading.

- both drifting up: broad deterioration in reporting timeliness.

A detected drift means a nowcasting model fitted on a **fixed** delay
distribution will be biased, because it is averaging over delay regimes
that are not exchangeable. Consider a model with a time-varying delay,
or fitting only to the recent, homogeneous stretch of data.

Because this is a trend test it will *not* find an abrupt one-off shift;
a step change can even cancel out to a non-significant monotonic trend.
Pair it with
[`test_delay_changepoint()`](https://rodrigozepeda.github.io/tbl.now/reference/test_delay_changepoint.md),
which is built for exactly that case.

## Choosing a method

All three options are Mann-Kendall tests that correct for the serial
correlation of a delay series; they differ in what they assume about
that correlation, and in cost.

- `"hamed-rao"` (default):

  Inflates the Mann-Kendall variance using all *significant*
  autocorrelation lags of the detrended ranks. It makes no AR(1)
  assumption, is deterministic, and is effectively instantaneous, so it
  is the sensible default for a routine diagnostic. Its variance
  correction is known to be unstable on short series — treat results
  with `n` below roughly 30 as indicative only.

- `"yue-pilon"`:

  Trend-free pre-whitening, which effectively assumes the series is
  **AR(1)**. That assumption is a poor fit for daily reporting delays,
  which carry strong day-of-week periodicity, and pre-whitening is known
  to remove part of the very trend being tested. Offered for
  comparability with the hydrology literature; rarely the right choice
  here.

- `"block-bootstrap"`:

  Resamples contiguous blocks, so it accommodates arbitrary dependence
  *within* a block — including weekly periodicity, if the block length
  covers it. Statistically the most defensible for daily data, and the
  best cross-check when a `hamed-rao` result is borderline. Two caveats:
  it is **stochastic**, so call
  [`set.seed()`](https://rdrr.io/r/base/Random.html) first for a
  reproducible p-value, and it is **thousands of times slower** — it
  scales at roughly the square of the series length, so a multi-year
  daily series can take many minutes per statistic. Reduce `nsim`
  (passed through `...`) or restrict to a shorter window before reaching
  for it.

When a decision matters, run the default first and confirm a borderline
result with `method = "block-bootstrap"` on a restricted window.

## See also

[`test_delay_changepoint()`](https://rodrigozepeda.github.io/tbl.now/reference/test_delay_changepoint.md)
for abrupt shifts,
[`plot_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_drift.md)
to visualise the series being tested.

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
