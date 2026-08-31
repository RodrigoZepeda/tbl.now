# Detect an abrupt change point in the reporting-delay distribution

**\[experimental\]**

Complements
[`diagnose_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_drift.md).
Where that tests for a *gradual* monotonic trend, this tests for a
**single abrupt shift** (e.g. a reporting-system change on some date) in
the per-period delay summaries, using **Pettitt's** nonparametric
change-point test. As with
[`diagnose_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_drift.md)
it works on both a location statistic (median / mean) and a dispersion
statistic (IQR / 10-90 spread), on mature data only, and — being
rank-based — it is robust to the skew and serial dependence of a delay
series.

## Usage

``` r
diagnose_changepoint(
  x,
  ...,
  stat = c("median", "spread"),
  by_strata = FALSE,
  strata = NULL,
  mature_only = TRUE,
  level = 0.95,
  alpha = 0.05,
  axis = c("report", "confirmation")
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

- axis:

  Which time axis the delay is measured to: `"report"` (default) or
  `"confirmation"`. Both are measured *from the event*, so the two are
  directly comparable – run each in turn and the gap between them is the
  time the laboratory adds. (This is not the same quantity as the
  `.confirmation_delay` column, which is the laboratory's own
  turnaround, measured from the report.) Needs a confirmation process
  (see
  [`add_confirmation()`](https://rodrigozepeda.github.io/tbl.now/reference/confirmation_setters.md));
  cases still `"pending"` are left out.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with
**one row per requested `stat` per stratum**, and the following columns:

- `strata`:

  `character`. The stratum the row refers to. When `by_strata = FALSE`
  (the default) there is a single stratum labelled `"all"`; otherwise
  one level per observed combination of `strata`.

- `stat`:

  `character`. Which delay summary was tested — one of `"median"`,
  `"mean"`, `"iqr"` or `"spread"`. As in
  [`diagnose_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_drift.md),
  the first two are *location* statistics and the last two *dispersion*
  statistics.

- `n`:

  `integer`. **Length of the tested series**: the number of event dates
  contributing a non-missing value after the `mature_only` filter —
  periods, not cases. Series shorter than 8 periods, or with zero
  variance, are not tested and return `NA` throughout.

- `changepoint`:

  `Date`. The event date of the **last period before the estimated
  change**; the shift is taken to occur immediately after it. `NA` when
  the series was too short to test. Note this is reported even when
  `changepoint_detected` is `FALSE` — Pettitt's test always returns the
  most extreme candidate split, so this field is only meaningful once
  the p-value supports it.

- `statistic`:

  `numeric`. Pettitt's `K`, the maximum absolute value of the rank
  statistic `U_t` over all candidate split points. Larger means a
  cleaner separation between the two sides. It is not standardised, so
  it grows with `n` and is not comparable across series of different
  lengths.

- `p_value`:

  `numeric`. Two-sided p-value for the null of *no change point*, from
  the standard approximation \\2\exp(-6K^2 / (n^3 + n^2))\\, capped
  at 1. This approximation is known to be conservative for small `n`.

- `before`, `after`:

  `numeric`. The mean of the statistic on each side of `changepoint`, in
  the object's delay units. These are plain means of the per-period
  summaries, so they describe the two regimes directly.

- `shift`:

  `numeric`. `after - before`: the estimated size and direction of the
  jump, in delay units. Positive means delays got longer after the
  change point. This is the number to judge operational relevance by.

- `changepoint_detected`:

  `logical`. The verdict: `TRUE` when `p_value < alpha`. `NA` p-values
  give `FALSE`.

## Interpreting the result

Judge `shift` first and `p_value` second. A statistically detected
change point with a `shift` far smaller than the day-to-day noise in the
delay series is not worth acting on; a large `shift` is, even at a
marginal p-value.

Two structural caveats matter in practice:

- Pettitt's test assumes **exactly one** change point. Given several, it
  returns the most prominent and silently ignores the rest. If you
  suspect more, re-run on each side of the first `changepoint` to search
  recursively.

- A slow monotonic drift will often trip this test too, with the change
  point landing near the middle of the series. Running
  [`diagnose_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_drift.md)
  alongside disambiguates: a genuine step shows up here and not
  necessarily there, while a gradual drift shows up in both.

A confirmed change point usually has an operational explanation — a new
laboratory information system, a change in case definition, a reporting
mandate, a holiday backlog being cleared. Where it lands is a strong
hint about the cause, and about how far back a nowcasting model can
safely be fitted: data before the change point comes from a different
reporting regime.

Unlike
[`diagnose_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_drift.md),
this test has no third-party dependency and no meaningful runtime cost,
so it is cheap to run routinely.

## See also

[`diagnose_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_drift.md)
for a gradual trend rather than a single break;
[`plot_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_drift.md)
to see the series and where the break was found;
[`diagnose_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches.md)
for a one-day spike rather than a lasting shift.

## Examples

``` r
data(denguedat)
dengue <- tbl_now(denguedat,
  event_date = "onset_week", report_date = "report_week", verbose = FALSE
)
diagnose_changepoint(dengue)
#> Warning: ! `diagnose_changepoint()` is experimental: results are not guaranteed and the
#>   interface may change.
#> ℹ Treat a detected change as a potential change point, not a confirmed one.
#> This warning is displayed once every 8 hours.
#> # A tibble: 2 × 10
#>   strata stat       n changepoint statistic  p_value before after  shift
#>   <chr>  <chr>  <int> <date>          <dbl>    <dbl>  <dbl> <dbl>  <dbl>
#> 1 all    median  1090 1998-01-19      38402 2.17e- 3   1.53  1.39 -0.136
#> 2 all    spread  1090 1997-09-01      93409 5.77e-18   2.55  1.91 -0.639
#> # ℹ 1 more variable: changepoint_detected <lgl>
```
