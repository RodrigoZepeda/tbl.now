# Screen the report axis for batched reporting

**\[experimental\]**

Detects **batches**: report dates at which a stalled reporting system
releases a backlog. A batch *moves* reports along the report axis
without creating them, so it shows up as a spike preceded by a deficit,
while the total over a window spanning both is unchanged.
`batch_screen()` is completely **model-free** – it needs only a
[`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md),
not a fitted model – which makes it the right tool for exploratory data
analysis before any nowcasting model is chosen.

## Usage

``` r
batch_screen(
  data,
  lookback = 3L,
  baseline_window = NULL,
  baseline_method = c("repeated_median", "running_median"),
  period = NULL,
  null_model = c("auto", "poisson", "robust"),
  alpha = 0.05
)
```

## Arguments

- data:

  A
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
  object of any `data_type`.

- lookback:

  Integer `k`: how many report dates before `r` the window reaches back.
  Should comfortably cover the longest plausible stall. Default `3`.

- baseline_window:

  Odd integer width of the running median used to estimate the baseline.
  Must satisfy `baseline_window >= 2 * lookback + 3` so that a clean
  date is never outvoted by a batch episode. Defaults to the smallest
  admissible odd value (adjusted upward to a multiple of `period` plus
  one when `period` is supplied).

- baseline_method:

  How the baseline is smoothed. `"repeated_median"` (default) fits a
  robust local *line* (Siegel's repeated median), which keeps the 50%
  breakdown that makes a median immune to the batch episode while
  remaining unbiased when the report series trends – essential on an
  epidemic curve, where a local *constant* fit would call every rising
  date a surge. `"running_median"` fits a local constant; use it for a
  flat series.

- period:

  Optional integer cycle length of a *scheduled* reporting pattern (e.g.
  `7` for a weekly cycle on daily data). `NULL` (default) disables the
  calendar correction.

- null_model:

  `"auto"` (default) uses the exact Poisson/Binomial null for
  non-negative counts and the robust normal approximation for signed
  (count-cumulative) increments. `"poisson"` and `"robust"` force the
  choice.

- alpha:

  Significance level for the Benjamini-Hochberg flag and for the
  `classification` column. Default `0.05`.

## Value

A tibble of class `batch_screen`, one row per (report date, stratum),
with the columns described under **Details**. Has a
[`print()`](https://rodrigozepeda.github.io/tbl.now/reference/print.md)
method that summarises the flagged dates.

## The mathematics

Index each item by its **event date** \\t\\ (when it happened) and its
**report date** \\r\\ (when it was recorded); the reporting delay is \\d
= r - t\\. Let \\R_r\\ be the number of items reported on date \\r\\ and
\\\mu_r\\ its expected value under a stable reporting process.

**A batch is a transport, not a creation.** When a stalled desk releases
a backlog, it *relabels the report date* of items that already exist; it
never creates or destroys them, and it can only ever move an item
*later*. This has an exact consequence. Over a window \\\mathcal{W} =
\\r-k,\dots,r\\\\ that contains both the lull and the spike, write the
window total and its null mean

\$\$S = \sum\_{j\in\mathcal{W}} R_j, \qquad M = \sum\_{j\in\mathcal{W}}
\mu_j.\$\$

The **transport discriminant** is their difference,

\$\$\Delta_r(k) = (R_r - \mu_r) -
\underbrace{\textstyle\sum\_{j\<r}(\mu_j - R_j)}\_{\text{deficit }
W_r(k)} = S - M,\$\$

i.e. simply the window total, centred. **Theorem.** Under *any* batch
mechanism whose displacements stay inside \\\mathcal{W}\\ –
deterministic or random, clearing the backlog in any order – the count
in the window is a *pathwise* invariant, so \\S \sim
\mathrm{Poisson}(M)\\ exactly, with \\\mathbb{E}\\\Delta = 0\\. A
genuine surge that *creates* items with mean \\\eta\\ instead gives \\S
\sim \mathrm{Poisson}(M + \eta)\\, so \\\mathbb{E}\\\Delta = \eta\\.
Hence \\\Delta\\ sees only creation and the deficit \\W\\ sees only
transport, and the pair separates a batch from a surge:

|                      |                 |                                 |
|----------------------|-----------------|---------------------------------|
|                      | \\W \approx 0\\ | \\W \gg 0\\                     |
| \\\Delta \approx 0\\ | nothing         | **batch**                       |
| \\\Delta \gg 0\\     | **surge**       | batch and surge                 |
| \\\Delta \< 0\\      | –               | **hold in progress / deletion** |

**Estimating \\M\\ without cheating.** The theorem is about the *true*
mean. If \\M\\ were smoothed from a series containing the very episode
under test, the deficits would drag it down and \\\Delta\\ would acquire
a spurious positive mean – the batch would mask itself as a surge.
`batch_screen()` therefore refits the baseline for each candidate window
from report dates lying strictly *outside* that window (the model-free
analogue of leaving an observation out). Because the transport never
crosses the window boundary, \\M\\ cannot see it, and \\\Delta\\ is
invariant to the batch pathwise. The baseline is a robust local line
(Siegel's repeated median), which keeps a 50% breakdown point against
the episode while remaining unbiased under a trend.

## What is computed

For every report date `r` and stratum, with a look-back window
`W = {r - lookback, ..., r}`:

- `reported`:

  \\R_r\\, the number of items reported on date `r`. For
  `"count-cumulative"` data these are the *signed increments* of the
  cumulative curve, so `reported` may be negative (a net down-revision).

- `baseline`:

  \\\hat\mu_r\\, a robust running-median estimate of how many reports
  date `r` *should* have carried.

- `deficit`:

  \\W_r(k) = \sum\_{j\<r}(\hat\mu_j - R_j)\\, the reports that went
  missing in the `lookback` dates before `r`.

- `delta`:

  \\\Delta_r(k) = S - M\\, the window total minus its null mean. Blind
  to any transport confined to the window; sensitive to creation.

- `p_transport`, `p_creation`, `p_deletion`:

  One-sided \\p\\-values for the three directions.

- `classification`:

  One of `"batch"`, `"surge"`, `"batch_and_surge"`,
  `"hold_or_deletion"`, `"none"`.

- `batch`:

  Logical: `p_transport` survives a Benjamini-Hochberg correction across
  all (report date, stratum) pairs at level `alpha`.

## The null distribution

For non-negative counts (`"linelist"` and `"count-incidence"` data):

- *transport* (**exact**): conditionally on the window total \\S\\, the
  reports allocate across the window's dates multinomially, so the share
  landing on the final date is \\R_r \mid S \sim \mathrm{Binomial}(S,
  \hat\mu_r / M)\\. A batch pushes reports to the *last* date, so we
  test the upper tail. Conditioning on \\S\\ removes the unknown
  intensity entirely, which is what makes this test exactly the right
  size whatever the truth happens to be – **and what makes it
  insensitive to error in the baseline.**

- *creation* (**dispersion-corrected**): the theory gives \\S \sim
  \mathrm{Poisson}(M)\\ exactly under any within-window batch, but with
  \\M\\ *known*. Here \\M\\ is estimated from the same series, so
  \\\Delta\\ carries the baseline's error as well as Poisson noise. An
  exact Poisson tail would then be anti-conservative, so the creation
  test is widened by a robustly-estimated quasi-Poisson dispersion.

The asymmetry is not an accident: it is the practical face of the
conditioning argument. The batch verdict is the trustworthy one.

For `"count-cumulative"` data the increments are signed, the window
total is a difference of two counting processes rather than a count, and
the exact Poisson/Binomial reference is replaced by a robust normal
approximation (`null_model = "robust"`). The conservation logic is
unchanged; only the reference law degrades.

## A caveat on the `"surge"` verdict

The two channels are *not* equally trustworthy without a model. The
**transport** verdict (`"batch"`) compares dates *within* one window and
is insensitive to the overall level – in the exact Poisson path it
literally conditions on the window total – so it is stable. The
**creation** verdict (`"surge"`) compares the window total against the
baseline, and is therefore only as good as the baseline. A robust
local-linear baseline is unbiased under a *linear* trend but not under a
sharply *curved* one, so on a steeply growing epidemic curve `"surge"`
will fire on ordinary growth. That is not a bug: mass really is being
created. It does mean that if you want to detect genuine surges you
should fit a model rather than rely on this screen.

## Calendar effects

A reporting system that is always closed at weekends produces every
batch symptom, every week. Pass `period` (e.g. `period = 7` for daily
data) and the baseline is corrected by per-phase medians taken across
cycles, which recovers the schedule exactly provided each phase is hit
by *irregular* batches in fewer than half of its cycles. An irregular
batch is then an excursion *relative to the schedule*.

## See also

[`batch_shape_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_shape_test.md)
for the complementary test on *which* event dates a report date drew
from, and
[`simulate_batch()`](https://rodrigozepeda.github.io/tbl.now/reference/simulate_batch.md)
to inject a known batch for validation.

## Examples

``` r
library(tbl.now)
data(denguedat, package = "tbl.now")

dengue_tbl <- tbl_now(
  denguedat,
  event_date  = onset_week,
  report_date = report_week,
  data_type   = "linelist",
  verbose     = FALSE
)

screened <- batch_screen(dengue_tbl, lookback = 2)
#> Warning: ! `batch_screen()` is experimental: results are not guaranteed and the
#>   interface may change.
#> ℹ Treat a flagged report date as a potential batch, not a confirmed one.
#> This warning is displayed once every 8 hours.
head(screened)
#> # A tibble: 6 × 16
#>   report_date stratum reported baseline window_total window_mean deficit  delta
#>   <date>      <chr>      <dbl>    <dbl>        <dbl>       <dbl>   <dbl>  <dbl>
#> 1 1990-01-01  all            3     NA             NA        NA     NA     NA   
#> 2 1990-01-08  all           26     NA             NA        NA     NA     NA   
#> 3 1990-01-15  all           62     43.6           91       138.    65    -46.6 
#> 4 1990-01-22  all           41     41.2          129       132.     2.38  -2.62
#> 5 1990-01-29  all           40     29.8          143        94.8  -38     48.2 
#> 6 1990-02-05  all           36     29            117        93    -17     24   
#> # ℹ 8 more variables: window_scale <dbl>, deficit_scale <dbl>,
#> #   p_creation <dbl>, p_deletion <dbl>, p_transport <dbl>,
#> #   p_transport_bh <dbl>, classification <chr>, batch <lgl>
```
