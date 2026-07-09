# Test whether one report date drew from unusually old event dates

**\[experimental\]**

A complement to
[`batch_screen()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_screen.md),
which sees only report *volumes*. This test asks whether the reports
that arrived on a candidate date came from systematically *older* event
dates – the signature of a released backlog – by comparing their delays
with those of neighbouring report dates. It is **model-free** and, under
the conditions below, **exactly distribution-free**.

## Usage

``` r
batch_shape_test(
  data,
  at,
  neighbours = 3L,
  guard = 1L,
  permute = c("items", "blocks"),
  n_permutations = 999L,
  seed = NULL
)
```

## Arguments

- data:

  A
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
  object.

- at:

  The candidate report date (coercible to the class of the report
  column), typically one flagged by
  [`batch_screen()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_screen.md).

- neighbours:

  Number of report dates on each side used as the reference group.
  Default `3`.

- guard:

  Number of report dates immediately either side of `at` to skip.
  Default `1`. Increase it to at least the longest plausible stall.

- permute:

  `"items"` (default; exact under log-linear intensity and Poisson
  counts) or `"blocks"` (permutes whole report dates; valid under
  overdispersion).

- n_permutations:

  Number of permutations. Default `999`.

- seed:

  Optional RNG seed.

## Value

A tibble, one row per stratum, with `stratum`, `n_at`, `n_reference`,
`mean_delay_at`, `mean_delay_reference`, `statistic` (standardised
rank-sum) and `p_value` (one-sided: longer delays on `at`).

## The mathematics

Under a stable reporting process, an item reported on date \\r\\ came
from \\\delta\\ days earlier with probability proportional to how many
items that day produced times the chance of a delay \\\delta\\:

\$\$q_r(\delta) \\\propto\\ \lambda\_{r-\delta}\\ g_D(\delta),\$\$

where \\\lambda_t\\ is the number of items with event date \\t\\ and
\\g_D\\ is the reporting-delay distribution. This looks as though it
needs a model. It does not. Suppose the incidence is *log-linear* across
a short window, \\\lambda_t = e^{a + \zeta t}\\. Then

\$\$q_r(\delta) \\\propto\\ e^{a + \zeta(r-\delta)} g_D(\delta)
\\\propto\\ e^{-\zeta\delta} g_D(\delta),\$\$

because the factor \\e^{a + \zeta r}\\ is constant in \\\delta\\ and
cancels in the normalisation. **The result does not depend on \\r\\:**
neighbouring report dates share one common delay profile, whatever
\\\lambda\\, \\g_D\\ and even \\\zeta\\ happen to be.

When two samples share a distribution their labels are *exchangeable*,
so a permutation test comparing the delays on `at` against those of its
neighbours is **exactly distribution-free** – it needs neither the delay
distribution nor the epidemic curve, only that the curve be locally
log-linear (which any smooth curve is). A released backlog is old, so
`batch_shape_test()` uses a one-sided rank-sum directed at *longer*
delays; this is the rank analogue of the score statistic \\\sum\_\delta
\delta\\ c_r(\delta)\\ (the mean delay), which is the locally most
powerful test against a backlog tilt. Only the *curvature* of
\\\log\lambda\\ biases it, at second order; overdispersion breaks
exactness (neighbouring dates share event dates), which
`permute = "blocks"` repairs by permuting whole report dates.

## What is tested

The delays of the reports arriving on `at` are compared with the pooled
delays of the reports arriving on nearby dates, using a one-sided
rank-sum (Wilcoxon) statistic directed at *longer* delays on `at`. The
\\p\\-value is obtained by permutation, so no asymptotic approximation
is used.

## When it is exact

Neighbouring report dates share a common delay profile provided \\\log
\lambda_t\\ is *linear* across the comparison window (not constant –
linear, which any smooth trend is, locally). Under that condition, and
with Poisson counts, the date labels are exchangeable and
`permute = "items"` gives an exact test.

If the counts are overdispersed (a shared per-event-date random effect,
as in the negative-binomial model), neighbouring report dates are
*dependent* because they draw on the same event dates. Exchangeability
of individual items then fails. Use `permute = "blocks"`, which permutes
whole report dates rather than individual reports and so respects that
dependence.

The `guard` argument omits report dates immediately adjacent to `at`
from the comparison set: if a batch is present, its own deficit dates
sit right beside the spike and would contaminate the reference group.

## Signed (count-cumulative) data

Only positive increments describe reports *appearing*; negative
increments are down-revisions of an earlier total and carry no
meaningful delay for this test. They are dropped, with a message.

## See also

[`batch_screen()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_screen.md),
[`simulate_batch()`](https://rodrigozepeda.github.io/tbl.now/reference/simulate_batch.md)

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

batch_shape_test(dengue_tbl, at = as.Date("1990-06-25"), n_permutations = 99)
#> Warning: ! `batch_shape_test()` is experimental: results are not guaranteed and the
#>   interface may change.
#> ℹ Treat a flagged report date as a potential batch, not a confirmed one.
#> This warning is displayed once every 8 hours.
#> # A tibble: 1 × 7
#>   stratum  n_at n_reference mean_delay_at mean_delay_reference statistic p_value
#>   <chr>   <int>       <int>         <dbl>                <dbl>     <dbl>   <dbl>
#> 1 all         4          26             1                 2.77     -1.56    0.98
```
