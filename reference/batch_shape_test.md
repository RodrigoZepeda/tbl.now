# Test whether one report date drew from unusually old event dates

**\[experimental\]**

A complement to
[`batch_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_test.md),
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
  [`batch_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_test.md).

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

## Details

The delays of the reports arriving on `at` are compared with the pooled
delays of the reports arriving on nearby dates, using a one-sided
rank-sum (Wilcoxon) statistic directed at *longer* delays on `at`. The
p-value comes from a permutation, so no asymptotic approximation is
used.

The test is model-free: as long as the epidemic curve is locally smooth,
neighbouring report dates share one common delay profile, so their delay
labels are exchangeable and the permutation test is (essentially)
distribution-free – it needs neither the delay distribution nor the
epidemic curve. With Poisson counts `permute = "items"` is exact; if the
counts are overdispersed (neighbouring report dates share event dates,
so individual items are not exchangeable) use `permute = "blocks"`,
which permutes whole report dates.

The `guard` argument omits report dates immediately adjacent to `at`
from the comparison set: if a batch is present, its own deficit dates
sit right beside the spike and would contaminate the reference group.
For `"count-cumulative"` data only positive increments carry a
meaningful delay; negative increments (down-revisions) are dropped with
a message.

## See also

[`batch_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_test.md),
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
