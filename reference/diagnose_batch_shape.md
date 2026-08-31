# Test whether one report date drew from unusually old event dates

**\[experimental\]**

A complement to
[`diagnose_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches.md),
which sees only report *volumes*. This test asks whether the reports
that arrived on a candidate date came from systematically *older* event
dates – the signature of a released backlog – by comparing their delays
with those of neighbouring report dates. It is **model-free** and, under
the conditions below, **exactly distribution-free**.

## Usage

``` r
diagnose_batch_shape(
  x,
  at,
  neighbours = 3L,
  guard = 1L,
  permute = c("items", "blocks"),
  n_permutations = 999L,
  axis = c("report", "confirmation"),
  seed = NULL
)
```

## Arguments

- x:

  A
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
  object.

- at:

  The candidate report date (coercible to the class of the report
  column), typically one flagged by
  [`diagnose_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches.md).

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

- axis:

  Which time axis to scan for arrivals: `"report"` (default) or
  `"confirmation"`. The question is the same either way – did an unusual
  number of records land on this date? – so a laboratory clearing its
  backlog is found exactly as a surveillance system clearing its inbox
  is. `"confirmation"` needs a confirmation process (see
  [`add_confirmation()`](https://rodrigozepeda.github.io/tbl.now/reference/confirmation_setters.md))
  and ignores cases that are still `"pending"`, which have no
  confirmation date to arrive on.

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

[`diagnose_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches.md),
which finds the report dates worth passing to `at`;
[`simulate_batch()`](https://rodrigozepeda.github.io/tbl.now/reference/simulate_batch.md)
to plant a batch of known shape and check it is recovered;
[`plot_delay_profiles()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_profiles.md)
to see the delay profile this tests.

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

# Pick a report date to interrogate. A real workflow takes this from
## diagnose_batches(); here we simply name one.
diagnose_batch_shape(dengue_tbl, at = as.Date("1990-06-25"), n_permutations = 99)
#> Warning: ! `diagnose_batch_shape()` is experimental: results are not guaranteed and the
#>   interface may change.
#> ℹ Treat a flagged report date as a potential batch, not a confirmed one.
#> This warning is displayed once every 8 hours.
#> # A tibble: 1 × 7
#>   stratum  n_at n_reference mean_delay_at mean_delay_reference statistic p_value
#>   <chr>   <int>       <int>         <dbl>                <dbl>     <dbl>   <dbl>
#> 1 all         4          26             1                 2.77     -1.56       1

# `n_permutations` sets the resolution of the p-value: 99 keeps the example
## fast, but use the default (999) for anything you intend to report.
```
