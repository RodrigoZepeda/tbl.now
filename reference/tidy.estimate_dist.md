# Tidy the delay distribution from an EpiNow2 `estimate_dist()` fit

**\[experimental\]**

[`EpiNow2::estimate_dist()`](https://epiforecasts.io/EpiNow2/reference/estimate_dist.html)
(new in EpiNow2 1.9.0) estimates a **reporting-delay distribution**, not
a nowcast, so – like
[`tidy.epidist_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.epidist_fit.md)
– this returns a *delay-shaped* table: one row per distribution
parameter, with `term` rather than `event_date`.

## Usage

``` r
# S3 method for class 'estimate_dist'
tidy(x, probs = NULL, level = 0.95, ...)
```

## Arguments

- x:

  A fit from
  [`EpiNow2::estimate_dist()`](https://epiforecasts.io/EpiNow2/reference/estimate_dist.html).

- probs:

  Optional numeric vector of probabilities in `[0, 1]`, adding one `q*`
  column each.

- level:

  Width of the reported interval. Defaults to `0.95`, matching
  [`tidy.epidist_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.epidist_fit.md).

- ...:

  Unused, for generic consistency.

## Value

A tibble with `term`, `estimate`, `conf.low`, `conf.high`, `level` and
`engine`.

## Value

One row per parameter of the fitted distribution – whichever `dist` was
fitted, named as EpiNow2 names them – plus the derived **`mean`** and
**`sd`** of the delay, which are the numbers most people want and which
make the result directly comparable with
[`tidy.epidist_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.epidist_fit.md).

Everything is summarised from the posterior draws, so `level` is the
interval you asked for rather than whichever `CrIs` the fit happened to
use, and `probs` can add any quantile.

## How `mean` and `sd` are obtained

Not from the family's algebra. EpiNow2 can fit five families today and
may add more, and a [`switch()`](https://rdrr.io/r/base/switch.html) in
this package would quietly stop reporting anything the day it does.
Instead each draw's parameters are put back into the fit's own
`dist_spec` and discretised with
[`EpiNow2::discretise()`](https://epiforecasts.io/EpiNow2/reference/discretise.html),
which knows the families; the moments are then a summation over the PMF.
Nothing here names a distribution, so a new family works as soon as
`discretise()` supports it.

The trade-off is that these are the moments of the **discretised** delay
– the distribution EpiNow2 convolves with downstream. Against the closed
forms the mean is exact and the sd runs about 1% high, that being the
variance a discrete grid adds. epidist reports continuous-distribution
moments via
[`epidist::add_mean_sd()`](https://epidist.epinowcast.org/reference/add_mean_sd.html),
so expect a difference of that order when comparing the two.

## A name collision worth knowing about

[`summary()`](https://rdrr.io/r/base/summary.html) on an `estimate_dist`
fit has `mean` and `sd` **columns**, and those are the posterior mean
and sd **of the parameter** on that row – not of the delay. The `mean`
and `sd` this method reports are **rows**, and are the delay
distribution's own moments, matching
[`tidy.epidist_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.epidist_fit.md).
Same words, different quantities.

## See also

[`tidy.epidist_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.epidist_fit.md)
for the epidist equivalent,
[`tbl_now_to_EpiNow2()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_EpiNow2.md)
for the conversion.
