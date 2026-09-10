# Tidy a fitted delay distribution

**\[stable\]**

epidist and
[`EpiNow2::estimate_dist()`](https://epiforecasts.io/EpiNow2/reference/estimate_dist.html)
(new in EpiNow2 1.9.0) do not produce a nowcast: they estimate a
**reporting-delay distribution**. There are therefore no per-event-date
case estimates to tidy, and the columns
[`tidy.nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
promises (`event_date`, `stratum`, ...) would all be meaningless. Both
methods instead return the same *delay-shaped* table: one row per
distribution parameter, with `term` rather than `event_date`, so the two
packages' fits can be compared side by side.

## Usage

``` r
# S3 method for class 'estimate_dist'
tidy(x, probs = NULL, level = 0.95, ...)

# S3 method for class 'epidist_fit'
tidy(x, probs = NULL, level = 0.95, newdata = NULL, ...)
```

## Arguments

- x:

  A fit from
  [`epidist::epidist()`](https://epidist.epinowcast.org/reference/epidist.html)
  or
  [`EpiNow2::estimate_dist()`](https://epiforecasts.io/EpiNow2/reference/estimate_dist.html).

- probs:

  Optional numeric vector of probabilities in `[0, 1]`, adding one `q*`
  column each.

- level:

  Width of the reported interval. Defaults to `0.95`.

- ...:

  Unused, for generic consistency.

- newdata:

  `tidy.epidist_fit()` only. Optional data frame passed to
  [`epidist::predict_delay_parameters()`](https://epidist.epinowcast.org/reference/predict_delay_parameters.html),
  for a fit with covariates in the delay model
  (`formula = mu ~ 1 + gender`, say). `NULL` uses the fit's own data.

## Value

A tibble, as described in *Value*.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with one
row per parameter of the fitted delay distribution – whichever family
was fitted, named as the fitting package names it – plus the derived
**`mean`** and **`sd`** of the delay, which are the numbers most people
actually want. The columns are:

- `term`:

  `character`. The parameter: the distribution's own parameters (`mu`,
  `sigma`, ...) plus `mean` and `sd`.

- `estimate`:

  `numeric`. Posterior median.

- `conf.low`, `conf.high`:

  `numeric`. Interval bounds, following broom's naming.

- `level`:

  `numeric`. The width of that interval.

- `engine`:

  `character`. `"epidist"` or `"EpiNow2"`.

Everything is summarised from the posterior draws, so `level` is the
interval you asked for rather than whichever `CrIs` the fit happened to
use, and `probs` appends one `q*` column per requested probability – a
real quantile rather than an approximation.

## How `mean` and `sd` are obtained

epidist reports continuous-distribution moments via
[`epidist::add_mean_sd()`](https://epidist.epinowcast.org/reference/add_mean_sd.html).

EpiNow2 gets them without naming a distribution. It can fit five
families today and may add more, and a
[`switch()`](https://rdrr.io/r/base/switch.html) in this package would
quietly stop reporting anything the day it does. Instead each draw's
parameters are put back into the fit's own `dist_spec` and discretised
with
[`EpiNow2::discretise()`](https://epiforecasts.io/EpiNow2/reference/discretise.html),
which knows the families; the moments are then a summation over the PMF.
A new family works as soon as `discretise()` supports it.

The trade-off is that the EpiNow2 numbers are the moments of the
**discretised** delay – the distribution EpiNow2 convolves with
downstream. Against the closed forms the mean is exact and the sd runs
about 1% high, that being the variance a discrete grid adds. Expect a
difference of that order when comparing the two packages' fits.

## A name collision worth knowing about

[`summary()`](https://rdrr.io/r/base/summary.html) on an `estimate_dist`
fit has `mean` and `sd` **columns**, and those are the posterior mean
and sd **of the parameter** on that row – not of the delay. The `mean`
and `sd` these methods report are **rows**, and are the delay
distribution's own moments. Same words, different quantities.

## Dispatch

`epidist()` returns an object of class `c("brmsfit", "epidist_fit")`, in
that order, so if broom.mixed is loaded its `tidy.brmsfit()` method
matches **first** and you get raw brms parameters instead of this table.
Call `tidy.epidist_fit(fit)` explicitly when you want the delay
distribution and cannot be sure which method will win.

## See also

[`tbl_now_to_epidist()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epidist.md)
and
[`tbl_now_to_EpiNow2()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_EpiNow2.md)
for the conversions;
[tidy()](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
for tidying a *case-count* nowcast rather than a delay distribution;
[revision_delay](https://rodrigozepeda.github.io/tbl.now/reference/revision_delay.md)
for the delay these are estimating.

## Examples

``` r
data(denguedat)
# A short window: fitting a delay distribution does not need twenty years of
# data, and Stan is slow.
recent <- subset(denguedat, onset_week >= as.Date("2010-06-01"))
nowobj <- tbl_now(recent,
  event_date = "onset_week", report_date = "report_week", verbose = FALSE
)

# `target = "estimate_dist"` gives the censored linelist EpiNow2 wants: one
# row per case, each date as the interval it is known to fall in.
delays <- tbl_now_to_EpiNow2(nowobj,
  target = "estimate_dist", verbose = FALSE, quiet = TRUE
)
head(delays)
#>    pdate_lwr  pdate_upr  sdate_lwr  sdate_upr   obs_date
#> 1 2010-06-07 2010-06-14 2010-06-14 2010-06-21 2010-12-27
#> 2 2010-06-07 2010-06-14 2010-06-14 2010-06-21 2010-12-27
#> 3 2010-06-07 2010-06-14 2010-06-14 2010-06-21 2010-12-27
#> 4 2010-06-07 2010-06-14 2010-06-14 2010-06-21 2010-12-27
#> 5 2010-06-07 2010-06-14 2010-06-14 2010-06-21 2010-12-27
#> 6 2010-06-07 2010-06-14 2010-06-14 2010-06-21 2010-12-27

# A short chain keeps the example quick -- use EpiNow2's defaults for real
## work. `try()` guards the case where EpiNow2 is installed but its Stan
# toolchain is not.
fit <- try(
  EpiNow2::estimate_dist(
    delays,
    stan = EpiNow2::stan_opts(samples = 100, chains = 1)
  ),
  silent = TRUE
)
#> WARN [2026-09-10 04:44:52] estimate_dist (chain: 1): Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess - 
#> WARN [2026-09-10 04:44:52] estimate_dist (chain: 1): Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess - 

# One row per fitted parameter, plus the delay's own mean and sd.
if (!inherits(fit, "try-error")) {
  print(tidy(fit))
  print(tidy(fit, probs = c(0.05, 0.95)))
}
#> # A tibble: 4 × 6
#>   term    estimate conf.low conf.high level engine 
#>   <chr>      <dbl>    <dbl>     <dbl> <dbl> <chr>  
#> 1 meanlog    2.33     2.32      2.34   0.95 EpiNow2
#> 2 sdlog      0.368    0.355     0.380  0.95 EpiNow2
#> 3 mean      11.0     10.9      11.1    0.95 EpiNow2
#> 4 sd         4.21     4.06      4.37   0.95 EpiNow2
#> # A tibble: 4 × 8
#>   term    estimate conf.low conf.high level engine      q5    q95
#>   <chr>      <dbl>    <dbl>     <dbl> <dbl> <chr>    <dbl>  <dbl>
#> 1 meanlog    2.33     2.32      2.34   0.95 EpiNow2  2.32   2.34 
#> 2 sdlog      0.368    0.355     0.380  0.95 EpiNow2  0.356  0.376
#> 3 mean      11.0     10.9      11.1    0.95 EpiNow2 10.9   11.1  
#> 4 sd         4.21     4.06      4.37   0.95 EpiNow2  4.08   4.32 
```
