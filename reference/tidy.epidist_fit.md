# Tidy the delay distribution from an epidist fit

**\[experimental\]**

epidist is the one supported package that does **not** produce a
nowcast. It estimates the **reporting-delay distribution**, so there are
no per-event-date case estimates to tidy and the columns
[`tidy.nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
promises (`event_date`, `stratum`, ...) would all be meaningless. This
method therefore returns a different, delay-shaped table – one row per
distribution parameter rather than one row per date.

## Usage

``` r
# S3 method for class 'epidist_fit'
tidy(x, probs = NULL, level = 0.95, newdata = NULL, ...)
```

## Arguments

- x:

  A fit from
  [`epidist::epidist()`](https://epidist.epinowcast.org/reference/epidist.html).

- probs:

  Optional numeric vector of probabilities in `[0, 1]`, adding one `q*`
  column each.

- level:

  Width of the reported interval. Defaults to `0.95`.

- newdata:

  Optional data frame passed to
  [`epidist::predict_delay_parameters()`](https://epidist.epinowcast.org/reference/predict_delay_parameters.html),
  for a fit with covariates in the delay model
  (`formula = mu ~ 1 + gender`, say). `NULL` uses the fit's own data.

- ...:

  Unused, for generic consistency.

## Value

A tibble, as described in *Value*.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with one
row per parameter of the fitted delay distribution and these columns:

- `term`:

  `character`. The parameter: the distribution's own parameters (`mu`,
  `sigma`, ... – whichever the `family` has) plus the derived `mean` and
  `sd`, which are the numbers most people actually want.

- `estimate`:

  `numeric`. Posterior median.

- `conf.low`, `conf.high`:

  `numeric`. Interval bounds, following broom's naming.

- `level`:

  `numeric`. The width of that interval.

- `engine`:

  `character`. Always `"epidist"`.

`probs` appends one `q*` column per requested probability, exactly as it
does for the nowcast methods – the fit exposes draws, so any quantile is
real rather than an approximation.

## Dispatch

`epidist()` returns an object of class `c("brmsfit", "epidist_fit")`, in
that order, so if broom.mixed is loaded its `tidy.brmsfit()` method
matches **first** and you get raw brms parameters instead of this table.
Call `tidy.epidist_fit(fit)` explicitly when you want the delay
distribution and cannot be sure which method will win.

## See also

[`tidy.nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
for the case-count nowcast engines,
[`tbl_now_to_epidist()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epidist.md)
for the conversion.

## Examples

``` r
data(denguedat)
# A short window: fitting a delay distribution does not need twenty years of
# data, and Stan is slow.
recent <- subset(denguedat, onset_week >= as.Date("2010-06-01"))
nowobj <- tbl_now(recent,
  event_date = "onset_week", report_date = "report_week", verbose = FALSE
)

## The conversion itself is quick, and is what tidy() will later summarise.
converted <- suppressWarnings(tbl_now_to_epidist(nowobj, verbose = FALSE))
#> ℹ No observation time column provided, using 2010-12-27 as the observation date (the maximum of the secondary event upper bound).
head(converted)
#> # A tibble: 6 × 10
#>   ptime_lwr ptime_upr stime_lwr stime_upr obs_time pdate_lwr  pdate_upr 
#>       <dbl>     <dbl>     <dbl>     <dbl>    <dbl> <date>     <date>    
#> 1         0         7         7        14      203 2010-06-07 2010-06-14
#> 2         0         7         7        14      203 2010-06-07 2010-06-14
#> 3         0         7         7        14      203 2010-06-07 2010-06-14
#> 4         0         7         7        14      203 2010-06-07 2010-06-14
#> 5         0         7         7        14      203 2010-06-07 2010-06-14
#> 6         0         7         7        14      203 2010-06-07 2010-06-14
#> # ℹ 3 more variables: sdate_lwr <date>, sdate_upr <date>, obs_date <date>

# Fitting compiles a Stan model, so this takes about a minute even on a short
## chain. `try()` guards the case where \pkg{epidist} is installed but its Stan
# toolchain is not; use \pkg{brms}'s defaults for real work.
fit <- try(
  converted |>
    epidist::as_epidist_marginal_model() |>
    epidist::epidist(chains = 1, iter = 200, refresh = 0),
  silent = TRUE
)
#> ! Setting 560 observation times beyond 182 (=2x max delay) to Inf. This
#>   improves model efficiency by reducing unique observation times while
#>   maintaining model accuracy as these times should have negligible impact.
#> Warning: Found infinite values in the data, which may cause issues for Stan.
#> ℹ Data summarised by unique combinations of:
#> * Model variables: delay bounds, observation time, and primary censoring window
#> ! Reduced from 5426 to 116 rows.
#> ℹ This should improve model efficiency with no loss of information.
#> Warning: Found infinite values in the data, which may cause issues for Stan.
#> Warning: Found infinite values in the data, which may cause issues for Stan.
#> Compiling Stan program...

if (!inherits(fit, "try-error")) {
  print(tidy(fit))
  print(tidy(fit, probs = c(0.05, 0.95)))
}
```
