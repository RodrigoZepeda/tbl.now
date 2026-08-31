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
if (FALSE) {
# Fitting needs Stan, so this is not run.
data(denguedat)
nowobj <- tbl_now(denguedat,
  event_date = "onset_week", report_date = "report_week", verbose = FALSE
)
fit <- tbl_now_to_epidist(nowobj) |>
  epidist::as_epidist_marginal_model() |>
  epidist::epidist()

tidy(fit)
tidy(fit, probs = c(0.05, 0.95))
}
```
