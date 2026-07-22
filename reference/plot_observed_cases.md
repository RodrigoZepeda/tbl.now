# Observed epidemic process with the incompleteness line

**\[experimental\]**

The `"epidemic"` panel of
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html),
drawn on its own: the latest reported counts per `event_date`, with a
dashed vertical line marking where the data become incomplete (less than
`level` of the delay distribution has arrived). Holidays from an
attached
[`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md)
spec are marked with red dots.

[`plot_epidemic_process()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_epidemic_process.md)
draws the same curve without the incompleteness line, next to its
reporting twin
[`plot_reporting_process()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_reporting_process.md).

## Usage

``` r
plot_observed_cases(x, ...)
```

## Arguments

- x:

  A
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
  object.

- ...:

  Further arguments passed to
  [`autoplot.tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/autoplot.tbl_now.md),
  e.g. `level`, `by_strata`, `strata`, `event_date_xlim`, `plotly` or
  `palette`.

## Value

A ggplot2 object (or a plotly widget when `plotly = TRUE`).

## See also

[`autoplot.tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/autoplot.tbl_now.md),
[`plot_epidemic_process()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_epidemic_process.md),
[`plot_reporting_process()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_reporting_process.md).

## Examples

``` r
data(denguedat)
dengue_now <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)
plot_observed_cases(dengue_now)
```
