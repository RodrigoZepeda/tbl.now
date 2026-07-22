# Periodogram of the case counts or of the reporting delay

**\[experimental\]**

The `"seasonality"` / `"delay_seasonality"` panels of
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html),
drawn on their own: a periodogram whose dominant peak is marked. For
`type = "epidemic"` (green) the peak suggests a Fourier season length to
pass to
[`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md);
for `type = "report"` (red) it marks a cycle in the reporting delay
itself, such as a weekly reporting rhythm.

For a *time-resolved* view — which cycles are strong **when** — see
[`plot_scalogram()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_scalogram.md).

## Usage

``` r
plot_cycles(x, type = c("epidemic", "report"), ...)
```

## Arguments

- x:

  A
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
  object.

- type:

  `"epidemic"` (default) or `"report"`.

- ...:

  Further arguments passed to
  [`autoplot.tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/autoplot.tbl_now.md),
  e.g. `by_strata`, `strata`, `plotly` or `palette`.

## Value

A ggplot2 object (or a plotly widget when `plotly = TRUE`).

## See also

[`autoplot.tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/autoplot.tbl_now.md),
[`plot_scalogram()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_scalogram.md),
[calendar_effect_plots](https://rodrigozepeda.github.io/tbl.now/reference/calendar_effect_plots.md).

## Examples

``` r
data(denguedat)
dengue_now <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)
plot_cycles(dengue_now)
```
