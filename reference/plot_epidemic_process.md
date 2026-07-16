# Plot the epidemic process

**\[experimental\]**

## Usage

``` r
plot_epidemic_process(x, plotly = FALSE, palette = .tbl_now_palette())
```

## Arguments

- x:

  A
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
  object.

- plotly:

  If `TRUE`, return an interactive plotly widget instead of a static
  plot. Default `FALSE`.

- palette:

  A named colour palette. Defaults to the package palette.

## Value

A ggplot2 object (or a plotly widget when `plotly = TRUE`).

## Details

Shows total cases by **event date** (when the cases occurred), facetted
by stratum when present. The mirror image of
[`plot_reporting_process()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_reporting_process.md)
(which is by *report* date): a real epidemic is smooth, so a lone spike
here would be a surge, not a reporting artefact.

## See also

[`plot_reporting_process()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_reporting_process.md),
[`diagnostic_plot()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnostic_plot.md).

## Examples

``` r
data(denguedat)
dn <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)
plot_epidemic_process(dn)
```
