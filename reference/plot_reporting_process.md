# Plot the reporting process

**\[experimental\]**

## Usage

``` r
plot_reporting_process(x, plotly = FALSE, palette = .tbl_now_palette())
```

## Arguments

- x:

  A
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
  object.

- plotly:

  If `TRUE`, return an interactive plotly widget (hover, zoom) instead
  of a static ggplot2 plot. Default `FALSE`.

- palette:

  A named colour palette. Defaults to the package palette.

## Value

A ggplot2 object (or a plotly widget when `plotly = TRUE`).

## Details

Shows total reports by **report date** (when the reports arrived),
facetted by stratum when present.

## See also

[`diagnostic_plot()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnostic_plot.md).

## Examples

``` r
data(denguedat)
dn <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)
plot_reporting_process(dn)
```
