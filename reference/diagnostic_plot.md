# Diagnostic plots of the reporting process

**\[experimental\]**

## Usage

``` r
diagnostic_plot(
  x,
  panels = "all",
  by = c("report", "event"),
  max_delay = NULL,
  ...,
  plotly = FALSE,
  palette = .tbl_now_palette()
)
```

## Arguments

- x:

  A
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
  object.

- panels:

  Which panels, `"all"` (default) or any subset of `"reporting"`,
  `"triangle"`, `"profiles"`, `"delay_drift"` and `"transport"`.

- by:

  For the `"profiles"` panel, one mark per `"report"` date (default) or
  per `"event"` date.

- max_delay:

  Largest delay on the delay-based panels. `NULL` (default) caps at the
  delay covering 99% of reported mass.

- ...:

  Batch controls (`lookback`, `period`, `alpha`) routed to the
  `"transport"` panel.

- plotly:

  If `TRUE`, return an interactive plotly widget (the panels stacked)
  instead of a static patchwork. Default `FALSE`.

- palette:

  A named colour palette. Defaults to the package palette.

## Value

A patchwork object, or a single plot when one panel is selected (or a
plotly widget when `plotly = TRUE`).

## Details

Lays out a gallery of complementary views of a `tbl_now`'s reporting
process, all aimed at spotting reporting artefacts – especially *batch
reporting*. Each view is also available on its own (see **See also**);
`diagnostic_plot()` picks the ones named in `panels` and combines them
with patchwork. Selecting a single panel returns it as a plain plot.
Every view is facetted by stratum when the `tbl_now` declares strata.

## See also

[`plot_reporting_process()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_reporting_process.md),
[`plot_epidemic_process()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_epidemic_process.md),
[`plot_reporting_triangle()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_reporting_triangle.md),
[`plot_delay_profiles()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_profiles.md),
[`plot_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_drift.md),
[`plot_transport_discriminant()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_transport_discriminant.md),
[`plot_scalogram()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_scalogram.md).

## Examples

``` r
data(denguedat)
dn <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)
diagnostic_plot(dn, panels = c("triangle", "transport"))
#> Warning: ! `transport_discriminant()` is experimental: results are not guaranteed and
#>   the interface may change.
#> ℹ Treat a flagged report date as a potential batch, not a confirmed one.
#> This warning is displayed once every 8 hours.
```
