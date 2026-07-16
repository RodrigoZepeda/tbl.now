# Plot the per-date delay profiles

**\[experimental\]**

## Usage

``` r
plot_delay_profiles(
  x,
  by = c("report", "event"),
  max_delay = NULL,
  plotly = FALSE,
  palette = .tbl_now_palette()
)
```

## Arguments

- x:

  A
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
  object.

- by:

  One line per `"report"` date (default) or per `"event"` date.

- max_delay:

  Largest delay to draw. `NULL` (default) caps at the delay covering 99%
  of reported mass.

- plotly:

  If `TRUE`, return an interactive plotly widget instead of a static
  plot. Default `FALSE`.

- palette:

  A named colour palette. Defaults to the package palette.

## Value

A ggplot2 object (or a plotly widget when `plotly = TRUE`).

## Details

One translucent curve per date (see `by`) giving that date's share of
reports at each delay, coloured by its mean delay. A batch is a lone
right-shifted (long-delay) curve.

## See also

[`diagnostic_plot()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnostic_plot.md).

## Examples

``` r
data(denguedat)
dn <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)
plot_delay_profiles(dn)
```
