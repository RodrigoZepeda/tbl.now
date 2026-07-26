# Plot the reporting triangle

**\[experimental\]**

## Usage

``` r
plot_reporting_triangle(
  x,
  max_delay = NULL,
  report_ticks = 6L,
  mark_batches = 0L,
  plotly = FALSE,
  palette = .tbl_now_palette()
)
```

## Arguments

- x:

  A
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
  object.

- max_delay:

  Largest delay to draw. `NULL` (default) caps at the delay covering 99%
  of reported mass.

- report_ticks:

  Integer: how many evenly spaced report-date diagonals to draw as the
  third (report-date) axis. `0` disables it. Default `6`.

- mark_batches:

  Integer: additionally highlight this many of the biggest batch stripes
  with a stronger dashed diagonal labelled by report date. `0` (default)
  disables it. Found cheaply from volume spikes, not
  [`batch_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_test.md).

- plotly:

  If `TRUE`, return an interactive plotly widget instead of a static
  plot. Default `FALSE`.

- palette:

  A named colour palette. Defaults to the package palette.

## Value

A ggplot2 object (or a plotly widget when `plotly = TRUE`).

## Details

Tiles over (event date, delay), filled by the reported count. Cells that
are **observable but empty** (a genuine reported zero) are drawn in a
muted blue; cells that are **not yet reportable** (report date beyond
`now`, the upper-right wedge) are left blank. A **third axis for report
date** is drawn as evenly spaced dashed diagonals
(`report = event + delay`) running up-right at 45 degrees, so all three
quantities – event date, delay and report date – can be read off one
plot. A batch is a single report date, i.e. one such diagonal.

## See also

[`diagnostic_plot()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnostic_plot.md).

## Examples

``` r
data(denguedat)
dn <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)
plot_reporting_triangle(dn)
```
