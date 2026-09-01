# Plot the reporting triangle

**\[experimental\]**

Tiles over (event date, delay), filled by the reported count. Cells that
are **observable but empty** (a genuine reported zero) are drawn in a
muted blue; cells that are **not yet reportable** (report date beyond
`now`, the upper-right wedge) are left blank. A **third axis for report
date** is drawn as evenly spaced dashed diagonals
(`report = event + delay`) running up-right at 45 degrees, so all three
quantities – event date, delay and report date – can be read off one
plot. A batch is a single report date, i.e. one such diagonal.

## Usage

``` r
plot_reporting_triangle(
  x,
  max_delay = NULL,
  report_ticks = 6L,
  mark_batches = 0L,
  plotly = FALSE,
  axis = c("report", "validation"),
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
  [`diagnose_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches.md).

- plotly:

  If `TRUE`, return an interactive plotly widget instead of a static
  plot. Default `FALSE`.

- axis:

  Which time axis to draw: `"report"` (default) or `"validation"`. On
  the validation axis the picture answers the laboratory's version of
  the question – when results arrived, rather than when reports did.
  Needs a validation process (see
  [`add_validation_date()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md));
  cases still `"pending"` have no validation date and are left out.

- palette:

  A named colour palette. Defaults to the package palette.

## Value

A ggplot2 object (or a plotly widget when `plotly = TRUE`).

## See also

[`plot_reporting_hexamap()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_reporting_hexamap.md)
for the same grid drawn so that event date, report date and delay are
all read the same way;
[`plot_delay_profiles()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_profiles.md)
for one curve per date instead of a grid;
[`complete_zeroes()`](https://rodrigozepeda.github.io/tbl.now/reference/complete_zeroes.md)
to fill the cells that are genuinely zero;
[`diagnostic_plot()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnostic_plot.md)
for the whole gallery.

## Examples

``` r
data(denguedat)
dn <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)

# Rows are event dates, columns are delays. The blank upper-right wedge is
# the future: those reports cannot have arrived yet. That wedge is what a
# nowcast fills in.
plot_reporting_triangle(dn)
```
