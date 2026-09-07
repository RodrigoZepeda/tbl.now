# Plot the per-date delay profiles

**\[experimental\]**

One translucent curve per date (see `by`) giving that date's share of
reports at each delay, coloured by its mean delay. A batch is a lone
right-shifted (long-delay) curve.

## Usage

``` r
plot_delay_profiles(
  x,
  by = c("report", "event"),
  max_delay = NULL,
  plotly = FALSE,
  axis = c("report", "revision"),
  linewidth = 1,
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

- axis:

  Which time axis the delay is measured on: `"report"` (default) or
  `"revision"`. Report-axis delays are measured from event to report;
  revision-axis delays are measured from report to revision, the same
  quantity as `.revision_delay`. Needs a revision process (see
  [`add_revision_date()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md));
  cases still `"pending"` are left out.

- linewidth:

  Multiplier on the width of the per-date curves. Default `1` (drawn at
  `0.4`). The curves are deliberately faint and overplotted – it is
  their envelope that carries the message – so raising this on a long
  series fills the panel in.

- palette:

  A named colour palette (see
  [`tbl_now_palette()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_palette.md)).

## Value

A ggplot2 object (or a plotly widget when `plotly = TRUE`).

## See also

[`plot_delay_distribution()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_distribution.md)
for the pooled delay distribution rather than one curve per date;
[`plot_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_drift.md)
for whether those curves move over time;
[`diagnose_batches2()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches2.md)
for the test behind the eyeball;
[`diagnostic_plot()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnostic_plot.md)
for the whole gallery.

## Examples

``` r
data(denguedat)
dn <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)
plot_delay_profiles(dn)
```
