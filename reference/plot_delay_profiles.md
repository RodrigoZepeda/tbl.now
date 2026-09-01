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
  axis = c("report", "validation"),
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

  Which time axis the delay is measured to: `"report"` (default) or
  `"validation"`. Both are measured *from the event*, so the two are
  directly comparable – run each in turn and the gap between them is the
  time the laboratory adds. (This is not the same quantity as the
  `.validation_delay` column, which is the laboratory's own turnaround,
  measured from the report.) Needs a validation process (see
  [`add_validation_date()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md));
  cases still `"pending"` are left out.

- palette:

  A named colour palette. Defaults to the package palette.

## Value

A ggplot2 object (or a plotly widget when `plotly = TRUE`).

## See also

[`plot_delay_distribution()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_distribution.md)
for the pooled delay distribution rather than one curve per date;
[`plot_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_drift.md)
for whether those curves move over time;
[`diagnose_batch_shape()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batch_shape.md)
for the test behind the eyeball;
[`diagnostic_plot()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnostic_plot.md)
for the whole gallery.

## Examples

``` r
data(denguedat)
dn <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)
plot_delay_profiles(dn)
```
