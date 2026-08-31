# Plot the transport-discriminant plane

**\[experimental\]**

Places each report date by its creation score (x) and transport /
deficit score (y) from
[`transport_discriminant()`](https://rodrigozepeda.github.io/tbl.now/reference/transport_discriminant.md),
shading the region that decides the batch call. Surges are not
distinguished here (they fold into the quiet background) since only the
batch call is of interest.

## Usage

``` r
plot_transport_discriminant(
  x,
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

- ...:

  Passed to
  [`transport_discriminant()`](https://rodrigozepeda.github.io/tbl.now/reference/transport_discriminant.md)
  (e.g. `lookback`, `period`, `alpha`).

- plotly:

  If `TRUE`, return an interactive plotly widget instead of a static
  plot. Default `FALSE`.

- palette:

  A named colour palette. Defaults to the package palette.

## Value

A ggplot2 object (or a plotly widget when `plotly = TRUE`).

## Details

Only the
[`diagnose_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches.md)-confirmed
batches (Benjamini-Hochberg-corrected) are coloured red; the dashed
lines and shaded region are a reference for where a batch sits (deficit
cleared, and significant), not the flagging rule. The most
extreme-looking points (far left, far up) are *holds* – windows still
depleted because the release has not happened yet – not batches. A
genuine batch sits in the band just to the right of the vertical line,
once the window total recovers.

## See also

[`transport_discriminant()`](https://rodrigozepeda.github.io/tbl.now/reference/transport_discriminant.md)
for the numbers behind the plane;
[`diagnose_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches.md)
for the hypothesis test that flags the red points;
[plot_reporting_process()](https://rodrigozepeda.github.io/tbl.now/reference/plot_epidemic_process.md)
for the series they come from;
[`diagnostic_plot()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnostic_plot.md)
for the whole gallery.

## Examples

``` r
data(denguedat)
dn <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)
plot_transport_discriminant(dn)
```
