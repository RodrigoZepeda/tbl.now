# Plot the reporting "V"

**\[experimental\]**

## Usage

``` r
plot_reporting_v(
  x,
  max_delay = NULL,
  point_size = NULL,
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

  Largest delay to draw. `NULL` (default) fills *every* delay that could
  have been observed (0 up to `now` minus the earliest event).

- point_size:

  Size of the cell markers. `NULL` (default) scales it to the number of
  cells.

- plotly:

  If `TRUE`, return an interactive plotly widget instead of a static
  plot. Default `FALSE`.

- palette:

  A named colour palette. Defaults to the package palette.

## Value

A ggplot2 object (or a plotly widget when `plotly = TRUE`).

## Details

The **same information** as
[`plot_reporting_triangle()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_reporting_triangle.md)
– every (event date, delay) cell of the reporting triangle – drawn in
**rotated coordinates**, so that report date runs straight up the page
and the data opens into a *V*. Where
[`plot_reporting_triangle()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_reporting_triangle.md)
uses square axes (event date across, delay up), this view rotates them
45 degrees:

- the vertical axis is `event + delay` = the **report date** (the V
  opens upward over report time);

- the **left arm** (delay 0, where event = report) is the **event-date**
  axis;

- the **right arm** (the earliest event) is the **delay** axis, reading
  `0` outward;

- every **horizontal** line is one report date.

A **batch** – a single report date releasing a pile of old cases – is
then a single **horizontal streak** across the V, where the reporting
triangle would show it as a diagonal. The whole observable triangle is
filled: every (event date, delay) that *could* have been reported by
`now` is drawn, pale blue where nothing was reported (a genuine zero)
and coloured where reports landed.

## See also

[`plot_reporting_triangle()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_reporting_triangle.md)
for the same data on square axes,
[`diagnostic_plot()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnostic_plot.md).

## Examples

``` r
data(denguedat)
dn <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)
plot_reporting_v(dn)

```
