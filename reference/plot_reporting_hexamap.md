# Plot the reporting triangle as an age-period-cohort hexamap

**\[experimental\]**

Draws the reporting triangle as a hexagonal age-period-cohort map, using
the projection of Jalal and Burke (2020). Event date, report date and
reporting delay are the cohort, period and age of the map
(`report = event + delay`), and each `(event, delay)` cell is one
hexagon coloured by its report count. Because a batch is a single
**report date**, it appears as a clean **vertical stripe**; the
fast-reporting bulk sits along the short-delay bottom edge.

## Usage

``` r
plot_reporting_hexamap(
  x,
  max_delay = NULL,
  complete = FALSE,
  iso = NULL,
  iso_minor = NULL,
  format = "%d/%b/%y",
  max_cells = 12000L,
  trans = "sqrt",
  palette = .tbl_now_palette()
)
```

## Arguments

- x:

  A
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
  object.

- max_delay:

  Largest delay (in report units) to draw. `NULL` (default) shows the
  observed range, auto-capped to respect `max_cells`.

- complete:

  If `TRUE`, fill the whole observable triangle with zeros so the grey
  background shows every observable cell. Default `FALSE` (observed
  cells only). Coerces linelist input to counts via
  [`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md).

- iso, iso_minor:

  Major and minor grid spacings (in report units). `NULL` picks sensible
  defaults from the data.

- format:

  Date format for the event/report tick labels (see
  [`strftime()`](https://rdrr.io/r/base/strptime.html)). Default
  `"%d/%b/%y"`.

- max_cells:

  Safety cap on the number of hexagons. Default `12000`.

- trans:

  Fill transform for the count scale. Default `"sqrt"`.

- palette:

  A named colour palette. Defaults to the package palette.

## Value

A ggplot2 object.

## Details

The three axes are read off three families of iso-lines: **report date**
(period) runs vertically, **delay** (age) up the right-hand spine, and
**event date** (cohort) up the left. A major/minor triangular grid is
drawn so any hexagon can be traced back to its event date, report date
and delay.

The number of hexagons is `#\{observed (event, delay) cells\}`, which
grows with the delay range. To stay responsive the delay axis is capped
so at most `max_cells` hexagons are drawn (raise `max_cells`, or set
`max_delay`, to change this). `complete = TRUE` first fills the whole
observable triangle with explicit zeros (via
[`complete_zeroes()`](https://rodrigozepeda.github.io/tbl.now/reference/complete_zeroes.md))
so the empty cells are shown in grey.

## References

Jalal, H. and Burke, D. S. (2020). Hexamaps for Age-Period-Cohort Data
Visualization. *Epidemiology* **31**, e47-e49.

## See also

[`plot_reporting_triangle()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_reporting_triangle.md),
[`diagnostic_plot()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnostic_plot.md).

## Examples

``` r
data(denguedat)
dn <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)
plot_reporting_hexamap(dn)

```
