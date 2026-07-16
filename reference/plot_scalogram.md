# Plot the reporting or epidemic scalogram

**\[experimental\]**

## Usage

``` r
plot_scalogram(
  x,
  type = c("reporting", "epidemic"),
  windowrad = 1,
  wname = "PAUL",
  format = "%d/%b/%y",
  plotly = FALSE,
  palette = .tbl_now_palette()
)
```

## Arguments

- x:

  A
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
  object.

- type:

  `"reporting"` (default; reports by report date) or `"epidemic"` (cases
  by event date).

- windowrad:

  Radius of the time window (in report-grid steps). `NULL` lets
  wavScalogram choose.

- wname:

  Mother wavelet passed to wavScalogram (e.g. `"PAUL"`, `"MORLET"`).
  `"PAUL"` localises a batch more sharply in time.

- format:

  Date format for the x-axis tick labels (see
  [`strftime()`](https://rdrr.io/r/base/strptime.html)). Default
  `"%d/%b/%y"`.

- plotly:

  If `TRUE`, return an interactive plotly widget instead of a static
  plot. Default `FALSE`.

- palette:

  A named colour palette. Defaults to the package palette.

## Value

A ggplot2 object (or a plotly widget when `plotly = TRUE`).

## Details

A **wavelet scalogram** splits a count series, at every moment, into
fast wiggles (short periods, at the bottom) and slow swings (long
periods, at the top), and shows the energy at each as a heat map. A
**batch** – a sudden one-step burst of reports – lights up as a bright
**short-period ridge** in the *reporting* scalogram that the *epidemic*
(event) scalogram lacks, since real cases arrive smoothly. Periods are
measured in the object's own time step (days, weeks, ...), so the series
is analysed on its integer grid, not forced to days.

This uses a **window-inner** scalogram (wavScalogram,
`border_effects = "INNER"`): it is computed from the observed data only,
with **no border padding**. That matters for surveillance / nowcasting,
where the usual periodic or zero padding would fabricate structure
exactly at the most recent ("now") edge we care about. The price is that
the estimate near the time edges uses a smaller window (the blank
region), so there is no need to hedge what *is* shown.

## See also

[`plot_reporting_process()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_reporting_process.md),
[`plot_epidemic_process()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_epidemic_process.md),
[`diagnostic_plot()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnostic_plot.md).

## Examples

``` r
data(denguedat)
dn <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)
plot_scalogram(dn, type = "reporting")
```
