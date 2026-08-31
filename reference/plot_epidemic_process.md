# The epidemic process and the reporting process

**\[experimental\]**

The same cases, counted on two different clocks. Comparing the two is
the single most useful thing you can do to tell a real outbreak from a
reporting artifact.

- `plot_epidemic_process()` counts by **event date** – when the cases
  actually happened. Epidemics grow and shrink smoothly, so this curve
  should be smooth.

- `plot_reporting_process()` counts by **report date** – when news of
  them arrived. Reporting is administrative, so this curve is spiky:
  weekends, holidays and backlog releases all show up here.

A lone spike in the reporting process with nothing under it in the
epidemic process is a **batch** – a day the system cleared its inbox,
not a day people got sick. A spike in both is a genuine surge.

## Usage

``` r
plot_reporting_process(
  x,
  plotly = FALSE,
  axis = c("report", "confirmation"),
  palette = .tbl_now_palette()
)

plot_epidemic_process(
  x,
  plotly = FALSE,
  axis = c("report", "confirmation"),
  palette = .tbl_now_palette()
)
```

## Arguments

- x:

  A
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
  object.

- plotly:

  If `TRUE`, return an interactive plotly widget (hover, zoom) instead
  of a static ggplot2 plot. Default `FALSE`.

- axis:

  Which time axis to draw: `"report"` (default) or `"confirmation"`. On
  the confirmation axis the picture answers the laboratory's version of
  the question – when results arrived, rather than when reports did.
  Needs a confirmation process (see
  [add_confirmation()](https://rodrigozepeda.github.io/tbl.now/reference/confirmation_setters.md));
  cases still `"pending"` have no confirmation date and are left out.

- palette:

  A named colour palette. Defaults to the package palette.

## Value

A ggplot2 object (or a plotly widget when `plotly = TRUE`).

## Details

Both are facetted by stratum when the object has strata.

## See also

[`diagnostic_plot()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnostic_plot.md),
which draws these alongside the rest of the reporting-process gallery;
[`plot_observed_cases()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_observed_cases.md)
for the epidemic process with the incompleteness cutoff marked;
[`plot_scalogram()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_scalogram.md)
to separate the two processes by timescale;
[`diagnose_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches.md)
to test a suspicious spike rather than eyeball it.

## Examples

``` r
data(denguedat)
dn <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)

# When cases happened: smooth, because epidemics are.
plot_epidemic_process(dn)


# When news of them arrived: spikier, because reporting is administrative.
plot_reporting_process(dn)

```
