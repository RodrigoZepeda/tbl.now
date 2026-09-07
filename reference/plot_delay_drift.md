# Visualise whether the reporting-delay distribution drifts over time

**\[experimental\]**

Draws a **rolling fan chart** of the count-weighted reporting-delay
distribution indexed by **event date**: a solid line for the rolling
median, a dashed line for the rolling mean, and two shaded bands (the
25-75% and 10-90% quantile ranges). Reading it left to right answers "do
delays drift?" — a rising/falling centre line is *location* drift,
widening/narrowing bands are *spread* drift.

## Usage

``` r
plot_delay_drift(
  x,
  ...,
  window = NULL,
  step = NULL,
  min_n = 1,
  by_strata = FALSE,
  strata = NULL,
  changepoint = FALSE,
  level = 0.95,
  plotly = FALSE,
  axis = c("report", "revision"),
  linewidth = 1,
  grid_linewidth = 0.5,
  palette = .tbl_now_palette()
)
```

## Arguments

- x:

  A `tbl_now` object.

- ...:

  Unused.

- window:

  Rolling-window width, in event-time **periods**. `NULL` (default) uses
  `7` periods regardless of the time unit — that is, 7 days for daily
  data and 7 weeks for weekly data.

- step:

  Step between window centres, in periods. `NULL` (default) uses
  `max(1, window / 4)`.

- min_n:

  Minimum total case count for a window to be drawn (default `1`).

- by_strata:

  Logical (default `FALSE`). When `TRUE`, one fan is drawn per stratum
  (faceted).

- strata:

  Character vector of columns to group on when `by_strata = TRUE`.
  `NULL` (default) uses the object's `strata`.

- changepoint:

  Logical (default `FALSE`). When `TRUE`, mark the estimated abrupt
  change point of the **median** delay (Pettitt's test, on mature data)
  with a vertical line, when one is detected (p \< 0.05). See
  [`diagnose_changepoint()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_changepoint.md).

- level:

  Completeness level for the immature-region shading (default `0.95`;
  see
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)).

- plotly:

  If `TRUE`, return an interactive plotly widget instead of a static
  plot. Default `FALSE`.

- axis:

  Which time axis the delay is measured to: `"report"` (default) or
  `"revision"`. Report-axis delays are measured from event to report;
  revision-axis delays are measured from report to revision, the same
  quantity as `.revision_delay`. Needs a revision process (see
  [`add_revision_date()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md));
  cases still `"pending"` are left out.

- linewidth:

  Multiplier on the width of the mean and median delay lines. Default
  `1` (drawn at `0.6` and `0.8`, so the median stays the heavier of the
  two at any setting).

- grid_linewidth:

  Line width of the dashed maturity line and of the `changepoint` marker
  – the reference lines the package draws itself, not ggplot2's panel
  grid. Default `0.5`.

- palette:

  A named colour palette (see
  [`tbl_now_palette()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_palette.md)).

## Value

A ggplot2 object.

## Details

Because recent event dates have not had time to be fully reported, their
delay summaries are downward-biased (only short delays are observable
yet). That immature region — event dates after the `level`
incompleteness cutoff — is **shaded grey** and should not be read as
drift. Pair the plot with
[`diagnose_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_drift.md)
for a formal test.

## See also

[`diagnose_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_drift.md)
for the formal trend test behind this picture, and
[`diagnose_changepoint()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_changepoint.md)
for an abrupt shift rather than a gradual one;
[`plot_delay_distribution()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_distribution.md)
for the delay pooled over the whole period;
[autoplot()](https://rodrigozepeda.github.io/tbl.now/reference/autoplot.tbl_now.md)
and
[`diagnostic_plot()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnostic_plot.md)
for the galleries this belongs to.

## Examples

``` r
data(denguedat)
dengue <- tbl_now(denguedat,
  event_date = "onset_week", report_date = "report_week", verbose = FALSE
)
plot_delay_drift(dengue)
```
