# Empirical distribution of the reporting or revision delay

**\[stable\]**

The `"delay_distribution"` panel of
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html),
drawn on its own: a case-count weighted histogram of `.delay`. For
**`count-cumulative`** data it becomes the *cumulative growth by delay*
instead — boxplots, on a log scale, of the ratio of each event date's
cumulative count at a delay to its count at the previous delay.

`axis = "revision"` draws the same histogram of `.revision_delay`, the
time from a report to its resolution, in the revision process's colours.
A case still `"pending"` has no resolution, and so no revision delay,
and does not appear.

## Usage

``` r
plot_delay_distribution(
  x,
  axis = c("report", "revision"),
  by_revision_type = TRUE,
  ...
)
```

## Arguments

- x:

  A
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
  object.

- axis:

  Which delay to draw: `"report"` (default), the time from the event to
  the report, or `"revision"`, the time from the report to its
  resolution. `"revision"` needs a revision process (see
  [add_revision_date()](https://rodrigozepeda.github.io/tbl.now/reference/add.md)).

- by_revision_type:

  Logical (default `TRUE`). Split the histogram by how each case
  eventually resolved — `confirmed`, `pending`, `retracted` and
  `unknown`, stacked, in the palette's outcome colours (see
  [`tbl_now_palette()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_palette.md)).
  Whether a negative result comes back faster than a positive one is the
  question the split exists to answer, and
  [`diagnose_revision_delay()`](https://rodrigozepeda.github.io/tbl.now/reference/revision_delay.md)
  is the test of it. Ignored on an object with no revision axis, and
  when `by_strata = TRUE`, which already uses the fill for the strata.

- ...:

  Further arguments passed to
  [`autoplot.tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/autoplot.tbl_now.md),
  e.g. `by_strata`, `strata`, `delay_distribution_xlim`, `plotly` or
  `palette`.

## Value

A ggplot2 object (or a plotly widget when `plotly = TRUE`).

## See also

[`autoplot.tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/autoplot.tbl_now.md),
[`plot_delay_profiles()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_profiles.md),
[`plot_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_drift.md);
[`diagnose_revision_delay()`](https://rodrigozepeda.github.io/tbl.now/reference/revision_delay.md)
for the test behind the outcome split.

## Examples

``` r
data(denguedat)
dengue_now <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)
plot_delay_distribution(dengue_now)


# On the revision axis, split by how each case resolved.
cases <- data.frame(
  onset = as.Date("2021-01-04") + rep(0:9, each = 4),
  visit = as.Date("2021-01-05") + rep(0:9, each = 4),
  result = as.Date("2021-01-05") + rep(0:9, each = 4) +
    rep(c(1, 1, 5, 6), times = 10),
  outcome = rep(c("confirmed", "confirmed", "retracted", "retracted"), times = 10)
)
flu <- tbl_now(cases,
  event_date = onset, report_date = visit,
  revision_date = result, revision_type = outcome,
  data_type = "linelist", verbose = FALSE
)
plot_delay_distribution(flu, axis = "revision")
```
