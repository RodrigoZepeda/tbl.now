# Visualise batch reporting over the report-date axis

**\[experimental\]**

## Usage

``` r
plot_report_batches(
  x,
  ...,
  signals = c("volume", "delay"),
  threshold = 3,
  backlog_delay = NULL,
  backlog_quantile = 0.9,
  baseline_window = NULL,
  gap_window = 3,
  min_reports = 1,
  by_strata = FALSE,
  strata = NULL,
  palette = .tbl_now_palette()
)
```

## Arguments

- x:

  A `tbl_now` object.

- ...:

  Unused.

- signals:

  Character vector of the signals to require (AND-ed). Any of
  `"volume"`, `"delay"`, `"span"`, `"gap"`. Defaults to
  `c("volume", "delay")`.

- threshold:

  Robust-z cutoff a signal must reach to fire (default `3`).

- backlog_delay:

  Delay at/above which a report counts as backlog (for the `backlog`
  column). `NULL` (default) uses the `backlog_quantile` of the observed
  delays.

- backlog_quantile:

  Delay quantile used to set `backlog_delay` when it is `NULL` (default
  `0.9`).

- baseline_window:

  Rolling-baseline width in report periods. `NULL` (default) uses
  roughly two months.

- gap_window:

  Number of preceding periods assessed by the `"gap"` signal (default
  `3`).

- min_reports:

  Minimum reports for a period to be eligible (default `1`).

- by_strata:

  Logical (default `FALSE`). Detect batches separately per stratum.

- strata:

  Character vector of columns to group on when `by_strata = TRUE`.
  `NULL` (default) uses the object's `strata`.

- palette:

  A named colour palette (defaults to the package palette).

## Value

A ggplot2 object.

## Details

Two stacked panels over the report date: the **reporting volume** and
the **mean reporting delay** of each report date, with the batches
detected by
[`detect_report_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/detect_report_batches.md)
marked. A batch shows up as a spike in *both* the volume and the delay
(a backlog of old cases cleared at once), which is what separates it
from an epidemic peak (a volume spike with a normal, short delay).

## See also

[`detect_report_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/detect_report_batches.md)

## Examples

``` r
data(denguedat)
dengue <- tbl_now(denguedat,
  event_date = "onset_week", report_date = "report_week", verbose = FALSE
)
plot_report_batches(dengue)
```
