# Detect batch reporting (backlog dumps) on the report-date axis

**\[experimental\]**

## Usage

``` r
detect_report_batches(
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
  strata = NULL
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

## Value

A tibble with one row per candidate report date (`n_reports > 0`),
carrying `report_date` (and `strata` when grouped), the features
(`n_reports`, `n_event_dates`, `mean_delay`, `median_delay`,
`max_delay`, `backlog`), the robust scores (`score_volume`,
`score_delay`, `score_span`, `score_gap`) and the `batch` flag.

## Details

Laboratories sometimes withhold results and then release a whole backlog
at once. Operationally such a **batch** is a *report date* that is
anomalous on the report axis. This scans each report-date period and
flags batches using up to four robust signals, of which you choose the
combination to require:

- `"volume"` — unusually many cases reported that period (robust z of
  the report count vs a rolling median/MAD baseline).

- `"delay"` — the reports that period have unusually **long** delays (a
  cleared backlog). **This is the signal that distinguishes a batch from
  an epidemic peak**: an epidemic peak also inflates the volume, but its
  cases are still reported with the normal, short delay distribution, so
  its `delay` score stays low.

- `"span"` — the period covers an unusually large number of **distinct
  event dates**.

- `"gap"` — the period is preceded by a run of unusually **low**
  reporting (the "silence then dump" cadence).

A report date is flagged when **all** the activated `signals` fire (each
score at or above `threshold`). Requiring `"delay"` (or `"span"`)
alongside `"volume"` is what keeps epidemic peaks from being flagged.

## See also

[`plot_report_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_report_batches.md)

## Examples

``` r
data(denguedat)
dengue <- tbl_now(denguedat,
  event_date = "onset_week", report_date = "report_week", verbose = FALSE
)
batches <- detect_report_batches(dengue)
batches[batches$batch, ]
#> # A tibble: 2 × 12
#>   report_date n_reports n_event_dates mean_delay median_delay max_delay backlog
#>   <date>          <dbl>         <dbl>      <dbl>        <dbl>     <dbl>   <dbl>
#> 1 1993-09-27         65             8       2.38            2         8      22
#> 2 1996-02-12         46             6       2.98            3         6      26
#> # ℹ 5 more variables: score_volume <dbl>, score_delay <dbl>, score_span <dbl>,
#> #   score_gap <dbl>, batch <lgl>
```
