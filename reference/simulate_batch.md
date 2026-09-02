# Inject a batch into a `tbl_now` by withholding and then releasing reports

**\[experimental\]**

Simulates a reporting system that is **closed** on a given set of report
dates and releases its accumulated backlog on the next open date.
Reports keep their event dates and merely move *later* on the report
axis, so no cases are created or destroyed – the defining property of a
batch. Useful for checking that
[`diagnose_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches.md)
and
[`diagnose_batch_shape()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batch_shape.md)
recover a batch you planted.

## Usage

``` r
simulate_batch(
  x,
  closed_dates,
  held_fraction = 1,
  drop_unreleased = TRUE,
  verbose = TRUE
)
```

## Arguments

- x:

  A
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
  object.

- closed_dates:

  A vector of report dates on which the reporting system is closed. Must
  be coercible to the class of the report-date column.

- held_fraction:

  Fraction of the reports due on each closed date that are actually
  **held back** (and released later); the rest report on time, so the
  closure is only partial. Default `1` (the whole desk is closed). With,
  say, `held_fraction = 0.5`, roughly half of each closed day's reports
  are held and half report normally. This uses the random number
  generator (Binomial / Bernoulli sampling), so set a seed for
  reproducibility. Only supported for `"linelist"` and
  `"count-incidence"` data (a `"count-cumulative"` total cannot be
  split).

- drop_unreleased:

  Logical; drop reports whose closed run never reopens before the end of
  the report axis. Default `TRUE`.

- verbose:

  Logical; report what was moved. Default `TRUE`.

## Value

A new `tbl_now` with the same event dates, strata and data type, and
modified report dates.

## Details

A batch is a **transport**: it moves an item's report date later while
leaving its event date untouched, creating and destroying nothing. Every
report whose report date lies in `closed_dates` (or, with
`held_fraction < 1`, a random share of them) is re-stamped with the
first report date at or after it that is *not* closed. Consequently:

- the closed dates report nothing (the **deficit**);

- the release date reports its own items *plus* the whole backlog (the
  **spike**);

- items released late have inflated **delays**;

- the release date draws on an unusually large number of distinct
  **event dates**.

All four symptoms come from the one mechanism, which is why they should
not be treated as four independent pieces of evidence.

## Reports that never come back

If a closed run extends to the end of the report axis there is no open
date to release into. Those reports are then unobservable – a stall that
has not yet cleared is indistinguishable from data loss, an honest
identification failure. `drop_unreleased = TRUE` (default) discards
them, reproducing exactly what a real analyst would see.

## Cumulative data

For `"count-cumulative"` data a report announces a running total. When
two reports for the same event date are pushed onto the same release
date, only the later one survives: it is that report date's final word
on the total.

## See also

[`diagnose_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches.md)
and
[`diagnose_batch_shape()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batch_shape.md),
the tests this exists to validate;
[`censor_reporting_delays_above()`](https://rodrigozepeda.github.io/tbl.now/reference/censoring.md)
for recording a real batch rather than planting one. The [*Diagnosing a
tbl_now*
article](https://rodrigozepeda.github.io/tbl.now/articles/diagnosing-a-tbl-now.html)
uses this to calibrate the screen.

## Examples

``` r
data(denguedat)

dengue_tbl <- tbl_now(
  denguedat,
  event_date  = onset_week,
  report_date = report_week,
  data_type   = "linelist",
  verbose     = FALSE
)

# Pretend the reporting desk was shut for three consecutive weeks: everything
# that would have been reported then is held, and released together afterwards.
closed <- as.Date(c("1990-06-04", "1990-06-11", "1990-06-18"))
batched_tbl <- simulate_batch(dengue_tbl, closed_dates = closed, verbose = FALSE)
#> Warning: ! `simulate_batch()` is experimental: results are not guaranteed and the
#>   interface may change.
#> ℹ Treat a flagged report date as a potential batch, not a confirmed one.
#> This warning is displayed once every 8 hours.

# No cases are lost -- they are only moved later in the reporting process.
nrow(dengue_tbl)
#> [1] 52987
nrow(batched_tbl)
#> [1] 52987

# Which is the point: you now have data with a batch you planted yourself, so
# you can check whether the screen finds it.
found <- suppressWarnings(diagnose_batches(batched_tbl, lookback = 2))
found$report_date[found$batch]
#> [1] "1991-08-12" "2007-11-26" "2009-11-16" "2010-09-13"
```
