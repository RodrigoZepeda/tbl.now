# Diagnose a `tbl_now`

**\[experimental\]**

`diagnose()` is a structural health check. It looks for the things that
make a nowcast wrong before any model is fitted – dates out of order,
missing values, repeated rows, units that disagree, data after `now`,
event dates too recent to be complete – and returns them as a tibble of
findings, sorted worst first.

It is **deterministic and runs no statistical test.** Whether the
reporting delay drifts, and whether reports arrive in batches, are
questions about a *distribution*, not about the object's structure, so
`diagnose()` leaves them to
[`diagnose_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_drift.md)
and
[`diagnose_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches.md)
rather than quietly running a test whose method, window and multiplicity
correction you did not choose.

Every block is also available on its own – see
[nowcast_diagnose_components](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_diagnose_components.md)
– and `diagnose()` is exactly the
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
of those pieces.

## Usage

``` r
diagnose(x, ...)

# Default S3 method
diagnose(x, ...)

# S3 method for class 'tbl_now'
diagnose(
  x,
  ...,
  checks = NULL,
  by_strata = NULL,
  strata = NULL,
  warn_non_uniqueness = TRUE
)
```

## Arguments

- x:

  A `tbl_now` object.

- ...:

  Unused, for extensibility.

- checks:

  Character vector of checks to run, a subset of
  `c("declarations", "ordering", "missing", "duplicates", "units", "negatives", "now", "truncation", "strata")`.
  Defaults to all of them.

- by_strata:

  Logical. Add one set of rows per stratum, for the checks that are
  naturally per-stratum (missingness, negative increments,
  right-truncation, the gap to `now`). Defaults to `TRUE` when the
  object has strata. The checks that are statements about the object as
  a whole (declarations, units, duplicates, ordering) are always
  reported once, with `stratum = "all"`.

- strata:

  Character vector of columns to stratify by. Defaults to
  `get_strata(x)`.

- warn_non_uniqueness:

  Logical. Run the duplicate-row check. Defaults to `TRUE` here, unlike
  [`validate_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md),
  where it defaults to `FALSE` because it runs on every `dplyr` verb.

## Value

A tibble with the columns described above, sorted worst first.

## The columns

Every function in this family returns the same schema, so results can be
stacked with
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
and filtered with
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html).

- `check`:

  Which block the row belongs to: `"declarations"`, `"ordering"`,
  `"missing"`, `"duplicates"`, `"units"`, `"negatives"`, `"now"`,
  `"truncation"` or `"strata"`.

- `scope`:

  What the row is about: a column name, a time axis, a pair of axes, or
  `"all"`.

- `stratum`:

  Which subset of the data the row describes: `"all"` for the pooled
  rows, or the stratum label otherwise.

- `status`:

  An **ordered factor**, worst first, so the tibble sorts itself:
  `error` \> `warning` \> `note` \> `ok` \> `skipped`. See the section
  below.

- `n_affected`:

  How many rows (or cases, or dates) the finding is about.

- `n_total`:

  How many were considered.

- `prop`:

  `n_affected / n_total`.

- `message`:

  One human sentence, already formatted.

- `hint`:

  What to do about it, or `NA`.

- `rows`:

  A list-column of offending row indices, so `x[result$rows[[1]], ]`
  goes straight to the bad rows. Empty when the finding is not about
  particular rows, or when it was computed on a de-accumulated view
  whose rows are not the object's own.

## What the statuses mean

- `error`:

  [`validate_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md)
  aborts on this. The object is not a usable `tbl_now`.

- `warning`:

  [`validate_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md)
  warns about this.

- `note`:

  A `diagnose()`-only observation worth your attention. It is
  deliberately never promoted to a warning:
  [`validate_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md)
  runs on every `dplyr` verb, and a new warning there would turn a quiet
  construction into a noisy one for data that has always been accepted.

- `ok`:

  The check ran and found nothing.

- `skipped`:

  Could not be assessed – no revision process, the wrong data type, or
  an optional package that is not installed.

## See also

[nowcast_diagnose_components](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_diagnose_components.md)
for the individual blocks;
[summary()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_summary.md)
for the descriptive counterpart – what is in the data rather than what
is wrong with it;
[`validate_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md)
for the same findings raised as errors and warnings;
[`diagnostic_plot()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnostic_plot.md)
for the picture version. The [*Diagnosing a tbl_now*
article](https://rodrigozepeda.github.io/tbl.now/articles/diagnosing-a-tbl-now.html)
goes through the findings one at a time.

## Examples

``` r
data(denguedat)
# The last five years. The full twenty-year series gives the same shape
# of answer, it just takes longer to compute.
recent <- denguedat[denguedat$onset_week >= as.Date("2006-01-01"), ]
ndata <- tbl_now(recent,
  event_date = "onset_week",
  report_date = "report_week",
  strata = "gender",
  verbose = FALSE
)

# Everything, worst first
diagnose(ndata)
#> ── Diagnosis of a <tbl_now> ────────────────────────────────────────────────────
#> 6 notes, 18 passed, 5 skipped.
#> 
#> Notes (6)
#> ℹ now/now_gap_event [Female]: The last event date is 3 weeks before now ("2010-12-20").
#>   → Everything in that window is still arriving; it is what a nowcast is for, and it is also what makes the last points of any plot look like a decline.
#> ℹ now/now_gap_event [Male]: The last event date is 3 weeks before now ("2010-12-20").
#> ℹ now/now_gap_event: The last event date is 3 weeks before now ("2010-12-20").
#> ℹ now/now_gap_report [Male]: The last report date is 1 week before now ("2010-12-20").
#> ℹ strata/size [Female]: The smallest stratum is "Female" with 6998 cases, 49.5% of the total.
#> ℹ strata/sparsity [Female]: The sparsest stratum is "Female": 8 of the 260 weeks between the minimum event (2006-01-02) and the now (2010-12-20) carry no cases at all (3.1%, against 1.2% pooled over every stratum).
#>   → A stratum that is mostly zeros is the one a per-stratum fit will struggle with; pooling it is often better than fitting it. When every stratum is mostly zeros the grid is finer than the data -- `aggregate_time_units()` coarsens it.
#> 
#> ✔ 18 passed: declarations/temporal_effects, declarations/undeclared, missing/gender, missing/onset_week, missing/report_week, now/event_date, now/now_gap_report, now/report_date, ordering/event_to_report, simultaneously missing/event and report dates, truncation/event_date, units/declared, units/delay, units/event_grid, and units/report_grid
#> ─ 5 skipped: duplicates/key, negatives/count, ordering/event_to_revision, ordering/report_to_revision, and strata/pending
#> 
#> ℹ 29 findings. Use `dplyr::filter()` or `tibble::as_tibble()` for the table.

# Only what needs acting on
diagnose(ndata) |> dplyr::filter(status <= "note")
#> ── Diagnosis of a <tbl_now> ────────────────────────────────────────────────────
#> 6 notes.
#> 
#> Notes (6)
#> ℹ now/now_gap_event [Female]: The last event date is 3 weeks before now ("2010-12-20").
#>   → Everything in that window is still arriving; it is what a nowcast is for, and it is also what makes the last points of any plot look like a decline.
#> ℹ now/now_gap_event [Male]: The last event date is 3 weeks before now ("2010-12-20").
#> ℹ now/now_gap_event: The last event date is 3 weeks before now ("2010-12-20").
#> ℹ now/now_gap_report [Male]: The last report date is 1 week before now ("2010-12-20").
#> ℹ strata/size [Female]: The smallest stratum is "Female" with 6998 cases, 49.5% of the total.
#> ℹ strata/sparsity [Female]: The sparsest stratum is "Female": 8 of the 260 weeks between the minimum event (2006-01-02) and the now (2010-12-20) carry no cases at all (3.1%, against 1.2% pooled over every stratum).
#>   → A stratum that is mostly zeros is the one a per-stratum fit will struggle with; pooling it is often better than fitting it. When every stratum is mostly zeros the grid is finer than the data -- `aggregate_time_units()` coarsens it.
#> 
#> ℹ 6 findings. Use `dplyr::filter()` or `tibble::as_tibble()` for the table.

# One block on its own
diagnose(ndata, checks = "units")
#> ── Diagnosis of a <tbl_now> ────────────────────────────────────────────────────
#> 4 passed.
#> 
#> Passed (4)
#> ✔ units/declared: The declared units agree: "weeks" and "weeks".
#> ✔ units/delay: Every `.delay` is a whole number of units.
#> ✔ units/event_grid: "onset_week" lands on the object's "weeks" grid.
#> ✔ units/report_grid: "report_week" lands on the object's "weeks" grid.
#> 
#> ℹ 4 findings. Use `dplyr::filter()` or `tibble::as_tibble()` for the table.

## `diagnose()` never stops your pipeline -- it hands back a table for you to
## read. Use validate_tbl_now() when you want a broken object to be an error.
nrow(diagnose(ndata))
#> [1] 29
```
