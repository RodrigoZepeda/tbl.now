# Individual blocks of a `tbl_now` diagnosis

**\[experimental\]**

Each function returns one block of
[`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md),
in the same schema, so they can be stacked with
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
or used on their own.

- `diagnose_declarations()` – the attributes and the columns they name:
  types, existence, collisions, columns the object was never told about,
  and temporal effects that were added but never materialised.

- `diagnose_ordering()` – the `event <= report <= validation` timeline.

- `diagnose_missing()` – `NA` values, per column and per stratum. An
  `NA` *count* is reported neutrally: in a reporting triangle it means
  *not yet observed*, which is correct data rather than a defect.

- `diagnose_duplicates()` – rows that repeat on the full key.

- `diagnose_units()` – the declared units against each other, against
  the calendar the dates actually land on, and against the delay they
  produce.

- `diagnose_negatives()` – negative counts, and the negative increments
  a downward revision leaves behind when cumulative data is
  de-accumulated.

- `diagnose_now()` – anything dated after `now`, and how stale the
  object is.

- `diagnose_truncation()` – how many recent event dates are still
  immature, and how much of their eventual total is probably still
  missing.

- `diagnose_strata()` – the smallest and the sparsest stratum, and the
  validations still pending.

- `diagnose_signposts()` – the questions
  [`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
  deliberately does not answer, and the call that answers each one.

## Usage

``` r
diagnose_declarations(x, by_strata = NULL, strata = NULL)

diagnose_ordering(x, by_strata = NULL, strata = NULL)

diagnose_missing(x, by_strata = NULL, strata = NULL)

diagnose_duplicates(
  x,
  by_strata = NULL,
  strata = NULL,
  warn_non_uniqueness = TRUE
)

diagnose_units(x, by_strata = NULL, strata = NULL)

diagnose_negatives(x, by_strata = NULL, strata = NULL)

diagnose_now(x, by_strata = NULL, strata = NULL)

diagnose_truncation(x, by_strata = NULL, strata = NULL)

diagnose_strata(x, by_strata = NULL, strata = NULL)

diagnose_signposts(x, by_strata = NULL, strata = NULL)
```

## Arguments

- x:

  A `tbl_now` object.

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

A tibble in the schema documented in
[`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md).

## See also

[`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md),
which stacks all of these and sorts them worst-first;
[`validate_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md)
for the same findings raised as errors and warnings;
[nowcast_summary_components](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md)
for what *is* in the data rather than what is wrong with it;
[`diagnose_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_drift.md),
[`diagnose_changepoint()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_changepoint.md)
and
[`diagnose_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches.md)
for the statistical tests `diagnose_signposts()` points you at. The
[*Diagnosing a tbl_now*
article](https://rodrigozepeda.github.io/tbl.now/articles/diagnosing-a-tbl-now.html)
explains how to read each finding.

## Examples

``` r
data(denguedat)
ndata <- tbl_now(denguedat,
  event_date = "onset_week",
  report_date = "report_week",
  strata = "gender",
  verbose = FALSE
)

# Is the object described correctly, and do the dates make sense?
diagnose_declarations(ndata)
#> ── Diagnosis of a <tbl_now> ────────────────────────────────────────────────────
#> 2 passed.
#> 
#> Passed (2)
#> ✔ declarations/temporal_effects: 0 temporal effect columns are materialised.
#> ✔ declarations/undeclared: Every column is declared or protected.
#> 
#> ℹ 2 findings. Use `dplyr::filter()` or `tibble::as_tibble()` for the table.
diagnose_ordering(ndata)
#> ── Diagnosis of a <tbl_now> ────────────────────────────────────────────────────
#> 1 passed, 2 skipped.
#> 
#> Passed (1)
#> ✔ ordering/event_to_report: Every report is on or after its event.
#> 
#> Skipped (2)
#> ─ ordering/event_to_validation: The object carries no validation process.
#> ─ ordering/report_to_validation: The object carries no validation process.
#> 
#> ℹ 3 findings. Use `dplyr::filter()` or `tibble::as_tibble()` for the table.
diagnose_units(ndata)
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
diagnose_now(ndata)
#> ── Diagnosis of a <tbl_now> ────────────────────────────────────────────────────
#> 4 notes, 4 passed.
#> 
#> Notes (4)
#> ℹ now/now_gap_event [Female]: The last event date is 3 weeks before now ("2010-12-20").
#>   → Everything in that window is still arriving; it is what a nowcast is for, and it is also what makes the last points of any plot look like a decline.
#> ℹ now/now_gap_event [Male]: The last event date is 3 weeks before now ("2010-12-20").
#> ℹ now/now_gap_event: The last event date is 3 weeks before now ("2010-12-20").
#> ℹ now/now_gap_report [Male]: The last report date is 1 week before now ("2010-12-20").
#> 
#> ✔ 4 passed: now/event_date, now/now_gap_report, and now/report_date
#> 
#> ℹ 8 findings. Use `dplyr::filter()` or `tibble::as_tibble()` for the table.

# Is anything missing, repeated, negative, or cut off at the recent edge?
diagnose_missing(ndata)
#> ── Diagnosis of a <tbl_now> ────────────────────────────────────────────────────
#> 3 passed.
#> 
#> Passed (3)
#> ✔ missing/gender: No missing values in the stratum column "gender".
#> ✔ missing/onset_week: No missing values in the event_date column "onset_week".
#> ✔ missing/report_week: No missing values in the report_date column "report_week".
#> 
#> ℹ 3 findings. Use `dplyr::filter()` or `tibble::as_tibble()` for the table.
diagnose_duplicates(ndata)
#> ── Diagnosis of a <tbl_now> ────────────────────────────────────────────────────
#> 1 skipped.
#> 
#> Skipped (1)
#> ─ duplicates/key: A line list is one row per case, so identical rows are two cases rather than a repeat.
#> 
#> ℹ 1 finding. Use `dplyr::filter()` or `tibble::as_tibble()` for the table.
diagnose_negatives(ndata)
#> ── Diagnosis of a <tbl_now> ────────────────────────────────────────────────────
#> 1 skipped.
#> 
#> Skipped (1)
#> ─ negatives/count: A line list has no count column to go negative.
#> 
#> ℹ 1 finding. Use `dplyr::filter()` or `tibble::as_tibble()` for the table.
diagnose_truncation(ndata)
#> ── Diagnosis of a <tbl_now> ────────────────────────────────────────────────────
#> 3 notes.
#> 
#> Notes (3)
#> ℹ truncation/event_date [Female]: 1 event date is younger than the 95th percentile of the delay, so its counts are still filling in; an estimated 5.8% of its eventual total has not arrived.
#>   → This is right-truncation, and it is the reason to nowcast rather than a defect. Cut the series at "2010-11-22" to describe it instead.
#> ℹ truncation/event_date [Male]: 1 event date is younger than the 95th percentile of the delay, so its counts are still filling in; an estimated 5.9% of its eventual total has not arrived.
#> ℹ truncation/event_date: 1 event date is younger than the 95th percentile of the delay, so its counts are still filling in; an estimated 5.9% of its eventual total has not arrived.
#> 
#> ℹ 3 findings. Use `dplyr::filter()` or `tibble::as_tibble()` for the table.

# Are the strata usable, and which statistical tests does the data call for?
diagnose_strata(ndata)
#> ── Diagnosis of a <tbl_now> ────────────────────────────────────────────────────
#> 2 notes, 1 skipped.
#> 
#> Notes (2)
#> ℹ strata/size [Male]: The smallest stratum is "Male" with 26395 cases, 49.8% of the total.
#> ℹ strata/sparsity [Female]: The sparsest stratum is "Female": 1.2% of the event dates on the grid carry no cases at all.
#>   → A stratum that is mostly zeros is the one a per-stratum fit will struggle with; pooling it is often better than fitting it.
#> 
#> ─ 1 skipped: strata/pending
#> 
#> ℹ 3 findings. Use `dplyr::filter()` or `tibble::as_tibble()` for the table.
diagnose_signposts(ndata)
#> ── Diagnosis of a <tbl_now> ────────────────────────────────────────────────────
#> 3 not run, 1 skipped.
#> 
#> Not run (3)
#> → signposts/report: Run: diagnose_drift(x, axis = "report")
#>   → `diagnose()` runs no statistical test: a trend test needs a method, a maturity window and an alpha, and those are the caller's to choose.
#> → signposts/report_batches: Run: diagnose_batches(x, axis = "report")
#>   → `diagnose()` runs no statistical test: batch detection needs a look-back, a null model and a multiplicity correction.
#> → signposts/validation_batches: Run: diagnose_batches(x, axis = "validation")
#> 
#> ─ 1 skipped: signposts/validation
#> 
#> ℹ 4 findings. Use `dplyr::filter()` or `tibble::as_tibble()` for the table.

## Each returns the same schema, so they stack the way diagnose() stacks them.
dplyr::bind_rows(
  diagnose_units(ndata),
  diagnose_now(ndata)
)
#> ── Diagnosis of a <tbl_now> ────────────────────────────────────────────────────
#> 4 notes, 8 passed.
#> 
#> Notes (4)
#> ℹ now/now_gap_event [Female]: The last event date is 3 weeks before now ("2010-12-20").
#>   → Everything in that window is still arriving; it is what a nowcast is for, and it is also what makes the last points of any plot look like a decline.
#> ℹ now/now_gap_event [Male]: The last event date is 3 weeks before now ("2010-12-20").
#> ℹ now/now_gap_event: The last event date is 3 weeks before now ("2010-12-20").
#> ℹ now/now_gap_report [Male]: The last report date is 1 week before now ("2010-12-20").
#> 
#> ✔ 8 passed: units/declared, units/delay, units/event_grid, units/report_grid, now/event_date, now/now_gap_report, and now/report_date
#> 
#> ℹ 12 findings. Use `dplyr::filter()` or `tibble::as_tibble()` for the table.
```
