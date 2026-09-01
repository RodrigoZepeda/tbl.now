# Print a `tbl_now` diagnosis

**\[experimental\]**

Prints the findings
[`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
returned as a report: the errors, warnings and notes in full, each with
its hint, and the checks that passed, that were deliberately not run,
and that could not be assessed as one line each.

The object is an ordinary tibble underneath, so
`print(tibble::as_tibble(x))` gives the table and every `dplyr` verb
still works on it.

## Usage

``` r
# S3 method for class 'tbl_now_diagnosis'
print(x, ..., all = FALSE)
```

## Arguments

- x:

  A findings tibble, from
  [`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
  or one of the
  [nowcast_diagnose_components](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_diagnose_components.md).

- ...:

  Unused.

- all:

  Logical. Spell out the `ok` and `skipped` findings too, instead of
  counting them. Defaults to `FALSE`, and to `TRUE` when there is
  nothing else to report – a block that found nothing wrong would
  otherwise print an empty report.

## Value

`x`, invisibly.

## See also

[`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md),
[nowcast_diagnose_components](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_diagnose_components.md)

## Examples

``` r
data(denguedat)
ndata <- tbl_now(denguedat,
  event_date = "onset_week", report_date = "report_week",
  strata = "gender", verbose = FALSE
)

diagnose(ndata)
#> ── Diagnosis of a <tbl_now> ────────────────────────────────────────────────────
#> 9 notes, 14 passed, 3 not run, 6 skipped.
#> 
#> Notes (9)
#> ℹ now/now_gap_event [Female]: The last event date is 3 weeks before now ("2010-12-20").
#>   → Everything in that window is still arriving; it is what a nowcast is for, and it is also what makes the last points of any plot look like a decline.
#> ℹ now/now_gap_event [Male]: The last event date is 3 weeks before now ("2010-12-20").
#> ℹ now/now_gap_event: The last event date is 3 weeks before now ("2010-12-20").
#> ℹ now/now_gap_report [Male]: The last report date is 1 week before now ("2010-12-20").
#> ℹ strata/size [Male]: The smallest stratum is "Male" with 26395 cases, 49.8% of the total.
#> ℹ strata/sparsity [Female]: The sparsest stratum is "Female": 1.2% of the event dates on the grid carry no cases at all.
#>   → A stratum that is mostly zeros is the one a per-stratum fit will struggle with; pooling it is often better than fitting it.
#> ℹ truncation/event_date [Female]: 1 event date is younger than the 95th percentile of the delay, so its counts are still filling in; an estimated 5.8% of its eventual total has not arrived.
#>   → This is right-truncation, and it is the reason to nowcast rather than a defect. Cut the series at "2010-11-22" to describe it instead.
#> ℹ truncation/event_date [Male]: 1 event date is younger than the 95th percentile of the delay, so its counts are still filling in; an estimated 5.9% of its eventual total has not arrived.
#> ℹ truncation/event_date: 1 event date is younger than the 95th percentile of the delay, so its counts are still filling in; an estimated 5.9% of its eventual total has not arrived.
#> 
#> Not run (3)
#> → signposts/report: Run: diagnose_drift(x, axis = "report")
#>   → `diagnose()` runs no statistical test: a trend test needs a method, a maturity window and an alpha, and those are the caller's to choose.
#> → signposts/report_batches: Run: diagnose_batches(x, axis = "report")
#>   → `diagnose()` runs no statistical test: batch detection needs a look-back, a null model and a multiplicity correction.
#> → signposts/validation_batches: Run: diagnose_batches(x, axis = "validation")
#> 
#> ✔ 14 passed: declarations/temporal_effects, declarations/undeclared, missing/gender, missing/onset_week, missing/report_week, now/event_date, now/now_gap_report, now/report_date, ordering/event_to_report, units/declared, units/delay, units/event_grid, and units/report_grid
#> ─ 6 skipped: duplicates/key, negatives/count, ordering/event_to_validation, ordering/report_to_validation, signposts/validation, and strata/pending
#> 
#> ℹ 32 findings. Use `dplyr::filter()` or `tibble::as_tibble()` for the table.

# Every finding, including the ones that passed.
print(diagnose(ndata), all = TRUE)
#> ── Diagnosis of a <tbl_now> ────────────────────────────────────────────────────
#> 9 notes, 14 passed, 3 not run, 6 skipped.
#> 
#> Notes (9)
#> ℹ now/now_gap_event [Female]: The last event date is 3 weeks before now ("2010-12-20").
#>   → Everything in that window is still arriving; it is what a nowcast is for, and it is also what makes the last points of any plot look like a decline.
#> ℹ now/now_gap_event [Male]: The last event date is 3 weeks before now ("2010-12-20").
#> ℹ now/now_gap_event: The last event date is 3 weeks before now ("2010-12-20").
#> ℹ now/now_gap_report [Male]: The last report date is 1 week before now ("2010-12-20").
#> ℹ strata/size [Male]: The smallest stratum is "Male" with 26395 cases, 49.8% of the total.
#> ℹ strata/sparsity [Female]: The sparsest stratum is "Female": 1.2% of the event dates on the grid carry no cases at all.
#>   → A stratum that is mostly zeros is the one a per-stratum fit will struggle with; pooling it is often better than fitting it.
#> ℹ truncation/event_date [Female]: 1 event date is younger than the 95th percentile of the delay, so its counts are still filling in; an estimated 5.8% of its eventual total has not arrived.
#>   → This is right-truncation, and it is the reason to nowcast rather than a defect. Cut the series at "2010-11-22" to describe it instead.
#> ℹ truncation/event_date [Male]: 1 event date is younger than the 95th percentile of the delay, so its counts are still filling in; an estimated 5.9% of its eventual total has not arrived.
#> ℹ truncation/event_date: 1 event date is younger than the 95th percentile of the delay, so its counts are still filling in; an estimated 5.9% of its eventual total has not arrived.
#> 
#> Passed (14)
#> ✔ declarations/temporal_effects: 0 temporal effect columns are materialised.
#> ✔ declarations/undeclared: Every column is declared or protected.
#> ✔ missing/gender: No missing values in the stratum column "gender".
#> ✔ missing/onset_week: No missing values in the event_date column "onset_week".
#> ✔ missing/report_week: No missing values in the report_date column "report_week".
#> ✔ now/event_date: No event is dated after now.
#> ✔ now/now_gap_report [Female]: The last report date is 0 weeks before now ("2010-12-20").
#> ✔ now/now_gap_report: The last report date is 0 weeks before now ("2010-12-20").
#> ✔ now/report_date: now is on or after the last report.
#> ✔ ordering/event_to_report: Every report is on or after its event.
#> ✔ units/declared: The declared units agree: "weeks" and "weeks".
#> ✔ units/delay: Every `.delay` is a whole number of units.
#> ✔ units/event_grid: "onset_week" lands on the object's "weeks" grid.
#> ✔ units/report_grid: "report_week" lands on the object's "weeks" grid.
#> 
#> Not run (3)
#> → signposts/report: Run: diagnose_drift(x, axis = "report")
#>   → `diagnose()` runs no statistical test: a trend test needs a method, a maturity window and an alpha, and those are the caller's to choose.
#> → signposts/report_batches: Run: diagnose_batches(x, axis = "report")
#>   → `diagnose()` runs no statistical test: batch detection needs a look-back, a null model and a multiplicity correction.
#> → signposts/validation_batches: Run: diagnose_batches(x, axis = "validation")
#> 
#> Skipped (6)
#> ─ duplicates/key: A line list is one row per case, so identical rows are two cases rather than a repeat.
#> ─ negatives/count: A line list has no count column to go negative.
#> ─ ordering/event_to_validation: The object carries no validation process.
#> ─ ordering/report_to_validation: The object carries no validation process.
#> ─ signposts/validation: The object carries no validation process.
#> ─ strata/pending: The object carries no validation process.
#> 
#> ℹ 32 findings. Use `dplyr::filter()` or `tibble::as_tibble()` for the table.

# Still a tibble.
print(tibble::as_tibble(diagnose(ndata)))
#> # A tibble: 32 × 10
#>    check    scope stratum status n_affected n_total     prop message hint  rows 
#>    <chr>    <chr> <chr>   <ord>       <dbl>   <dbl>    <dbl> <chr>   <chr> <lis>
#>  1 now      now_… Female  note            3      NA NA       "The l… "Eve… <int>
#>  2 now      now_… Male    note            3      NA NA       "The l… "Eve… <int>
#>  3 now      now_… all     note            3      NA NA       "The l… "Eve… <int>
#>  4 now      now_… Male    note            1      NA NA       "The l… "Eve… <int>
#>  5 strata   size  Male    note        26395   52987  4.98e-1 "The s…  NA   <int>
#>  6 strata   spar… Female  note           13    1095  1.19e-2 "The s… "A s… <int>
#>  7 truncat… even… Female  note            1    1082  9.24e-4 "1 eve… "Thi… <int>
#>  8 truncat… even… Male    note            1    1082  9.24e-4 "1 eve… "Thi… <int>
#>  9 truncat… even… all     note            1    1091  9.17e-4 "1 eve… "Thi… <int>
#> 10 declara… temp… all     ok              0       0 NA       "0 tem…  NA   <int>
#> # ℹ 22 more rows
```
