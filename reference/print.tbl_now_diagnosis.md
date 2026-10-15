# Print a `tbl_now` diagnosis

**\[stable\]**

Prints the findings
[`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
returned as a report: the errors, warnings and notes in full, each with
its hint, and the checks that passed and that could not be assessed as
one line each.

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
# The last five years. The full twenty-year series gives the same shape
# of answer, it just takes longer to compute.
recent <- denguedat[denguedat$onset_week >= as.Date("2006-01-01"), ]
ndata <- tbl_now(recent,
  event_date = "onset_week", report_date = "report_week",
  strata = "gender", verbose = FALSE
)

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

# Every finding, including the ones that passed.
print(diagnose(ndata), all = TRUE)
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
#> Passed (18)
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
#> ✔ simultaneously missing/event and report dates: No simultaneously missing values in the event and report date columns "onset_week" and "report_week".
#> ✔ truncation/event_date [Female]: Every event date is old enough to be complete.
#> ✔ truncation/event_date [Male]: Every event date is old enough to be complete.
#> ✔ truncation/event_date: Every event date is old enough to be complete.
#> ✔ units/declared: The declared units agree: "weeks" and "weeks".
#> ✔ units/delay: Every `.delay` is a whole number of units.
#> ✔ units/event_grid: "onset_week" lands on the object's "weeks" grid.
#> ✔ units/report_grid: "report_week" lands on the object's "weeks" grid.
#> 
#> Skipped (5)
#> ─ duplicates/key: A line list is one row per case, so identical rows are two cases rather than a repeat.
#> ─ negatives/count: A line list has no count column to go negative.
#> ─ ordering/event_to_revision: The object carries no revision process.
#> ─ ordering/report_to_revision: The object carries no revision process.
#> ─ strata/pending: The object carries no revision process.
#> 
#> ℹ 29 findings. Use `dplyr::filter()` or `tibble::as_tibble()` for the table.

# Still a tibble.
print(tibble::as_tibble(diagnose(ndata)))
#> # A tibble: 29 × 10
#>    check     scope stratum status n_affected n_total    prop message hint  rows 
#>    <chr>     <chr> <chr>   <ord>       <dbl>   <dbl>   <dbl> <chr>   <chr> <lis>
#>  1 now       now_… Female  note            3      NA NA      "The l… Ever… <int>
#>  2 now       now_… Male    note            3      NA NA      "The l… Ever… <int>
#>  3 now       now_… all     note            3      NA NA      "The l… Ever… <int>
#>  4 now       now_… Male    note            1      NA NA      "The l… Ever… <int>
#>  5 strata    size  Female  note         6998   14135  0.495  "The s… NA    <int>
#>  6 strata    spar… Female  note            8     260  0.0308 "The s… A st… <int>
#>  7 declarat… temp… all     ok              0       0 NA      "0 tem… NA    <int>
#>  8 declarat… unde… all     ok              0       6  0      "Every… NA    <int>
#>  9 missing   gend… all     ok              0   14135  0      "No mi… NA    <int>
#> 10 missing   onse… all     ok              0   14135  0      "No mi… NA    <int>
#> # ℹ 19 more rows
```
