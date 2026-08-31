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

- `diagnose_ordering()` – the `event <= report <= confirmation`
  timeline.

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
  confirmations still pending.

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
[*Describing and diagnosing a tbl_now*
article](https://rodrigozepeda.github.io/tbl.now/articles/describing-and-diagnosing.html)
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
#> # A tibble: 2 × 10
#>   check        scope stratum status n_affected n_total  prop message hint  rows 
#>   <chr>        <chr> <chr>   <ord>       <dbl>   <dbl> <dbl> <chr>   <chr> <lis>
#> 1 declarations temp… all     ok              0       0    NA 0 temp… NA    <int>
#> 2 declarations unde… all     ok              0       6     0 Every … NA    <int>
diagnose_ordering(ndata)
#> # A tibble: 3 × 10
#>   check    scope     stratum status n_affected n_total  prop message hint  rows 
#>   <chr>    <chr>     <chr>   <ord>       <dbl>   <dbl> <dbl> <chr>   <chr> <lis>
#> 1 ordering event_to… all     ok              0   52987     0 Every … NA    <int>
#> 2 ordering event_to… all     skipp…         NA      NA    NA The ob… NA    <int>
#> 3 ordering report_t… all     skipp…         NA      NA    NA The ob… NA    <int>
diagnose_units(ndata)
#> # A tibble: 4 × 10
#>   check scope       stratum status n_affected n_total  prop message  hint  rows 
#>   <chr> <chr>       <chr>   <ord>       <dbl>   <dbl> <dbl> <chr>    <chr> <lis>
#> 1 units declared    all     ok              0       2     0 "The de… NA    <int>
#> 2 units delay       all     ok              0   52987     0 "Every … NA    <int>
#> 3 units event_grid  all     ok              0   52987     0 "\"onse… NA    <int>
#> 4 units report_grid all     ok              0   52987     0 "\"repo… NA    <int>
diagnose_now(ndata)
#> # A tibble: 8 × 10
#>   check scope        stratum status n_affected n_total  prop message hint  rows 
#>   <chr> <chr>        <chr>   <ord>       <dbl>   <dbl> <dbl> <chr>   <chr> <lis>
#> 1 now   now_gap_eve… Female  note            3      NA    NA "The l… Ever… <int>
#> 2 now   now_gap_eve… Male    note            3      NA    NA "The l… Ever… <int>
#> 3 now   now_gap_eve… all     note            3      NA    NA "The l… Ever… <int>
#> 4 now   now_gap_rep… Male    note            1      NA    NA "The l… Ever… <int>
#> 5 now   event_date   all     ok              0   52987     0 "No ev… NA    <int>
#> 6 now   now_gap_rep… Female  ok              0      NA    NA "The l… Ever… <int>
#> 7 now   now_gap_rep… all     ok              0      NA    NA "The l… Ever… <int>
#> 8 now   report_date  all     ok              0       1     0 "now i… NA    <int>

# Is anything missing, repeated, negative, or cut off at the recent edge?
diagnose_missing(ndata)
#> # A tibble: 3 × 10
#>   check   scope      stratum status n_affected n_total  prop message hint  rows 
#>   <chr>   <chr>      <chr>   <ord>       <dbl>   <dbl> <dbl> <chr>   <chr> <lis>
#> 1 missing gender     all     ok              0   52987     0 "No mi… NA    <int>
#> 2 missing onset_week all     ok              0   52987     0 "No mi… NA    <int>
#> 3 missing report_we… all     ok              0   52987     0 "No mi… NA    <int>
diagnose_duplicates(ndata)
#> # A tibble: 1 × 10
#>   check      scope stratum status  n_affected n_total  prop message  hint  rows 
#>   <chr>      <chr> <chr>   <ord>        <dbl>   <dbl> <dbl> <chr>    <chr> <lis>
#> 1 duplicates key   all     skipped         NA      NA    NA A line … NA    <int>
diagnose_negatives(ndata)
#> # A tibble: 1 × 10
#>   check     scope stratum status  n_affected n_total  prop message   hint  rows 
#>   <chr>     <chr> <chr>   <ord>        <dbl>   <dbl> <dbl> <chr>     <chr> <lis>
#> 1 negatives count all     skipped         NA      NA    NA A line l… NA    <int>
diagnose_truncation(ndata)
#> # A tibble: 3 × 10
#>   check      scope stratum status n_affected n_total    prop message hint  rows 
#>   <chr>      <chr> <chr>   <ord>       <dbl>   <dbl>   <dbl> <chr>   <chr> <lis>
#> 1 truncation even… Female  note            1    1082 9.24e-4 1 even… "Thi… <int>
#> 2 truncation even… Male    note            1    1082 9.24e-4 1 even… "Thi… <int>
#> 3 truncation even… all     note            1    1091 9.17e-4 1 even… "Thi… <int>

# Are the strata usable, and which statistical tests does the data call for?
diagnose_strata(ndata)
#> # A tibble: 3 × 10
#>   check  scope    stratum status  n_affected n_total    prop message hint  rows 
#>   <chr>  <chr>    <chr>   <ord>        <dbl>   <dbl>   <dbl> <chr>   <chr> <lis>
#> 1 strata size     Male    note         26395   52987  0.498  "The s… NA    <int>
#> 2 strata sparsity Female  note            13    1095  0.0119 "The s… A st… <int>
#> 3 strata pending  all     skipped         NA      NA NA      "The o… NA    <int>
diagnose_signposts(ndata)
#> # A tibble: 4 × 10
#>   check     scope    stratum status n_affected n_total  prop message hint  rows 
#>   <chr>     <chr>    <chr>   <ord>       <dbl>   <dbl> <dbl> <chr>   <chr> <lis>
#> 1 signposts confirm… all     not_r…         NA      NA    NA "Run: … `dia… <int>
#> 2 signposts report   all     not_r…         NA      NA    NA "Run: … `dia… <int>
#> 3 signposts report_… all     not_r…         NA      NA    NA "Run: … `dia… <int>
#> 4 signposts confirm… all     skipp…         NA      NA    NA "The o… NA    <int>

## Each returns the same schema, so they stack the way diagnose() stacks them.
dplyr::bind_rows(
  diagnose_units(ndata),
  diagnose_now(ndata)
)
#> # A tibble: 12 × 10
#>    check scope       stratum status n_affected n_total  prop message hint  rows 
#>    <chr> <chr>       <chr>   <ord>       <dbl>   <dbl> <dbl> <chr>   <chr> <lis>
#>  1 units declared    all     ok              0       2     0 "The d… NA    <int>
#>  2 units delay       all     ok              0   52987     0 "Every… NA    <int>
#>  3 units event_grid  all     ok              0   52987     0 "\"ons… NA    <int>
#>  4 units report_grid all     ok              0   52987     0 "\"rep… NA    <int>
#>  5 now   now_gap_ev… Female  note            3      NA    NA "The l… Ever… <int>
#>  6 now   now_gap_ev… Male    note            3      NA    NA "The l… Ever… <int>
#>  7 now   now_gap_ev… all     note            3      NA    NA "The l… Ever… <int>
#>  8 now   now_gap_re… Male    note            1      NA    NA "The l… Ever… <int>
#>  9 now   event_date  all     ok              0   52987     0 "No ev… NA    <int>
#> 10 now   now_gap_re… Female  ok              0      NA    NA "The l… Ever… <int>
#> 11 now   now_gap_re… all     ok              0      NA    NA "The l… Ever… <int>
#> 12 now   report_date all     ok              0       1     0 "now i… NA    <int>
```
