# Diagnose a `tbl_now`

**\[experimental\]**

`diagnose()` is a structural health check. It looks for the things that
make a nowcast wrong before any model is fitted – dates out of order,
missing values, repeated rows, units that disagree, data after `now`,
event dates too recent to be complete – and returns them as a tibble of
findings, sorted worst first.

It is **deterministic and runs no statistical test.** Whether the
reporting delay drifts, and whether reports arrive in batches, are
questions about a *distribution*, not about the object's structure;
`diagnose()` emits a `"not_run"` signpost naming the function to call
instead
([`diagnose_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_drift.md),
[`diagnose_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches.md))
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
  `c("declarations", "ordering", "missing", "duplicates", "units", "negatives", "now", "truncation", "strata", "signposts")`.
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
  `"truncation"`, `"strata"` or `"signposts"`.

- `scope`:

  What the row is about: a column name, a time axis, a pair of axes, or
  `"all"`.

- `stratum`:

  Which subset of the data the row describes: `"all"` for the pooled
  rows, or the stratum label otherwise.

- `status`:

  An **ordered factor**, worst first, so the tibble sorts itself:
  `error` \> `warning` \> `note` \> `ok` \> `not_run` \> `skipped`. See
  the section below.

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

- `not_run`:

  A signpost: this question needs a statistical test, and `message`
  names the call that answers it.

- `skipped`:

  Could not be assessed – no confirmation process, the wrong data type,
  or an optional package that is not installed.

## See also

[nowcast_diagnose_components](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_diagnose_components.md)
for the individual blocks;
[summary()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_summary.md)
for the descriptive counterpart – what is in the data rather than what
is wrong with it;
[`validate_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md)
for the same findings raised as errors and warnings;
[`diagnostic_plot()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnostic_plot.md)
for the picture version. The [*Describing and diagnosing a tbl_now*
article](https://rodrigozepeda.github.io/tbl.now/articles/describing-and-diagnosing.html)
goes through the findings one at a time.

## Examples

``` r
data(denguedat)
ndata <- tbl_now(denguedat,
  event_date = "onset_week",
  report_date = "report_week",
  strata = "gender",
  verbose = FALSE
)

# Everything, worst first
diagnose(ndata)
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

# Only what needs acting on
diagnose(ndata) |> dplyr::filter(status <= "note")
#> # A tibble: 9 × 10
#>   check     scope stratum status n_affected n_total     prop message hint  rows 
#>   <chr>     <chr> <chr>   <ord>       <dbl>   <dbl>    <dbl> <chr>   <chr> <lis>
#> 1 now       now_… Female  note            3      NA NA       "The l… "Eve… <int>
#> 2 now       now_… Male    note            3      NA NA       "The l… "Eve… <int>
#> 3 now       now_… all     note            3      NA NA       "The l… "Eve… <int>
#> 4 now       now_… Male    note            1      NA NA       "The l… "Eve… <int>
#> 5 strata    size  Male    note        26395   52987  4.98e-1 "The s…  NA   <int>
#> 6 strata    spar… Female  note           13    1095  1.19e-2 "The s… "A s… <int>
#> 7 truncati… even… Female  note            1    1082  9.24e-4 "1 eve… "Thi… <int>
#> 8 truncati… even… Male    note            1    1082  9.24e-4 "1 eve… "Thi… <int>
#> 9 truncati… even… all     note            1    1091  9.17e-4 "1 eve… "Thi… <int>

# One block on its own
diagnose(ndata, checks = "units")
#> # A tibble: 4 × 10
#>   check scope       stratum status n_affected n_total  prop message  hint  rows 
#>   <chr> <chr>       <chr>   <ord>       <dbl>   <dbl> <dbl> <chr>    <chr> <lis>
#> 1 units declared    all     ok              0       2     0 "The de… NA    <int>
#> 2 units delay       all     ok              0   52987     0 "Every … NA    <int>
#> 3 units event_grid  all     ok              0   52987     0 "\"onse… NA    <int>
#> 4 units report_grid all     ok              0   52987     0 "\"repo… NA    <int>

## `diagnose()` never stops your pipeline -- it hands back a table for you to
## read. Use validate_tbl_now() when you want a broken object to be an error.
nrow(diagnose(ndata))
#> [1] 32
```
