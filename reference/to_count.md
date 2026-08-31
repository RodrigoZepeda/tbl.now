# Convert between linelist and aggregated count data

**\[stable\]**

Surveillance data comes in three shapes, and different nowcasting
packages want different ones. `to_count()` moves a
[`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
between them:

- **`linelist`** – one row per case. The most detailed shape.

- **`count-incidence`** – one row per (event date, report date) pair,
  holding the number of cases reported *on exactly that report date*.

- **`count-cumulative`** – the same grid, but holding the number of
  cases known *up to and including* that report date. This is the shape
  most public dashboards publish.

You can go from `linelist` to either count shape, and back and forth
between the two count shapes. You cannot go back to `linelist`: once
cases have been added up, the individual rows are gone.

## Usage

``` r
to_count(x, to = NULL, ...)

# S3 method for class 'tbl_now'
to_count(x, to = NULL, ...)
```

## Arguments

- x:

  A
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
  object to convert.

- to:

  Character. The data type to produce: `"linelist"`, `"count-incidence"`
  or `"count-cumulative"`. Defaults to the object's current type, i.e.
  no change.

- ...:

  Additional arguments passed to methods.

## Value

A `tbl_now` object of the requested `to` data type, with the counts
aggregated into the `case_count` column.

## Details

This is an S3 generic. The package provides a method for `tbl_now`
objects, which aggregates into the `case_count` column, creating one
named `n` when the object does not already have one.

Aggregation sums over every column the object has *not* been told about,
so a column you care about should be declared as a strata or covariate
first (see
[`add_strata()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md))
or it will be summed away.

## Note

`linelist` data cannot be reconstructed from `count-*` data. Asking for
it throws an error, because aggregated data cannot be un-counted.

## Statistical details

Converting `count-cumulative` to `count-incidence` **de-accumulates**
the series: within each event date (and grouping), ordered by report
date, the increment is that cumulative total minus the previous one.
Because published cumulative totals are sometimes revised *downward*, an
increment can be **negative**. That is not a bug – it is a retraction
showing through – but code that requires non-negative counts (for
example
[`tbl_now_to_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_baselinenowcast.md))
must handle or refuse it.

## See also

[`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
and its *Data types* section for what each shape means;
[`get_data_type()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
to ask an object which shape it currently is;
[`complete_zeroes()`](https://rodrigozepeda.github.io/tbl.now/reference/complete_zeroes.md)
to fill in the (event, report) pairs where nothing was reported;
[`get_latest_reported_cases()`](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.md)
to pull out the most recent counts.

## Examples

``` r
data(denguedat)
ndata <- tbl_now(denguedat,
  event_date = "onset_week",
  report_date = "report_week",
  strata = "gender"
)
#> ℹ Identified data as <linelist-data> where each observation is a test.

# A linelist has one row per case ...
nrow(ndata)
#> [1] 52987

## ... which becomes one row per (onset week, report week, gender), with the
# number of cases in `n`.
counts <- to_count(ndata, to = "count-incidence")
counts
#> # A tibble:  8,265 × 7
#> # Data type: "count-incidence"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week   .event_num .report_num gender         n .delay
#>    <date>       <date>             <dbl>       <dbl> <chr>      <int>  <dbl>
#>    [event_date] [report_date]      [...]       [...] [strata] [cases]  [...]
#>  1 1990-01-01   1990-01-01             0           0 Female         2      0
#>  2 1990-01-01   1990-01-08             0           1 Female        13      1
#>  3 1990-01-01   1990-01-15             0           2 Female        16      2
#>  4 1990-01-01   1990-01-22             0           3 Female         7      3
#>  5 1990-01-01   1990-03-05             0           9 Female         1      9
#>  6 1990-01-01   1990-01-01             0           0 Male           1      0
#>  7 1990-01-01   1990-01-08             0           1 Male          11      1
#>  8 1990-01-01   1990-01-15             0           2 Male           7      2
#>  9 1990-01-01   1990-01-22             0           3 Male           1      3
#> 10 1990-01-01   1990-01-29             0           4 Male           1      4
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "onset_week" | Report date: "report_week"
#> # Strata: "gender"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 8,255 more rows

# Cumulative totals instead: how many cases for that onset week were known by
# each report week. Within an onset week these only ever go up.
to_count(counts, to = "count-cumulative")
#> # A tibble:  8,265 × 7
#> # Data type: "count-cumulative"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week   .event_num .report_num gender         n .delay
#>    <date>       <date>             <dbl>       <dbl> <chr>      <int>  <dbl>
#>    [event_date] [report_date]      [...]       [...] [strata] [cases]  [...]
#>  1 1990-01-01   1990-01-01             0           0 Female         2      0
#>  2 1990-01-01   1990-01-08             0           1 Female        15      1
#>  3 1990-01-01   1990-01-15             0           2 Female        31      2
#>  4 1990-01-01   1990-01-22             0           3 Female        38      3
#>  5 1990-01-01   1990-03-05             0           9 Female        39      9
#>  6 1990-01-01   1990-01-01             0           0 Male           1      0
#>  7 1990-01-01   1990-01-08             0           1 Male          12      1
#>  8 1990-01-01   1990-01-15             0           2 Male          19      2
#>  9 1990-01-01   1990-01-22             0           3 Male          20      3
#> 10 1990-01-01   1990-01-29             0           4 Male          21      4
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "onset_week" | Report date: "report_week"
#> # Strata: "gender"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 8,255 more rows

# Going back to a linelist is impossible -- the individual cases are gone.
try(to_count(counts, to = "linelist"))
#> Error in to_count(counts, to = "linelist") : 
#>   Transformation from `data_type` count-incidence to linelist not
#> implemented
```
