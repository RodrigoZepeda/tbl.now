# Cases at a chosen point in the reporting process

**\[stable\]**

The same event date has more than one count, depending on when you look.
A week of dengue onsets might show 12 cases the day reporting starts, 40
a week later, and 47 once everything has arrived. These functions let
you pick which of those numbers you want.

## Usage

``` r
get_latest_reported_cases(x, type = "total")

get_initial_reported_cases(x, type = "total")

get_nth_reported_cases(x, delay, type = "total")
```

## Arguments

- x:

  A `tbl_now` object.

- type:

  Which cases to count. One of:

  `"total"`

  :   (default) every case, whatever the outcome. On the revision axis
      that means every case that has been settled at all.

  `"confirmed"`, `"retracted"`, `"pending"`

  :   only the cases with that outcome. `"pending"` is a reporting-axis
      question only – a pending case has no revision date – and the
      revision getters refuse it.

  `"unknown"`

  :   the cases whose `revision_type` is `NA`: settled, but the data
      does not say which way.

  `"net"`

  :   confirmed **minus** retracted – the running total as a
      surveillance system publishes it, which can go **down** when a
      case is withdrawn. This is the quantity a `count-cumulative`
      stream actually reports, and the one diseasenowcasting's
      cumulative signed-change likelihood is built for.

  `"by_type"`

  :   one row per outcome instead of one number: the outcome column
      joins the keys, so you get pending, confirmed and retracted side
      by side.

  On an object with no revision process anything but `"total"` warns and
  pools, because there is no outcome to filter on.

- delay:

  A single non-negative number (or `Inf`) giving the maximum reporting
  delay, in event units, to include. Only used by
  `get_nth_reported_cases()`.

## Value

A `count-cumulative` `tbl_now` with one row per event date (and stratum,
and grouping column), containing:

- the event-date column – when the cases happened. Its numeric version
  is `.event_num`.

- the report-date column – the report that was selected for that event
  date. Its numeric version is `.report_num`.

- `n` – the number of cases reported for that event date at the selected
  point.

- `.delay` – the delay of the selected report.

- any strata, covariate, censoring indicator and temporal-effect columns
  the object carried, plus the caller's grouping columns.

The **revision** columns are not carried: the count pools over many
revision dates, so the result has no single one and does not pretend to.
`type = "by_type"` is the exception – it keeps the outcome column,
declared as a covariate, because that is the whole point of the call and
an undeclared column is one
[`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md)
would pool away. Use
[get_latest_revised_cases()](https://rodrigozepeda.github.io/tbl.now/reference/revised_cases.md)
when you want the third date on the result.

## Details

- `get_initial_reported_cases()` – the count as **first** seen: the
  earliest report for that event date. This is what a dashboard would
  have shown you at the time, and it is always an undercount.

- `get_latest_reported_cases()` – the count as **latest** seen: the most
  recent report. This is the current best estimate of what really
  happened, and it is what you score a nowcast against.

- `get_nth_reported_cases()` – the count accumulated **within a given
  delay**. **\[stable\]** `delay = 0` gives the cases reported on the
  event date itself, `delay = 1` adds those reported one period later,
  and so on. `delay = Inf` is the same as `get_latest_reported_cases()`.

The gap between the first and the latest count *is* the reporting delay
problem that nowcasting exists to solve.

## Grouping is respected

Unlike
[`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md),
these functions **keep the caller's grouping** and answer by it: the
grouping columns join the event date and the strata as keys, and come
back on the result. That is what lets you ask for the latest count by a
**covariate** – a column that matters but is not something you nowcast
by – which grouping is the only way to express.

They can do this because they *select* a point in the process rather
than reshaping the object: one row in is still one case (or one cell)
out.
[`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md)
cannot, and warns instead.

## See also

[get_latest_revised_cases()](https://rodrigozepeda.github.io/tbl.now/reference/revised_cases.md)
and friends for the same idea on the revision process;
[`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md)
for the underlying data shapes;
[`score_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/score_nowcast.md),
which uses the latest counts as truth.

## Examples

``` r
data(denguedat)
# The last five years. The counters work the same on the full twenty-year
# series, they just have more weeks to walk.
recent <- denguedat[denguedat$onset_week >= as.Date("2006-01-01"), ]
dengue <- tbl_now(recent,
  report_date = "report_week",
  event_date = "onset_week",
  strata = "gender",
  verbose = FALSE
)

# What the surveillance system showed the very first time it reported each
# week -- an undercount, because the late reports had not arrived yet.
first <- get_initial_reported_cases(dengue)
first
#> # A tibble:  505 × 7
#> # Data type: "count-cumulative"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week   .event_num .report_num gender         n .delay
#>    <date>       <date>             <dbl>       <dbl> <chr>      <dbl>  <dbl>
#>    [event_date] [report_date]      [...]       [...] [strata] [cases]  [...]
#>  1 2006-01-02   2006-01-09             0           1 Female         1      1
#>  2 2006-01-02   2006-01-09             0           1 Male           7      1
#>  3 2006-01-09   2006-01-16             1           2 Female         2      1
#>  4 2006-01-09   2006-01-16             1           2 Male           5      1
#>  5 2006-01-16   2006-01-16             2           2 Female         3      0
#>  6 2006-01-16   2006-01-16             2           2 Male           1      0
#>  7 2006-01-23   2006-01-23             3           3 Female         2      0
#>  8 2006-01-23   2006-01-30             3           4 Male           4      1
#>  9 2006-01-30   2006-02-06             4           5 Female         1      1
#> 10 2006-01-30   2006-01-30             4           4 Male           1      0
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "onset_week" | Report date: "report_week"
#> # Strata: "gender"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 495 more rows

# What it shows now, after all the corrections.
latest <- get_latest_reported_cases(dengue)
latest
#> # A tibble:  505 × 7
#> # Data type: "count-cumulative"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week   .event_num .report_num gender         n .delay
#>    <date>       <date>             <dbl>       <dbl> <chr>      <dbl>  <dbl>
#>    [event_date] [report_date]      [...]       [...] [strata] [cases]  [...]
#>  1 2006-01-02   2006-01-23             0           3 Female         9      3
#>  2 2006-01-02   2006-01-30             0           4 Male          10      4
#>  3 2006-01-09   2006-01-16             1           2 Female         2      1
#>  4 2006-01-09   2006-02-13             1           6 Male           9      5
#>  5 2006-01-16   2006-02-13             2           6 Female        11      4
#>  6 2006-01-16   2006-01-30             2           4 Male          11      2
#>  7 2006-01-23   2006-02-06             3           5 Female        16      2
#>  8 2006-01-23   2006-02-06             3           5 Male           8      2
#>  9 2006-01-30   2006-02-27             4           8 Female         3      4
#> 10 2006-01-30   2006-02-06             4           5 Male           6      1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "onset_week" | Report date: "report_week"
#> # Strata: "gender"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 495 more rows

# The difference between them is what a nowcast tries to predict.
sum(latest$n) - sum(first$n)
#> [1] 11278

# Everything known within two weeks of onset.
get_nth_reported_cases(dengue, delay = 2)
#> # A tibble:  500 × 7
#> # Data type: "count-cumulative"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week   .event_num .report_num gender         n .delay
#>    <date>       <date>             <dbl>       <dbl> <chr>      <dbl>  <dbl>
#>    [event_date] [report_date]      [...]       [...] [strata] [cases]  [...]
#>  1 2006-01-02   2006-01-16             0           2 Female         6      2
#>  2 2006-01-02   2006-01-16             0           2 Male           9      2
#>  3 2006-01-09   2006-01-16             1           2 Female         2      1
#>  4 2006-01-09   2006-01-23             1           3 Male           8      2
#>  5 2006-01-16   2006-01-30             2           4 Female        10      2
#>  6 2006-01-16   2006-01-30             2           4 Male          11      2
#>  7 2006-01-23   2006-02-06             3           5 Female        16      2
#>  8 2006-01-23   2006-02-06             3           5 Male           8      2
#>  9 2006-01-30   2006-02-13             4           6 Female         2      2
#> 10 2006-01-30   2006-02-06             4           5 Male           6      1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "onset_week" | Report date: "report_week"
#> # Strata: "gender"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 490 more rows

# A grouping is answered by, not dropped.
dengue |>
  dplyr::group_by(gender) |>
  get_latest_reported_cases() |>
  dplyr::group_vars()
#> [1] "gender"
```
