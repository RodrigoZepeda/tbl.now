# Reported cases at a chosen point in the reporting process

**\[stable\]**

The same event date has more than one count, depending on when you look.
A week of dengue onsets might show 12 cases the day reporting starts, 40
a week later, and 47 once everything has arrived. These functions let
you pick which of those numbers you want.

## Usage

``` r
get_latest_reported_cases(x)

get_initial_reported_cases(x)

get_nth_reported_cases(x, delay)
```

## Arguments

- x:

  A `tbl_now` object.

- delay:

  A single non-negative number (or `Inf`) giving the maximum reporting
  delay, in report units, to include. Only used by
  `get_nth_reported_cases()`.

## Value

A `count-cumulative` `tbl_now` with one row per event date (and
stratum), containing:

- the event-date column – when the cases happened. Its numeric version
  is `.event_num`.

- the report-date column – the report that was selected for that event
  date. Its numeric version is `.report_num`.

- `n` – the number of cases reported for that event date at the selected
  point.

- `.delay` – the delay of the selected report.

- any strata, censoring indicator and temporal-effect columns the object
  carried.

## Details

- `get_initial_reported_cases()` – the count as **first** seen: the
  earliest report for that event date. This is what a dashboard would
  have shown you at the time, and it is always an undercount.

- `get_latest_reported_cases()` – the count as **latest** seen: the most
  recent report. This is the current best estimate of what really
  happened, and it is what you score a nowcast against.

- `get_nth_reported_cases()` – the count accumulated **within a given
  delay**. **\[experimental\]** `delay = 0` gives the cases reported on
  the event date itself, `delay = 1` adds those reported one period
  later, and so on. `delay = Inf` is the same as
  `get_latest_reported_cases()`.

The gap between the first and the latest count *is* the reporting delay
problem that nowcasting exists to solve.

## See also

[get_latest_confirmed()](https://rodrigozepeda.github.io/tbl.now/reference/confirmation_counts.md)
and friends for the same idea on the confirmation process;
[`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md)
for the underlying data shapes;
[`score_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/score_nowcast.md),
which uses the latest counts as truth;
[reporting_completeness()](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md)
for the same information as a proportion.

## Examples

``` r
data(denguedat)
dengue <- tbl_now(denguedat,
  report_date = "report_week",
  event_date = "onset_week",
  strata = "gender",
  verbose = FALSE
)

# What the surveillance system showed the very first time it reported each
# week -- an undercount, because the late reports had not arrived yet.
first <- get_initial_reported_cases(dengue)
first
#> # A tibble:  2,164 × 7
#> # Data type: "count-cumulative"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week   .event_num .report_num gender         n .delay
#>    <date>       <date>             <dbl>       <dbl> <chr>      <int>  <dbl>
#>    [event_date] [report_date]      [...]       [...] [strata] [cases]  [...]
#>  1 1990-01-01   1990-01-01             0           0 Female         2      0
#>  2 1990-01-01   1990-01-01             0           0 Male           1      0
#>  3 1990-01-08   1990-01-08             1           1 Female         1      0
#>  4 1990-01-08   1990-01-08             1           1 Male           1      0
#>  5 1990-01-15   1990-01-15             2           2 Female         2      0
#>  6 1990-01-15   1990-01-15             2           2 Male           4      0
#>  7 1990-01-22   1990-01-22             3           3 Female         5      0
#>  8 1990-01-22   1990-01-22             3           3 Male           3      0
#>  9 1990-01-29   1990-01-29             4           4 Female         3      0
#> 10 1990-01-29   1990-01-29             4           4 Male           1      0
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "onset_week" | Report date: "report_week"
#> # Strata: "gender"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 2,154 more rows

# What it shows now, after all the corrections.
latest <- get_latest_reported_cases(dengue)
latest
#> # A tibble:  2,164 × 7
#> # Data type: "count-cumulative"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week   .event_num .report_num gender         n .delay
#>    <date>       <date>             <dbl>       <dbl> <chr>      <int>  <dbl>
#>    [event_date] [report_date]      [...]       [...] [strata] [cases]  [...]
#>  1 1990-01-01   1990-03-05             0           9 Female        39      9
#>  2 1990-01-01   1990-02-12             0           6 Male          22      6
#>  3 1990-01-08   1990-02-05             1           5 Female        25      4
#>  4 1990-01-08   1990-02-12             1           6 Male          25      5
#>  5 1990-01-15   1990-03-05             2           9 Female        21      7
#>  6 1990-01-15   1990-02-12             2           6 Male          23      4
#>  7 1990-01-22   1990-02-19             3           7 Female        24      4
#>  8 1990-01-22   1990-03-19             3          11 Male          22      8
#>  9 1990-01-29   1990-03-19             4          11 Female        21      7
#> 10 1990-01-29   1990-03-12             4          10 Male          18      6
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "onset_week" | Report date: "report_week"
#> # Strata: "gender"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 2,154 more rows

# The difference between them is what a nowcast tries to predict.
sum(latest$n) - sum(first$n)
#> [1] 42691

# Everything known within two weeks of onset.
get_nth_reported_cases(dengue, delay = 2)
#> # A tibble:  2,151 × 7
#> # Data type: "count-cumulative"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week   .event_num .report_num gender         n .delay
#>    <date>       <date>             <dbl>       <dbl> <chr>      <int>  <dbl>
#>    [event_date] [report_date]      [...]       [...] [strata] [cases]  [...]
#>  1 1990-01-01   1990-01-15             0           2 Female        31      2
#>  2 1990-01-01   1990-01-15             0           2 Male          19      2
#>  3 1990-01-08   1990-01-22             1           3 Female        21      2
#>  4 1990-01-08   1990-01-22             1           3 Male          20      2
#>  5 1990-01-15   1990-01-29             2           4 Female        14      2
#>  6 1990-01-15   1990-01-29             2           4 Male          22      2
#>  7 1990-01-22   1990-02-05             3           5 Female        18      2
#>  8 1990-01-22   1990-02-05             3           5 Male          20      2
#>  9 1990-01-29   1990-02-12             4           6 Female        19      2
#> 10 1990-01-29   1990-02-12             4           6 Male          12      2
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "onset_week" | Report date: "report_week"
#> # Strata: "gender"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 2,141 more rows
```
