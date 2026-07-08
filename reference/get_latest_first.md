# Get the latest / first / nth-delay reported cases for each event date

**\[stable\]**

## Usage

``` r
get_latest_reported_cases(x)

get_initial_reported_cases(x)

get_nth_reported_cases(x, delay)
```

## Arguments

- x:

  A `tbl.now` object

- delay:

  A single non-negative number (or `Inf`) giving the maximum reporting
  delay, in report units, to include (only for
  `get_nth_reported_cases()`).

## Value

A `tbl.now` containing the following columns:

- `event_date` The date the event happened. Its numerical version is
  `.event_num`.

- `report_date` The date of the selected report for events happening on
  `event_date`. Its numerical version is `.report_num`.

- `n` The number of events reported for `event_date` at the selected
  point.

- `.delay` The delay of the selected report for that `event_date`.

- Other columns that include the strata or the censoring indicators and
  the temporal effects for that event.

## Details

Functions that extract, for each `event_date` (and stratum), the number
of cases reported at a particular point in the reporting process:

- `get_initial_reported_cases()` — the count as **first** observed (the
  earliest report for that event date).

- `get_latest_reported_cases()` — the count as **latest** observed (the
  most recent report; the current best estimate of the incidence).

- `get_nth_reported_cases()` — the cumulative count observed **within a
  given delay**. **\[experimental\]** With `delay = 0` you get the cases
  reported at delay 0 (the initial snapshot when reporting starts at
  delay 0); `delay = 1` adds those reported at delay 1, and so on.
  `delay = Inf` (or the maximum delay) is identical to
  `get_latest_reported_cases()`.

## Examples

``` r
data(denguedat)
dengue <- tbl_now(denguedat,
  report_date = "report_week",
  event_date = "onset_week",
  strata = "gender",
  verbose = FALSE
)

# Gets the first reported cases (what as initially thought of to be the incidence)
get_initial_reported_cases(dengue)
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

# Gets the latest reported cases (what is now thought of to be the incidence)
get_latest_reported_cases(dengue)
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

# Gets the cases reported within a delay of at most 2 weeks
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
