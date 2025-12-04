# Get the latest/first reported cases for each event date

Function that gets the latest (respectively first) number of cases that
has been reported for each \`event_date\`

## Usage

``` r
get_latest_reported_cases(x)

get_initial_reported_cases(x)
```

## Arguments

- x:

  A \`tbl_now\` object

## Examples

``` r
data(denguedat)
dengue <- tbl_now(denguedat,
                  report_date = "report_week",
                  event_date = "onset_week",
                  strata = "gender",
                  verbose = FALSE)

dengue <- to_count(dengue)

#Gets the first reported cases (what as initially thought of to be the incidence)
get_initial_reported_cases(dengue)
#> # A tibble:  2,164 × 7
#> # Data type: "count-cumulative"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week   .event_num .report_num gender         n .delay
#>    <date>       <date>             <dbl>       <dbl> <chr>      <int>  <dbl>
#>    [event_date] [report_date]      [...]       [...] [strata] [cases]  [...]
#>  1 1990-01-01   1990-01-01             0           0 Female         2      0
#>  2 1990-01-01   1990-01-01             0           0 Male           1      0
#>  3 1990-01-08   1990-01-08             1           1 Female        16      0
#>  4 1990-01-08   1990-01-08             1           1 Male          13      0
#>  5 1990-01-15   1990-01-15             2           2 Female        34      0
#>  6 1990-01-15   1990-01-15             2           2 Male           4      0
#>  7 1990-01-22   1990-01-22             3           3 Female        22      0
#>  8 1990-01-22   1990-01-22             3           3 Male          23      0
#>  9 1990-01-29   1990-01-29             4           4 Female        17      0
#> 10 1990-01-29   1990-01-29             4           4 Male          19      0
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "onset_week" | Report date: "report_week"
#> # Strata: "gender"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 2,154 more rows

#Gets the latest reported cases (what is now thought of to be the incidence)
get_latest_reported_cases(dengue)
#> # A tibble:  2,164 × 7
#> # Data type: "count-cumulative"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week   .event_num .report_num gender         n .delay
#>    <date>       <date>             <dbl>       <dbl> <chr>      <int>  <dbl>
#>    [event_date] [report_date]      [...]       [...] [strata] [cases]  [...]
#>  1 1990-01-01   1990-03-05             0           9 Female         1      9
#>  2 1990-01-01   1990-02-12             0           6 Male          12      6
#>  3 1990-01-08   1990-02-05             1           5 Female        20      4
#>  4 1990-01-08   1990-02-12             1           6 Male          18      5
#>  5 1990-01-15   1990-03-05             2           9 Female         1      7
#>  6 1990-01-15   1990-02-12             2           6 Male          13      4
#>  7 1990-01-22   1990-02-19             3           7 Female         9      4
#>  8 1990-01-22   1990-03-19             3          11 Male           1      8
#>  9 1990-01-29   1990-03-19             4          11 Female         2      7
#> 10 1990-01-29   1990-03-12             4          10 Male           6      6
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "onset_week" | Report date: "report_week"
#> # Strata: "gender"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 2,154 more rows

```
