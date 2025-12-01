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
#> # A tibble: 52,987 × 6
#> # Groups:   onset_week, gender [2,164]
#>    onset_week report_week gender .event_num .report_num .delay
#>    <date>     <date>      <chr>       <dbl>       <dbl>  <dbl>
#>  1 1990-01-01 1990-01-01  Male            0           0      0
#>  2 1990-01-01 1990-01-01  Female          0           0      0
#>  3 1990-01-01 1990-01-01  Female          0           0      0
#>  4 1990-01-01 1990-01-08  Female          0           1      1
#>  5 1990-01-01 1990-01-08  Male            0           1      1
#>  6 1990-01-01 1990-01-15  Female          0           2      2
#>  7 1990-01-01 1990-01-15  Female          0           2      2
#>  8 1990-01-01 1990-01-15  Female          0           2      2
#>  9 1990-01-01 1990-01-22  Female          0           3      3
#> 10 1990-01-01 1990-01-08  Female          0           1      1
#> # ℹ 52,977 more rows
#> # A tibble:  10,296 × 6
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week   gender   .event_num .report_num .delay
#>    <date>       <date>        <chr>         <dbl>       <dbl>  <dbl>
#>    [event_date] [report_date] [strata]      [...]       [...]  [...]
#>  1 1990-01-01   1990-01-01    Male              0           0      0
#>  2 1990-01-01   1990-01-01    Female            0           0      0
#>  3 1990-01-01   1990-01-01    Female            0           0      0
#>  4 1990-01-08   1990-01-08    Male              1           1      0
#>  5 1990-01-08   1990-01-08    Female            1           1      0
#>  6 1990-01-15   1990-01-15    Female            2           2      0
#>  7 1990-01-15   1990-01-15    Male              2           2      0
#>  8 1990-01-15   1990-01-15    Female            2           2      0
#>  9 1990-01-15   1990-01-15    Male              2           2      0
#> 10 1990-01-15   1990-01-15    Male              2           2      0
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "onset_week" | Report date: "report_week"
#> # Strata: "gender"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 10,286 more rows

#Gets the latest reported cases (what is now thought of to be the incidence)
get_latest_reported_cases(dengue)
#> # A tibble: 52,987 × 6
#> # Groups:   onset_week, gender [2,164]
#>    onset_week report_week gender .event_num .report_num .delay
#>    <date>     <date>      <chr>       <dbl>       <dbl>  <dbl>
#>  1 1990-01-01 1990-01-01  Male            0           0      0
#>  2 1990-01-01 1990-01-01  Female          0           0      0
#>  3 1990-01-01 1990-01-01  Female          0           0      0
#>  4 1990-01-01 1990-01-08  Female          0           1      1
#>  5 1990-01-01 1990-01-08  Male            0           1      1
#>  6 1990-01-01 1990-01-15  Female          0           2      2
#>  7 1990-01-01 1990-01-15  Female          0           2      2
#>  8 1990-01-01 1990-01-15  Female          0           2      2
#>  9 1990-01-01 1990-01-22  Female          0           3      3
#> 10 1990-01-01 1990-01-08  Female          0           1      1
#> # ℹ 52,977 more rows
#> # A tibble:  4,398 × 6
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week   gender   .event_num .report_num .delay
#>    <date>       <date>        <chr>         <dbl>       <dbl>  <dbl>
#>    [event_date] [report_date] [strata]      [...]       [...]  [...]
#>  1 1990-01-01   1990-03-05    Female            0           9      9
#>  2 1990-01-01   1990-02-12    Male              0           6      6
#>  3 1990-01-08   1990-02-12    Male              1           6      5
#>  4 1990-01-08   1990-02-12    Male              1           6      5
#>  5 1990-01-08   1990-02-05    Female            1           5      4
#>  6 1990-01-15   1990-02-12    Male              2           6      4
#>  7 1990-01-15   1990-03-05    Female            2           9      7
#>  8 1990-01-22   1990-02-19    Female            3           7      4
#>  9 1990-01-22   1990-03-19    Male              3          11      8
#> 10 1990-01-22   1990-02-19    Female            3           7      4
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "onset_week" | Report date: "report_week"
#> # Strata: "gender"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 4,388 more rows

```
