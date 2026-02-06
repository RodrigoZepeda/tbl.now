# Complete zeroes

Takes a \`tbl.now\` object and completes observations for event_dates or
onset_weeks that have not been registered by each strata with a 0

## Usage

``` r
complete_zeroes(x, max_delay = 1)
```

## Arguments

- x:

  A \`tbl.now\` object.

- max_delay:

  Maximum delay to fill. For example if set to 5 it will complete with
  0's all reports with delays 0 to 4. But will not fill other delays
  (say 6)

## Examples

``` r
ndata <- dplyr::tibble(
  event  = rep(c(as.Date("2020/01/01"), as.Date("2020/01/01"),
                 as.Date("2020/01/02"), as.Date("2020/01/02"), as.Date("2020/01/04"),
                 as.Date("2020/01/04")), 2),
  report = rep(c(as.Date("2020/01/01"), as.Date("2020/01/02"),
                 as.Date("2020/01/02"), as.Date("2020/01/03"), as.Date("2020/01/04"),
                 as.Date("2020/01/05")), 2),
  n = rpois(12, lambda = 5),
  sex = c(rep("Male", 6), rep("Female", 6))
)
ndata <- tbl_now(ndata, event_date = event, report_date = report,
     verbose = FALSE, strata = sex, case_count = n, data_type = "count-incidence")

#Notice that ndata has no 2020-01-03 event date
ndata
#> # A tibble:  12 × 7
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>    event        report              n sex      .event_num .report_num .delay
#>    <date>       <date>          <int> <chr>         <dbl>       <dbl>  <dbl>
#>    [event_date] [report_date] [cases] [strata]      [...]       [...]  [...]
#>  1 2020-01-01   2020-01-01          4 Male              0           0      0
#>  2 2020-01-01   2020-01-02          3 Male              0           1      1
#>  3 2020-01-02   2020-01-02          7 Male              1           1      0
#>  4 2020-01-02   2020-01-03          6 Male              1           2      1
#>  5 2020-01-04   2020-01-04          2 Male              3           3      0
#>  6 2020-01-04   2020-01-05          8 Male              3           4      1
#>  7 2020-01-01   2020-01-01          9 Female            0           0      0
#>  8 2020-01-01   2020-01-02          4 Female            0           1      1
#>  9 2020-01-02   2020-01-02          7 Female            1           1      0
#> 10 2020-01-02   2020-01-03          2 Female            1           2      1
#> 11 2020-01-04   2020-01-04          5 Female            3           3      0
#> 12 2020-01-04   2020-01-05          5 Female            3           4      1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2020-01-05 | Event date: "event" | Report date: "report"
#> # Strata: "sex"
#> # ────────────────────────────────────────────────────────────────────────────────

#But complete zeroes adds it with a 0
ndata <- complete_zeroes(ndata)
ndata
#> # A tibble:  16 × 7
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>    event        report              n sex      .event_num .report_num .delay
#>    <date>       <date>          <int> <chr>         <int>       <dbl>  <dbl>
#>    [event_date] [report_date] [cases] [strata]      [...]       [...]  [...]
#>  1 2020-01-01   2020-01-01          4 Male              1           1      0
#>  2 2020-01-01   2020-01-02          3 Male              1           2      1
#>  3 2020-01-02   2020-01-02          7 Male              2           2      0
#>  4 2020-01-02   2020-01-03          6 Male              2           3      1
#>  5 2020-01-04   2020-01-04          2 Male              4           4      0
#>  6 2020-01-04   2020-01-05          8 Male              4           5      1
#>  7 2020-01-01   2020-01-01          9 Female            1           1      0
#>  8 2020-01-01   2020-01-02          4 Female            1           2      1
#>  9 2020-01-02   2020-01-02          7 Female            2           2      0
#> 10 2020-01-02   2020-01-03          2 Female            2           3      1
#> 11 2020-01-04   2020-01-04          5 Female            4           4      0
#> 12 2020-01-04   2020-01-05          5 Female            4           5      1
#> 13 2020-01-03   2020-01-03          0 Male              3           3      0
#> 14 2020-01-03   2020-01-03          0 Female            3           3      0
#> 15 2020-01-03   2020-01-04          0 Male              3           4      1
#> 16 2020-01-03   2020-01-04          0 Female            3           4      1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2020-01-05 | Event date: "event" | Report date: "report"
#> # Strata: "sex"
#> # ────────────────────────────────────────────────────────────────────────────────
```
