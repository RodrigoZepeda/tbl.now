# Complete zeroes

Takes a \`tbl.now\` object and completes observations for event_dates or
onset_weeks that have not been registered by each strata with a 0

## Usage

``` r
complete_zeroes(x, max_delay = NULL)
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
                 as.Date("2020/01/02"), as.Date("2020/01/04"),
                 as.Date("2020/01/04")), 2),
  report = rep(c(as.Date("2020/01/01"), as.Date("2020/01/02"),
                 as.Date("2020/01/02"), as.Date("2020/01/04"),
                 as.Date("2020/01/05")), 2),
  n = rpois(10, lambda = 5),
  sex = c(rep("Male", 5), rep("Female", 5))
)
ndata <- tbl_now(ndata, event_date = event, report_date = report,
     verbose = FALSE, strata = sex, case_count = n, data_type = "count-incidence")
#> Warning: Please use a `temporal_effects` object. Setting from colnames is not
#> recommended and could lead to unexpected behaviour.

#Notice that ndata has no 2020-01-03 event date
ndata
#> # A tibble:  10 × 7
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>    event        report              n sex      .event_num .report_num .delay
#>    <date>       <date>          <int> <chr>         <dbl>       <dbl>  <dbl>
#>    [event_date] [report_date] [cases] [strata]      [...]       [...]  [...]
#>  1 2020-01-01   2020-01-01          4 Male              0           0      0
#>  2 2020-01-01   2020-01-02          6 Male              0           1      1
#>  3 2020-01-02   2020-01-02          4 Male              1           1      0
#>  4 2020-01-04   2020-01-04          7 Male              3           3      0
#>  5 2020-01-04   2020-01-05          6 Male              3           4      1
#>  6 2020-01-01   2020-01-01          4 Female            0           0      0
#>  7 2020-01-01   2020-01-02          3 Female            0           1      1
#>  8 2020-01-02   2020-01-02          5 Female            1           1      0
#>  9 2020-01-04   2020-01-04          2 Female            3           3      0
#> 10 2020-01-04   2020-01-05          5 Female            3           4      1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2020-01-05 | Event date: "event" | Report date: "report"
#> # Strata: "sex"
#> # ────────────────────────────────────────────────────────────────────────────────

#But complete zeroes adds it with a 0
complete_zeroes(ndata)
#> # A tibble:  14 × 7
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>    event        report              n sex      .event_num .report_num .delay
#>    <date>       <date>          <int> <chr>         <int>       <dbl>  <dbl>
#>    [event_date] [report_date] [cases] [strata]      [...]       [...]  [...]
#>  1 2020-01-01   2020-01-01          4 Male              0           0      0
#>  2 2020-01-01   2020-01-02          6 Male              0           1      1
#>  3 2020-01-02   2020-01-02          4 Male              1           1      0
#>  4 2020-01-04   2020-01-04          7 Male              3           3      0
#>  5 2020-01-01   2020-01-01          4 Female            0           0      0
#>  6 2020-01-01   2020-01-02          3 Female            0           1      1
#>  7 2020-01-02   2020-01-02          5 Female            1           1      0
#>  8 2020-01-04   2020-01-04          2 Female            3           3      0
#>  9 2020-01-02   2020-01-03          0 Male              1           2      1
#> 10 2020-01-02   2020-01-03          0 Female            1           2      1
#> 11 2020-01-03   2020-01-03          0 Male              2           2      0
#> 12 2020-01-03   2020-01-03          0 Female            2           2      0
#> 13 2020-01-03   2020-01-04          0 Male              2           3      1
#> 14 2020-01-03   2020-01-04          0 Female            2           3      1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2020-01-05 | Event date: "event" | Report date: "report"
#> # Strata: "sex"
#> # ────────────────────────────────────────────────────────────────────────────────

#Also works for count-cumulative
ndata %>%
 to_count("count-cumulative") %>%
 complete_zeroes() %>%
 dplyr::arrange(event, sex, report)
#> Warning: Please use a `temporal_effects` object. Setting from colnames is not
#> recommended and could lead to unexpected behaviour.
#> Warning: Please use a `temporal_effects` object. Setting from colnames is not
#> recommended and could lead to unexpected behaviour.
#> Warning: Please use a `temporal_effects` object. Setting from colnames is not
#> recommended and could lead to unexpected behaviour.
#> # A tibble:  14 × 7
#> # Data type: "count-cumulative"
#> # Frequency: Event: `days` | Report: `days`
#>    event        report        .event_num .report_num sex            n .delay
#>    <date>       <date>             <dbl>       <dbl> <chr>      <dbl>  <dbl>
#>    [event_date] [report_date]      [...]       [...] [strata] [cases]  [...]
#>  1 2020-01-01   2020-01-01             0           0 Female         4      0
#>  2 2020-01-01   2020-01-02             0           1 Female         7      1
#>  3 2020-01-01   2020-01-01             0           0 Male           4      0
#>  4 2020-01-01   2020-01-02             0           1 Male          10      1
#>  5 2020-01-02   2020-01-02             1           1 Female         5      0
#>  6 2020-01-02   2020-01-03             1           2 Female         5      1
#>  7 2020-01-02   2020-01-02             1           1 Male           4      0
#>  8 2020-01-02   2020-01-03             1           2 Male           4      1
#>  9 2020-01-03   2020-01-03             2           2 Female         0      0
#> 10 2020-01-03   2020-01-04             2           3 Female         0      1
#> 11 2020-01-03   2020-01-03             2           2 Male           0      0
#> 12 2020-01-03   2020-01-04             2           3 Male           0      1
#> 13 2020-01-04   2020-01-04             3           3 Female         2      0
#> 14 2020-01-04   2020-01-04             3           3 Male           7      0
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2020-01-05 | Event date: "event" | Report date: "report"
#> # Strata: "sex"
#> # ────────────────────────────────────────────────────────────────────────────────
```
