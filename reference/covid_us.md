# covid_us: CDC COVID-19 Case Surveillance Public Use Data (2020-2021)

A compact aggregation of the U.S. CDC's individual-level COVID-19 case
surveillance database, prepared to illustrate **batch reporting**. Each
row is a unique (event date, report date) pair with the number of cases
`n`.

## Usage

``` r
data(covid_us)
```

## Format

A data frame with three variables:

- cdc_case_earliest_dt:

  `Date`. The event date – the earlier of the clinical date and the date
  received by CDC.

- cdc_report_dt:

  `Date`. The report date – when the case was first reported to CDC.

- n:

  `integer`. Number of cases with this (event date, report date) pair.

## Source

Centers for Disease Control and Prevention (CDC), COVID-19 Response.
*COVID-19 Case Surveillance Public Use Data* (version date: June 21,
2024).
<https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data/vbim-akqf/about_data>.
COVID-19 case surveillance data are collected by jurisdictions and
reported voluntarily to CDC.

## Details

In the nowcasting context the **event date** is `cdc_case_earliest_dt`
(the earlier of the clinical/specimen date and the date the case was
received by CDC) and the **report date** is `cdc_report_dt` (the date
the case was first reported to CDC). The delay between them is enormous
and heavily right-skewed: cases were reported to CDC not smoothly but in
large backlog dumps – a textbook batch-reporting pattern that
[`batch_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_test.md)
and
[`transport_discriminant()`](https://rodrigozepeda.github.io/tbl.now/reference/transport_discriminant.md)
recover.

Cases are kept when both their event date and their report date fall
between 2020-01-01 and 2021-12-31 – a self-consistent "as of the end of
2021" snapshot, so the epidemic and its reporting are seen over the same
two years. The handful of rows whose report date precedes their event
date (data-entry errors) were dropped. See `data-raw/covid_us.R` for the
exact duckdb aggregation of the 14 GB source file.

## Examples

``` r
data(covid_us)
tn <- tbl_now(
  covid_us,
  event_date  = cdc_case_earliest_dt,
  report_date = cdc_report_dt,
  case_count  = n,
  data_type   = "count-incidence",
  verbose     = FALSE
)
tn
#> # A tibble:  173,190 × 6
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>    cdc_case_earliest_dt cdc_report_dt       n .event_num .report_num .delay
#>    <date>               <date>          <int>      <dbl>       <dbl>  <dbl>
#>    [event_date]         [report_date] [cases]      [...]       [...]  [...]
#>  1 2020-01-01           2020-01-01         29          0           0      0
#>  2 2020-01-01           2020-04-03          1          0          93     93
#>  3 2020-01-01           2020-04-07          1          0          97     97
#>  4 2020-01-01           2020-04-14          4          0         104    104
#>  5 2020-01-01           2020-04-15          2          0         105    105
#>  6 2020-01-01           2020-04-17          4          0         107    107
#>  7 2020-01-01           2020-04-18          4          0         108    108
#>  8 2020-01-01           2020-04-19          1          0         109    109
#>  9 2020-01-01           2020-04-21          2          0         111    111
#> 10 2020-01-01           2020-04-22          1          0         112    112
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2021-12-31 | Event date: "cdc_case_earliest_dt" | Report date:
#> # "cdc_report_dt"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 173,180 more rows
```
