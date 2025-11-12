# Transform an object to count data

This is an S3 generic. This package provides methods for the following
classes:

\* \`tbl_now\`: takes a \`tbl_now\` object and creates a new column with
name \`n\` of counts of observations if \`data_type = "linelist"\`.

## Usage

``` r
to_count(x, ...)

# S3 method for class 'tbl_now'
to_count(x, ...)
```

## Arguments

- x:

  Data to be transformed from \`linelist\` to count data

- ...:

  Additional arguments

## Examples

``` r
data(denguedat)
ndata <- tbl_now(denguedat,
    event_date = "onset_week",
    report_date = "report_week",
    strata = "gender")
#> ℹ Identified data as linelist-data where each observation is a test.

to_count(ndata)
#> # A tibble:  8,265 × 6
#> # Data type: "count"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week   .event_num .report_num gender       n
#>    <date>       <date>             <dbl>       <dbl> <chr>    <int>
#>    [event_date] [report_date]      [...]       [...] [strata] [...]
#>  1 1990-01-01   1990-01-01             0           0 Female       2
#>  2 1990-01-01   1990-01-01             0           0 Male         1
#>  3 1990-01-01   1990-01-08             0           1 Female      13
#>  4 1990-01-01   1990-01-08             0           1 Male        11
#>  5 1990-01-01   1990-01-15             0           2 Female      16
#>  6 1990-01-01   1990-01-15             0           2 Male         7
#>  7 1990-01-01   1990-01-22             0           3 Female       7
#>  8 1990-01-01   1990-01-22             0           3 Male         1
#>  9 1990-01-01   1990-01-29             0           4 Male         1
#> 10 1990-01-01   1990-02-12             0           6 Male         1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "onset_week" | Report date: "report_week"
#> # Strata: "gender"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 8,255 more rows

data("covidat")
ndata <- tbl_now(covidat,
event_date = "date_of_symptom_onset",
              report_date = "date_of_registry",
              strata = "sex")
#> ℹ Identified data as count-data with counts in column `n`.
#> Warning: 147 row(s) have a `report_date` before `event_date`
to_count(ndata)
#> Warning: 147 row(s) have a `report_date` before `event_date`
#> Warning: 147 row(s) have a `report_date` before `event_date`
#> Warning: 147 row(s) have a `report_date` before `event_date`
#> # A tibble:  40,822 × 6
#> # Data type: "count"
#> # Frequency: Event: `days` | Report: `days`
#>    date_of_symptom_onset date_of_registry .event_num .report_num sex          n
#>    <date>                <date>                <dbl>       <dbl> <chr>    <int>
#>    [event_date]          [report_date]         [...]       [...] [strata] [...]
#>  1 2020-03-06            2020-04-05                0          30 FEMALE       1
#>  2 2020-03-06            2020-04-05                0          30 MALE         1
#>  3 2020-03-07            2020-04-05                1          30 FEMALE       1
#>  4 2020-03-11            2020-04-07                5          32 MALE         1
#>  5 2020-03-18            2020-04-05               12          30 FEMALE       1
#>  6 2020-03-23            2020-04-08               17          33 FEMALE       1
#>  7 2020-03-24            2020-04-05               18          30 MALE         1
#>  8 2020-03-26            2020-04-06               20          31 FEMALE       1
#>  9 2020-03-27            2020-04-05               21          30 FEMALE       1
#> 10 2020-03-31            2020-04-05               25          30 FEMALE       1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2023-01-01 | Event date: "date_of_symptom_onset" | Report date:
#> # "date_of_registry"
#> # Strata: "sex"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 40,812 more rows

data("vectordat")
#> Warning: data set ‘vectordat’ not found
ndata <- tbl_now(vectordat,
    event_date = "symptoms",
    report_date = "update",
    strata = c("state","sex"))
#> Error: object 'vectordat' not found
to_count(ndata)
#> Warning: 147 row(s) have a `report_date` before `event_date`
#> Warning: 147 row(s) have a `report_date` before `event_date`
#> Warning: 147 row(s) have a `report_date` before `event_date`
#> # A tibble:  40,822 × 6
#> # Data type: "count"
#> # Frequency: Event: `days` | Report: `days`
#>    date_of_symptom_onset date_of_registry .event_num .report_num sex          n
#>    <date>                <date>                <dbl>       <dbl> <chr>    <int>
#>    [event_date]          [report_date]         [...]       [...] [strata] [...]
#>  1 2020-03-06            2020-04-05                0          30 FEMALE       1
#>  2 2020-03-06            2020-04-05                0          30 MALE         1
#>  3 2020-03-07            2020-04-05                1          30 FEMALE       1
#>  4 2020-03-11            2020-04-07                5          32 MALE         1
#>  5 2020-03-18            2020-04-05               12          30 FEMALE       1
#>  6 2020-03-23            2020-04-08               17          33 FEMALE       1
#>  7 2020-03-24            2020-04-05               18          30 MALE         1
#>  8 2020-03-26            2020-04-06               20          31 FEMALE       1
#>  9 2020-03-27            2020-04-05               21          30 FEMALE       1
#> 10 2020-03-31            2020-04-05               25          30 FEMALE       1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2023-01-01 | Event date: "date_of_symptom_onset" | Report date:
#> # "date_of_registry"
#> # Strata: "sex"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 40,812 more rows
```
