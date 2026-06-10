# Transform an object to count data

**\[experimental\]**

## Usage

``` r
to_count(x, to = NULL, ...)

# S3 method for class 'tbl_now'
to_count(x, to = NULL, ...)
```

## Arguments

- x:

  Data to be transformed from `linelist` to count data

- to:

  Either `linelist`, `count-incidence` or `count-cumulative` the
  resulting data-type to be created.

- ...:

  Additional arguments

## Details

This is an S3 generic. This package provides methods for the following
classes:

- `tbl_now`: takes a `tbl_now` object and creates a new column with name
  `n` of counts of observations if `data_type = "linelist"`.

## Note

`linelist` data cannot be reconstructed from `count-*` data. Tring this
will throw an error as you cannot un-count aggregated data.

## Examples

``` r
data(denguedat)
ndata <- tbl_now(denguedat,
    event_date = "onset_week",
    report_date = "report_week",
    strata = "gender")
#> ℹ Identified data as <linelist-data> where each observation is a test.

to_count(ndata, to = "count-incidence")
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

data("covidat")
suppressWarnings({
ndata <- tbl_now(covidat,
event_date = "date_of_symptom_onset",
              report_date = "date_of_registry",
              strata = "sex")
to_count(ndata)
})
#> ℹ Identified data as <linelist-data> where each observation is a test.
#> # A tibble:  40,822 × 7
#> # Data type: "linelist"
#> # Frequency: Event: `days` | Report: `days`
#>    date_of_registry date_of_symptom_onset sex          n .event_num .report_num
#>    <date>           <date>                <chr>    <int>      <dbl>       <dbl>
#>    [report_date]    [event_date]          [strata] [...]      [...]       [...]
#>  1 2020-04-05       2020-03-06            FEMALE       1          0          30
#>  2 2020-04-05       2020-03-06            MALE         1          0          30
#>  3 2020-04-05       2020-03-07            FEMALE       1          1          30
#>  4 2020-04-07       2020-03-11            MALE         1          5          32
#>  5 2020-04-05       2020-03-18            FEMALE       1         12          30
#>  6 2020-04-08       2020-03-23            FEMALE       1         17          33
#>  7 2020-04-05       2020-03-24            MALE         1         18          30
#>  8 2020-04-06       2020-03-26            FEMALE       1         20          31
#>  9 2020-04-05       2020-03-27            FEMALE       1         21          30
#> 10 2020-04-05       2020-03-31            FEMALE       1         25          30
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2023-01-01 | Event date: "date_of_symptom_onset" | Report date:
#> # "date_of_registry"
#> # Strata: "sex"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 40,812 more rows
#> # ℹ 1 more variable: .delay <dbl>

```
