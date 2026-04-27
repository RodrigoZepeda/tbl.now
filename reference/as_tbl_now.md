# Transform into a `tbl_now` object

Takes a `data.frame` or `tbl_now` and transforms it into a `tbl_now`
object

## Usage

``` r
as_tbl_now(object, event_date, report_date, ...)

# S3 method for class 'tbl_now'
as_tbl_now(object, event_date, report_date, ...)

# S3 method for class 'data.frame'
as_tbl_now(object, event_date, report_date, ...)
```

## Arguments

- object:

  An object to convert to `tbl_now`.

- event_date:

  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  name of the column containing the event date. Optional when `delay` is
  provided together with `report_date`; the event date will be computed
  as `report_date - delay`.

- report_date:

  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  name of the column containing the report date. Optional when `delay`
  is provided together with `event_date`; the report date will be
  computed as `event_date + delay`.

- ...:

  Additional parameters to pass to
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)

## Examples

``` r
#Convert a data.frame to tbl_now
data(denguedat)
as_tbl_now(denguedat, event_date = "onset_week", report_date = "report_week")
#> ℹ Identified data as <linelist-data> where each observation is a test.
#> # A tibble:  52,987 × 6
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week   gender .event_num .report_num .delay
#>    <date>       <date>        <chr>       <dbl>       <dbl>  <dbl>
#>    [event_date] [report_date] [...]       [...]       [...]  [...]
#>  1 1990-01-01   1990-01-01    Male            0           0      0
#>  2 1990-01-01   1990-01-01    Female          0           0      0
#>  3 1990-01-01   1990-01-01    Female          0           0      0
#>  4 1990-01-01   1990-01-08    Female          0           1      1
#>  5 1990-01-01   1990-01-08    Male            0           1      1
#>  6 1990-01-01   1990-01-15    Female          0           2      2
#>  7 1990-01-01   1990-01-15    Female          0           2      2
#>  8 1990-01-01   1990-01-15    Female          0           2      2
#>  9 1990-01-01   1990-01-22    Female          0           3      3
#> 10 1990-01-01   1990-01-08    Female          0           1      1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "onset_week" | Report date: "report_week"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows
```
