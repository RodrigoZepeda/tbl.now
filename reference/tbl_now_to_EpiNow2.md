# Convert a `tbl_now` into EpiNow2 input

EpiNow2 works with a single incidence time series (`date`, `confirm`)
and therefore has no delay/report dimension. This function collapses a
`tbl_now` to that single time series, keyed on the `event_date`, using
the most recently reported counts (see
[`get_latest_reported_cases()`](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.md)).

Because EpiNow2 has only one time index, there is intentionally **no**
`tbl_now_from_EpiNow2()`.

## Usage

``` r
tbl_now_to_EpiNow2(x, ..., verbose = TRUE)
```

## Arguments

- x:

  A `tbl_now` object.

- ...:

  Forwarded to
  [`data.table::as.data.table()`](https://rdrr.io/pkg/data.table/man/as.data.table.html).

- verbose:

  Logical. Print the choices that were made.

## Value

A `data.table` with columns `date` and `confirm`.

## Details

**\[experimental\]**

## Examples

``` r
data(mpoxdat)
nowobj <- tbl_now(mpoxdat, event_date = "dx_date", report_date = "dx_report_date",
                  case_count = "n", data_type = "count-incidence", verbose = FALSE)
#> Warning: *Non-unique*: Data has multiple rows for the same event (dx_date) and
#> report(dx_report_date) dates. Consider using `to_count()` to aggregate the data
#> or`distinct()` to remove repeated observations.
tbl_now_to_EpiNow2(nowobj, verbose = FALSE)
#>           date confirm
#>         <Date>   <int>
#>  1: 2022-07-08      49
#>  2: 2022-07-09      21
#>  3: 2022-07-10      19
#>  4: 2022-07-11      65
#>  5: 2022-07-12      61
#>  6: 2022-07-13      65
#>  7: 2022-07-14      63
#>  8: 2022-07-15      58
#>  9: 2022-07-16      30
#> 10: 2022-07-17      33
#> 11: 2022-07-18      77
#> 12: 2022-07-19      86
#> 13: 2022-07-20      90
#> 14: 2022-07-21      72
#> 15: 2022-07-22      82
#> 16: 2022-07-23      37
#> 17: 2022-07-24      42
#> 18: 2022-07-25      98
#> 19: 2022-07-26      87
#> 20: 2022-07-27      76
#> 21: 2022-07-28      77
#> 22: 2022-07-29      71
#> 23: 2022-07-30      58
#> 24: 2022-07-31      41
#> 25: 2022-08-01      93
#> 26: 2022-08-02      82
#> 27: 2022-08-03      81
#> 28: 2022-08-04      75
#> 29: 2022-08-05      71
#> 30: 2022-08-06      36
#> 31: 2022-08-07      31
#> 32: 2022-08-08      83
#> 33: 2022-08-09      64
#> 34: 2022-08-10      70
#> 35: 2022-08-11      77
#> 36: 2022-08-12      60
#> 37: 2022-08-13      21
#> 38: 2022-08-14      25
#> 39: 2022-08-15      71
#> 40: 2022-08-16      52
#> 41: 2022-08-17      37
#> 42: 2022-08-18      51
#> 43: 2022-08-19      48
#> 44: 2022-08-20      22
#> 45: 2022-08-21      20
#> 46: 2022-08-22      37
#> 47: 2022-08-23      37
#> 48: 2022-08-24      28
#> 49: 2022-08-25      29
#> 50: 2022-08-26      36
#> 51: 2022-08-27      17
#> 52: 2022-08-28      17
#> 53: 2022-08-29      33
#> 54: 2022-08-30      34
#> 55: 2022-08-31      25
#> 56: 2022-09-01      20
#> 57: 2022-09-02      17
#> 58: 2022-09-03      14
#> 59: 2022-09-04      15
#> 60: 2022-09-05      13
#> 61: 2022-09-06      27
#> 62: 2022-09-07      19
#> 63: 2022-09-08      23
#> 64: 2022-09-09      18
#> 65: 2022-09-10      16
#> 66: 2022-09-11       7
#> 67: 2022-09-12      26
#> 68: 2022-09-13      11
#> 69: 2022-09-14      13
#> 70: 2022-09-15      11
#> 71: 2022-09-16      14
#> 72: 2022-09-17       9
#> 73: 2022-09-18       6
#> 74: 2022-09-19      16
#> 75: 2022-09-20      11
#> 76: 2022-09-21       8
#> 77: 2022-09-22      12
#> 78: 2022-09-23       7
#> 79: 2022-09-24       5
#> 80: 2022-09-25       3
#> 81: 2022-09-26      13
#> 82: 2022-09-27       8
#> 83: 2022-09-28       8
#> 84: 2022-09-29       4
#> 85: 2022-09-30      10
#> 86: 2022-10-01       1
#> 87: 2022-10-02       1
#> 88: 2022-10-03       2
#> 89: 2022-10-04       8
#> 90: 2022-10-05       2
#> 91: 2022-10-06       1
#> 92: 2022-10-08       1
#> 93: 2022-10-10       1
#> 94: 2022-10-12       1
#>           date confirm
#>         <Date>   <int>
```
