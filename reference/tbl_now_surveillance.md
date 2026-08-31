# Convert a `tbl_now` into the line list surveillance nowcasts from

**\[experimental\]**

[`surveillance::nowcast()`](https://rdrr.io/pkg/surveillance/man/nowcast.html)
works from an individual-level line list with one column holding the
event date and another the report date, named by its `dEventCol` /
`dReportCol` arguments. `tbl_now_to_surveillance()` produces exactly
that data frame, renaming the two dates to surveillance's own defaults
so the result can be passed straight through.

With `format = "sts"` it instead returns the observed epidemic curve as
an
[surveillance::sts](https://rdrr.io/pkg/surveillance/man/sts-class.html)
object via
[`surveillance::linelist2sts()`](https://rdrr.io/pkg/surveillance/man/linelist2sts.html),
which is what surveillance's plotting and outbreak-detection verbs
consume.

`now` and the delay unit are *not* baked into the result: pass them from
the object with
[`get_now()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
and
[`get_event_units()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md),
as in the example below.

## Usage

``` r
tbl_now_to_surveillance(
  x,
  ...,
  event_col = "dHospital",
  report_col = "dReport",
  format = c("linelist", "sts"),
  aggregate_by = NULL,
  verbose = TRUE
)
```

## Arguments

- x:

  A `tbl_now` object.

- ...:

  Forwarded to
  [`surveillance::linelist2sts()`](https://rdrr.io/pkg/surveillance/man/linelist2sts.html)
  when `format = "sts"`; ignored otherwise.

- event_col, report_col:

  Names to give the event and report date columns in the result. Default
  to surveillance's own `"dHospital"` and `"dReport"`, so
  [`surveillance::nowcast()`](https://rdrr.io/pkg/surveillance/man/nowcast.html)
  finds them without further arguments.

- format:

  `"linelist"` (default) for the data frame
  [`surveillance::nowcast()`](https://rdrr.io/pkg/surveillance/man/nowcast.html)
  expects, or `"sts"` for an
  [surveillance::sts](https://rdrr.io/pkg/surveillance/man/sts-class.html)
  object of the observed curve.

- aggregate_by:

  Aggregation interval, e.g. `"1 week"`. `NULL` (default) derives it
  from the object's event units (`"days"` -\> `"1 day"`, `"weeks"` -\>
  `"1 week"`, `"months"` -\> `"1 month"`, `"years"` -\> `"1 year"`), and
  aborts on a `"numeric"` grid, which has integer indices rather than
  the calendar dates
  [`surveillance::linelist2sts()`](https://rdrr.io/pkg/surveillance/man/linelist2sts.html)
  needs. Pass a value explicitly to override, including on a numeric
  grid if you know what the index steps mean.

- verbose:

  Logical. Print the choices that were made.

## Value

A `data.frame` line list (`format = "linelist"`) or an
[surveillance::sts](https://rdrr.io/pkg/surveillance/man/sts-class.html)
object (`format = "sts"`).

## Cost of expanding counts

surveillance counts rows, so `count-incidence` input is expanded to one
row per case. **Trim before converting** on a large series: the
package's own windowing arguments (`when`, `control$dRange`) limit what
it *fits*, not what it is handed, and a multi-year daily series can
expand into millions of rows.

## Censored delays

A censoring indicator that is a property of the **case** rather than of
the delay – an administrative "this date is only an upper bound" mark,
say – puts a censored and an uncensored row in the same
`(event_date, report_date)` cell. A reporting triangle has one slot per
cell, so the extra dimension has to go before the conversion. It is
removed automatically, with a warning either way:

- **count data**: the counts are summed over the flag, leaving case
  totals unchanged;

- **line lists**: the column is dropped, leaving one row per case.

[`tbl_now_to_epidist()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epidist.md)
is the exception and keeps the flag: estimating a delay distribution is
the one job that can use it.

## See also

[`tbl_now_to_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md)

## Examples

``` r
data(denguedat)
nowobj <- tbl_now(denguedat,
  event_date = "onset_week", report_date = "report_week", verbose = FALSE
)
sur <- tbl_now_to_surveillance(nowobj, verbose = FALSE)
head(sur)
#>    dHospital    dReport
#> 1 1990-01-01 1990-01-01
#> 2 1990-01-01 1990-01-01
#> 3 1990-01-01 1990-01-01
#> 4 1990-01-01 1990-01-08
#> 5 1990-01-01 1990-01-08
#> 6 1990-01-01 1990-01-15

# `now` and the aggregation unit come from the object itself:
get_now(nowobj)
#> [1] "2010-12-20"
```
