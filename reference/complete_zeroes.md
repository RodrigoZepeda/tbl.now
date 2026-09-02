# Fill in the days when nothing was reported

**\[experimental\]**

Surveillance data records what happened, not what didn't. If no dengue
case with onset on 3 January was reported on 5 January, there is simply
no row for that combination – which is *not* the same as a row saying
zero, even though it means the same thing.

Most nowcasting models need the difference spelled out. They work on a
complete rectangle of (event date x report date) cells, and a missing
cell is ambiguous: it could be a genuine zero, or a delay so long the
report has not arrived yet. `complete_zeroes()` writes the genuine zeros
in explicitly, for every stratum, leaving only the not-yet-reported
cells absent.

## Usage

``` r
complete_zeroes(x, max_delay = NULL, until = NULL)
```

## Arguments

- x:

  A `tbl_now` object.

- max_delay:

  Maximum delay to fill. For example if set to 5 it will complete with
  0's all reports with delays 0 to 4. But will not fill other delays
  (say 6)

- until:

  Event date to complete up to. `NULL` (the default) completes to
  whichever is later, the object's
  [`get_now()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
  or the last event date present in the data. Completing only up to the
  last *observed* event date would leave a gap precisely at the `now`
  edge, because an event date with no reports at all does not appear in
  the data; several downstream converters build their time grid from the
  rows they are given and would silently stop short. A supplied `until`
  is never allowed to truncate below the data, and has no effect beyond
  the `now`: an event date later than the `now` cannot carry any report
  on or before it, so no row would survive for it.

## Value

A `tbl_now` object with the same columns as `x`, plus the rows that were
implicitly zero, carrying `0` in the `case_count` column. The data type
is preserved.

## Details

Zeros are only filled where a report *could* have arrived: cells with a
report date on or before the event date's `now`, and within `max_delay`.
Filling beyond that would invent observations from the future.

## See also

[`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md)
for the data shapes this operates on;
[`censor_reporting_delays_above()`](https://rodrigozepeda.github.io/tbl.now/reference/censoring.md)
for the opposite problem, delays that are too long;
[`diagnose_missing()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_diagnose_components.md)
and
[`diagnose_truncation()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_diagnose_components.md)
to find the gaps first;
[`plot_reporting_triangle()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_reporting_triangle.md)
to see the rectangle being filled.

## Examples

``` r
ndata <- dplyr::tibble(
  event = rep(c(
    as.Date("2020/01/01"), as.Date("2020/01/01"),
    as.Date("2020/01/02"), as.Date("2020/01/04"),
    as.Date("2020/01/04")
  ), 2),
  report = rep(c(
    as.Date("2020/01/01"), as.Date("2020/01/02"),
    as.Date("2020/01/02"), as.Date("2020/01/04"),
    as.Date("2020/01/05")
  ), 2),
  n = rpois(10, lambda = 5),
  sex = c(rep("Male", 5), rep("Female", 5))
)
ndata <- tbl_now(ndata,
  event_date = event, report_date = report,
  verbose = FALSE, strata = sex, case_count = n, data_type = "count-incidence"
)

# Nothing happened on 2020-01-03, so the data has no row for it at all.
sort(unique(ndata$event))
#> [1] "2020-01-01" "2020-01-02" "2020-01-04"

## complete_zeroes() writes that absence down as an explicit zero, for every
# stratum, so a model can tell "no cases" from "not reported yet".
filled <- complete_zeroes(ndata)
sort(unique(filled$event))
#> [1] "2020-01-01" "2020-01-02" "2020-01-03" "2020-01-04" "2020-01-05"
nrow(ndata)
#> [1] 10
nrow(filled)
#> [1] 18

# Also works for count-cumulative
ndata |>
  to_count("count-cumulative") |>
  complete_zeroes() |>
  dplyr::arrange(event, sex, report)
#> # A tibble:  18 × 7
#> # Data type: "count-cumulative"
#> # Frequency: Event: `days` | Report: `days`
#>    event        report        .event_num .report_num sex            n .delay
#>    <date>       <date>             <dbl>       <dbl> <chr>      <dbl>  <dbl>
#>    [event_date] [report_date]      [...]       [...] [strata] [cases]  [...]
#>  1 2020-01-01   2020-01-01             0           0 Female         5      0
#>  2 2020-01-01   2020-01-02             0           1 Female        10      1
#>  3 2020-01-01   2020-01-01             0           0 Male          10      0
#>  4 2020-01-01   2020-01-02             0           1 Male          15      1
#>  5 2020-01-02   2020-01-02             1           1 Female         2      0
#>  6 2020-01-02   2020-01-03             1           2 Female         2      1
#>  7 2020-01-02   2020-01-02             1           1 Male           4      0
#>  8 2020-01-02   2020-01-03             1           2 Male           4      1
#>  9 2020-01-03   2020-01-03             2           2 Female         0      0
#> 10 2020-01-03   2020-01-04             2           3 Female         0      1
#> 11 2020-01-03   2020-01-03             2           2 Male           0      0
#> 12 2020-01-03   2020-01-04             2           3 Male           0      1
#> 13 2020-01-04   2020-01-04             3           3 Female         5      0
#> 14 2020-01-04   2020-01-05             3           4 Female         8      1
#> 15 2020-01-04   2020-01-04             3           3 Male           4      0
#> 16 2020-01-04   2020-01-05             3           4 Male           9      1
#> 17 2020-01-05   2020-01-05             4           4 Female         0      0
#> 18 2020-01-05   2020-01-05             4           4 Male           0      0
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2020-01-05 | Event date: "event" | Report date: "report"
#> # Strata: "sex"
#> # ────────────────────────────────────────────────────────────────────────────────
```
