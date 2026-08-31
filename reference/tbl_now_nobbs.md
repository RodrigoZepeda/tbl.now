# Convert a `tbl_now` into the line list NobBS nowcasts from

**\[experimental\]**

[`NobBS::NobBS()`](https://rdrr.io/pkg/NobBS/man/NobBS.html) counts
**rows**: it takes an individual-level line list with one column for the
event date and one for the report date, and treats each row as a case.
Handing it `count-incidence` data directly is therefore silently wrong –
a table of 1,174 rows carrying 50,160 cases is nowcast as 1,174 cases.
This converter expands counts to one row per case first, so the totals
NobBS sees are the totals in your data.

Trim **before** converting when the series is long: the expansion is one
row per case, and `NobBS()`'s own `moving_window` only limits what it
*fits*, not what it is handed.

## Usage

``` r
tbl_now_to_nobbs(
  x,
  ...,
  event_col = "onset_date",
  report_col = "report_date",
  verbose = TRUE
)
```

## Arguments

- x:

  A `tbl_now`.

- ...:

  Unused, for extensibility.

- event_col, report_col:

  Names the two date columns should take in the result. The defaults
  match the arguments of
  [`NobBS::NobBS()`](https://rdrr.io/pkg/NobBS/man/NobBS.html).

- verbose:

  Print what the conversion did. The `units` line prints the string
  [`NobBS::NobBS()`](https://rdrr.io/pkg/NobBS/man/NobBS.html) itself
  accepts (`"1 day"` or `"1 week"`), not the object's own `"days"` /
  `"weeks"`, so it can be pasted straight into the call.

## Value

A `data.frame` with one row per case, ready for
[`NobBS::NobBS()`](https://rdrr.io/pkg/NobBS/man/NobBS.html). The
strata, covariates and temporal-effect columns ride along so a
per-stratum loop can split on them.

## Units NobBS can model

[`NobBS::NobBS()`](https://rdrr.io/pkg/NobBS/man/NobBS.html) documents
`units` as `"1 day"` or `"1 week"` and nothing else, so this converter
aborts on any other grid rather than hand back a line list NobBS cannot
use. That includes a `"numeric"` grid: its date columns are integer
indices, and coercing them with
[`as.Date()`](https://rdrr.io/r/base/as.Date.html) would anchor them at
the 1970 epoch and return a plausible-looking line list of invented
dates. Aggregate to days or weeks first (see
[`align_weeks()`](https://rodrigozepeda.github.io/tbl.now/reference/align_weeks.md)).

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

[`tbl_now_to_surveillance()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_surveillance.md),
[`tbl_now_to_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md)

## Examples

``` r
data(denguedat)
nowobj <- tbl_now(denguedat,
  event_date = "onset_week", report_date = "report_week", verbose = FALSE
)
nb <- tbl_now_to_nobbs(nowobj, verbose = FALSE)
head(nb)
#>   onset_date report_date
#> 1 1990-01-01  1990-01-01
#> 2 1990-01-01  1990-01-01
#> 3 1990-01-01  1990-01-01
#> 4 1990-01-01  1990-01-08
#> 5 1990-01-01  1990-01-08
#> 6 1990-01-01  1990-01-15
```
