# Convert a `tbl_now` into the line list surveillance nowcasts from

**\[experimental\]**

[`surveillance::nowcast()`](https://rdrr.io/pkg/surveillance/man/nowcast.html)
works from an individual-level line list with one column holding the
event date and another the report date, named by its `dEventCol` /
`dReportCol` arguments. `tbl_now_to_surveillance()` produces exactly
that data frame, renaming the two dates to surveillance's own defaults
so the result can be passed straight through.

With `format = "linelist_list"` it returns **one line list per stratum**
as a
[tbl_now_surveillance_list](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_surveillance_list.md),
ready to [`lapply()`](https://rdrr.io/r/base/lapply.html) over –
[`surveillance::nowcast()`](https://rdrr.io/pkg/surveillance/man/nowcast.html)
has no strata argument, so a stratified analysis is one fit per stratum
and this saves splitting by hand.

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
  format = c("linelist", "linelist_list", "sts"),
  aggregate_by = NULL,
  strata_col = "strata",
  strata_sep = " | ",
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

  One of

  - `"linelist"` (default) – the single data frame
    [`surveillance::nowcast()`](https://rdrr.io/pkg/surveillance/man/nowcast.html)
    expects;

  - `"linelist_list"` – one line list **per stratum**, as a
    [tbl_now_surveillance_list](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_surveillance_list.md).
    Still a plain list, so it goes straight into
    [`lapply()`](https://rdrr.io/r/base/lapply.html); length one and
    named `"all"` when the object declares no strata, so the return type
    does not depend on whether strata happen to be attached;

  - `"sts"` – an
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

- strata_col:

  Name of a single column to add, holding every declared stratum pasted
  together, for splitting the line list into one fit per stratum. `NULL`
  leaves it out. Ignored when the object declares no strata.

- strata_sep:

  Separator used to paste the strata into `strata_col`.

- verbose:

  Logical. Print the choices that were made.

## Value

A `data.frame` line list (`format = "linelist"`), a
[tbl_now_surveillance_list](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_surveillance_list.md)
(`format = "linelist_list"`) or an
[surveillance::sts](https://rdrr.io/pkg/surveillance/man/sts-class.html)
object (`format = "sts"`).

## Stratified nowcasts

[`surveillance::nowcast()`](https://rdrr.io/pkg/surveillance/man/nowcast.html)
models one series and has no strata argument, so a stratified analysis
means fitting each stratum separately. `format = "linelist_list"` does
the splitting, so the fit is an
[`lapply()`](https://rdrr.io/r/base/lapply.html):

    pieces <- tbl_now_to_surveillance(x, format = "linelist_list", verbose = FALSE)
    fits <- lapply(pieces, function(piece) {
      surveillance::nowcast(
        now = get_now(x), when = get_surveillance_when(x),
        data = piece, dEventCol = "dHospital", dReportCol = "dReport",
        control = list(dRange = get_surveillance_range(x))
      )
    })

The `control$dRange` comes from the **whole object**, not from the
piece: every stratum has to be laid on the same time axis, or a stratum
whose first case arrived late starts its own time on a different day.

The default `format = "linelist"` keeps the same information in one
frame: the declared strata are pasted into a single `strata` column, so
`split(sur, sur$strata)` reproduces the list. The original columns are
kept alongside it, so you can split on them instead.

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

[`tbl_now_to_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md),
[tbl_now_surveillance_list](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_surveillance_list.md)

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
