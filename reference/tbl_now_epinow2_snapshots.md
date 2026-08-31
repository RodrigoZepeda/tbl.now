# Snapshots of one series, as EpiNow2 estimates truncation from

**\[experimental\]**

The object returned by
`tbl_now_to_EpiNow2(x, target = "estimate_truncation")`: a list of
`date`/`confirm` data frames, one per report date, plus the report dates
themselves so the object can be turned back into a `tbl_now`.

It is a **thin** class – still a list, so it can be handed to
[`EpiNow2::estimate_truncation()`](https://epiforecasts.io/EpiNow2/reference/estimate_truncation.html)
unchanged:

    snaps <- tbl_now_to_EpiNow2(x, target = "estimate_truncation")
    EpiNow2::estimate_truncation(snaps)

The class exists because a bare list of `date`/`confirm` frames does not
say *when* each snapshot was taken, and without that the reporting
triangle cannot be recovered from it. Printing also distinguishes it
from the superficially similar list
[`EpiNow2::estimate_secondary()`](https://epiforecasts.io/EpiNow2/reference/estimate_secondary.html)
does *not* take.

## Usage

``` r
# S3 method for class 'tbl_now_epinow2_snapshots'
print(x, ...)
```

## Arguments

- x:

  A `tbl_now_epinow2_snapshots`.

- ...:

  Ignored.

## Value

[`print()`](https://rdrr.io/r/base/print.html) returns `x` invisibly.

## See also

[`tbl_now_to_EpiNow2()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_EpiNow2.md),
[`as_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md)

## Examples

``` r
data(denguedat)
dengue <- tbl_now(denguedat[1:3000, ],
  event_date = onset_week, report_date = report_week, verbose = FALSE
)

# A stack of snapshots: what the series looked like at each of several past
## report dates. EpiNow2::estimate_truncation() uses these to learn how much
# the most recent counts are still going to grow.
snaps <- tbl_now_to_EpiNow2(dengue,
  target = "estimate_truncation", verbose = FALSE, quiet = TRUE
)

# Printing summarises the stack rather than dumping every snapshot.
snaps
#> ── 5 reporting snapshots from a <tbl_now> ──────────────────────────────────────
#> • One per report date: "1991-09-02", "1991-09-09", "1991-09-16", "1991-09-23", and "1991-10-14"
#> • Rows each: 602, 602, 602, 602, and 602
#> • Now: "1991-10-14"
#> ℹ Pass this to `EpiNow2::estimate_truncation()`. `EpiNow2::estimate_secondary()` wants a single data frame of linked series instead -- not this.

length(snaps)
#> [1] 5
head(snaps[[1]])
#>         date confirm accumulate
#> 1 1989-12-26      NA       TRUE
#> 2 1989-12-27      NA       TRUE
#> 3 1989-12-28      NA       TRUE
#> 4 1989-12-29      NA       TRUE
#> 5 1989-12-30      NA       TRUE
#> 6 1989-12-31      NA       TRUE
```
