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
