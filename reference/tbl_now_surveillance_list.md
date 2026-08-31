# One surveillance line list per stratum

**\[experimental\]**

The object returned by
`tbl_now_to_surveillance(x, format = "linelist_list")`: one
individual-level line list per observed combination of the object's
strata, together with the metadata needed to rebuild a `tbl_now` from
it.

It is a **thin** class – it is still a list of plain data frames, so
[`lapply()`](https://rdrr.io/r/base/lapply.html), `[[` and friends work
as usual:

    pieces <- tbl_now_to_surveillance(x, format = "linelist_list")
    lapply(pieces, function(piece) {
      surveillance::nowcast(
        now = get_now(x), when = get_surveillance_when(x),
        data = piece, dEventCol = "dHospital", dReportCol = "dReport",
        control = list(dRange = get_surveillance_range(x))
      )
    })

The class exists for the same reason
[tbl_now_triangle_list](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_triangle_list.md)
does: printing says plainly that these are strata rather than something
else shaped like a list of line lists, and it carries the `now`, the
units and the original date-column names, none of which survive in a
bare [`split()`](https://rdrr.io/r/base/split.html).

`now` and the time grid are deliberately **not** baked into each piece.
The grid must come from the whole object
([`get_surveillance_range()`](https://rodrigozepeda.github.io/tbl.now/reference/surveillance_grids.md)),
not from the piece: every stratum has to be laid on the same axis, or a
stratum whose first case arrived late starts its own time on a different
day.

[`as_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md)
binds the pieces back together and restores the original date-column
names, the strata and the covariates. Two things do **not** survive,
because they are not in the line list to survive: count input comes back
as a `"linelist"` (one row per case, so the totals are unchanged but the
`case_count` column is gone), and materialised temporal-effect columns
come back as ordinary columns rather than as a spec.

## Usage

``` r
# S3 method for class 'tbl_now_surveillance_list'
print(x, ...)
```

## Arguments

- x:

  A `tbl_now_surveillance_list`.

- ...:

  Ignored.

## Value

[`print()`](https://rdrr.io/r/base/print.html) returns `x` invisibly.

## See also

[`tbl_now_to_surveillance()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_surveillance.md),
[`as_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md),
[tbl_now_triangle_list](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_triangle_list.md)

## Examples

``` r
data(denguedat)
dengue <- tbl_now(denguedat[1:3000, ],
  event_date = onset_week, report_date = report_week, verbose = FALSE
)

## One line list per stratum, in the shape surveillance::nowcast() wants.
linelists <- tbl_now_to_surveillance(dengue,
  format = "linelist_list", verbose = FALSE
)

# Printing summarises the set rather than dumping every data frame.
linelists
#> ── 1 surveillance line list from a <tbl_now> ───────────────────────────────────
#> • No strata; a single line list named "all"
#> • Date columns: "dHospital" (event), "dReport" (report)
#> • Rows each: 3000
#> • Now: "1991-10-14"
#> ℹ `lapply()` over this, passing `control$dRange = get_surveillance_range(x)` from the WHOLE object so every stratum shares one time axis.

length(linelists)
#> [1] 1
head(linelists[[1]])
#>    dHospital    dReport
#> 1 1990-01-01 1990-01-01
#> 2 1990-01-01 1990-01-01
#> 3 1990-01-01 1990-01-01
#> 4 1990-01-01 1990-01-08
#> 5 1990-01-01 1990-01-08
#> 6 1990-01-01 1990-01-15
```
