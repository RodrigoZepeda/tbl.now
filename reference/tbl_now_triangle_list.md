# One reporting triangle per stratum

**\[stable\]**

The object returned by
`tbl_now_to_baselinenowcast(x, format = "triangle_list")`: a list of
[`baselinenowcast::as_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.html)
matrices, one per observed combination of the object's strata, together
with the metadata needed to rebuild a `tbl_now` from it.

It is a **thin** class – it is still a list, so
[`lapply()`](https://rdrr.io/r/base/lapply.html), `[[` and friends work
as usual. Use it for **inspecting** per-stratum triangles; for fitting a
stratified nowcast, hand the long shape to
[`baselinenowcast::baselinenowcast()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.html)
with its `strata_cols` argument instead – that is the shape it consumes
natively, and what
[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
does under the hood:

    long_df <- tbl_now_to_baselinenowcast(x, format = "long")
    baselinenowcast::baselinenowcast(long_df, strata_cols = tbl.now::get_strata(x))

The class exists for one reason. baselinenowcast has a function,
[`baselinenowcast::estimate_and_apply_delays()`](https://baselinenowcast.epinowcast.org/reference/estimate_and_apply_delays.html),
whose first argument `retro_reporting_triangles` is *also* a list of
triangles – but a list of **retrospective** snapshots of one series,
used to estimate uncertainty, not one triangle per stratum. Passing this
object there would be accepted and would silently treat your strata as
successive points in time. Printing the object says plainly what it is,
so the mistake is visible rather than silent.

## Usage

``` r
# S3 method for class 'tbl_now_triangle_list'
print(x, ...)
```

## Arguments

- x:

  A `tbl_now_triangle_list`.

- ...:

  Ignored.

## Value

[`print()`](https://rdrr.io/r/base/print.html) returns `x` invisibly.

## See also

[`tbl_now_to_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_baselinenowcast.md),
[`as_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md)

## Examples

``` r
data(denguedat)
dengue <- tbl_now(denguedat[1:3000, ],
  event_date = onset_week, report_date = report_week, verbose = FALSE
)

# One reporting triangle per stratum, in the shape baselinenowcast wants.
triangles <- suppressWarnings(
  tbl_now_to_baselinenowcast(dengue, format = "triangle_list", verbose = FALSE)
)

# Printing summarises the set rather than dumping every matrix.
triangles
#> ── 1 reporting triangle from a <tbl_now> ───────────────────────────────────────
#> • No strata; a single triangle named "all"
#> • Delays unit: "weeks"
#> • Now: "1991-10-14"
#> • Dimensions (event x delay): "94 x 14"
#> ℹ This is one triangle per STRATUM. `baselinenowcast::estimate_and_apply_delays()` expects retrospective snapshots of a single series instead -- do not pass this object to it.

# It is a list underneath, so the usual accessors work.
length(triangles)
#> [1] 1
names(triangles)
#> [1] "all"
```
