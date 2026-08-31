# One reporting triangle per stratum

**\[experimental\]**

The object returned by
`tbl_now_to_baselinenowcast(x, format = "triangle_list")`: a list of
[`baselinenowcast::as_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.html)
matrices, one per observed combination of the object's strata, together
with the metadata needed to rebuild a `tbl_now` from it.

It is a **thin** class – it is still a list, so
[`lapply()`](https://rdrr.io/r/base/lapply.html), `[[` and friends work
as usual:

    triangles <- tbl_now_to_baselinenowcast(x, format = "triangle_list")
    lapply(triangles, baselinenowcast::baselinenowcast)

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
