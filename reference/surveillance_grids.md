# The date grids [`surveillance::nowcast()`](https://rdrr.io/pkg/surveillance/man/nowcast.html) needs

**\[stable\]**

[`surveillance::nowcast()`](https://rdrr.io/pkg/surveillance/man/nowcast.html)
takes three dates and two date *grids*, and none of them have defaults
you can rely on. These two helpers build the grids from the `tbl_now`
itself, so the object stays the single source of truth for what "now" is
and how wide a time step is:

- `get_surveillance_when()` – the dates you want **estimated**, passed
  as `when`. The most recent `length` steps up to and including
  [`get_now()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md).

- `get_surveillance_range()` – the **whole** time axis the model is laid
  on, passed as `control$dRange`. Every step from the first event to
  `now`. Its last element is also what `now` itself should be:
  [`get_now()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
  can fall mid-epoch, and
  [`surveillance::nowcast()`](https://rdrr.io/pkg/surveillance/man/nowcast.html)
  refuses that (see below).

    sur_fit <- surveillance::nowcast(
      now  = max(get_surveillance_range(x)),
      when = get_surveillance_when(x, length = 30),
      data = tbl_now_to_surveillance(x, verbose = FALSE),
      dEventCol = "dHospital", dReportCol = "dReport",
      control = list(dRange = get_surveillance_range(x))
    )

## Usage

``` r
get_surveillance_when(x, length = 30L, ..., to = NULL, by = NULL)

get_surveillance_range(x, ..., from = NULL, to = NULL, by = NULL)
```

## Arguments

- x:

  A `tbl_now`.

- length:

  Number of time steps to estimate, counting back from `to`. The result
  has `length` elements, the last of which is `to`.

- ...:

  Unused, for extensibility.

- to:

  Last date of the grid. Defaults to
  [`get_now()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md).

- by:

  Step, as a [`seq.Date()`](https://rdrr.io/r/base/seq.Date.html) `by`
  string (`"1 day"`, `"1 week"`, ...). Defaults to the object's own
  event units.

- from:

  First date of the grid. Defaults to the earliest event date in `x`.

## Value

A `Date` vector, in increasing order.

## Why `dRange` has to be given explicitly

Left to itself,
[`surveillance::nowcast()`](https://rdrr.io/pkg/surveillance/man/nowcast.html)
infers the time axis from the data it was handed – and a **line list
cannot express a zero**. A day on which nothing was reported has no
rows, so it is not in the line list, so it is not in the inferred axis.
That is exactly the situation at the `now` edge, which is the part you
are nowcasting: the last few days are quiet precisely because their
reports have not arrived yet, and the axis silently stops short of
`now`. Passing `dRange` states the grid instead of letting it be
guessed, so the quiet days at the end are modelled as zeros observed so
far rather than as days that do not exist.

This is also why
[`complete_zeroes()`](https://rodrigozepeda.github.io/tbl.now/reference/complete_zeroes.md)
is no help here: it can only add zero *counts*, and a line list has no
count column to put a zero in.

## Which weekday the grid lands on

[`surveillance::nowcast()`](https://rdrr.io/pkg/surveillance/man/nowcast.html)
refuses a grid that does not sit at the **first day of an epoch**: a
Monday for `"1 week"`, the first of the month for `"1 month"`.
Epidemiological weeks routinely start on a Sunday instead, so both grids
are snapped back to the epoch start, and both may therefore begin a few
days before the dates in `x`.
[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
shifts surveillance's estimates back onto the object's own weekday when
they return, so a nowcast fitted through the engine is still indexed by
the event dates you gave it.

## See also

[`tbl_now_to_surveillance()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_surveillance.md),
[`get_now()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md),
[`get_event_units()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)

## Examples

``` r
data(denguedat)
nowobj <- tbl_now(denguedat,
  event_date = "onset_week", report_date = "report_week", verbose = FALSE
)
get_surveillance_when(nowobj, length = 4)
#> [1] "2010-11-29" "2010-12-06" "2010-12-13" "2010-12-20"
range(get_surveillance_range(nowobj))
#> [1] "1990-01-01" "2010-12-20"
```
