# Convert between `tbl_now` and tsibble

**\[experimental\]**

A
[`tsibble::tsibble()`](https://tsibble.tidyverts.org/reference/tsibble.html)
has a single time `index` and a `key` identifying each series.
Nowcasting needs two time indices, so the conversion keeps both date
columns: the `index` is the **event date** and the report date (plus any
strata) becomes part of the `key`.

`tbl_now_from_tsibble()` converts a `tbl_ts` into a `tbl_now`. You must
say which column is the `report_date`; `event_date` defaults to the
tsibble's index
([`tsibble::index_var()`](https://tsibble.tidyverts.org/reference/index-rd.html)).

`tbl_now_to_tsibble()` converts a `tbl_now` into a `tbl_ts`, using
`index` (`"event_date"`, the default, or `"report_date"`) as the tsibble
index and the other date plus the strata as the key. Linelist data is
aggregated to `count-incidence` first (a tsibble requires unique
index/key combinations). The covariates, the censoring indicator and any
materialised temporal-effect columns (see
[`compute_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/compute_temporal_effects.md))
ride along as measurement columns.

## Usage

``` r
tbl_now_from_tsibble(
  data,
  report_date,
  event_date = NULL,
  strata = NULL,
  ...,
  verbose = TRUE
)

tbl_now_to_tsibble(
  x,
  ...,
  index = c("event_date", "report_date"),
  verbose = TRUE
)
```

## Arguments

- data:

  A `tbl_ts` (tsibble).

- report_date:

  Column name of the report date (required for `from`).

- event_date:

  Column name of the event date (for `from`); defaults to the tsibble
  index.

- strata:

  Optional character vector of strata columns (`from`). If `NULL`
  (default) the tsibble key columns other than the date columns are
  used.

- ...:

  Forwarded to
  [`as_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md)
  (`from`) or
  [`tsibble::as_tsibble()`](https://tsibble.tidyverts.org/reference/as-tsibble.html)
  (`to`).

- verbose:

  Logical. Print the choices that were made.

- x:

  A `tbl_now` object.

- index:

  For `to`: which date becomes the tsibble index, `"event_date"`
  (default) or `"report_date"`.

## Value

A `tbl_now` (`from`) or a `tbl_ts` (`to`).

## Examples

``` r
data(denguedat)
nowobj <- tbl_now(denguedat,
  event_date = "onset_week",
  report_date = "report_week", verbose = FALSE
)
# The tsibble is indexed by the event date; the report date is in the key.
ts   <- tbl_now_to_tsibble(nowobj, verbose = FALSE)
#> Warning: tsibble requires unique index/key rows; aggregating linelist to
#> "count-incidence" with `to_count()`.
back <- tbl_now_from_tsibble(ts, report_date = "report_week", verbose = FALSE)
```
