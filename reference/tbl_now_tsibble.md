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
[`compute_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.md))
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

  The report-date column (required for `from`), as a
  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  expression – a bare column name or a string.

- event_date:

  The event-date column (for `from`), as a
  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  expression – a bare column name or a string. Defaults to the tsibble
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

[as_tsibble()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_coercion_methods.md),
the tsibble method that calls this;
[`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md),
since a tsibble needs unique index/key rows and a line list has to be
aggregated first;
[`align_weeks()`](https://rodrigozepeda.github.io/tbl.now/reference/align_weeks.md)
for regular weekly indexes.
[`as_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md)
for the generic that dispatches to the `*_from_*()` side;
[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md),
which does the conversion for you when you fit through an
[`engine()`](https://rodrigozepeda.github.io/tbl.now/reference/engine.md).
The [*One dataset, many nowcasts*
article](https://rodrigozepeda.github.io/tbl.now/articles/nowcasting-models.html)
fits the same data with every supported package.

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
