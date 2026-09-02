# Convert between `tbl_now` and epinowcast

**\[experimental\]**

epinowcast represents the same observations in several shapes:

- the **raw long input** `data.frame` (`reference_date`, `report_date`
  and a **cumulative** `confirm` column, plus optional grouping columns)
  consumed by
  [`epinowcast::enw_preprocess_data()`](https://package.epinowcast.org/reference/enw_preprocess_data.html);

- the **preprocessed object** returned by
  [`epinowcast::enw_preprocess_data()`](https://package.epinowcast.org/reference/enw_preprocess_data.html)
  (a nested `data.table` used downstream for modelling, summaries and
  plotting);

- a fitted `epinowcast` object (which extends the preprocessed object).

`tbl_now_from_epinowcast()` accepts **any** of these and converts the
cumulative observations into a `tbl_now` of
`data_type = "count-cumulative"`. When given a preprocessed or fitted
object, the grouping (`by`) columns are detected automatically and the
observations are those retained by preprocessing (i.e. truncated at
`max_delay`).

`tbl_now_to_epinowcast()` takes a `tbl_now` and, by default, builds the
preprocessed
[`epinowcast::enw_preprocess_data()`](https://package.epinowcast.org/reference/enw_preprocess_data.html)
object (the representation used for epinowcast's summaries and plots).
With `preprocess = FALSE` it returns the completed long observation
`data.table` (the model *input* format, as produced by
[`epinowcast::enw_complete_dates()`](https://package.epinowcast.org/reference/enw_complete_dates.html)).

## Usage

``` r
tbl_now_from_epinowcast(
  data,
  ...,
  reference_date = "reference_date",
  report_date = "report_date",
  confirm = "confirm",
  strata = NULL,
  verbose = TRUE
)

tbl_now_to_epinowcast(
  x,
  ...,
  max_delay = NULL,
  timestep = NULL,
  missing_reference = FALSE,
  preprocess = TRUE,
  verbose = TRUE,
  quiet = FALSE
)
```

## Arguments

- data:

  Source data: a raw long `data.frame`/`data.table`, an
  `enw_preprocess_data` object, or a fitted `epinowcast` object.

- ...:

  Additional arguments forwarded to
  [`as_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md)
  (for `from`) or to
  [`epinowcast::enw_preprocess_data()`](https://package.epinowcast.org/reference/enw_preprocess_data.html)
  (for `to`).

- reference_date, report_date, confirm:

  Column names (raw input only; ignored for preprocessed/fitted
  objects).

- strata:

  Optional character vector of grouping columns. If `NULL` (default) the
  grouping is taken from the preprocessed object's `by`, or, for raw
  input, any column other than `reference_date`, `report_date` and
  `confirm`.

- verbose:

  Logical. Print the choices that were made.

- x:

  A `tbl_now` object.

- max_delay:

  Maximum delay (in `timestep`s) to use when preprocessing. If `NULL` it
  is inferred from the data as `max(.delay) + 1`. Because `.delay` is
  measured in the object's report units, this is only in `timestep`s
  when `timestep` matches those units — which is what the default
  infers.

- timestep:

  The epinowcast timestep: `"day"`, `"week"`, or a whole number of days.
  `NULL` (default) infers it from the object's report units (`"days"`
  -\> `"day"`, `"weeks"` -\> `"week"`), which keeps `max_delay` and the
  temporal-effect covariates on the same grid as the data. Other units
  cannot be inferred (epinowcast does not support calendar months) —
  pass a number of days explicitly, e.g. `timestep = 28`.

- missing_reference:

  Passed to
  [`epinowcast::enw_complete_dates()`](https://package.epinowcast.org/reference/enw_complete_dates.html).
  Defaults to `FALSE` (unlike epinowcast's own default of `TRUE`): a
  `tbl_now` never carries reports with a missing `reference_date`, so
  leaving this `TRUE` would synthesise NA-reference padding rows the
  data never had.

- preprocess:

  If `TRUE` (default) returns an `enw_preprocess_data` object; if
  `FALSE` returns the completed observation `data.table`.

- quiet:

  Logical. A *different* channel from `verbose`: `verbose` controls the
  informational summary of what the conversion did, while `quiet`
  suppresses the lossy-conversion **warning** emitted by
  `tbl_now_to_epinowcast()` (see the Round-trip section).

## Value

`tbl_now_from_epinowcast()` returns a `tbl_now`.
`tbl_now_to_epinowcast()` returns an `enw_preprocess_data` object or a
`data.table`.

## Round-trip

The round-trip is **not** the identity, and `tbl_now_to_epinowcast()`
warns to that effect (silence it with `quiet = TRUE`). If you already
have the data in epinowcast's format, work from it directly rather than
converting through `tbl_now` and back.

`tbl_now_from_epinowcast(tbl_now_to_epinowcast(x))` recovers `x` up to
the `max_delay` truncation that epinowcast applies during preprocessing:
reports with a delay beyond `max_delay` are dropped by
[`epinowcast::enw_preprocess_data()`](https://package.epinowcast.org/reference/enw_preprocess_data.html)
and so are absent from the result.

Conversely, `tbl_now_to_epinowcast(tbl_now_from_epinowcast(pobs))` is
not identical to `pobs`, because a `tbl_now` does not retain everything
an `enw_preprocess_data` object carries:

- **Covariate columns** that are neither the core
  `reference_date`/`report_date`/`confirm`, a grouping (`by`) column,
  nor a materialised temporal-effect column are dropped. The
  temporal-effect columns (holidays, Fourier terms, calendar effects)
  *are* carried over: the lazy
  [`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md)
  spec is materialised with
  [`compute_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.md)
  and the resulting columns are passed through to the observations and
  `metareference`/`metareport` tables.

- **Grouping indices** (`.group`) are reassigned from the factor levels,
  so the row order of the nested tables can differ even though the
  underlying values match.

- **NA-reference padding** is not regenerated by default (see
  `missing_reference`).

- **`max_confirm`** (and the derived `cum_prop_reported`) will not match
  for reference dates whose reporting completes *after* `max_delay`. The
  modelled `confirm` (the reporting triangle) is truncated at
  `max_delay`, but epinowcast computes `max_confirm` as the eventual
  final total from the *untruncated* history. A `tbl_now` only stores
  the truncated triangle, so reports arriving beyond `max_delay` are
  gone: on the way back
  [`epinowcast::enw_preprocess_data()`](https://package.epinowcast.org/reference/enw_preprocess_data.html)
  recomputes `max_confirm` from the within-window data and obtains a
  smaller value. The `confirm` counts themselves still round-trip
  exactly; only these truncation-derived summary columns differ. (For
  example, in `germany_covid19_hosp` the 2021-04-06 / 00-04 cell reaches
  7 by delay 40 but a final 11 only at delay 74, so its `max_confirm` is
  11 in `pobs` and 7 after the round-trip.)

## Negative delays

A reporting triangle is indexed by delay from **0**, so a report that
arrived *before* its event has no cell to go in.
[`baselinenowcast::as_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.html)
drops it, and the cell then reads `0` – indistinguishable from an
observed zero. Both triangle formats therefore **warn**, naming how many
rows and cases go, so the loss is not silent; `format = "long"` is a
tidy data frame with no delay axis and keeps them.
`tbl_now_to_epinowcast()` drops them the same way, and warns the same
way.

Filter first if you want to decide what happens:

    x |> dplyr::filter(.delay >= 0) |> tbl_now_to_baselinenowcast()

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

[engine_epinowcast()](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md)
to fit through this package rather than converting by hand;
[`align_weeks()`](https://rodrigozepeda.github.io/tbl.now/reference/align_weeks.md),
because epinowcast lays its grid out in whole timesteps;
[`complete_zeroes()`](https://rodrigozepeda.github.io/tbl.now/reference/complete_zeroes.md)
to fill the grid;
[tidy()](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
for the fitted result.
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
library(data.table)
#> 
#> Attaching package: ‘data.table’
#> The following object is masked from ‘package:base’:
#> 
#>     %notin%
library(epinowcast)
#> ! `enw_cache_location` is not set.
#> ℹ Using `tempdir()` at /tmp/RtmpzegIZH for the epinowcast model cache location.
#> ℹ Set a specific cache location using `enw_set_cache` to control Stan
#>   recompilation in this R session or across R sessions.
#> ℹ For example: `enw_set_cache(tools::R_user_dir(package = "epinowcast",
#>   "cache"), type = c('session', 'persistent'))`.
#> ℹ See `?enw_set_cache` for details.
#> This message is displayed once per session.
#> 
#> Attaching package: ‘epinowcast’
#> The following objects are masked from ‘package:stats’:
#> 
#>     ar, arima

## CRAN asks examples to use at most two cores; data.table would otherwise
## take every one it can find.
data.table::setDTthreads(2)

# epinowcast's own example data: German COVID-19 hospitalisations by age.
obs <- germany_covid19_hosp[location == "DE"][, location := NULL]

## A few weeks and a short delay keep the example quick; preprocessing the whole
## series with `max_delay = 40` costs about ten times as much CPU.
recent <- obs[reference_date >= as.Date("2021-10-15")]
pobs <- epinowcast::enw_preprocess_data(recent, max_delay = 10, by = "age_group")
#> Warning: You specified a maximum delay of 10 days, but the maximum observed delay is
#> only 6 days.
#> • This is justified if you don't have much data yet (e.g. early phase of an
#>   outbreak) and expect a longer maximum delay than currently observed.
#>   epinowcast will then extrapolate the delay distribution beyond the observed
#>   maximum delay.
#> • Otherwise, we recommend using a shorter maximum delay to speed up the
#>   nowcasting.

# From the data.table input format ...
nowobj <- tbl_now_from_epinowcast(recent, strata = c("age_group"))
#> 
#> ── Converted epinowcast <data> into a <tbl_now> 
#> • event_date: "reference_date"
#> • report_date: "report_date"
#> • data_type: "count-cumulative"
#> • now: "2021-10-20"
#> • event_units: "days"
#> • report_units: "days"
#> • strata: "age_group"
#> • case_count: "confirm"
nowobj
#> # A tibble:  146 × 7
#> # Data type: "count-cumulative"
#> # Frequency: Event: `days` | Report: `days`
#>    reference_date age_group confirm report_date   .event_num .report_num .delay
#>    <IDate>        <fct>       <int> <date>             <dbl>       <dbl>  <dbl>
#>    [event_date]   [strata]  [cases] [report_date]      [...]       [...]  [...]
#>  1 2021-10-15     00+           142 2021-10-15             0           0      0
#>  2 2021-10-16     00+           115 2021-10-16             1           1      0
#>  3 2021-10-17     00+            52 2021-10-17             2           2      0
#>  4 2021-10-18     00+            43 2021-10-18             3           3      0
#>  5 2021-10-19     00+           223 2021-10-19             4           4      0
#>  6 2021-10-20     00+           235 2021-10-20             5           5      0
#>  7 2021-10-15     00-04           5 2021-10-15             0           0      0
#>  8 2021-10-16     00-04           1 2021-10-16             1           1      0
#>  9 2021-10-17     00-04           1 2021-10-17             2           2      0
#> 10 2021-10-18     00-04           1 2021-10-18             3           3      0
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2021-10-20 | Event date: "reference_date" | Report date: "report_date"
#> # Strata: "age_group"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 136 more rows

# ... or from a preprocessed epinowcast object.
tbl_epi <- tbl_now_from_epinowcast(pobs)
#> 
#> ── Converted epinowcast <data> into a <tbl_now> 
#> • event_date: "reference_date"
#> • report_date: "report_date"
#> • data_type: "count-cumulative"
#> • now: "2021-10-20"
#> • event_units: "days"
#> • report_units: "days"
#> • strata: "age_group"
#> • case_count: "confirm"

# And back out again.
preprocessed_tbl <- tbl_now_to_epinowcast(tbl_epi, quiet = TRUE)
#> 
#> ── Converting <tbl_now> into an epinowcast object 
#> • reference_date <- "reference_date"
#> • report_date <- "report_date"
#> • confirm <- "confirm"
#> • by: "age_group"
#> • timestep: "day"
#> • max_delay: 6 day
#> • missing_reference: FALSE
#> • preprocess: TRUE
```
