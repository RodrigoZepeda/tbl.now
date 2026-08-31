# A nowcast produced by [`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)

**\[experimental\]**

An S7 object holding a nowcast in a package-agnostic shape. It is what
[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
returns and what
[`nowcast_ensemble()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_ensemble.md)
and
[`score_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/score_nowcast.md)
consume.

## Usage

``` r
tbl_nowcast(
  predictions = dplyr::tibble(),
  draws = NULL,
  method = NA_character_,
  fit = NULL,
  now = NULL,
  event_date = "event_date",
  strata = character(0),
  data = NULL,
  call = NULL,
  metadata = list()
)
```

## Arguments

- predictions:

  A `tibble` of quantile predictions. One row per (event date, stratum,
  quantile level) with the columns `<event_date>`, the strata columns,
  `.quantile_level` and `.value`.

- draws:

  Either `NULL` (backends that only return quantiles) or a `tibble` with
  one row per (event date, stratum, draw) and the columns
  `<event_date>`, the strata columns, `.draw` and `.value`.

- method:

  The name of the method that produced the nowcast.

- fit:

  The untouched object returned by the backend.

- now:

  The `now` of the nowcast.

- event_date:

  Name of the event-date column in `predictions`/`draws`.

- strata:

  Character vector with the names of the strata columns (`character(0)`
  when the nowcast is not stratified).

- data:

  The `tbl_now` the nowcast was produced from.

- call:

  The matched call of
  [`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md).

- metadata:

  A named list with anything else the backend wants to keep.

## Value

A `tbl_nowcast` object.

## See also

[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md),
[`nowcast_ensemble()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_ensemble.md),
[`score_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/score_nowcast.md)

## Examples

``` r
# Normally built for you by the one-call front door, but the constructor is
# exported because a new backend -- or a test -- needs to build one directly.
predictions <- data.frame(
  onset_week = as.Date("2020-01-05") + c(0, 0, 7, 7),
  .quantile_level = c(0.5, 0.9, 0.5, 0.9),
  .value = c(10, 14, 12, 17)
)
tbl_nowcast(predictions = predictions, method = "toy", event_date = "onset_week")
#> ── A <tbl_nowcast> from method "toy" ───────────────────────────────────────────
#> • now: 
#> • event dates: 2
#> • quantile levels: 0.5 and 0.9
#> • draws: none (quantiles only)
#> # A tibble: 4 × 3
#>   onset_week .quantile_level .value
#>   <date>               <dbl>  <dbl>
#> 1 2020-01-05             0.5     10
#> 2 2020-01-05             0.9     14
#> 3 2020-01-12             0.5     12
#> 4 2020-01-12             0.9     17
```
