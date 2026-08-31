# Tidy a nowcast produced by `run_nowcast()` or `nowcast_ensemble()`

**\[experimental\]**

Turns a
[tbl_nowcast](https://rodrigozepeda.github.io/tbl.now/reference/tbl_nowcast.md)
into the same table every other
[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
method in this package returns, so a nowcast produced through
[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
and one produced by calling a modelling package by hand are read the
same way.

## Arguments

- x:

  A
  [tbl_nowcast](https://rodrigozepeda.github.io/tbl.now/reference/tbl_nowcast.md).

- probs:

  Optional numeric vector of probabilities in `[0, 1]`, adding one `q*`
  column each. Only available when the nowcast carries **draws**: a
  quantile-only nowcast cannot produce a level it was not summarised at,
  so asking for one is an error rather than an interpolation dressed up
  as a quantile.

- ...:

  Unused, for generic consistency.

## Value

A tibble, as described in *Value*.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with the
columns documented at
[`tidy.nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md):
`event_date`, `stratum`, `estimate`, `conf.low`, `conf.high`, `level`
and `engine`, plus one `q*` column per element of `probs`.

Two of those columns are read off the object rather than assumed:

- `level`:

  A `tbl_nowcast` holds whatever quantile levels it was summarised at,
  and those need not be symmetric. `level` is the width of the **widest
  symmetric pair actually present** – `0.95` for the default
  [`nowcast_quantile_levels()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_quantile_levels.md),
  `0.8` for a fit summarised at `c(0.1, 0.5, 0.9)`. When no symmetric
  pair exists, `level`, `conf.low` and `conf.high` are all `NA`: a
  guessed width defeats the point of the column.

- `engine`:

  The nowcast's `method`, so `"baselinenowcast"` for a single fit and
  `"ensemble"` (or whatever `name`
  [`nowcast_ensemble()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_ensemble.md)
  was given) for a combined one.

`stratum` is `"all"` only when the nowcast declares no strata. Several
strata columns are pasted `" | "`-separated, matching the rest of the
package, so `(stratum, event_date)` is a unique key.

`estimate` is the `0.5` quantile, and `NA` when the nowcast was
summarised at levels that do not include the median.

## See also

[`tidy.nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
for the same table off a raw engine fit,
[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md),
[`nowcast_ensemble()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_ensemble.md).

## Examples

``` r
predictions <- tidyr::expand_grid(
  onset_week = as.Date("2020-01-05") + c(0, 7),
  .quantile_level = c(0.025, 0.5, 0.975)
)
predictions$.value <- c(5, 10, 18, 6, 12, 21)
nc <- tbl_nowcast(
  predictions = predictions, method = "toy", event_date = "onset_week"
)

tidy(nc)
#> # A tibble: 2 × 7
#>   event_date stratum estimate conf.low conf.high level engine
#>   <date>     <chr>      <dbl>    <dbl>     <dbl> <dbl> <chr> 
#> 1 2020-01-05 all           10        5        18  0.95 toy   
#> 2 2020-01-12 all           12        6        21  0.95 toy   
```
