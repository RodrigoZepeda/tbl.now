# Standardise a fitted nowcast

**\[experimental\]**

The second extension point of the nowcasting framework (see
[`nowcast_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_fit.md)).
It receives the object the modelling package returned and must express
its predictions in the tidy format tbl.now uses everywhere else.

## Usage

``` r
# S3 method for class 'diseasenowcasting'
nowcast_tidy(engine, fit, x, ..., quantile_levels)

# S3 method for class 'baselinenowcast'
nowcast_tidy(engine, fit, x, ..., quantile_levels)

# S3 method for class 'epinowcast'
nowcast_tidy(engine, fit, x, ..., quantile_levels)

# S3 method for class 'NobBS'
nowcast_tidy(engine, fit, x, ..., quantile_levels)

# S3 method for class 'surveillance'
nowcast_tidy(engine, fit, x, ..., quantile_levels)

# S3 method for class 'EpiNow2'
nowcast_tidy(engine, fit, x, ..., quantile_levels)

nowcast_tidy(engine, fit, x, ..., quantile_levels)

# Default S3 method
nowcast_tidy(engine, fit, x, ..., quantile_levels)
```

## Arguments

- engine:

  An
  [`engine()`](https://rodrigozepeda.github.io/tbl.now/reference/engine.md)
  object; see
  [`nowcast_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_fit.md).

- fit:

  The object returned by
  [`nowcast_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_fit.md).

- x:

  The `tbl_now` the nowcast was produced from.

- ...:

  Not forwarded by
  [`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md),
  which passes the user's `...` to
  [`nowcast_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_fit.md)
  only. Anything the tidying step needs – a number of draws, a tuning
  parameter, a lookup table – must therefore travel inside the object
  [`nowcast_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_fit.md)
  returned.

- quantile_levels:

  Numeric vector of probabilities the predictions should be summarised
  at.

## Value

A list with two elements:

- predictions:

  A data frame with one row per (event date, stratum, quantile level)
  and the columns `<event_date>`, the strata columns, `.quantile_level`
  and `.value`.

- draws:

  Either `NULL`, or a data frame with one row per (event date, stratum,
  draw) and the columns `<event_date>`, the strata columns, `.draw` and
  `.value`.

When `draws` is supplied and `predictions` is `NULL`,
[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
derives the quantiles from the draws for you.

## See also

[`nowcast_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_fit.md),
[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)

## Examples

``` r
# See `?nowcast_fit` for a complete two-method backend.
methods(nowcast_tidy)
#> [1] nowcast_tidy.EpiNow2*           nowcast_tidy.NobBS*            
#> [3] nowcast_tidy.baselinenowcast*   nowcast_tidy.default*          
#> [5] nowcast_tidy.diseasenowcasting* nowcast_tidy.epinowcast*       
#> [7] nowcast_tidy.example*           nowcast_tidy.surveillance*     
#> see '?methods' for accessing help and source code
```
