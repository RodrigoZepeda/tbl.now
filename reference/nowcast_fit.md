# Fit a nowcast with one modelling package

**\[experimental\]**

`nowcast_fit()` and
[`nowcast_tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_tidy.md)
are the two extension points of the nowcasting framework. Together they
teach
[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
about a new modelling package: `nowcast_fit()` runs the model,
[`nowcast_tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_tidy.md)
turns whatever it returned into the tidy quantile format every other
function in tbl.now understands.

Dispatch happens on the object built by
[`engine()`](https://rodrigozepeda.github.io/tbl.now/reference/engine.md),
so a method for `"mypackage"` is a function called
`nowcast_fit.mypackage()`. It can live in any package.

## Usage

``` r
# S3 method for class 'diseasenowcasting'
nowcast_fit(
  engine,
  x,
  ...,
  quantile_levels = nowcast_quantile_levels(),
  verbose = TRUE
)

# S3 method for class 'baselinenowcast'
nowcast_fit(
  engine,
  x,
  ...,
  draws = 1000,
  delays_unit = NULL,
  quantile_levels = nowcast_quantile_levels(),
  verbose = TRUE
)

# S3 method for class 'epinowcast'
nowcast_fit(
  engine,
  x,
  ...,
  preprocess_args = list(),
  quantile_levels = nowcast_quantile_levels(),
  verbose = TRUE
)

# S3 method for class 'NobBS'
nowcast_fit(
  engine,
  x,
  ...,
  specs = list(),
  quantile_levels = nowcast_quantile_levels(),
  verbose = TRUE
)

# S3 method for class 'surveillance'
nowcast_fit(
  engine,
  x,
  ...,
  when = NULL,
  D = NULL,
  fit_method = "bayes.notrunc.bnb",
  control = list(),
  quantile_levels = nowcast_quantile_levels(),
  verbose = TRUE
)

# S3 method for class 'EpiNow2'
nowcast_fit(
  engine,
  x,
  ...,
  convert_args = list(),
  quantile_levels = nowcast_quantile_levels(),
  verbose = TRUE
)

nowcast_fit(
  engine,
  x,
  ...,
  quantile_levels = nowcast_quantile_levels(),
  verbose = TRUE
)

# Default S3 method
nowcast_fit(
  engine,
  x,
  ...,
  quantile_levels = nowcast_quantile_levels(),
  verbose = TRUE
)
```

## Arguments

- engine:

  An
  [`engine()`](https://rodrigozepeda.github.io/tbl.now/reference/engine.md)
  object – the modelling package plus its arguments. S3 dispatch is on
  its class, so a backend for `"mypackage"` is a function called
  `nowcast_fit.mypackage()`. The whole engine arrives, not just its
  name, so `engine$args`, `engine$label` and the rest are available to a
  backend that wants them.

- x:

  A `tbl_now` object.

- ...:

  Arguments passed straight to the underlying modelling function.
  [`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
  splices the engine's own arguments in here.

- quantile_levels:

  Numeric vector of probabilities. Most backends ignore it at fit time
  (the quantiles are computed from the draws afterwards), but some need
  to be told up front which levels to report.

- verbose:

  Logical. Whether the backend (and the converters feeding it) should be
  chatty.

- draws:

  (`"baselinenowcast"` only) Number of nowcast samples to draw.

- delays_unit:

  (`"baselinenowcast"` only) Unit of the reporting triangle's delay
  axis; inferred from the object's time units when `NULL`.

- preprocess_args:

  (`"epinowcast"` only) A list of arguments for
  [`tbl_now_to_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md),
  e.g. `list(max_delay = 20)`.

- specs:

  (`"NobBS"` only) The `specs` list of
  [`NobBS::NobBS()`](https://rdrr.io/pkg/NobBS/man/NobBS.html). The
  `quantiles` element is filled from `quantile_levels` unless you set
  it.

- when, D, fit_method, control:

  (`"surveillance"` only) The `when`, `D`, `method` and `control`
  arguments of
  [`surveillance::nowcast()`](https://rdrr.io/pkg/surveillance/man/nowcast.html).
  `when` defaults to `get_surveillance_when(x, length = D + 1)`, `D` to
  the largest delay in the data, and `control$dRange` to
  [`get_surveillance_range()`](https://rodrigozepeda.github.io/tbl.now/reference/surveillance_grids.md)
  – the grid running to
  [`get_now()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md),
  which a line list cannot express on its own. Both grids are built from
  the whole object, so every stratum is fitted on the same time axis.
  `fit_method` is surveillance's `method` argument, renamed so it cannot
  collide with
  [`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)'s
  own `method`.

- convert_args:

  (`"EpiNow2"` only) A list of arguments for
  [`tbl_now_to_EpiNow2()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_EpiNow2.md),
  e.g. `list(accumulate = FALSE)`.

## Value

`nowcast_fit()` returns the modelling package's own object, verbatim. It
is stored in the `fit` property of the resulting
[tbl_nowcast](https://rodrigozepeda.github.io/tbl.now/reference/tbl_nowcast.md),
and it is the only thing
[`nowcast_tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_tidy.md)
is given besides the `tbl_now` itself, so put whatever the tidying step
will need into it.

## See also

[`nowcast_tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_tidy.md),
[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md),
[`list_nowcast_methods()`](https://rodrigozepeda.github.io/tbl.now/reference/list_nowcast_methods.md)
and the [*Adding your own nowcasting model*
article](https://rodrigozepeda.github.io/tbl.now/articles/custom-nowcast-models.html)
for a worked example of a new backend.

## Examples

``` r
# A minimal backend: two S3 methods and you are done.
nowcast_fit.constant <- function(engine, x, ..., quantile_levels, verbose = TRUE) {
  counts <- get_latest_reported_cases(x)
  list(dates = counts[[get_event_date(x)]], value = counts[[ncol(counts)]])
}

nowcast_tidy.constant <- function(engine, fit, x, ..., quantile_levels) {
  predictions <- tidyr::expand_grid(
    event_date = fit$dates, .quantile_level = quantile_levels
  )
  predictions$.value <- rep(fit$value, each = length(quantile_levels))
  names(predictions)[1] <- get_event_date(x)
  list(predictions = predictions, draws = NULL)
}
```
