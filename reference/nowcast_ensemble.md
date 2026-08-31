# Combine several nowcasts into an ensemble

**\[experimental\]**

Takes the nowcasts produced by different modelling packages on the
*same* `tbl_now` and combines them into a single
[tbl_nowcast](https://rodrigozepeda.github.io/tbl.now/reference/tbl_nowcast.md).
Ensembles are routinely better calibrated than any of their members, and
because
[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
puts every backend on the same tidy footing, combining them needs no
reshaping on your side.

## Usage

``` r
nowcast_ensemble(
  ...,
  type = c("quantile", "linear_pool"),
  weights = "equal",
  backtest = NULL,
  quantile_levels = NULL,
  n_draws = 4000L,
  name = "ensemble",
  verbose = TRUE
)
```

## Arguments

- ...:

  [tbl_nowcast](https://rodrigozepeda.github.io/tbl.now/reference/tbl_nowcast.md)
  objects, or a single list of them. Named arguments rename the members.

- type:

  How to combine the members:

  `"quantile"` (default)

  :   Average the members' predictive quantiles level by level
      (Vincentization). Applicable to every backend.

  `"linear_pool"`

  :   Pool the members' posterior draws into a mixture distribution and
      re-summarise it. Requires that every member returned draws, and
      generally yields wider intervals.

- weights:

  Either the string `"equal"` (default), a numeric vector (named by
  method, or in the order the members were given), or one of
  `"inverse_score"` / `"optim"`. The last two require `backtest` and are
  passed to
  [`nowcast_weights()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_weights.md).

- backtest:

  A
  [`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md)
  object, required when `weights` is `"inverse_score"` or `"optim"`.

- quantile_levels:

  Quantile levels to report the ensemble at. Defaults to the levels
  shared by all members.

- n_draws:

  Number of draws in the pooled sample when `type = "linear_pool"`.
  Default `4000`.

- name:

  Name to record as the ensemble's `method`. Default `"ensemble"`.

- verbose:

  Logical. Whether to report the weights that were used.

## Value

A
[tbl_nowcast](https://rodrigozepeda.github.io/tbl.now/reference/tbl_nowcast.md)
whose `fit` property is the list of member nowcasts and whose `metadata`
holds the `weights` and the combination `type`.

## See also

[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
to produce the nowcasts being combined;
[`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md)
and
[`nowcast_weights()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_weights.md)
to decide how much to trust each one, instead of weighting them equally;
[`score_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/score_nowcast.md)
to check the ensemble beats its members. The [*One call, many models*
article](https://rodrigozepeda.github.io/tbl.now/articles/ensemble-nowcasting.html)
builds one end to end.

## Examples

``` r
toy <- function(method, shift) {
  predictions <- data.frame(
    onset_week = as.Date("2020-01-05"),
    .quantile_level = c(0.25, 0.5, 0.75),
    .value = c(8, 10, 13) + shift
  )
  tbl_nowcast(predictions = predictions, method = method, event_date = "onset_week")
}

nowcast_ensemble(toy("a", 0), toy("b", 4), verbose = FALSE)
#> ── A <tbl_nowcast> from method "ensemble" ──────────────────────────────────────
#> • now: 
#> • event dates: 1
#> • quantile levels: 0.25, 0.5, and 0.75
#> • draws: none (quantiles only)
#> # A tibble: 3 × 3
#>   onset_week .quantile_level .value
#>   <date>               <dbl>  <dbl>
#> 1 2020-01-05            0.25     10
#> 2 2020-01-05            0.5      12
#> 3 2020-01-05            0.75     15

# Unequal weights
nowcast_ensemble(toy("a", 0), toy("b", 4), weights = c(a = 0.75, b = 0.25), verbose = FALSE)
#> ── A <tbl_nowcast> from method "ensemble" ──────────────────────────────────────
#> • now: 
#> • event dates: 1
#> • quantile levels: 0.25, 0.5, and 0.75
#> • draws: none (quantiles only)
#> # A tibble: 3 × 3
#>   onset_week .quantile_level .value
#>   <date>               <dbl>  <dbl>
#> 1 2020-01-05            0.25      9
#> 2 2020-01-05            0.5      11
#> 3 2020-01-05            0.75     14
```
