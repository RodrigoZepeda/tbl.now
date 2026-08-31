# Ensemble weights from a backtest

**\[experimental\]**

Turns the retrospective scores of a
[`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md)
into a vector of weights for
[`nowcast_ensemble()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_ensemble.md).

## Usage

``` r
nowcast_weights(backtest, type = c("inverse_score", "optim", "equal"), ...)
```

## Arguments

- backtest:

  A `nowcast_backtest` object.

- type:

  How to derive the weights:

  `"inverse_score"` (default)

  :   \\w_i \propto 1/\overline{WIS}\_i\\. Cheap, robust, and never puts
      all the mass on one model.

  `"optim"`

  :   The weights on the simplex that minimise the WIS of the
      quantile-averaged ensemble over the training window. Better in
      principle, but prone to overfitting when the window is short.

  `"equal"`

  :   \\w_i = 1/M\\. Included so that the same code path can produce the
      unweighted ensemble.

- ...:

  Unused.

## Value

A named numeric vector of weights summing to 1.

## See also

[`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md),
which produces the scores these weights come from;
[`nowcast_ensemble()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_ensemble.md),
which consumes them;
[`engine()`](https://rodrigozepeda.github.io/tbl.now/reference/engine.md)'s
`label` argument, which is what tells two configurations of the same
package apart in the result.

## Examples

``` r
data(denguedat)

# A short recent window keeps the example quick.
recent <- subset(denguedat, onset_week >= as.Date("2010-06-01"))
dengue <- tbl_now(recent,
  event_date = onset_week, report_date = report_week, verbose = FALSE
)

## `example_engine()` is a toy that ignores the reporting delay entirely; it
# is used here only so the example runs without a modelling package.
## Swap in a real one -- `engine_baselinenowcast()`, `engine_epinowcast()`,
## `engine_nobbs()` -- for anything you intend to act on.

# Two engines that differ in how wide they claim their intervals are.
bt <- nowcast_backtest(dengue,
  example_engine(spread = 0.2, label = "narrow"),
  example_engine(spread = 0.5, label = "wide"),
  now_dates = as.Date(c("2010-10-04", "2010-11-15")),
  verbose = FALSE
)

# Weights sum to one, and the better-scoring engine takes the larger share.
nowcast_weights(bt)
#>    narrow      wide 
#> 0.6350625 0.3649375 
sum(nowcast_weights(bt))
#> [1] 1

## Hand them to nowcast_ensemble() to pool the nowcasts they came from.
```
