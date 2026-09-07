# Tidy the predictions and scores of a `nowcast_backtest()`

**\[experimental\]**

One row per (method, `now` date, target) carrying both halves of the
comparison – what the model said and what happened – with the
dot-prefixed internal column names traded for ordinary ones so the
result goes straight into dplyr or ggplot2.

## Usage

``` r
# S3 method for class 'nowcast_backtest'
tidy(x, ...)
```

## Arguments

- x:

  A `nowcast_backtest` object.

- ...:

  Unused, for generic consistency.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with the
columns `method`, `now`, `event_date`, `stratum`, `observed`,
`estimate`, `conf.low`, `conf.high`, `level`, `wis`, `ae_median`,
`coverage_50` and `coverage_90`. `stratum` is `"all"` for an
unstratified backtest and the `" | "`-pasted strata otherwise, so
`(method, now, stratum, event_date)` is a unique key.

`estimate`, `conf.low`, `conf.high` and `level` are the retrospective
prediction itself, read off the same quantiles the scores were computed
from and named as
[tidy()](https://rodrigozepeda.github.io/tbl.now/reference/tidy.tbl_nowcast.md)
names them: `estimate` is the `0.5` quantile and `level` the width of
the **widest symmetric pair actually present**.
[`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md)
refuses engines that report different quantile levels, so `level` is one
number for the whole table. When no symmetric pair exists all three of
`conf.low`, `conf.high` and `level` are `NA`, and `estimate` is `NA`
when the median was not among the levels reported – a guessed width
defeats the point of the column.

## See also

[`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md),
which produces the object being tidied;
[`nowcast_weights()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_weights.md)
to turn the same scores into ensemble weights;
[`score_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/score_nowcast.md)
for scoring a single nowcast;
[tidy()](https://rodrigozepeda.github.io/tbl.now/reference/tidy.tbl_nowcast.md)
for a fitted nowcast rather than a backtest.

## Examples

``` r
data(denguedat)
recent <- subset(denguedat, onset_week >= as.Date("2010-06-01"))
dengue <- tbl_now(recent,
  event_date = onset_week, report_date = report_week, verbose = FALSE
)

## `example_engine()` is a toy that ignores the reporting delay entirely; it
# is used here only so the example runs without a modelling package.
## Swap in a real one -- `engine_baselinenowcast()`, `engine_epinowcast()`,
## `engine_nobbs()` -- for anything you intend to act on.

bt <- nowcast_backtest(dengue,
  example_engine(label = "carry forward"),
  now_dates = as.Date(c("2010-10-04", "2010-11-15")), verbose = FALSE
)

# One tidy row per method, `now` date, stratum and event date, carrying the
# retrospective prediction next to the resolved truth used for scoring.
head(tidy(bt))
#> # A tibble: 6 × 13
#>   method      now        event_date stratum observed estimate conf.low conf.high
#>   <chr>       <date>     <date>     <chr>      <dbl>    <dbl>    <dbl>     <dbl>
#> 1 carry forw… 2010-10-04 2010-06-07 all          157      157      127       187
#> 2 carry forw… 2010-10-04 2010-06-14 all          210      210      170       250
#> 3 carry forw… 2010-10-04 2010-06-21 all          193      193      156       230
#> 4 carry forw… 2010-10-04 2010-06-28 all          193      193      156       230
#> 5 carry forw… 2010-10-04 2010-07-05 all          258      258      209       307
#> 6 carry forw… 2010-10-04 2010-07-12 all          315      315      255       375
#> # ℹ 5 more variables: level <dbl>, wis <dbl>, ae_median <dbl>,
#> #   coverage_50 <lgl>, coverage_90 <lgl>
```
