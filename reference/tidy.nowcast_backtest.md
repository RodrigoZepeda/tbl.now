# Tidy the scores of a `nowcast_backtest()`

**\[experimental\]**

One row per (method, `now` date, target) with the scores that target
earned, with the dot-prefixed internal column names traded for ordinary
ones so the result goes straight into dplyr or ggplot2.

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
columns `method`, `now`, `event_date`, `stratum`, `observed`, `wis`,
`ae_median`, `coverage_50` and `coverage_90`. `stratum` is `"all"` for
an unstratified backtest and the `" | "`-pasted strata otherwise, so
`(method, now, stratum, event_date)` is a unique key.

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

# One tidy row per method, `now` date, stratum and event date.
head(tidy(bt))
#> # A tibble: 6 × 9
#>   method      now        event_date stratum observed   wis ae_median coverage_50
#>   <chr>       <date>     <date>     <chr>      <dbl> <dbl>     <dbl> <lgl>      
#> 1 carry forw… 2010-10-04 2010-06-07 all          157  3.84         0 TRUE       
#> 2 carry forw… 2010-10-04 2010-06-14 all          210  5.13         0 TRUE       
#> 3 carry forw… 2010-10-04 2010-06-21 all          193  4.68         0 TRUE       
#> 4 carry forw… 2010-10-04 2010-06-28 all          193  4.68         0 TRUE       
#> 5 carry forw… 2010-10-04 2010-07-05 all          258  6.28         0 TRUE       
#> 6 carry forw… 2010-10-04 2010-07-12 all          315  7.6          0 TRUE       
#> # ℹ 1 more variable: coverage_90 <lgl>
```
