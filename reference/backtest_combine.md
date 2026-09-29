# Combine backtests

**\[experimental\]**

Joins
[`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md)
results that were run separately into one, so that an expensive backtest
never has to be repeated:

- **Different models, same dates.** Backtest each engine on its own
  (perhaps on different machines, or as it becomes available) and
  combine them to compare their scores, derive
  [`nowcast_weights()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_weights.md)
  or build a
  [`nowcast_ensemble()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_ensemble.md).

- **Same model, different dates.** Backtest last year's dates once, and
  next year backtest only the new dates and add them to the old result.

Combining backtests of *the same data, dates and engines* gives the
object a single call would have – the tables have the same rows in the
same order:

    bt1 <- nowcast_backtest(x, engine_a, now_dates = my_dates)
    bt2 <- nowcast_backtest(x, engine_b, now_dates = my_dates)
    backtest_combine(bt1, bt2)
    # is the same as
    nowcast_backtest(x, engine_a, engine_b, now_dates = my_dates)

(Only the `elapsed_seconds` of the `timings` differ.)

## Usage

``` r
backtest_combine(..., only_common_dates = FALSE)
```

## Arguments

- ...:

  Two or more
  [`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md)
  objects, or a single list of them. The order of the methods in the
  result follows the order given.

- only_common_dates:

  Logical. Keep only the `now` dates at which every method has a
  successful fit. Default `FALSE` keeps them all.

## Value

A `nowcast_backtest`; see
[`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md).

## Details

The backtests must agree on the event date, the strata, `truth_axis`,
`truth_type`, `keep_draws` and the quantile levels; otherwise they are
not measuring the same thing and the call aborts.

Two backtests must not both have a successful fit of the same method at
the same `now` date: that would be two answers to one question, so the
call aborts. A fit that *failed* in one and succeeded in another is fine
– the success is kept, which lets you re-run only the failures and
combine.

Scores are kept as they were computed. When the backtests were run at
different times the data may have been revised in between, so each was
scored against its own `truth`. The combined `truth` is the one from the
backtest with the latest `now` date, extended with any event dates only
the others have; the call warns when an event date it shares has a
different observed count in the others. Re-run the earlier backtest if
you need every score against the same truth.

Methods that cover different dates are allowed (that is how you add
dates to one model only), and comparisons –
[`nowcast_weights()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_weights.md),
the print method – then use only the targets every method scored, and
say so. Use `only_common_dates = TRUE` to drop the dates that not every
method has.

## See also

[`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md),
whose `checkpoint_file` argument resumes an interrupted backtest,
[`nowcast_weights()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_weights.md)
and
[`nowcast_ensemble()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_ensemble.md).

## Examples

``` r
data(denguedat)
recent <- subset(denguedat, onset_week >= as.Date("2010-06-01"))
dengue <- tbl_now(recent,
  event_date = onset_week, report_date = report_week, verbose = FALSE
)
dates <- as.Date(c("2010-10-04", "2010-11-15"))

# Different models, same dates: backtest separately, then combine.
narrow <- nowcast_backtest(dengue,
  example_engine(spread = 0.1, label = "narrow"),
  now_dates = dates, verbose = FALSE
)
wide <- nowcast_backtest(dengue,
  example_engine(spread = 0.5, label = "wide"),
  now_dates = dates, verbose = FALSE
)
both <- backtest_combine(narrow, wide)
both$methods
#> [1] "narrow" "wide"  

# Same model, new dates: add them to what you already have.
later <- nowcast_backtest(dengue,
  example_engine(spread = 0.1, label = "narrow"),
  now_dates = as.Date("2010-11-22"), verbose = FALSE
)
backtest_combine(narrow, later)$now_dates
#> [1] "2010-10-04" "2010-11-15" "2010-11-22"
```
