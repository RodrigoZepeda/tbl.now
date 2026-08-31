# Default quantile levels for a nowcast

**\[experimental\]**

The quantile levels
[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
summarises a nowcast at by default: nine probabilities, symmetric about
the median, spanning the 50%, 80%, 90% and 95% central intervals.

They are a **subset** of the 23 levels the US and European COVID-19
forecast hubs and FluSight ask for (0.01, 0.025, 0.05, then 0.10 to 0.90
in steps of 0.05, then 0.975 and 0.99). Nine cover the intervals people
actually read at a fraction of the storage, and every one of them is a
hub level, so the output still scores against hub submissions in
scoringutils without interpolation. Pass `quantile_levels` explicitly
when you need the full hub set:

    hub_levels <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
    run_nowcast(x, engine("baselinenowcast", quantile_levels = hub_levels))

The levels live on the
[`engine()`](https://rodrigozepeda.github.io/tbl.now/reference/engine.md),
not on
[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md),
because for some backends they are a fit-time model argument rather than
a way of summarising afterwards.

Backends that expose draws can honour any levels you ask for. Ones that
report a point estimate and a single interval (`"surveillance"`,
`"EpiNow2"`) cannot, and say so rather than interpolating.

## Usage

``` r
nowcast_quantile_levels()
```

## Value

A numeric vector of nine probabilities in `(0, 1)`, sorted increasingly.

## See also

[`engine()`](https://rodrigozepeda.github.io/tbl.now/reference/engine.md),
whose `quantile_levels` argument this is the default for;
[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
and
[`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md),
which report at these levels;
[`score_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/score_nowcast.md)
and
[`as_scoringutils()`](https://rodrigozepeda.github.io/tbl.now/reference/score_nowcast.md),
which score them.

## Examples

``` r
nowcast_quantile_levels()
#> [1] 0.025 0.050 0.100 0.250 0.500 0.750 0.900 0.950 0.975

# The 50%, 80%, 90% and 95% central intervals, as lower/upper pairs.
matrix(nowcast_quantile_levels()[-5], ncol = 2)
#>       [,1]  [,2]
#> [1,] 0.025 0.750
#> [2,] 0.050 0.900
#> [3,] 0.100 0.950
#> [4,] 0.250 0.975

# Ask an engine for something else -- here the full forecast-hub set.
hub_levels <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
engine("baselinenowcast", quantile_levels = hub_levels)
#> ── <nowcast_engine: "baselinenowcast"> ─────────────────────────────────────────
#> • quantile levels: 0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, …, 0.975, and 0.99
```
