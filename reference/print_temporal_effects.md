# Print temporal effects

Print function for printing the a \[temporal_effects()\].

## Arguments

- x:

  A temporal_effects object created with \[temporal_effects()\]

- ...:

  Additional arguments to pass to print.

## Examples

``` r
print(temporal_effects(day_of_week = TRUE, week_of_year = TRUE))
#> 
#> ── Temporal Effects ────────────────────────────────────────────────────────────
#> The following effects are in place:
#> • "day_of_week"
#> • "week_of_year"
print(temporal_effects(day_of_week = FALSE, week_of_year = FALSE))
#> 
#> ── Temporal Effects ────────────────────────────────────────────────────────────
#> No temporal effects are considered.
print(temporal_effects(day_of_week = FALSE, week_of_year = FALSE, seasons = 52))
#> 
#> ── Temporal Effects ────────────────────────────────────────────────────────────
#> The following effects are in place:
#> • "season" periods: 52
print(temporal_effects(seasons = 52, season_length = 7))
#> 
#> ── Temporal Effects ────────────────────────────────────────────────────────────
#> The following effects are in place:
#> • "season" periods: 52*7=364
```
