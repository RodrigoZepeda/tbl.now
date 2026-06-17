# Temporal Effects Class

**\[stable\]**

The `temporal_effects` class specifies which temporal covariates or
effects should be included in a nowcasting model (e.g., day of week,
month, holidays, etc.).

## Usage

``` r
temporal_effects(
  day_of_week = FALSE,
  weekend = FALSE,
  day_of_month = FALSE,
  month_of_year = FALSE,
  week_of_year = FALSE,
  seasons = integer(0),
  season_length = 1,
  holidays = NULL
)
```

## Arguments

- day_of_week:

  Logical. Whether to include an effect for each of the seven days of
  the week.

- weekend:

  Logical. Whether to include an effect for the weekend vs the weekday.

- day_of_month:

  Logical. Whether to include an effect for the day of the month (1 to
  31).

- month_of_year:

  Logical. Whether to include an effect for the month of the year.

- week_of_year:

  Logical. Whether to include an effect for the epidemiological week.

- seasons:

  Vector. Either `integer(0)` (no seasonal effects) or a
  positive-numeric vector where each entry is the number of seasons
  (cycles) to model. The actual Fourier period for the i-th entry is
  `seasons[i] * season_length[i]`.

- season_length:

  Either a single positive number or a vector of the same length as
  `seasons`. Specifies the duration (in data units) of each season
  cycle. Defaults to `1`, meaning the period equals `seasons` directly.
  Use a value greater than 1 when the data unit is finer than the
  season. For example, to model 52-week annual seasonality in **daily**
  data set `seasons = 52, season_length = 7` (period = 364 days).

- holidays:

  Either `NULL` or an
  [`almanac::rcalendar()`](https://rdrr.io/pkg/almanac/man/rcalendar.html)
  specifying how to calculate holidays.

## Value

An object of class `temporal_effects`.

## Details

US Federal holidays can be passed by providing the
[`almanac::cal_us_federal()`](https://rdrr.io/pkg/almanac/man/cal_us_federal.html)
calendar.

Example:

    library(almanac)
    temporal_effects(holidays = cal_us_federal())

## Examples

``` r
temporal_effects(day_of_week = TRUE, week_of_year = TRUE)
#> 
#> ── Temporal Effects ────────────────────────────────────────────────────────────
#> The following effects are in place:
#> • "day_of_week"
#> • "week_of_year"

# Annual seasonality in weekly data (period = 52 weeks)
temporal_effects(seasons = 52)
#> 
#> ── Temporal Effects ────────────────────────────────────────────────────────────
#> The following effects are in place:
#> • "season" periods: 52

# Annual seasonality in daily data (52 weeks x 7 days = 364-day period)
temporal_effects(seasons = 52, season_length = 7)
#> 
#> ── Temporal Effects ────────────────────────────────────────────────────────────
#> The following effects are in place:
#> • "season" periods: 52*7=364

if (rlang::is_installed("almanac")) {
  cal <- almanac::rcalendar(almanac::hol_christmas())
  temporal_effects(holidays = cal, day_of_month = TRUE, seasons = c(7, 365))
}
#> 
#> ── Temporal Effects ────────────────────────────────────────────────────────────
#> The following effects are in place:
#> • "day_of_month"
#> • "season" periods: 7, 365
#> • "holidays":
#>   1. Christmas
```
