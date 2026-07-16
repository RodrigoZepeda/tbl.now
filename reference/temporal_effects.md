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
  holiday_lags = 0,
  weekend_lags = 0,
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

- holiday_lags:

  Single integer (default `0`). Signed depth `N` of the *holiday* lag
  effect; `holidays` must be supplied whenever `N != 0`.

  When `N > 0` the effect is placed **after** the holiday: indicator
  columns `..._holiday_lag_1`, ..., `..._holiday_lag_N` are created,
  where `..._holiday_lag_k` flags dates that fall exactly `k` **working
  days** after a holiday. Working days skip weekends (see `weekend_days`
  in
  [`add_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.md))
  and other holidays, so the effect lands on the first day back at work.
  Use it to capture a rise in cases just after a holiday.

  When `N < 0` the effect is placed **before** the holiday instead:
  columns `..._holiday_lead_1`, ..., `..._holiday_lead_|N|` are created,
  where `..._holiday_lead_k` flags dates that fall exactly `k` working
  days *before* a holiday. So `holiday_lags = -1` flags Christmas Eve,
  and `holiday_lags = -3` flags the last three working days leading up
  to Christmas.

- weekend_lags:

  Single integer (default `0`). Signed depth `N` of the *weekend* lag
  effect, mirroring `holiday_lags` but resetting on weekend days rather
  than holidays (and needing no calendar).

  When `N > 0`, columns `..._weekend_lag_1`, ..., `..._weekend_lag_N`
  flag dates exactly `k` working days after a weekend, so with Sat/Sun
  weekends `weekend_lags = 1` flags the Monday. When `N < 0`, columns
  `..._weekend_lead_1`, ..., `..._weekend_lead_|N|` flag dates `k`
  working days before a weekend instead: `weekend_lags = -1` flags the
  Friday, and `weekend_lags = -3` flags the Wednesday, Thursday and
  Friday.

  To model both sides of the same break, add two specifications (see the
  examples).

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

## Using a different holiday calendar

`holidays` accepts **any**
[`almanac::rcalendar()`](https://rdrr.io/pkg/almanac/man/rcalendar.html).

A calendar is a set of *recurrence rules*, so you describe how a holiday
is constructed . say "the fourth Thursday of November", and almanac
generates it for every year. In general, you should avoid hardcoding
specific dates (like "18/11/2021").

A calendar has four building blocks:

- **Built-in holidays.** almanac ships rules for common US holidays:
  [`hol_us_thanksgiving()`](https://rdrr.io/pkg/almanac/man/holidays.html),
  [`hol_us_memorial_day()`](https://rdrr.io/pkg/almanac/man/holidays.html),
  [`hol_christmas()`](https://rdrr.io/pkg/almanac/man/holidays.html),
  [`hol_us_election_day()`](https://rdrr.io/pkg/almanac/man/holidays.html),
  and so on. See
  [`almanac::rholiday()`](https://rdrr.io/pkg/almanac/man/rholiday.html)
  for the list.

- **Your own holidays.** Anything without a built-in rule is a
  [`yearly()`](https://rdrr.io/pkg/almanac/man/rrule.html) recurrence
  narrowed with `recur_on_*()` and named with
  [`almanac::rholiday()`](https://rdrr.io/pkg/almanac/man/rholiday.html).

- **Observance.**
  [`almanac::hol_observe()`](https://rdrr.io/pkg/almanac/man/holiday-utilities.html)
  shifts a fixed-date holiday that lands on a weekend onto a working
  day. `adjustment = adj_nearest` gives the usual US rule (Saturday
  moves back to Friday, Sunday forward to Monday); `adj_following` and
  `adj_preceding` always move one way.

- **Editing a calendar.**
  [`almanac::cal_add()`](https://rdrr.io/pkg/almanac/man/calendar-add-remove.html)
  and
  [`almanac::cal_remove()`](https://rdrr.io/pkg/almanac/man/calendar-add-remove.html)
  tweak an existing calendar, and
  [`almanac::cal_names()`](https://rdrr.io/pkg/almanac/man/cal_names.html)
  lists what is in one.

Use
[`almanac::cal_events()`](https://rdrr.io/pkg/almanac/man/cal_events.html)
to check what you built before modelling with it.

### Worked example: the New York City calendar

NYC observes the US federal holidays plus Lincoln's Birthday and
Election Day, and calls the October holiday Columbus Day. Only Lincoln's
Birthday needs a hand-written rule; everything else is built-in, with
[`hol_observe()`](https://rdrr.io/pkg/almanac/man/holiday-utilities.html)
on the fixed-date holidays.

    library(almanac)

    cal_nyc <- function(since = NULL, until = NULL) {

      #Adjust if a holiday happens on a weekend move to the closest date
      #i.e. 4th of July on Saturday in 2026 moves to Friday July 3rd
      on_weekends <- recur_on_weekends(weekly(since = since, until = until))
      observed <- function(x) {
        hol_observe(x, adjust_on = on_weekends, adjustment = adj_nearest)
      }

      # Build a rule for Lincoln's birthday: February 12th, every year.
      lincolns_birthday <- yearly(since = since, until = until) |>
        recur_on_month_of_year("February") |>
        recur_on_day_of_month(12L) |>
        rholiday(name = "Lincoln's Birthday")

      rcalendar(
        #New years day moves to closest weekday
        observed(hol_new_years_day(since = since, until = until)),
        #MLK day happens that day
        hol_us_martin_luther_king_junior_day(since = since, until = until),
        #Lincoln's birthday moves to closest weekday
        observed(lincolns_birthday),
        #President's day happens that day
        hol_us_presidents_day(since = since, until = until),
        #Memorials day happens that day
        hol_us_memorial_day(since = since, until = until),
        #Juneteenth is moved to closest weekday
        observed(hol_us_juneteenth(since = since, until = until)),
        #4th of July is moved to closest weekday
        observed(hol_us_independence_day(since = since, until = until)),
        #Labor day happens that specific day
        hol_us_labor_day(since = since, until = until),
        #We can rename what almanac names Indigenous People's day to Columbus
        hol_rename(
          hol_us_indigenous_peoples_day(since = since, until = until),
          "Columbus Day"
        ),
        #Election day
        hol_us_election_day(since = since, until = until),
        #Veteran's day moves closest
        observed(hol_us_veterans_day(since = since, until = until)),
        #Thanksgiving happens that specific Thursday
        hol_us_thanksgiving(since = since, until = until),
        #Christmas moves to closest day
        observed(hol_christmas(since = since, until = until))
      )
    }

    # Check it before using it. The same rules generate any year you ask for:
    cal_events(cal_nyc(), year = 2026, observed = TRUE)
    cal_events(cal_nyc(), year = 2027, observed = TRUE)

    # Then hand it to temporal_effects() like any other calendar:
    temporal_effects(holidays = cal_nyc())

Two of those show the rules we implemented:

- **Independence Day is Jul 3, not Jul 4.** Jul 4 2026 is a Saturday, so
  `adj_nearest` moves the observance *back* to Friday Jul 3. In 2027 it
  lands on a Sunday and moves *forward* to Mon Jul 5.

- **Christmas 2027 is observed on Fri Dec 24**, and New Year's Day 2028
  is pulled back to Fri Dec 31 2027 — so it appears in the 2027 events,
  not 2028.

**Note** NYC's Lincoln's Birthday is a floating holiday so consider
removing from here.

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

# After-weekend effect: flag the first two working days after a weekend
temporal_effects(weekend = TRUE, weekend_lags = 2)
#> 
#> ── Temporal Effects ────────────────────────────────────────────────────────────
#> The following effects are in place:
#> • "weekend"
#> • "after-weekend" effect: first 2 working days

# Before-weekend effect: flag the last working day before a weekend (Friday)
temporal_effects(weekend = TRUE, weekend_lags = -1)
#> 
#> ── Temporal Effects ────────────────────────────────────────────────────────────
#> The following effects are in place:
#> • "weekend"
#> • "before-weekend" effect: last working day

if (rlang::is_installed("almanac")) {
  cal <- almanac::rcalendar(almanac::hol_christmas())
  temporal_effects(holidays = cal, day_of_month = TRUE, seasons = c(7, 365))

  # After-holiday effect: flag the first 3 working days back after a holiday
  temporal_effects(holidays = cal, holiday_lags = 3)

  # Before-holiday effect: flag the 2 working days leading up to a holiday
  temporal_effects(holidays = cal, holiday_lags = -2)

  # A calendar of your own: write a rule for the holiday, not a date, and
  # almanac generates it for every year (see "Using a different holiday
  # calendar" above for a full local calendar).
  lincolns_birthday <- almanac::yearly() |>
    almanac::recur_on_month_of_year("February") |>
    almanac::recur_on_day_of_month(12L) |>
    almanac::rholiday(name = "Lincoln's Birthday")

  # Add it to the federal calendar and check what you built
  cal_local <- almanac::cal_add(almanac::cal_us_federal(), lincolns_birthday)
  almanac::cal_events(cal_local, year = 2026, observed = TRUE)

  temporal_effects(holidays = cal_local, holiday_lags = 2)

  # Both sides of the holiday: add one specification per direction
  data(denguedat)
  tbl_now(denguedat,
    event_date = onset_week, report_date = report_week, verbose = FALSE
  ) |>
    add_temporal_effects(temporal_effects(holidays = cal, holiday_lags = -2)) |>
    add_temporal_effects(temporal_effects(holidays = cal, holiday_lags = 2))
}
#> # A tibble:  52,987 × 6
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week   gender .event_num .report_num .delay
#>    <date>       <date>        <chr>       <dbl>       <dbl>  <dbl>
#>    [event_date] [report_date] [...]       [...]       [...]  [...]
#>  1 1990-01-01   1990-01-01    Male            0           0      0
#>  2 1990-01-01   1990-01-01    Female          0           0      0
#>  3 1990-01-01   1990-01-01    Female          0           0      0
#>  4 1990-01-01   1990-01-08    Female          0           1      1
#>  5 1990-01-01   1990-01-08    Male            0           1      1
#>  6 1990-01-01   1990-01-15    Female          0           2      2
#>  7 1990-01-01   1990-01-15    Female          0           2      2
#>  8 1990-01-01   1990-01-15    Female          0           2      2
#>  9 1990-01-01   1990-01-22    Female          0           3      3
#> 10 1990-01-01   1990-01-08    Female          0           1      1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "onset_week" | Report date: "report_week"
#> # T. effects (lazy): [event_date] holidays | [event_date] holidays
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows
```
