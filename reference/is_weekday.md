# Is a date a weekday or a weekend?

**\[stable\]**

Reporting almost always slows down at the weekend, which is one of the
strongest and most predictable patterns in surveillance data. This tells
you which days are which, and lets you say what "weekend" means – it is
Friday and Saturday in much of the Middle East, and Sunday alone in some
countries.

## Usage

``` r
is_weekday(date, weekend_days = c("Sat", "Sun"))
```

## Arguments

- date:

  A Date (or POSIXt) object. May be a vector.

- weekend_days:

  A character or numeric vector defining which days count as the
  weekend. Defaults to Saturday and Sunday.

  - Character: day names or abbreviations, case-insensitive –
    `c("Mon", "Tuesday", "wed", ...)`.

  - Numeric: integers 1-7 in
    [`lubridate::wday()`](https://lubridate.tidyverse.org/reference/day.html)
    numbering with `week_start = 1`, so **1 = Monday** and 7 = Sunday.

## Value

A logical vector, `TRUE` where the date is a weekday and `FALSE` where
it falls on the weekend.

## See also

[`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md)
and
[`add_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.md),
which use this to build the day-of-week and weekend terms a model can
fit;
[plot_day_of_week_effects()](https://rodrigozepeda.github.io/tbl.now/reference/calendar_effect_plots.md)
to see the effect in the data;
[`align_weeks()`](https://rodrigozepeda.github.io/tbl.now/reference/align_weeks.md),
whose `align_on_day` uses this same ISO numbering.

## Examples

``` r
is_weekday(as.Date("2020-04-22")) # TRUE (Wed)
#> [1] TRUE
is_weekday(as.Date("2020-04-19")) # FALSE (Sun)
#> [1] FALSE

## Middle East weekend (Fri - Sat)
is_weekday(as.Date("2020-04-17"), weekend_days = c("Fri", "Sat"))
#> [1] FALSE

# Weekend only on Friday
is_weekday(as.Date("2020-04-17"), weekend_days = "Friday")
#> [1] FALSE
is_weekday(as.Date("2020-04-18"), weekend_days = "Friday")
#> [1] TRUE

## Weekend on Sun - Mon (numeric: 7 = Sun, 1 = Mon)
is_weekday(as.Date("2020-04-20"), weekend_days = c(7, 1))
#> [1] FALSE
```
