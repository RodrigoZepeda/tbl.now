# Put weekly data on a common weekday

**\[experimental\]**

Weekly surveillance data is rarely as tidy as it looks. The same series
may be stamped with a Wednesday one year and a Thursday the next, or
event dates may fall on a Sunday while reports fall on a Saturday. When
that happens the delay between the two stops being a whole number of
weeks – you get delays of 2.86 weeks – and most nowcasting models, which
count in whole periods, either refuse the data or quietly round it.

`align_weeks()` snaps every date to the same weekday, so week
differences come out as integers. `week_2_date()` solves the
neighbouring problem: you have epiweek (or ISO week) *numbers* rather
than dates, and need real dates to build a `tbl_now` from.

## Usage

``` r
align_weeks(.data, align_on_day = 7, type = "epi", ...)

# S3 method for class 'data.frame'
align_weeks(
  .data,
  align_on_day = 7,
  type = "epi",
  ...,
  date_col,
  new_date_col = NULL
)

# S3 method for class 'tbl_now'
align_weeks(.data, align_on_day = 7, type = "epi", ...)

week_2_date(
  .data,
  week_col,
  year_col,
  align_on_day = 7,
  week_fun = lubridate::epiweek,
  year_fun = lubridate::epiyear,
  date_col_name = "date"
)
```

## Arguments

- .data:

  A `data.frame`, tibble or `tbl_now`.

- align_on_day:

  Integer 1-7 giving the weekday to align to, in ISO numbering: **1 =
  Monday**, 2 = Tuesday, ..., **7 = Sunday**. This is
  [`lubridate::wday()`](https://lubridate.tidyverse.org/reference/day.html)
  with `week_start = 1`, the same convention
  [`is_weekday()`](https://rodrigozepeda.github.io/tbl.now/reference/is_weekday.md)
  uses. Defaults to `7` (Sunday), the start of an epidemiological week.

- type:

  Either `"epi"` (default) or `"iso"`, choosing whether week and year
  are read with
  [`lubridate::epiweek()`](https://lubridate.tidyverse.org/reference/week.html)
  or
  [`lubridate::isoweek()`](https://lubridate.tidyverse.org/reference/week.html).

- ...:

  Additional arguments passed to methods.

- date_col:

  For the `data.frame` method, the
  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  column holding the dates to align.

- new_date_col:

  Name for the aligned column. Defaults to `\{date_col\}_aligned`.

- week_col, year_col:

  For `week_2_date()`, the
  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  columns holding the week number and the year.

- week_fun, year_fun:

  For `week_2_date()`, the functions defining the week convention:
  [`lubridate::epiweek()`](https://lubridate.tidyverse.org/reference/week.html)/[`lubridate::epiyear()`](https://lubridate.tidyverse.org/reference/year.html)
  (the default) or
  [`lubridate::isoweek()`](https://lubridate.tidyverse.org/reference/week.html)/[`lubridate::isoyear()`](https://lubridate.tidyverse.org/reference/year.html).

- date_col_name:

  For `week_2_date()`, the name of the date column to create.

## Value

`align_weeks()` returns its input with an aligned date column added
(`data.frame` method), or a `tbl_now` whose dates have been aligned and
whose `.delay` has been recomputed.

`week_2_date()` returns the input `data.frame` with a new date column
appended.

## Details

Applied to a `data.frame`, `align_weeks()` adds an aligned copy of the
column you name. Applied to a `tbl_now`, it aligns the event and report
dates together and recomputes the delay, so the object stays coherent.

Epi weeks and ISO weeks disagree about where a year starts, so `type`
picks which convention to use: `"epi"` uses
[`lubridate::epiweek()`](https://lubridate.tidyverse.org/reference/week.html)
/
[`lubridate::epiyear()`](https://lubridate.tidyverse.org/reference/year.html),
`"iso"` uses
[`lubridate::isoweek()`](https://lubridate.tidyverse.org/reference/week.html)
/
[`lubridate::isoyear()`](https://lubridate.tidyverse.org/reference/year.html).

## Note

Useful whenever week boundaries differ between systems or between years,
which is the normal state of affairs for epiweek and ISO week data.

## See also

[`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md),
whose `align_weeks = TRUE` argument does this at construction time;
[`is_weekday()`](https://rodrigozepeda.github.io/tbl.now/reference/is_weekday.md),
which numbers weekdays the same way;
[`complete_zeroes()`](https://rodrigozepeda.github.io/tbl.now/reference/complete_zeroes.md)
for filling the weeks where nothing was reported;
[`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md)
for using week-of-year as a model term.

## Examples

``` r
## ---- Plain data frames ------------------------------------------------

# Three dates falling on different weekdays.
df <- data.frame(date = as.Date(c("2022-11-02", "2022-11-07", "2022-11-13")))
weekdays(df$date)
#> [1] "Wednesday" "Monday"    "Sunday"   

# Snap them all back to the Sunday that starts their week.
aligned <- align_weeks(df, date_col = date)
aligned
#>         date date_aligned
#> 1 2022-11-02   2022-10-30
#> 2 2022-11-07   2022-11-06
#> 3 2022-11-13   2022-11-13
weekdays(aligned$date_aligned)
#> [1] "Sunday" "Sunday" "Sunday"

# Or to Tuesday. Weekday numbers are ISO: 1 = Monday, so Tuesday is 2.
align_weeks(df, date_col = date, align_on_day = 2)
#>         date date_aligned
#> 1 2022-11-02   2022-11-01
#> 2 2022-11-07   2022-11-08
#> 3 2022-11-13   2022-11-15

## ---- A tbl_now: making the delays whole numbers -------------------------

data(flusight)

# One state is enough to see the problem.
texas <- flusight[flusight$location_name == "Texas", ]
flutbl <- tbl_now(texas,
  event_date = "target_end_date",
  report_date = "as_of", case_count = "observation",
  strata = "location_name", verbose = FALSE
)

# `as_of` is sometimes a Saturday and sometimes a Wednesday, so some delays
# land between whole weeks.
mean(flutbl$.delay != round(flutbl$.delay))
#> [1] 0.08841785

# After aligning, every delay is a whole number of weeks.
flutbl <- align_weeks(flutbl)
mean(flutbl$.delay != round(flutbl$.delay))
#> [1] 0

## ---- Week numbers instead of dates --------------------------------------

# Data reported as "week 1 of 2024" and so on, with no usable date column.
df <- data.frame(
  epidemiological_week = 1:5,
  epidemiological_year = rep(2024, 5)
)

## week_2_date() turns those into the Sunday that starts each epiweek.
week_2_date(df,
  week_col = epidemiological_week,
  year_col = epidemiological_year
)
#>   epidemiological_week epidemiological_year       date
#> 1                    1                 2024 2023-12-31
#> 2                    2                 2024 2024-01-07
#> 3                    3                 2024 2024-01-14
#> 4                    4                 2024 2024-01-21
#> 5                    5                 2024 2024-01-28
```
