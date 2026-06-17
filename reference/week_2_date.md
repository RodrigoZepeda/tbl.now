# Convert epidemiological (or ISO) week/year to aligned dates

**\[experimental\]**

Takes week numbers and year numbers and returns the date corresponding
to a specified weekday within that week. This is typically used for
aligning epiweek or isoweek data to a consistent weekday.

## Usage

``` r
week_2_date(
  .data,
  week_col,
  year_col,
  align_on_day = 1,
  week_fun = lubridate::epiweek,
  year_fun = lubridate::epiyear,
  date_col_name = "date"
)
```

## Arguments

- .data:

  A data.frame or tibble.

- week_col:

  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  column name containing epidemiological week numbers.

- year_col:

  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  column name containing epidemiological year numbers.

- align_on_day:

  Integer 1–7
  ([`lubridate::wday()`](https://lubridate.tidyverse.org/reference/day.html)
  numbering) indicating the weekday to align to.

- week_fun:

  Function that extracts week numbers from a date (either
  [`lubridate::epiweek()`](https://lubridate.tidyverse.org/reference/week.html),
  [`lubridate::isoweek()`](https://lubridate.tidyverse.org/reference/week.html)).

- year_fun:

  Function that extracts the epidemiological/ISO year from a date
  (either
  [`lubridate::epiyear()`](https://lubridate.tidyverse.org/reference/year.html)
  or
  [`lubridate::isoyear()`](https://lubridate.tidyverse.org/reference/year.html)).

- date_col_name:

  Name of the resulting date column.

## Value

The given `data.frame` with a new date column appended.

## Examples

``` r
df <- data.frame(
  epidemiological_week = 1:5,
  epidemiological_year = rep(2024, 5)
)

df %>%
  week_2_date(
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
