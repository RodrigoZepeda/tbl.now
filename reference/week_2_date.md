# Convert epidemiological (or ISO) week/year to aligned dates

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

  Column name (string) containing week numbers.

- year_col:

  Column name (string) containing year numbers.

- align_on_day:

  Integer 1–7 (lubridate weekday numbering) indicating the weekday to
  align to.

- week_fun:

  Function that extracts week numbers from a date (e.g.,
  \[lubridate::epiweek()\], \[lubridate::isoweek()\]).

- year_fun:

  Function that extracts the epidemiological/ISO year from a date.

- date_col_name:

  Name of the resulting date column.

## Value

The input dataframe with a new date column appended.

## Examples

``` r
df <- data.frame(
  week_col = 1:5,
  year_col = rep(2024, 5)
)

week_2_date(df, week_col = "week_col", year_col = "year_col")
#>   week_col year_col       date
#> 1        1     2024 2023-12-31
#> 2        2     2024 2024-01-07
#> 3        3     2024 2024-01-14
#> 4        4     2024 2024-01-21
#> 5        5     2024 2024-01-28
```
