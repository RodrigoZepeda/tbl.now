# Align weeks to a common weekday

Aligns all dates in a dataset so that week boundaries occur on a
specified day of the week. This is useful in the context of nowcasting
for cases when weekly reports are changed from say Wednesday to
Thursday.

## Usage

``` r
align_week(
  .data,
  date_col,
  align_on_day = 1,
  type = "epiweek",
  new_date_col = paste0(deparse(substitute(date_col)), "_aligned")
)
```

## Arguments

- .data:

  A \`data.frame\` or tibble.

- date_col:

  A column name (string or tidy-selection) containing dates.

- align_on_day:

  Integer 1–7 indicating the weekday to align to. Uses
  \[lubridate::wday()\] numbering (1 = Sunday, 7 = Saturday).

- type:

  Either \`"epiweek"\` (default) or \`"isoweek"\`. Determines which
  week/year functions to use.

- new_date_col:

  Name of the new aligned date column to be created.

## Value

A tibble identical to \`.data\` but with an added aligned date column.

## Note

This is also useful when working with epiweeks or isoweeks where week
boundaries may differ between systems or years

## Examples

``` r
df <- data.frame(
  date = as.Date(c("2020-10-31", "2022-11-07", "2022-11-13"))
)

# Align to Sundays
align_week(df, date_col = date)
#>         date date_aligned
#> 1 2020-10-31   2020-10-25
#> 2 2022-11-07   2022-11-06
#> 3 2022-11-13   2022-11-13

# Align to Tuesday
align_week(df, date_col = date, align_on_day = 3)
#>         date date_aligned
#> 1 2020-10-31   2020-10-27
#> 2 2022-11-07   2022-11-08
#> 3 2022-11-13   2022-11-15
```
