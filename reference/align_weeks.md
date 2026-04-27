# Align weeks to a common weekday

Aligns all dates in either a \`data.frame\` or a \`tbl.now\` so that
week boundaries occur on a specified day of the week. This is useful in
the context of nowcasting for cases when weekly reports are changed from
say Wednesday to Thursday so that delays don't have a decimal point.

## Usage

``` r
align_weeks(.data, align_on_day = 1, type = "epi", ...)

# S3 method for class 'data.frame'
align_weeks(
  .data,
  align_on_day = 1,
  type = "epi",
  ...,
  date_col,
  new_date_col = NULL
)

# S3 method for class 'tbl_now'
align_weeks(.data, align_on_day = 1, type = "epi", ...)
```

## Arguments

- .data:

  A \`data.frame\` or tibble.

- align_on_day:

  Integer 1–7 indicating the weekday to align to. Uses
  \[lubridate::wday()\] numbering (1 = Sunday, 7 = Saturday).

- type:

  Either \`"epi"\` (default) or \`"iso"\`. Determines which week/year
  functions to use whether \[lubridate::epiweek()\] or
  \[lubridate::isoweek()\]

- ...:

  Additional arguments to pass to function

- date_col:

  A
  \[tidy-select\](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  column name containing dates.

- new_date_col:

  Name of the new aligned date column to be created. By default it
  creates a column named \`{date_col}\_aligned\` where \`date_col\`
  corresponds to the column name passed to that parameter.

## Value

A tibble identical to \`.data\` but with an added aligned date column.

## Details

In some cases, to calculate the delay of information, what matters is
the week distance (reports from week 3 in week 7) and not the specific
date distance (reports from Saturday of week 3 vs Monday in week 7). The
\`align_weeks\` function ensures that all week reports are aligned to
the same day of the week (if applied to a \`tbl_now\`) or

## Note

This is also useful when working with epiweeks or isoweeks where week
boundaries may differ between systems or years

## Examples

``` r
# DATA.FRAMES:
# The function aligns weekly data to be "reported" on the same day of the week
df <- data.frame(
  date = as.Date(c("2020-10-31", "2022-11-07", "2022-11-13"))
)

# Align to Sundays
align_weeks(df, date_col = date)
#>         date date_aligned
#> 1 2020-10-31   2020-10-25
#> 2 2022-11-07   2022-11-06
#> 3 2022-11-13   2022-11-13

# Align to Tuesday
align_weeks(df, date_col = date, align_on_day = 3)
#>         date date_aligned
#> 1 2020-10-31   2020-10-27
#> 2 2022-11-07   2022-11-08
#> 3 2022-11-13   2022-11-15

## TBL_NOWS
# If not used you can see the delay has decimal points because
# reports (`as_of`) are sometimes on Saturday and sometimes Wednesday
data(flusight)

#Get the table
flutbl <- tbl_now(flusight, event_date = "target_end_date",
  report_date = "as_of", case_col = "observation",
  strata = c("location_name"))
#> ℹ Identified data as <linelist-data> where each observation is a test.

#See that some delays have decimals
suppressWarnings(as.numeric(flutbl[413484, ".delay"]))
#> [1] 163.5714

#Align the weeks so that they all start on Sunday
flutbl <- flutbl %>% align_weeks()

#Delayed decimals are now integer as all weeks start in Sunday!
suppressWarnings(as.numeric(flutbl[413484, ".delay"]))
#> [1] 164
```
