# Check the onset and report dates

Check the onset and report dates

## Usage

``` r
check_date_columns(data, event_date, report_date)
```

## Arguments

- data:

  A `data.frame` or `tibble` to be converted.

- event_date:

  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  name of the column containing the event date. Optional when `delay` is
  provided together with `report_date`; the event date will be computed
  as `report_date - delay`.

- report_date:

  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  name of the column containing the report date. Optional when `delay`
  is provided together with `event_date`; the report date will be
  computed as `event_date + delay`.

## Value

\`TRUE\` (invisible) if the \`event_date\` and \`report_date\` are date
columns present in the data and \`event_date\` \<= \`report_date\` for
all observations. Called for its side effects
