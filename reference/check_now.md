# Check the \`now\` argument

Checks that the \`now\` argument is a date and within range of the data

## Usage

``` r
check_now(data, event_date, report_date, now)
```

## Arguments

- data:

  A `data.frame` or `tibble` to be converted.

- event_date:

  [`` <`tidy-select`> ``](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  name of the column containing the event date. Optional when `delay` is
  provided together with `report_date`; the event date will be computed
  as `report_date - delay`.

- report_date:

  [`` <`tidy-select`> ``](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  name of the column containing the report date. Optional when `delay`
  is provided together with `event_date`; the report date will be
  computed as `event_date + delay`.

- now:

  (optional) Date or `NULL` (default). The date that is considered the
  `now` of the nowcast. If no `now` is given then the function
  automatically uses the last `event_date`.

## Value

(invisible) TRUE if the \`now\` date is achievable considering the
\`data\` dataset and the columns \`event_date\` and \`report_date\`
