# Report and event units to numeric

Function that takes a dataframe \`data\` with two columns with names
\`event_date\` and \`report_date\` which can either be dates or numeric.
It transforms the dates

## Usage

``` r
time_cols_to_numeric(
  data,
  event_date,
  report_date,
  event_units,
  report_units,
  force
)
```

## Arguments

- data:

  A data.frame

- event_date:

  Name of the column corresponding to date of the event

- report_date:

  Name of the column corresponding to date of the report

- event_units:

  Either \`days\`, \`weeks\`, \`months\`, \`years\` or \`numeric\`

- report_units:

  Either \`days\`, \`weeks\`, \`months\`, \`years\` or \`numeric\`

## Value

The dataframe \`data\` with two additional columns \`.event_num\` and
\`.report_num\` corresponding to transforming the dates to numeric.
