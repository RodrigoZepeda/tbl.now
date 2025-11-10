# Check the onset and report dates

Check the onset and report dates

## Usage

``` r
check_date_columns(data, event_date, report_date)
```

## Arguments

- data:

  A \`data.frame\` to be converted.

- event_date:

  Character. The name of the column containing the event date.

- report_date:

  Character. The name of the column containing the report date.

## Value

\`TRUE\` (invisible) if the \`event_date\` and \`report_date\` are date
columns present in the data and \`event_date\` \<= \`report_date\` for
all observations. Called for its side effects
