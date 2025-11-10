# Automatically infer which value is \`date_units\` for one column.

Function returns whether data is daily or weekly \`date_column\`.

## Usage

``` r
infer_units_one_column(data, date_column, date_units)
```

## Arguments

- date_column:

  Name of a column of \`data\` that contains the dates.

## Value

Whether the data's date_units are \`days\`, \`weeks\`, \`months\`,
\`years\` or \`numeric\`.

## Details

@inheritParams tbl_now

## Examples

``` r
if (FALSE) { # \dontrun{
#Get the maximum report date and infer the data distribution
daily_data <- data.frame(
    event_date   = c(as.Date("2020/07/08"), as.Date("2020/07/09"), as.Date("2020/07/10")),
    report_date = c(as.Date("2020/07/11"), as.Date("2020/07/12"), as.Date("2020/07/14"))
)
infer_units_one_column(daily_data, NULL, "report_date")

#Get the maximum report date and infer the data distribution
weekly_data <- data.frame(
    event_date   = c(as.Date("2020/07/08"), as.Date("2020/07/15"), as.Date("2020/07/22")),
    report_date = c(as.Date("2020/07/11"), as.Date("2020/07/18"), as.Date("2020/07/25"))
)
infer_units_one_column(weekly_data, NULL, "report_date")

#Or just input the type
infer_units_one_column(weekly_data, "weeks", "report_date")

#Also works with numeric
infer_units_one_column(tibble(report_date = 1:20), date_units = "auto", "report_date")

} # }
```
