# Automatically infer the \`now\`.

Function returns the maximum onset date of \`data\` if \`now = NULL\`.
Else it returns the \`now\` it was given.

## Usage

``` r
infer_now(data, now, event_date, report_date)
```

## Value

The \`now\` value for the nowcast which can be the last date of the data
or specified by the user

## Details

@inheritParams tbl_now

## Examples

``` r
if (FALSE) { # \dontrun{
#Get the maximum report date
ddata <- data.frame(
    event_date   = c(as.Date("2020/07/08"), as.Date("2020/07/09")),
    report_date = c(as.Date("2020/07/11"), as.Date("2020/07/12"))
)
infer_now(ddata, NULL, event_date = "event_date", report_date = "report_date")

#Otherwise get the date given
infer_now(ddata, as.Date("2020/07/11"), event_date = "event_date",
  report_date = "report_date")
} # }
```
