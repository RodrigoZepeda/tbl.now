# Cases at a chosen point in the validation process

**\[experimental\]**

The same three questions as
[get_latest_reported_cases()](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.md),
asked of the **third** date: not when the system heard about a case, but
when the laboratory settled it.

- `get_initial_validated_cases()` – the count as of the **first** result
  to come back for that event date.

- `get_latest_validated_cases()` – the count as of the **most recent**
  result: everything settled so far.

- `get_nth_validated_cases()` – the count settled **within a given delay
  of the event**.

A case that is still `"pending"` has no validation date, so it has not
arrived on this axis and none of these count it. That is the point: the
gap between
[get_latest_reported_cases()](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.md)
and `get_latest_validated_cases()` is the backlog the laboratory still
owes you.

## Usage

``` r
get_latest_validated_cases(x, type = "total")

get_initial_validated_cases(x, type = "total")

get_nth_validated_cases(x, delay, type = "total")
```

## Arguments

- x:

  A `tbl_now` with a validation process (see
  [`add_validation_date()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md)).

- type:

  Which cases to count. One of:

  `"total"`

  :   (default) every case, whatever the outcome. On the validation axis
      that means every case that has been settled at all.

  `"confirmed"`, `"retracted"`, `"pending"`

  :   only the cases with that outcome. `"pending"` is a reporting-axis
      question only – a pending case has no validation date – and the
      validation getters refuse it.

  `"unknown"`

  :   the cases whose `validation_type` is `NA`: settled, but the data
      does not say which way.

  `"net"`

  :   confirmed **minus** retracted – the running total as a
      surveillance system publishes it, which can go **down** when a
      case is withdrawn. This is the quantity a `count-cumulative`
      stream actually reports, and the one diseasenowcasting's
      signed-increment (Skellam / SkNB) likelihood is built for; see
      `diseasenowcasting::confirmation_process()`.

  `"by_type"`

  :   one row per outcome instead of one number: the outcome column
      joins the keys, so you get pending, confirmed and retracted side
      by side.

  On an object with no validation process anything but `"total"` warns
  and pools, because there is no outcome to filter on.

- delay:

  A single non-negative number (or `Inf`) giving the longest delay
  **from the event** to include, in the object's units. Only used by
  `get_nth_validated_cases()`.

## Value

A `count-cumulative` `tbl_now` with one row per event date (and stratum,
grouping column, and outcome when `type = "by_type"`), carrying the
event, report and validation dates of the selected arrival, the
generated numeric columns, and the count.

## Which date the count is indexed by

By the **event date**, as every other `get_*_cases()` function is. A
case confirmed three weeks after onset still belongs to the week it
began. If you want counts by validation date instead, group on
`get_validation_date(x)` yourself – that is a different question (how
busy was the laboratory) and this package does not silently answer it.

## Which delay `get_nth_validated_cases()` counts

The delay **from the event**, so that `get_nth_reported_cases(x, 7)` and
`get_nth_validated_cases(x, 7)` describe the same seven days and can be
read against each other. It is deliberately *not* `.validation_delay`,
which is the laboratory's turnaround measured from the report.
[`diagnose_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_drift.md)
and
[summary()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_summary.md)
make the same choice for the same reason.

## Grouping is respected

As with the reporting-axis getters: the caller's grouping becomes a key
and comes back on the result. See
[get_latest_reported_cases()](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.md).

## See also

[get_latest_reported_cases()](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.md)
for the same counts on the reporting process;
[add_validation_date()](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
to attach a validation;
[validation_delay](https://rodrigozepeda.github.io/tbl.now/reference/validation_delay.md)
for how long resolution takes;
[`plot_validation_status()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_validation_status.md)
to see confirmed, retracted and pending over time.

## Examples

``` r
cases <- data.frame(
  onset = as.Date("2021-01-04") + c(0, 0, 1, 1, 2),
  visit = as.Date("2021-01-05") + c(0, 0, 1, 1, 2),
  result = as.Date("2021-01-06") + c(0, 0, 1, 1, 2),
  outcome = c("confirmed", "retracted", "confirmed", "confirmed", "retracted")
)
flu <- tbl_now(cases,
  event_date = onset, report_date = visit,
  validation_date = result, validation_type = outcome,
  data_type = "linelist", verbose = FALSE
)

# Three answers to "how many cases were there?".
get_latest_reported_cases(flu) # everything reported
#> # A tibble:  3 × 6
#> # Data type: "count-cumulative"
#> # Frequency: Event: `days` | Report: `days`
#>   onset        visit         .event_num .report_num       n .delay
#>   <date>       <date>             <dbl>       <dbl>   <dbl>  <dbl>
#>   [event_date] [report_date]      [...]       [...] [cases]  [...]
#> 1 2021-01-04   2021-01-05             0           1       2      1
#> 2 2021-01-05   2021-01-06             1           2       2      1
#> 3 2021-01-06   2021-01-07             2           3       1      1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2021-01-08 | Event date: "onset" | Report date: "visit"
#> # ────────────────────────────────────────────────────────────────────────────────
get_latest_validated_cases(flu, type = "confirmed") # only the positives
#> # A tibble:  2 × 10
#> # Data type: "count-cumulative"
#> # Frequency: Event: `days` | Report: `days`
#>   onset        visit      .event_num .report_num result     outcome     n .delay
#>   <date>       <date>          <dbl>       <dbl> <date>     <chr>   <dbl>  <dbl>
#>   [event_date] [report_d…      [...]       [...] [validati… [valid… [cas…  [...]
#> 1 2021-01-04   2021-01-05          0           1 2021-01-06 confir…     1      1
#> 2 2021-01-05   2021-01-06          1           2 2021-01-07 confir…     2      1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2021-01-08 | Event date: "onset" | Report date: "visit"
#> # Validation date: "result" ("days") | resolved: 2/2
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 2 more variables: .validation_num <dbl>, .validation_delay <dbl>
get_latest_validated_cases(flu, type = "net") # positives minus withdrawals
#> # A tibble:  3 × 10
#> # Data type: "count-cumulative"
#> # Frequency: Event: `days` | Report: `days`
#>   onset        visit      .event_num .report_num result     outcome     n .delay
#>   <date>       <date>          <dbl>       <dbl> <date>     <chr>   <dbl>  <dbl>
#>   [event_date] [report_d…      [...]       [...] [validati… [valid… [cas…  [...]
#> 1 2021-01-04   2021-01-05          0           1 2021-01-06 NA          0      1
#> 2 2021-01-05   2021-01-06          1           2 2021-01-07 NA          2      1
#> 3 2021-01-06   2021-01-07          2           3 2021-01-08 NA         -1      1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2021-01-08 | Event date: "onset" | Report date: "visit"
#> # Validation date: "result" ("days") | resolved: 0/3
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 2 more variables: .validation_num <dbl>, .validation_delay <dbl>

# Every outcome side by side.
get_latest_validated_cases(flu, type = "by_type")
#> # A tibble:  4 × 10
#> # Data type: "count-cumulative"
#> # Frequency: Event: `days` | Report: `days`
#>   onset        visit      .event_num .report_num result     outcome     n .delay
#>   <date>       <date>          <dbl>       <dbl> <date>     <chr>   <dbl>  <dbl>
#>   [event_date] [report_d…      [...]       [...] [validati… [valid… [cas…  [...]
#> 1 2021-01-04   2021-01-05          0           1 2021-01-06 confir…     1      1
#> 2 2021-01-04   2021-01-05          0           1 2021-01-06 retrac…     1      1
#> 3 2021-01-05   2021-01-06          1           2 2021-01-07 confir…     2      1
#> 4 2021-01-06   2021-01-07          2           3 2021-01-08 retrac…     1      1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2021-01-08 | Event date: "onset" | Report date: "visit"
#> # Validation date: "result" ("days") | resolved: 4/4
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 2 more variables: .validation_num <dbl>, .validation_delay <dbl>

# And the same question asked earlier in the process: what had come back by
# the first result, and within two days of onset.
get_initial_validated_cases(flu)
#> # A tibble:  3 × 10
#> # Data type: "count-cumulative"
#> # Frequency: Event: `days` | Report: `days`
#>   onset        visit      .event_num .report_num result     outcome     n .delay
#>   <date>       <date>          <dbl>       <dbl> <date>     <chr>   <dbl>  <dbl>
#>   [event_date] [report_d…      [...]       [...] [validati… [valid… [cas…  [...]
#> 1 2021-01-04   2021-01-05          0           1 2021-01-06 NA          2      1
#> 2 2021-01-05   2021-01-06          1           2 2021-01-07 NA          2      1
#> 3 2021-01-06   2021-01-07          2           3 2021-01-08 NA          1      1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2021-01-08 | Event date: "onset" | Report date: "visit"
#> # Validation date: "result" ("days") | resolved: 0/3
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 2 more variables: .validation_num <dbl>, .validation_delay <dbl>
get_nth_validated_cases(flu, delay = 2)
#> # A tibble:  3 × 10
#> # Data type: "count-cumulative"
#> # Frequency: Event: `days` | Report: `days`
#>   onset        visit      .event_num .report_num result     outcome     n .delay
#>   <date>       <date>          <dbl>       <dbl> <date>     <chr>   <dbl>  <dbl>
#>   [event_date] [report_d…      [...]       [...] [validati… [valid… [cas…  [...]
#> 1 2021-01-04   2021-01-05          0           1 2021-01-06 NA          2      1
#> 2 2021-01-05   2021-01-06          1           2 2021-01-07 NA          2      1
#> 3 2021-01-06   2021-01-07          2           3 2021-01-08 NA          1      1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2021-01-08 | Event date: "onset" | Report date: "visit"
#> # Validation date: "result" ("days") | resolved: 0/3
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 2 more variables: .validation_num <dbl>, .validation_delay <dbl>
```
