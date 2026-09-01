# Confirmed, retracted and net counts per event date

**\[experimental\]**

Once a `tbl_now` carries a validation process, "how many cases were
there" has three different answers, and which one you want depends on
the question:

- **[`get_latest_reported_cases()`](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.md)**
  (the existing function) counts everything that was ever *reported*,
  whatever the laboratory later said. It is what a nowcast of the
  reporting process predicts.

- **`get_latest_confirmed()`** counts only the cases that came back
  **confirmed**. Pending and retracted cases are not counted.

- **`get_net_confirmed()`** counts **confirmed minus retracted**: the
  running total as a surveillance system would publish it, which can go
  **down** when a case is withdrawn.

That last one is the quantity a `count-cumulative` stream actually
reports, and the one diseasenowcasting's signed-increment (Skellam /
SkNB) likelihood is built for – see
`diseasenowcasting::confirmation_process()`.

## Usage

``` r
get_latest_confirmed(x)

get_net_confirmed(x)

get_nth_confirmed(x, delay)

get_initial_confirmed(x)
```

## Arguments

- x:

  A `tbl_now` with a validation process (see
  [`add_validation_date()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md)).

- delay:

  Longest validation delay to count, in the object's validation units.
  `get_nth_confirmed(x, delay = 7)` answers "how many cases per event
  date had been resolved within a week of being reported".

## Value

A `tibble` with the event-date column, the strata columns and a count
column named after the object's own `case_count` (or `n` for a line
list).

`get_initial_confirmed()` and `get_nth_confirmed()` answer the same
three questions at an earlier point in the process: what was confirmed
by the first result to arrive, and what was confirmed within a given
delay.

## Which date the count is indexed by

By the **event date**, as every other `get_*_cases()` function is. A
case confirmed three weeks after onset still belongs to the week it
began. If you want counts by validation date instead, group on
`get_validation_date(x)` yourself – that is a different question (how
busy was the laboratory) and this package does not silently answer it.

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
#>   <date>       <date>             <dbl>       <dbl>   <int>  <dbl>
#>   [event_date] [report_date]      [...]       [...] [cases]  [...]
#> 1 2021-01-04   2021-01-05             0           1       2      1
#> 2 2021-01-05   2021-01-06             1           2       2      1
#> 3 2021-01-06   2021-01-07             2           3       1      1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2021-01-08 | Event date: "onset" | Report date: "visit"
#> # ────────────────────────────────────────────────────────────────────────────────
get_latest_confirmed(flu) # only the positives
#> # A tibble: 3 × 2
#>   onset          n
#>   <date>     <dbl>
#> 1 2021-01-04     1
#> 2 2021-01-05     2
#> 3 2021-01-06     0
get_net_confirmed(flu) # positives minus withdrawals
#> # A tibble: 3 × 2
#>   onset          n
#>   <date>     <dbl>
#> 1 2021-01-04     0
#> 2 2021-01-05     2
#> 3 2021-01-06    -1

# And the same question asked at an earlier point in the process: what was
# confirmed by the first result to come back, and within one day of report.
get_initial_confirmed(flu)
#> # A tibble: 3 × 2
#>   onset          n
#>   <date>     <dbl>
#> 1 2021-01-04     0
#> 2 2021-01-05     0
#> 3 2021-01-06     0
get_nth_confirmed(flu, delay = 1)
#> # A tibble: 3 × 2
#>   onset          n
#>   <date>     <dbl>
#> 1 2021-01-04     1
#> 2 2021-01-05     2
#> 3 2021-01-06     0
```
