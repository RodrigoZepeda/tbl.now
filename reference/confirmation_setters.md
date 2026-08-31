# Attach, change or drop a confirmation process

**\[experimental\]**

A confirmation is the **third** date in a surveillance record: after the
event happened and after it was reported, somebody decided whether it
was real. For influenza that is the laboratory result – and it can come
back negative, in which case the case is *retracted* rather than
confirmed.

`add_confirmation()` attaches one to an object that has none;
`change_confirmation()` replaces whatever is there;
`remove_confirmation()` drops it, leaving an ordinary two-date
`tbl_now`.

## Usage

``` r
add_confirmation(
  x,
  confirmation_date,
  confirmation_type = NULL,
  confirmation_units = "auto"
)

change_confirmation(
  x,
  confirmation_date,
  confirmation_type = NULL,
  confirmation_units = "auto"
)

remove_confirmation(x)
```

## Arguments

- x:

  A `tbl_now` object.

- confirmation_date:

  The confirmation-date column (tidy-select: a bare name or a string).

- confirmation_type:

  Optional column holding `"confirmed"`, `"retracted"` or `"pending"`.
  When you leave it out, rows with a confirmation date get `NA` and a
  warning: a date on its own cannot say whether the test came back
  positive or negative.

- confirmation_units:

  `"auto"` (default) infers the grid from the column, as `event_units`
  does.

## Value

A `tbl_now`.

## What attaching one changes

- **`now` moves.** A confirmation is an observation, so the as-of moment
  becomes the latest of the report and confirmation dates. Validation
  refuses an object whose `now` falls before a confirmation that has
  already happened.

- **Two columns appear.** `.confirmation_num` is the date on the same
  numeric anchor as `.event_num`/`.report_num`; `.confirmation_delay` is
  the time from report to resolution. Both are protected, like `.delay`.

- **Counting gains a dimension.**
  [`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md)
  groups by the confirmation date and outcome as well, so a confirmed
  and a retracted case on the same `(event, report)` pair stay separate
  rather than being summed together.

- **The timeline is checked.**
  `event_date <= report_date <= confirmation_date`; rows that break it
  are warned about, not silently accepted.

## See also

[confirmation_getters](https://rodrigozepeda.github.io/tbl.now/reference/confirmation_getters.md)
to read the attributes back;
[confirmation_counts](https://rodrigozepeda.github.io/tbl.now/reference/confirmation_counts.md)
to count confirmed, retracted and pending cases;
[confirmation_delay](https://rodrigozepeda.github.io/tbl.now/reference/confirmation_delay.md)
and
[`diagnose_confirmation_delay()`](https://rodrigozepeda.github.io/tbl.now/reference/confirmation_delay.md)
for how long resolution takes;
[`plot_confirmation_status()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_confirmation_status.md)
to see it;
[`add()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md) for
the event- and report-date attributes.

## Examples

``` r
data(hai_bucaramanga)

## specimen taken -> reported -> (here) the laboratory receipt as the
# confirmation step.
hai <- hai_bucaramanga |>
  dplyr::filter(
    !is.na(specimen_date), !is.na(report_date), !is.na(received_date)
  ) |>
  tbl_now(
    event_date = specimen_date, report_date = report_date,
    data_type = "linelist", verbose = FALSE
  )
#> Warning: 16 rows have a `report_date` before `event_date`
#> ℹ A negative reporting delay is not a delay; the two date columns may be
#>   swapped, or the rows may be data-entry errors.

hai <- suppressWarnings(add_confirmation(hai, received_date))
has_confirmation(hai)
#> [1] TRUE
get_confirmation_date(hai)
#> [1] "received_date"

# A date alone cannot say whether the case was confirmed or retracted, which
# is why the call above warns. Supplying the outcome column removes the doubt.
hai$outcome <- ifelse(seq_len(nrow(hai)) %% 10 == 0, "retracted", "confirmed")
hai <- change_confirmation(hai, received_date, confirmation_type = outcome)
#> Warning: 16 rows have a `report_date` before `event_date`
#> ℹ A negative reporting delay is not a delay; the two date columns may be
#>   swapped, or the rows may be data-entry errors.
#> Warning: 125 rows are confirmed BEFORE they were reported.
#> ℹ The timeline is `event_date <= report_date <= confirmation_date`; a negative
#>   confirmation delay is not a delay. First affected rows: 2, 3, 4, 5, and 6.
#> Warning: 16 rows have a `report_date` before `event_date`
#> ℹ A negative reporting delay is not a delay; the two date columns may be
#>   swapped, or the rows may be data-entry errors.
#> Warning: 125 rows are confirmed BEFORE they were reported.
#> ℹ The timeline is `event_date <= report_date <= confirmation_date`; a negative
#>   confirmation delay is not a delay. First affected rows: 2, 3, 4, 5, and 6.
get_confirmation_type(hai)
#> [1] "outcome"
table(hai[[get_confirmation_type(hai)]])
#> 
#> confirmed retracted 
#>       197        21 

# Dropping it leaves an ordinary two-date object.
hai <- remove_confirmation(hai)
#> Warning: 16 rows have a `report_date` before `event_date`
#> ℹ A negative reporting delay is not a delay; the two date columns may be
#>   swapped, or the rows may be data-entry errors.
#> Warning: 125 rows are confirmed BEFORE they were reported.
#> ℹ The timeline is `event_date <= report_date <= confirmation_date`; a negative
#>   confirmation delay is not a delay. First affected rows: 2, 3, 4, 5, and 6.
#> Warning: 16 rows have a `report_date` before `event_date`
#> ℹ A negative reporting delay is not a delay; the two date columns may be
#>   swapped, or the rows may be data-entry errors.
has_confirmation(hai)
#> [1] FALSE
```
