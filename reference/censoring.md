# Record a report or a delay as a bound rather than a fact

**\[experimental\]**

Surveillance data is full of dates that are not really dates. A case
with onset in March turns up in December; a report date is missing
altogether; a system codes "never reported" as `2222-02-22`. Deleting
those records throws away real cases, and believing them drags the
estimated delay distribution to the right until the nowcast thinks
reporting is far slower than it is.

These functions **censor** instead: the object keeps the case, but
records its delay as *"at least this long"* rather than *"exactly this
long"*.

There are two axes to censor and three ways to say which rows, so there
are six verbs. On the **reporting** axis (event date to report date, the
`is_censored_report` flag):

- `censor_reports()` – rows matching a condition get a **replacement
  report date** (the missing ones become `now`, say) and the flag.

- `censor_reporting_delays()` – the same, said as a **delay** rather
  than a date; with no replacement it only sets the flag.

- `censor_reporting_delays_above()` – **considers as censored every
  delay longer than `max_delay`**, in the object's event units, and
  leaves every other row alone. This is the one you want when the
  threshold *is* the rule: "anything that took more than 60 days is a
  lower bound, not a measurement".

On the **validation** axis (report date to resolution, the
`is_censored_validation` flag), the same three:

- `censor_validations()` – rows matching a condition get a **replacement
  validation date** and the flag.

- `censor_validation_delays()` – the same, said as a delay from the
  report.

- `censor_validation_delays_above()` – **considers as censored every
  validation delay longer than `max_delay`**, in the object's validation
  units: a laboratory result that took months is a case you have stopped
  believing the turnaround of.

## Usage

``` r
censor_reporting_delays_above(x, max_delay, verbose = TRUE)

censor_reports(x, condition, to_report = get_now(x), verbose = TRUE)

censor_reporting_delays(x, condition, to_delay = NULL, verbose = TRUE)

censor_validations(x, condition, to_validation = get_now(x), verbose = TRUE)

censor_validation_delays(x, condition, to_delay = NULL, verbose = TRUE)

censor_validation_delays_above(x, max_delay, verbose = TRUE)
```

## Arguments

- x:

  A `tbl_now` object. The three validation verbs require one that
  carries a validation process (see
  [add_validation_date()](https://rodrigozepeda.github.io/tbl.now/reference/add.md)).

- max_delay:

  Numeric. Every delay **strictly greater** than this is considered
  censored; the rest are left alone. In the object's event units for
  `censor_reporting_delays_above()`, validation units for
  `censor_validation_delays_above()`.

- verbose:

  Logical. Whether to report how many rows were affected. Default
  `TRUE`.

- condition:

  An unquoted expression evaluated in `x`, as in
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html).
  Rows where it is `TRUE` are censored.

- to_report:

  The replacement report date for the matching rows: a single value, or
  one per row of `x`. Must match the class of the report column (a
  `Date`, or a number for a numeric axis). Defaults to `get_now(x)` –
  the case has not been reported as of now, which is the whole point of
  the censoring flag. `NULL` leaves the dates alone and only sets the
  flag.

- to_delay:

  The replacement delay for the matching rows. For
  `censor_reporting_delays()` it is in the object's **event** units and
  the report date becomes `event_date + to_delay`; for
  `censor_validation_delays()` it is in **validation** units and the
  validation date becomes `report_date + to_delay`, because that is what
  `.validation_delay` measures. A single number or one per row. `NULL`
  (the default) leaves the dates alone and only sets the flag. It must
  be a **whole number** of those units, on every axis: there is no such
  date as half a day later, and a calendar axis used to bend `2.5` to
  `2` and `3.5` to `4` without saying so.

- to_validation:

  The replacement validation date for the matching rows: a single value,
  or one per row of `x`. Must match the class of the validation column.
  Defaults to `get_now(x)` – the case has not been resolved as of now.
  `NULL` leaves the dates alone and only sets the flag. Pending cases
  are skipped; see *Pending cases are skipped*.

## Value

A `tbl_now` with that axis's censoring column updated, creating it when
absent (`.is_censored_report` or `.is_censored_validation`), and with
the dates replaced where a replacement was asked for. The three
reporting verbs touch `is_censored_report` and the report date; the
three validation verbs touch `is_censored_validation` and the validation
date. Neither rewrites `validation_type`, and nothing is ever deleted.

## Details

The reporting delay is read from the generated `.delay` column (report
date minus event date, in the object's event units); the validation
delay from `.validation_delay` (validation date minus report date, in
validation units). Existing censoring flags are merged rather than
overwritten, so a delay that was already censored stays censored, and
the flag column is created (as `.is_censored_report` /
`.is_censored_validation`) when the object has none.

The threshold functions keep the case **and its date**. Nothing is
deleted and no outcome is rewritten: the flag says the delay is a bound
rather than a measurement, and it is up to the model to use that. A case
that was confirmed after 200 days is still a confirmed case, and
[get_latest_validated_cases()](https://rodrigozepeda.github.io/tbl.now/reference/validated_cases.md)
still counts it.

`condition` is evaluated inside the data, like a
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
expression, so it can name any column – including the generated
`.delay`. Rows where it comes out `NA` are **not** censored: a condition
that cannot be evaluated is not a condition that was met.

The four verbs that take a `condition` *can* move a date, and then they
rebuild the object: the generated numeric and delay columns are
recomputed, and `now` moves **forward** when a replacement lands after
it, never backwards – `now` is where you are standing, not the last date
in the data. Nothing stops a replacement from landing before the date it
is measured from; that is a negative delay, and
[`validate_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md)
says so.

Any temporal-effect column materialised **on the report date**
(`.report_*`, from
[`compute_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.md))
is dropped when the report date moves, because it describes a date that
has just changed; run
[`compute_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.md)
again to rebuild it. The `.event_*` ones are kept – the event dates
never move – and neither is touched by the validation verbs.

## Pending cases are skipped

`"pending"` means **reported and still waiting**, so a pending case has
no validation date – that is the whole difference between it and a
resolution that was never recorded. Writing a date onto one would assert
a resolution that never happened, and make the case look resolved to
everything counting arrivals on the validation axis.

So `censor_validations()` and `censor_validation_delays()` **skip
pending rows** when they would write a date, and say how many they
skipped. To censor a case that really was resolved but whose date is
missing, make sure its `validation_type` says so first. Flagging without
a replacement is not affected: no date is written, so there is nothing
to contradict.

## See also

[add_is_censored_report()](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
and
[change_is_censored_report()](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
to set the flag by hand, and
[add_is_censored_validation()](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
for the validation axis;
[`diagnose_validation_delay()`](https://rodrigozepeda.github.io/tbl.now/reference/validation_delay.md)
and
[`plot_delay_distribution()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_distribution.md)
to find the threshold worth using;
[`diagnose_truncation()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_diagnose_components.md)
for the delays that are missing rather than long;
[`complete_zeroes()`](https://rodrigozepeda.github.io/tbl.now/reference/complete_zeroes.md)
for the opposite problem.

## Examples

``` r
# Four cases, one of which took 300 days to be reported.
df <- data.frame(
  onset = as.Date("2020-01-01") + c(0, 0, 1, 2),
  reported = as.Date("2020-01-01") + c(1, 5, 2, 300)
)
tn <- tbl_now(df,
  event_date = onset, report_date = reported,
  data_type = "linelist", verbose = FALSE
)
tn$.delay
#> [1]   1   5   1 298

# Anything slower than 60 days is recorded as a lower bound, not a fact.
censored <- censor_reporting_delays_above(tn, max_delay = 60)
#> ℹ Marked 1 report with delay > 60 days as censored.
#> • This delay is now an upper bound (is_censored_report).
censored[[get_is_censored_report(censored)]]
#> [1] FALSE FALSE FALSE  TRUE

# The same rule written by hand, and capped at 60 days as well, so the
# 300-day outlier stops dominating the delay distribution.
capped <- censor_reporting_delays(tn, .delay > 60, to_delay = 60, verbose = FALSE)
capped$.delay
#> [1]  1  5  1 60

## ---- Reports that never arrived ---------------------------------------

# A missing report date, and a system that codes "never" as a date in 2222.
messy <- data.frame(
  onset = as.Date("2020-01-01") + 0:3,
  reported = as.Date(c("2020-01-03", NA, "2222-02-22", "2020-01-06"))
)
messy_now <- suppressWarnings(tbl_now(messy,
  event_date = onset, report_date = reported,
  data_type = "linelist", units = "days", verbose = FALSE,
  now = as.Date("2020-01-10")
))

# Both are "not reported yet", so both become `now` and are flagged censored.
# (Wrapped because the object keeps warning about the dates being fixed.)
fixed <- suppressWarnings(censor_reports(messy_now,
  is.na(reported) | reported > as.Date("2100-01-01"),
  verbose = FALSE
))
fixed[[get_report_date(fixed)]]
#> [1] "2020-01-03" "2020-01-10" "2020-01-10" "2020-01-06"
fixed[[get_is_censored_report(fixed)]]
#> [1] FALSE  TRUE  TRUE FALSE

## ---- The validation counterpart ----------------------------------------

cases <- data.frame(
  onset = as.Date("2021-01-04") + 0:4,
  visit = as.Date("2021-01-05") + 0:4,
  result = as.Date("2021-01-05") + 0:4 + c(1, 2, 1, 90, 2),
  outcome = rep("confirmed", 5)
)
flu <- tbl_now(cases,
  event_date = onset, report_date = visit,
  validation_date = result, validation_type = outcome,
  data_type = "linelist", verbose = FALSE
)

# That one is flagged; all five stay confirmed, and the date is kept.
flagged <- censor_validation_delays_above(flu, 30, verbose = FALSE)
flagged[[get_is_censored_validation(flagged)]]
#> [1] FALSE FALSE FALSE  TRUE FALSE
table(flagged[["outcome"]])
#> 
#> confirmed 
#>         5 

# The condition form: cap that turnaround at 30 days from the report, which
# moves the validation date to match.
capped_lab <- censor_validation_delays(flu, .validation_delay > 30,
  to_delay = 30, verbose = FALSE
)
capped_lab$.validation_delay
#> [1]  1  2  1 30  2

## A pending case has no resolution date, so there is nothing to censor --
# it is skipped rather than given a date it never had.
waiting <- flu
waiting[["outcome"]][2] <- "pending"
waiting[["result"]][2] <- as.Date(NA)
waiting <- change_validation_date(waiting, "result", "outcome")
out <- censor_validations(waiting, is.na(result), verbose = FALSE)
out[["result"]][2] # still NA
#> [1] NA
```
