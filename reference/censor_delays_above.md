# Treat implausibly long delays as censored rather than exact

**\[experimental\]**

Every surveillance system has a handful of records that arrive absurdly
late – a case with onset in March that turns up in December. Taken at
face value those delays drag the estimated delay distribution to the
right and make the nowcast think reporting is far slower than it is.

Rather than deleting those records (which throws away a real case) these
functions mark them as **censored**: the object keeps the case, but
records its delay as *"at least this long"* instead of *"exactly this
long"*.

- `censor_delays_above()` does this for the **reporting** delay, by
  setting the `is_censored_report` flag.

- `censor_validation_delays_above()` does the same for the
  **validation** delay – a case whose laboratory result took months to
  come back – by setting the `is_censored_validation` flag.

## Usage

``` r
censor_delays_above(x, max_delay, verbose = TRUE)

censor_validation_delays_above(x, max_delay, verbose = TRUE)
```

## Arguments

- x:

  A `tbl_now` object. `censor_validation_delays_above()` requires one
  that carries a validation process (see
  [add_validation_date()](https://rodrigozepeda.github.io/tbl.now/reference/add.md)).

- max_delay:

  Numeric. Delays strictly greater than this are censored, in the
  object's event units (reporting) or validation units (validation).

- verbose:

  Logical. Whether to report how many rows were affected. Default
  `TRUE`.

## Value

`censor_delays_above()` returns the `tbl_now` with its
`is_censored_report` column updated, creating it when absent.

`censor_validation_delays_above()` returns the `tbl_now` with its
`is_censored_validation` column updated, creating it when absent.

## Details

The reporting delay is read from the generated `.delay` column (report
date minus event date, in the object's event units); the validation
delay from `.validation_delay` (validation date minus report date, in
validation units). Existing censoring flags are merged rather than
overwritten, so a delay that was already censored stays censored.

Both functions keep the case **and its date**. Nothing is deleted and no
outcome is rewritten: the flag says the delay is a bound rather than a
measurement, and it is up to the model to use that. A case that was
confirmed after 200 days is still a confirmed case, and
[get_latest_confirmed()](https://rodrigozepeda.github.io/tbl.now/reference/validation_counts.md)
still counts it.

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
censored <- censor_delays_above(tn, max_delay = 60)
#> ℹ Marked 1 report with delay > 60 days as censored.
#> • This delay is now an upper bound (is_censored_report).
censored[[get_is_censored_report(censored)]]
#> [1] FALSE FALSE FALSE  TRUE

# The validation counterpart: a laboratory result that took 90 days.
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
```
