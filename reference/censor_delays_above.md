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
  setting the `is_censored` flag.

- `censor_confirmation_delays_above()` does the same for the
  **confirmation** delay – a case still waiting on a laboratory result
  months later is, in practice, never going to be resolved – by
  returning it to `"pending"`.

## Usage

``` r
censor_delays_above(x, max_delay, verbose = TRUE)

censor_confirmation_delays_above(x, max_delay, verbose = TRUE)
```

## Arguments

- x:

  A `tbl_now` object. `censor_confirmation_delays_above()` requires one
  that carries a confirmation process (see
  [add_confirmation()](https://rodrigozepeda.github.io/tbl.now/reference/confirmation_setters.md)).

- max_delay:

  Numeric. Delays strictly greater than this are censored, in the
  object's event units (reporting) or confirmation units (confirmation).

- verbose:

  Logical. Whether to report how many rows were affected. Default
  `TRUE`.

## Value

`censor_delays_above()` returns the `tbl_now` with its `is_censored`
column updated, creating it when absent.

`censor_confirmation_delays_above()` returns the `tbl_now` with the
offending rows' `confirmation_type` set to `"pending"` and their
confirmation date set to `NA` – a resolution you refuse to believe is
not a resolution.

## Details

The reporting delay is read from the generated `.delay` column (report
date minus event date, in the object's event units). Existing censoring
flags are merged rather than overwritten, so a report that was already
censored stays censored.

## See also

[add_is_censored()](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
and
[change_is_censored()](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
to set the flag by hand;
[`diagnose_confirmation_delay()`](https://rodrigozepeda.github.io/tbl.now/reference/confirmation_delay.md)
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
#> • This delay is now an upper bound (is_censored).
censored[[get_is_censored(censored)]]
#> [1] FALSE FALSE FALSE  TRUE

# The confirmation counterpart: a laboratory result that took 90 days.
cases <- data.frame(
  onset = as.Date("2021-01-04") + 0:4,
  visit = as.Date("2021-01-05") + 0:4,
  result = as.Date("2021-01-05") + 0:4 + c(1, 2, 1, 90, 2),
  outcome = rep("confirmed", 5)
)
flu <- tbl_now(cases,
  event_date = onset, report_date = visit,
  confirmation_date = result, confirmation_type = outcome,
  data_type = "linelist", verbose = FALSE
)

# That one goes back to "pending"; the other four stay confirmed.
table(censor_confirmation_delays_above(flu, 30, verbose = FALSE)[["outcome"]])
#> 
#> confirmed   pending 
#>         4         1 
```
