# Read what a `tbl_now` was told about itself

**\[experimental\]**

When you build a
[`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
you tell it which column is the event date, which is the report date,
which are strata, and so on. These functions read that information back.

They are how the rest of the package – and any modelling code you write
yourself – finds the right columns without hard-coding names. Rather
than assuming a column is called `onset_week`, write
`x[[get_event_date(x)]]` and your code works on any `tbl_now`.

## Usage

``` r
get_event_date(x)

get_report_date(x)

get_strata(x)

get_num_strata(x)

get_covariates(x)

get_num_covariates(x)

get_now(x)

get_report_units(x)

get_event_units(x)

get_data_type(x)

get_temporal_effects(x)

get_temporal_effect_cols(x)

get_is_censored(x)

get_case_count(x)
```

## Arguments

- x:

  A `tbl_now` object.

## Value

A column name, a count, or a metadata value, depending on the function:

- `get_event_date()`, `get_report_date()`:

  Character. The name of the column holding the date the event happened
  / was reported.

- `get_case_count()`:

  Character, or `NULL` for linelist data. The name of the column holding
  the number of cases.

- `get_strata()`, `get_covariates()`:

  Character vector of column names, or `NULL` when there are none.

- `get_num_strata()`, `get_num_covariates()`:

  Integer count, `0` when there are none.

- `get_is_censored()`:

  Character, or `NULL`. The name of the column flagging reports whose
  date is only an upper bound.

- `get_now()`:

  The `Date` (or number) the nowcast is anchored on.

- `get_event_units()`, `get_report_units()`:

  One of `"days"`, `"weeks"`, `"months"`, `"years"` or `"numeric"` – the
  grid each date lives on.

- `get_data_type()`:

  One of `"linelist"`, `"count-incidence"` or `"count-cumulative"`. See
  [`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md).

- `get_temporal_effects()`:

  The
  [`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md)
  specification the object carries, or `NULL`. This is the *request*,
  not the data.

- `get_temporal_effect_cols()`:

  Character vector of the temporal-effect columns actually materialised
  in the data by
  [`compute_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.md);
  `character(0)` when none have been.

## Details

Most of these return a **column name**, not the column itself. To get
the values, index with the name: `x[[get_event_date(x)]]`.

A getter returns `NULL` when the object was never told about that
attribute, so `is.null(get_strata(x))` is the test for "unstratified".
The two counting helpers, `get_num_strata()` and `get_num_covariates()`,
return `0` instead, which is usually easier to work with.

## See also

[`tbl_now_attributes()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_attributes.md)
to get all of them at once;
[`add()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md),
[change()](https://rodrigozepeda.github.io/tbl.now/reference/add.md) and
[remove()](https://rodrigozepeda.github.io/tbl.now/reference/add.md) to
set them;
[confirmation_getters](https://rodrigozepeda.github.io/tbl.now/reference/confirmation_getters.md)
for the third-date attributes;
[get_latest_reported_cases()](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.md)
and friends for reading the counts rather than the metadata.

## Examples

``` r
data(denguedat)
ndata <- denguedat |>
  tbl_now(
    event_date = onset_week,
    report_date = report_week,
    strata = gender,
    t_effects = temporal_effects(month_of_year = TRUE),
    verbose = FALSE
  ) |>
  compute_temporal_effects()

# The two dates every nowcast needs.
get_event_date(ndata)
#> [1] "onset_week"
get_report_date(ndata)
#> [1] "report_week"

# Use the name to reach the column, so the code does not depend on it.
head(ndata[[get_event_date(ndata)]])
#> [1] "1990-01-01" "1990-01-01" "1990-01-01" "1990-01-01" "1990-01-01"
#> [6] "1990-01-01"

# Strata are groups you want separate nowcasts for; covariates are not.
get_strata(ndata)
#> [1] "gender"
get_num_strata(ndata)
#> [1] 1

## Nothing was declared a covariate, so this is NULL (and the count is 0).
get_covariates(ndata)
#> NULL
get_num_covariates(ndata)
#> [1] 0

# Likewise for a censoring indicator that was never supplied.
get_is_censored(ndata)
#> NULL

# The as-of moment, and the calendar grid the dates live on.
get_now(ndata)
#> [1] "2010-12-20"
get_event_units(ndata)
#> [1] "weeks"
get_report_units(ndata)
#> [1] "weeks"

# Linelist means one row per case; there is no count column yet.
get_data_type(ndata)
#> [1] "linelist"
get_case_count(ndata)
#> NULL

## After to_count() there is one.
counts <- to_count(ndata, to = "count-incidence")
get_data_type(counts)
#> [1] "count-incidence"
get_case_count(counts)
#> [1] "n"

# The temporal-effects request, versus the columns it actually produced.
get_temporal_effects(ndata)
#> [[1]]
#> [[1]]$t_effects
#> ── Temporal Effects ────────────────────────────────────────────────────────────
#> The following effects are in place:
#> • "month_of_year"
#> 
#> [[1]]$date_type
#> [1] "event_date"
#> 
#> [[1]]$weekend_days
#> [1] "Sat" "Sun"
#> 
#> 
get_temporal_effect_cols(ndata)
#> [1] ".event_month_of_year"
```
