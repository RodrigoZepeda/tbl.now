# Confirmation attributes of a `tbl_now`

**\[experimental\]**

A `tbl_now` may carry a **third** date beyond the event and the report:
the date a case was resolved, either confirmed or retracted. Think of
influenza: symptom onset is the event, the medical visit is the report,
and the laboratory result is the confirmation – which can come back
negative, in which case the case is *retracted* rather than confirmed.

These getters return the **column names** the object was told about, not
the data. Get the values with `x[[get_confirmation_date(x)]]`, as with
every other getter in the package.

## Usage

``` r
get_confirmation_date(x)

get_confirmation_type(x)

get_confirmation_units(x)

has_confirmation(x)
```

## Arguments

- x:

  A `tbl_now` object.

## Value

- `get_confirmation_date()` – the confirmation-date column name, or
  `NULL`.

- `get_confirmation_type()` – the outcome column name, or `NULL`.

- `get_confirmation_units()` – `"days"`, `"weeks"`, `"months"`,
  `"years"`, `"numeric"`, or `NULL` when the object carries no
  confirmation.

- `has_confirmation()` – `TRUE` when the object carries a confirmation
  date.

## See also

[add_confirmation()](https://rodrigozepeda.github.io/tbl.now/reference/confirmation_setters.md)
to attach one;
[get_latest_confirmed()](https://rodrigozepeda.github.io/tbl.now/reference/confirmation_counts.md)
and
[get_net_confirmed()](https://rodrigozepeda.github.io/tbl.now/reference/confirmation_counts.md)
to count the outcomes;
[confirmation_delay](https://rodrigozepeda.github.io/tbl.now/reference/confirmation_delay.md)
for how long resolution takes;
[nowcast_data_getters](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
for the event- and report-date attributes.

## Examples

``` r
data(hai_bucaramanga)

# Specimen taken -> received at the laboratory -> result reported.
hai <- hai_bucaramanga |>
  dplyr::filter(!is.na(specimen_date), !is.na(report_date)) |>
  tbl_now(
    event_date = specimen_date,
    report_date = report_date,
    data_type = "linelist",
    verbose = FALSE
  )
#> Warning: 88 rows have a `report_date` before `event_date`
#> ℹ A negative reporting delay is not a delay; the two date columns may be
#>   swapped, or the rows may be data-entry errors.

# No third date was declared, so there is no confirmation process ...
has_confirmation(hai)
#> [1] FALSE
get_confirmation_date(hai)
#> NULL

# ... until one is attached. Here the laboratory receipt plays that role.
hai <- suppressWarnings(add_confirmation(hai, received_date))
has_confirmation(hai)
#> [1] TRUE
get_confirmation_date(hai)
#> [1] "received_date"
get_confirmation_units(hai)
#> [1] "days"

# As always, the getter gives you the column NAME; index to get the values.
head(hai[[get_confirmation_date(hai)]])
#> [1] "2018-10-01" "2018-01-27" "2018-01-27" "2018-04-20" "2018-01-22"
#> [6] "2018-01-02"
```
