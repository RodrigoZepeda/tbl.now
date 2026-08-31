# Attach calendar effects to a `tbl_now`, and turn them into columns

**\[stable\]**

These are the second and third steps of using calendar structure in a
nowcast. First you write down which patterns you want with
[`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md);
then:

- `add_temporal_effects()` **records** that request on the object.
  Nothing is computed and no columns appear – the specification is
  stored lazily, so it survives filtering and joining without going
  stale.

- `compute_temporal_effects()` **materialises** it, building one column
  per effect from the object's dates.

The split matters because the columns depend on the data. If you
computed them first and then filtered, or changed the event-date column,
the columns would silently describe the wrong rows. Recording the
request and computing it at the end avoids that.

Call `add_temporal_effects()` more than once to accumulate several
specifications on the same object; `compute_temporal_effects()` builds
all of them.

## Usage

``` r
add_temporal_effects(x, t_effects = NULL, overwrite = FALSE, ...)

# S3 method for class 'data.frame'
add_temporal_effects(
  x,
  t_effects = NULL,
  overwrite = FALSE,
  ...,
  date_col = NULL,
  numeric_col = NULL,
  name_prefix = paste0(".", date_col),
  weekend_days = c("Sat", "Sun")
)

# S3 method for class 'tbl_now'
add_temporal_effects(
  x,
  t_effects = NULL,
  overwrite = FALSE,
  ...,
  date_type = "event_date",
  weekend_days = c("Sat", "Sun")
)

compute_temporal_effects(x, overwrite = FALSE)
```

## Arguments

- x:

  A `tbl_now` object or a `data.frame`.

- t_effects:

  A
  [`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md)
  object codifying the temporal effects to be used.

- overwrite:

  Logical. When `TRUE`, columns that already exist are overwritten. When
  `FALSE` (the default) an existing column of the same name is an error,
  so an accidental second computation cannot silently replace your data.

- ...:

  Additional arguments (unused)

- date_col:

  The column which contains the `<Date>` values from which effects will
  be calculated. This applies to all `temporal_effects` except for
  `seasonal`.

- numeric_col:

  The column which contains the values from which the seasonal effects
  will be calculated. This applies only to seasonal effects. For
  date-related effects (such as month or day of the week) use
  `date_col`.

- name_prefix:

  Character. Prefix for the names of the created columns.

- weekend_days:

  A character or numeric vector defining which days count as the
  weekend. Defaults to Saturday and Sunday.

  - Character: day names or abbreviations, case-insensitive –
    `c("Mon", "Tuesday", "wed", ...)`.

  - Numeric: integers 1-7 in
    [`lubridate::wday()`](https://lubridate.tidyverse.org/reference/day.html)
    numbering with `week_start = 1`, so **1 = Monday** and 7 = Sunday.

- date_type:

  Either `event_date` (default) or `report_date` to add temporal effects
  to those columns.

## Value

`add_temporal_effects()` returns the object with the specification
recorded. For a `tbl_now` **no columns are added**; for a plain
`data.frame`, which has nowhere to record a specification, the columns
are computed immediately.

`compute_temporal_effects()` returns a `tbl_now` with one column per
effect appended. The specification is kept, so it still prints; the
names of the columns just created are available from
[get_temporal_effect_cols()](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md).

## See also

[`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md)
to build the specification, and for what each effect means;
[replace_temporal_effects()](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
and
[remove_temporal_effects()](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
to swap or drop it;
[get_temporal_effects()](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
and
[get_temporal_effect_cols()](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
to read back the request and the columns;
[calendar_effect_plots](https://rodrigozepeda.github.io/tbl.now/reference/calendar_effect_plots.md)
to see the patterns;
[`add()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md) for
the other attribute setters.

## Examples

``` r
data(denguedat)
dengue <- tbl_now(denguedat,
  event_date = "onset_week",
  report_date = "report_week",
  strata = "gender",
  verbose = FALSE
)

# Step 1-2: say you want a week-of-year effect, and record it.
dengue <- dengue |>
  add_temporal_effects(t_effects = temporal_effects(week_of_year = TRUE))

# The request is stored, but no column has been built yet.
get_temporal_effects(dengue)
#> [[1]]
#> [[1]]$t_effects
#> ── Temporal Effects ────────────────────────────────────────────────────────────
#> The following effects are in place:
#> • "week_of_year"
#> 
#> [[1]]$date_type
#> [1] "event_date"
#> 
#> [[1]]$weekend_days
#> [1] "Sat" "Sun"
#> 
#> 
get_temporal_effect_cols(dengue)
#> character(0)

# Step 3: materialise it.
computed <- compute_temporal_effects(dengue)
get_temporal_effect_cols(computed)
#> [1] ".event_week_of_year"
head(computed[[get_temporal_effect_cols(computed)[1]]])
#> [1] 1 1 1 1 1 1
#> 52 Levels: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 ... 52

# Specifications accumulate, so you can add a second pattern ...
both <- dengue |>
  add_temporal_effects(t_effects = temporal_effects(month_of_year = TRUE))
get_temporal_effect_cols(compute_temporal_effects(both))
#> [1] ".event_week_of_year"  ".event_month_of_year"

# ... swap the whole specification for another ...
dengue |>
  replace_temporal_effects(t_effects = temporal_effects(seasons = 52)) |>
  get_temporal_effects()
#> [[1]]
#> [[1]]$t_effects
#> ── Temporal Effects ────────────────────────────────────────────────────────────
#> The following effects are in place:
#> • "season" periods: 52
#> 
#> [[1]]$date_type
#> [1] "event_date"
#> 
#> [[1]]$weekend_days
#> [1] "Sat" "Sun"
#> 
#> 

# ... or forget it entirely.
dengue |>
  remove_temporal_effects() |>
  get_temporal_effects()
#> list()
```
