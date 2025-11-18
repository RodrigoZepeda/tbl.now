# Add temporal effect coding to a \`tbl_now\`

Takes a \`tbl_now\` or a \`data.frame\` and adds the temporal effects
\`t_effect\` as columns

## Usage

``` r
add_temporal_effects(x, t_effect = NULL, overwrite = FALSE, ...)

# S3 method for class 'data.frame'
add_temporal_effects(
  x,
  t_effect = NULL,
  overwrite = FALSE,
  ...,
  date_col = NULL,
  numeric_col = NULL,
  name_prefix = paste0(".", date_col)
)

# S3 method for class 'tbl_now'
add_temporal_effects(
  x,
  t_effect = NULL,
  overwrite = FALSE,
  ...,
  date_type = "event_date"
)
```

## Arguments

- x:

  A \`tbl_now\` object or a \`data.frame\`.

- t_effect:

  A \[temporal_effects()\] object codifying the temporal effects to be
  used.

- overwrite:

  If \`TRUE\` ignores that the columns already exist and overwrites
  them. If \`FALSE\` it throws an errors if the columns it is creating
  already exist (default).

- ...:

  Additional arguments (unused)

- date_col:

  The column which contains the \`\<Date\>\` values from which effects
  will be calculated. This applies to all \`temporal_effects\` except
  for \`seasonal\`.

- numeric_col:

  The column which contains the values from which the seasonal effects
  will be calculated. This applies only to seasonal effects. For
  date-related effects (such as month or day of the week) use
  \`date_col\`.

- name_prefix:

  What preffix to add to the column names

- date_type:

  Either \`event_date\` (default) or \`report_date\` to add temporal
  effects to those columns.

## Value

A \`tbl_now\` or \`data.frame\` containing all of the effects as new
columns.

## Examples

``` r
data(denguedat)

# Get disease
disease_data <- tbl_now(denguedat,
    event_date = "onset_week",
    report_date = "report_week",
    strata = "gender")
#> ℹ Identified data as linelist-data where each observation is a test.

# Add an effect for epidemiological week
add_temporal_effects(disease_data, t_effect = temporal_effects(week_of_year = TRUE))
#> # A tibble:  52,987 × 7
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week   gender   .event_num .report_num .delay
#>    <date>       <date>        <chr>         <dbl>       <dbl>  <dbl>
#>    [event_date] [report_date] [strata]      [...]       [...]  [...]
#>  1 1990-01-01   1990-01-01    Male              0           0      0
#>  2 1990-01-01   1990-01-01    Female            0           0      0
#>  3 1990-01-01   1990-01-01    Female            0           0      0
#>  4 1990-01-01   1990-01-08    Female            0           1      1
#>  5 1990-01-01   1990-01-08    Male              0           1      1
#>  6 1990-01-01   1990-01-15    Female            0           2      2
#>  7 1990-01-01   1990-01-15    Female            0           2      2
#>  8 1990-01-01   1990-01-15    Female            0           2      2
#>  9 1990-01-01   1990-01-22    Female            0           3      3
#> 10 1990-01-01   1990-01-08    Female            0           1      1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "onset_week" | Report date: "report_week"
#> # Strata: "gender"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows
#> # ℹ 1 more variable: .event_week_of_year <int>
```
