# Compute temporal effects for a \`tbl_now\`

Materializes the lazy \[temporal_effects()\] specification stored in a
\`tbl_now\` into actual columns. \[add_temporal_effects()\] and the
\`t_effects\` argument of \[tbl_now()\] only \*record\* what should be
computed; this function performs the computation. If no temporal effects
have been configured the object is returned unchanged.

## Usage

``` r
compute_temporal_effects(x, overwrite = FALSE)
```

## Arguments

- x:

  A \`tbl_now\` object.

- overwrite:

  Logical. When \`TRUE\`, existing columns are silently overwritten.
  When \`FALSE\` (default) an error is thrown if a column that would be
  created already exists.

## Value

A \`tbl_now\` with the temporal-effect columns appended. The
\`temporal_effects\` attribute (the specification) is \*\*preserved\*\*
so the spec is always visible when printing; in addition a new internal
attribute \`computed_temporal_effect_cols\` records the names of the
columns that were just created.

## See also

\[add_temporal_effects()\], \[temporal_effects()\],
\[remove_temporal_effects()\]

## Examples

``` r
data(denguedat)

# 1. Define the spec (lazy — no columns are added yet)
df_now <- tbl_now(denguedat,
  event_date  = onset_week,
  report_date = report_week,
  t_effects   = temporal_effects(week_of_year = TRUE, month_of_year = TRUE),
  verbose     = FALSE)

# No temporal-effect columns yet:
".event_week_of_year" %in% names(df_now)   # FALSE
#> [1] FALSE

# 2. Materialise the columns
df_computed <- compute_temporal_effects(df_now)
".event_week_of_year" %in% names(df_computed)  # TRUE
#> [1] TRUE
```
