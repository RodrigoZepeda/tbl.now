# Update a \`tbl_now\`

Updates a \`tbl_now\` object with new observations either from another
\`tbl_now\` or a \`data.frame\`

## Usage

``` r
# S3 method for class 'tbl_now'
update(
  object,
  ...,
  new_data,
  strata = "left",
  covariates = strata,
  t_effects = strata,
  now = NULL,
  remove_duplicates = NULL
)
```

## Arguments

- object:

  A \`tbl_now\` object

- ...:

  Additional arguments to pass to \`tbl_now\`

- new_data:

  Another \`tbl_now\` with the same \`strata\`, \`covariates\`,
  \`is_censored\`, and \`temporal_effects\` or a \`data.frame\` with
  additional (newer) data not present in \`x\`

- strata:

  (optional) Whether to keep the strata from \`object\` ("left"), from
  \`new_data\` ("right") or from \`both\` ("both")

- covariates:

  (optional) Whether to keep the covariates from \`object\` ("left"),
  from \`new_data\` ("right") or from \`both\` ("both")

- t_effects:

  (optional) Whether to keep the temporal_effects from \`object\`
  ("left"), from \`new_data\` ("right") or from \`both\` ("both")

- now:

  (optional) Date or `NULL` (default). The date that is considered the
  `now` of the nowcast. If no `now` is given then the function
  automatically uses the last `event_date`.

- remove_duplicates:

  Whether to remove duplicated rows from data (only applies for
  \`count\` data)

## Value

A \`tbl_now\` object with all the properties of \`object\`

## Note

By default it keeps the strata, covariates and temporal effects of
\`object\`. Use the \`strata\`, \`covariates\` and \`t_effects\`
arguments to change it.

## Examples

``` r
data(denguedat)
initial_data <- denguedat[1:500,]
update_data  <- denguedat[501:1000,]

initial_tbl <- tbl_now(denguedat, event_date = "onset_week",
                       report_date = "report_week", strata = "gender",
                       verbose = FALSE)
#> Warning: Please use a `temporal_effects` object. Setting from colnames is not
#> recommended and could lead to unexpected behaviour.

#Update collapses everything into a single data.frame
update(initial_tbl, new_data = update_data)
#> Warning: Please use a `temporal_effects` object. Setting from colnames is not
#> recommended and could lead to unexpected behaviour.
#> # A tibble:  53,487 × 6
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
#> # ℹ 53,477 more rows
```
