# Create a \`tbl_now\` object

A special \`data.frame\` class that includes information for the
nowcast. See the Attributes section for more information.

## Usage

``` r
new_tbl_now(
  data,
  event_date,
  report_date,
  strata = NULL,
  covariates = NULL,
  is_batched = NULL,
  now = NULL,
  event_units = "auto",
  report_units = "auto",
  data_type = "auto",
  verbose = TRUE,
  force = FALSE,
  ...
)

tbl_now(
  data,
  event_date,
  report_date,
  strata = NULL,
  covariates = NULL,
  is_batched = NULL,
  now = NULL,
  event_units = "auto",
  report_units = "auto",
  data_type = "auto",
  verbose = TRUE,
  ...
)
```

## Arguments

- data:

  A \`data.frame\` to be converted.

- event_date:

  Character. The name of the column containing the event date.

- report_date:

  Character. The name of the column containing the report date.

- strata:

  (optional) Character vector or \`NULL\` (default). Name of different
  variables (column names) in strata. Strata correspond to variables
  that are of interest by themselves. For example if it is of interest
  to generate nowcasts by gender then \`gender\` is a \`strata\`.

- covariates:

  (optional) Character vector or \`NULL\` (default). Name of different
  variables (column names) that influence the nowcast but are not
  strata. For example precipitation might influence a dengue nowcast but
  in general it is not of interest to generate nowcasts by precipitation
  levels.

- is_batched:

  (optional) Character or \`NULL\` (default). The name of a column
  containing either \`TRUE\` or \`FALSE\` indicating whether the
  \`report_date\` is correctly specified or corresponds to a \`batch\`
  and thus is censored. In other words, if the \`report_date\` is
  accurately measured set \`report_date = TRUE\` but if the
  \`report_date\` corresponds to an error and is only an upper bound of
  the real, idealized, report date set \`is_batched = TRUE\`.

- now:

  (optional) Date or \`NULL\` (default). The date that is considered the
  \`now\` of the nowcast. If no \`now\` is given then the function
  automatically uses the last \`event_date\`.

- event_units:

  (optional) Character. Either "auto" (default), "days", "weeks",
  "months", "years" or "numeric".

- report_units:

  (optional) Character. Either "auto" (default), "days", "weeks",
  "months", "years" or "numeric".

- data_type:

  (optional) Character. Either "auto", "linelist" or "count".

- verbose:

  (optional) Logical. Whether to throw a message. Default = \`TRUE\`.

- force:

  (optional) Logical. Whether to force computation overwriting
  pre-existing variables. Default = \`FALSE\`.

- ...:

  Additional metadata to be stored as attributes.

## Value

An object of class \`tbl_now\`.

## Attributes

The following attributes are part of a \`tbl_now\` and are validated by
the \[validate_tbl_now()\] function:

- event_date:

  Name of the column refering to the event of interest.

- report_date:

  Name of the column refering to when the event of interest was
  reported.

- strata:

  Names of the columns corresponding to the strata (for modelling).

- num_strata:

  Number of strata. Corresponds to \`length(strata)\`.

- covariates:

  Names of the columns corresponding to covariates (for modelling).

- num_covariates:

  Number of covariates Corresponds to \`length(covariates)\`.

- now:

  Date of the \`now\` for a nowcast.

- is_batched:

  Column indicating whether the measurement is noisy (only upper bound)
  or not.

- event_units:

  Either \`days\`, \`weeks\`, \`months\`, \`years\` or \`numeric\`.
  Corresponds to the units of \`event_date\`

- report_units:

  Either \`days\`, \`weeks\`, \`months\`, \`years\` or \`numeric\`.
  Corresponds to the units of \`report_date\`

- repot_num:

  Column where the \`report_date\` was transformed to numeric values

- event_num:

  Column where the \`event_date\` was transformed to numeric values

- data_type:

  Either \`linelist\` or \`count\` depending on whether it is linelist
  data or count data

## Examples

``` r
# The `tbl_now` is a data.frame with additional attributes
data(denguedat)
ndata <- tbl_now(denguedat,
    event_date = "onset_week",
    report_date = "report_week",
    strata = "gender")
#> ℹ Identified data as linelist-data where each observation is a test.

# You can see that it documents the `event_date`, `report_date`, `strata`,
# `covariates` as well as the `now`.
ndata
#> # A tibble:  52,987 × 5
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week   gender   .event_num .report_num
#>    <date>       <date>        <chr>         <dbl>       <dbl>
#>    [event_date] [report_date] [strata]      [...]       [...]
#>  1 1990-01-01   1990-01-01    Male              0           0
#>  2 1990-01-01   1990-01-01    Female            0           0
#>  3 1990-01-01   1990-01-01    Female            0           0
#>  4 1990-01-01   1990-01-08    Female            0           1
#>  5 1990-01-01   1990-01-08    Male              0           1
#>  6 1990-01-01   1990-01-15    Female            0           2
#>  7 1990-01-01   1990-01-15    Female            0           2
#>  8 1990-01-01   1990-01-15    Female            0           2
#>  9 1990-01-01   1990-01-22    Female            0           3
#> 10 1990-01-01   1990-01-08    Female            0           1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "onset_week" | Report date: "report_week"
#> # Strata: "gender"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows


#A `tbl_now` is an extension of a `tibble` which means normal
#`data.frame` operations are permitted
ndata$newcolumn <- "something"
ndata
#> # A tibble:  52,987 × 6
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week   gender   .event_num .report_num newcolumn
#>    <date>       <date>        <chr>         <dbl>       <dbl> <chr>    
#>    [event_date] [report_date] [strata]      [...]       [...] [...]    
#>  1 1990-01-01   1990-01-01    Male              0           0 something
#>  2 1990-01-01   1990-01-01    Female            0           0 something
#>  3 1990-01-01   1990-01-01    Female            0           0 something
#>  4 1990-01-01   1990-01-08    Female            0           1 something
#>  5 1990-01-01   1990-01-08    Male              0           1 something
#>  6 1990-01-01   1990-01-15    Female            0           2 something
#>  7 1990-01-01   1990-01-15    Female            0           2 something
#>  8 1990-01-01   1990-01-15    Female            0           2 something
#>  9 1990-01-01   1990-01-22    Female            0           3 something
#> 10 1990-01-01   1990-01-08    Female            0           1 something
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "onset_week" | Report date: "report_week"
#> # Strata: "gender"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows

#Like removing a column
ndata <- ndata[,-4]
#> Warning: Dropped protected column(?s): ".event_num". Returning a `tibble`
ndata
#> # A tibble: 52,987 × 5
#>    onset_week report_week gender .report_num newcolumn
#>    <date>     <date>      <chr>        <dbl> <chr>    
#>  1 1990-01-01 1990-01-01  Male             0 something
#>  2 1990-01-01 1990-01-01  Female           0 something
#>  3 1990-01-01 1990-01-01  Female           0 something
#>  4 1990-01-01 1990-01-08  Female           1 something
#>  5 1990-01-01 1990-01-08  Male             1 something
#>  6 1990-01-01 1990-01-15  Female           2 something
#>  7 1990-01-01 1990-01-15  Female           2 something
#>  8 1990-01-01 1990-01-15  Female           2 something
#>  9 1990-01-01 1990-01-22  Female           3 something
#> 10 1990-01-01 1990-01-08  Female           1 something
#> # ℹ 52,977 more rows

#Like selecting
ndata[1:10,]
#> # A tibble: 10 × 5
#>    onset_week report_week gender .report_num newcolumn
#>    <date>     <date>      <chr>        <dbl> <chr>    
#>  1 1990-01-01 1990-01-01  Male             0 something
#>  2 1990-01-01 1990-01-01  Female           0 something
#>  3 1990-01-01 1990-01-01  Female           0 something
#>  4 1990-01-01 1990-01-08  Female           1 something
#>  5 1990-01-01 1990-01-08  Male             1 something
#>  6 1990-01-01 1990-01-15  Female           2 something
#>  7 1990-01-01 1990-01-15  Female           2 something
#>  8 1990-01-01 1990-01-15  Female           2 something
#>  9 1990-01-01 1990-01-22  Female           3 something
#> 10 1990-01-01 1990-01-08  Female           1 something
ndata
#> # A tibble: 52,987 × 5
#>    onset_week report_week gender .report_num newcolumn
#>    <date>     <date>      <chr>        <dbl> <chr>    
#>  1 1990-01-01 1990-01-01  Male             0 something
#>  2 1990-01-01 1990-01-01  Female           0 something
#>  3 1990-01-01 1990-01-01  Female           0 something
#>  4 1990-01-01 1990-01-08  Female           1 something
#>  5 1990-01-01 1990-01-08  Male             1 something
#>  6 1990-01-01 1990-01-15  Female           2 something
#>  7 1990-01-01 1990-01-15  Female           2 something
#>  8 1990-01-01 1990-01-15  Female           2 something
#>  9 1990-01-01 1990-01-22  Female           3 something
#> 10 1990-01-01 1990-01-08  Female           1 something
#> # ℹ 52,977 more rows

#You can also apply all dplyr functions:
ndata %>%
  dplyr::filter(report_week <= as.Date("1991-01-02", format = "%Y-%m-%d"))
#> # A tibble: 1,981 × 5
#>    onset_week report_week gender .report_num newcolumn
#>    <date>     <date>      <chr>        <dbl> <chr>    
#>  1 1990-01-01 1990-01-01  Male             0 something
#>  2 1990-01-01 1990-01-01  Female           0 something
#>  3 1990-01-01 1990-01-01  Female           0 something
#>  4 1990-01-01 1990-01-08  Female           1 something
#>  5 1990-01-01 1990-01-08  Male             1 something
#>  6 1990-01-01 1990-01-15  Female           2 something
#>  7 1990-01-01 1990-01-15  Female           2 something
#>  8 1990-01-01 1990-01-15  Female           2 something
#>  9 1990-01-01 1990-01-22  Female           3 something
#> 10 1990-01-01 1990-01-08  Female           1 something
#> # ℹ 1,971 more rows

#Removing an important column automatically transforms to tibble
#losing its property
suppressWarnings(
  ndata %>%
    dplyr::select(-onset_week)
)
#> # A tibble: 52,987 × 4
#>    report_week gender .report_num newcolumn
#>    <date>      <chr>        <dbl> <chr>    
#>  1 1990-01-01  Male             0 something
#>  2 1990-01-01  Female           0 something
#>  3 1990-01-01  Female           0 something
#>  4 1990-01-08  Female           1 something
#>  5 1990-01-08  Male             1 something
#>  6 1990-01-15  Female           2 something
#>  7 1990-01-15  Female           2 something
#>  8 1990-01-15  Female           2 something
#>  9 1990-01-22  Female           3 something
#> 10 1990-01-08  Female           1 something
#> # ℹ 52,977 more rows

#Removing strata just changes the overall structure
ndata %>% dplyr::select(-gender)
#> # A tibble: 52,987 × 4
#>    onset_week report_week .report_num newcolumn
#>    <date>     <date>            <dbl> <chr>    
#>  1 1990-01-01 1990-01-01            0 something
#>  2 1990-01-01 1990-01-01            0 something
#>  3 1990-01-01 1990-01-01            0 something
#>  4 1990-01-01 1990-01-08            1 something
#>  5 1990-01-01 1990-01-08            1 something
#>  6 1990-01-01 1990-01-15            2 something
#>  7 1990-01-01 1990-01-15            2 something
#>  8 1990-01-01 1990-01-15            2 something
#>  9 1990-01-01 1990-01-22            3 something
#> 10 1990-01-01 1990-01-08            1 something
#> # ℹ 52,977 more rows
```
