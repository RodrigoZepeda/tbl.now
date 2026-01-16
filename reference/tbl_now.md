# Create a `tbl_now` object

A special `tibble` class that includes information for the nowcast. See
the Attributes section for more information.

## Usage

``` r
tbl_now(
  data,
  event_date,
  report_date,
  strata = NULL,
  covariates = NULL,
  case_count = NULL,
  is_censored = NULL,
  now = NULL,
  event_units = "auto",
  report_units = "auto",
  data_type = "auto",
  t_effects = NULL,
  verbose = TRUE,
  force = FALSE,
  warn_non_uniqueness = TRUE,
  align_weeks = FALSE,
  ...
)
```

## Arguments

- data:

  A `data.frame` or `tibble` to be converted.

- event_date:

  [`` <`tidy-select`> ``](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  name of the column containing the event date.

- report_date:

  [`` <`tidy-select`> ``](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  name of the column containing the report date.

- strata:

  (optional)
  [`` <`tidy-select`> ``](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  or `NULL` (default). Name of different variables (column names) in
  strata. Strata correspond to variables that are of interest by
  themselves. For example if it is of interest to generate nowcasts by
  gender then `gender` is a `strata`.

- covariates:

  (optional)
  [`` <`tidy-select`> ``](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  or `NULL` (default). Name of different variables (column names) that
  influence the nowcast but are not strata. For example precipitation
  might influence a dengue nowcast but in general it is not of interest
  to generate nowcasts by precipitation levels.

- case_count:

  (optional)
  [`` <`tidy-select`> ``](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  or `NULL` Name of the column with the case counts if `data_type` is
  "count-incidence" or "count-cumulative".

- is_censored:

  (optional)
  [`` <`tidy-select`> ``](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  or `NULL` (default). The name of a column containing either `TRUE` or
  `FALSE` indicating whether the `report_date` is correctly specified or
  corresponds to a `batch` and thus is censored. In other words, if the
  `report_date` is accurately measured set `is_censored = FALSE` but if
  the `report_date` corresponds to an error and is only an upper bound
  of the real report date set `is_censored = TRUE`.

- now:

  (optional) Date or `NULL` (default). The date that is considered the
  `now` of the nowcast. If no `now` is given then the function
  automatically uses the last `event_date`.

- event_units:

  (optional) Character. Either "auto" (default), "days", "weeks",
  "months", "years" or "numeric".

- report_units:

  (optional) Character. Either "auto" (default), "days", "weeks",
  "months", "years" or "numeric".

- data_type:

  (optional) Character. Either "auto", "linelist" or "count-incidence"
  or "count-cumulative". See section below for an explanation on data
  types.

- t_effects:

  (optional) Either `NULL` (default), a
  [`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md)
  object or a character vector with the names of the columns containing
  the temporal effects.

- verbose:

  (optional) Logical. Whether to throw a message. Default = `TRUE`.

- force:

  (optional) Logical. Whether to force computation overwriting
  pre-existing variables. Default = `FALSE`.

- warn_non_uniqueness:

  (optional) Logical. Whether to throw a warning if data has multiple
  observations for same event and report date (conditional on covariates
  and strata)

- align_weeks:

  (optional) Logical. If both event and report units are weeks and
  `align_weeks = TRUE` it ensures that all weeks start in a Sunday so
  that week differences and `.delays` are all integer.

- ...:

  Additional metadata to be stored as attributes.

## Value

An object of class `tbl_now`.

## Attributes

The following attributes are part of a `tbl_now` and are validated by
the
[`validate_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md)
function:

- event_date:

  Name of the column refering to the event of interest.

- report_date:

  Name of the column refering to when the event of interest was
  reported.

- strata:

  Names of the columns corresponding to the strata (for modelling).

- covariates:

  Names of the columns corresponding to covariates (for modelling).

- case_count:

  Column containing the number of observations for that moment if
  `data_type` is `count-incidence` or `count-cumulative`.

- temporal_effects:

  Names of the columns refering to the temporal effects.

- now:

  Date of the `now` for a nowcast.

- is_censored:

  Column indicating whether the measurement is noisy (only upper bound)
  or not.

- event_units:

  Either `days`, `weeks`, `months`, `years` or `numeric`. Corresponds to
  the units of `event_date`

- report_units:

  Either `days`, `weeks`, `months`, `years` or `numeric`. Corresponds to
  the units of `report_date`

- repot_num:

  Column where the `report_date` was transformed to numeric values

- event_num:

  Column where the `event_date` was transformed to numeric values

- data_type:

  Either `linelist`, `count-incidence` or `count-cumulative` depending
  on whether it is linelist data or count data with incidence (each
  report date's incidence) or cumulative (overall known cases at report
  date)

You can list all `tbl_now` related attributes in a specific `tbl_now`
with
[`tbl_now_attributes()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_attributes.md).

## Data types

The following data-types are admitted at `tbl_now` objects.

*Linelist*

Each row is an individual that was reported at `report_date` as
happening at `event_date`.

    df <- data.frame(
     patient     = 1:6,
     event_date  = c(rep(as.Date("2020/09/12"), 3),
                     rep(as.Date("2020/09/13"), 3)),
     report_date = c(as.Date("2020/09/12"),
                     as.Date("2020/09/13"),
                     as.Date("2020/09/14"),
                     as.Date("2020/09/13"),
                     as.Date("2020/09/14"),
                     as.Date("2020/09/15")))
    print(df)
    #>   patient event_date report_date
    #> 1       1 2020-09-12  2020-09-12
    #> 2       2 2020-09-12  2020-09-13
    #> 3       3 2020-09-12  2020-09-14
    #> 4       4 2020-09-13  2020-09-13
    #> 5       5 2020-09-13  2020-09-14
    #> 6       6 2020-09-13  2020-09-15

*Count-incidence*

Each `report_date`-`event_date` combination contains the total number of
cases observed *exactly* at `report_date` for `event_date`.

    df <- data.frame(
     n           = c(7, 1, 9, 5, 0, 2),
     event_date  = c(rep(as.Date("2020/09/12"), 3),
                     rep(as.Date("2020/09/13"), 3)),
     report_date = c(as.Date("2020/09/12"),
                     as.Date("2020/09/13"),
                     as.Date("2020/09/14"),
                     as.Date("2020/09/13"),
                     as.Date("2020/09/14"),
                     as.Date("2020/09/15")))
    print(df)
    #>   n event_date report_date
    #> 1 7 2020-09-12  2020-09-12
    #> 2 1 2020-09-12  2020-09-13
    #> 3 9 2020-09-12  2020-09-14
    #> 4 5 2020-09-13  2020-09-13
    #> 5 0 2020-09-13  2020-09-14
    #> 6 2 2020-09-13  2020-09-15

*Count-cumulative*

Each `report_date`-`event_date` combination contains the total number of
cases observed up until `report_date` for `event_date`. The most recent
`report_date` contains the best estimation of cases happening at
`event_date`.

    df <- data.frame(
     n           = c(1,5, 8, 2, 2, 4),
     event_date  = c(rep(as.Date("2020/09/12"), 3),
                     rep(as.Date("2020/09/13"), 3)),
     report_date = c(as.Date("2020/09/12"),
                     as.Date("2020/09/13"),
                     as.Date("2020/09/14"),
                     as.Date("2020/09/13"),
                     as.Date("2020/09/14"),
                     as.Date("2020/09/15")))
    print(df)
    #>   n event_date report_date
    #> 1 1 2020-09-12  2020-09-12
    #> 2 5 2020-09-12  2020-09-13
    #> 3 8 2020-09-12  2020-09-14
    #> 4 2 2020-09-13  2020-09-13
    #> 5 2 2020-09-13  2020-09-14
    #> 6 4 2020-09-13  2020-09-15

The
[`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md)
function allows you to easily convert from between different data-types.

## Examples

``` r
# The `tbl_now` is a data.frame with additional attributes
data(denguedat)
ndata <- denguedat %>%
  tbl_now(event_date = onset_week, report_date = report_week,
    strata = gender)
#> ℹ Identified data as <linelist-data> where each observation is a test.

# You can see that it documents the `event_date`, `report_date`, `strata`,
# `covariates` as well as the `now`.
ndata
#> # A tibble:  52,987 × 6
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


#A `tbl_now` is an extension of a `tibble` which means normal
#`data.frame` operations are permitted
ndata$newcolumn <- "something"
ndata
#> # A tibble:  52,987 × 7
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week   gender   .event_num .report_num .delay newcolumn
#>    <date>       <date>        <chr>         <dbl>       <dbl>  <dbl> <chr>    
#>    [event_date] [report_date] [strata]      [...]       [...]  [...] [...]    
#>  1 1990-01-01   1990-01-01    Male              0           0      0 something
#>  2 1990-01-01   1990-01-01    Female            0           0      0 something
#>  3 1990-01-01   1990-01-01    Female            0           0      0 something
#>  4 1990-01-01   1990-01-08    Female            0           1      1 something
#>  5 1990-01-01   1990-01-08    Male              0           1      1 something
#>  6 1990-01-01   1990-01-15    Female            0           2      2 something
#>  7 1990-01-01   1990-01-15    Female            0           2      2 something
#>  8 1990-01-01   1990-01-15    Female            0           2      2 something
#>  9 1990-01-01   1990-01-22    Female            0           3      3 something
#> 10 1990-01-01   1990-01-08    Female            0           1      1 something
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "onset_week" | Report date: "report_week"
#> # Strata: "gender"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows

#Like removing a column
ndata[,-4]
#> Warning: Dropped protected column(?s): ".event_num". Returning a `tibble`
#> # A tibble: 52,987 × 6
#>    onset_week report_week gender .report_num .delay newcolumn
#>    <date>     <date>      <chr>        <dbl>  <dbl> <chr>    
#>  1 1990-01-01 1990-01-01  Male             0      0 something
#>  2 1990-01-01 1990-01-01  Female           0      0 something
#>  3 1990-01-01 1990-01-01  Female           0      0 something
#>  4 1990-01-01 1990-01-08  Female           1      1 something
#>  5 1990-01-01 1990-01-08  Male             1      1 something
#>  6 1990-01-01 1990-01-15  Female           2      2 something
#>  7 1990-01-01 1990-01-15  Female           2      2 something
#>  8 1990-01-01 1990-01-15  Female           2      2 something
#>  9 1990-01-01 1990-01-22  Female           3      3 something
#> 10 1990-01-01 1990-01-08  Female           1      1 something
#> # ℹ 52,977 more rows

#Like selecting
ndata[1:10,]
#> # A tibble:  10 × 7
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week   gender   .event_num .report_num .delay newcolumn
#>    <date>       <date>        <chr>         <dbl>       <dbl>  <dbl> <chr>    
#>    [event_date] [report_date] [strata]      [...]       [...]  [...] [...]    
#>  1 1990-01-01   1990-01-01    Male              0           0      0 something
#>  2 1990-01-01   1990-01-01    Female            0           0      0 something
#>  3 1990-01-01   1990-01-01    Female            0           0      0 something
#>  4 1990-01-01   1990-01-08    Female            0           1      1 something
#>  5 1990-01-01   1990-01-08    Male              0           1      1 something
#>  6 1990-01-01   1990-01-15    Female            0           2      2 something
#>  7 1990-01-01   1990-01-15    Female            0           2      2 something
#>  8 1990-01-01   1990-01-15    Female            0           2      2 something
#>  9 1990-01-01   1990-01-22    Female            0           3      3 something
#> 10 1990-01-01   1990-01-08    Female            0           1      1 something
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "onset_week" | Report date: "report_week"
#> # Strata: "gender"
#> # ────────────────────────────────────────────────────────────────────────────────

#You can also apply all dplyr functions:
ndata %>%
  dplyr::filter(report_week <= as.Date("1991-01-02", format = "%Y-%m-%d"))
#> # A tibble:  1,981 × 7
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week   gender   .event_num .report_num .delay newcolumn
#>    <date>       <date>        <chr>         <dbl>       <dbl>  <dbl> <chr>    
#>    [event_date] [report_date] [strata]      [...]       [...]  [...] [...]    
#>  1 1990-01-01   1990-01-01    Male              0           0      0 something
#>  2 1990-01-01   1990-01-01    Female            0           0      0 something
#>  3 1990-01-01   1990-01-01    Female            0           0      0 something
#>  4 1990-01-01   1990-01-08    Female            0           1      1 something
#>  5 1990-01-01   1990-01-08    Male              0           1      1 something
#>  6 1990-01-01   1990-01-15    Female            0           2      2 something
#>  7 1990-01-01   1990-01-15    Female            0           2      2 something
#>  8 1990-01-01   1990-01-15    Female            0           2      2 something
#>  9 1990-01-01   1990-01-22    Female            0           3      3 something
#> 10 1990-01-01   1990-01-08    Female            0           1      1 something
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "onset_week" | Report date: "report_week"
#> # Strata: "gender"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 1,971 more rows

#Removing an important column automatically transforms to tibble
#losing its property
suppressWarnings(
  ndata %>%
    dplyr::select(-onset_week)
)
#> # A tibble: 52,987 × 6
#>    report_week gender .event_num .report_num .delay newcolumn
#>    <date>      <chr>       <dbl>       <dbl>  <dbl> <chr>    
#>  1 1990-01-01  Male            0           0      0 something
#>  2 1990-01-01  Female          0           0      0 something
#>  3 1990-01-01  Female          0           0      0 something
#>  4 1990-01-08  Female          0           1      1 something
#>  5 1990-01-08  Male            0           1      1 something
#>  6 1990-01-15  Female          0           2      2 something
#>  7 1990-01-15  Female          0           2      2 something
#>  8 1990-01-15  Female          0           2      2 something
#>  9 1990-01-22  Female          0           3      3 something
#> 10 1990-01-08  Female          0           1      1 something
#> # ℹ 52,977 more rows

#Removing strata just changes the overall structure
ndata %>% dplyr::select(-gender)
#> # A tibble:  52,987 × 6
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week   .event_num .report_num .delay newcolumn
#>    <date>       <date>             <dbl>       <dbl>  <dbl> <chr>    
#>    [event_date] [report_date]      [...]       [...]  [...] [...]    
#>  1 1990-01-01   1990-01-01             0           0      0 something
#>  2 1990-01-01   1990-01-01             0           0      0 something
#>  3 1990-01-01   1990-01-01             0           0      0 something
#>  4 1990-01-01   1990-01-08             0           1      1 something
#>  5 1990-01-01   1990-01-08             0           1      1 something
#>  6 1990-01-01   1990-01-15             0           2      2 something
#>  7 1990-01-01   1990-01-15             0           2      2 something
#>  8 1990-01-01   1990-01-15             0           2      2 something
#>  9 1990-01-01   1990-01-22             0           3      3 something
#> 10 1990-01-01   1990-01-08             0           1      1 something
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "onset_week" | Report date: "report_week"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows
```
