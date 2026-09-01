# Create a `tbl_now` object

**\[experimental\]**

Surveillance data arrives late. A case that happened on Monday may only
reach the surveillance system on Thursday, so counts for the most recent
days always look artificially low. *Nowcasting* corrects that artifact:
it estimates how many cases have already happened but have not been
reported yet.

To do that, a model needs two dates for every case – when it
**happened** (`event_date`) and when it was **reported** (`report_date`)
– together with the date you are standing on (`now`). `tbl_now()` takes
an ordinary `data.frame` and records which of its columns play those
roles, so you only have to say it once.

The result still behaves like a `tibble`: `dplyr` verbs, `$`, `[` and
`ggplot2` keep working, and every `tbl.now` function knows where to find
the dates without being told again.

## Usage

``` r
tbl_now(
  data,
  event_date = NULL,
  report_date = NULL,
  delay = NULL,
  strata = NULL,
  covariates = NULL,
  case_count = NULL,
  is_censored_report = NULL,
  validation_date = NULL,
  validation_type = NULL,
  validation_units = "auto",
  validation_levels = NULL,
  is_censored_validation = NULL,
  now = NULL,
  event_units = "auto",
  report_units = "auto",
  data_type = "auto",
  t_effects = character(0),
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

  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  name of the column containing the event date. Optional when `delay` is
  provided together with `report_date`; the event date will be computed
  as `report_date - delay`.

- report_date:

  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  name of the column containing the report date. Optional when `delay`
  is provided together with `event_date`; the report date will be
  computed as `event_date + delay`.

- delay:

  (optional)
  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  or `NULL` (default). Name of a numeric column containing the delay (in
  `event_units`) between `event_date` and `report_date`. When provided
  with only one of `event_date` or `report_date`, the missing date is
  reconstructed from the known date and the delay. Requires units to be
  known (either specified via `event_units` or inferrable from the
  provided date column).

- strata:

  (optional)
  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  or `NULL` (default). Name of different variables (column names) in
  strata. Strata correspond to variables that are of interest by
  themselves. For example if it is of interest to generate nowcasts by
  gender then `gender` is a `strata`.

- covariates:

  (optional)
  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  or `NULL` (default). Name of different variables (column names) that
  influence the nowcast but are not strata. For example precipitation
  might influence a dengue nowcast but in general it is not of interest
  to generate nowcasts by precipitation levels.

- case_count:

  (optional)
  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  or `NULL` Name of the column with the case counts if `data_type` is
  "count-incidence" or "count-cumulative".

- is_censored_report:

  (optional)
  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  or `NULL` (default). The name of a column containing either `TRUE` or
  `FALSE` indicating whether the `report_date` is correctly specified or
  corresponds to a `batch` and thus is censored. In other words, if the
  `report_date` is accurately measured set `is_censored_report = FALSE`
  but if the `report_date` corresponds to an error and is only an upper
  bound of the real report date set `is_censored_report = TRUE`.

- validation_date:

  (optional)
  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  column holding a **third** date: the day the report was resolved.
  Influenza is the picture to keep in mind – symptoms begin (the event),
  the patient sees a doctor (the report), and days later a swab comes
  back. The assumed timeline is
  `event_date <= report_date <= validation_date <= now`. Leave `NULL`
  (the default) for the usual two-date object. See
  [`add_validation_date()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md).

- validation_type:

  (optional)
  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  column saying what the resolution *was*: `"confirmed"`, `"retracted"`
  (it was reported, but it is not a case after all), `"pending"` or
  `NA`. **`"pending"` means reported and still waiting**, so it carries
  no validation date – which is a different thing from a result that was
  never recorded (`NA`). A validation date with no type warns rather
  than guessing, because a date alone cannot say whether the case was
  confirmed or retracted.

- validation_units:

  (optional) Character. Either `"auto"` (default), `"days"`, `"weeks"`,
  `"months"`, `"years"` or `"numeric"` – the grid the validation date
  lives on, resolved the same way as `report_units`.

- validation_levels:

  (optional) `NULL` (default) or a **named** character vector
  translating the labels in `validation_type` into the canonical
  outcomes, for data that was not recorded in English:
  `c(confirmado = "confirmed", retractado = "retracted", pendiente = "pending")`.
  The names are the labels in your data, the values are the canonical
  ones. The column is rewritten to the canonical values and the
  dictionary is kept as an attribute, readable with
  [`get_validation_levels()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md).
  Only `"confirmed"`, `"retracted"`, `"pending"` and `NA` are ever
  stored.

- is_censored_validation:

  (optional)
  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  or `NULL` (default). The validation-axis counterpart of
  `is_censored_report`: the name of a logical column marking rows whose
  **validation delay** is a bound rather than a measurement. Requires a
  `validation_date`. See
  [censor_validation_delays_above()](https://rodrigozepeda.github.io/tbl.now/reference/censor_delays_above.md).

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

  Additional metadata to be stored as attributes on the object. Use this
  for provenance you want to travel with the data – `data_source`,
  `citation`, `population` – and read it back with
  [`tbl_now_attributes()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_attributes.md).

  Because anything unmatched lands here, a misspelled argument name
  would otherwise be accepted in silence. Names close enough to a real
  argument to be a typo (`case_col` for `case_count`, `stata` for
  `strata`) warn instead; the warning is safe to ignore if the name
  really was metadata.

## Value

An object of class `tbl_now`: the input `data` as a `tibble`, carrying
extra attributes that record which columns hold the event date, report
date, strata, covariates and so on, plus the `now` of the nowcast. List
them with
[`tbl_now_attributes()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_attributes.md).

## Details

The minimum you must supply is `event_date` and `report_date` (or one of
them plus a `delay` column, from which the other is reconstructed).
Everything else is optional and can be added later with
[`add_strata()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md),
[`add_covariates()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md),
[`add_validation_date()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
and the rest of the
[`add()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
family.

Once the object exists the usual path is
[summary()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_summary.md)
to see what is in the data,
[`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
to see what is wrong with it,
[autoplot()](https://rodrigozepeda.github.io/tbl.now/reference/autoplot.tbl_now.md)
to look at it, and
[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
to fit a model.
[`vignette("tbl.now")`](https://rodrigozepeda.github.io/tbl.now/articles/tbl.now.md)
walks through that path end to end.

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

- is_censored_report:

  Column indicating whether the measurement is noisy (only upper bound)
  or not.

- event_units:

  Either `days`, `weeks`, `months`, `years` or `numeric`. Corresponds to
  the units of `event_date`

- report_units:

  Either `days`, `weeks`, `months`, `years` or `numeric`. Corresponds to
  the units of `report_date`

- data_type:

  Either `linelist`, `count-incidence` or `count-cumulative` depending
  on whether it is linelist data or count data with incidence (each
  report date's incidence) or cumulative (overall known cases at report
  date)

- validation_date:

  Name of the column with the (optional) third date: when the report was
  resolved.

- validation_type:

  Name of the column saying what that resolution was (`"confirmed"`,
  `"retracted"`, `"pending"`).

- validation_units:

  Units of `validation_date`, resolved like `report_units`.

- validation_levels:

  The (optional) dictionary translating the labels in `validation_type`
  into the canonical outcomes.

- is_censored_validation:

  Column indicating whether the *validation* delay is only a bound (the
  validation-axis counterpart of `is_censored_report`).

- computed_temporal_effect_cols:

  Names of the temporal-effect columns that have actually been
  materialised in the data by
  [`compute_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.md).

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

## See also

[`as_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md)
to convert an object created by another nowcasting package;
[`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md)
to move between linelist and aggregated count data;
[`tbl_now_attributes()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_attributes.md)
to list what the object recorded;
[`validate_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md)
and
[`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
to check it;
[summary()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_summary.md)
to describe it;
[autoplot()](https://rodrigozepeda.github.io/tbl.now/reference/autoplot.tbl_now.md)
to plot it;
[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
to fit a nowcast.

## Examples

``` r
# `denguedat` is a linelist: one row per dengue case, with the week symptoms
## began (`onset_week`) and the week the case reached the surveillance system
## (`report_week`).
data(denguedat)
head(denguedat)
#>   onset_week report_week gender
#> 1 1990-01-01  1990-01-01   Male
#> 2 1990-01-01  1990-01-01 Female
#> 3 1990-01-01  1990-01-01 Female
#> 4 1990-01-01  1990-01-08 Female
#> 5 1990-01-01  1990-01-08   Male
#> 6 1990-01-01  1990-01-15 Female

# Tell tbl.now which column plays which role. `now` defaults to the last
# event date seen in the data.
ndata <- tbl_now(denguedat,
  event_date = onset_week,
  report_date = report_week,
  strata = gender
)
#> ℹ Identified data as <linelist-data> where each observation is a test.

# Printing reports back the roles it recorded, and the `now` it chose.
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

# A `tbl_now` is still a tibble, so ordinary manipulation works ...
ndata$newcolumn <- "something"
ndata[1:10, ]
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

# ... including dplyr verbs.
ndata |>
  dplyr::filter(report_week <= as.Date("1991-01-02"))
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

# Dropping a strata column simply forgets that stratum.
ndata |> dplyr::select(-gender)
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

# But dropping a column the class depends on demotes the object back to a
## plain tibble (with a warning): without an event date it can no longer
# describe a nowcast.
suppressWarnings(
  ndata |> dplyr::select(-onset_week)
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
```
