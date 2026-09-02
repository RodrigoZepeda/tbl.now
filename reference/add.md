# Set, change and remove the attributes of a `tbl_now`

**\[stable\]**

A
[`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
remembers which of its columns are the event date, the report date, the
strata, and so on. These functions edit that memory after the object has
been built – useful when new columns appear part-way through a pipeline,
or when you want to nowcast the same data broken down a different way.

There are three verbs, and they differ only in what they do to what is
already recorded:

- **`add_*()`** keeps what is there and adds to it.
  `add_strata(x, age_group)` on an object already stratified by gender
  leaves you stratified by both.

- **`change_*()`** replaces it. `change_strata(x, age_group)` on the
  same object leaves you stratified by age group *only*.

- **`remove_*()`** takes it away. `remove_all_strata(x)` forgets every
  stratum; `remove_strata(x, gender)` forgets just that one.

Nothing here touches the data itself. Renaming an attribute does not
rename, create or delete a column – it only changes which existing
column the object treats as playing that role.

## Usage

``` r
change_now(x, now = NULL, verbose = TRUE)

update_now(x, verbose = TRUE)

change_event_date(x, event_date)

change_report_date(x, report_date)

change_case_count(x, case_count)

change_is_censored_report(x, is_censored_report)

remove_is_censored_report(x)

add_is_censored_report(x, is_censored_report)

change_strata(x, ..., warn_now = TRUE, warn_non_uniqueness = TRUE)

remove_strata(x, ...)

add_strata(x, ...)

remove_all_strata(x)

change_covariates(x, ..., warn_now = TRUE, warn_non_uniqueness = TRUE)

remove_covariates(x, ...)

add_covariates(x, ...)

remove_all_covariates(x)

replace_temporal_effects(x, t_effects)

remove_temporal_effects(x)

change_is_censored_validation(x, is_censored_validation)

add_is_censored_validation(x, is_censored_validation)

remove_is_censored_validation(x)

add_validation_date(
  x,
  validation_date,
  validation_type = NULL,
  validation_units = "auto",
  validation_levels = NULL
)

change_validation_date(
  x,
  validation_date,
  validation_type = NULL,
  validation_units = "auto",
  validation_levels = NULL
)

remove_validation_date(x)
```

## Arguments

- x:

  A `tbl_now` object.

- now:

  (optional) Date or `NULL` (default). The date that is considered the
  `now` of the nowcast. If no `now` is given then the function
  automatically uses the last `event_date`.

- verbose:

  (optional) Logical. Whether to throw a message. Default = `TRUE`.

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

- ...:

  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  columns for the attribute being set. For `strata` and `covariates`
  this may name several columns at once.

- warn_now:

  Boolean. Whether to warn if `now` falls before the last report date,
  or unreasonably far into the future.

- warn_non_uniqueness:

  (optional) Logical. Whether to throw a warning if data has multiple
  observations for same event and report date (conditional on covariates
  and strata)

- t_effects:

  (optional) Either `NULL` (default), a
  [`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md)
  object or a character vector with the names of the columns containing
  the temporal effects.

- is_censored_validation:

  (optional)
  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  or `NULL` (default). The validation-axis counterpart of
  `is_censored_report`: the name of a logical column marking rows whose
  **validation delay** is a bound rather than a measurement. Requires a
  `validation_date`. See
  [censor_validation_delays_above()](https://rodrigozepeda.github.io/tbl.now/reference/censoring.md).

- validation_date:

  (optional)
  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  column holding a **third** date: the day the report was resolved.
  Influenza is the picture to keep in mind – symptoms begin (the event),
  the patient sees a doctor (the report), and days later a swab comes
  back. The assumed timeline is
  `event_date <= report_date <= validation_date <= now`. Leave `NULL`
  (the default) for the usual two-date object. See
  `add_validation_date()`.

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

## Value

A `tbl_now` object with the attribute updated. The data are returned
unchanged; only what the object records about itself differs.

## Details

Columns are chosen with
[tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html),
so a bare column name works, and so do the helpers
[`dplyr::starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
[`dplyr::all_of()`](https://tidyselect.r-lib.org/reference/all_of.html)
and
[`dplyr::where()`](https://tidyselect.r-lib.org/reference/where.html).
See
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
for the full set.

`update_now()` deserves a note of its own. The `now` of a nowcast is the
day you are standing on, and it does **not** move when you filter the
data – an object filtered down to 1992 still believes `now` is 2010,
which is usually not what you want. `update_now()` resets it to the
latest date actually present:

    data(denguedat)
    ndata <- tbl_now(denguedat,
      event_date = onset_week, report_date = report_week, verbose = FALSE
    )

    # `now` is in 2010, because the data runs that far.
    get_now(ndata)
    #> [1] "2010-12-20"

    # Filtering the data does not move it ...
    ndata_1992 <- ndata |>
      dplyr::filter(
        onset_week <= as.Date("1992/01/01") & report_week <= as.Date("1992/01/01")
      )
    get_now(ndata_1992)
    #> [1] "2010-12-20"

    # ... but `update_now()` does.
    get_now(update_now(ndata_1992))
    #> [1] "1991-12-30"

## The validation process, the optional third date

`add_validation_date()`, `change_validation_date()` and
`remove_validation_date()` set the **third** date a surveillance record
can carry: after the event happened and after it was reported, somebody
decided whether it was real. For influenza that is the laboratory result
– and it can come back negative, in which case the case is *retracted*
rather than confirmed.

Attaching one is the only verb on this page that changes more than a
name:

- **`now` moves.** A validation is an observation, so the as-of moment
  becomes the latest of the report and validation dates. Validation
  refuses an object whose `now` falls before a validation that has
  already happened.

- **Two columns appear.** `.validation_num` is the date on the same
  numeric anchor as `.event_num`/`.report_num`; `.validation_delay` is
  the time from report to resolution. Both are protected, like `.delay`.

- **Counting gains a dimension.**
  [`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md)
  groups by the validation date and outcome as well, so a confirmed and
  a retracted case on the same `(event, report)` pair stay separate
  rather than being summed together.

- **The timeline is checked.**
  `event_date <= report_date <= validation_date`; rows that break it are
  warned about, not silently accepted.

A date on its own cannot say whether the test came back positive or
negative, so leaving `validation_type` out gives every dated row `NA`
and warns.

Two optional pieces travel with the third date. `validation_levels` is a
named dictionary translating the labels in your data into the four
values `validation_type` may hold – `c(confirmado = "confirmed", ...)` –
so the recoding happens once rather than in every script. And
`add_is_censored_validation()` names a logical column marking rows whose
*validation delay* is a bound rather than a measurement, the
validation-axis twin of `add_is_censored_report()`;
[censor_validation_delays_above()](https://rodrigozepeda.github.io/tbl.now/reference/censoring.md)
sets it for you.

`change_now()` is validation-aware in both directions. Moving `now`
forward does nothing to the data; moving it **backwards**, which is how
a backtest asks what was known at an earlier date, returns every
validation dated after that moment to `"pending"` and masks its date. A
resolution that has not happened yet is not a resolution.

## See also

[`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
for setting these when the object is first built; the
[getters](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
for reading them back;
[`tbl_now_attributes()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_attributes.md)
to list them all at once;
[`add_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.md)
and
[`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md)
for calendar structure;
[update()](https://rodrigozepeda.github.io/tbl.now/reference/update.tbl_now.md)
for appending new rows rather than editing attributes.

## Examples

``` r
data(denguedat)
ndata <- tbl_now(denguedat,
  event_date = onset_week,
  report_date = report_week,
  strata = gender,
  verbose = FALSE
)

## ---- Strata: add, change, remove ------------------------------------

ndata$age_group <- sample(c("<18", "18-60", "60+"), nrow(ndata), replace = TRUE)

## `add_strata()` keeps gender and adds age group.
get_strata(add_strata(ndata, age_group))
#> [1] "age_group" "gender"   

## `change_strata()` replaces gender with age group.
get_strata(change_strata(ndata, age_group))
#> [1] "age_group"

## `remove_strata()` drops one; `remove_all_strata()` drops the lot.
get_strata(remove_strata(ndata, gender))
#> NULL
get_strata(remove_all_strata(add_strata(ndata, age_group)))
#> NULL

## ---- Covariates behave the same way ---------------------------------

# Covariates influence the nowcast but are not of interest in themselves.
ndata$temperature <- rnorm(nrow(ndata), 25, 4)
ndata$humidity <- rbeta(nrow(ndata), 0.6, 0.4)
ndata <- ndata |> add_covariates(temperature, humidity)
get_covariates(ndata)
#> [1] "temperature" "humidity"   

ndata |>
  remove_covariates(humidity) |>
  get_covariates()
#> [1] "temperature"

## ---- Pointing an attribute at a different column ---------------------

## Suppose onset was recorded a day late and you correct it. `change_event_date()`
# tells the object to use the corrected column instead.
ndata$corrected_onset <- ndata$onset_week - lubridate::days(1)
ndata <- ndata |> change_event_date(corrected_onset)
#> Warning: 52987 rows have a fractional `.delay`.
#> ℹ A fractional delay is what a converter chokes on: the two date columns are on
#>   different grids. `align_weeks()` is the fix for weekly data.
get_event_date(ndata)
#> [1] "corrected_onset"

## ---- The censoring indicators ----------------------------------------

## TRUE means the report date is only an upper bound (e.g. a backlog dump).
ndata$is_censored_report <- FALSE
ndata <- ndata |> add_is_censored_report(is_censored_report)
#> Warning: 52987 rows have a fractional `.delay`.
#> ℹ A fractional delay is what a converter chokes on: the two date columns are on
#>   different grids. `align_weeks()` is the fix for weekly data.
get_is_censored_report(ndata)
#> [1] "is_censored_report"
ndata <- remove_is_censored_report(ndata)
#> Warning: 52987 rows have a fractional `.delay`.
#> ℹ A fractional delay is what a converter chokes on: the two date columns are on
#>   different grids. `align_weeks()` is the fix for weekly data.

## ---- `now` -----------------------------------------------------------

# Set it by hand ...
get_now(change_now(ndata, now = as.Date("2011-01-01")))
#> Warning: 52987 rows have a fractional `.delay`.
#> ℹ A fractional delay is what a converter chokes on: the two date columns are on
#>   different grids. `align_weeks()` is the fix for weekly data.
#> [1] "2011-01-01"

# ... or snap it back to the latest date actually observed.
get_now(update_now(ndata))
#> Warning: 52987 rows have a fractional `.delay`.
#> ℹ A fractional delay is what a converter chokes on: the two date columns are on
#>   different grids. `align_weeks()` is the fix for weekly data.
#> [1] "2010-12-20"

## ---- Count data: which column holds the counts ------------------------

counts <- to_count(ndata, to = "count-incidence")
#> Warning: 52987 rows have a fractional `.delay`.
#> ℹ A fractional delay is what a converter chokes on: the two date columns are on
#>   different grids. `align_weeks()` is the fix for weekly data.
counts |>
  dplyr::mutate(inflated = round(1.15 * n)) |>
  change_case_count(inflated) |>
  get_case_count()
#> Warning: 52987 rows have a fractional `.delay`.
#> ℹ A fractional delay is what a converter chokes on: the two date columns are on
#>   different grids. `align_weeks()` is the fix for weekly data.
#> [1] "inflated"

## ---- The validation process, the optional third date -----------------

data(covid_us)
covid <- covid_us |>
  dplyr::filter(onset_dt >= as.Date("2020-11-01")) |>
  tbl_now(
    event_date = onset_dt, report_date = pos_spec_dt,
    case_count = n, data_type = "count-incidence",
    verbose = FALSE, warn_non_uniqueness = FALSE
  )

## Onset -> positive specimen -> registration at CDC. A date alone cannot say
# how the case resolved, so this warns until an outcome column is supplied.
covid <- suppressWarnings(add_validation_date(covid, cdc_report_dt))
get_validation_date(covid)
#> [1] "cdc_report_dt"

## CDC's own labels are not this package's four, which is what
# `validation_levels` translates.
covid <- change_validation_date(covid, cdc_report_dt,
  validation_type = current_status,
  validation_levels = c(
    "Laboratory-confirmed case" = "confirmed", "Probable Case" = "pending"
  )
)
table(covid[[get_validation_type(covid)]])
#> 
#> confirmed   pending 
#>     27273     11961 
get_validation_levels(covid)
#> Laboratory-confirmed case             Probable Case 
#>               "confirmed"                 "pending" 

## A validation delay you refuse to believe is a bound, not a measurement.
covid <- censor_validation_delays_above(covid, 45, verbose = FALSE)
get_is_censored_validation(covid)
#> [1] ".is_censored_validation"

## Dropping the third date leaves an ordinary two-date object.
has_validation(remove_validation_date(covid))
#> [1] FALSE

## ---- Temporal effects --------------------------------------------------

# Recorded lazily: `replace_*` swaps the specification, `remove_*` forgets it.
ndata <- ndata |>
  add_temporal_effects(
    t_effects = temporal_effects(week_of_year = TRUE, month_of_year = TRUE)
  )
get_temporal_effects(ndata)
#> [[1]]
#> [[1]]$t_effects
#> ── Temporal Effects ────────────────────────────────────────────────────────────
#> The following effects are in place:
#> • "month_of_year"
#> • "week_of_year"
#> 
#> [[1]]$date_type
#> [1] "event_date"
#> 
#> [[1]]$weekend_days
#> [1] "Sat" "Sun"
#> 
#> 

ndata |>
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

ndata |>
  remove_temporal_effects() |>
  get_temporal_effects()
#> list()
```
