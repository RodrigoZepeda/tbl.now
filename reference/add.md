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
change_now(x, now = NULL)

update_now(x)

change_event_date(x, event_date)

change_report_date(x, report_date)

change_case_count(x, case_count)

change_is_censored(x, is_censored)

remove_is_censored(x)

add_is_censored(x, is_censored)

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
```

## Arguments

- x:

  A `tbl_now` object.

- now:

  (optional) Date or `NULL` (default). The date that is considered the
  `now` of the nowcast. If no `now` is given then the function
  automatically uses the last `event_date`.

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

- is_censored:

  (optional)
  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  or `NULL` (default). The name of a column containing either `TRUE` or
  `FALSE` indicating whether the `report_date` is correctly specified or
  corresponds to a `batch` and thus is censored. In other words, if the
  `report_date` is accurately measured set `is_censored = FALSE` but if
  the `report_date` corresponds to an error and is only an upper bound
  of the real report date set `is_censored = TRUE`.

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

## See also

[`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
for setting these when the object is first built; the
[getters](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
for reading them back;
[`tbl_now_attributes()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_attributes.md)
to list them all at once;
[add_confirmation()](https://rodrigozepeda.github.io/tbl.now/reference/confirmation_setters.md)
for the third-date attributes;
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
get_event_date(ndata)
#> [1] "corrected_onset"

## ---- The censoring indicator -----------------------------------------

## TRUE means the report date is only an upper bound (e.g. a backlog dump).
ndata$is_censored <- FALSE
ndata <- ndata |> add_is_censored(is_censored)
get_is_censored(ndata)
#> [1] "is_censored"
ndata <- remove_is_censored(ndata)

## ---- `now` -----------------------------------------------------------

# Set it by hand ...
get_now(change_now(ndata, now = as.Date("2011-01-01")))
#> [1] "2011-01-01"

# ... or snap it back to the latest date actually observed.
get_now(update_now(ndata))
#> [1] "2010-12-20"

## ---- Count data: which column holds the counts ------------------------

counts <- to_count(ndata, to = "count-incidence")
counts |>
  dplyr::mutate(inflated = round(1.15 * n)) |>
  change_case_count(inflated) |>
  get_case_count()
#> [1] "inflated"

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
