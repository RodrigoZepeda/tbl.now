# tbl.now — AI Agent Skill Guide

How to **use** the `tbl.now` R package correctly. This is a
task-oriented guide (“how do I …”); a condensed API reference is at the
end.

------------------------------------------------------------------------

## What `tbl.now` is (mental model)

`tbl.now` provides the **`tbl_now`** S3 class: a `tibble` extension for
epidemiological **nowcasting** that carries **two time indices at once**
— `event_date` (when something happened) and `report_date` (when it was
recorded) — plus modelling metadata (strata, covariates, temporal
effects, `now`, units, data type). It is fully **dplyr-compatible**.

`tbl.now` is a **data-structure + specification layer**, *not* a
modelling engine. It standardises/validates data and stores lazy specs;
the actual nowcasting is done downstream by **`diseasenowcasting`**.

Every `tbl_now` auto-computes three **protected** numeric columns:

| Column        | Meaning                                                 |
|---------------|---------------------------------------------------------|
| `.event_num`  | `event_date` as a number, anchored at `min(event_date)` |
| `.report_num` | `report_date` as a number, same anchor                  |
| `.delay`      | `.report_num - .event_num` (the reporting delay)        |

Removing a protected column downgrades the object back to a plain tibble
(with a warning). Protected columns also include `event_date`,
`report_date`, and `case_count` (for count types).

``` r

library(tbl.now)
library(dplyr)
```

------------------------------------------------------------------------

## Skill: create a `tbl_now`

`event_date`/`report_date` accept **tidy-select** (bare names *or*
strings). Units and data type are inferred by default (`"auto"`).

``` r

# Linelist — one row per observed case (no counts column)
tn <- tbl_now(
  denguedat,
  event_date  = onset_week,      # or "onset_week"
  report_date = report_week,
  strata      = gender,
  verbose     = FALSE
)

# Count data — one row per (event, report[, strata]) with a counts column
tn <- tbl_now(
  mpoxdat,
  event_date  = dx_date,
  report_date = dx_report_date,
  case_count  = n,               # REQUIRED for count data types
  strata      = race,
  data_type   = "count-incidence",
  verbose     = FALSE
)
```

Useful arguments:

- `now =` — the “as-of” date for the nowcast. Defaults to
  `max(report_date)`.
- `event_units` / `report_units` —
  `"days" | "weeks" | "months" | "years" | "numeric"`; `"auto"` infers
  from spacing. `report_units` must be **coarser than or equal to**
  `event_units`.
- `align_weeks = TRUE` — for weekly data, snaps dates so `.delay` is
  integer (see below).
- You may pass **`.delay` + one date** instead of both dates; the
  missing date is reconstructed.

Always check it worked:

``` r

is_tbl_now(tn)            # TRUE
validate_tbl_now(tn)      # errors if invalid
tn                        # print() shows the footer with all attributes
```

------------------------------------------------------------------------

## Skill: tell which data type a dataset is

Three data types:

| Type | One row = | Counts column? |
|----|----|----|
| `"linelist"` | a single individual observation | none |
| `"count-incidence"` | count reported **exactly on** that `report_date` | yes |
| `"count-cumulative"` | **cumulative** count up to that `report_date` | yes |

To find out, build with `data_type = "auto"` and read it back:

``` r

get_data_type(tn)
```

The inference heuristic (`infer_data_type`):

1.  **No `case_count` column** → `"linelist"`.
2.  With a `case_count` column, group by `event_date` (+ strata), order
    by `report_date`, and difference the counts:
    - any **decrease** → `"count-incidence"` (incremental counts go up
      and down);
    - **monotonically non-decreasing** → `"count-cumulative"`.

If the auto guess is wrong (e.g. genuinely cumulative data that happens
to never decrease in a small sample), pass `data_type =` explicitly.

------------------------------------------------------------------------

## Skill: convert between data types (`to_count`)

``` r

to_count(x, to = "count-incidence")
to_count(x, to = "count-cumulative")
```

**Conversion is one-directional — toward more aggregation only:**

    linelist  ──►  count-incidence  ──►  count-cumulative

Supported: `linelist→incidence`, `linelist→cumulative`,
`incidence→cumulative`, and re-aggregation within the same type. **Not
supported** (errors):

- `count-cumulative → count-incidence` (cannot de-accumulate here)
- `count-* → linelist` (cannot un-count aggregated data)

So decide the granularity early. Keep a linelist if you might need
incidence later.

------------------------------------------------------------------------

## Skill: initial vs latest reported cases

Two views of “how many cases occurred on each `event_date`”:

``` r

get_initial_reported_cases(tn)   # the FIRST reported count per event_date
get_latest_reported_cases(tn)    # the MOST-RECENT reported count per event_date
```

- **`get_initial_reported_cases`** = what was *first* observed for each
  event date (the naive, most-incomplete incidence).
- **`get_latest_reported_cases`** = the best current estimate given
  everything reported up to `now`. This is the de-facto **“truth so
  far”** used for plotting the observed epidemic curve and for scoring
  nowcasts.

Both return a `count-cumulative`-style `tbl_now` collapsed to one row
per `event_date` (× strata). The gap between them is exactly what a
nowcast predicts.

------------------------------------------------------------------------

## Skill: add / change / remove strata and covariates

- **strata** = variables the nowcast is computed *separately* for (age
  group, sex, region).
- **covariates** = predictors that may improve the nowcast
  (e.g. weather), not split-by.

All accept tidy-select. **Adders** append, **changers** replace the
whole set, **removers** drop:

``` r

# strata
tn <- add_strata(tn, age_group, region)   # append
tn <- change_strata(tn, sex)              # replace the strata set with just `sex`
tn <- remove_strata(tn, region)           # drop one
tn <- remove_all_strata(tn)

# covariates
tn <- add_covariates(tn, humidity)
tn <- change_covariates(tn, temperature, humidity)
tn <- remove_covariates(tn, temperature)
tn <- remove_all_covariates(tn)

get_strata(tn);     get_num_strata(tn)
get_covariates(tn); get_num_covariates(tn)
```

A strata/covariate column **must exist in the data**. Strata should be
integer, character, or factor.

------------------------------------------------------------------------

## Skill: mark whether data is right-censored

`is_censored` flags reports that arrive in artificial **batches**
representing right-censoring rather than true reporting dynamics (e.g. a
lab outage where a week of reports all land on the same later day). The
`is_censored` attribute stores the **name of a logical column**
(`TRUE`/`FALSE` per row).

``` r

df <- df %>% mutate(was_batched = report_date == as.Date("2021-03-15"))
tn <- tbl_now(df, event_date = onset, report_date = reported,
              is_censored = was_batched, verbose = FALSE)

# or after creation:
tn <- add_is_censored(tn, was_batched)     # errors if one is already set
tn <- change_is_censored(tn, was_batched)  # replace (pass NULL to clear)
tn <- remove_is_censored(tn)

get_is_censored(tn)   # column name, or NULL if not censored
```

To tell whether a dataset is censored: `is.null(get_is_censored(tn))` →
`TRUE` means *no* censoring indicator is set. The censoring column
itself must be logical or
[`validate_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md)
rejects it.

------------------------------------------------------------------------

## Skill: temporal effects (lazy, two-step)

Temporal effects (calendar covariates and Fourier seasonality) are
stored **lazily** as a spec, then **materialised** into columns only on
demand.

``` r

# 1) build a spec
spec <- temporal_effects(
  day_of_week   = FALSE,
  weekend       = FALSE,
  day_of_month  = FALSE,
  month_of_year = TRUE,
  week_of_year  = TRUE,
  seasons       = integer(0),   # Fourier periods, e.g. c(7, 52, 365)
  season_length = 1,            # multiply each season; period = seasons * season_length
  holidays      = NULL          # an almanac::rcalendar(), see next skill
)

# 2) attach (NO columns created yet). date_type = "event_date" (default) or "report_date"
tn <- add_temporal_effects(tn, spec, date_type = "event_date")

# 3) materialise the columns when ready for modelling
tn <- compute_temporal_effects(tn, overwrite = FALSE)

get_temporal_effects(tn)       # list of specs (length 0 = none attached)
get_temporal_effect_cols(tn)   # character(0) before compute; column names after
```

- [`add_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.md)
  can be called repeatedly (appends specs); you can mix
  `date_type = "event_date"` and `"report_date"`.
- **Seasonality / Fourier**: `seasons` are the cycle periods. For
  *daily* data with weekly seasonality use
  `seasons = 52, season_length = 7` (period = 364 days); `season_length`
  defaults to `1` (period = `seasons`).
- Replace or clear:

``` r

tn <- replace_temporal_effects(tn, temporal_effects(week_of_year = TRUE))  # drops old computed cols
tn <- remove_temporal_effects(tn)                                          # clears spec + cols
```

> **Order matters:**
> [`compute_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/compute_temporal_effects.md)
> before any
> [`add_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.md)
> is a no-op.
> [`replace_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/remove.md)
> removes the computed columns — call
> [`compute_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/compute_temporal_effects.md)
> again afterward.

------------------------------------------------------------------------

## Skill: calendar effects for ANY country (custom almanac holidays)

Holidays use the [`almanac`](https://davisvaughan.github.io/almanac/)
package (v1.0). `temporal_effects(holidays = ...)` accepts any
`almanac_rcalendar`. Do **not** restrict yourself to
[`almanac::cal_us_federal()`](https://rdrr.io/pkg/almanac/man/cal_us_federal.html)
— build your own calendar from recurring rules with
`rholiday(rschedule, name)` and
[`rcalendar()`](https://rdrr.io/pkg/almanac/man/rcalendar.html).

``` r

library(almanac)

# A fixed-date holiday: Mexican Independence Day, every Sep 16
indep <- yearly() %>%
  recur_on_month_of_year("Sep") %>%
  recur_on_day_of_month(16)

# Combine custom + built-in holidays into one calendar
mx_calendar <- rcalendar(
  rholiday(indep, "Mexican Independence Day"),
  hol_new_years_day(),
  hol_christmas()
)

# Sanity-check which dates are holidays
almanac::alma_in(as.Date(c("2023-09-16", "2023-12-25", "2023-06-01")), mx_calendar)
#> TRUE TRUE FALSE

# Use it as a temporal effect
tn <- tn %>%
  add_temporal_effects(temporal_effects(holidays = mx_calendar)) %>%
  compute_temporal_effects()
```

Building blocks:
[`yearly()`](https://rdrr.io/pkg/almanac/man/rrule.html),
[`monthly()`](https://rdrr.io/pkg/almanac/man/rrule.html),
[`weekly()`](https://rdrr.io/pkg/almanac/man/rrule.html) recurrence
rules refined with
[`recur_on_day_of_month()`](https://rdrr.io/pkg/almanac/man/recur_on_day_of_month.html),
[`recur_on_month_of_year()`](https://rdrr.io/pkg/almanac/man/recur_on_month_of_year.html),
[`recur_on_day_of_week()`](https://rdrr.io/pkg/almanac/man/recur_on_day_of_week.html),
etc.; wrap each rule in
[`rholiday()`](https://rdrr.io/pkg/almanac/man/rholiday.html); combine
with [`rcalendar()`](https://rdrr.io/pkg/almanac/man/rcalendar.html).
`almanac` is a **Suggests** — install it if missing.

------------------------------------------------------------------------

## Skill: which dplyr verbs to use (and which to avoid)

`tbl_now` implements dplyr generics so attributes survive data
manipulation:

| Verb | Behaviour |
|----|----|
| `filter`, `slice`, `arrange`, `distinct` | reconstruct `tbl_now`, attributes preserved |
| `mutate`, `select`, `relocate`, `rename` | reconstruct; `rename`/`rename_with` update attribute references automatically |
| `group_by` | returns a `grouped_tbl_now`; [`ungroup()`](https://dplyr.tidyverse.org/reference/group_by.html) returns a `tbl_now` |
| `summarise`, `reframe` | attempt reconstruction, **fall back to a plain tibble** if the result no longer looks like a `tbl_now` |
| `left_join` etc. | generally preserved, but verify with [`is_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/is_tbl_now.md) |

**Rules of thumb:**

- ✅ Use `filter`, `select`, `mutate`, `arrange`, `group_by`/`ungroup`,
  `rename` freely — the temporal-effects spec and all attributes are
  preserved and dplyr never triggers temporal-effect computation.
- ⚠️ **Do not remove protected columns** (`.event_num`, `.report_num`,
  `.delay`, `event_date`, `report_date`, or `case_count`) via
  [`select()`](https://dplyr.tidyverse.org/reference/select.html)/`mutate(... = NULL)`
  — doing so **downgrades to a plain tibble** with a warning. Removing a
  strata or covariate column instead silently drops it from that
  attribute.
- ❌ **[`rowwise()`](https://dplyr.tidyverse.org/reference/rowwise.html)
  is NOT supported** — behaviour is undefined; avoid it.
- After any non-trivial pipe, confirm with `is_tbl_now(x)` before
  handing the object to `diseasenowcasting`.

------------------------------------------------------------------------

## Skill: visualize with `autoplot`

[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
(the ggplot2 generic, re-exported by `tbl.now`) returns a multi-panel
`patchwork` diagnostic. `ggplot2` is a hard dependency; `patchwork` is a
**Suggests** (install it).

``` r

library(tbl.now)
autoplot(tn)                       # bare call works after library(tbl.now)
```

Panels:

1.  **Empirical delay distribution** — case-count-weighted kernel
    density of `.delay`.
2.  **Observed epidemic process** —
    [`get_latest_reported_cases()`](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.md)
    by `event_date`, with a dashed vertical **incompleteness line** at
    `now - quantile(delay, level)`. Holidays from the temporal-effects
    spec are marked as red dots.
3.  **Calendar effect** — *normalized* boxplots (cases relative to the
    overall mean, 1 = average). Daily data shows **both** day-of-week
    and week-of-year panels; weekly shows week-of-year; monthly shows
    month-of-year.
4.  **Seasonality periodogram** — the dominant peak suggests a Fourier
    `seasons` length to pass to
    [`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md).

Key arguments:

``` r

autoplot(
  tn,
  level = 0.95,                                   # completeness threshold for the incompleteness line
  delay_distribution_xlim = c(0, 10),             # per-panel x limits (all optional)
  event_date_xlim = as.Date(c("2020-01-01","2020-12-31")),
  calendar_effect_xlim = NULL,
  seasonality_xlim = c(0, 60),
  palette = .tbl_now_palette()                    # override colours
)
```

`level` is the delay quantile where the line is drawn; the default in
the signature is `1` (most conservative), but `level = 0.95` is the
typical choice (“dates still missing ≥5% of their eventual counts are
incomplete”).

------------------------------------------------------------------------

## Skill: convert to/from other packages

`tbl_now_from_*()` build a `tbl_now` (they wrap
[`as_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md),
so `...` flows to
[`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md));
`tbl_now_to_*()` produce the target package’s object. All take
`verbose = TRUE` (prints the chosen `now`, type, units, mapping). Target
packages are **Suggests**.
[`as_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md)
also has methods for each `to_*` output class, so a converted object
round-trips straight back.

| Package | from | to | Mapping |
|----|:--:|:--:|----|
| epinowcast | ✅ | ✅ | `reference_date`/`report_date`/`confirm` ↔︎ count-cumulative; `to` builds `enw_preprocess_data` (or completed `data.table` with `preprocess = FALSE`) |
| baselinenowcast | ✅ | ✅ | long df **or** reporting-triangle matrix ↔︎ count-incidence; `to` has `format = c("long","matrix")` |
| EpiNow2 | ❌ | ✅ | `to` only — collapses to a single `date`/`confirm` series (single time index) |
| data.table | ✅ | ✅ | [`tbl_now_from_data_table()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_data_table.md) / [`tbl_now_to_data_table()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_data_table.md) (underscores) |
| epidist | ✅ | ✅ | epidist 0.4.0 interval-censored dates; `format = "linelist"` uses lower bounds as dates, `format = "interval"` attaches upper bounds as covariates |
| tsibble | ✅ | ✅ | `to` builds a `tbl_ts` (index defaults to `report_date`, key = other date + strata); `from` needs `event_date`, recovers strata from the key |

``` r

nowobj <- tbl_now_from_epinowcast(epinowcast::germany_covid19_hosp,
                                  strata = c("location", "age_group"))
ts     <- tbl_now_to_tsibble(nowobj, verbose = FALSE)
back   <- as_tbl_now(ts, event_date = "reference_date")   # round-trip
```

------------------------------------------------------------------------

## Skill: other utilities

``` r

complete_zeroes(tn)                  # fill missing event/report/strata cells with 0
update(tn, new_data = new_rows)      # bind newer data, preserving attributes
align_weeks(tn, date_col)            # snap dates to a consistent epiweek day -> integer .delay
week_2_date(df, week_col, year_col)  # epiweek + year -> Date
is_weekday(date, weekend_days = c("Sat","Sun"))
change_now(tn, as.Date("2023-06-01"))   # move the as-of date (re-censors later reports)
tbl_now_attributes(tn)               # list of just the tbl_now-specific attributes
```

- **`align_weeks` / weekly data:** weekly dates reported on inconsistent
  weekdays give fractional `.delay`. Use `align_weeks = TRUE` in
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
  or
  [`align_weeks()`](https://rodrigozepeda.github.io/tbl.now/reference/align_weeks.md)
  afterward to force integer delays.
- **`complete_zeroes`** is important before modelling count data:
  unobserved (event, report\[, strata\]) combinations become explicit
  zeros.

------------------------------------------------------------------------

## End-to-end example

``` r

library(tbl.now); library(dplyr); library(almanac)

tn <- denguedat |>
  tbl_now(event_date = onset_week, report_date = report_week,
          strata = gender, verbose = FALSE) |>
  add_temporal_effects(
    temporal_effects(week_of_year = TRUE,
                     holidays = rcalendar(hol_christmas(), hol_new_years_day()))
  )

tn |> filter(gender == "Female")            # dplyr preserves the spec
get_data_type(tn)                            # "linelist"
get_latest_reported_cases(tn)                # observed-so-far incidence
autoplot(tn, level = 0.95)                   # diagnostics

ready <- compute_temporal_effects(tn)        # materialise covariates for modelling
get_temporal_effect_cols(ready)
```

------------------------------------------------------------------------

## Reference: getters

``` r

get_event_date(x) / get_report_date(x)   # COLUMN NAMES (character), not the dates
get_event_units(x) / get_report_units(x) # "days"|"weeks"|"months"|"years"|"numeric"
get_now(x)                                # Date — the as-of date
get_strata(x) / get_num_strata(x)
get_covariates(x) / get_num_covariates(x)
get_case_count(x) / get_is_censored(x)    # column name or NULL
get_data_type(x)                          # "linelist"|"count-incidence"|"count-cumulative"
get_temporal_effects(x)                   # list of lazy specs
get_temporal_effect_cols(x)               # computed column names
get_initial_reported_cases(x) / get_latest_reported_cases(x)
```

## Reference: package data

``` r

data(denguedat)   # dengue surveillance linelist (weekly, Brazil)
data(mpoxdat)     # mpox count-incidence data (has a `race` stratum + `n` counts)
```

------------------------------------------------------------------------

## Common pitfalls (quick list)

- [`get_event_date()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
  returns the **column name**, not the dates.
- [`get_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
  returns **specs**; use
  [`get_temporal_effect_cols()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
  for column names.
- [`compute_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/compute_temporal_effects.md)
  before
  [`add_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.md)
  is a **no-op**.
- [`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md)
  only aggregates **forward** (linelist→incidence→cumulative); it cannot
  reverse.
- Removing a protected column **downgrades to a tibble** — check
  [`is_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/is_tbl_now.md)
  after heavy dplyr.
- [`rowwise()`](https://dplyr.tidyverse.org/reference/rowwise.html) is
  **unsupported**.
- For weekly data with fractional `.delay`, use `align_weeks`.
- Count data types **require** a `case_count` column.
