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
the actual nowcasting is done downstream by **`diseasenowcasting`**. It
also ships **model-free diagnostics** —
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html),
reporting-delay drift / change-point tests, and batch (backlog-dump)
detection — for exploring data before a model is chosen (all
experimental).

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

**Conversions between the two count types go both ways; you can never
rebuild a linelist:**

    linelist  ──►  count-incidence  ◄──►  count-cumulative

Supported: `linelist→incidence`, `linelist→cumulative`,
`incidence→cumulative`, **`cumulative→incidence`**, and re-aggregation
within the same type.

`cumulative → incidence` **de-accumulates**: within each event date (×
grouping), ordered by report date, the increment is the cumulative total
minus the previous one. Because cumulative totals can be revised
*downward*, an increment can be **negative** — that is a legitimate net
down-revision, and delay diagnostics drop non-positive weights. (This is
why
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
etc. now work on `count-cumulative` data such as FluSight.) A downstream
model needing non-negative increments —
e.g. [`tbl_now_to_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_baselinenowcast.md)
— still refuses cumulative input.

**Not supported** (errors): `count-* → linelist` (cannot un-count
aggregated data). So if you might need individual records, keep the
linelist.

> **Pre-aggregation is automatic.** Every function that needs incidence
> calls
> [`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md)
> first, which groups by
> `(event_date, report_date, .event_num, .report_num, strata, is_censored, covariates, temporal-effect cols)`
> and sums the counts — so any **extra column you did *not* declare as a
> stratum is summed away**, not plotted. (If a column like `race` is
> present but not a stratum, its rows are correctly collapsed.) A
> `tbl_now` with such extra columns prints a non-uniqueness warning at
> construction to flag this.

------------------------------------------------------------------------

## Skill: initial vs latest reported cases

Two views of “how many cases occurred on each `event_date`”:

``` r

get_initial_reported_cases(tn)   # the FIRST reported count per event_date
get_latest_reported_cases(tn)    # the MOST-RECENT reported count per event_date
get_nth_reported_cases(tn, delay = 2)   # cumulative count observed within delay <= 2
```

- **`get_initial_reported_cases`** = what was *first* observed for each
  event date (the naive, most-incomplete incidence).
- **`get_latest_reported_cases`** = the best current estimate given
  everything reported up to `now`. This is the de-facto **“truth so
  far”** used for plotting the observed epidemic curve and for scoring
  nowcasts.
- **`get_nth_reported_cases(tn, delay)`** = the cumulative count for
  each event date using only reports with **delay ≤ `delay`** (in report
  units). `delay = 0` is the initial snapshot (cases reported at delay
  0); `delay = 1` adds delay-1 reports; `delay = Inf` (or the maximum
  delay) equals
  [`get_latest_reported_cases()`](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.md).
  Use it to reconstruct the reporting triangle “as observed `delay`
  periods after the event”.

All three return a `count-cumulative`-style `tbl_now` collapsed to one
row per `event_date` (× strata). The gap between initial and latest is
exactly what a nowcast predicts.

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

## Skill: mark whether data is left-censored

`is_censored` flags reports that arrive in artificial **batches**
representing left-censoring rather than true reporting dynamics (e.g. a
lab outage where a week of reports all land on the same later day). The
`is_censored` attribute stores the **name of a logical column**
(`TRUE`/`FALSE` per row).

``` r

df <- df |> mutate(was_batched = report_date == as.Date("2021-03-15"))
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

**The converters drop it, and say so.** A flag that varies *within* an
`(event_date, report_date)` cell (a per-case “upper bound only” mark,
unlike one derived from the delay) puts two rows in a cell a reporting
triangle has one slot for. Every converter but
[`tbl_now_to_epidist()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epidist.md)
therefore collapses it first, warning either way:

| input      | what happens                                           |
|------------|--------------------------------------------------------|
| count data | counts **summed** over the flag; case totals unchanged |
| line list  | column **dropped**; one row per case, unchanged        |

[`tbl_now_to_epidist()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epidist.md)
keeps it — a delay-distribution fit is the one consumer that can use
censoring.

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
  holiday_lags  = 0,            # HOLIDAY lag effect (see below); needs `holidays`
  weekend_lags  = 0,            # WEEKEND lag effect (see below)
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
- **Holiday / weekend lags** (capture the lull *before* a break or the
  rebound *after* it): `holiday_lags = N` / `weekend_lags = N` are
  signed integer depths. Each flags dates exactly `k` **working days**
  from a holiday / weekend, with weekends *and* holidays skipped when
  counting, so the effect lands on the first day back at work or the
  last day before the break.
  - `N > 0` → *after* the event: columns `..._holiday_lag_1 … _N` /
    `..._weekend_lag_1 … _N`. With Sat/Sun weekends `weekend_lags = 1`
    is the Monday.
  - `N < 0` → *before* the event: columns `..._holiday_lead_1 … _|N|` /
    `..._weekend_lead_1 … _|N|`, counting backwards, so `_lead_1` is the
    working day closest to the event. `weekend_lags = -1` is the Friday,
    `-3` is the Wednesday, Thursday and Friday, and `holiday_lags = -1`
    is Christmas Eve.
  - For both sides of the same break, attach one spec per direction.

  `holiday_lags` requires a `holidays` calendar (either sign). Best for
  daily data.
- Replace or clear:

``` r

tn <- replace_temporal_effects(tn, temporal_effects(week_of_year = TRUE))  # drops old computed cols
tn <- remove_temporal_effects(tn)                                          # clears spec + cols
```

> **Order matters:**
> [`compute_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.md)
> before any
> [`add_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.md)
> is a no-op.
> [`replace_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
> removes the computed columns — call
> [`compute_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.md)
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
indep <- yearly() |>
  recur_on_month_of_year("Sep") |>
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
tn <- tn |>
  add_temporal_effects(temporal_effects(holidays = mx_calendar)) |>
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
| `left_join` etc. | generally preserved, but verify with [`is_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md) |

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

Two panel **families**. *Case-count* panels:

1.  **Empirical delay distribution** (`"delay_distribution"`) —
    case-count-weighted histogram of `.delay`.
2.  **Observed epidemic process** (`"epidemic"`) —
    [`get_latest_reported_cases()`](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.md)
    by `event_date`, with a dashed vertical **incompleteness line** at
    `now - quantile(delay, level)`. Holidays from the temporal-effects
    spec are marked with dots.
3.  **Calendar effect** (`"calendar_weekday"`, `"calendar_week"`,
    `"calendar_month"`) — *normalized* boxplots (cases ÷ overall mean, 1
    = average).
4.  **Holiday effect** (`"calendar_holiday"`) — the same normalized
    boxplots by **day type**. Categories follow the attached spec:
    holidays + `weekend = TRUE` → `Weekday`/`Weekend`/`Holiday`; a
    calendar alone → `Non-holiday`/`Holiday`; a weekend effect alone →
    `Weekday`/`Weekend`. A holiday on a weekend counts as a **holiday**.
5.  **Holiday lag effect** (`"calendar_holiday_lag"`) — normalized
    boxplots by position relative to the nearest holiday, as asked for
    by `holiday_lags`: `"2 before"`, `"1 before"`, `"Holiday"`,
    `"1 after"`, … plus `"Other"` (every other day = the reference).
    Shows exactly the days the `..._holiday_lag_k` /
    `..._holiday_lead_k` columns flag (weekends/other holidays skipped
    when counting working days). A date that is both after one holiday
    and before the next goes to the **nearer** one, ties to `"after"`.
6.  **Seasonality periodogram** (`"seasonality"`) — dominant peak
    suggests a Fourier `seasons` length.

*Reporting-delay* panels — to inspect **delay effects** (is the *delay
itself* patterned?):

7.  **Delay calendar effect** (`"delay_weekday"`, `"delay_week"`,
    `"delay_month"`) — boxplots of the **normalized** mean reporting
    delay (mean delay ÷ overall mean delay, 1 = average) by calendar
    group.
8.  **Delay holiday effects** (`"delay_holiday"`, `"delay_holiday_lag"`)
    — the delay twins of panels 4/5. Often the more telling pair: a
    holiday rarely changes how many cases *occur*, but very much changes
    how long they take to be *reported*.
9.  **Delay periodicity periodogram** (`"delay_seasonality"`) — a cycle
    in the delay (e.g. a weekly reporting rhythm).

Which panels are available depends on the object. **Calendar/delay**
panels follow the unit: **daily** → day-of-week *and* week-of-year;
**weekly** → week-of-year; **monthly** → month-of-year. (So on *daily*
data each week-of-year box legitimately summarizes ~7 daily values —
that is not a stratification artefact.) The four **holiday** panels
follow the *temporal-effects spec*, not the unit: they appear only when
there is one to describe (`calendar_holiday`/`delay_holiday` need a
`holidays` calendar **or** `weekend = TRUE`; the `_lag` panels
additionally need a non-zero `holiday_lags`). Asking for one without the
effect warns and skips it. The spec is read directly — **no**
[`compute_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.md)
needed first.

Key arguments:

``` r

autoplot(
  tn,
  panels = "all",         # "all" (default) | "calendar" | "delay_calendar" |
                          #   a vector of concrete panel keys above.
                          #   A SINGLE key returns a plain ggplot (not a patchwork).
  by_strata = FALSE,      # TRUE => split every panel by stratum (dodged boxes /
                          #   coloured lines / dodged bars, viridis, per-stratum
                          #   normalization). Errors if no strata are set.
  strata = NULL,          # which columns to group on when by_strata=TRUE (default =
                          #   the object's strata). NEED NOT be declared strata: any
                          #   data column works and is declared for you, so
                          #   `autoplot(x, strata = "race", by_strata = TRUE)` ==
                          #   `autoplot(add_strata(x, race), by_strata = TRUE)`.
  level = 0.95,           # completeness threshold for the incompleteness line
  delay_distribution_xlim = c(0, 10),   # per-panel x limits (all optional)
  event_date_xlim = as.Date(c("2020-01-01","2020-12-31")),
  calendar_effect_xlim = NULL,
  seasonality_xlim = c(0, 60),
  palette = .tbl_now_palette()          # override colours
)

autoplot(tn, panels = "delay_week")     # just one panel -> a ggplot you can + to
autoplot(tn, by_strata = TRUE)          # one fan of panels per stratum
```

`level` is the delay quantile where the incompleteness line is drawn;
`0.95` is the typical choice (“dates still missing ≥5% of their eventual
counts are incomplete”).
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
works on **all three data types**, including `count-cumulative` (it
de-accumulates internally).

------------------------------------------------------------------------

## Skill: summarise a `tbl_now` (`summary`)

[`summary()`](https://rdrr.io/r/base/summary.html) on a `tbl_now`
returns a **tibble**, one row per statistic of one quantity of one
stratum, rather than
[`summary.data.frame()`](https://rdrr.io/r/base/summary.html)’s column
listing. Read the block you want with
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html).

``` r

summary(tn)                              # everything
summary(tn) |> dplyr::filter(component == "delay")
summary(tn, by_strata = FALSE)           # pooled rows only
```

Columns: `component`, `quantity`, `stratum`, then `n`, `total`, `mean`,
`sd`, `min`, `q25`, `q50`, `q75`, `q90`, `max`, `prop_zero`, `prop`,
`value`, `date_min`, `date_max`. A row uses the columns that apply to it
and leaves the rest `NA`. `stratum` is `"all"` for the pooled rows.

`component` is one of:

| component | rows |
|----|----|
| `cases` | counts per event / report / confirmation date, and per confirmation outcome; plus `censored_per_*_date` when there is a censoring flag |
| `delay` | `event_to_report`, `event_to_confirmation`, `report_to_confirmation`, split by outcome when there is more than one |
| `zero_run` | lengths of the runs of consecutive zero dates, per axis |
| `composition` | shares: `censored`, `confirmation_type = ...`, `strata = ...`, `covariate: <col> = <level>` (in `prop`) |
| `autocorrelation` | lag-*k* correlation of each case series (in `value`) |
| `completeness` | share of each event date’s eventual total arrived by delay *d* |
| `coverage` | `total_cases`, the date ranges, `now`, `max_delay`, the triangle cell counts and occupancy, `now_gap_*` |
| `growth` | ratio of each event date’s running total from one delay to the next (`count-cumulative` only) |

Each block is also its own exported function, returning the same schema,
so they stack with
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html):

``` r

cases_per_date(tn, axis = "event")       # "event" / "report" / "confirmation"
delay_summary(tn, delay = "event_to_report")
zero_run_summary(tn, axis = "event")
prop_censored(tn); prop_strata(tn)
prop_confirmation_type(tn); prop_covariate_levels(tn)
case_autocorrelation(tn, lags = 1)
date_ranges(tn); triangle_occupancy(tn)
reporting_completeness(tn, delays = 0:7)
cumulative_growth(tn, k = 7)
```

Three things to know before reading the numbers:

- **The grids run to `now`, not to the last row.** A date with no rows
  is a **zero**, not an absence — which is what makes `prop_zero` and
  the zero-run lengths mean anything, and why a **line list** summarises
  to exactly the same numbers as its counts. The grid is *global*, so a
  stratum whose cases start late shows its leading zeros and the strata
  stay comparable. So does the triangle-occupancy denominator.
- **Quantiles are inverse-ECDF (type 1)**, not
  [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html)’s
  default: `q50` is the smallest value whose cumulative weight reaches
  `0.5`. Same estimator as
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  /
  [`diagnose_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_drift.md),
  so the table matches the figures. `mean`/`sd` are the ordinary
  case-weighted ones (equal to expanding the counts to one row per
  case).
- **`NA` counts are dropped** as not-yet-observed cells (an `NA` is “not
  seen yet”, a `0` is “seen, and it was zero”). The `unobserved_cells`
  coverage row counts them, so the drop is visible rather than silent.
- **`count-cumulative` gets no `delay` rows.**
  [`delay_summary()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md)
  errors on it — a cumulative total is not additive across delays. Use
  the `growth` rows, or `to_count(x, to = "count-incidence")` first
  (remembering that de-accumulating can produce negative increments).

`report_to_confirmation` is the **laboratory’s turnaround, measured from
the report**; `event_to_confirmation` is measured from the event, so it
is directly comparable with `event_to_report`. They are different
quantities.

------------------------------------------------------------------------

## Skill: health-check a `tbl_now` (`diagnose`)

[`summary()`](https://rdrr.io/r/base/summary.html) describes the object;
[`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
looks for what is **wrong** with it. It returns a tibble of findings
sorted worst first, and it is **structural and deterministic** — it runs
no statistical test.

``` r

diagnose(tn)                                   # everything, worst first
diagnose(tn) |> dplyr::filter(status <= "note")  # only what needs acting on
diagnose(tn, checks = "units")                 # one block
diagnose(tn, by_strata = FALSE)                # pooled rows only

# The offending rows are carried, so you can go straight to them:
bad <- diagnose(tn) |> dplyr::filter(check == "ordering")
tn[bad$rows[[1]], ]
```

Columns: `check`, `scope`, `stratum`, `status`, `n_affected`, `n_total`,
`prop`, `message`, `hint`, `rows` (a list-column of row indices into
`tn`).

`status` is an **ordered factor**, worst first, which is why the tibble
sorts itself and why `status <= "note"` reads as “anything worth acting
on”:

| status | meaning |
|----|----|
| `error` | [`validate_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md) aborts on it |
| `warning` | [`validate_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md) warns about it |
| `note` | a [`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)-only observation. **Never promoted to a warning**: [`validate_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md) runs on every dplyr verb, and a new warning there would make construction noisy for data that has always been accepted |
| `ok` | the check ran and found nothing |
| `not_run` | a signpost: the question needs a statistical test, and `message` is the call that answers it |
| `skipped` | could not be assessed (no confirmation process, wrong data type, package not installed) |

`check` is one of:

| check | what it looks for |
|----|----|
| `declarations` | attribute types, the columns they name, role collisions, **undeclared columns**, temporal effects added but never materialised |
| `ordering` | `event <= report <= confirmation`, including the transitive leg a missing `report_date` would otherwise hide |
| `missing` | `NA`s per column and per stratum. An `NA` **count** is reported *neutrally* — in a triangle it means *not yet observed*, which is correct data |
| `duplicates` | rows repeating on the full key (including the confirmation columns). Defaults **on** here, unlike [`validate_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md) |
| `units` | the declared units against each other, against the calendar the dates land on, and against the `.delay` they produce |
| `negatives` | negative incidence counts, and the negative increments a downward revision leaves when cumulative data is de-accumulated |
| `now` | anything dated after `now`, and the gap from the last observation to `now` |
| `truncation` | how many recent event dates are still immature, and how much of their eventual total has not arrived |
| `strata` | the smallest and the sparsest stratum (named, **not** thresholded), and the confirmations still pending |
| `signposts` | the four questions [`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md) refuses to answer |

Each block is also its own exported function, same schema, so they stack
with
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
— and `diagnose(x)` *is* that bind:

``` r

diagnose_declarations(tn); diagnose_ordering(tn); diagnose_missing(tn)
diagnose_duplicates(tn);   diagnose_units(tn);    diagnose_negatives(tn)
diagnose_now(tn);          diagnose_truncation(tn)
diagnose_strata(tn);       diagnose_signposts(tn)
```

Three things to know:

- **It never runs a test.** Drift and batching are statements about a
  *distribution*; answering them means choosing a method, a window and a
  multiplicity correction.
  [`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
  emits `not_run` rows carrying the call instead:
  `diagnose_drift(x, axis =)` and `diagnose_batches(x, axis =)`.
- **Outage detection is deliberately absent.** A `tbl_now` does not
  carry the zeroes, so a quiet Sunday and a three-week outage are
  structurally identical. The descriptive answer is
  [`zero_run_summary()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md);
  the inferential one is
  [`diagnose_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches.md).
- **[`validate_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md)
  is the same engine, presented as conditions.** One implementation:
  [`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
  returns the findings as data,
  [`validate_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md)
  aborts on the `error`s and warns about the `warning`s.

------------------------------------------------------------------------

## Skill: diagnose reporting-delay drift & change points

Ask whether the reporting delay is **stable over time** before trusting
a fixed delay model. All are experimental and index by event date.

``` r

# Visual: rolling fan chart of the delay distribution (median, mean, 25-75 & 10-90 bands)
plot_delay_drift(tn, window = 7, by_strata = FALSE, changepoint = FALSE)

# Gradual monotonic trend (autocorrelation-robust Mann-Kendall; needs `modifiedmk`)
diagnose_drift(tn, stat = c("median", "spread"))   # location AND dispersion

# Abrupt shift (Pettitt change-point test; no extra dependency)
diagnose_changepoint(tn, stat = c("median", "spread"))
```

- **[`plot_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_drift.md)**
  — solid = rolling median, dashed = rolling mean, bands = 25–75% /
  10–90%. `window` defaults to **7 periods** (7 days for daily, 7 weeks
  for weekly). The recent, not-yet-complete region (after the `level`
  incompleteness cutoff) is **shaded grey** — do not read it as drift.
  `changepoint = TRUE` marks the estimated median change point. Supports
  `by_strata`.
- **[`diagnose_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_drift.md)**
  returns a tidy tibble (per `stat` × stratum) with the Kendall `tau`,
  Sen’s slope, `p_value` and a `drift` verdict; `method` is
  `"hamed-rao"` (default), `"yue-pilon"` or `"block-bootstrap"`. Tests a
  *location* (`"median"`/`"mean"`) and a *dispersion*
  (`"iqr"`/`"spread"`) statistic — drift can be in either. Runs on
  **mature** data only (`mature_only = TRUE`).
- **[`diagnose_changepoint()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_changepoint.md)**
  returns the estimated `changepoint` date, the `before`/`after` level,
  the `shift`, and a `changepoint_detected` verdict.
- Both test functions **emit an experimental `cli` warning** and treat a
  flag as a *potential* trend change / change point, not a confirmed
  one.

Use trend + change-point together: a significant trend with no change
point = slow drift; a change point with weak trend = one abrupt switch.

------------------------------------------------------------------------

## Skill: detect batch reporting (backlog dumps)

A **batch** = a report date where a stalled desk releases a backlog. It
*moves* reports later on the report axis **without creating them** (mass
is conserved), so a window spanning the lull + the release has an
unchanged total, whereas a real epidemic **surge** inflates it. That
conservation law is the whole method — these are experimental,
**model-free** (need only a `tbl_now`), and distinct from an epidemic
surge by construction.

``` r

# 1) Volume screen over the report axis (per report date x stratum)
scr <- diagnose_batches(tn, lookback = 3, alpha = 0.05)
scr[scr$batch, ]            # the flagged report dates
# LEAN output (v0.13.0): report_date, stratum, reported, baseline,
#   deficit (reports missing beforehand -> batch), delta (window total minus
#   expected -> creation), p_transport, p_transport_bh, batch (BH verdict).
#   The raw per-point `classification` column was REMOVED (over-identified;
#   BH `batch` is the trustworthy verdict). `baseline_method` arg also removed
#   (always repeated_median). transport_discriminant() KEEPS its classification.

# 2) Shape test: did ONE report date draw from unusually OLD event dates?
#    (complements the volume screen; `at` must be an observed report date)
diagnose_batch_shape(tn, at = as.Date("2010-05-24"),
                 permute = "items")   # use "blocks" if counts are overdispersed

# 3) Validate a detector: plant a known batch and check it is recovered
planted <- simulate_batch(tn, closed_dates = as.Date(c("2010-05-10","2010-05-17")))
diagnose_batches(planted)
```

- **[`diagnose_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches.md)**
  — the transport test conditions on the window total, so its size does
  **not** depend on the unknown incidence; the local baseline is refit
  from report dates *outside* each candidate window (Siegel’s repeated
  median, robust to the episode). `null_model = "auto"` is
  **overdispersion-aware**: it uses the exact Poisson null only when
  non-negative counts show no overdispersion (dispersion `<= 1.5`) and
  otherwise the dispersion-corrected robust null (always robust for
  signed count-cumulative increments). The exact Poisson null over-flags
  on overdispersed surveillance counts, so prefer the default; add
  `period = 7` to absorb a weekly reporting cadence. BH-adjusted
  p-values in `p_transport_bh`. Works on `count-cumulative` data too
  (then `reported` are signed increments and can be negative).
  **`period` AUTO-RESOLVED from temporal effects (v0.13.0,
  `.batch_resolve_period`): a day_of_week effect -\> `period=7`,
  week_of_year -\> `period=52` (reads `get_temporal_effects(x)` list;
  each spec is `list(t_effects=<S7>, date_type,...)`, access via
  `spec$t_effects@day_of_week`). User `period` wins (informs on
  disagreement). Daily data + no temporal effect + no period -\>
  `cli_inform` suggests period=7.**
- **[`diagnose_batch_shape()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batch_shape.md)**
  — a one-sided rank-sum on the delays at `at` vs neighbouring report
  dates; **exactly distribution-free** when incidence is locally
  log-linear and counts are Poisson. `permute = "blocks"` for
  overdispersed (NB) counts; `guard` omits dates adjacent to `at` (a
  batch’s own deficit sits there).
- **[`simulate_batch()`](https://rodrigozepeda.github.io/tbl.now/reference/simulate_batch.md)**
  — closes reporting on `closed_dates` and re-stamps those reports to
  the next open date; returns a `tbl_now`. For testing/teaching.
- **`diagnostic_plot(x, panels=, by=)`** — a gallery to *see* the
  reporting process. LAYOUT: **5 panels in TWO COLUMNS**
  (`wrap_plots(ncol=2, byrow=FALSE)` column-major:
  col1=reporting/triangle/profiles, col2=delay_drift/transport, 1 empty
  cell). `panels` (default `"all"`, exactly these 5): `"reporting"`
  (reports/report date; y-axis CAPPED at 99th pct via coord_cartesian
  when a dump dwarfs the curve — covid_us has a 1.8M-report dump
  2021-12-06 vs 21k median), `"triangle"` (event x delay; muted
  blue=reported zero, blank=not yet reportable, FULL-calendar event
  axis; THIRD REPORT-DATE AXIS = dashed 45° iso-report diagonals
  labelled via `report_ticks=6`; `mark_batches=0` optional),
  `"profiles"` (one delay curve per date, SINGLE colour fixed alpha),
  `"delay_drift"`, `"transport"`
  ([`transport_discriminant()`](https://rodrigozepeda.github.io/tbl.now/reference/transport_discriminant.md)
  scatter, y LIMITED to batch region via
  `coord_cartesian(ylim=c(y_lo,y_hi))` WITHOUT clip=“off” — clip=“off”
  BLED into panel below, default clip does not). Single panel → plain
  plot; facetted by stratum; `by="event"` switches profiles; `...`
  routed. Each panel = stand-alone exported fn
  (`plot_reporting_process`, `plot_reporting_triangle`,
  `plot_delay_profiles`, `plot_delay_drift`,
  `plot_transport_discriminant`). Reporting process = RED, epidemic
  process = GREEN (`.diag_build_process` fill keyed on axis).
  **`plot_epidemic_process(x)`** now EXPORTED (green bars, cases by
  event date; mirror of reporting process).
  **`plot_creation_transport`** MOVED to devel
  (`devel/conservation_extras.R`, uses `tbl.now:::` for internals) — NOT
  exported. **`plot_reporting_v(x)`** (exported, R/v_triangle.R, NOT in
  gallery) = the reporting “V” — SAME data as plot_reporting_triangle
  ROTATED 45° (y=report date, left arm=event date, right arm=delay
  reading 0 out), batch = HORIZONTAL slice (diagonal in the square
  triangle), whole observable triangle filled w/ pale-blue zeros; coords
  RELATIVE to ev_min (epoch nums wreck coord_fixed); arm
  ticks/labels/titles all OUTSIDE each arm, perpendicular. All panels
  carry grey captions. **EVERY plot fn takes `plotly=TRUE`** (→
  interactive plotly widget via `.as_plotly`/`.combine_panels` in
  R/plotly_support.R; plotly in Suggests).
  **`plot_scalogram(x, type=c("reporting","epidemic"), windowrad=NULL)`**
  EXPORTED (R/scalogram.R) — window-inner wavelet scalogram via
  `wavScalogram::windowed_scalogram(sqrt(n), border_effects="INNER", energy_density=TRUE)`:
  computed from OBSERVED DATA ONLY, NO border padding (honest at the
  “now” edge; the FFT/periodic scalogram fabricates edge structure).
  Batch = bright SHORT-PERIOD ridge in reporting (red) that epidemic
  (green) lacks. **Analysed on the INTEGER unit grid: `dt=1` (NOT
  dt=unit_days) so periods come out in the tbl’s units (weeks/days) and
  `tcentral` are grid INDICES; passing dt=unit_days made tcentral come
  back in DAY-units (range 63-1043 on a 157-week grid) → over-ran the
  date grid, dropped ~29k cells, scalogram looked empty on weekly
  denguedat. y-label `sprintf("Period (%s)", report_unit)`. ALSO for
  LONG series wavScalogram SUBSAMPLES tcentral (e.g. every 5th step, 197
  of 1095) → drawing on the Date axis with `geom_tile(width=unit_days)`
  left 4-in-5 columns BLANK. FIX: plot x on the UNIFORM INTEGER window
  index `df$xi=seq_along(dates)` with `geom_tile(width=1)`, then RELABEL
  the x-axis with dates via
  `xpos=approx(as.numeric(dates), seq_along(dates), xout=as.numeric(pretty(dates)))$y` +
  `scale_x_continuous(breaks=xpos, labels=format(date_breaks))`. Works
  on full/filtered/daily.** DEFAULTS `wname="PAUL"` (sharper batch
  localisation), `windowrad=1`, `format="%d/%b/%y"` (x-axis date fmt
  arg). Outside cone = `panel.background=element_rect(fill="gray10")`
  (NOT na.value); NO subtitle/caption. Devel
  `plot_scalogram_difference(x)` (`devel/wavscalogram_explore.R`) =
  log2(reporting_scalogram) − log2(epidemic_scalogram) (each
  per-period-normalised window-inner PAUL), `scale_fill_gradient2`
  green↔︎red, batch = red short-period stripe. NOT wavScalogram::wsd()
  (its ratio didn’t isolate the batch). GOTCHAS (all real, hit during
  build): (1) `ws$tcentral` are time INDICES not dates → map
  `s$date[round(ws$tcentral)]`; (2) plot on `logp=log2(period)`
  (geometric scales → regular grid) NOT raw period; (3) `geom_raster`
  renders BLANK on the ragged INNER cone AND `geom_tile` auto-height
  COLLAPSES to a thin line (dense scales) → use
  `geom_tile(width=unit_days, height=hstep)` with explicit
  `hstep=median(diff(sort(unique(log2(periods)))))`; (4) normalise per
  period (÷ per-period median) so the short-period burst shows over the
  epidemic trend. Result = the INNER dome cone (blank outside reliable
  region) with the ridge inside. The old devel `scalogram_plot(x)` 2×2
  is SUPERSEDED. **MOVED TO devel (`devel/conservation_extras.R`, uses
  `tbl.now:::`, NOT in package):** `plot_creation_transport` (2 stacked
  panels transport red / creation green, signed-log),
  `plot_cumulative_backlog`, `plot_reporting_lag` (band floored at 0),
  `plot_conservation_dashboard` (=ct overlay + batch-score
  `transport−creation`; user found backlog/lag/dashboard/residual not
  worth the gallery space — batch score conflates holds+big-dumps so
  unreliable). Other devel: `plot_rotated_triangle`
  (`devel/rotated_triangle.R`),
  `plot_ternary_reporting`/`plot_ternary_transport`
  (`devel/ternary_plots.R`),
  `plot_transport_timeline`/`plot_delay_band_ternary`
  (`devel/removed_plots.R`). IMPORTANT gotcha in
  [`diagnose_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches.md)’s
  classification: `hold_or_deletion` OVERRIDES `batch`/`surge` whenever
  creation_z \< -z_star, REGARDLESS of transport_z — so the most extreme
  top-left points are holds, not batches. Also:
  `plot_transport_discriminant` colours RED only BH-confirmed batches
  (`td$batch`), NOT the raw per-point `classification` (which
  over-identifies ~10-20% at alpha by construction).
- **`transport_discriminant(x, lookback=, period=, alpha=)`** — the
  plane behind
  [`diagnose_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches.md):
  per report date the deficit `W` (transport) and `Δ = S − M`
  (creation), standardised as `transport_z` / `creation_z`, plus the
  quadrant `classification`. A batch = high transport, ~0 creation
  (top-left). **DEFAULT lookback = 7L** (changed from 3L, 2026-07-10)
  for diagnose_batches/transport_discriminant. Discriminant shaded
  region labelled “Potential batch region”; confirmed batches get bold
  white-on-red date labels (y_hi has +18% headroom so labels aren’t
  clipped). Devel plot_creation_transport titles “(a batch)”/“(a surge)”
  — NO question marks. \> TEMPORAL-EFFECT FINDING
  (`devel/temporal_baseline_explore.R`): the day-to-day squiggle in \>
  the creation/transport SCORES is NOT a weekday effect. Raw covid_us
  daily reports have a real \> ~25% weekly pattern (Sun 0.73×, Wed
  1.22×) BUT the scores already remove it (weekly R²≈0 with \> or
  without period=7; transport_z lag-1 AC≈0.92, no zig-zag). The squiggle
  is overdispersion \> noise seen through overlapping windows. Explored
  trend×weekday (=period=7, best for daily \> residual 0.011→0.004),
  trend+weekday, and the average — none reaches the scores. period=7 \>
  still worth passing for the daily reporting views. \> The
  conservation-law time-series monitors — `plot_cumulative_backlog`
  (detrended \> cumulative residual; a batch is a V, a run of batches a
  staircase), \> `plot_conservation_dashboard` (3 standardised series),
  `plot_reporting_lag` (mean \> delay vs a local band) — are clearest
  when batches are LARGE relative to the noise \> (e.g. `covid_us`,
  where they read beautifully). On small overdispersed counts they \>
  can be noisy — there the transport-discriminant scatter +
  [`diagnose_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches.md)
  are the \> robust batch story. All three mark only BH-confirmed
  batches in red.
  - **`covid_us`** dataset — CDC COVID-19 case surveillance, 2020-2021
    events aggregated event×report (no strata), built to DEMONSTRATE
    batch reporting (huge right-skewed delay; big 2022 backlog dumps at
    2022-01-21 ~25× and 2022-05-25 ~15×). See the “Finding batch
    reporting…” article + `data-raw/covid_us.R` (duckdb over the 14GB
    source).

> The older `detect_report_batches()` / `plot_report_batches()` are
> **removed** — use
> [`diagnose_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches.md) +
> [`diagnose_batch_shape()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batch_shape.md).

------------------------------------------------------------------------

## Skill: coerce a `tbl_now` to a plain tibble / data.frame

[`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
(re-exported, so it works after
[`library(tbl.now)`](https://rodrigozepeda.github.io/tbl.now/)) and
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) drop the
`tbl_now` class and metadata:

``` r

as_tibble(tn)                                    # plain tibble; spec stays LAZY
as_tibble(tn, compute_temporal_effects = TRUE)   # materialise effect cols first
as.data.frame(tn, compute_temporal_effects = TRUE)
```

The default is **lazy** on purpose (dplyr uses these coercions
internally as cheap declassers). Pass `compute_temporal_effects = TRUE`
to get a modelling-ready frame with the holiday / Fourier / calendar
columns filled in; the input `tbl_now` is left unchanged.

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
| epinowcast | ✅ | ✅ | `reference_date`/`report_date`/`confirm` ↔︎ count-cumulative. `from` accepts the raw long input, a preprocessed `enw_preprocess_data` object, **or** a fitted `epinowcast` object (grouping auto-detected). `to` builds the preprocessed `enw_preprocess_data` object (or the completed-input `data.table` with `preprocess = FALSE`) |
| baselinenowcast | ✅ | ✅ | long df **or** reporting-triangle matrix ↔︎ count-incidence; `to` has `max_delay =` (delay periods kept, counted as in [`tbl_now_to_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md): `30` → delays 0–29) and `format = c("matrix","long","triangle_list")` — **`"matrix"` is the default**; `"triangle_list"` returns ONE TRIANGLE PER STRATUM as a thin `tbl_now_triangle_list` (still a plain list, so [`lapply()`](https://rdrr.io/r/base/lapply.html) works), length-1 and named `"all"` when there are no strata, and [`as_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md) rebuilds a `tbl_now` from it with the strata recoded. `to`’s `delays_unit` defaults to `NULL` and is **inferred** from the object units (equal event/report units of `"days"`/`"weeks"`) for the matrix format, else supply it. Refuses `count-cumulative` input (would need to de-accumulate to possibly-negative increments) |
| EpiNow2 | ✅ | ✅ | `to` takes `target =`, named for the EpiNow2 function the result is passed to: `"estimate_infections"` (default, a `date`/`confirm` series, also what `epinow()` takes), `"regional_epinow"` (the same plus a `region` column from the strata), `"estimate_truncation"` (a list of `date`/`confirm` snapshots, one per report date — the one model that uses the report dimension), `"estimate_dist"` (the interval-censored delay frame). `from` inverts the snapshot form only. **EpiNow2 models a DAILY process and has no `timestep`**, so `accumulate = "auto"` lays non-daily data on its grid |
| data.table | ✅ | ✅ | [`tbl_now_from_data_table()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_data_table.md) / [`tbl_now_to_data_table()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_data_table.md) (underscores) |
| epidist | ✅ | ✅ | epidist 0.4.0 interval-censored dates; `format = "linelist"` uses lower bounds as dates, `format = "interval"` attaches upper bounds as covariates |
| tsibble | ✅ | ✅ | `to` builds a `tbl_ts` (index defaults to `report_date`, key = other date + strata); `from` needs `event_date`, recovers strata from the key |
| NobBS | ❌ | ✅ | `to` only. Builds the line list [`NobBS::NobBS()`](https://rdrr.io/pkg/NobBS/man/NobBS.html) takes (`onset_date`/`report_date`). **NobBS counts ROWS**, so count input is expanded to one row per case — handing it counts directly nowcasts 1,174 rows as 1,174 cases when they carry 50,160. Daily or weekly grids only |
| surveillance | ❌ | ✅ | `to` only. Builds the individual-level line list [`surveillance::nowcast()`](https://rdrr.io/pkg/surveillance/man/nowcast.html) takes, renaming the dates to its own `dHospital`/`dReport` defaults. `format = c("linelist","linelist_list","sts")` — **`"linelist"` is the default**; `"linelist_list"` returns ONE LINE LIST PER STRATUM as a thin `tbl_now_surveillance_list` (still a plain list, so [`lapply()`](https://rdrr.io/r/base/lapply.html) works), length-1 and named `"all"` when there are no strata, and [`as_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md) rebuilds a `tbl_now` from it (as a `linelist`, since counts were expanded); `"sts"` returns the observed curve as an `sts` object instead. Count input is expanded back to one row per case |

``` r

nowobj <- tbl_now_from_epinowcast(epinowcast::germany_covid19_hosp,
                                  strata = c("location", "age_group"))
ts     <- tbl_now_to_tsibble(nowobj, verbose = FALSE)
back   <- as_tbl_now(ts, event_date = "reference_date")   # round-trip
```

> **You do not have to aggregate first.** A column the object was never
> told about — `sex` in `covid_colombia` — puts two rows in every
> `(event, report)` cell. Every `tbl_now_to_*()` converter **pools
> undeclared columns for you** via
> [`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md),
> so case totals are preserved and no
> [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html) is
> needed.
> [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
> still *warns* that the cells are non-unique; that is information, not
> a fault.
> **[`distinct()`](https://dplyr.tidyverse.org/reference/distinct.html)
> does not fix it** — those rows are distinct, they differ in `sex` —
> and on data with genuine repeats it deletes cases. Declare the column
> (`strata = sex`) when you want it modelled separately. Line lists are
> never pooled: one row is already one case.

------------------------------------------------------------------------

## Skill: fit several models at once and ensemble them (`run_nowcast`)

The converters are one front door: convert, fit,
[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md).
[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
is the other: it does all three in one call and always returns a
**`tbl_nowcast`**, the shape
[`nowcast_ensemble()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_ensemble.md)
and
[`score_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/score_nowcast.md)
need in order to compare models at all. Both stay supported — reach for
the converter when you want to pass that package’s own arguments or
inspect what it was handed.

`run_nowcast(x, engine, verbose =)` takes an **engine**: the model AND
every argument it needs. **The data and `verbose` are the ONLY things
outside it** — a bare method string is an error (0.27.0; before that it
was `run_nowcast(x, "NobBS", max_D = 10)` and an argument that missed
its backend vanished silently).

``` r

nc <- run_nowcast(x, engine_baselinenowcast(draws = 1000))

list_nowcast_methods()          # what is available in this session
list_nowcast_methods(installed_only = FALSE)
```

One constructor per package, each naming that package’s own arguments,
plus the general `engine(method, ...)` for anything else (including your
own backend):

| engine | fits | key named args | needs |
|----|----|----|----|
| [`engine_diseasenowcasting()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md) | `diseasenowcasting::nowcast()`, straight off the `tbl_now` | `model`, `type`, `n_draws` | — |
| [`engine_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md) | one reporting triangle, or one **per stratum** | `draws`, `delays_unit` | — |
| [`engine_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md) | [`epinowcast::epinowcast()`](https://package.epinowcast.org/reference/epinowcast.html) | `preprocess_args`, `expectation`, `reference`, `report`, `fit` | Stan |
| [`engine_nobbs()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md) | `NobBS()`, or `NobBS.strat()` when strata are declared | `max_D`, `moving_window`, `specs` | JAGS |
| [`engine_surveillance()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md) | [`surveillance::nowcast()`](https://rdrr.io/pkg/surveillance/man/nowcast.html), **one fit per stratum** | `D`, `when`, `fit_method`, `control` | — |
| [`engine_epinow2()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md) | `estimate_infections()`, or `regional_epinow()` when strata are declared | `generation_time`, `delays`, `truncation`, `rt`, `stan`, `convert_args` | Stan |
| `engine(method, ...)` | any registered method, yours included | — | — |

Every engine also takes:

- **`min_date`** — how much history to fit on. `NULL` (default) = the
  whole series; a **`Date`** = a fixed cut; a **number** = the last *n*
  periods before `get_now(x)`, **in the object’s own units**. Per engine
  on purpose: `baselinenowcast`/`diseasenowcasting` take a long series
  happily, while `epinowcast`/`EpiNow2` scale with the number of
  reference dates. In a
  [`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md)
  prefer the number: `now` moves, so a fixed `Date` makes the window
  grow as the backtest walks forward. It trims the **event axis**, not
  `now`, and the trimmed object is what the result carries.
- **`quantile_levels`** — default
  [`nowcast_quantile_levels()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_quantile_levels.md).
  It lives on the engine because for `NobBS` it is a **fit-time**
  argument (it lands in `specs$quantiles`; NobBS keeps no draws, so a
  level it was not asked for is unrecoverable), and `surveillance`
  reports a fixed set and warns. The draw-keeping engines
  (`baselinenowcast`, `diseasenowcasting`, `epinowcast`, `EpiNow2`)
  answer any level after the fact.
- **`label`** — its name in a
  [`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md),
  defaulting to the method. This is how one package appears twice with
  different settings.

What comes back:

``` r

nc                          # print: method, now, dates, strata, quantile levels
as_tibble(nc)               # the quantile predictions (long)
as_tibble(nc, type = "draws")  # the draws, where the backend has them
tidy(nc)                    # the standard event_date/stratum/estimate/... table
autoplot(nc)                # green fan chart (a nowcast is the epidemic process)
nc@fit                      # the backend's OWN object, untouched
```

### Scoring

``` r

score_nowcast(nc, truth = x_full)   # wis, ae_median, coverage_50, coverage_90
as_scoringutils(nc, truth = x_full) # hand it to scoringutils instead
```

`truth` is the **full `tbl_now`** (the one that still holds the reports
which arrived after the nowcast’s `now`), or `NULL` to reuse the
nowcast’s own source data. There is **no `observed_col`** (removed
0.27.0): the observed counts are read off the object with
[`get_case_count()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md),
and a **line list is aggregated first**, so a bare data frame is refused
rather than guessed at. `nowcast_truth()` was un-exported in 0.19.0: it
was
[`get_latest_reported_cases()`](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.md)
reshaped, and the reshaping now happens inside the scoring functions.

Score against data the model had not seen: snapshot at a past `now`, fit
there, score against the full series.

### Ensembles

``` r

members <- list(a = run_nowcast(x, engine_baselinenowcast()),
                b = run_nowcast(x, engine_diseasenowcasting()))

nowcast_ensemble(members)                              # quantile average
nowcast_ensemble(members, type = "linear_pool")        # pool the draws
nowcast_ensemble(members, weights = c(a = 0.7, b = 0.3))
```

- **`type = "quantile"`** (default) averages the members’ values level
  by level. Always applies, and tends to be **narrower** than the
  members.
- **`type = "linear_pool"`** pools their draws into a mixture. Usually
  **wider** and better calibrated, and it **errors** if any member has
  no draws (`NobBS` and `surveillance` do not) rather than quietly
  dropping it.
- Members must share the event-date column and the strata. Levels not
  shared by every member are dropped, with a warning — no member is
  silently discarded.

### Learning the weights

``` r

bt <- nowcast_backtest(x_full,
                       engine_baselinenowcast(),
                       engine_nobbs(max_D = 10),
                       now_dates = as.Date(c("2010-08-01", "2010-09-01")),
                       seed = 20260824)
tidy(bt)                                   # one row per (method, now, target)
nowcast_weights(bt, type = "inverse_score")  # w proportional to 1 / mean WIS
nowcast_ensemble(members, weights = "inverse_score", backtest = bt)
```

`nowcast_backtest(x, ...)` takes the engines variadically (or one list
of them); their `label`s must be unique and their `quantile_levels` must
all agree, or it errors. It is `length(engines) x length(now_dates)`
model fits — keep `now_dates` short with Bayesian members. **Pass
`seed`**: it seeds immediately before each fit, so a fit depends only on
which fit it is. One [`set.seed()`](https://rdrr.io/r/base/Random.html)
before the whole backtest silently moves every other fit the moment you
drop a method or refit one date.

### Adding your own model

Two S3 methods, in any package:

``` r
nowcast_fit.mymodel  <- function(method, x, ..., quantile_levels, verbose = TRUE) { ... }
nowcast_tidy.mymodel <- function(method, fit, x, ..., quantile_levels) {
  list(predictions = NULL, draws = <event_date, strata, .draw, .value>)
}
```

Return `draws` where you can — the quantiles are derived for you, and it
is what `type = "linear_pool"` needs. Return `predictions`
(`<event_date>`, strata, `.quantile_level`, `.value`) otherwise. One of
the two may be `NULL`, not both. See `vignette("ensemble-nowcasting")`.

------------------------------------------------------------------------

## Skill: get predictions out of ANY nowcast (`tidy`)

[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
is the mirror of the converters: they normalise what goes **into** a
nowcasting package,
[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
normalises what comes **out**. Same columns, same types, whichever
engine produced the fit.

``` r

tidy(fit)                              # one table, whatever `fit` is
tidy(fit, probs = c(0.05, 0.5, 0.95))  # adds q5, q50, q95 columns
tidy(fit, engine = "NobBS")            # when the object is an unclassed list
```

Columns: `event_date`, `stratum` (`"all"` when unstratified),
`estimate`, `conf.low`, `conf.high`, `level`, `engine` — plus one `q*`
column per `probs` entry (named after the probability: `0.025` →
`q2.5`).

Supported: `diseasenowcasting` (pass `predict(fit)`), `baselinenowcast`
(`output_type = "samples"`), `epinowcast`, `NobBS`, `surveillance`
(`stsNC`), `EpiNow2` — **and the `tbl_nowcast` that
[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
/
[`nowcast_ensemble()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_ensemble.md)
return**, so a nowcast fitted through the one-call front door reads
exactly like one fitted by hand.

- **`level` is not decoration.** It records the width each engine’s
  interval actually has. `epinowcast` reports a q5–q95 band (**90%**) by
  default while the others report 95%; without it you would compare the
  two as if identical.
- **`probs` only works where draws exist.** Through
  [`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
  that is `diseasenowcasting`, `baselinenowcast`, `epinowcast` and
  `EpiNow2` (the last since 0.27.0, via
  `get_predictions(format = "sample")`). `NobBS` and `surveillance`
  report a fixed summary set and **error** rather than approximate. Note
  [`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
  called on a **bare** `estimate_infections` object still reads its
  summary, so `probs` there is a different question.
- **On a `tbl_nowcast`, `level` is read off the object.** It is the
  width of the widest **symmetric** pair of quantile levels the nowcast
  actually carries: `0.95` for the default
  [`nowcast_quantile_levels()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_quantile_levels.md),
  `0.8` for `c(0.1, 0.5, 0.9)`, and `NA` (with `NA` bounds) when no
  symmetric pair exists. `engine` is the method, or the ensemble’s name.
- **[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
  also works on a `nowcast_backtest`**, giving one row per (method,
  `now` date, target) with `wis`, `ae_median` and the coverage flags —
  ready for `dplyr` or `ggplot2`.
- **[`library(broom)`](https://broom.tidymodels.org/) overwrites
  `tbl.now`’s
  [`tidy.list()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
  method**, which is what `NobBS` fits and per-stratum `baselinenowcast`
  lists dispatch on. Qualify as `tbl.now::tidy(...)` when broom is
  attached.
- **`diseasenowcasting` needs \>= 2.1.0 for a bare `tidy(fit)`.** From
  2.1.0 it re-exports the shared generic and supplies its own method, so
  `tidy(fit)` returns the nowcast and `model_parameters()` returns the
  parameter table. Before 2.1.0 it declared its **own**
  [`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
  generic, so `tidy(fit)` silently returned parameters; on those
  versions use `tbl.now::tidy(predict(fit))`. `tbl.now` registers its
  own method only when the package does not supply one, so it never
  overrides the newer version.
- **`epidist` has its own
  [`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md),
  with different columns.** It estimates a delay distribution, not a
  nowcast, so
  [`tidy.epidist_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.epidist_fit.md)
  returns `term` / `estimate` / `conf.low` / `conf.high` / `level` /
  `engine`, one row per distribution parameter (`mu`, `sigma`, plus the
  derived `mean` and `sd`). There is no `event_date`. Beware dispatch:
  the fit is `c("brmsfit", "epidist_fit")`, so a loaded `broom.mixed`
  matches first.
- **Engines without draws can still give you quantiles — ask at fit
  time.** `tidy(probs =)` errors for `NobBS` and `surveillance`, but
  `NobBS(specs = list(quantiles = c(0.1, 0.5, 0.9)))` computes them
  during the fit and returns `q_0.1` / `q_0.5` / `q_0.9` columns on
  `$estimates`; join them onto
  [`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
  output by date.
- **`surveillance` does report an interval**, read from the `stsNC`
  object’s `pi` slot at the width `control$alpha` sets (95% by default).
  You do NOT need the JAGS-backed `bayes.trunc`/`bayes.trunc.ddcp`
  methods to get uncertainty; `lawless` and `unif` may leave the slot
  empty, and then the bounds are `NA`.
- **`NobBS`, `regional_epinow()` and a per-stratum `baselinenowcast`
  list all arrive as unclassed lists**, so they are told apart by
  structure; pass `engine =` if that ever fails.
- **[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
  does NOT re-grid.** Engines that bin onto their own week starts keep
  them. Snapping silently would hide a real difference between packages.
- The generic comes from `generics`, so it composes with `broom` rather
  than masking it.

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
  zeros. It completes out to `max(get_now(x), last event date)` — an
  event date with NO reports at all does not exist as a row, so stopping
  at the last observed event leaves a hole exactly at the `now` edge,
  where nowcasting matters. `until =` sets a different end date (never
  truncating below the data). **Counts only** — a line list cannot
  represent a zero period, so convert with
  [`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md)
  first.

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
get_nth_reported_cases(x, delay)          # cumulative count within a given delay
```

## Reference: nowcasting & ensembles (all experimental)

``` r
run_nowcast(x, engine = engine_diseasenowcasting(), verbose = TRUE)  # -> tbl_nowcast
engine(method, ..., min_date =, quantile_levels =, label =)
engine_diseasenowcasting/baselinenowcast/epinowcast/nobbs/surveillance/epinow2(...)
is_nowcast_engine(x)
list_nowcast_methods(installed_only = TRUE)
nowcast_quantile_levels()                       # the hub levels, the default
tbl_nowcast(predictions =, draws =, ...)        # the constructor (for backends/tests)
is_tbl_nowcast(x)

nowcast_ensemble(..., type =, weights =, backtest =, n_draws =, name =)
score_nowcast(nc, truth =) / as_scoringutils(nc, truth =)  # truth = the full tbl_now
nowcast_backtest(x, <engines>, now_dates =, seed =) / nowcast_weights(bt, type =)

nowcast_fit(method, x, ...) / nowcast_tidy(method, fit, x, ...)  # extension points
engine("nobbs")                                 # -> the dispatch object
```

## Reference: diagnostics & batches (all experimental)

``` r

summary(x, by_strata =)                   # the whole summary, as a tibble
diagnose(x, checks =, by_strata =)        # the structural health check
diagnose_declarations/ordering/missing/duplicates/units/negatives(x)
diagnose_now/truncation/strata/signposts(x)
cases_per_date(x, axis =) / delay_summary(x, delay =) / zero_run_summary(x, axis =)
prop_censored(x) / prop_strata(x) / prop_confirmation_type(x) / prop_covariate_levels(x)
case_autocorrelation(x, lags =) / date_ranges(x) / triangle_occupancy(x)
reporting_completeness(x, delays =) / cumulative_growth(x, k =)
autoplot(x, panels =, by_strata =)        # multi-panel diagnostic (patchwork)
autoplot(nc, levels =, show_reported =)   # nowcast fan: reported counts as grey
                                          #   COLUMNS one period wide, fan over them
plot_delay_drift(x) / diagnose_drift(x) / diagnose_changepoint(x)
diagnose_batches(x) / diagnose_batch_shape(x, at =) / simulate_batch(x, closed_dates =)
transport_discriminant(x)                 # deficit W vs discriminant Delta, per report date
diagnostic_plot(x, panels =, by =)        # reporting-process gallery (reporting/triangle/profiles/delay_drift/transport)
```

## Reference: package data

``` r

data(denguedat)        # dengue surveillance linelist (weekly, Puerto Rico)
data(flusight)         # flu in the United States (count-cumulative)
data(mpoxdat)          # mpox count-incidence (a `race` column + `n` counts)
data(covid_colombia)   # daily COVID-19 counts, Colombia (a `sex` column + `n`)
data(covid_us)         # daily COVID-19 counts, US CDC -- the batch-dump example
data(hai_bucaramanga)  # healthcare-associated infections; deliberately messy
```

------------------------------------------------------------------------

## Common pitfalls (quick list)

- [`get_event_date()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
  returns the **column name**, not the dates.
- [`get_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
  returns **specs**; use
  [`get_temporal_effect_cols()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
  for column names.
- [`compute_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.md)
  before
  [`add_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.md)
  is a **no-op**.
- [`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md)
  converts between the two count types **both ways**
  (incidence↔︎cumulative); `cumulative→incidence` de-accumulates and can
  yield **negative** increments. It can never rebuild a `linelist`.
- Undeclared extra columns are **summed away** by
  [`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md),
  not plotted per-value — a boxplot with spread on *daily* data is the
  days within a week, not a hidden stratum.
- Removing a protected column **downgrades to a tibble** — check
  [`is_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md)
  after heavy dplyr.
- [`rowwise()`](https://dplyr.tidyverse.org/reference/rowwise.html) is
  **unsupported**.
- For weekly data with fractional `.delay`, use `align_weeks`.
- Count data types **require** a `case_count` column.
- `diagnose_batch_shape(at =)` needs `at` to be an **observed report
  date**; the volume screen
  [`diagnose_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches.md)
  scans them all.
- [`diagnose_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_drift.md)
  needs the `modifiedmk` package (a Suggests); the batch and drift
  diagnostics all print an experimental warning.
- `detect_report_batches()` / `plot_report_batches()` were **removed** —
  use
  [`diagnose_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches.md) +
  [`diagnose_batch_shape()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batch_shape.md).
- **A zero period is invisible in a line list.** An event date with no
  reports has no rows, so engines that build their time grid from the
  rows they are handed stop short of the `now`.
  [`complete_zeroes()`](https://rodrigozepeda.github.io/tbl.now/reference/complete_zeroes.md)
  fixes this for *count*-shaped converters (`baselinenowcast`,
  `epinowcast`); for line-list engines the padding evaporates (a
  zero-count row expands to zero rows) and you must give the grid
  another way — `control$dRange` in `surveillance`.
- `tidy(fit, probs =)` **errors** for `NobBS` and `surveillance`: they
  keep no draws, so an arbitrary quantile would be an approximation. The
  same is true of a quantile-only `tbl_nowcast`.
- **`NobBS`’s `moving_window` counts EVENT PERIODS and must not exceed
  the history you hand it.** Ask for more and it pads its grid backwards
  and returns **zero for every date, with no error** — which reads as a
  catastrophic score rather than as the misconfiguration it is.
- **[`EpiNow2::estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.html)
  defaults to NO reporting delay** (`delay_opts()` is `Fixed(0)`) and a
  one-day generation time (`gt_opts()` is `Fixed(1)`). Those defaults
  describe a process with nothing to nowcast; pass `generation_time =`
  and `delays =` yourself.
- **`nowcaster` is no longer supported** (dropped in 0.16.0, and its
  [`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
  backend in 0.18.0). `tbl_now_to_nowcaster()`, `get_nowcaster_strata()`
  and a `"nowcaster"` engine do not exist.
- **A
  [`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
  backend that reports only a point estimate and one interval cannot
  honour arbitrary `quantile_levels`.**
  [`engine_surveillance()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md)
  warns and returns the three levels it does have (the median and the
  two tails of its own interval) rather than interpolating the rest.
  **`EpiNow2` no longer does** (0.27.0):
  [`nowcast_tidy.EpiNow2()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_tidy.md)
  reads the posterior samples with
  `EpiNow2::get_predictions(format = "sample")` instead of the fit’s
  `lower_<pct>`/`upper_<pct>` summary, so it reports any level and keeps
  draws.
