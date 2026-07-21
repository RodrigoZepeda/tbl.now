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

## Skill: mark whether data is right-censored

`is_censored` flags reports that arrive in artificial **batches**
representing right-censoring rather than true reporting dynamics (e.g. a
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
[`compute_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/compute_temporal_effects.md)
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

## Skill: diagnose reporting-delay drift & change points

Ask whether the reporting delay is **stable over time** before trusting
a fixed delay model. All are experimental and index by event date.

``` r

# Visual: rolling fan chart of the delay distribution (median, mean, 25-75 & 10-90 bands)
plot_delay_drift(tn, window = 7, by_strata = FALSE, changepoint = FALSE)

# Gradual monotonic trend (autocorrelation-robust Mann-Kendall; needs `modifiedmk`)
test_delay_drift(tn, stat = c("median", "spread"))   # location AND dispersion

# Abrupt shift (Pettitt change-point test; no extra dependency)
test_delay_changepoint(tn, stat = c("median", "spread"))
```

- **[`plot_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_drift.md)**
  — solid = rolling median, dashed = rolling mean, bands = 25–75% /
  10–90%. `window` defaults to **7 periods** (7 days for daily, 7 weeks
  for weekly). The recent, not-yet-complete region (after the `level`
  incompleteness cutoff) is **shaded grey** — do not read it as drift.
  `changepoint = TRUE` marks the estimated median change point. Supports
  `by_strata`.
- **[`test_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/test_delay_drift.md)**
  returns a tidy tibble (per `stat` × stratum) with the Kendall `tau`,
  Sen’s slope, `p_value` and a `drift` verdict; `method` is
  `"hamed-rao"` (default), `"yue-pilon"` or `"block-bootstrap"`. Tests a
  *location* (`"median"`/`"mean"`) and a *dispersion*
  (`"iqr"`/`"spread"`) statistic — drift can be in either. Runs on
  **mature** data only (`mature_only = TRUE`).
- **[`test_delay_changepoint()`](https://rodrigozepeda.github.io/tbl.now/reference/test_delay_changepoint.md)**
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
scr <- batch_test(tn, lookback = 3, alpha = 0.05)
scr[scr$batch, ]            # the flagged report dates
# LEAN output (v0.13.0): report_date, stratum, reported, baseline,
#   deficit (reports missing beforehand -> batch), delta (window total minus
#   expected -> creation), p_transport, p_transport_bh, batch (BH verdict).
#   The raw per-point `classification` column was REMOVED (over-identified;
#   BH `batch` is the trustworthy verdict). `baseline_method` arg also removed
#   (always repeated_median). transport_discriminant() KEEPS its classification.

# 2) Shape test: did ONE report date draw from unusually OLD event dates?
#    (complements the volume screen; `at` must be an observed report date)
batch_shape_test(tn, at = as.Date("2010-05-24"),
                 permute = "items")   # use "blocks" if counts are overdispersed

# 3) Validate a detector: plant a known batch and check it is recovered
planted <- simulate_batch(tn, closed_dates = as.Date(c("2010-05-10","2010-05-17")))
batch_test(planted)
```

- **[`batch_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_test.md)**
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
- **[`batch_shape_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_shape_test.md)**
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
  [`batch_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_test.md)’s
  classification: `hold_or_deletion` OVERRIDES `batch`/`surge` whenever
  creation_z \< -z_star, REGARDLESS of transport_z — so the most extreme
  top-left points are holds, not batches. Also:
  `plot_transport_discriminant` colours RED only BH-confirmed batches
  (`td$batch`), NOT the raw per-point `classification` (which
  over-identifies ~10-20% at alpha by construction).
- **`transport_discriminant(x, lookback=, period=, alpha=)`** — the
  plane behind
  [`batch_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_test.md):
  per report date the deficit `W` (transport) and `Δ = S − M`
  (creation), standardised as `transport_z` / `creation_z`, plus the
  quadrant `classification`. A batch = high transport, ~0 creation
  (top-left). **DEFAULT lookback = 7L** (changed from 3L, 2026-07-10)
  for batch_test/transport_discriminant. Discriminant shaded region
  labelled “Potential batch region”; confirmed batches get bold
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
  [`batch_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_test.md)
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
> [`batch_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_test.md) +
> [`batch_shape_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_shape_test.md).

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
| baselinenowcast | ✅ | ✅ | long df **or** reporting-triangle matrix ↔︎ count-incidence; `to` has `format = c("matrix","long")` — **`"matrix"` is the default**. `to`’s `delays_unit` defaults to `NULL` and is **inferred** from the object units (equal event/report units of `"days"`/`"weeks"`) for the matrix format, else supply it. Refuses `count-cumulative` input (would need to de-accumulate to possibly-negative increments) |
| EpiNow2 | ❌ | ✅ | `to` only. `model = "estimate_infections"` (default) → a single `date`/`confirm` series for `estimate_infections()`/`epinow()`. `model = "estimate_truncation"` → a list of `date`/`confirm` snapshots (one per report date) for `estimate_truncation()`, which *does* use the report dimension |
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
get_nth_reported_cases(x, delay)          # cumulative count within a given delay
```

## Reference: diagnostics & batches (all experimental)

``` r

autoplot(x, panels =, by_strata =)        # multi-panel diagnostic (patchwork)
plot_delay_drift(x) / test_delay_drift(x) / test_delay_changepoint(x)
batch_test(x) / batch_shape_test(x, at =) / simulate_batch(x, closed_dates =)
transport_discriminant(x)                 # deficit W vs discriminant Delta, per report date
diagnostic_plot(x, panels =, by =)        # reporting-process gallery (reporting/triangle/profiles/delay_drift/transport)
```

## Reference: package data

``` r

data(flusight)    # flu data in the United States (count-cumulative)
data(denguedat)   # dengue surveillance linelist (weekly, Puerto Rico)
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
  converts between the two count types **both ways**
  (incidence↔︎cumulative); `cumulative→incidence` de-accumulates and can
  yield **negative** increments. It can never rebuild a `linelist`.
- Undeclared extra columns are **summed away** by
  [`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md),
  not plotted per-value — a boxplot with spread on *daily* data is the
  days within a week, not a hidden stratum.
- Removing a protected column **downgrades to a tibble** — check
  [`is_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/is_tbl_now.md)
  after heavy dplyr.
- [`rowwise()`](https://dplyr.tidyverse.org/reference/rowwise.html) is
  **unsupported**.
- For weekly data with fractional `.delay`, use `align_weeks`.
- Count data types **require** a `case_count` column.
- `batch_shape_test(at =)` needs `at` to be an **observed report date**;
  the volume screen
  [`batch_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_test.md)
  scans them all.
- [`test_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/test_delay_drift.md)
  needs the `modifiedmk` package (a Suggests); the batch and drift
  diagnostics all print an experimental warning.
- `detect_report_batches()` / `plot_report_batches()` were **removed** —
  use
  [`batch_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_test.md) +
  [`batch_shape_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_shape_test.md).
