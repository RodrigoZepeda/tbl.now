---
name: tbl-now
description: Use the tbl.now R package to declare, validate, manipulate, diagnose, visualize, convert, backtest, score, and ensemble epidemiological nowcasting data and results. Use for tbl_now inputs and package-agnostic tbl_nowcast workflows; use DEVELOPMENT_SKILL.md for changes to tbl.now itself.
---

# Use `tbl.now`

`tbl.now` provides two shared data contracts:

- `tbl_now`: a tibble plus the roles and metadata needed for nowcasting.
- `tbl_nowcast`: package-agnostic quantile predictions, optional draws, the
  original fit, and data provenance.

It is a data/specification and cross-engine workflow package, not a statistical
model. For native Bayesian models and RTMB diagnostics, use
`diseasenowcasting`; read that package's `SKILL.md` when needed.

Prefer exported functions over reading attributes or rebuilding objects by hand.
If an exact signature or backend limitation matters, inspect `?function`, the
package vignette, or the current source rather than relying on memory.

## Declare data

```r
library(tbl.now)

# One row per case
x <- tbl_now(
  cases,
  event_date = onset_date,
  report_date = report_date,
  strata = region,
  data_type = "linelist",
  verbose = FALSE
)

# One row per event/report/stratum cell
x <- tbl_now(
  counts,
  event_date = onset_date,
  report_date = report_date,
  case_count = n,
  strata = region,
  data_type = "count-incidence",
  now = as.Date("2026-09-01"),
  verbose = FALSE
)

is_tbl_now(x)
validate_tbl_now(x)
```

`event_date`, `report_date`, `case_count`, strata, covariates, censoring flags,
and revision columns use tidy-select. A string also works. You may supply one
date plus `delay`; the other date is reconstructed.

Set `units = "days"`, `"weeks"`, `"months"`, `"years"`, or `"numeric"` as a
shared default, or set `event_units`, `report_units`, and `revision_units`
separately. `"auto"` infers units. All delays must be whole numbers in their
declared units. For weekly dates recorded on inconsistent weekdays, use
`align_weeks = TRUE` or `align_weeks()`.

If `now` is omitted it is the latest observed event, report, or revision date.
Supply it explicitly for an as-of analysis.

### Data types

| `data_type` | Meaning | `case_count` |
|---|---|---|
| `"linelist"` | one row per case | absent |
| `"count-incidence"` | newly reported count in a cell | required |
| `"count-cumulative"` | cumulative count known by each report date | required |

Declare the type when semantics are known. Automatic inference treats any
decrease within an event-date series as incidence, so a cumulative revision
stream with downward corrections can otherwise be misclassified.

```r
get_data_type(x)
inc <- to_count(x, to = "count-incidence")
cum <- to_count(inc, to = "count-cumulative")
```

Line lists convert to either count type, but counts cannot reconstruct a line
list. Cumulative-to-incidence conversion differences successive snapshots and
may legitimately produce negative increments after downward revisions.
`to_count()` aggregates over undeclared columns; declare meaningful split
variables as strata before converting.

## Understand the object

`tbl_now()` creates protected columns:

- `.event_num` and `.report_num`, sharing the event-date anchor;
- `.delay = .report_num - .event_num`;
- when revisions exist, `.revision_num` and `.revision_delay`.

The declared date columns, count column, revision fields, censoring flags, and
generated columns are protected. Removing one demotes the object to a plain
tibble with a warning.

Use getters. Date/count/censoring getters return column names, not values.

```r
get_event_date(x); get_report_date(x); get_now(x)
get_event_units(x); get_report_units(x); get_data_type(x)
get_case_count(x); get_strata(x); get_covariates(x)
get_is_censored_report(x)
tbl_now_attributes(x)
```

Strata define separate nowcast series. Covariates are predictors, not output
series. In retrospective fits, covariates must reflect what was available at
that historical `now`; do not leak future realized values.

Use `add_*()` to append metadata, `change_*()` to replace it, and `remove_*()`
to drop it. Relevant families cover strata, covariates, dates, the count column,
censoring, revisions, and temporal effects.

```r
x <- x |> add_strata(age_group) |> add_covariates(humidity)
x <- change_now(x, as.Date("2026-08-15"))
x <- remove_covariates(x, humidity)
```

Most dplyr verbs preserve the class and metadata. `rename()` updates stored
column names. `summarise()` and `reframe()` preserve the class only when the
result still satisfies its contract. `rowwise()` deliberately demotes. After a
major reshape, check `is_tbl_now()` and rebuild with `as_tbl_now()` if needed.

## Revisions and censoring

An optional revision axis represents a report later resolving as
`"confirmed"`, `"retracted"`, or `"pending"`:

```r
x <- tbl_now(
  cases,
  event_date = onset,
  report_date = reported,
  revision_date = resolved,
  revision_type = outcome,
  is_censored_report = report_is_bound,
  is_censored_revision = revision_is_bound,
  data_type = "linelist"
)
```

The timeline is event <= report <= revision <= `now`. Pending rows normally have
no revision date. `revision_levels` maps source labels to canonical outcomes,
for example `c(confirmado = "confirmed", retirado = "retracted")`.

```r
has_revision(x)
get_revision_date(x); get_revision_type(x); get_revision_units(x)
get_revision_levels(x); get_is_censored_revision(x)
```

Censoring means a recorded date is a bound, not an exact arrival time. Use the
report- and revision-axis families consistently:

```r
censor_reports(x, condition, to_report = get_now(x))
censor_reporting_delays(x, .delay > 60, to_delay = 60)
censor_reporting_delays_above(x, max_delay = 60)

censor_revisions(x, condition, to_revision = get_now(x))
censor_revision_delays(x, .revision_delay > 30, to_delay = 30)
censor_revision_delays_above(x, max_delay = 30)
```

Conditions use data-mask expressions. Existing flags are merged. Revision
helpers do not invent resolution dates for pending cases.

## Obtain observed counts

```r
get_initial_reported_cases(x)
get_nth_reported_cases(x, delay = 2)
get_latest_reported_cases(x)

get_initial_revised_cases(x)
get_nth_revised_cases(x, delay = 7)
get_latest_revised_cases(x)
```

These return cumulative-style counts per event date and retain caller grouping.
The revision-family delay is measured from the event date; `.revision_delay` is
the separate report-to-revision lag. `type` can select `"total"`,
`"confirmed"`, `"retracted"`, `"pending"`, `"unknown"`, `"net"`, or
`"by_type"` where meaningful. Pending cases do not appear on the revision axis.

## Temporal effects

Temporal effects are lazy specifications. Attach first, then materialize only
when a consumer needs columns.

```r
spec <- temporal_effects(
  day_of_week = TRUE,
  weekend = TRUE,
  week_of_year = TRUE,
  seasons = 365,
  holidays = calendar,
  holiday_lags = 2,
  weekend_lags = 1
)

x <- x |>
  add_temporal_effects(spec, date_type = "event_date") |>
  compute_temporal_effects()

get_temporal_effects(x)      # specifications
get_temporal_effect_cols(x)  # materialized column names
```

`date_type` may be event, report, or revision date. Repeated
`add_temporal_effects()` calls append; `replace_temporal_effects()` replaces;
`remove_temporal_effects()` clears. Calling `compute_temporal_effects()` before
attaching a spec is a no-op. Holiday effects accept `almanac::rcalendar()`
objects; validate a custom calendar with `almanac::cal_events()` or
`almanac::alma_in()` before modelling it.

`aggregate_time_units()` coarsens dates and compatible effect specifications;
materialized effect columns are recomputed later. It never refines time units.

## Describe, diagnose, and plot

Use `summary()` to answer what the data contain and `diagnose()` for structural
problems. `validate_tbl_now()` and `diagnose()` share the same findings engine.

```r
summary(x, by_strata = TRUE)
diagnose(x, by_strata = TRUE)
autoplot(x, by_strata = TRUE)
diagnostic_plot(x)
```

Focused summaries include `cases_per_date()`, `delay_summary()`,
`zero_run_summary()`, `prop_censored()`, `prop_revision_type()`,
`prop_strata()`, `prop_covariate_levels()`, `date_ranges()`,
`triangle_occupancy()`, and `cumulative_growth()`.

`autoplot()` chooses applicable panels for the object's units, temporal-effect
spec, and revision axis, and lays them out as one column per process: epidemic,
reporting, and revision when the object declares one. The number of columns
follows the selection. Pass one panel name to receive one ggplot or select
families such as `"calendar"`, `"delay_calendar"`, or
`"revision_calendar"`. Use `tbl_now_palette()` and role names when recoloring.

`plot_delay_distribution()` and `plot_reporting_process()` take `axis` to swap
the reporting axis for the revision one, and `by_revision_type` to split the
panel by `confirmed` / `pending` / `retracted` / `unknown`. Both split by
default; `autoplot()` and `diagnostic_plot()` do not.

```r
plot_delay_distribution(x, axis = "revision")
plot_reporting_process(x, by_revision_type = FALSE)
```

Statistical diagnostics are explicit rather than part of `diagnose()`:

```r
plot_delay_drift(x)
diagnose_drift(x)          # requires suggested package modifiedmk
diagnose_changepoint(x)
diagnose_batches(x)        # volume screen
diagnose_batches2(x, at = candidate_date) # delay-shape test
transport_discriminant(x)
simulate_batch(x, closed_dates = dates)
```

Batch tests exclude censored arrivals by default because a bound is not an
arrival time. Other analyses retain those cases.

## Complete, update, aggregate, and convert

```r
complete_zeroes(x, max_delay = NULL, until = NULL)
update(x, new_data = new_rows)
aggregate_time_units(x, to = "weeks", axes = "all")
align_weeks(x)
week_2_date(data, week, year)
as_tibble(x, compute_temporal_effects = FALSE)
```

`complete_zeroes()` is for count data. A line list cannot represent a zero-row
period. Grids should extend to `now`, not merely the final observed row.

Converters cover `data.table`, `tsibble`, `epidist`, `epinowcast`, EpiNow2,
`baselinenowcast`, NobBS, and `surveillance`; use `as_tbl_now()` for supported
foreign objects and `tbl_now_to_*()` / `tbl_now_from_*()` when explicit control
is needed. Conversions may be lossy and report what they drop. Qualify
`tbl.now::tidy()` if another package changes `tidy.list()` dispatch.

## Fit and compare engines

Use a modelling package directly when you need its model constructors, priors,
optimizer controls, or native diagnostics. Use this common layer to run several
packages uniformly or compare their outputs.

```r
fit <- run_nowcast(x, engine_diseasenowcasting())

fit_a <- run_nowcast(x, engine_baselinenowcast(draws = 1000))
fit_b <- run_nowcast(x, engine_epinowcast())

tidy(fit)
as_tibble(fit)                 # quantiles
as_tibble(fit, type = "draws")
autoplot(fit)
autoplot(fit, date_lim = c(as.Date("2026-06-01"), NA), ylim = c(0, 500))
fit@fit                        # untouched backend result
```

`autoplot()`'s `date_lim` and `ylim` crop the plot with
`ggplot2::coord_cartesian()` rather than filtering the data, so the fan still
runs to the edge of the panel. `NA` leaves one end free.

Available constructors are `engine_diseasenowcasting()`,
`engine_baselinenowcast()`, `engine_epinowcast()`, `engine_nobbs()`,
`engine_surveillance()`, and `engine_epinow2()`. Use
`list_nowcast_methods()` to inspect availability. Set engine-specific controls
on the engine object, including `min_date`, `quantile_levels`, and `label`.
A `label` becomes the fit's `@method`, which is how two configurations of one
backend stay distinguishable in a backtest and an ensemble; without one the
fit is named after its package.

`engine_surveillance()` fits on `surveillance`'s own epochs, which start on a
Monday for weekly data and on the first of the month for monthly data. The
grids are snapped to that and the estimates are shifted back onto the object's
weekday, so Sunday-start epi weeks work unchanged; event dates spread over
several weekdays are refused, and `align_weeks()` is the cure.

Score against the full data, not the historical snapshot used to fit:

```r
score_nowcast(fit, truth = full_x)

bt <- nowcast_backtest(
  full_x,
  engine_diseasenowcasting(label = "structural"),
  engine_epinowcast(label = "renewal"),
  now_dates = as.Date(c("2026-06-01", "2026-07-01")),
  seed = 42
)

weights <- nowcast_weights(bt, type = "inverse_score")
ensemble <- nowcast_ensemble(
  structural = fit_a,
  renewal = fit_b,
  weights = weights
)
```

Engines in a backtest need unique labels and identical quantile levels. A seed
is derived per engine/date fit. `nowcast_ensemble(type = "quantile")` works with
quantiles; `type = "linear_pool"` requires draws from every member. Combine
only models with the same target semantics, dates, and strata. Use
`scoringutils::as_forecast_*()` for additional scoring workflows.

## Common failure modes

- Do not sum cumulative snapshots across report delays.
- Do not treat missing count cells as observed zeros; preserve `NA` versus `0`.
- Do not use future covariate values in historical backtests.
- Do not remove protected columns or use `rowwise()` and expect a `tbl_now`.
- Do not assume a censored report date is an exact arrival.
- Do not request arbitrary post-fit quantiles from backends that retained no
  draws; choose quantile levels at fit time where supported.
- NobBS `moving_window` counts event periods and must not exceed supplied
  history.
- EpiNow2 defaults may imply no reporting delay; provide epidemiologically
  meaningful generation-time and delay options.

For package development, read `DEVELOPMENT_SKILL.md` before editing source.
