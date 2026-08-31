# Diagnostic `autoplot` for a `tbl_now`

**\[experimental\]**

Produces a multi-panel diagnostic overview of a `tbl_now` using
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
and patchwork. Two families of panels are available — one describing the
**case counts** and one describing the **reporting delay** — and you
choose which to draw with the `panels` argument.

**Case-count panels**

- `"delay_distribution"` — a (case-count weighted) histogram of the
  reporting delay (`.delay`). For **`count-cumulative`** data this panel
  instead shows the *cumulative growth by delay*: boxplots (on a log
  scale, with a dashed reference at `1`) of the ratio of each event
  date's cumulative count at a delay to its cumulative count at the
  previous delay. A ratio above `1` is an upward revision, below `1` a
  downward one, and the boxes converge to `1` as reporting completes.

- `"epidemic"` — the latest reported case counts per `event_date`, with
  a dashed vertical line marking where the data become incomplete (less
  than `level` of the delay distribution has arrived). Holidays from the
  attached
  [`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md)
  spec are marked with dots.

- `"calendar_weekday"`, `"calendar_week"`, `"calendar_month"` — boxplots
  of the *normalized* case effect (each event date's cases divided by
  the overall mean, so 1 is average) by day of week, epidemiological
  week, or month.

- `"calendar_holiday"` — the same normalized boxplots by **day type**.
  The categories follow the attached
  [`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md)
  spec: a holiday calendar and a `weekend` effect together give
  `Weekday` / `Weekend` / `Holiday`, a calendar alone gives
  `Non-holiday` / `Holiday`, and a `weekend` effect alone gives
  `Weekday` / `Weekend`. A holiday falling on a weekend counts as a
  holiday.

- `"calendar_holiday_lag"` — the same normalized boxplots by **position
  relative to the nearest holiday**, as asked for by `holiday_lags` (see
  [`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md)):
  `"2 before"`, `"1 before"`, `"Holiday"`, `"1 after"`, ..., plus
  `"Other"` for every other day as the reference. It shows exactly the
  days the `..._holiday_lag_k` / `..._holiday_lead_k` columns flag —
  weekends and other holidays are skipped when counting working days —
  so you can see whether the lags you asked for are the ones that
  matter.

- `"seasonality"` — a **cycles** periodogram of the incidence series
  whose dominant peak suggests a Fourier season length for
  [`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md).

**Reporting-delay panels** (to inspect *delay effects*)

- `"delay_weekday"`, `"delay_week"`, `"delay_month"` — boxplots of the
  *normalized* mean reporting delay (each event date's mean delay
  divided by the overall mean delay, so 1 is average) by day of week,
  epidemiological week, or month; these reveal whether the delay itself
  has a calendar pattern. Normalizing keeps them on the same scale as
  the case-count calendar panels and makes them comparable across
  strata.

- `"delay_holiday"`, `"delay_holiday_lag"` — the reporting-delay twins
  of the two holiday panels above: the *normalized* mean delay by day
  type and by position relative to the nearest holiday. These are often
  the more telling pair — a holiday usually does not change how many
  cases occur, but it very much changes how long they take to be
  reported.

- `"delay_seasonality"` — a **cycles** periodogram of the mean-delay
  series, whose peak marks a cycle in the reporting delay (e.g. a weekly
  reporting rhythm).

Every panel is colour-coded by the process it describes — **red** for
the reporting-delay panels, **green** for the case-count (epidemic) ones
— and says which one it is in its subtitle, so a single panel still
reads on its own.

Which panels are available depends on the object. The **calendar/delay**
panels follow the event unit: daily data offers day-of-week **and**
week-of-year panels, weekly data week-of-year, monthly data
month-of-year. The four **holiday** panels describe the attached
[`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md)
spec, so they appear only when there is one to describe:
`"calendar_holiday"` / `"delay_holiday"` need a `holidays` calendar or a
`weekend` effect, and the two lag panels additionally need a non-zero
`holiday_lags`. Requesting a holiday panel without the matching effect
warns and skips it. The spec is read directly, so you do **not** need to
call
[`compute_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.md)
first.

The delay panels are computed on the *complete* portion of the series
(event dates on or before the incompleteness line) so the recent
reporting truncation does not bias them.

## Usage

``` r
# S3 method for class 'tbl_now'
autoplot(
  object,
  ...,
  panels = "all",
  by_strata = FALSE,
  strata = NULL,
  measure = c("percent", "normalized"),
  level = 0.95,
  plotly = FALSE,
  palette = .tbl_now_palette(),
  delay_distribution_xlim = NULL,
  event_date_xlim = NULL,
  calendar_effect_xlim = NULL,
  seasonality_xlim = NULL
)
```

## Arguments

- object:

  A `tbl_now` object.

- ...:

  Unused; present for compatibility with
  [`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).

- panels:

  Which panels to draw. Either a vector of the concrete keys listed
  above, or one of the aliases `"all"` (default; every applicable
  panel), `"calendar"` (the case-count calendar panels) or
  `"delay_calendar"` (the reporting-delay calendar panels). Selecting a
  single panel returns that panel as a plain ggplot2 object instead of a
  patchwork.

- by_strata:

  Logical (default `FALSE`). When `TRUE`, every panel is split by
  stratum: the calendar / delay boxplots become dodged boxes (one per
  stratum, side by side), the epidemic process and both periodograms
  become one coloured line per stratum (no area fill), and the delay
  distribution becomes dodged bars (one per stratum). The boxplots are
  then normalized **per stratum** (1 = that stratum's own average) so
  the calendar pattern is comparable across strata. Colours use a
  viridis scale. In this mode the holiday dots are omitted from the
  epidemic panel.

- strata:

  Character vector of column names to group by when `by_strata = TRUE`.
  `NULL` (default) uses the object's `strata` (see
  [`get_strata()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md));
  pass a subset (e.g. `strata = "gender"`) to group by only some of
  them. Ignored when `by_strata = FALSE`.

- measure:

  How to express the calendar-effect boxplots (the day-of-week,
  week-of-year, month-of-year, holiday and holiday-lag panels; every
  other panel ignores it).

  - `"normalized"` — the value divided by its overall mean, so `1` (the
    dashed line) marks an average level. Case-count panels normalize the
    cases per event date; delay panels normalize the mean reporting
    delay.

  - `"percent"` (default) — the **share of cases** falling in each
    group, as a percentage, so the box reads directly as "10% of cases
    at the weekend versus 90% on weekdays" with the IQR around it. One
    observation per calendar block: the seven weekdays (and the day
    types) are shared out within each **week**, the holiday lags within
    each **month**, and the epidemiological weeks and months within each
    **year**. The reporting-delay panels then switch from the event date
    to the **report date**, so they answer "what share of the reports
    *arrive* on a weekend?". Needs `Date` event/report columns.

- level:

  Completeness level used for the incompleteness line in the
  `"epidemic"` panel (and to trim the delay panels). The line is drawn
  at `now - q`, where `q` is the `level` quantile of the delay
  distribution. With the default `0.95`, the line marks where at least 5
  percent of delays are yet to arrive.

- plotly:

  If `TRUE`, return an interactive plotly widget (the panels stacked)
  instead of a static patchwork. Default `FALSE`.

- palette:

  A named character vector of colours. Defaults to the package palette.

- delay_distribution_xlim, event_date_xlim, calendar_effect_xlim,
  seasonality_xlim:

  Optional length-2 vectors giving the x-axis limits for the
  corresponding panel (delay-distribution histogram, epidemic process,
  calendar-effect boxplots, incidence periodogram). `NULL` (default)
  lets each panel pick its own range. For `event_date_xlim` pass
  `Date`s; the others take numeric limits.

## Value

A patchwork object combining the selected panels, or — when a single
panel is selected — that panel as a ggplot2 object.

## See also

[`diagnostic_plot()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnostic_plot.md)
for the companion gallery, which looks at the *reporting process* – when
reports arrived and whether any of it is artificial – rather than at the
case counts; the panels here as standalone functions:
[`plot_observed_cases()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_observed_cases.md),
[`plot_delay_distribution()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_distribution.md),
[`plot_cycles()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_cycles.md),
[calendar_effect_plots](https://rodrigozepeda.github.io/tbl.now/reference/calendar_effect_plots.md)
and
[`plot_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_drift.md);
[summary()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_summary.md)
and
[`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
for the same information as tables. The [*Describing and diagnosing a
tbl_now*
article](https://rodrigozepeda.github.io/tbl.now/articles/describing-and-diagnosing.html)
reads the panels one at a time.

## Examples

``` r
data(denguedat)
# A few recent months keep the example fast; the panels look the same on
# twenty years of data, they just take longer to draw.
recent <- denguedat[denguedat$onset_week >= as.Date("2010-11-01"), ]
dengue <- tbl_now(recent,
  event_date = "onset_week",
  report_date = "report_week", strata = "gender", verbose = FALSE
)
autoplot(dengue)


# Only the reporting-delay calendar effect
autoplot(dengue, panels = "delay_calendar")


## A single panel (returned as a plain ggplot)
autoplot(dengue, panels = "delay_week")


# Split by stratum. `by_strata = TRUE` works on the whole gallery too; one
# panel keeps the example quick.
autoplot(dengue, panels = "delay_week", by_strata = TRUE)


# Zoom the delay panel to delays of 0-10 weeks
autoplot(dengue, panels = "delay_distribution", delay_distribution_xlim = c(0, 10))
```
