# Calendar effects on the case counts or on the reporting delay

**\[experimental\]**

One panel of
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html),
drawn on its own. Each function shows the same boxplots the
corresponding
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
panel does, for one calendar grouping:

- `plot_day_of_week_effects()` — by day of week (daily data only).

- `plot_week_of_year_effects()` — by epidemiological week.

- `plot_month_of_year_effects()` — by month (monthly data only).

- `plot_holiday_effects()` — by **day type** (`Weekday` / `Weekend` /
  `Holiday`, following the attached
  [`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md)
  spec).

- `plot_weekend_effects()` — the same panel, on an object that may not
  carry a spec yet: it attaches `temporal_effects(weekend = TRUE)` when
  there is no weekend effect already, so the weekend boxes appear
  without a separate
  [`add_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.md)
  call. The spec goes on a copy — your object is not modified — and a
  calendar already attached still contributes its `Holiday` box. Daily
  data only, since a weekend is a property of the day.

- `plot_holiday_lag_effects()` — by position relative to the nearest
  holiday (`"1 before"`, `"Holiday"`, `"1 after"`, ..., plus `"Other"`).

`type` picks which process to describe: `"epidemic"` (green — how the
*cases* vary by calendar group) or `"report"` (red — how the *reporting*
does).

The three day-type / holiday-lag functions have no `measure` argument:
they are always normalized. Their categories are not equal-sized parts
of a calendar block — the weekend is two days in seven — so a percentage
share would mostly restate the calendar rather than the data ("29% of
the cases at the weekend" is average, not low). The day-of-week,
week-of-year and month-of-year functions keep both measures.

Use these when you want one effect, in its own figure, at its own size;
use
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
when you want the diagnostic grid in one call. Everything else is the
same: `autoplot(x, panels = "calendar_weekday")` and
`plot_day_of_week_effects(x)` return the identical plot.

## Usage

``` r
plot_day_of_week_effects(
  x,
  type = c("epidemic", "report"),
  measure = c("percent", "normalized"),
  ...
)

plot_week_of_year_effects(
  x,
  type = c("epidemic", "report"),
  measure = c("percent", "normalized"),
  ...
)

plot_month_of_year_effects(
  x,
  type = c("epidemic", "report"),
  measure = c("percent", "normalized"),
  ...
)

plot_holiday_effects(x, type = c("epidemic", "report"), ...)

plot_weekend_effects(
  x,
  type = c("epidemic", "report"),
  weekend_days = c("Sat", "Sun"),
  ...
)

plot_holiday_lag_effects(x, type = c("epidemic", "report"), ...)
```

## Arguments

- x:

  A
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
  object.

- type:

  `"epidemic"` (default) for the case-count effect, or `"report"` for
  the reporting-delay one.

- measure:

  `"percent"` (default) for the share of cases in each group — "10% of
  cases in week 1 versus 3% in week 2" — with the IQR around it, or
  `"normalized"` for the value divided by its overall mean (`1` =
  average). See
  [`autoplot.tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/autoplot.tbl_now.md)
  for the blocks the percentages are taken over. The day-type and
  holiday-lag functions do not take it; they are always normalized.

- ...:

  Further arguments passed to
  [`autoplot.tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/autoplot.tbl_now.md),
  e.g. `by_strata`, `strata`, `plotly` or `palette`.

- weekend_days:

  Character vector naming the weekend days (default `c("Sat", "Sun")`),
  as in
  [`is_weekday()`](https://rodrigozepeda.github.io/tbl.now/reference/is_weekday.md).
  Used **only** when `plot_weekend_effects()` has to attach the effect
  itself; an object that already carries a weekend effect keeps the
  definition it was given.

## Value

A ggplot2 object (or a plotly widget when `plotly = TRUE`).

## See also

[`autoplot.tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/autoplot.tbl_now.md),
[`plot_cycles()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_cycles.md),
[`plot_delay_distribution()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_distribution.md),
[`plot_observed_cases()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_observed_cases.md);
[`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md)
and
[`add_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.md)
for the specification the day-type and holiday-lag panels describe.

## Examples

``` r
data(denguedat)
# First few years only, to keep the example quick; the full data works the same.
dengue_now <- tbl_now(denguedat[1:2500, ], onset_week, report_week, verbose = FALSE)

# How the cases vary by epidemiological week
plot_week_of_year_effects(dengue_now)


# The weekend on its own, on daily data, with no spec to attach first
days <- seq(as.Date("2021-01-01"), as.Date("2021-06-30"), by = "day")
daily_now <- tbl_now(
  data.frame(event_date = days, report_date = days + 1),
  event_date, report_date, verbose = FALSE
)
plot_weekend_effects(daily_now)


# ... and how the weekend shows up in the reporting delay
plot_weekend_effects(daily_now, type = "report")


# By month, on monthly-unit data
monthly_now <- tbl_now(
  data.frame(
    event_date  = seq(as.Date("2018-01-01"), as.Date("2021-12-01"), by = "month"),
    report_date = seq(as.Date("2018-02-01"), as.Date("2022-01-01"), by = "month")
  ),
  event_date, report_date,
  event_units = "months", report_units = "months", verbose = FALSE
)
plot_month_of_year_effects(monthly_now)


# ... and how the reporting does, as a share of the year's cases rather than
# normalized. `type` and `measure` compose.
plot_week_of_year_effects(dengue_now, type = "report", measure = "percent")


if (requireNamespace("almanac", quietly = TRUE)){

  ## By day type (weekday / weekend / holiday), once a holiday calendar is attached
  holiday_now <- dengue_now |>
   add_temporal_effects(temporal_effects(weekend = TRUE, holidays = almanac::cal_us_federal()))
  plot_holiday_effects(holiday_now)

  # By position relative to the nearest holiday
  holiday_lag_now <- dengue_now |>
    add_temporal_effects(temporal_effects(holidays = almanac::cal_us_federal(), holiday_lags = 2))
  plot_holiday_lag_effects(holiday_lag_now)

}
```
