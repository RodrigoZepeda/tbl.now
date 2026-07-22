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
  spec). This is also the **weekend** effect: attach
  `temporal_effects(weekend = TRUE)` and the weekend becomes one of the
  boxes.

- `plot_holiday_lag_effects()` — by position relative to the nearest
  holiday (`"1 before"`, `"Holiday"`, `"1 after"`, ..., plus `"Other"`).

`type` picks which process to describe: `"epidemic"` (green — how the
*cases* vary by calendar group) or `"report"` (red — how the *reporting*
does).

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
  measure = c("normalized", "percent"),
  ...
)

plot_week_of_year_effects(
  x,
  type = c("epidemic", "report"),
  measure = c("normalized", "percent"),
  ...
)

plot_month_of_year_effects(
  x,
  type = c("epidemic", "report"),
  measure = c("normalized", "percent"),
  ...
)

plot_holiday_effects(
  x,
  type = c("epidemic", "report"),
  measure = c("normalized", "percent"),
  ...
)

plot_holiday_lag_effects(
  x,
  type = c("epidemic", "report"),
  measure = c("normalized", "percent"),
  ...
)
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

  `"normalized"` (default) for the value divided by its overall mean
  (`1` = average), or `"percent"` for the share of cases in each group —
  "10% of cases at the weekend versus 90% on weekdays" — with the IQR
  around it. See
  [`autoplot.tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/autoplot.tbl_now.md)
  for the blocks the percentages are taken over.

- ...:

  Further arguments passed to
  [`autoplot.tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/autoplot.tbl_now.md),
  e.g. `by_strata`, `strata`, `plotly` or `palette`.

## Value

A ggplot2 object (or a plotly widget when `plotly = TRUE`).

## See also

[`autoplot.tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/autoplot.tbl_now.md),
[`plot_cycles()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_cycles.md),
[`plot_delay_distribution()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_distribution.md),
[`plot_observed_cases()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_observed_cases.md).

## Examples

``` r
data(denguedat)
dengue_now <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)

# How the cases vary by epidemiological week
plot_week_of_year_effects(dengue_now)


# \donttest{
# ... and how the reporting does
plot_week_of_year_effects(dengue_now, type = "report")


# As a share of the year's cases, rather than normalized
plot_week_of_year_effects(dengue_now, measure = "percent")

# }
```
