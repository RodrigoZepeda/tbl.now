# Diagnostic `autoplot` for a `tbl_now`

**\[experimental\]**

Produces a multi-panel diagnostic overview of a `tbl_now` using
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
and patchwork. Two families of panels are available — one describing the
**case counts** and one describing the **reporting delay** — and you
choose which to draw with the `panels` argument.

**Case-count panels**

- `"delay_distribution"` — a (case-count weighted) histogram of the
  reporting delay (`.delay`).

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

- `"seasonality"` — a periodogram of the incidence series whose dominant
  peak suggests a Fourier season length for
  [`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md).

**Reporting-delay panels** (to inspect *delay effects*)

- `"delay_weekday"`, `"delay_week"`, `"delay_month"` — boxplots of the
  *normalized* mean reporting delay (each event date's mean delay
  divided by the overall mean delay, so 1 is average) by day of week,
  epidemiological week, or month; these reveal whether the delay itself
  has a calendar pattern. Normalizing keeps them on the same scale as
  the case-count calendar panels and makes them comparable across
  strata.

- `"delay_seasonality"` — a periodogram of the mean-delay series, whose
  peak marks a cycle in the reporting delay (e.g. a weekly reporting
  rhythm).

The calendar/delay panels shown depend on the event unit: daily data
offers day-of-week **and** week-of-year panels, weekly data
week-of-year, monthly data month-of-year. The delay panels are computed
on the *complete* portion of the series (event dates on or before the
incompleteness line) so the recent reporting truncation does not bias
them.

## Usage

``` r
# S3 method for class 'tbl_now'
autoplot(
  object,
  ...,
  panels = "all",
  by_strata = FALSE,
  strata = NULL,
  level = 0.95,
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

- level:

  Completeness level used for the incompleteness line in the
  `"epidemic"` panel (and to trim the delay panels). The line is drawn
  at `now - q`, where `q` is the `level` quantile of the delay
  distribution. With the default `0.95`, the line marks where at least 5
  percent of delays are yet to arrive.

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

## Examples

``` r
data(denguedat)
# A recent window keeps the example fast.
recent <- denguedat[denguedat$onset_week >= as.Date("2010-01-01"), ]
dengue <- tbl_now(recent,
  event_date = "onset_week",
  report_date = "report_week", strata = "gender", verbose = FALSE
)
autoplot(dengue)


# \donttest{
# Only the reporting-delay calendar effect
autoplot(dengue, panels = "delay_calendar")


# A single panel (returned as a plain ggplot)
autoplot(dengue, panels = "delay_week")


# Split every panel by stratum
autoplot(dengue, by_strata = TRUE)


# Zoom the delay panel to delays of 0-10 weeks
autoplot(dengue, delay_distribution_xlim = c(0, 10))

# }
```
