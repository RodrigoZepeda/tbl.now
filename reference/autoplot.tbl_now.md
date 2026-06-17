# Diagnostic `autoplot` for a `tbl_now`

Produces a four-panel diagnostic overview of a `tbl_now` using ggplot2
and patchwork:

1.  **Empirical delay distribution** — a (case-count weighted) kernel
    density of the reporting delay (`.delay`).

2.  **Observed epidemic process** — the latest reported case counts per
    `event_date`, with a dashed vertical line marking where the data
    become incomplete (less than `level` of the delay distribution has
    arrived).

3.  **Calendar effect** — boxplots of the *normalized* effect (each
    event date's cases divided by the overall mean, so 1 is average) by
    calendar group. Daily data shows **both** a day-of-week and a
    week-of-year panel; weekly data shows week-of-year; monthly data
    shows month-of-year.

4.  **Seasonality** — a periodogram of the incidence series whose
    dominant peak suggests a Fourier season length for
    [`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md).

If the attached
[`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md)
spec contains a holidays calendar, the event dates that fall on a
holiday are marked with red dots in panel 2.

## Usage

``` r
# S3 method for class 'tbl_now'
autoplot(
  object,
  ...,
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

- level:

  Completeness level used for the incompleteness line in panel 2. The
  line is drawn at `now - q`, where `q` is the `level` quantile of the
  delay distribution. With the default `0.95`, the line marks where at
  least 5 percent of delays are yet to arrive.

- palette:

  A named character vector of colours. Defaults to the package palette.

- delay_distribution_xlim, event_date_xlim, calendar_effect_xlim,
  seasonality_xlim:

  Optional length-2 vectors giving the x-axis limits for the
  corresponding panel (delay density, epidemic process, calendar effect,
  periodogram). `NULL` (default) lets each panel pick its own range. For
  `event_date_xlim` pass `Date`s; the others take numeric limits.

## Value

A patchwork object combining the four panels.

## Details

**\[experimental\]**

## Examples

``` r
data(denguedat)
dengue <- tbl_now(denguedat, event_date = "onset_week",
                  report_date = "report_week", verbose = FALSE)
autoplot(dengue)


# Zoom the delay panel to delays of 0-10 weeks
if (FALSE){
  autoplot(dengue, delay_distribution_xlim = c(0, 10))
}
```
