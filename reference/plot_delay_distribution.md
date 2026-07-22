# Empirical distribution of the reporting delay

**\[experimental\]**

The `"delay_distribution"` panel of
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html),
drawn on its own: a case-count weighted histogram of `.delay`. For
**`count-cumulative`** data it becomes the *cumulative growth by delay*
instead — boxplots, on a log scale, of the ratio of each event date's
cumulative count at a delay to its count at the previous delay.

## Usage

``` r
plot_delay_distribution(x, ...)
```

## Arguments

- x:

  A
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
  object.

- ...:

  Further arguments passed to
  [`autoplot.tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/autoplot.tbl_now.md),
  e.g. `by_strata`, `strata`, `delay_distribution_xlim`, `plotly` or
  `palette`.

## Value

A ggplot2 object (or a plotly widget when `plotly = TRUE`).

## See also

[`autoplot.tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/autoplot.tbl_now.md),
[`plot_delay_profiles()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_profiles.md),
[`plot_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_drift.md).

## Examples

``` r
data(denguedat)
dengue_now <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)
plot_delay_distribution(dengue_now)
```
