# Plot a nowcast

**\[experimental\]**

Draws a fan chart of a
[tbl_nowcast](https://rodrigozepeda.github.io/tbl.now/reference/tbl_nowcast.md):
the counts reported so far as grey columns, one shaded band per central
prediction interval over them, and the median as a line, so that the
size of the correction the model is applying is visible as the gap
between the bars and the fan.

## Arguments

- object:

  A
  [tbl_nowcast](https://rodrigozepeda.github.io/tbl.now/reference/tbl_nowcast.md)
  object.

- ...:

  Unused; present for compatibility with
  [`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).

- levels:

  Numeric vector of central interval widths to shade. Defaults to the
  widest intervals available in the object.

- show_reported:

  Logical. Whether to draw the cases **reported so far** as columns
  under the fan –
  [`get_latest_reported_cases()`](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.md)
  on the object's own source data, so the bars are what the model was
  actually shown as of `now`, not what those dates eventually reached.
  The vertical gap between the top of a bar and the fan is the
  correction the nowcast is making, which is the whole reason to draw
  it. Requires the nowcast to carry its source data.

  The bars are one period wide, taken from
  [`get_event_units()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md).
  A fixed width would draw hairlines on a weekly series, and one wider
  than the step would make ggplot2 stack overlapping bars, so each would
  show several periods' counts rather than its own.

- colour:

  Colour of the fan. Defaults to the `tbl.now` palette's green: a
  nowcast is an estimate of the **epidemic** process (cases by event
  date), which the package always draws in green, with red reserved for
  the reporting process.

## Value

A `ggplot` object.

## See also

[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md),
[`nowcast_ensemble()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_ensemble.md)

## Examples

``` r
predictions <- tidyr::expand_grid(
  onset_week = as.Date("2020-01-05") + seq(0, 28, by = 7),
  .quantile_level = c(0.05, 0.25, 0.5, 0.75, 0.95)
)
predictions$.value <- 10 + 30 * predictions$.quantile_level
nc <- tbl_nowcast(predictions = predictions, method = "toy", event_date = "onset_week")

autoplot(nc)

```
