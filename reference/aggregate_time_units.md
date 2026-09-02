# Coarsen a `tbl_now` onto a bigger time unit

**\[experimental\]**

Daily surveillance data is often too sparse to nowcast: most (event
date, report date) cells hold a zero or a one, and the delay
distribution is mostly noise. The usual fix is to work in weeks instead.
`aggregate_time_units()` does that in one call – it moves every date
onto the coarser grid, adds the counts up, and returns a `tbl_now` that
*knows* it is now weekly, so `.delay`, the converters and the models all
count in weeks from then on.

## Usage

``` r
aggregate_time_units(
  x,
  to = "weeks",
  axes = "all",
  label = c("start", "end"),
  align_on_day = 7,
  type = "epi",
  verbose = TRUE
)
```

## Arguments

- x:

  A
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
  object.

- to:

  Character. The unit to aggregate **to**: `"weeks"` (the default),
  `"months"`, `"years"` or `"days"`. It must be at least as coarse as
  the units of every axis being aggregated – there is no un-aggregating,
  so asking a weekly object for `"days"` is an error rather than a
  guess.

- axes:

  Character. Which time axes to aggregate: `"all"` (the default, meaning
  the event and report axes plus the validation axis when there is one),
  or any of `"event"`, `"report"` and `"validation"`. The unit they are
  aggregated to is `to`.

- label:

  Character. Which end of the period names it: `"start"` (the default –
  the Sunday of the epi week, the first of the month) or `"end"` (its
  last day). It only changes the *labels*, never which rows are pooled
  together, but see *Aggregating one axis only* for when it matters.

- align_on_day, type:

  Passed to
  [`align_weeks()`](https://rodrigozepeda.github.io/tbl.now/reference/align_weeks.md)
  when `to = "weeks"`: the weekday to snap to (ISO numbering, **1 =
  Monday**, default `7` = Sunday) and the week convention, `"epi"`
  (default) or `"iso"`.

- verbose:

  Logical. Whether to report what was aggregated. Default `TRUE`.

## Value

A `tbl_now` on the coarser grid, with `event_units`, `report_units` and
`validation_units` updated for the axes that were aggregated, and `now`
moved onto the new grid.

## Details

Each date is replaced by the **start of the period it falls in**: the
Sunday that begins its epidemiological week, the first of its month, the
first of January of its year (`label = "end"` names the period by its
last day instead). Weeks go through the same epi/ISO machinery
[`align_weeks()`](https://rodrigozepeda.github.io/tbl.now/reference/align_weeks.md)
uses, so `type` and `align_on_day` mean exactly what they mean there.

What happens to the rows depends on the data type:

- **`linelist`** – one row is still one case; only the dates move.

- **`count-incidence`** – rows that land in the same (event, report)
  cell are summed.

- **`count-cumulative`** – cumulative totals are **not** additive, so
  the series is de-accumulated to increments first, aggregated, and
  accumulated again on the new grid.

Two consequences worth knowing:

- **Weeks do not nest inside months.** Aggregating daily data to weeks
  and *then* to months is not the same as going straight to months: the
  second pass sees only the week's label, so a week beginning 31
  December lands in December even though most of its cases happened in
  January. Aggregate once, to the unit you actually want.

- An `NA` count means *not yet observed*, so a period containing one has
  an unknown total, not a total that quietly leaves it out. Use
  [`complete_zeroes()`](https://rodrigozepeda.github.io/tbl.now/reference/complete_zeroes.md)
  first if the `NA`s are really zeroes.

Any temporal-effect **columns** that were materialised by
[`compute_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.md)
are dropped, because a day-of-week term computed on daily dates is
meaningless once those dates are weeks. The lazy
[`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md)
spec is kept, so
[`compute_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.md)
will rebuild them on the new grid.

## Aggregating one axis only

`axes` exists because the two axes do not always move together: a system
may record the day a specimen was taken but only publish weekly report
batches. Two things follow:

- [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
  requires the report axis to be **at least as coarse** as the event
  axis, so aggregating only the event axis of a daily object is refused.
  Aggregate the report axis too, or use `axes = "all"`.

- A week named by the day it *starts* sits before every date inside it,
  so coarsening a **later** axis alone – the report against a daily
  event, or the validation against a daily report – with
  `label = "start"` produces negative delays, and
  [`validate_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md)
  warns. Use `label = "end"`: a report that arrived somewhere in week
  *W* is known by the end of *W*, which is the honest bound. When the
  axes move together the labelling cancels out and either choice gives
  the same delays.

## See also

[`align_weeks()`](https://rodrigozepeda.github.io/tbl.now/reference/align_weeks.md),
which snaps weekly dates to a common weekday without changing the units
or the counts;
[`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md)
to change data type without touching the dates;
[`complete_zeroes()`](https://rodrigozepeda.github.io/tbl.now/reference/complete_zeroes.md)
for the cells the coarser grid still leaves empty;
[`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)'s
`units` argument to declare the units up front.

## Examples

``` r
# A sparse daily line list: one or two cases a day.
df <- data.frame(
  onset = as.Date("2024-01-01") + c(0, 1, 3, 8, 9, 15),
  reported = as.Date("2024-01-01") + c(2, 2, 5, 9, 12, 16),
  sex = c("F", "M", "F", "M", "F", "M")
)
daily <- tbl_now(df,
  event_date = onset, report_date = reported, strata = sex,
  data_type = "linelist", units = "days", verbose = FALSE
)
get_event_units(daily)
#> [1] "days"

# The same cases, on a weekly grid: every date moves to the Sunday that
# starts its epidemiological week, and the delays are whole weeks.
weekly <- aggregate_time_units(daily, to = "weeks", verbose = FALSE)
get_event_units(weekly)
#> [1] "weeks"
weekly[[get_event_date(weekly)]]
#> [1] "2023-12-31" "2023-12-31" "2023-12-31" "2024-01-07" "2024-01-07"
#> [6] "2024-01-14"
weekly$.delay
#> [1] 0 0 0 0 0 0

# Count data is added up rather than merely relabelled.
counts <- to_count(daily, to = "count-incidence")
sum(counts$n)
#> [1] 6
sum(aggregate_time_units(counts, to = "months", verbose = FALSE)$n)
#> [1] 6
```
