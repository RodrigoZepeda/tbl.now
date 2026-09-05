# tbl.now 0.33.0

## `aggregate_time_units()` now coarsens the temporal-effect specification (#65)

Aggregating dropped the materialised temporal-effect *columns* but kept the
lazy `temporal_effects()` spec untouched, so the next
`compute_temporal_effects()` rebuilt a day-of-week column on dates that are all
the same weekday. The spec now moves onto the new grid with the dates:

* `day_of_week`, `weekend`, `day_of_month`, `holiday_lags` and `weekend_lags`
  are properties of a day, so they survive only `to = "days"`.
* `week_of_year` survives `"weeks"`, `month_of_year` survives `"months"`.
* `seasons` are **rescaled** rather than dropped -- a Fourier period is a
  length, so a 365-day season becomes a 52.14-week one, and
  `seasons = 52, season_length = 7` becomes `seasons = 52` in weeks. A period
  that comes out at two units or shorter is dropped: it is at or below the new
  grid's Nyquist limit.
* `holidays` are **kept**. On a grid coarser than days the holiday column stops
  being a 0/1 indicator and becomes the **share of the period's days that the
  calendar marks** (1/7 for a week containing Christmas Day). On daily data it
  is the same integer indicator as before.

A specification left with nothing in it is removed. `verbose = TRUE` reports
what was dropped and what was rescaled.

`add_temporal_effects()` (the `data.frame` method) gained a `units` argument for
this; `compute_temporal_effects()` reads it off the object.


## Breaking: the palette is named by ROLE, not by hue

Every colour in the package now comes from the exported `tbl_now_palette()`,
and every element of it is named for the **role** it plays rather than for the
colour it happens to be. The old names encoded the hue (`primary_green`,
`accent_red`, `near_black`), which made the palette impossible to re-theme:
somebody handing in a blue-and-orange palette had to call their orange
`primary_green` for the plots to find it.

| was | is |
|---|---|
| `accent_red` | `reporting` |
| `light_red` | `reporting_light` |
| `primary_green` | `epidemic` |
| `light_green` | `epidemic_light` |
| `medium_green` | `epidemic_mid` |
| `dark_green` | `epidemic_dark` |
| `near_black` | `ink` |
| `muted_green` | `ink_muted` |

The red/green *grammar* is unchanged -- red is still the reporting process and
green still the epidemic one. What changed is that the names now say that
instead of naming the pigment.

There is no deprecation shim. A `palette` argument carrying the old names is
rejected with an error listing the roles it is missing.

The palette also grew the colours that used to be written into the plotting
code as literals (`"grey60"`, `"white"`, `"#C4D5DE"`): `ink_inverse`,
`surface`, `surface_muted`, `surface_dark`, `grid_major`, `grid_minor`,
`guide`, `guide_strong`, `annotation`, `neutral`, `zero`, `pending` and
`observed`. A re-theme now reaches every mark on the page, and no plotting
function contains a hex code.

`tbl_now_palette()` fills every role from its own defaults, so a partial
override is still a complete palette:

```r
plot_reporting_triangle(x, palette = tbl_now_palette(reporting = "#5B4B8A"))
```

## New: `plot_weekend_effects()`, and the day-type panels are normalized only

`plot_weekend_effects()` is `plot_holiday_effects()` for the common case of
wanting to see the weekend without first writing the specification down. It
attaches `temporal_effects(weekend = TRUE)` when the object does not already
ask for a weekend effect -- on a copy, so your object is unchanged -- and then
draws the same day-type panel. A holiday calendar that is already attached
still contributes its `Holiday` box, and a `weekend_days` argument sets the
weekend definition when this is the call that has to attach it. It needs daily
data, and says so rather than drawing a single box:

```r
plot_weekend_effects(daily_now)                  # the weekend, in one call
plot_weekend_effects(daily_now, type = "report") # ... and in the delay
plot_weekend_effects(weekly_now)
#> Error: A weekend effect needs daily data.
#> x `event_units` is "weeks".
```

**Breaking:** the four holiday panels -- `"calendar_holiday"`,
`"calendar_holiday_lag"` and their `delay_*` twins -- are now always drawn
`measure = "normalized"`, and `measure` is gone from the signatures of
`plot_holiday_effects()` and `plot_holiday_lag_effects()` (it stays on
`plot_day_of_week_effects()`, `plot_week_of_year_effects()` and
`plot_month_of_year_effects()`). `autoplot()` keeps the argument and simply
ignores it for those four panels.

A percentage share only means something when the groups are comparable slices
of the block it is taken over. A weekend is two days in seven, so "29% of the
cases at the weekend" is exactly average and reads as low; the same goes for
"1 working day after a holiday", which is however many days the calendar
happens to put there. Normalizing against the mean asks the question the panel
is for -- is this day type *unusual*? -- and 1 is the answer for "no".

The day-type panels are also retitled **"Weekend and/or holiday effects"**
(and "Weekend and/or holiday delay effects" for the reporting twin), because
that is what their categories are: the panel has never been only about
holidays. The holiday-lag panels keep their own titles.

## Every plot takes `size` and `linewidth`

Point, label and line sizes were hard-coded, so a figure drawn large had marks
too small to see. Outside `plot_reporting_hexamap()` they are **multipliers**
defaulting to `1`, which leaves every existing figure unchanged and, unlike an
absolute size, preserves a panel's own hierarchy: `plot_transport_discriminant(x,
size = 2)` doubles both the unflagged points (`1.1`) and the confirmed batches
(`2.6`) rather than flattening them to one value.

Where a function draws a reference grid of its own -- not \pkg{ggplot2}'s panel
grid, which these plots switch off -- that grid has its own absolute argument:

* `plot_reporting_hexamap()`: `size`, `shape`, `text_size`,
  `grid_linewidth_major`, `grid_linewidth_minor`, `axis_linewidth`,
  `legend_width`, `legend_height`
* `plot_reporting_triangle()`: `size`, `grid_linewidth`
* `plot_transport_discriminant()`: `size`, `grid_linewidth`
* `plot_delay_drift()`: `linewidth`, `grid_linewidth`
* `plot_delay_profiles()`: `linewidth`
* `plot_validation_delay()`: `linewidth`
* `autoplot()` and the `plot_*_effects()` panels: `size`, `linewidth`
* `diagnostic_plot()`: `size`, `linewidth`, `grid_linewidth`, forwarded to
  whichever panels draw them
* `autoplot()` on a `tbl_nowcast`: `linewidth`

`plot_reporting_process()`, `plot_epidemic_process()`, `plot_scalogram()` and
`plot_validation_status()` draw only bars, tiles or areas, so they take
neither, and say so in their documentation rather than offering an argument
that would do nothing.

`plot_validation_delay()` and `plot_validation_status()` also gained the
`palette` argument they had been missing -- they used to call the default
palette internally -- and now use the shared package theme instead of a bare
`ggplot2::theme_minimal()`.

## Breaking: the hexamap draws points, not hexagons

`plot_reporting_hexamap()` marks each `(event, delay)` cell with a point at the
centre of the hexagon it used to fill. A hexagon is drawn in data units: it
tiles at any zoom, and that is exactly why it cannot be made bigger -- there is
no room. A point is drawn in millimetres, so `size` is a free knob, which is
what the plot needed. The projection, the triangular grid and the axes are
unchanged; `shape = 15` gives squares, which tile the lattice closely.

Because a point is sized in millimetres and the lattice in data units, no
default `size` can suit every combination of cell count and figure size. That
is the argument's reason for existing: raise it until the marks nearly touch at
the size you are actually drawing.

Code reading the built plot changes with it -- the marks are one row per cell
in a `geom_point` layer rather than six polygon vertices per hexagon.

## A batch screen you have subset stops claiming to be one

`diagnose_batches()` and `transport_discriminant()` return tibbles with a
`print()` method that summarises the screen. Taking a few columns out of one --
the obvious way to look at a result --

```r
flagged <- screened[screened$batch, ]
flagged[order(flagged$p_transport_bh), c("report_date", "reported", "baseline")]
```

kept the class, so auto-print went looking for the `batch` column it needed and
aborted *inside the print method*, which is the hardest place to read an error:

```
Error: Can't subset rows with `!is.na(x$batch) & x$batch`.
x Logical subscript must be size 1 or 4, not 0.
```

Both classes now demote to a plain tibble when a subset drops a column their
summary reads, taking the screen's own attributes (`lookback`, `alpha`, ...)
with them -- the same rule as a `tbl_now` losing a protected column, silently
rather than with a warning, since what is lost here is a print format and not
the ability to nowcast. `[`, `dplyr::select()` and `dplyr::mutate()` all follow
it; row subsetting, `head()` and `dplyr::filter()` keep every column and so
keep the screen. As a backstop, both `print()` methods now fall back to printing
the table when a column has gone missing some other way (`x$batch <- NULL`),
rather than erroring.

## `diagnose_batches()` and `transport_discriminant()` print properly again

Both classes have a `print()` method, and neither was reached. They were
registered with a plain `@export`, which puts the method in the package's own
methods table; the namespace defines an S7 `print` generic that shadows
`base::print` once `tbl.now` is attached, so auto-printing either object fell
through to the default and showed a bare tibble instead of the batch screen or
the discriminant summary. Both are now registered with
`@exportS3Method base::print`, as every other `print()` method in the package
already was, and there is a regression test asserting auto-print dispatch for
each.

## Plot backgrounds match the site in dark mode

On the pkgdown site every figure was a black rectangle on a charcoal page. The
cause was not in the R code: pkgdown ships one PNG for both themes and relies
on a bslib CSS filter, `invert(100%) hue-rotate(180deg)`, which takes a
ggplot2 background from white to `#000000` while the body is `#212529`.

`pkgdown/extra.css` now overrides that filter with
`brightness(0.871) invert(100%) hue-rotate(180deg)` plus
`mix-blend-mode: lighten`. The `brightness()` before the invert maps white to
`#212121` while still taking black to white, so no contrast is lost; the blend
closes the remaining few units to the page's slightly blue `#212529` and, since
nothing in the filtered image is darker than `#212121`, touches nothing else.
The page background is unchanged, and light mode is untouched.

## Breaking: `diagnose_batch_shape()` is now `diagnose_batches2()`

The shape test is the second half of one question -- `diagnose_batches()` asks
*how many* records arrived on a date, `diagnose_batches2()` asks *which event
dates they came from* -- and the old name read as an unrelated function. It is
a straight rename with no deprecation shim: the function is experimental and
warns on every call.

## Breaking: `summary()` no longer reports autocorrelation or completeness

`case_autocorrelation()` and `reporting_completeness()` were written by an AI
and have not been reviewed by a human. They were part of `summary()`, so every
reader of a summary got two numbers nobody had checked, with nothing in the
output saying so.

Both are still exported, and both now **warn on every call** -- deliberately
not throttled, unlike the experimental diagnostics, because the caveat belongs
to the number rather than to the session that produced it. `summary()` loses
its `lags`, `completeness_delays` and `mature_only` arguments, which only ever
fed those two blocks; the `autocorrelation` and `completeness` components are
gone from its output.

```r
summary(x)                        # no longer contains those two components
case_autocorrelation(x, lags = 1) # still there, and says what it is
reporting_completeness(x)
```

## The batch family ignores censored arrival dates

The batch *tests* -- `diagnose_batches()`, `diagnose_batches2()` and
`transport_discriminant()` -- now drop the rows flagged censored on the axis
they are scanning: `is_censored_report` for `axis = "report"`,
`is_censored_validation` for `axis = "validation"`.

Only those three. The flag is a statement about the arrival axis and about
nothing else, so a row censored on the report axis is still a case that
happened on its event date: every plot, including `plot_epidemic_process()`
and the `diagnostic_plot()` panels other than `transport`, keeps every row.

A censored date is a **bound**, not the date the record arrived. Censoring is
usually applied *because* something was already known about those dates, and
the censored rows all carry the same bound, so leaving them in piles them onto
one date and the detector rediscovers, as a finding, the artefact it was told
about. Pass `drop_censored = FALSE` to scan them anyway.

## `diagnose_batches2()` no longer errors on a date with no arrivals

A line list cannot represent a zero, so a report date on which nothing arrived
has no rows at all. That is the observation "no arrivals", not a missing date,
and the test now reports `n_at = 0` for it instead of aborting. A date off the
object's report grid -- where there is nothing to compare against -- is still
an error, and the message now says the grid's step.

## `plot_transport_discriminant()` is usable as a plotly widget

* The axis labels were `expression()`s. `ggplotly()` cannot render plotmath and
  drops them silently, so the interactive plot had unnamed axes. They are now
  plain text ("Creation z", "Transport z").
* Hovering a point shows the dates behind it: the report (or validation) date
  the point *is*, the mean event date of the records that arrived then, the
  mean delay that implies, and the arrivals against their baseline. The two
  z-scores are what the point is already positioned by, so they are no longer
  all the tooltip says.

## `summary()` says what `n` and `total` count

One shared schema means the two count columns mean different things in
different blocks, and printing them side by side left the reader guessing. Each
block now prints a one-line gloss: `total` is always **cases**, and `n` is the
block's own unit -- dates on the grid for `cases`, runs for `zero_run`,
(event, report) cells for `delay` and `composition`.

## `complete_zeroes()` works when a date is missing (#66)

A single `NA` report date made every bound of the grid `NA`: `max_delay` came
out `NA` and `seq(0, NA)` aborted with `'to' must be a finite number`, which
says nothing about the missing date that caused it. The only workaround was to
`censor_reports()` first, which is a real answer but not the only one a user
might want.

Every bound is now computed ignoring the missing dates. A row whose event or
report date is `NA` has no cell on the rectangle, so it takes no part in the
grid -- but it is still a case, and it is carried through unchanged rather than
deleted by the closing `report_date <= bound` filter (`NA <= bound` is `NA`,
which `dplyr::filter()` drops). Only an object in which *every* row is missing
one of the two dates is refused, with a message saying so.

Two things fixed alongside it:

* `.event_num` was read off the completing join, so any row with no counterpart
  on the grid -- a negative delay, a missing report date -- had its event
  number blanked even though its event date was perfectly well known. It is now
  numbered from the event date itself.
* `max_delay = NULL` on data whose delays are all negative built a *decreasing*
  `seq(0, max_delay)`. The floor is now 0.

## Breaking (behaviour): a stratified `baselinenowcast` fit completes its grid (#67)

`run_nowcast(x, engine_baselinenowcast())` returned a different nowcast for a
line list than for the same object passed through
`to_count() |> complete_zeroes()` -- with the same seed. The stratified path
built its triangles from `tbl_now_to_baselinenowcast(format = "long")`, which
is a tidy data frame with no grid and so is deliberately never completed. A
line list has no row at all for an event period in which nothing was reported,
so the reference axis silently stopped short: 54 reference times where the
completed counts gave 81.

`nowcast_fit.baselinenowcast()` now asks for `format = "triangle_list"`, which
is the format that exists for exactly this -- one triangle per stratum. It
completes the grid, restores the not-yet-observed cells to `NA`, and absorbs
negative increments, none of which the hand-rolled split did. Stratified fits
on line-list input will change, and they now agree with the count path.

Because the triangle really does drop declared covariates, a stratified fit on
an object carrying them now warns that it did; the long format used to carry
them into a frame the fit then ignored.

## `tbl_now_to_EpiNow2()` completes a line list's grid (#67, audit)

Found by auditing every converter for the defect behind #67. A line list has no
row for an event period in which nothing was reported, and
`.epinow2_series_data()` built its `date`/`confirm` series from the rows it was
handed: on daily data the series stopped at the last period carrying a report
rather than at the object's [`get_now()`], which is the period the nowcast is
about. `estimate_truncation()` was worse -- `.epinow2_snapshots()` completes
each snapshot with `complete_zeroes()`, which *refuses* a line list, and the
surrounding `tryCatch()` swallowed the refusal and kept the short snapshot,
though `?estimate_truncation` asks for "a complete vector of dates".

`tbl_now_to_EpiNow2()` gains a `complete` argument with the same contract as
`tbl_now_to_baselinenowcast()`'s: `"auto"` (the default) completes **line-list**
input only, because count data can say "observed zero" itself and filling those
cells would claim reporting was complete when it was not. `TRUE` / `FALSE`
force either behaviour. All three series targets (`estimate_infections`,
`regional_epinow`, `estimate_truncation`) now reach the `now`.

The rest of the audit came back clean, and is now pinned by tests:
`epinowcast` completes through `enw_complete_dates()`, `NobBS` and
`surveillance` are handed the `now` by their fit methods (`NobBS(now =)`,
`get_surveillance_range()`), and `epidist` fits a delay distribution with no
event grid at all. `tests/testthat/test-engines-linelist-equivalence.R` asserts
that **every** engine returns the same nowcast, under the same seed, from a
line list and from the equivalent `to_count() |> complete_zeroes()` object.

# tbl.now 0.32.0

## Breaking: `diagnose()` no longer signposts the statistical tests

`diagnose_signposts()` is **removed**, along with the `"signposts"` check, and
the `not_run` status is gone from the findings schema -- `status` is now
`error` > `warning` > `note` > `ok` > `skipped`.

`diagnose()` still runs no statistical test, and for the same reason: drift and
batching are statements about a distribution, and answering them means choosing
a method, a window and a multiplicity correction. What changed is that it no
longer spends four rows of every report saying so. Call the tests yourself when
you want them:

```r
diagnose_drift(x, axis = "report")
diagnose_batches(x, axis = "report")
```

They are listed under `@seealso` on `diagnose()`, which is where a pointer
belongs.

## `diagnose()` findings

* **Right-truncation no longer reports an estimated 0%.** An event date can sit
  past the 95th percentile of the delay with its eventual total already in --
  the percentile is a bound on the delay, not a promise that something is
  outstanding. That case is now an `ok` finding ("none of their eventual total
  is still to arrive") instead of a note asking the reader to act on a 0%.
* **A truncation estimate with no mature history is `skipped`, not a note.**
  Without mature event dates there is no arrival curve to read the recent ones
  against, and the old code reported the resulting `0%` as if it were an
  estimate.
* **The sparsity finding now carries its denominator and a baseline.** It read
  "87% of the event dates on the grid carry no cases at all", which is both
  self-contradictory (a date on the grid is not an event date until it has a
  case) and unreadable without knowing how sparse the object is as a whole. It
  now reads "2489 of the 2861 dates on the event grid carry no cases at all
  (87%, against 73.2% pooled over every stratum)", and the hint says that when
  every stratum is mostly zeros the grid is finer than the data and
  `aggregate_time_units()` is the fix.

## Documentation

* The `covid_colombia` example no longer wraps itself in
  `requireNamespace("tbl.now")` -- a package's own example can assume the
  package.

# tbl.now 0.31.0

## Breaking: the `*_confirmed()` counters are gone, replaced by a validated-cases family (#64)

`get_latest_confirmed()`, `get_net_confirmed()`, `get_initial_confirmed()` and
`get_nth_confirmed()` are **removed**. They answered a version of the question
`get_latest_reported_cases()` already answered, in a different return shape (a
plain tibble), with a delay measured from a different anchor -- so the two
families could not be read against each other.

In their place, the reporting getters have an exact twin on the validation axis:

```r
get_initial_validated_cases(x)                     # as of the FIRST result back
get_latest_validated_cases(x)                      # everything settled so far
get_nth_validated_cases(x, delay = 7)              # settled within 7 periods
get_latest_validated_cases(x, type = "confirmed")  # was get_latest_confirmed()
get_latest_validated_cases(x, type = "net")        # was get_net_confirmed()
get_latest_validated_cases(x, type = "by_type")    # every outcome, side by side
```

* They return the **same `count-cumulative` `tbl_now`** the reporting getters
  return, carrying all three dates and the generated numeric columns, rather
  than a bare tibble.
* `type =` is new on **both** families, so the reporting axis can be filtered
  the same way: `"total"` (default), `"confirmed"`, `"retracted"`, `"pending"`,
  `"unknown"`, `"net"`, or `"by_type"` for one row per outcome. On an object
  with no validation process anything but `"total"` warns and pools.
* `get_nth_validated_cases()` counts the delay **from the event**, so it and
  `get_nth_reported_cases()` describe the same period. `get_nth_confirmed()`
  measured from the report, which is `.validation_delay` -- a different
  quantity. Reading the old and new numbers as the same thing is the one
  migration hazard.
* A pending case has no validation date, so it never appears on the validation
  axis; `type = "pending"` is refused there and belongs on the reporting axis.
* An empty selection -- nothing validated yet, no case with that outcome, no
  arrival within the delay -- is an **error naming the reason**, rather than a
  failure inside `tbl_now()` about an empty data frame.

## The reported-cases getters respect a grouping; `to_count()` says it does not (#61)

`get_latest_reported_cases()`, `get_initial_reported_cases()` and
`get_nth_reported_cases()` (and the three new validated ones) now **keep the
caller's grouping and answer by it**: the grouping columns join the event date
and the strata as keys, and come back on the result.

```r
tn |> dplyr::group_by(hospital) |> get_latest_reported_cases()
```

This is the only way to ask for a count by a **covariate** -- a column that
matters without being something you nowcast by. These verbs can do it because
they *select* a point in the process rather than reshaping the object.

`to_count()` cannot, and now **warns** rather than dropping the grouping in
silence: after aggregating, one row is an (event, report) cell rather than one
of the rows that were grouped, so the grouping describes nothing that is left.
Declare the column with `add_strata()` or `add_covariates()` to keep it out of
the sum.

## `is_tbl_now()` is a class check again, not a validation run (#62)

`is_tbl_now()` used to call `validate_tbl_now()` inside a `tryCatch()` that
caught errors but not warnings, so the object's findings escaped from wherever
the predicate was called -- which is every `.assert_tbl_now()` in the package.
A verb that fixed a problem warned about it twice, after the fix.

It is now a structural check: the class, the attributes a `tbl_now` cannot do
without, and the columns those attributes name. Cheap, and silent.

* `tbl_now_can_reconstruct()` suppresses warnings while asking its hypothetical.
* An object can be a `tbl_now` and still have data `validate_tbl_now()` warns
  about. That is the point: the class is a container, and a container is not a
  claim that what is in it is clean.

## Fractional delays are refused where they are created, and reported where they are found (#63)

A calendar has no half-days, so a fractional delay had to become something. It
became `round()` -- round-half-to-*even*, so `2.5` went down and `3.5` went up,
silently -- while the numeric axis refused the same value outright.

* `censor_reporting_delays(to_delay =)`, `censor_validation_delays(to_delay =)`
  and `tbl_now(delay =)` now **abort** on a delay that is not a whole number of
  the axis's units, on every axis. Round it yourself if that is what you mean.
* `validate_tbl_now()` **warns** when an object's `.delay` is fractional; it was
  a `diagnose()`-only note. The remaining way in is two date columns on
  different weekday grids, which is exactly what `align_weeks()` fixes -- so
  this stays a warning rather than an error, and the object you need to hand to
  `align_weeks()` can still be built.

# tbl.now 0.30.0

## New: coarsen the time grid in one call (#56)

`aggregate_time_units()` moves a `tbl_now` onto a bigger time unit -- daily to
weekly, weekly to monthly, monthly to yearly -- and updates the object so that
`.delay`, the converters and the models all count in the new unit:

```r
hai <- hai_bucaramanga |>
  tbl_now(event_date = specimen_date, report_date = report_date,
          strata = sex, data_type = "linelist", units = "days")

hai |> aggregate_time_units(to = "weeks")
```

* Counts are **added up**, not merely relabelled. `count-cumulative` totals are
  de-accumulated first, aggregated as increments and accumulated again on the
  new grid, because a cumulative total is not additive.
* `axes =` picks which axes move (`"all"`, `"event"`, `"report"`,
  `"validation"`), and `label =` picks whether a period is named by its first or
  its last day. Use `label = "end"` when you coarsen only a later axis, or a
  report lands before its own event.
* Weeks go through the same epi/ISO machinery as `align_weeks()`, so `type` and
  `align_on_day` mean what they mean there.
* It only ever coarsens: asking a weekly object for `"days"` is an error, not a
  guess. So is aggregating a `numeric` axis, which has no calendar.
* Weeks do **not** nest inside months. Aggregating to weeks and then to months
  is not the same as going straight to months; aggregate once, to the unit you
  want.

## New: censor by condition, and replace the date (#57)

`censor_reports()` and `censor_reporting_delays()` take a `filter()`-style condition and
record the matching rows as *bounds* rather than measurements -- optionally
replacing the date at the same time. This is the fix for the two dates that are
not really dates: the missing one, and the sentinel far in the future.

```r
hai |> censor_reports(is.na(report_date), to_report = Sys.Date())
hai |> censor_reports(report_date == as.Date("2222-02-22"), to_report = Sys.Date())
tn  |> censor_reporting_delays(.delay > 60, to_delay = 60)
```

### The censoring family is now six verbs, two axes by three ways to select

| | by date | by delay | threshold |
|---|---|---|---|
| **reporting** (`is_censored_report`) | `censor_reports()` | `censor_reporting_delays()` | `censor_reporting_delays_above()` |
| **validation** (`is_censored_validation`) | `censor_validations()` | `censor_validation_delays()` | `censor_validation_delays_above()` |

* `censor_delays_above()` is renamed **`censor_reporting_delays_above()`** and
  `censor_delays()` (added earlier in this release, never shipped) is renamed
  **`censor_reporting_delays()`**, so every name says which axis it moves. Their
  behaviour is unchanged, and the `_above()` help now says plainly that it
  considers as censored **every** delay longer than `max_delay`.
* `censor_validations()` and `censor_validation_delays()` are new: the
  validation-axis twins of `censor_reports()` and `censor_reporting_delays()`.
* All six are documented together on `?censoring`.

**`"pending"` cases are skipped when a validation date would be written**, with a
warning saying how many. A pending case is reported and still waiting, so it has
no resolution date; writing one would assert a resolution that never happened and
make the case look resolved to everything counting arrivals on that axis. Set
`validation_type` to `"confirmed"` or `"retracted"` first if the case really was
resolved. Flagging without a replacement is unaffected -- no date is written, so
nothing is contradicted.

* `NA` is not a match: a condition that cannot be evaluated on a row is not a
  condition that row met.
* Existing censoring flags are merged, never cleared, and the flag column is
  created as `.is_censored_report` when the object has none.
* Replacing a date moves `now` **forward** when the replacement lands after it,
  never backwards, and drops any `.report_*` temporal-effect column that the
  move has made stale.

## New: one `units` argument instead of three (#58)

`tbl_now()` gains `units`, the shared default for `event_units`, `report_units`
and `validation_units`:

```r
tbl_now(hai_bucaramanga, event_date = specimen_date, report_date = report_date,
        strata = sex, data_type = "linelist", units = "days")
```

Anything given explicitly still wins, so `units = "days", report_units = "weeks"`
reads a daily event date against a weekly report date, and an explicit
`event_units = "auto"` still means *infer*.

## Fixes

* `group_by()` (with no grouping variables), `summarise()` and `reframe()` copied
  the **event** units onto the report axis when rebuilding, so a mixed-unit
  object silently became a uniform one. They now carry `report_units` across.
* `infer_units()` on a column with a single distinct date warned about `min()`
  returning `Inf` before aborting with an unrelated message. It now says which
  column it is, and points at `units`.
* Censoring a grouped `tbl_now` aborted inside `add_is_censored_report()` /
  `add_is_censored_validation()`, which refuse a `grouped_tbl_now`. All four
  censoring verbs -- `censor_reports()`, `censor_reporting_delays()`,
  `censor_reporting_delays_above()` and `censor_validation_delays_above()` -- now ungroup,
  work, and put the grouping back.
* The two censoring axes share one implementation of "merge this flag in without
  un-censoring anything", rather than a copy each.
* **Demotion is now one operation.** Dropping a protected column returns a plain
  tibble, and that used to be `as_tibble()` -- which leaves unknown attributes
  alone on a tibble but rebuilds a `grouped_df` and drops them. So a demoted
  object kept the class's attributes, or lost them, according to whether the
  caller had grouped it. It now strips them explicitly, either way.
* `align_weeks()` failed on a grouped `tbl_now` with ``Column "now" not found in
  data``: it read `get_now()` and nine other attributes off its own input *after*
  demoting it, and worked only by the asymmetry above. It now reads them first,
  and returns the grouping.
* `complete_zeroes()` aborted on a grouped `tbl_now`
  (`'length = 2' in coercion to 'logical(1)'`): every bound it computes is a
  `filter()`/`distinct()`/`pull()` that a grouping turns into one value per
  group, so the date grid was built from a vector. The grid is a property of the
  object, not of how the caller grouped it.
* `tbl_now_to_epidist()` aborted on a grouped `tbl_now`; it was the only
  converter that did.
* `DEVELOPMENT_SKILL.md` gains *Every new function gets a grouped test* (§8) and
  a line in the definition of done, because the six grouping fixes above are all
  the same bug; `devel/audit_grouped_verbs.R` sweeps every exported function for
  it. Three verbs drop the grouping **deliberately** and are left for
  [#61](https://github.com/RodrigoZepeda/tbl.now/issues/61) to decide:
  `to_count()`, `get_latest_reported_cases()` and
  `get_initial_reported_cases()`.

# tbl.now 0.29.0

## Breaking: `is_censored` is now `is_censored_report` (#54)

There are two censoring axes now, so the unqualified name had to go. The old
spelling is removed outright, not deprecated:

| was | is |
|---|---|
| `tbl_now(is_censored = )` | `tbl_now(is_censored_report = )` |
| `get_is_censored()` | `get_is_censored_report()` |
| `add_is_censored()`, `change_is_censored()`, `remove_is_censored()` | `add_is_censored_report()`, `change_is_censored_report()`, `remove_is_censored_report()` |
| `is_censored` attribute | `is_censored_report` attribute |
| `.is_censored` (the column `censor_reporting_delays_above()` creates) | `.is_censored_report` |

## New: `is_censored_validation`, the validation-axis censoring flag (#53)

The twin of `is_censored_report`, for models that use censored validation
delays. It marks rows whose time from report to resolution is a **bound** rather
than a measurement.

* `tbl_now(is_censored_validation = )`, `get_is_censored_validation()`,
  `add_is_censored_validation()`, `change_is_censored_validation()`,
  `remove_is_censored_validation()`. It requires a `validation_date`: there is
  no validation delay to bound without one.
* The column is protected, is carried through every `dplyr` verb and through
  `to_count()`, `update()` and `align_weeks()`, and joins the grouping keys --
  a censored resolution and an exact one on the same `(event, report, outcome)`
  triple stay two rows rather than being summed into one.

### Breaking: `censor_validation_delays_above()` flags instead of erasing

It used to set the offending rows' `validation_type` to `"pending"` and delete
their validation date. That was wrong: a case confirmed after 200 days is still
a confirmed case, and the object should say so. It now sets
`is_censored_validation` and leaves the date and the outcome alone, exactly as
`censor_reporting_delays_above()` does on the report axis. `get_latest_confirmed()`
therefore still counts those cases.

## New: `validation_levels`, for data not recorded in English (#54)

`validation_type` may hold only `"confirmed"`, `"retracted"`, `"pending"` or
`NA` -- that was already enforced, and the error now names the way out.
`tbl_now(validation_levels = )` is that way out: a named dictionary whose names
are the labels in your data and whose values are the canonical four.

```r
tbl_now(casos,
  validation_type   = desenlace,
  validation_levels = c(
    confirmado = "confirmed", retractado = "retracted", pendiente = "pending"
  ),
  ...
)
```

The column is rewritten to the canonical values; the dictionary is kept on the
object and read back with `get_validation_levels()`. A dictionary that would
recode a canonical value into a different one is refused, because it would flip
the column on every rebuild.

## Fixed: `change_now()` re-censors instead of erroring (#51)

Moving `now` **backwards** is what `change_now()` is for -- it is how a backtest
walks through time. On an object carrying a validation process it aborted for
every `now` earlier than the last validation, which is nearly every historical
as-of date.

It now masks validations dated after the new `now`: the validation date becomes
`NA` and the outcome returns to `"pending"`, because a resolution that has not
happened yet is not a resolution. `change_now()` and `update_now()` gain
`verbose` to silence the report of how many rows were masked.

## `covid_us` carries a validation process (#52)

No shipped dataset had one, so every example fabricated an outcome by row
position. `covid_us` is rebuilt from the same CDC source with the two date
columns that were being left on the floor, and it now runs onset -> positive
specimen -> registration at CDC:

| was | is |
|---|---|
| `cdc_case_earliest_dt`, `cdc_report_dt`, `n` (2020-2021) | `onset_dt`, `pos_spec_dt`, `cdc_report_dt`, `current_status`, `sex`, `n` (2020) |

`cdc_case_earliest_dt` is CDC-derived and equals `onset_dt` for 99.997% of the
rows kept, so it is gone as redundant; `sex` is a stratum, and `current_status`
is the validation outcome -- in CDC's own words, so that mapping it is a worked
example of `validation_levels`. The relationship between outcome and validation
delay is real rather than fabricated: probable cases are registered a median of
2 days after the specimen, laboratory-confirmed ones 4 days. CDC does not
withdraw cases, so `"retracted"` does not occur.

# tbl.now 0.28.0

## Breaking: the confirmation process is now the validation process

The optional third date a `tbl_now` can carry is called a **validation** rather
than a confirmation, throughout. The old spelling is gone, not deprecated -- it
had not shipped.

| was | is |
|---|---|
| `add_confirmation()`, `change_confirmation()`, `remove_confirmation()` | `add_validation_date()`, `change_validation_date()`, `remove_validation_date()` |
| `get_confirmation_date()`, `get_confirmation_type()`, `get_confirmation_units()`, `has_confirmation()` | `get_validation_date()`, `get_validation_type()`, `get_validation_units()`, `has_validation()` |
| `confirmation_counts`, `confirmation_delay` | `validation_counts`, `validation_delay` |
| `censor_confirmation_delays_above()`, `diagnose_confirmation_delay()` | `censor_validation_delays_above()`, `diagnose_validation_delay()` |
| `plot_confirmation_delay()`, `plot_confirmation_status()`, `prop_confirmation_type()` | `plot_validation_delay()`, `plot_validation_status()`, `prop_validation_type()` |
| `confirmation_date`, `confirmation_type`, `confirmation_units` arguments | `validation_date`, `validation_type`, `validation_units` |
| `.confirmation_num`, `.confirmation_delay` columns | `.validation_num`, `.validation_delay` |
| `axis = "confirmation"` | `axis = "validation"` |
| `"event_to_confirmation"`, `"report_to_confirmation"` | `"event_to_validation"`, `"report_to_validation"` |

The **outcome values are unchanged**: a case is still `"confirmed"`,
`"retracted"` or `"pending"`. Validation is what the process does; confirmed is
one of the things it can conclude.

`diseasenowcasting::confirmation_process()` is that package's name and is
untouched -- `model(confirmation = confirmation_process())` still reads exactly
as it did.

## Documentation: fewer, fuller reference pages

* The validation getters now live on `?nowcast_data_getters`, next to
  `get_event_date()`, and the validation setters on `?add`, next to
  `change_event_date()`. Someone asking "what did this object record, and how do
  I change it" now finds every answer on one page instead of four.
* *Describing and diagnosing a tbl_now* and *Diagnosing reporting batches* are
  now **one article**,
  [*Diagnosing a tbl_now*](https://rodrigozepeda.github.io/tbl.now/articles/diagnosing-a-tbl-now.html),
  running structure-first: what is in the data (`summary()`), what is
  structurally wrong with it (`diagnose()`), and then the statistical tests
  `diagnose()` signposts but refuses to run.
* The attribute diagrams in the README now appear on the pkgdown site. They
  lived in `inst/figures/`, which pkgdown does not copy; `man/figures/` is the
  directory it publishes, and GitHub renders it just as happily.
* `summary()`'s `"completeness"` and `"growth"` rows are distributions over
  event dates, so they populate `mean`/`sd`/the quantiles (and, for
  completeness, `prop`) and leave the scalar `value` column empty. The
  documented examples selected `value` and got a column of `NA`; they now select
  the columns that carry the answer.

## Fixed: `baselinenowcast` on a snapshot ("as of") series

A snapshot stream restates the whole history in every snapshot, so its delay
axis is as long as the series itself and the reporting triangle comes out
square. `baselinenowcast` needs more reference dates than delay columns -- it
spends `max_delay` of them estimating the delay distribution and keeps two back
for the uncertainty model -- so it refused, with a message about reference-time
arithmetic that mentioned neither the delay axis nor anything to do about it.
Three of the six shipped datasets are that shape.

* `engine_baselinenowcast()` gains **`max_delay`**, the number of delay periods
  to keep, forwarded to `tbl_now_to_baselinenowcast()`. `?run_nowcast` already
  documented it ("`max_delay` caps the triangle's width"); what actually
  happened is that it fell into `...` and reached the modelling call, which has
  no such argument and ignored it.
* A triangle too wide to fit is now refused by `tbl.now`, naming the delay axis
  and a concrete cap -- the delay covering 99% of the reported cases:

  ```r
  run_nowcast(x, engine_baselinenowcast(max_delay = 21))
  ```

Note that a snapshot series must be **declared** `data_type = "count-cumulative"`.
`infer_data_type()` reads a single downward revision as incidence, by design,
and a revised running total has them; left to the inference, every delay carries
a whole period's count instead of an increment and nothing downstream can tell.

## New: `diagnose()` and `summary()` print as reports

Both still return the tibbles they always returned, and every `dplyr` verb still
works on them. What changed is what you see when you print one.

* `diagnose()` -- and each of its blocks -- prints the errors, warnings and
  notes in full, each with its hint, and counts the checks that passed, that
  were deliberately not run, and that could not be assessed. `print(x, all =
  TRUE)` spells those out too.
* `summary()` -- and each of its blocks -- prints one block per component,
  dropping the columns that component does not populate. The schema is wide
  because it holds every block at once; no block fills more than a handful of
  it.
* `tibble::as_tibble()` gives the plain table back in both cases.

## New: a nowcast prints its value at the `now` edge

`print()` on a `tbl_nowcast` now leads with the number it was fitted to produce
-- the estimate and interval at the last event date it covers, one line per
stratum -- before the quantile table, which starts at the oldest event date.

# tbl.now 0.27.0

## Breaking: a nowcast is specified with an `engine()`

`run_nowcast()` and `nowcast_backtest()` used to take a method **name** plus a
`...` (and, for the backtest, a `method_args` list of lists keyed by label). Both
failed the same silent way: an argument that missed its backend simply vanished,
and you got a fitted model at its default with nothing on the object to say so.

An **engine** is one modelling package plus every argument it needs:

```r
run_nowcast(x, engine_nobbs(max_D = 10, moving_window = 64))

nowcast_backtest(x,
  engine_baselinenowcast(draws = 1000),
  engine_nobbs(max_D = 10),
  now_dates = dates, seed = 20260824
)
```

* One constructor per supported package -- `engine_diseasenowcasting()`,
  `engine_baselinenowcast()`, `engine_epinowcast()`, `engine_nobbs()`,
  `engine_surveillance()`, `engine_epinow2()` -- each **naming** that package's
  own arguments, so they are visible in the signature and a typo is an error at
  the call rather than a default nobody notices. `...` still carries anything a
  named argument does not cover.
* `engine(method, ...)` is the general constructor and works for any registered
  method, including a backend you wrote yourself.
* **The data and `verbose` are the only arguments outside the engine.**
  `quantile_levels` moved onto it, because for `NobBS` it is a *fit-time* model
  argument (it lands in `specs$quantiles`, and NobBS keeps no draws, so a level
  it was never asked for cannot be recovered) rather than a way of summarising
  afterwards.
* `nowcast_backtest(x, ...)` now takes the engines **variadically**, or as one
  list. `methods` and `method_args` are gone. An engine's `label` is its name in
  the result; labels must be unique, and every engine must report the **same**
  quantile levels -- the WIS averages over the levels reported, so mismatched
  engines are not scoring the same quantity.
* `nowcast_method()` is **removed**. The engine is the object `nowcast_fit()` and
  `nowcast_tidy()` dispatch on, so an existing backend needs no change; write
  `engine("mymodel")` where you wrote `nowcast_method("mymodel")`.
* A bare method string is an error that names the constructor to use.

## New: `min_date`, per engine

Every engine takes `min_date`, saying how much history to fit on:

| `min_date` | means |
|---|---|
| `NULL` (default) | the whole series |
| a `Date` | keep event dates on or after it |
| a number | keep the last *n* periods before `now`, in the object's own units |

It is per engine on purpose. `baselinenowcast` and `diseasenowcasting` take a
long series in their stride, while `epinowcast` and `EpiNow2` scale with the
number of reference dates and are best given a window -- so one global `filter()`
over all of them was the wrong tool.

Prefer the **number** inside a `nowcast_backtest()`: `now` moves between fits, so
a fixed calendar cut makes the fitted window grow as the backtest walks forward
and the last fit is trained on more data than the first.

`min_date` trims the **event axis**, not `now`, and the trimmed object is what
the result carries -- so `score_nowcast()` and `autoplot()`'s reported counts
describe the series the model was actually shown.

## Breaking: `score_nowcast()` / `as_scoringutils()` take a `tbl_now` as `truth`

`observed_col` is **removed**, and a plain data frame is no longer accepted. The
`tbl_now` already knows which column holds the observed counts -- it is
`get_case_count()`, or the count `to_count()` produces from a line list -- so
naming it was a burden on the caller and the old default (*"the last column that
is neither the event date nor a stratum"*) was a guess that could mis-score
silently.

```r
score_nowcast(nc, truth = dengue)     # the FULL tbl_now, line list or counts
as_scoringutils(nc, truth = dengue)
```

## Breaking: argument names made consistent

A documentation audit read every exported function and found the same argument
wearing different names in different places. 116 of the 148 exports already took
`x` first; these were the exceptions.

* **`nowcast_fit()` and `nowcast_tidy()` take `engine`, not `method`.** This is
  the one that affects code outside the package: if you wrote a backend, rename
  the first argument of your methods.

  ```r
  # before
  nowcast_fit.mymodel  <- function(method, x, ..., quantile_levels, verbose) { }
  nowcast_tidy.mymodel <- function(method, fit, x, ..., quantile_levels) { }

  # after
  nowcast_fit.mymodel  <- function(engine, x, ..., quantile_levels, verbose) { }
  nowcast_tidy.mymodel <- function(engine, fit, x, ..., quantile_levels) { }
  ```

  What arrives has always been the engine -- `engine()`'s own documentation
  defines an engine as "the object `nowcast_fit()` and `nowcast_tidy()` dispatch
  on" -- and the old name was left over from the removed `nowcast_method()`. The
  argument is only a dispatch handle, so no method body needed changing;
  `R CMD check`'s S3 consistency check will flag yours until you rename it.

  `engine(method = )` and `list_nowcast_methods()` **keep** "method", where it
  correctly means the *name* of a backend rather than a configured engine.

* **`data` becomes `x`** in `diagnose_batches()`, `diagnose_batch_shape()`,
  `simulate_batch()`, `transport_discriminant()`, `censor_reporting_delays_above()` and
  `censor_confirmation_delays_above()`. Positional calls are unaffected. Two
  internal helpers also named `data` in their error messages, so
  `diagnose_batches(x = <not a tbl_now>)` used to complain about an argument that
  did not exist.

* **`quiet` becomes `verbose`** in `censor_reporting_delays_above()` and
  `censor_confirmation_delays_above()`, with the sense inverted and defaulting to
  `TRUE`, matching the twenty other functions that control messaging this way.
  Write `verbose = FALSE` where you wrote `quiet = TRUE`.

  The converters that carry **both** `verbose` and `quiet` keep both: they are
  different channels -- `verbose` is the conversion summary, `quiet` is the
  lossy-conversion warning -- and the documentation now says so.

## Breaking: `align_weeks()` numbers weekdays the ISO way

`align_weeks(align_on_day = )` counted weekdays from Sunday while
`is_weekday(weekend_days = )` counted them from Monday. `align_weeks()` now uses
ISO numbering too -- **1 = Monday ... 7 = Sunday** -- so the two agree.
`is_weekday()` is unchanged.

**The default is unchanged.** It becomes `7`, which is still Sunday, so
`align_weeks(x)` -- and `tbl_now(..., align_weeks = TRUE)`, which is where nearly
everyone meets it -- behaves exactly as before. Only an explicit `align_on_day`
changes meaning, and the migration is to subtract one, wrapping `1` to `7`:

| you wrote | you meant | now write |
|---|---|---|
| `1` | Sunday | `7` |
| `2` | Monday | `1` |
| `3` | Tuesday | `2` |
| ... | ... | ... |
| `7` | Saturday | `6` |

## New: `example_engine()`, a toy engine for examples

Every real engine needs its modelling package, so every example that fitted a
nowcast sat inside `\donttest{}` behind a `requireNamespace()` guard -- and none
of them ran on a default check. `example_engine()` needs nothing, is
deterministic, and returns in milliseconds, so the examples for `run_nowcast()`,
`nowcast_backtest()`, `nowcast_weights()`, `score_nowcast()` and `tidy()` on a
backtest now show real output.

It is not a nowcasting method. It ignores the reporting delay entirely --
reporting the counts that have arrived and putting a `spread`-wide band around
them -- so it under-predicts recent dates by construction. That is useful to
*see* and useless to rely on; the examples say so. Its source is also the
shortest complete `nowcast_fit()` / `nowcast_tidy()` pair in the package, if you
are writing a backend.

## New: `tbl_now()` warns on misspelled argument names

`tbl_now()` keeps unmatched `...` names as user metadata, which meant a typo in a
real argument name was accepted in silence. `case_col = "n"` set a useless
attribute and left count data typed as a line list -- as it had been doing in one
of this package's own examples.

Names close enough to a real argument to be a typo now warn and name the intended
one. Deliberate metadata (`data_source`, `citation`, `population`) stays silent:
a match needs a shared first letter and an edit distance under a third of the
longer name, which is what keeps `source` from being read as a misspelling of
`force`.

## `autoplot()` on a nowcast draws the reported counts as columns

The cases reported so far were points floating in the middle of the fan, which
reads as a second estimate. They are now grey **columns** under it, so they read
as a count measured from zero and the correction the nowcast applies is the
visible gap between the top of a bar and the band. The bars are one period wide,
taken from `get_event_units()`.

## `EpiNow2` keeps its draws

`nowcast_tidy.EpiNow2()` now reads the posterior samples with
`EpiNow2::get_predictions(format = "sample")` instead of the fit's
`lower_<pct>`/`upper_<pct>` summary. Before, EpiNow2 could report only a median
and the two tails of whatever `CrIs` it happened to be fitted with -- three
levels -- so `quantile_levels` could not be honoured, `tidy(probs =)` was an
error, and it could not join a `type = "linear_pool"` ensemble. It now does all
three. The summary path remains as a fallback for a fit `get_predictions()`
cannot read.

This has a visible knock-on: an ensemble containing EpiNow2 now shares all nine
of `nowcast_quantile_levels()` rather than collapsing to three.

## Performance: `tbl_now()` and every `dplyr` verb on one

No behaviour changed, but the class got substantially cheaper. `tbl_now()` is
about **3x faster** and `validate_tbl_now()` -- which runs on every `dplyr`
verb via `tbl_now_reconstruct()` -- about **4x**.

Almost all of the cost was building findings that were then discarded.
`validate_tbl_now()` reports at `floor = "note"`, so on a clean object it
formatted eleven `cli` messages and showed one; formatting is the expensive part
(a hint interpolating a vector of row numbers costs ~15 ms), and each finding
also built its own one-row tibble (~2 ms).

* `.diagnose_text()` now returns a **template** rather than a formatted string,
  and `.diagnose_finalise()` filters by the reporting floor *before* formatting,
  so only a finding somebody will read is paid for.
* Findings are plain lists until `.diagnose_finalise()` assembles the one tibble
  the caller sees.

`diagnose()` returns exactly the same tibble, and `validate_tbl_now()` the same
conditions.

## Documentation

Every reference page was read once, function by function, for an audience of
public-health practitioners first and statisticians second.

* **Eleven defects**, most of them found by running examples that had never been
  run. `tbl_now()` documented two attributes that do not exist (`repot_num` --
  a typo -- and `event_num`) and omitted four that do. The `align_weeks` example
  passed `case_col =`, which `...` swallowed, building count data as a line list.
  The `change` example referenced an undefined object that only survived because
  R never forced the promise. `update()`'s example built from the whole dataset
  and then "updated" it with rows it already held.
* **Fifteen pages shipped with an empty Description.** A block opening with a bare
  `` `r lifecycle::badge()` `` paragraph gets that badge as its *entire*
  `@description`, pushing the prose into Details; `?diagnose_drift` and fourteen
  others showed a badge and nothing else, in the help viewer and in the reference
  index.
* **Article links.** `vignettes/articles` is `.Rbuildignore`d, so
  `vignette("nowcasting-models")` and `vignette("custom-nowcast-models")` resolved
  to nothing in an installed package. All article references now use URLs.
* **Ten pages merged into five**, with aliases preserved so `?change` and existing
  links still resolve: `change` and `remove` onto `add`;
  `plot_reporting_process` onto `plot_epidemic_process`; `names_tbl_now` and
  `money_tbl_now` onto `assign_tbl`; `as_scoringutils` onto `score_nowcast`;
  `censor_confirmation_delays_above` onto `censor_reporting_delays_above`; `is_tbl_now` onto
  `validate_tbl_now`; `week_2_date` onto `align_weeks`;
  `compute_temporal_effects` onto `add_temporal_effects`.
* Every exported topic now has `@seealso`, `@return` and a runnable example; every
  internal function carries `@noRd`. Both `@examplesIf FALSE` blocks are gone, and
  nothing in `man/` contains `if (FALSE)` or `\dontrun{}`.
* `?tbl.now` was the DESCRIPTION text and nothing else. It now lays out the
  workflow -- declare, describe, diagnose, reshape, fit, check -- with a link into
  each step.
* Three slow examples trimmed: `align_weeks` ran the whole 452,567-row FluSight
  table (15.4s to 1.5s), `tbl_now_summary` computed `summary()` four times over,
  and both Stan examples fitted on twenty years of dengue data.

* `vignette("ensemble-nowcasting")` gains a figure of **the ensemble against each
  of its members**, and a section on `min_date` explaining why the engines are
  not all shown the same data.
* `data-raw/ensemble_comparison.R` fits both Stan back-ends with **approximate
  inference** (`epinowcast` through `enw_pathfinder()`, `EpiNow2` through
  `stan_opts(method = "pathfinder")`), so the article rebuilds in minutes rather
  than overnight. The article says so, so no member's band is mistaken for that
  package's tuned answer.
* It also **no longer fits three epidemics.** It scored every member on mpox and
  covid as well as dengue and cached the result as `forecasts`; no chunk in the
  article ever read that table, and it was roughly two thirds of the run time.
* `DEVELOPMENT_SKILL.md` records why the CRAN test path cannot be measured with
  `testthat::test_local()`, and `devel/TEST_SPEEDUP_BRIEF.md` is a standalone
  brief on the suite's runtime with measured per-file timings.

# tbl.now 0.26.0

## One `surveillance` line list per stratum

`tbl_now_to_surveillance()` gains `format = "linelist_list"`, which returns one
line list **per stratum** as a `tbl_now_surveillance_list` instead of one frame
with a pasted `strata` column. `surveillance::nowcast()` has no strata argument,
so a stratified analysis is one fit per stratum, and the split no longer has to
be done by hand:

```r
pieces <- tbl_now_to_surveillance(x, format = "linelist_list")
lapply(pieces, function(piece) surveillance::nowcast(data = piece, ...))
```

It mirrors `tbl_now_to_baselinenowcast(format = "triangle_list")` throughout: the
result is a **plain list**, so `lapply()`, `[[` and friends work unchanged; it is
length one and named `"all"` when the object declares no strata, so the return
type never depends on whether strata happen to be attached; it prints what it is;
and `as_tbl_now()` binds it back into a `tbl_now`, restoring the original
date-column names, the strata and the covariates. Count input comes back as a
`"linelist"` -- one row per case, totals unchanged -- because that is what a
`surveillance` line list holds.

`format = "linelist"` remains the default and is unchanged.

## Documentation

* The `surveillance` and `NobBS` sections of
  `vignette("nowcasting-models")` now say that the credible interval **is** in
  their figures and is simply too narrow to see: the median band over the plotted
  window is under 1% of the estimate for both, against 37% for `epinowcast`. The
  numbers quoted are computed from the cached `tidy()` tables rather than typed.
* `EpiNow2` gained the nowcast-vs-truth figure every other engine's section
  already had.
* The `surveillance` section fits its strata through the new
  `format = "linelist_list"`.

# tbl.now 0.25.0

## A vignette on writing your own back-end

`vignette("custom-nowcast-models")` is the full account of the `nowcast_fit()` /
`nowcast_tidy()` contract: what a method may assume about the `tbl_now` it is
handed (get the column names from the getters, work on `.event_num`/`.delay`
rather than the calendar, run the grid to `get_now()`, remember that a line list
cannot hold a zero), how to reuse the `tbl_now_to_*()` converters and
`as_tbl_now()` instead of reshaping by hand, and what shipping a back-end in a
package involves.

The worked example is a **delay-ratio nowcast**: for each delay it takes the
median of the factor by which past mature event dates grew from that delay to
their eventual total, and applies the empirical quantiles of that factor to the
counts reported so far. It needs no modelling package, so the article runs every
line of its own code -- including the scoring, the backtest and the ensemble --
and it is written twice, once returning `predictions` and once returning `draws`,
to show both branches of the contract.

`vignette("ensemble-nowcasting")`'s section 4 now points here instead of carrying
its own smaller version of the same material.

## Bug fixes

* `autoplot()` on a `tbl_nowcast` drew **only the 50% band**. The tails of each
  central interval were matched to the requested width by exact equality, and
  `(1 - (1 - 2 * 0.05)) / 2` is not `0.05`, so every other band came out as an
  `NA` ribbon and was silently dropped by `ggplot2`. The default nine quantile
  levels now draw all four bands, and `levels =` is matched with a tolerance too.

## Documentation

* `?nowcast_tidy` said its `...` was "available to your own" methods. It is not:
  `run_nowcast()` forwards the user's `...` to `nowcast_fit()` only, so anything
  the tidying step needs has to travel inside the fit object. Both help pages now
  say so.

# tbl.now 0.24.0

## `diagnose()`: a structural health check

`summary()` describes a `tbl_now`; `diagnose()` looks for what is **wrong** with
it. One row is one finding, sorted worst first, and the offending row indices
come with it:

```r
findings <- diagnose(dengue_now)
findings |> dplyr::filter(status <= "note")

bad <- findings |> dplyr::filter(check == "ordering")
dengue_now[bad$rows[[1]], ]
```

Ten checks: `declarations` (attribute types, the columns they name, role
collisions, columns the object was never told about, temporal effects added but
never materialised), `ordering` (`event <= report <= confirmation`, including
the transitive leg that a missing `report_date` would otherwise hide),
`missing`, `duplicates`, `units`, `negatives`, `now`, `truncation`, `strata` and
`signposts`. Each is also an exported function of its own -- see
`?nowcast_diagnose_components` -- and `diagnose(x)` is exactly the
`dplyr::bind_rows()` of them.

`status` is an **ordered factor**, worst first, which is why the tibble sorts
itself and why `status <= "note"` reads as "anything worth acting on":
`error` > `warning` > `note` > `ok` > `not_run` > `skipped`.

Four decisions worth knowing about:

* **It runs no statistical test, ever.** Whether the reporting delay drifts, and
  whether reports arrive in batches, are statements about a *distribution*.
  Answering them means choosing a method, a maturity window and a multiplicity
  correction, and `diagnose()` has no business choosing those on your behalf. It
  emits `not_run` rows carrying the call instead -- `diagnose_drift(x, axis =)`
  and `diagnose_batches(x, axis =)`.
* **Reporting outages are deliberately not detected.** A `tbl_now` does not
  carry the zeroes, so an absent row means "nothing was reported" and a quiet
  Sunday is structurally identical to a three-week outage. Telling them apart
  requires asking whether a run of zero-arrival dates is improbably long, which
  is a test. The descriptive answer is `zero_run_summary()`; the inferential one
  is `diagnose_batches()`.
* **An `NA` count is reported neutrally.** In a reporting triangle it means
  *not yet observed* -- correct data, and the thing that tells a nowcast the
  cell is still open -- so `diagnose_missing()` counts it without calling it a
  defect. An `NA` *date* is a different matter and stays a warning.
* **`diagnose_strata()` uses no thresholds.** "Too small to fit separately"
  depends on the engine and on the epidemic, so it names the extremes -- the
  smallest stratum, its case count and its share; the sparsest stratum and how
  much of the event grid it leaves empty -- and lets you judge.

## `validate_tbl_now()` is the same engine, presented as conditions

`validate_tbl_now()` no longer has a check list of its own. It calls the
findings engine and re-emits the result as the `cli` conditions it has always
emitted: it aborts on the `error`s and warns about the `warning`s. One
implementation, two presentations.

What that changes for you:

* **`validate_tbl_now()` now warns when a confirmation precedes its report.**
  That check existed, but only ran at construction, so an object that acquired
  the problem later never mentioned it again. `tbl_now()` no longer runs it
  separately, so it warns once rather than twice.
* Everything else aborts and warns exactly as before, including
  `warn_non_uniqueness`, which stays `FALSE` there. `diagnose()` defaults it
  `TRUE`.
* A `note` is never emitted as a warning. `validate_tbl_now()` runs inside every
  `dplyr` verb, and turning a `diagnose()` observation into a warning there
  would make construction noisy for data the class has always accepted.
* **One warning was reworded.** The missing-date warning said "*N* rows have
  NULL or NA values in column `event_date = "event_date"`" -- it printed the
  literal string rather than the column, and a column cannot hold `NULL`. It now
  reads "*N* rows have NA values in the event_date column `"onset_week"`".

## Breaking: the statistical tests take the `diagnose_` prefix

The five tests are named for what they are for rather than for the fact that
they are tests. **The old names are gone**, not deprecated:

| was | is now |
|---|---|
| `test_delay_drift()` | `diagnose_drift()` |
| `test_delay_changepoint()` | `diagnose_changepoint()` |
| `test_confirmation_delay()` | `diagnose_confirmation_delay()` |
| `batch_test()` | `diagnose_batches()` |
| `batch_shape_test()` | `diagnose_batch_shape()` |

The S3 class `batch_test`, and with it `print.batch_test()`, is renamed to
`diagnose_batches` to match.

## Documentation and website

`summary()` and `diagnose()` are now documented where people actually meet the
package:

* **A new article**, *Describing and diagnosing a `tbl_now`*, treats the two as
  one workflow: what the schema means, what the six statuses mean, why `skipped`
  is not `ok`, and why `diagnose()` refuses to run a statistical test.
* **The worked example article is restructured.** It now builds the `tbl_now`
  *before* cleaning and lets `diagnose()` report the defects, rather than
  checking for them by hand and hoping the list was complete. The hand-written
  cleaning is still there — it is now the *fix* for what was reported, and it
  keeps the one check `diagnose()` deliberately will not do for a line list
  (deduplicating on a record id).
* **The README and the introductory vignette** gain a compact section on each.
* **The reference index is now explicit.** `_pkgdown.yml` gained a `reference:`
  section grouping every exported topic, so `summary()`, `diagnose()` and their
  components are findable rather than buried in one alphabetical list. Note for
  contributors: `pkgdown` now **fails the build** on an exported topic that is
  not listed. `pkgdown::check_pkgdown()` catches it without building the site.

### Fixed: the light/dark switch never rendered

`template: light-switch: true` was set and `lightswitch.js` was being loaded,
but the site had no toggle. The control is a navbar **component**, and
`_pkgdown.yml` named an explicit `navbar: structure: right:` that replaced
pkgdown's default `[search, github, lightswitch]` without listing it. The script
loaded, the button did not exist, and nothing errored. `lightswitch` is now
listed explicitly.

# tbl.now 0.23.0

## `summary()` describes the object the way a nowcaster reads it

`summary()` on a `tbl_now` now returns a tibble rather than the column-by-column
listing `summary.data.frame()` produces, which said nothing about the structure
the class exists to carry. One row is one statistic of one quantity of one
stratum:

```r
summary(dengue_now) |> dplyr::filter(component == "delay")
```

It covers the case counts on each of the object's time axes (event, report and,
where there is one, confirmation), the delay distributions between them, the
lengths of the runs of zero dates, the compositional shares (censored, per
confirmation outcome, per stratum, per categorical covariate level), the lag-1
autocorrelation of each series, the reporting-completeness curve, the totals,
the date ranges and `now`, and how full the reporting triangle is.

Three decisions worth knowing about:

* **The date grids run to `now`, not to the last row present.** "Cases per event
  date" is a statement about a calendar; a date with no rows is a zero, not an
  absence. This is what makes `prop_zero` and the zero-run lengths mean
  anything, and it is why a **line list** -- which cannot represent a zero --
  summarises to exactly the same numbers as its counts. The grid is *global*, so
  a stratum whose cases start late shows its leading zeros and the strata stay
  comparable. So does the triangle-occupancy denominator.
* **Quantiles are the inverse-ECDF (type 1) estimator**, not
  `stats::quantile()`'s default: `q50` is the smallest value whose cumulative
  weight reaches `0.5`. This is the estimator `autoplot()` and
  `test_delay_drift()` already use, so the table and the figures agree, and it
  always returns a delay that was actually observed. The mean and standard
  deviation are the ordinary case-weighted ones, equal to expanding the counts
  to one row per case.
* **Not-yet-observed cells are dropped.** An `NA` count means the cell has not
  been observed yet, unlike a `0`, which was observed and was zero. Those rows
  carry no cases and are excluded, rather than turning every total they touch
  into `NA` -- which is what `flusight` did to an earlier draft. The
  `"unobserved_cells"` coverage row says how many were dropped.
* **`count-cumulative` data gets no delay rows.** A cumulative total is not
  additive across delays, so a case-weighted delay distribution would be
  meaningless; `delay_summary()` refuses it outright and points at `to_count()`.
  The new `"growth"` rows take its place, giving the ratio of each event date's
  running total from one delay to the next.

## Every block of the summary is its own function

`summary()` is exactly the `bind_rows()` of these, and each returns the same
schema, so they stack:

`cases_per_date()`, `delay_summary()`, `zero_run_summary()`, `prop_censored()`,
`prop_confirmation_type()`, `prop_strata()`, `prop_covariate_levels()`,
`case_autocorrelation()`, `date_ranges()`, `triangle_occupancy()`,
`reporting_completeness()` and `cumulative_growth()`.

`delay_summary()` names the three delays explicitly -- `"event_to_report"`,
`"event_to_confirmation"` and `"report_to_confirmation"` -- because the first
two are measured from the event and the last is the laboratory's own turnaround,
measured from the report, and confusing them is a documented hazard.

## Internal

One date-grid helper replaces three inlined copies of the same
`seq(from, to, by = <units>)` logic, including the one in `complete_zeroes()`
that only knew about days and weeks.

# tbl.now 0.22.0

## The back-ends that stratify by ONE column

`NobBS::NobBS.strat()` takes a single `strata` column name,
`EpiNow2::regional_epinow()` a single `region`, and `surveillance::nowcast()`
takes no strata argument at all. A `tbl_now` may declare several stratifying
columns, and their interaction -- "nowcast each observed combination separately"
-- is exactly one stratum to those back-ends. The converters now build that
column, so there is an argument to write:

* `tbl_now_to_nobbs()` and `tbl_now_to_surveillance()` gain `strata_col`
  (default `"strata"`) and `strata_sep` (default `" | "`). The declared strata
  are pasted into that one column, which `NobBS.strat(strata = "strata")` takes
  directly and which `split()` splits a `surveillance` line list on. The
  original columns ride along unchanged, and `strata_col = NULL` opts out.
* Pasting is refused rather than fudged when a **stratum value already contains
  the separator**: the label could not be split back apart, and a nowcast
  silently attached to the wrong stratum is worse than a failed conversion. The
  error names `strata_sep`. `tbl_now_to_EpiNow2(target = "regional_epinow")`
  gained the same check, which it did not have.
* Writing into an existing column is refused too, so a declared covariate
  called `strata` is not overwritten.

Previously `tbl_now_to_nobbs()` handed back the strata as ordinary columns and
nothing else, so there was no way to call `NobBS.strat()` on a multiply
stratified object at all. `run_nowcast(x, "NobBS")` had its own copy of the
pasting logic; it now uses the converter's column, so the two cannot disagree.

`tidy()` also learned the last per-stratum shape it did not know: a list of
`stsNC` fits, which is what `split()`-ing a `surveillance` line list and looping
produces.

## `tidy()` returns the quantiles a NobBS fit was asked for

`NobBS` keeps no draws, so `tidy(fit, probs = ...)` refused every `probs`
outright. But `NobBS(specs = list(quantiles = c(0.1, 0.5, 0.9)))` computes those
levels at fit time and puts them in `estimates` -- reading them back is a
lookup, not an approximation, and refusing it made the documented workflow
("ask at fit time, then request them with `probs`") impossible to complete.

`tidy()` now returns them. A level the fit was **not** asked for still aborts,
because that one really is unrecoverable, and the message now names the missing
levels and the `specs = list(quantiles = ...)` call that would have produced
them.

## The two date grids `surveillance::nowcast()` needs

* `get_surveillance_when(x, length = 30)` -- the dates to estimate, the most
  recent `length` steps ending exactly at `get_now()`.
* `get_surveillance_range(x)` -- the whole time axis, passed as
  `control$dRange`.

Both read the step off the object's own event units and abort on a `"numeric"`
grid rather than anchoring integer indices at the 1970 epoch. `dRange` matters
more than it looks: left to itself `nowcast()` infers the axis from the line
list it was handed, and **a line list cannot express a zero** -- the quiet days
at the `now` edge have no rows, so the inferred axis stops short of exactly the
days being nowcast.

## The article now runs the code it shows

`vignettes/articles/nowcasting-models.Rmd` displayed cached results next to code
that a separate script, `data-raw/nowcast_comparison.R`, kept its own copy of.
The two drifted, invisibly, because the article never ran what it printed.

`data-raw/nowcast_models_precompute.R` replaces it: it `knitr::purl()`s the
article, runs the article's own chunks with the fits live, and reads the
displayed objects back out by name. The code that produced every number is now
literally the code printed above it. Renaming an object in the article stops the
script with a list of what is missing instead of quietly saving a shorter file.

Fixed along the way, all of it drift the old arrangement hid:

* the Summary figure showed an unnamed grey `NA` line, because `EpiNow2` had no
  entry in the figure's colour scale and the factor dropped it to `NA`;
* two chunk labels were duplicated and two chunks called `tidy()` on objects the
  article never created, so the article could not be knitted at all;
* the `EpiNow2` delay section tidied a `dist_fit` that was never fitted, and the
  `epinowcast` seasonal fit was never assigned to a name;
* `regional_epinow()` was called without `truncation`, which is the one argument
  that makes it a nowcast -- the same trap the pooled section spends a warning
  box on;
* `epidist`'s **marginal** model is used, now that it compiles. It reads the
  aggregated weights the converter produces instead of expanding 6.1M cases back
  to one row each, which is why the latent model was there;
* the `epinowcast` sections filtered to **two years** of daily reference dates
  while every other engine used 60 days, and the article claimed that "keeps the
  Stan fit tractable". It does not: one chain spent **six hours** in a bad region
  of the posterior while the other chain of the same fit finished in sixteen
  minutes. The cached numbers had come from a 180-day fit that took six minutes,
  so the article had never run its own window. It is 180 days now, with the
  discrepancy explained in the text;
* the `epinowcast` fits were **unseeded** -- `epinowcast()` does not take R's
  `set.seed()`, so Stan drew its own each run and the same fit took 41 minutes
  once and six hours the next. Both now pass `seed` through `enw_fit_opts()`.

# tbl.now 0.21.0

## The confirmation process

A `tbl_now` can now carry a **third** date. Influenza is the picture to keep in
mind: symptoms begin (the event), the patient visits a doctor (the report), and
days later a swab comes back positive (the confirmation) or negative (a
*retraction* -- reported, but not a case after all). The assumed timeline is
`event <= report <= confirmation <= now`.

* `tbl_now()` gains `confirmation_date`, `confirmation_type` and
  `confirmation_units`. `confirmation_type` takes `"confirmed"`, `"retracted"`,
  `"pending"` or `NA`; **pending** means reported and still waiting, so it has no
  confirmation date, which is a different thing from a result you never
  recorded (`NA`). Two columns are derived: `.confirmation_num` (on the same
  numeric grid as the other dates) and `.confirmation_delay`, the laboratory's
  turnaround, measured **from the report**.
* `add_confirmation()`, `change_confirmation()`, `remove_confirmation()`,
  `get_confirmation_date()`, `get_confirmation_type()`,
  `get_confirmation_units()` and `has_confirmation()`.
* A date with no type warns rather than guessing: a date alone cannot say
  whether the case was confirmed or retracted. A confirmation before its own
  report warns too.
* `now` is confirmation-aware. A result issued on a date means the system was
  still being observed then, so `now` is never earlier than the last
  confirmation, and setting one earlier is an error.
* The confirmation columns survive `dplyr` verbs, `update()`, `align_weeks()`
  (which now aligns all three dates) and `to_count()` (which groups by the
  confirmation, so a case is never summed together with its own retraction).
* The print footer gains a confirmation line: the column, its units, and how
  many cases are resolved.

### Counting when cases can be undone

`get_latest_confirmed()`, `get_net_confirmed()` (confirmed minus retracted),
`get_nth_confirmed(x, delay)` and `get_initial_confirmed()` -- the confirmation
mirrors of the report-axis getters. `censor_confirmation_delays_above()` returns
implausibly long confirmations to `"pending"`, which is what they really were.

### Diagnostics on the confirmation axis

A laboratory clearing a backlog looks exactly like a surveillance system
clearing its inbox, so rather than duplicate every diagnostic, they take an
`axis = c("report", "confirmation")` argument: `batch_test()`, `batch_screen()`,
`batch_shape_test()`, `transport_discriminant()`, `plot_reporting_process()`,
`plot_epidemic_process()`, `plot_reporting_triangle()`, `plot_delay_profiles()`,
`plot_reporting_hexamap()`, `plot_scalogram()`, `plot_delay_drift()`,
`test_delay_drift()`, `test_delay_changepoint()` and `diagnostic_plot()`.

On the confirmation axis, delays are still measured **from the event**, so the
two axes are directly comparable and the gap between them is the time the
laboratory adds. Cases still `"pending"` are excluded -- counting them would
invent an arrival on a date they do not have.

New in their own right: `plot_confirmation_status()` (the confirmed / retracted
/ pending shares over time), and `test_confirmation_delay()` /
`plot_confirmation_delay()`, which ask whether retractions come back faster than
confirmations -- a laboratory that rules cases out sooner than it confirms them
biases any nowcast that treats the two alike.

## Other changes

* **Calendar temporal effects are now factors.** `day_of_week`, `day_of_month`,
  `month_of_year` and `week_of_year` are `factor`s with their full level sets
  (all seven weekdays, 1-31, 1-12, 1-52) rather than character or numeric
  columns, so a model gets dummy coding rather than treating "Tuesday" as
  twice "Monday", and a level absent from a stratum still exists. `weekend`
  stays 0/1 and the Fourier `seasons` stay numeric, as both are already
  correctly numeric.
* **Fixed:** the non-uniqueness warning fired on every confirmed/retracted pair.
  A case and its own retraction share an (event, report) combination and are
  still two different rows; the confirmation columns are now part of the key.
* `run_nowcast(x, "diseasenowcasting")` passes straight through to
  `diseasenowcasting::nowcast()`. The confirmation process belongs to that
  package's `model()`, not to `tbl.now`, so pass it there.

# tbl.now 0.20.0

## Bugs found by the new engine test suite

Every one of these was found by writing the tests, not before:

* **`count-cumulative` data failed on `diseasenowcasting` for want of a
  confirmation process.** `diseasenowcasting::nowcast()` auto-detects cumulative
  data and switches to the signed-increment Skellam / SkNB likelihood, but that
  likelihood needs a `confirmation_process()` -- the retraction side of a stream
  that can revise **down** -- and `model()`'s default is `no_confirmation()`.
  Without one the fit reports "Joint fit failed to converge for all init
  attempts". Pass one through, as
  `run_nowcast(x, "diseasenowcasting", model = model(confirmation = confirmation_process()))`.

  De-accumulating to incidence first would also "work", and is wrong: it
  discards the downward revisions the cumulative likelihood exists to model.
* **A censored report's window started before its own event date.** For
  `is_censored` rows, `.delay_censoring_windows()` bounded the secondary window
  below by the *earliest event in the data* rather than by that row's event, so
  every censored row implied a possibly-negative delay, and the zero-width guard
  pushed one strictly negative. \pkg{epidist} refuses it outright ("Assertion on
  `data$stime_lwr` failed: not >= 0") and `EpiNow2::estimate_dist()` would have
  fitted a delay distribution with mass below zero. The window is now
  `[event_date, report_date]`, and the zero-width guard widens **upward**.
* **`as_tbl_now(x, verbose = )` failed on two classes.**
  `as_tbl_now.tbl_now_triangle_list()` and
  `as_tbl_now.tbl_now_epinow2_snapshots()` passed `verbose` both explicitly and
  through `...`: "formal argument 'verbose' matched by multiple actual
  arguments". Both now default it into the dots, so the caller still wins.
* **`verbose = FALSE` was suppressing warnings, not just chatter.**
  `.quietly_if()` wrapped every backend in `suppressWarnings()`, which hid
  exactly the messages that say what the model actually saw -- strata pooled, a
  censoring flag collapsed, covariates dropped. It now suppresses **messages
  only**. This is the same failure mode DEVELOPMENT_SKILL section 9 records for
  `run_engine()`.
* **`diseasenowcasting` and `NobBS` were pooling multi-column strata
  needlessly.** `diseasenowcasting` models any number of strata and labels each
  combination `"F|N"`; `tbl.now` only ever read the one-column case and pooled
  otherwise. `NobBS.strat()` takes one column, so several are now joined into
  their interaction and split back apart. Both take **any** number of strata,
  and `?run_nowcast`'s table says so.

## Covariates and censoring are no longer dropped in silence

* `tbl_now_to_baselinenowcast()` (matrix and triangle formats),
  `tbl_now_to_epinowcast()` and `tbl_now_to_EpiNow2()` now **warn** when declared
  covariates cannot be carried, naming them and saying what to do instead.
  Materialised temporal-effect columns count as covariates: they are the case
  where somebody asked for an effect and would otherwise never learn it was
  ignored.
* The censoring collapse already warned; it is now *reachable* through
  `run_nowcast(verbose = FALSE)` because of the `.quietly_if()` fix above.

## `nowcast_truth()` removed

Dropped entirely rather than kept internal. It was `get_latest_reported_cases()`
reshaped. `score_nowcast()` and `as_scoringutils()` take the `tbl_now` itself as
`truth`.

## `covidat` removed

`covid_us` is kept: it is the only shipped dataset that actually exhibits
backlog dumps, which `vignette("batch-reporting")` is about. Measured against a
15-day rolling baseline, `covid_us` has 21 report days above 2x and 5 above 3x;
`covid_colombia` has one above 2x and none above 3x.

## New tests

All `skip_on_cran()`, all on **synthetic fixtures** built by
`tests/testthat/helper-engines.R` rather than on shipped data, so one axis can be
varied at a time:

* `test-engines-matrix.R` -- 24 real fits per fast engine
  ({0,2 covariates} x {0,2 strata} x {days, weeks} x the three data types), plus
  numeric-grid refusals, weekly-grid preservation, strata labelling for 0/1/2
  columns, counts-are-cases, and `run_nowcast()` against the hand-written call.
* `test-engines-covariates.R` -- used, or complained about, per converter.
* `test-engines-censoring.R` -- used, or announced, per converter.
* `test-converter-roundtrip-all.R` -- a registry of every `tbl_now_to_*()` shape
  and whether `as_tbl_now()` brings it back; **fails when a converter is missing
  from it**.
* `test-coercion-methods.R` -- every converter must expose the target package's
  own coercion generic (`as_reporting_triangle()`, `as_tsibble()`, ...) as a thin
  wrapper, or record why that package has none. It re-checks the "has none"
  claims against the installed package, so we find out if one gains a verb.

## Articles

* `vignette("nowcasting-models")`: **EpiNow2 is now a two-step fit.** Given only
  a reporting delay it does not nowcast at all -- its median stayed flat and sat
  below the already-reported count. `delays` says how infections become reports;
  it does not say the newest days are incomplete. Only `truncation` does, and
  that is what the report dimension of a `tbl_now` measures. Step 1 fits it with
  `estimate_truncation()`, step 2 passes it as `trunc_opts()`. Over the last
  seven days -- about 50% complete -- the fit now sits below the reported count
  on 4 of 21 stratum-days instead of most of them.
* `vignette("ensemble-nowcasting")`: the three-epidemic comparison is removed;
  the article is now about how to use ensembles.

## DEVELOPMENT_SKILL

* A pre-flight grep before writing any new function, with the two questions that
  decide whether it should exist, and the two times this package got it wrong.
* The target package's own coercion verb is now part of "writing a converter".

# tbl.now 0.19.0

## Converters no longer make you aggregate first

`covid_colombia` carries `sex`. An object built without `strata = sex` therefore
has **two rows per `(notification_date, diagnosis_date)` cell**, and a reporting
triangle, a `tsibble` key and an epinowcast observation table each have exactly
one slot per cell. Until now that meant `tbl_now_to_baselinenowcast()` aborted
("duplicate `reference_date` and `report_date` combinations") and
`tbl_now_to_tsibble()` aborted ("a valid tsibble must have distinct rows"), and
you had to `group_by() |> summarise()` before converting.

Both now pool undeclared columns for you, as `tbl_now_to_nobbs()`,
`tbl_now_to_surveillance()`, `tbl_now_to_EpiNow2()`, `tbl_now_to_epinowcast()`
and `tbl_now_to_data_table()` already did. The pooling is `to_count()`, so case
totals are preserved exactly, and it is reported under `verbose = TRUE`:

```
i `tbl_now_to_baselinenowcast()`: pooled over 1 undeclared column ("sex");
  18195 rows -> 10129.
i Declare it with `add_strata()` to nowcast it separately.
```

Line lists are left alone: one row is already one case there, and collapsing
would destroy the individual records the target package is being handed.

## The non-uniqueness warning now names the culprit

It used to say "Consider using `to_count()` to aggregate the data or `distinct()`
to remove repeated observations". The `distinct()` half is wrong whenever the
cause is an undeclared column -- those rows **are** distinct, they differ in
`sex` -- so it sends you in a circle, and on data with genuine repeats it
silently deletes cases. The warning now inspects the object and says which:

* undeclared columns: names them, and points at `strata =` or `to_count()`,
  adding that the converters pool them for you so this is information rather
  than a fault;
* genuine duplicate rows: says so, and *then* recommends `distinct()`.

## `tbl_now_to_baselinenowcast(max_delay = )`

A cap on the delay axis, counted exactly as `tbl_now_to_epinowcast()` counts it
-- `max_delay = 30` keeps delays `0` to `29`, giving a 30-column triangle -- so
the same number means the same triangle in both. `NULL` (default) keeps every
delay, which is the previous behaviour. This replaces the
`filter(.delay <= 30) |>` idiom the docs used to recommend.

## `nowcast_truth()` is now internal

It was `get_latest_reported_cases()` with the class stripped, undeclared columns
summed away and the count renamed `.observed` -- the values were identical. A
second public name for that is a second thing to learn for no gain.

`score_nowcast()` and `as_scoringutils()` now accept the **`tbl_now` itself** as
`truth` and do the reshaping internally, which is shorter than what it replaces:

```r
score_nowcast(nowcast, truth = dengue)          # was: truth = nowcast_truth(dengue)
```

A data frame of observed counts still works, as does `NULL`.

## `?run_nowcast` says what the models actually are

Three new sections, because "it calls the package with its defaults" is not
enough to read the output:

* **Strata** -- a table of how many each backend can model and how.
  `baselinenowcast`, `surveillance`, `EpiNow2` and `epinowcast` take any number;
  `diseasenowcasting` and `NobBS` take exactly one and **pool with a warning**
  beyond that, because the single array dimension they return cannot be split
  back into two columns.
* **Temporal effects** -- the converters materialise them into columns, but only
  `diseasenowcasting` uses them automatically. `epinowcast` needs them named in
  a module formula; every other backend carries them and ignores them.
* **Censored delays** -- collapsed with a warning by every backend that goes
  through a converter; `diseasenowcasting` receives the flag intact.

And a section on how each engine's default model is specified, with the two
that most need saying out loud:

* **`epinowcast`** defaults to a per-day random effect on the growth rate (a
  random walk in all but name), a single time-constant lognormal reporting
  delay, and no day-of-week report effect.
* **`EpiNow2`** defaults to `delays = delay_opts()`, which is `Fixed(0)` -- **no
  reporting delay at all** -- and `generation_time = gt_opts()`, which is
  `Fixed(1)`. Those defaults describe a process with nothing to nowcast, so
  supply the epidemiology yourself. It also models \eqn{R_t} with a Gaussian
  process rather than a random walk.

## Article fixes

* **`vignette("nowcasting-models")` now cuts at 2021-04-01**, on the rising limb
  of Colombia's third wave, instead of 2023-03-03 where the epidemic had
  subsided and a nowcast had nothing to correct. The line-list engines trim to
  60 days (278,000 rows; NobBS 24s, surveillance 6s measured) rather than 180.
* **The per-package figures were drawing the wrong quantity.** `geom_col()` was
  given `width = 5.5` on a **daily** series, so each bar overlapped its
  neighbours and ggplot2 stacked the overlaps: the grey "reported by now" bars
  showed sums of about six days. The summary figure at the end of the article
  used `width = 0.8` and was correct, which is why the two disagreed. All the
  panels now use `width = 0.8`.
* **The stratified `NobBS` example handed it count rows**, which is the exact
  mistake the article's own warning box forbids two screens earlier -- it counts
  rows, so it was nowcasting counts as cases. It now goes through
  `tbl_now_to_nobbs()` like the unstratified example.
* The stratified `surveillance` example converted the **whole** series (2.3M
  cases) at build time and used `N.tInf.max = 1000` against per-stratum daily
  counts of ~4,000, which silently truncates the posterior. It now trims first
  and uses the same settings as the unstratified fit.
* `vignette("ensemble-nowcasting")` gains an **experimental badge and note** at
  the top, and a figure showing each member's median against the ensemble and
  the eventual truth.

# tbl.now 0.18.0

## New: one call per model, and ensembles

Until now `tbl.now` prepared data for six nowcasting packages and normalised what
they returned, but running several of them still meant six different calls and
six different result shapes to reconcile by hand. This release adds the layer
that removes that bookkeeping.

* **`run_nowcast(x, method)`** fits any supported package and always returns a
  **`tbl_nowcast`**: an S7 object holding the predictions as one row per (event
  date, stratum, quantile level), plus the draws where the backend has them, plus
  the backend's own untouched fit. Backends ship for `"diseasenowcasting"`,
  `"baselinenowcast"`, `"epinowcast"`, `"NobBS"`, `"surveillance"` and
  `"EpiNow2"`, each feeding its package through the matching `tbl_now_to_*()`
  converter rather than building the input by hand.

  It is called `run_nowcast()` and not `nowcast()` because \pkg{diseasenowcasting}
  exports `nowcast()`; keeping the names distinct means both can be attached at
  once.

* **`nowcast_ensemble()`** combines several of them, either by averaging their
  quantiles level by level (`type = "quantile"`, vincentization -- narrower) or
  by pooling their draws into a mixture (`type = "linear_pool"` -- wider, and
  refused outright when a member has no draws, rather than silently dropping it).

* **`nowcast_backtest()`**, **`score_nowcast()`** and **`nowcast_weights()`**
  score models retrospectively and turn those scores into ensemble weights
  (`"inverse_score"`, `"optim"` or `"equal"`). `as_scoringutils()` hands the same
  object to \pkg{scoringutils} for its full score suite.

* **`nowcast_fit()` / `nowcast_tidy()`** are the extension point: two S3 methods,
  in any package, and `run_nowcast()` knows about your model. See
  `vignette("ensemble-nowcasting")`.

* **`autoplot()`** for a `tbl_nowcast` draws a fan chart, in the palette's green
  -- a nowcast estimates the epidemic process, not the reporting one.

## New: `tidy()` for a nowcast and for a backtest

`tidy()` already worked on every raw engine fit. It now also works on what
`run_nowcast()` and `nowcast_ensemble()` return, which is the way round it should
always have been.

* **`tidy()` on a `tbl_nowcast`** returns the package's standard frame --
  `event_date`, `stratum`, `estimate`, `conf.low`, `conf.high`, `level`,
  `engine`, plus `q*` columns for `probs`. `engine` is the method (or the
  ensemble's name); `level` is the width of the **widest symmetric pair of
  quantile levels the object actually carries**, and is `NA`, with `NA` bounds,
  when no symmetric pair exists. A guessed 0.95 there would defeat the one column
  that exists to stop a 90% band being compared with a 95% one.

  `probs` is honoured only when the nowcast carries draws, and errors otherwise:
  a quantile-only nowcast cannot produce a level it was not summarised at.

  Registered in `.onLoad()`, because `tbl_nowcast` is S7 and
  `tidy.tbl.now::tbl_nowcast` is not a writable S3 method name.

* **`tidy()` on a `nowcast_backtest`** gives one row per (method, `now` date,
  target), with the internal dot-prefixed columns traded for ordinary ones.

## New: reproducible backtests

`nowcast_backtest()` gains a **`seed`** argument. When given, the RNG is seeded
immediately before each fit, from the seed and the method and date that fit is
for. One `set.seed()` before the whole backtest only pins anything if every
method draws the same random numbers in the same order -- which stops being true
the moment a method is dropped or one date is refitted. This is the same lesson
`data-raw/nowcast_comparison.R` already records.

`nowcast_weights(type = "optim")` now falls back to equal weights, with a
warning, when the optimiser does not converge on a usable point. It used to
return `NA` weights, which do not fail until much later inside
`nowcast_ensemble()`, as an all-`NA` nowcast that reads like a modelling problem
rather than an optimisation one.

## Removed: the `nowcaster` backend

`nowcaster` was dropped in 0.16.0 along with its converters, for the reasons
recorded there. The `run_nowcast()` backend for it is not shipped: it called
`tbl_now_to_nowcaster()` and `get_nowcaster_strata()`, which no longer exist.
Neither `nowcaster` nor `INLA` is reintroduced to `DESCRIPTION`.

## Other

* `scoringutils` added to `Suggests` (CRAN, so no `Additional_repositories`
  entry is needed). \pkg{diseasenowcasting} is deliberately **not** added: it is
  GitHub-only and sits in no repository `R CMD check --as-cran` can resolve, so
  declaring it would trade an undeclared-import warning for a CRAN-incoming NOTE
  about a dependency that cannot be found. `nowcast_fit.diseasenowcasting()`
  therefore looks its entry point up with `getExportedValue()` after
  `.need_pkg()` has confirmed the package is installed, rather than writing a
  literal `diseasenowcasting::`.
* `LICENSE` / `LICENSE.md` copyright year updated to 2026. The hand-rolled `.wis()` is now cross-checked against it in
  the test suite: two implementations agreeing is worth more than either alone.
* New article, `vignette("ensemble-nowcasting")`, with the fits precomputed by
  `data-raw/ensemble_comparison.R` so the build never fits anything. It reports
  WIS per model and per ensemble across three epidemics, and answers "does the
  ensemble beat its best member?" and "does performance weighting beat equal
  weighting?" from the cached numbers rather than by assertion.
* `vignette("nowcasting-models")` gains a `run_nowcast()` column in its package
  table, and a pointer to the new article.

# tbl.now 0.17.0

## New: \pkg{EpiNow2} support

`tbl_now_to_EpiNow2()` and `tbl_now_from_EpiNow2()`, against **EpiNow2 1.9.0**
(now the minimum in `Suggests`). EpiNow2 takes four different input shapes, one
per entry point, so `target` names the function the result is passed to and it can
be handed over unchanged:

* **`"estimate_infections"`** (default) -- `data.frame(date, confirm)`, the series
  as known at `get_now()`. Also what `epinow()` takes.
* **`"regional_epinow"`** -- the same plus a `region` column built from the
  object's strata (`" | "`-joined for several, matching the `triangle_list`
  convention). The other targets pool strata with a warning.
* **`"estimate_truncation"`** -- a `tbl_now_epinow2_snapshots` list, one
  `date`/`confirm` snapshot per report date. This is the one EpiNow2 model that
  uses the report dimension a `tbl_now` exists to carry.
* **`"estimate_dist"`** -- the interval-censored frame `EpiNow2::estimate_dist()`
  fits a delay distribution to. **New in EpiNow2 1.9.0**, and it documents the
  \pkg{epidist} schema exactly, so it shares `.delay_censoring_windows()` with
  `tbl_now_to_epidist()` rather than growing a second copy.

Three things worth knowing:

* **EpiNow2 models a daily process and has no `timestep`.** As of 1.9.0 there is
  no `timestep`, `interval` or `period` argument on any entry point (all four
  formals checked), so a weekly series passed as one row per week is read as one
  row per **day** -- no error, just an epidemic seven times too fast. The
  converter lays it on the daily grid with EpiNow2's own `accumulate` column
  instead -- built by [EpiNow2::fill_missing()] rather than by hand, because a
  hand-rolled version put each period's count on the period's *last* day where
  `fill_missing()` leaves it on the date given, shifting every weekly fit six days
  with no error. Units coarser than a week, and the `"numeric"` grid, are refused
  by name rather than approximated.

  `initial_accumulate` is passed explicitly rather than inferred: with `by`,
  EpiNow2 1.9.0's inference drops each group's first observation (a two-region
  weekly series of 336/167 cases came back as 295/147). Single-series inference is
  unaffected.
* **The snapshot form has a real inverse.** Snapshot *k* is the series as known at
  report date *k*, so differencing consecutive snapshots recovers
  `count-incidence` exactly. `tbl_now_epinow2_snapshots` carries the report dates
  so `as_tbl_now()` can do it; a bare list needs `report_dates`. Verified against
  `EpiNow2::example_truncated`, which round-trips to the case for the case.
  (The commented-out draft of this converter asserted no inverse was possible.
  For a single series that is true; for snapshots it is not.)
* **`estimate_secondary()` and `estimate_delay()` get no target.** The first models
  two data streams against each other and one `tbl_now` is one stream; the second
  is superseded by `estimate_dist()` by EpiNow2's own help and throws away the
  censoring a `tbl_now` carries.

**`obs_date` and the censoring windows are different quantities**, and the
converter now treats them as such. `[sdate_lwr, sdate_upr)` brackets *when the
report happened* -- at weekly resolution `[W, W + 7)`, a half-open interval whose
upper bound is the end of that week, not a claim that anything happened on day
`W + 7`. `obs_date` is *when observation stopped*, which `estimate_dist()` asserts
is `>= sdate_upr` on every row. A `tbl_now`'s `now` **labels a period**, so the
instant observation stopped is the end of it: `obs_date = now + w`. That makes the
assertion hold by construction, and nothing is observed after it. Clamping the
windows at `now` instead was tried and rejected -- it moves reports in the final
period into an earlier one, which the epidist round-trip test caught.

The `nowcasting-models` article now covers \pkg{EpiNow2} across all three strata,
with its results precomputed into `nowcast-comparison.rds` like every other
engine. Two caveats are stated in the article itself: the delay distributions are
\pkg{EpiNow2}'s shipped examples rather than distributions fitted to the
Colombian data, and sampling is lighter than the default (500 draws, 250 warmup,
2 chains) because it is much the slowest engine in the comparison.

`data-raw/nowcast_comparison.R` now takes engine names
(`Rscript data-raw/nowcast_comparison.R EpiNow2`) and merges them into the
existing file, leaving every other engine's rows and recorded timings alone; with
no arguments it rebuilds everything as before. This replaces a second script that
re-created the setup by parsing the first one.

Two correctness fixes came out of that. Every engine is now seeded per
`(engine, stratum)` immediately before its fit, rather than relying on a single
`set.seed()` at the top of the script -- which only pins results if every engine
consumes the same random numbers in the same order, and so does not survive
refitting a subset. Refitting `baselinenowcast` alone had been silently changing
its estimates, and one EpiNow2 fit produced a stratum whose upper credible bound
sat at `1e8` for all 181 days and would not reproduce. Both now refit to
`max abs diff == 0`. The script also refuses to cache any fit whose scale exceeds
100x the observed maximum for its stratum, since an unconverged Stan or INLA fit
returns numbers rather than an error.

`tidy()` gained methods for `estimate_infections`, `epinow`, `estimate_truncation`
and `estimate_dist`, plus a `regional_epinow` branch in `tidy.list()` giving one
block per region.

`tidy.estimate_dist()` reports the fitted distribution's **`mean` and `sd`**
alongside its parameters, so its output is directly comparable with
`tidy.epidist_fit()`. They are derived from the **distribution**, not from the
family's algebra: each draw's parameters go back into the fit's own `dist_spec`
and through [EpiNow2::discretise()], which knows the families, and the moments
follow by summation over the PMF. Nothing in this package names a distribution, so
a family \pkg{EpiNow2} adds later works as soon as `discretise()` supports it.
Against the closed forms the mean is exact and the sd runs about 1% high -- the
variance a discrete grid adds -- so expect a difference of that order against
\pkg{epidist}, which reports continuous-distribution moments.

It also honours `probs` and takes a `level` argument, matching
`tidy.epidist_fit()`. (An earlier draft rejected `probs` with a message claiming
the engine keeps no draws. It does: `summary.estimate_dist()` reads them.)

`tbl_now_to_EpiNow2(target = "estimate_dist")` warns when it pools strata --
`estimate_dist()` has no grouping argument, so it fits one distribution to
everything -- and warns when a large share of delays are exactly zero, since a
lognormal has zero density there and will inflate its variance rather than fail.
The message points at the families that do have positive density at zero
(`"exp"`, or `"gamma"`/`"weibull"` with shape below 1) rather than at a constant
shift, which would silently bias every parameter.

Two more points of care:

* `level` is read off the `lower_<pct>`/`upper_<pct>` column names, because
  EpiNow2's `CrIs` is a user argument -- a fit made with `CrIs = c(0.5, 0.95)` has
  no `lower_90` at all, and hard-coding `0.90` would report a width the fit never
  produced.
* `tidy.estimate_dist()` returns the **delay** schema (`term`, `estimate`, ...),
  not the nowcast one -- the second instance of the documented exception alongside
  `tidy.epidist_fit()`. Note that `summary()`'s `mean`/`sd` *columns* are the
  posterior mean and sd of each **parameter**, while the `mean`/`sd` *rows* this
  method reports are the **delay distribution's** moments. Same words, different
  quantities.

`.epidist_drop_unusable_counts()` is now `.drop_unusable_counts()` and shared:
`EpiNow2::estimate_dist()` asserts `n >= 1` with the identical message epidist
uses, so the same filter applies to both.


## Audit of the converters and `tidy()` against the target packages' own docs

Every claim the converters and `tidy()` methods make about `diseasenowcasting`,
`baselinenowcast`, `epinowcast`, `epidist`, `NobBS`, `surveillance`, `tsibble`
and `data.table` was re-checked against those packages' installed help pages and
source. Five defects came out of it, all of them cases where the code was
silently *plausible* rather than wrong-looking.

* **`tidy()` no longer pools strata under `"all"`.** `tidy.nowcast()` documents
  `stratum` as `"all"` *when the fit is unstratified*, so `(stratum, event_date)`
  is meant to be a unique key. Two methods broke that:

  * **`tidy.epinowcast()`** read `summary(fit, type = "nowcast")` and ignored
    both `.group` and the `by` columns sitting beside it. A real two-group fit
    (`by = "age_group"` on `germany_covid19_hosp`, age groups `00+` and `80+`)
    came back as 20 rows all labelled `"all"`, with every one of its 10 reference
    dates duplicated. It now emits one block per `by` group, and several
    grouping columns are pasted `" | "`-separated, matching
    `tbl_now_to_baselinenowcast(format = "triangle_list")`.
  * **`tidy.list()`** recognised a `NobBS::NobBS.strat()` fit -- it has the
    `estimates`/`onset_date` shape the detector looks for -- but ignored the
    `stratum` column that `NobBS.strat()` puts there. A two-stratum fit on
    `denguedat` returned 44 rows labelled `"all"`, 22 of them duplicate keys. It
    now reads `stratum` when present.

  The `probs` path for `epinowcast` was mispaired in the same way: it split the
  posterior samples on `reference_date` alone, so on a stratified fit each date's
  quantiles went to whichever stratum `split()` sorted first. The split is now
  keyed on `(stratum, reference_date)` and indexed by the summary's own rows.

* **`tidy()` no longer invents an interval for a `baselinenowcast` point fit.**
  `baselinenowcast(output_type = "point")` returns one value per reference date
  and stamps `output_type = "point"` on the result. `tidy()` ignored that column
  and took the 2.5%/97.5% quantiles of a single number, reporting
  `conf.low == conf.high == estimate` with `level = 0.95` -- a zero-width 95%
  band. It now returns `NA` bounds and `NA` `level`, and refuses `probs` rather
  than returning the point estimate under a quantile's name.

* **`tbl_now_to_nobbs()` prints the `units` string NobBS accepts.** Its verbose
  summary printed the object's own `"weeks"`, but `NobBS::NobBS()` documents
  `units` as `"1 day"` or `"1 week"`; pasting `"weeks"` into the call produces
  `-Inf`/`Inf` warnings from `seq()` and then an opaque `replacement has 1 row,
  data has 0`. It now prints `"1 week"`, and aborts up front for any grid NobBS
  cannot model.

* **The line-list back-ends no longer fabricate 1970 dates from a `numeric`
  grid.** `tbl_now_to_nobbs()` and `tbl_now_to_surveillance()` both coerced the
  event and report columns with `as.Date()`. On a `numeric`-unit `tbl_now` those
  columns are integer indices, so index 1 became 1970-01-02 and the conversion
  succeeded, silently, with a line list of invented dates. Both now abort naming
  the units, as `tbl_now_to_baselinenowcast()` and `tbl_now_to_epinowcast()`
  already did. `tbl_now_to_surveillance()` also gained `"years"` ->
  `"1 year"`; it previously fell through to `"1 week"`.

The remaining findings were addressed too:

* **A negative delay now warns instead of silently losing cases.** A reporting
  triangle is indexed by delay from 0, so a report that arrived *before* its
  event has no cell: 10 cases in gave a triangle summing to 9, with the affected
  cell reading `0` -- an *observed* zero -- rather than `NA`. Both triangle
  formats and `tbl_now_to_epinowcast()` now warn, naming how many rows and cases
  go and how to filter them yourself. `format = "long"` has no delay axis, keeps
  them, and stays quiet.

* **`tbl_now_to_epidist()` accepts `count-cumulative` data.** epidist asserts
  `n >= 1`, and de-accumulating a cumulative series produces a `0` wherever a
  report added nothing and a negative on any downward revision -- so the
  conversion died on epidist's own `Assertion on 'data$n' failed` for
  essentially any real cumulative input, and for plain incidence data that had
  been through `complete_zeroes()`. Rows carrying no case are now dropped before
  the epidist object is built: a zero contributes nothing to a delay
  distribution, so that is lossless and only reported under `verbose = TRUE`; a
  negative discards a revision, so it **warns**; and if nothing usable is left
  the converter aborts saying why. `flusight` -- the one `error` cell in the
  article's converter matrix -- now converts.

* **`tidy()` handles a per-stratum list of `baselinenowcast` fits.**
  `?tbl_now_triangle_list` recommends
  `lapply(triangles, baselinenowcast::baselinenowcast)`, and `tidy()` on the
  result used to error and suggest `engine = "NobBS"`. A list whose elements are
  all `baselinenowcast_df` is now recognised: each is tidied and labelled with
  its list name (or its position, when the list is unnamed), giving the same
  one-block-per-stratum table the natively stratified engines return. `probs`
  passes through.

* **`DEVELOPMENT_SKILL.md` section 2 corrected.** It claimed
  `tbl_now_to_surveillance()` sets `control$dRange`. It does not, and its own
  help page says so: `now` and the delay unit are deliberately left to the
  caller, because the converter cannot know which window you mean to fit.

## Behaviour changes

* **`tidy()` reports `level = NA` for a \pkg{NobBS} fit** instead of `0.95`.
  `NobBS()`'s `lower`/`upper` come from `specs$conf`, and its return value is
  `list(estimates, estimates.inflated, nowcast.post.samps, params.post)` -- no
  `specs`, so the width is genuinely unrecoverable from the fit. A guessed
  default is worse than `NA` in the one column that exists to stop widths being
  compared blindly. Pass `tidy(fit, level = 0.95)` to fill it in. The assertion
  in `test-tidy.R` that recorded the old behaviour was updated.

* **`tidy.epidist_fit()` warns on a delay model with covariates.**
  `epidist::predict_delay_parameters()` returns one row per draw *and*
  observation, and the reported quantiles pool over both. For `mu ~ 1` every
  observation shares the draw's value, so that is exactly the posterior
  interval; with covariates in the delay model the interval is a *mixture across
  covariate levels*, which the docs described simply as "Posterior median". The
  method now detects a parameter that varies within a single draw and says so,
  pointing at `newdata` for a specific covariate combination. The numbers are
  unchanged -- only the silence is.

## Tests

* New `test-tidy-strata.R`: stratified `tidy()` for `epinowcast` and
  `NobBS.strat()`, quantile-to-stratum alignment, the point-fit interval, the
  per-stratum list of `baselinenowcast_df` fits, and the `level` argument. The
  fits are mocked from the shape of real ones, so the file needs neither cmdstan
  nor JAGS.
* New `test-converter-grids.R`: the `numeric` grid across every converter,
  zero / negative / very long delays, gaps in the event grid, a trailing event
  period with no reports under each `complete` setting, the negative-delay
  warning, and epidist's `n >= 1` filtering (including `flusight`).
* New `test-converter-strata-shapes.R`: several stratifying columns, a factor
  level with no rows, and label-to-value pairing when the data order is not
  alphabetical.
* `test-converter-censoring.R` now covers `tbl_now_to_nobbs()`, which the
  converter loop skipped because its package name is not its suffix.


* **Removed \pkg{nowcaster} support**: `tbl_now_to_nowcaster()`,
  `get_nowcaster_strata()`, the `tidy()` branch for its fits, and its sections in
  the articles are all gone. The converter worked, but the package around it
  demanded enough special-casing that keeping it cost more than it returned:

  * **`Dmax` and `wdw` are counted in weeks, whatever grid you hand it.** On a
    daily series, values chosen as days are silently read as weeks: `Dmax = 30`,
    `wdw = 120` asked for a 30-week horizon over a 2.3-year window, which ran for
    **45 minutes** and had INLA reporting the fit diverging. The same fit with
    week-scaled values took **24 seconds**.
  * **It returns weekly estimates from daily data**, so its numbers are weekly
    *totals* while every other engine reports daily counts -- roughly 6x larger
    on the same axis, and not comparable without re-gridding. Its label is the
    week *start*.
  * **`age_col` must be numeric** even though the help calls it a stratum
    column: a character column errors inside `cut()`, and a character `bins_age`
    trips an `if (bins_age == "SI-PNI")` comparison against a vector. The
    converter existed largely to encode strata into codes and hand back the
    matching breaks.
  * **Results come back as those codes, not labels**, so a tidied stratified fit
    reported `stratum` values of `"1"` and `"2"` rather than the levels.
  * **It takes its maximum observable time from the last event date, not the
    last report**, so cutting the series anywhere except where
    `max(onset) == max(report)` made it NA-mask genuinely observed cells and
    nowcast *below* what had already been reported.
  * It needs **R-INLA**, and was itself installable only from GitHub.

  That last point has a side benefit: \pkg{nowcaster} was the only entry in
  `Remotes:`, so removing it drops that field entirely -- and with it the reason
  the package could not be submitted to CRAN as-is.
* **New `tbl_now_to_nobbs()`**, filling a real gap. \pkg{NobBS} counts *rows*,
  so handing it `count-incidence` data was silently wrong: a table of 1,174 rows
  carrying 50,160 cases was nowcast as 1,174 cases, with no error. The converter
  expands counts to one row per case first. The articles previously recommended
  `as.data.frame()`, which is correct only for a line list.

* **Fixed the pkgdown build on CI.** The shared "Learning more" fragment was
  pulled in with a relative child path (`../../man/fragments/...`). \pkg{rmarkdown}
  renders into an intermediates directory under `tempdir()` and copies relative
  resources alongside it, so a path containing `../..` escapes that directory --
  harmless where `tempdir()` is deep, fatal on CI where it sits two levels from
  the filesystem root (`cannot create file '/tmp/RtmpXXXX/../../man/...'`). The
  fragment moved to `inst/fragments/` and every caller now locates it with
  `system.file()`, which is path-independent and also works under
  `pkgload::load_all()`.
* **Dependency fixes for CI.** `almanac` is used by the package but was declared
  nowhere -- the `Remotes:` entry for it was inert, since `Remotes` only says
  *where* to fetch an already-declared dependency. It is now in `Suggests`, with
  its r-universe added to `Additional_repositories` (it was archived from CRAN).
  `nowcaster` was declared but unobtainable from any configured repository; it
  briefly gained a `Remotes:` entry, and was then dropped altogether (above).

* **`tidy()` on a \pkg{diseasenowcasting} fit now works directly.** From
  \pkg{diseasenowcasting} 2.1.0 that package re-exports the shared `generics`
  generic and ships its own method, so `tidy(fit)` returns the standard nowcast
  table. `tbl.now` now registers its own method for
  `diseasenowcasting::nowcast_prediction` **only when the package does not supply
  one**, so older versions keep working and newer ones are not overridden. The
  article calls plain `tidy(dnc_fit)` again.

* **Article fixes so the code on the page reproduces the output shown.** Three
  places displayed results the printed code could not produce: the
  \pkg{diseasenowcasting} section tidied the *fit* rather than `predict(fit)`,
  and the \pkg{baselinenowcast} section hid the
  trailing-row trim that keeps the final week from exploding. All three now
  match the precompute.
* **Documented two `tidy()` masking hazards.** `library(diseasenowcasting)`
  attaches its own `tidy()` generic, and `library(broom)` overwrites
  `tbl.now`'s `tidy.list()` method (which \pkg{NobBS} fits dispatch on). Neither errors; both silently return a different table.
  `tbl.now::tidy()` disambiguates.

* **New `tidy()` method for \pkg{epidist} fits.** \pkg{epidist} is the one
  supported package that does not nowcast -- it estimates the reporting-delay
  distribution -- so `tidy.epidist_fit()` returns a *delay-shaped* table (`term`,
  `estimate`, `conf.low`, `conf.high`, `level`, `engine`) with one row per
  distribution parameter, rather than forcing a delay fit into the per-event-date
  nowcast schema. `probs` works, because the fit keeps its draws. Note that
  `epidist()` returns `c("brmsfit", "epidist_fit")` in that order, so a loaded
  \pkg{broom.mixed} wins dispatch; call `tidy.epidist_fit()` explicitly if that
  matters.
* **`tidy()` now returns \pkg{surveillance}'s credible interval**, which it
  previously discarded. `surveillance::nowcast()` stores a prediction interval
  in the returned object's `pi` slot at the width `control$alpha` names (95% by
  default), but the method hard-coded `conf.low`, `conf.high` and `level` to
  `NA`, so surveillance was the one engine that appeared to report no
  uncertainty. Reaching for the JAGS-backed `bayes.trunc`/`bayes.trunc.ddcp`
  methods was never needed to get an interval.
* **Censored delays no longer break the converters.** A censoring indicator that
  is a property of the *case* rather than of the delay -- an administrative
  "this date is only an upper bound" mark, say -- splits one
  `(event_date, report_date)` cell into a censored and an uncensored row. A
  reporting triangle has one slot per cell, so `tbl_now_to_baselinenowcast()`
  and `tbl_now_to_epinowcast()` aborted on duplicate cells, and the converters
  that expand back to a line list picked the flag up as an unrequested
  stratifier. The censoring dimension is now collapsed before the conversion,
  and each route warns:
  * **count data**: counts are summed over the flag, so case totals are
    unchanged;
  * **line lists**: the column is dropped, leaving one row per case.

  `tbl_now_to_epidist()` is deliberately exempt: estimating a delay
  distribution is the one job that can use the flag.

* `tbl_now_to_baselinenowcast()` now handles a **line list on its own**. It
  already aggregated to incidence; it now also completes the zero periods out to
  the `now` (new `complete = TRUE` argument). A reporting triangle is a
  rectangular grid, and an event period with no reports has no rows, so the
  triangle used to stop short unless you remembered
  `to_count() |> complete_zeroes()` first. Linelist and count-incidence input now
  produce an **identical** triangle.
* `tbl_now_to_baselinenowcast()` also accepts **`count-cumulative`** data, which
  it used to refuse. De-accumulating produces negative increments wherever a
  total was revised downward, and \pkg{baselinenowcast} ships
  `preprocess_negative_values()` for exactly that; the converter applies it and
  warns. `negatives = "error"` restores the old refusal.
* **Bug fix: `tidy()` ignored the strata of a `diseasenowcasting` fit.** A
  stratified fit reports `strata_draws` (draws x event times x stratum), but the
  method read only the pooled `draws`, so every row came back with
  `stratum = "all"` even when the fit itself said "2 strata". It now returns one
  block per stratum, matching what the other engines do.
* Two new test files worth naming, because they exist to stop silent
  regressions:
  * `test-converter-equivalence.R` -- every converter accepts line-list input,
    and the triangle/preprocessing targets give the *same* result from a line
    list as from the equivalent count-incidence object.
  * `test-converter-datasets.R` -- every converter against every dataset the
    package ships. This is the testthat counterpart of the article's matrix: the
    article documents, this one fails.
* Website: fixed a regression that drew an empty scrollbar track ("a rectangle")
  under every code chunk. The no-wrap rules have to apply to `code` as well as
  its container, but `overflow-x` must apply ONLY to the container -- setting it
  on the inner `<code>` too made each one a second scroll context that reserved
  a gutter. Figure captions are now centred, smaller and grey.

* The nowcasting-models article now **shows the real output of every fit and
  every `tidy()` call**. The fits are far too slow to run on each build, so
  `data-raw/nowcast_comparison.R` captures what each one prints and what
  `tidy()` returns for it, and the article replays that. Each section pairs a
  copy-pasteable `tidy(fit)` (shown, not run) with a hidden `head(5)` whose
  output appears beneath it, so nothing in the visible code has to be trimmed for
  display. The ad-hoc result extraction each section used to do -- pulling
  `$estimates` out of NobBS, building a `data.frame()` from `epoch()` and
  `upperbound()` for surveillance -- is gone; every section now uses `tidy()`.
* New article section running **every converter, plus a nowcast, against every
  dataset the package ships** (`data-raw/converter_matrix.R`), recording which
  combinations work and explaining the ones that do not: `count-cumulative`
  cannot become a reporting triangle without inventing negative increments,
  `epidist` has no individual delays to censor in a cumulative series, and a
  `tsibble` needs a unique index/key. Each attempt is time-limited so the matrix
  is reproducible.
* The article states plainly that each modelling package is a **separate
  install** that `tbl.now` does not pull in, with the commands for the ones that
  are not on CRAN and the note that Stan, JAGS and R-INLA are software outside R.
* `SKILL.md` documents `tidy()`, the `surveillance` converter,
  `format = "triangle_list"`, the new `complete_zeroes()` behaviour, and the
  zero-period pitfalls.

* Every package section in that article now shows how to recover its predictions
  with `tidy()`.
* The comparison precompute falls back gracefully when \pkg{baselinenowcast}
  rejects a triangle whose most recent reference times are all zero. A thin
  stratum can hit that after the zero weeks are completed out to `now` -- here
  the female series has no case in the final week even though the pooled series
  does -- and the fallback completes only as far as the last week holding a case,
  costing that stratum one week rather than the whole nowcast.

* New **`tidy()`** methods, one shape of answer whatever engine produced the fit.
  The converters normalise what goes *into* a nowcasting package; `tidy()`
  normalises what comes back out. It returns `event_date`, `stratum`,
  `estimate`, `conf.low`, `conf.high`, `level` and `engine` for fits from
  \pkg{diseasenowcasting}, \pkg{baselinenowcast}, \pkg{epinowcast},
  \pkg{NobBS} and \pkg{surveillance}.
  * `probs` adds one column per requested quantile, named after the probability
    (`q5`, `q50`, `q2.5`, ...). Only the engines that keep draws
    (\pkg{diseasenowcasting}, \pkg{baselinenowcast}, \pkg{epinowcast}) can
    honour it; the others error rather than return an approximation dressed up
    as a quantile.
  * `level` records the width each engine's interval **actually** has --
    \pkg{epinowcast} reports a 90% band by default while the others report 95%,
    and without it the two get compared as if they were the same.
  * \pkg{NobBS} returns an *unclassed* list, so it is told
    apart by structure, with an `engine` argument to override.
  * `tidy()` deliberately does **not** re-grid: packages that bin onto their own
    week starts keep them, because snapping would hide a real difference.
  * The generic comes from \pkg{generics} (a new, dependency-free `Imports`), so
    it composes with \pkg{broom} rather than masking it.
* The nowcasting-models article now cuts at the **second week of July 2002**
  rather than the turn of the year: a December cut lands on the holiday reporting
  slump, which says more about December than about the models. Section headings
  are now just the package name, each package's figure carries a caption instead
  of a heading, each gains a *Simple nowcast* heading, and packages needing an
  external backend (Stan, JAGS, R-INLA) carry a coloured requirement callout. The
  overview table gained an *Additional requirements* column.
* Website: `.alert-warning` callouts now use the attenuated red the plots use for
  intervals, and code blocks are pinned to scroll sideways rather than wrap --
  pkgdown's `white-space: pre-wrap` on `code` inside `pre` was overriding the
  bare `pre` rule and folding long lines onto a second line.

* Print methods now write to **stdout** instead of emitting messages.
  `print.batch_test()`, `print.transport_discriminant()`,
  `print.tbl_now_triangle_list()` and the `temporal_effects` print method used
  the `cli_*()` family, whose output is a *message* -- so it vanished under
  `message = FALSE`, `sink()` or `capture.output()`, which is exactly where a
  print method is expected to work. They now use cli's `cat_*()` family. The
  matching tests were switched from `cli::cli_fmt()` to `capture.output()`.
* The `epinowcast` section filtered on a hard-coded `2008-12-20`, left over from
  the old window; against the new data that matched **zero rows** and aborted the
  build. It now trims relative to the series, using
  `tbl_now_to_epinowcast(preprocess = FALSE)` followed by
  `enw_filter_reference_dates()` and `enw_preprocess_data()`.


* `tbl_now_to_baselinenowcast()` gained `format = "triangle_list"`: one reporting
  triangle **per stratum**, instead of pooling them into a single matrix. Unlike
  splitting the long format by hand it takes the delay unit and the strata off
  the object, so neither has to be restated. With no strata attached the result
  is still a list — of length one, named `"all"` — so the return type never
  depends on whether strata happen to be present.
* The result is a thin `tbl_now_triangle_list` class: still an ordinary list, so
  `lapply()` and `[[` work as before, but with a `print()` method. The class
  earns its place as a guard: \pkg{baselinenowcast}'s
  `estimate_and_apply_delays()` also takes a list of triangles, but *retrospective
  snapshots of one series* rather than one per stratum, and would silently accept
  a per-stratum list and treat the strata as points in time.
* `as_tbl_now()` gained a method for `tbl_now_triangle_list`, rebuilding a
  `count-incidence` `tbl_now` with the strata recoded onto their column. The
  strata **values** are stored on the object rather than parsed back out of the
  element names, so a stratum containing the name separator still round-trips.
* **Bug fix: `as_tbl_now()` aborted on any weekly reporting triangle.**
  `tbl_now_from_baselinenowcast()` ignored the triangle's own `delays_unit`
  attribute and read the delay columns as *days*, so a weekly triangle produced
  daily report dates against weekly event dates and unit inference contradicted
  itself ("report_units must be coarser than or equal to event_units"). Both
  directions now default `delays_unit = NULL` and resolve it from the attribute;
  an explicit value still wins.

* **Bug fix in `complete_zeroes()`: it was silently deleting real cases.** The
  closing "don't look into the future" filter compared with `<` rather than
  `<=`, so every row reported on the final report date was dropped — in the
  function's own documented example, 5 of 55 cases vanished. A function whose
  job is to *add* zeroes was removing data at the boundary.
* `complete_zeroes()` now completes out to the object's `now`, not merely to the
  last event date present in the data, and gained an `until` argument to
  complete to a specific date instead. An event date with no reports at all does
  not appear in the data, so the old behaviour left a gap exactly at the `now`
  edge — where nowcasting matters. A supplied `until` never truncates below the
  data. The line-list error message now explains why a line list cannot hold a
  zero week and points at `to_count()`.
* `plot_reporting_hexamap()`'s `max_cells` is now a real bound. It previously
  took the delay at position `max_cells` and kept every cell sharing that delay,
  so a wide band at the cut overshot the documented cap.
* The nowcasting-models comparison now runs to the `now` for every engine.
  `baselinenowcast` gets there via `complete_zeroes()`; `surveillance` cannot (a
  zero-count row expands to zero line-list rows, so padding evaporates) and is
  instead given its grid directly through `control$dRange`. The article explains
  both, including that forcing `surveillance` to estimate a period with no
  observations is unstable on stratified data.

* The nowcasting-models article now builds its `tbl_now` from the **whole**
  `denguedat` series (52,987 cases over 1,091 weeks) instead of a pre-filtered
  two-year window. Every converter runs on the full object in a few seconds;
  where a package needs a shorter series to fit, the article now uses **that
  package's own argument** rather than subsetting the data first — `moving_window`
  in `NobBS` (which is what takes the full-series fit from impractical to about
  six seconds) and `when` in `surveillance`.
  `diseasenowcasting` (~12 s) and `baselinenowcast` (~10 s) take all 1,091 weeks
  as they are. `epinowcast` is the one engine with no such argument, since the
  reporting triangle is already built by the time you hold a preprocessed object;
  the article shows `tbl_now_to_epinowcast(preprocess = FALSE)` followed by
  `enw_filter_reference_dates()` and `enw_preprocess_data()`, and prints the full
  and trimmed objects side by side. Each section states which it is doing.
* Website: `.alert-info` callouts (pandoc `::: {.alert .alert-info}` fenced divs)
  are restyled from the Bootstrap default blue into the package's sage green,
  with a darker green left rule and heading colour.
* The nowcasting-models article's `baselinenowcast` fit referred to a
  `dengue_triangle2` object that no longer existed; it now uses
  `dengue_triangle`.

* The example article was rewritten from scratch around the new
  `hai_bucaramanga` dataset and is now a full **end-to-end tutorial**: cleaning a
  messy surveillance extract with `dplyr` + `tbl.now` (duplicate records, missing
  dates, and reports dated before the event they describe), reading the data with
  `autoplot()` and the standalone `plot_*()` diagnostics, testing the reporting
  delay for drift and change points, attaching only the temporal effects the data
  justifies, and finally nowcasting with `diseasenowcasting` and five other
  engines. Each modelling choice at the end is traced back to a diagnostic at the
  beginning. It moved from `vignettes/` to `vignettes/articles/` (the pkgdown
  URL is unchanged) because `diseasenowcasting` is not on CRAN and so cannot be
  fitted while building a shipped vignette.

* New converters for further back-ends:
  * `tbl_now_to_surveillance()` builds the individual-level line list
    [surveillance::nowcast()] works from, renaming the event and report dates to
    \pkg{surveillance}'s own `dHospital` / `dReport` defaults. `format = "sts"`
    instead returns the observed curve as a `surveillance` `sts` object.

  It accepts count data as well as line lists, expanding counts back to one row
  per case (de-accumulating first when the data is cumulative).
  \pkg{surveillance} is a new `Suggests`.
* The nowcasting-models article gained a section for it and a closing
  **comparison of every engine on one set of axes** — one plot for the
  unstratified object and one faceted by stratum, with a colour per package, the
  incomplete data each engine actually saw, and the counts those weeks
  eventually reached. The comparison deliberately uses an earlier 2002-2003
  window rather than the article's main `dengue_now`, because the latter runs to
  the end of `denguedat` and so has no ground truth to check against. The fits
  are precomputed by `data-raw/nowcast_comparison.R` and read from a saved file,
  so editing the prose no longer re-runs Stan, JAGS and INLA.

* `flusight` no longer ships duplicate rows (#25). The upstream FluSight
  `time-series.csv` contains 39,139 exact duplicates, which forced every example
  to open with a `distinct()` call; the dataset now goes from 491,706 to 452,567
  rows. The removal is lossless — every repeated
  (`as_of`, `target_end_date`, `location_name`) key carried an identical
  `observation`, with no conflicting values — so that triple is now a unique key.
  The help page documents the change, and the FluSight example vignette drops the
  de-duplication step.

* New dataset **`hai_bucaramanga`**: 1,423 healthcare-associated infections
  (IAAS) notified in Bucaramanga, Colombia, 2016-2023, from the Colombian open
  data portal. Column names and categorical values are translated from Spanish.
  It is a deliberately *unpolished* extract and its help page documents the
  defects in detail — a `1900-01-01` missing-date sentinel, 88 negative
  reporting delays, 100 exact duplicate records, and a strongly bimodal delay
  (3-day median, 92-day 90th percentile) — which makes it a realistic exercise
  for the delay diagnostics rather than a clean modelling example.
* `test_delay_drift()` and `test_delay_changepoint()` now document **every
  column of their output**, plus new *Interpreting the result* sections. The
  `test_delay_drift()` help gained a *Choosing a method* section explaining why
  `"hamed-rao"` is the default (deterministic, no AR(1) assumption, effectively
  instant) and when to cross-check with `"block-bootstrap"`, which is robust to
  weekly periodicity but stochastic and thousands of times slower.
* The Get Started vignette now opens the nowcasting problem with a **figure**
  showing observed-to-date cases, the reports still in transit, and the nowcast
  of the eventual total.
* The "Learning more" links live in a single `man/fragments/learning-more.Rmd`
  and are included in the README and at the end of every vignette and article,
  so they only have to be edited in one place.
* Website: the "Articles" navbar dropdown was rendering near-black with grey
  text because the styling targeted `.submenu`, which Bootstrap 5 does not use;
  it now targets `.dropdown-menu` and matches the pale red of the package
  plots. `pkgdown/extra.css` is also no longer listed under
  `includes: in_header:`, which was pasting raw CSS into `<head>` where it was
  ignored.
* README code blocks no longer wrap mid-tibble: printed output was being split
  into stacked column blocks by R itself, which no stylesheet could undo.
  
# tbl.now 0.15.0

* `autoplot()` panels are now consistently **colour-coded by process**: red for
  everything reporting-related (the delay distribution, the delay calendar/holiday
  effects, the delay periodogram) and green for the epidemic (event-date) process
  (the observed cases and their calendar/holiday effects). This matches the colours
  the standalone diagnostic plots (`plot_reporting_process()` /
  `plot_epidemic_process()`, `plot_scalogram()`, ...) already used, so a panel and
  its standalone twin read the same.
* Every `autoplot()` panel now says **which process it describes** in its subtitle
  — either "Reporting delay process" or "Epidemic (event-date) process" — replacing
  the per-panel explanatory subtitles. A single panel therefore reads on its own.
* The two periodogram panels are renamed **"Cycles (periodogram)"** (previously
  "Seasonality" / "Delay periodicity").
* Every `autoplot()` panel now has a standalone `plot_*()` twin that draws just
  that panel (identical data, colours and subtitle): `plot_day_of_week_effects()`,
  `plot_week_of_year_effects()`, `plot_month_of_year_effects()`,
  `plot_holiday_effects()`, `plot_holiday_lag_effects()` (each taking
  `type = "epidemic"` or `type = "report"`), plus `plot_cycles()`,
  `plot_delay_distribution()` and `plot_observed_cases()`. Use `autoplot()` for the
  grid and a `plot_*()` for one effect on its own.
* The day-of-week, week-of-year, month-of-year, holiday and weekend/holiday-lag
  panels gained a `measure` argument (in `autoplot()` and every `plot_*()` twin).
  `measure = "normalized"` (the default) is the existing view — each value divided
  by its overall mean, 1 = average. `measure = "percent"` instead shows the
  **share of cases** falling in each group with its IQR (e.g. "10% of cases at the
  weekend versus 90% on weekdays"); the reporting version shares out the reports by
  report date. Percentages need `Date` event/report columns.
* Vignettes: the Get Started guide documents the `plot_*()` twins and the
  `measure` argument, and marks the "Holiday effects", "Do delay distributions
  drift over time?" and "Detecting batch reporting" sections as AI-written, pointing
  readers to the human-written batch-reporting article. The FluSight example
  analysis is flagged as a work in progress.

# tbl.now 0.14.1

* Strata are now carried into the model converters that can use them.
  `tbl_now_to_epidist()` keeps the strata as data columns (usable as covariates in
  an epidist formula), and `tbl_now_to_baselinenowcast(format = "long")` keeps them
  so you can build one reporting triangle per stratum. A single reporting-triangle
  **matrix** has no strata dimension, so `format = "matrix"` now **pools** the
  strata with a warning instead of erroring on duplicate cells.
  `tbl_now_to_epinowcast()` already passed strata as its grouping (`by`).
* The nowcasting-models article was restructured: each package is now shown
  **bare** (from `dengue_now`) and then **enriched** — one `dengue_seasonal` object
  carrying a stratum and temporal effects flows through every converter — so the
  separate "Carrying delay effects into each model" section is gone. It adds a
  worked **per-stratum** `baselinenowcast` loop (one triangle per stratum). The
  \pkg{baselinenowcast} workflow also had a bug: it used the plural
  `estimate_and_apply_delays()` (which expects a *list* of retrospective triangles)
  on a single triangle; it now uses the one-call `baselinenowcast()` wrapper for
  samples and notes the singular `estimate_and_apply_delay()` for a point nowcast.
* New `plot_reporting_hexamap()`: draws the reporting triangle as an
  age-period-cohort **hexamap** (Jalal and Burke, 2020). Event date, report date
  and delay are the cohort, period and age (`report = event + delay`); each cell is
  a hexagon coloured by its report count, and a **batch** — a single report date —
  reads as a clean **vertical stripe**. The number of hexagons is bounded by a
  `max_cells` safety cap (the delay axis is auto-capped, with a message, rather
  than drawing an unbounded map). Replaces the reporting-V panel in the batch
  article.
* Bug fix for issue #33: `autoplot(x, strata = "race", by_strata = TRUE)` no longer errors with
  a strata passed as column name. 
* `autoplot()` gained four **holiday panels**, which describe the attached
  `temporal_effects()` spec rather than the event unit:
  - `"calendar_holiday"` / `"delay_holiday"` — normalized cases / mean reporting
    delay by **day type**. The categories follow the spec: a `holidays` calendar
    plus `weekend = TRUE` gives `Weekday`/`Weekend`/`Holiday`, a calendar alone
    gives `Non-holiday`/`Holiday`, and a weekend effect alone gives
    `Weekday`/`Weekend`. A holiday falling on a weekend counts as a holiday.
  - `"calendar_holiday_lag"` / `"delay_holiday_lag"` — the same, by **position
    relative to the nearest holiday** (`"2 before"`, `"1 before"`, `"Holiday"`,
    `"1 after"`, ..., plus `"Other"` as the reference), as asked for by
    `holiday_lags`. These show exactly the days the `..._holiday_lag_k` /
    `..._holiday_lead_k` columns flag, so you can check a lag is worth modelling
    before you model it. A date that is both after one holiday and before the next
    is attributed to the nearer one, ties going to the "after" side.

* Bug fix: `tbl_now_to_epinowcast()` now passes a `timestep` to \pkg{epinowcast},
  inferred from the object's report units (`"days"` -> `"day"`, `"weeks"` ->
  `"week"`) and overridable with the new `timestep` argument. It previously left
  \pkg{epinowcast} on its `"day"` default whatever the data, so **weekly** data was
  laid out on a daily grid. 

* Bug fix: `tbl_now_to_epinowcast()` now derives the temporal-effect covariates on
  \pkg{epinowcast}'s completed date grid instead of carrying them through
  `enw_complete_dates()`. That function fills the (reference, report) grid and
  extends the reference axis into the nowcast horizon, but sets every non-schema
  column to `NA` on the rows it adds — so the covariates previously survived only on
  the original rows. Becasue the effects are functions of a date alone, they are n
  ow re-derived from the completed grid and cover every row, 
  including the recent horizon dates a nowcast has to predict.

# tbl.now 0.14.0

* `holiday_lags` and `weekend_lags` in `temporal_effects()` now accept **negative
  depths**, placing the effect *before* the break instead of after it. A negative
  depth creates `..._holiday_lead_k` / `..._weekend_lead_k` indicator columns that
  flag dates exactly `k` **working days** before a holiday / weekend, counting
  backwards from it — so `_lead_1` is the working day closest to the break.
  `weekend_lags = -1` flags the Friday, `weekend_lags = -3` flags the Wednesday,
  Thursday and Friday, and `holiday_lags = -1` flags Christmas Eve. Working days
  skip weekends and holidays exactly as they do for positive depths, and
  `holiday_lags` still requires a `holidays` calendar for either sign. Use it to
  capture the reporting slowdown that precedes a break; attach one specification
  per direction to model both sides of it. Positive depths are unchanged.
* `?temporal_effects` gained a **"Using a different holiday calendar"** section.
  `holidays` has always accepted any `almanac::rcalendar()`, but the docs only
  showed `cal_us_federal()`; reporting holidays are local, so the section covers
  the building blocks (built-in `hol_*()` rules, custom `rholiday()` rules,
  weekend observance with `hol_observe()`, and editing a calendar with
  `cal_add()` / `cal_remove()`), and works through the New York City calendar as
  an example.

# tbl.now 0.13.1

* Fixed style in the batch reporting vignette
* Improved the axis title position in the v triangle to better visualize the dates

# tbl.now 0.13.0

* Bug fix: `batch_shape_test()` no longer errors ("missing value where TRUE/FALSE
  needed") on large count data. The standardised rank-sum expands counts to one
  value per item, so the group sizes could exceed the 32-bit integer range and
  their product overflowed to `NA`; the group sizes are now computed as doubles.
* `batch_test()` now returns a **lean, Benjamini-Hochberg-only** result:
  `report_date`, `stratum`, `reported`, `baseline`, `deficit`, `delta`,
  `p_transport`, `p_transport_bh` and the `batch` flag, each documented under
  `?batch_test`. The raw per-point `classification` column (and the
  `p_creation`/`p_deletion`/scale columns behind it) has been dropped: it was not
  multiplicity-corrected and over-identified, whereas `batch` controls the false
  discovery rate. (`transport_discriminant()` keeps its `classification`.)
* `batch_test()` (and `transport_discriminant()`) now infer the calendar
  `period` from the object's temporal effects: a **day-of-week** effect sets
  `period = 7`, a **week-of-year** effect `period = 52` (see
  [add_temporal_effects()]). A `period` you pass still wins, with a note if it
  disagrees; and if the data is daily with no temporal effect, the function
  suggests `period = 7`.
* The `baseline_method` argument of `batch_test()` and `transport_discriminant()`
  has been **removed** — the baseline is always the repeated-median local line.
  The running-median (local-constant) alternative had no advantage: it reduces to
  the same fit on a flat series and is biased the moment the series trends.

* New `covid_us` dataset: a compact aggregation of the U.S. CDC COVID-19 Case
  Surveillance Public Use Data, with both event and report dates in 2020-2021 (a
  self-consistent "as of the end of 2021" snapshot), built to demonstrate **batch
  reporting**. Its reporting delay is huge and heavily right-skewed — cases were
  released to CDC in large backlog dumps — so `batch_test()` and the batch plots
  recover a clear, real signal (and correctly call the biggest December-2021
  spikes *surges*, since they land on the Omicron wave). Prepared with duckdb from
  the 14 GB source (see `data-raw/covid_us.R`).
* New article, *Finding batch reporting in CDC COVID-19 case surveillance data*,
  written for public-health practitioners with no maths. It builds a made-up
  outbreak with a planted batch to show what each plot looks like (including a
  novel **V reporting triangle** -- the reporting triangle rotated 45° so a batch
  is a horizontal slice), rehearses on a **real dengue epidemic curve** with
  simulated log-normal reporting and self-planted batches, finds the batches in the
  real `covid_us` data, adds a **wavelet** view (window-inner report-vs-event
  scalograms, via \pkg{wavScalogram}), and ends with a one-page summary table. A
  new **transport-vs-creation tutorial** plants a hold, a batch and a surge in a
  made-up outbreak and colours each day the same way on the reporting timeline and
  in the creation/transport plane, so a reader can trace a bar to its dot and see
  why a batch goes *up*, a surge goes *right*, and a hold drifts *up-and-left*.
* Every plot function now takes **`plotly = TRUE`** to return an interactive
  \pkg{plotly} widget (hover, zoom) instead of a static \pkg{ggplot2} plot:
  `plot_reporting_process()`, `plot_epidemic_process()`, `plot_reporting_triangle()`,
  `plot_delay_profiles()`, `plot_delay_drift()`, `plot_transport_discriminant()`,
  `plot_reporting_v()`, `plot_scalogram()`, `diagnostic_plot()` and `autoplot()`.
  Needs the (suggested) \pkg{plotly} package.
* New `plot_reporting_v()`: the reporting **"V"** -- the same data as
  `plot_reporting_triangle()` (the same event-date x delay cells) rotated 45° so
  report date runs up the page and the data opens into a V (left arm = event date,
  right arm = delay). A batch, a diagonal in the square triangle, becomes a
  horizontal slice. The whole observable triangle is filled (pale-blue reported
  zeros + coloured reports).
* New wavelet **scalograms**, `plot_scalogram(type = "reporting")` and
  `plot_scalogram(type = "epidemic")`, plus the paired `plot_reporting_process()`
  and `plot_epidemic_process()` bar charts. The scalogram splits the count series
  into fast wiggles (short periods) and slow swings (long periods) and shows the energy
  at each: a **batch** lights up as a bright short-period ridge in the *reporting*
  scalogram that the *epidemic* (event) scalogram lacks. These use a **window-inner**
  scalogram (\pkg{wavScalogram}, `border_effects = "INNER"`): computed from observed
  data only, with **no border padding**, so nothing is fabricated at the recent
  ("now") edge that matters for nowcasting. Reporting views are drawn in red,
  epidemic views in green. `plot_scalogram()` defaults to the PAUL wavelet
  (`wname`), which localises a batch more sharply; takes a `format` argument for
  the x-axis date labels (default `"%d/%b/%y"`); and paints the region outside the
  cone of influence dark grey. The series is analysed on its own integer time grid,
  so weekly (or monthly) data is handled correctly, and the heat map tiles a
  uniform index relabelled with dates so it stays gapless even for long series.
* The conservation monitors — `plot_creation_transport()` (the two window scores as
  stacked panels) together with the cumulative-backlog, reporting-lag, dashboard and
  transport-minus-creation "batch score" plots — live in
  `devel/conservation_extras.R`, kept out of the package: clean on large batches but
  noisy in general. The transport diagnostics keep their exported
  `transport_discriminant()` / `plot_transport_discriminant()`.
* `simulate_batch()` gains a **`held_fraction`** argument: the fraction of each
  closed date's reports actually held back and released later (default `1`, a full
  closure). With `held_fraction = 0.5`, roughly half of each day's reports are held
  and half report on time -- a realistic partial slow-down rather than a total
  blackout. Supported for `"linelist"` and `"count-incidence"` data (a cumulative
  total cannot be split).
* The default `lookback` for `batch_test()` and `transport_discriminant()` is
  now **7** (a week of daily reporting) rather than 3.
* The `@details` of the batch functions (`batch_test()`,
  `transport_discriminant()`, `batch_shape_test()`, `simulate_batch()`) and the
  batch plots were trimmed: the formal theorem / null-distribution derivations
  were replaced with concise, plain-language explanations.
* New `diagnostic_plot()`: a gallery of complementary views of the reporting
  process for spotting reporting artefacts (above all *batch reporting*), laid out
  in **two columns**. The five panels are the **reporting process** (reports by
  report date), the **reporting triangle** (event date x delay), the per-date
  **delay profiles**, the **reporting-delay drift** (`plot_delay_drift()`), and the
  **transport discriminant** plane. Each is also its own exported function. Choose
  views with `panels` (a single one is returned as a plain plot), and every view
  is facetted by stratum. `by = c("report", "event")` switches the profiles panel;
  `...` (e.g. `period = 7`) is routed to whichever panels accept it.
    * Every panel carries a plain-language, grey **caption** explaining what it
      shows and what the colours mean, and legends are labelled in words.
    * The **reporting process** y-axis is capped at the 99th percentile only when a
      *pathological* dump (over 30x the median day, e.g. covid's 1.8M-report day)
      would otherwise flatten the whole series; an ordinary batch spike -- the very
      thing the plot exists to show -- is left to tower.
    * The **transport discriminant** y-axis is limited to the batch region (with
      default clipping, so points stop at the panel edge) so the deep-negative
      "hold" dates do not squash the confirmed batches; the shaded region is now
      labelled *"Potential batch region"* and each confirmed batch gets a bold,
      unclipped date label.
    * The **reporting triangle** draws a **third axis for report date**: evenly
      spaced dashed diagonals (`report = event + delay`) running up-right at 45°,
      labelled by report date, so event date (x), delay (y) and report date are all
      readable off one plot (`plot_reporting_triangle(report_ticks =)`, default 6;
      `mark_batches =` optionally highlights the biggest batch stripes). It also
      distinguishes an *observable reported zero* (muted blue) from a *not yet
      reportable* cell (blank), on the *full calendar* event axis.
    * The **delay profiles** draw in a single colour at fixed transparency.
    * The **transport discriminant** colours red only the
      `batch_test()`-confirmed batches (BH-corrected), not the raw per-point
      classification -- which at level `alpha` painted 10-20% of points
      batch/surge/hold by construction, ignoring multiplicity and the heavy
      autocorrelation of the window statistics. The shaded batch region and the
      `±z*` lines are drawn only as a reference for where a batch would sit.
* New `transport_discriminant()`: exposes the plane behind `batch_test()`'s
  conservation law -- for every report date the **deficit** (the transport axis:
  reports the preceding window is missing) and the window **discriminant** (the
  creation axis: the window total relative to its baseline), with robust
  standardised `transport_z` / `creation_z` and the same quadrant `classification`.
  A batch sits top-left (a deficit paid the spike, no net creation); a surge sits
  bottom-right. Returned as a `transport_discriminant` tibble and plotted by
  `diagnostic_plot(panels = "transport")`.
* The multi-panel `autoplot()` title changed from *"Diagnostic plots"* to
  **"Automatic plot of effects"** (that phrase now titles `diagnostic_plot()`).
* `batch_test(null_model = "auto")` is now **overdispersion-aware**. The exact
  Poisson/Binomial null assumes Poisson counts *and* a baseline that captures the
  mean; real surveillance counts are overdispersed, and the conditional transport
  test is then badly anti-conservative (on clean but overdispersed Poisson data it
  can fake dozens of batches). `auto` now reserves the exact null for non-negative
  counts with no detected overdispersion (dispersion `<= 1.5`) and otherwise falls
  back to the dispersion-corrected robust null; signed (count-cumulative)
  increments still always use the robust null. This makes the default far more
  realistic on overdispersed data (e.g. filtered `covid_colombia` drops from ~125
  flags to ~18; add `period = 7` for its weekly reporting cadence to reach ~4).
  Force the old behaviour with `null_model = "poisson"` if you need it.
* `autoplot()`'s **empirical delay distribution** panel now adapts to
  `count-cumulative` data: instead of a histogram of increments it shows the
  *cumulative growth by delay* — boxplots (on a log scale, with a dashed reference
  at `1`) of the ratio of each event date's cumulative count at a delay to its
  count at the previous delay. Ratios above `1` are upward revisions, below `1`
  downward ones, and they converge to `1` as reporting completes, so you can see
  the cumulative curve stabilise. The log scale makes a doubling and a halving
  symmetric about `1`. `linelist` / `count-incidence` data keep the histogram, and
  the panel respects `by_strata`.
* `tbl_now_to_baselinenowcast(delays_unit = )` now defaults to `NULL` and is
  **inferred** from the object's time units for the `"matrix"` format: when the
  event and report units are equal and either `"days"` or `"weeks"`, that unit is
  used; otherwise the function errors asking you to supply `delays_unit`
  explicitly. (The `"long"` format never uses it.)
* Added the `covid_colombia` dataset from `diseasenowcasting` to here. 
* Fixed several documentation issues that produced *"could not resolve link"*
  warnings when building the docs (links to internal helpers / to the un-declared
  `trend` package, a `[0, 1]` mis-parsed as a link, and a mis-ordered internal
  roxygen block).
* `to_count()` now supports `count-cumulative` -> `count-incidence` by
  **de-accumulating** the series (increment = cumulative total minus the previous
  one within each event date and grouping). Because cumulative totals can be
  revised downward, an increment can be negative. This fixes `autoplot()` (and the
  other delay diagnostics) on `count-cumulative` data such as FluSight, which
  previously errored with *"Transformation from `data_type` count-cumulative to
  count-incidence not implemented"* (#26).
* Updated `SKILL.md` (the AI-agent usage guide) to cover everything added since
  0.10.0: reporting-delay `autoplot()` panels and the `panels` / `by_strata`
  selectors, `plot_delay_drift()` / `test_delay_drift()` / `test_delay_changepoint()`,
  the model-free batch detectors (`batch_test()`, `batch_shape_test()`,
  `simulate_batch()`), `get_nth_reported_cases()`, the after-holiday/weekend
  temporal-effect lags, `as_tibble()` / `as.data.frame()` coercion, and the new
  `count-cumulative` -> `count-incidence` support.

# tbl.now 0.12.0

## Batch detection, rebuilt around a conservation law

The report-batch detectors were rebuilt on a single, exact principle: **a batch
moves reports along the report axis without creating them**, so a window of report
dates spanning both the lull and the release has an unchanged total, whereas a
genuine epidemic surge inflates it. The previous heuristic
`detect_report_batches()` / `plot_report_batches()` (multi-signal robust-z, and
the model-based conditional scan) are **removed** and replaced by three
model-free, `r lifecycle::badge("experimental")` functions. Each derives its
mathematics in a **"The mathematics"** section of its help page.

* New `batch_test()` returns, per (report date, stratum), the `deficit` (reports
  missing beforehand — sensitive to a batch) and `delta` (the window total minus
  its expected value — sensitive to a real surge), and classifies each date as
  `"batch"`, `"surge"`, `"batch_and_surge"`, `"hold_or_deletion"` or `"none"`. The
  transport (batch) test conditions on the window total, so its size does not
  depend on the unknown incidence nor on the quality of the baseline; the baseline
  itself is refit from report dates *outside* each candidate window, which makes
  `delta` invariant to a within-window batch pathwise. It handles all data types,
  including `"count-cumulative"` (signed increments), and takes a `period` argument
  that absorbs a fixed reporting schedule (weekends, holidays).
* New `batch_shape_test()` tests whether a flagged report date drew on unusually
  *old* event dates, by a permutation rank-sum on the reporting delays. It is
  exactly distribution-free whenever incidence is locally log-linear.
* New `simulate_batch()` plants a known batch (a deterministic close-and-release)
  in a `tbl_now`, for validation and teaching.
* New **Batch detection** article, with worked examples on dengue (a planted
  batch), FluSight (count-cumulative), and a weekend reporting schedule.

# tbl.now 0.10.1

* `autoplot()`'s reporting-delay calendar panels (`delay_weekday`, `delay_week`,
`delay_month`) are now **normalized**: each event date's mean delay is divided by
the overall mean delay, so `1` marks an average delay and a dashed reference line
is drawn there. Previously the ungrouped panels plotted the raw mean delay while
the `by_strata = TRUE` panels were already normalized. They now share one scale,
matching the case-count calendar panels and making the calendar *pattern*
comparable across strata (y-axis: `"Normalized delay"`).
* `plot_delay_drift()`'s `window` now defaults to **`7` periods regardless of the
time unit** — 7 days for daily data, 7 weeks for weekly data. Previously the
default was data-dependent (`max(5, n_periods / 20)`), which produced a very wide
window on long series. Pass `window =` to smooth a specific series.
* Internal: replaced the remaining base-R data-frame subsetting and column
assignment (`df[cond, ]`, `df$col <- ...`) outside the converters with the
equivalent `dplyr` verbs (`filter()`, `select()`, `slice()`, `mutate()`). No
user-facing behaviour change. The examples and vignettes now likewise use
`dplyr::filter()` rather than `[` (e.g. `dplyr::filter(batches, batch)`).

# tbl.now 0.10.0

* New `get_nth_reported_cases()`: the cumulative cases reported for each event
date **within a given delay**. `delay = 0` gives the initial snapshot, `delay = 1`
adds the delay-1 reports, and so on; `delay = Inf` (or the maximum delay) matches
`get_latest_reported_cases()`. Documented alongside `get_initial_reported_cases()`
and `get_latest_reported_cases()`.
* **Performance**: `get_latest_reported_cases()`, `get_initial_reported_cases()`
and `get_nth_reported_cases()` are substantially faster (~3-4x on the bundled
data) — the aggregation now runs on a declassed data frame and the `tbl_now` is
reconstructed once, with identical output.
* The experimental diagnostic functions (`plot_delay_drift()`,
`test_delay_drift()`, `test_delay_changepoint()`, `detect_report_batches()`,
`plot_report_batches()`) now carry a lifecycle **experimental** badge.
`test_delay_drift()` and `test_delay_changepoint()` additionally emit a `cli`
warning that they are experimental, their results are not guaranteed and their
interface may change. Flagged batches, change points and trend changes are
surfaced as **potential** (e.g. "potential batches", "potential change point").
* New `detect_report_batches()` and `plot_report_batches()` to detect **batch
reporting** — report dates on which a laboratory releases a backlog of many old
cases at once. Working on the report-date axis, it flags a report date using up
to four selectable robust-anomaly signals (`volume`, `delay`, `span`, `gap`),
AND-ed together. Requiring the `delay` (long/dispersed delays) signal alongside
`volume` is what **distinguishes a batch from an epidemic peak**: a peak also
spikes the report volume, but its cases keep the normal short delay distribution,
so its delay score stays low. `detect_report_batches()` returns a per-report-date
table with the features, robust scores and a `batch` flag; `plot_report_batches()`
shows the report-volume and mean-delay timelines with the flagged dates marked.

* New `test_delay_changepoint()` complements `test_delay_drift()`: where the
latter tests for a *gradual* monotonic trend, this tests for a **single abrupt
change point** in the per-period delay summaries using **Pettitt's**
nonparametric test (implemented directly, no extra dependency). It reports the
estimated change date, the before/after level of the statistic, the shift and a
`changepoint_detected` verdict, per stat (median / mean / IQR / 10-90 spread) and
per stratum, on mature data only.
* `plot_delay_drift()` gained a `changepoint` argument: set it to `TRUE` to mark
the estimated change point of the median delay on the fan chart with a vertical
line.

* New `plot_delay_drift()` and `test_delay_drift()` to answer *"do reporting
delay distributions drift over time?"*.
  * `plot_delay_drift()` draws a rolling **fan chart** of the count-weighted
  delay distribution indexed by event date: a solid rolling median, a dashed
  rolling mean, and 25-75% / 10-90% quantile bands. The recent, not-yet-fully
  reported region (after the `level` incompleteness cutoff) is shaded grey so the
  truncation-induced dip is not mistaken for drift. Supports `by_strata`.
  * `test_delay_drift()` runs an **autocorrelation-robust monotonic-trend test**
  (Hamed-Rao modified Mann-Kendall by default, with Yue-Pilon and block-bootstrap
  options via the new `modifiedmk` *Suggests*) on the per-period delay summaries,
  testing both a location statistic (median/mean) and a dispersion statistic
  (IQR / 10-90 spread), on mature data only. Returns a tidy tibble with the
  Kendall tau, Sen's slope, p-value and a `drift` verdict, per stat and stratum.

* `autoplot()` gained a `by_strata` argument (default `FALSE`). When `TRUE`,
every panel is split by stratum: the calendar and delay boxplots become dodged
boxes (one per stratum, side by side), the epidemic process and both
periodograms become one coloured line per stratum (no area fill), and the delay
distribution becomes dodged bars. Boxplots are normalized **per stratum** (1 =
that stratum's own average) so the calendar pattern is comparable across strata,
and strata are coloured with a `viridis` scale. A companion `strata` argument
chooses which columns to group on (defaults to the object's `strata`; pass a
subset such as `strata = "gender"` to override).

* `autoplot()` now draws **reporting-delay** diagnostic panels alongside the
case-count ones, so you can see *delay effects*: the mean reporting delay by day
of week / week of year / month (`delay_weekday`, `delay_week`, `delay_month`),
and a periodogram of the mean-delay series (`delay_seasonality`) that reveals
periodicity in the delay itself. The delay panels are computed on the complete
part of the series (before the incompleteness line) so recent truncation does not
bias them.
* `autoplot()` gained a `panels` argument to choose which panels to draw. It
accepts the concrete panel keys, or the aliases `"all"` (default), `"calendar"`
and `"delay_calendar"`. Selecting a single panel returns it as a plain `ggplot2`
object instead of a `patchwork`. Unknown panels error; panels that do not apply
to the data's time unit are skipped with a warning.
* New pkgdown article *"One dataset, many nowcasts"* now also demonstrates that
temporal (delay) effect columns are carried into `epinowcast`
(`metareference`/`metareport`), `baselinenowcast` (long) and `epidist`, with a
table clarifying which target formats can hold covariates and how each model can
use them.

* `temporal_effects()` gained an **after-holiday** and **after-weekend** effect
via the new `holiday_lags` and `weekend_lags` arguments. Each takes a
non-negative integer depth `N`; materialising the spec then adds indicator
columns `..._holiday_lag_1 … ..._holiday_lag_N` (and likewise `..._weekend_lag_k`)
that flag dates falling exactly `k` **working days** after a holiday / weekend.
Working days skip weekends and holidays, so the effect lands on the first day(s)
back at work — designed to capture the rise in cases just after a holiday or
weekend. `holiday_lags` requires a `holidays` calendar. The columns are picked up
automatically by every `tbl_now_to_*()` converter (as covariate columns) and by
`diseasenowcasting::nowcast()`.
* Documented and tested attaching temporal effects to the **report date** (in
addition to the default event date) via
`add_temporal_effects(x, spec, date_type = "report_date")`. Event- and
report-date effects can coexist on the same `tbl_now`; both sets of columns
(`.event_*` and `.report_*`) are carried through all converters.

# tbl.now 0.9.0

* Added `as_tibble()` and `as.data.frame()` methods for `tbl_now` with an opt-in
`compute_temporal_effects` argument (default `FALSE`). Passing
`compute_temporal_effects = TRUE` materialises the lazy `temporal_effects()`
spec (holidays, Fourier terms, calendar effects) into columns before returning
a plain `tibble` / `data.frame`; the input `tbl_now` is left unchanged. The
default stays lazy on purpose, because `dplyr` relies on these coercions being
cheap, non-materialising declassers internally (e.g. `group_by()`).
* The `tbl_now_to_*()` converters now carry the (lazy) temporal-effect columns
(holidays, Fourier seasonal terms, day-of-week / calendar effects) into the
target format as covariate columns. The spec is materialised on demand via
`compute_temporal_effects()` at conversion time (the input `tbl_now` is left
unchanged), and the columns are passed to `data.table`, `tsibble`,
`baselinenowcast` long format, `epidist`, and `epinowcast` (where they appear in
the observations and `metareference` tables for use in the reference module).
The `baselinenowcast` reporting-triangle matrix still cannot hold them.
* Removed the `|>` export and changed all the pipes to `|>`
* Refactored `converters.R` for readability (dplyr column operations instead of
base indexing, full variable names, lintr-clean).
* The `tbl_now_to_*()` converters now keep the `covariates` and `is_censored`
columns wherever the target format can hold them (`data.table`, `tsibble`,
`baselinenowcast` long format, `epidist` linelist); the fixed modelling objects
(`enw_preprocess_data`, the reporting-triangle matrix, the EpiNow2 series) still
cannot carry them.
* Added S3 methods on the other packages' coercion generics so they accept a
`tbl_now` directly: `as_epidist_linelist_data()`, `as_reporting_triangle()`,
`as_tsibble()` and `as.data.table()`, each wrapping the matching
`tbl_now_to_*()`.
* Fixed `tbl_now_to_data_table()` checking for `baselinenowcast` instead of
`data.table`, and `tbl_now_to_baselinenowcast(format = "long")` no longer
requiring `baselinenowcast` to be installed.

# tbl.now 0.8.0

* Modified the `update` as the `t_effect` argument was not doing anything. 
* Fixed bug that errored `complete_zeroes` when `is_censored` was given. 
* Removed explicit zeroes from the converters (`tbl_now_from_*`) as they
are not necessary in `tbl_now`. 
* Added `censor_reporting_delays_above()` to flag reports with an implausibly long delay
as censored (their delay becomes an upper bound).
* Improved documentation and README
* Documented all internal functions with roxygen (`@keywords internal` + `@noRd`)
and ensured every exported function has a `@return`.
* Homogenized `lifecycle` badges. 
* Brought the `censor_reporting_delays_above` function from `diseasenowcasting` to `tbl_now`. 
* `tbl_now_from_epinowcast()` now accepts not only the raw long input but also a
preprocessed `enw_preprocess_data` object or a fitted `epinowcast` object
(grouping auto-detected), matching the format `epinowcast` uses for summaries
and plots.
* `tbl_now_to_EpiNow2()` gained a `model` argument: `"estimate_infections"`
(default, the single `date`/`confirm` series) and `"estimate_truncation"` (a
list of report-date snapshots, the one EpiNow2 model that uses the report
dimension). Documentation clarified accordingly.
* Fixed two converter `requireNamespace()` guards: `tbl_now_to_data_table()`
checked for `baselinenowcast` instead of `data.table`, and
`tbl_now_to_baselinenowcast(format = "long")` no longer requires
`baselinenowcast` to be installed.

# tbl.now 0.7.5

* Bumped roxygen to version 8.0.0. This also resulted in updated documentation. 
* Changed `autoplot()`'s default level to 0.95 
* Added tests for converters and pillars.
* Throws warning when converting to `baselinenowcast` if data is 
`"count-cumulative"`. 

# tbl.now 0.7.3

* Added the `update_now()` function to make it more intuitive to update
the now. 

# tbl.now 0.7.0

* Added an `autoplot()` method for `tbl_now` objects that produces a multi-panel
diagnostic overview: the empirical delay distribution, the observed epidemic
process with an incompleteness line (controlled by `level`), normalized
calendar-effect boxplots (cases relative to the overall mean), and a periodogram
to help choose Fourier `seasons`. Daily data shows both a day-of-week and a
week-of-year boxplot panel; weekly data shows week-of-year. Built on `ggplot2`
and `patchwork`. The x-axis limits of each panel can be set individually
(`delay_distribution_xlim`, `event_date_xlim`, `calendar_effect_xlim`,
`seasonality_xlim`), and holidays from the temporal-effects spec are marked with
red dots on the epidemic process.
* Added converters to and from other packages, all of the form
`tbl_now_from_*()` / `tbl_now_to_*()`: `epinowcast`, `baselinenowcast`,
`EpiNow2` (to only), `epidist`, `data.table` and `tsibble`. The
`tbl_now_from_*()` functions wrap `as_tbl_now()` and forward `...` to
`tbl_now()`; the `tbl_now_to_*()` functions call into the target package. All
accept a `verbose` argument that reports the choices made (the inferred `now`,
data type, units, and column mapping).
* `as_tbl_now()` gained methods for the classes produced by `tbl_now_to_*()`
(`enw_preprocess_data`, `reporting_triangle`, `epidist_linelist_data`, `tbl_ts`
and `data.table`), so a converted object can be turned straight back into a
`tbl_now`.
* Documented `autoplot()` and the converters in the introduction vignette.

# tbl.now 0.6.4

* Fixed dependency on R >= 4.2.0
* Update function now defaults the censoring to FALSE if the update
is censored but the original is not. 

# tbl.now 0.6.3

* Added season length to seasons so we can get weekly seasonality. 

# tbl.now 0.6.2

* Removed warning when using columns for temporal effects that cascaded into `to_count`.
* Changed DESCRIPTION to fix ortographic error and trigger less messages of unknown words. 

# tbl.now 0.6.1

* Changed links in description of `tidy-select`

# tbl.now 0.6.0

* Changed temporal effects to be lazy (as required by #17) so that now its
easier to use `dplyr`
functions without compromising them. 
* Bumped the deprecated dplyr's `*_at` functions to use `all_of()`
* Fixed to no warnings during test. 
* Users can now pass the `.delay` column directly (#6) and it will recalculate 
the missing column (i.e. event or report)
* Added `complete_zeroes` to vignette (#13).
