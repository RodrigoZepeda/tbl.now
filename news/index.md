# Changelog

## tbl.now 0.14.0

- `holiday_lags` and `weekend_lags` in
  [`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md)
  now accept **negative depths**, placing the effect *before* the break
  instead of after it. A negative depth creates `..._holiday_lead_k` /
  `..._weekend_lead_k` indicator columns that flag dates exactly `k`
  **working days** before a holiday / weekend, counting backwards from
  it — so `_lead_1` is the working day closest to the break.
  `weekend_lags = -1` flags the Friday, `weekend_lags = -3` flags the
  Wednesday, Thursday and Friday, and `holiday_lags = -1` flags
  Christmas Eve. Working days skip weekends and holidays exactly as they
  do for positive depths, and `holiday_lags` still requires a `holidays`
  calendar for either sign. Use it to capture the reporting slowdown
  that precedes a break; attach one specification per direction to model
  both sides of it. Positive depths are unchanged.
- [`?temporal_effects`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md)
  gained a **“Using a different holiday calendar”** section. `holidays`
  has always accepted any
  [`almanac::rcalendar()`](https://rdrr.io/pkg/almanac/man/rcalendar.html),
  but the docs only showed
  [`cal_us_federal()`](https://rdrr.io/pkg/almanac/man/cal_us_federal.html);
  reporting holidays are local, so the section covers the building
  blocks (built-in `hol_*()` rules, custom
  [`rholiday()`](https://rdrr.io/pkg/almanac/man/rholiday.html) rules,
  weekend observance with
  [`hol_observe()`](https://rdrr.io/pkg/almanac/man/holiday-utilities.html),
  and editing a calendar with
  [`cal_add()`](https://rdrr.io/pkg/almanac/man/calendar-add-remove.html)
  /
  [`cal_remove()`](https://rdrr.io/pkg/almanac/man/calendar-add-remove.html)),
  and works through the New York City calendar as an example.

## tbl.now 0.13.1

- Fixed style in the batch reporting vignette
- Improved the axis title position in the v triangle to better visualize
  the dates

## tbl.now 0.13.0

- Bug fix:
  [`batch_shape_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_shape_test.md)
  no longer errors (“missing value where TRUE/FALSE needed”) on large
  count data. The standardised rank-sum expands counts to one value per
  item, so the group sizes could exceed the 32-bit integer range and
  their product overflowed to `NA`; the group sizes are now computed as
  doubles.

- [`batch_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_test.md)
  now returns a **lean, Benjamini-Hochberg-only** result: `report_date`,
  `stratum`, `reported`, `baseline`, `deficit`, `delta`, `p_transport`,
  `p_transport_bh` and the `batch` flag, each documented under
  [`?batch_test`](https://rodrigozepeda.github.io/tbl.now/reference/batch_test.md).
  The raw per-point `classification` column (and the
  `p_creation`/`p_deletion`/scale columns behind it) has been dropped:
  it was not multiplicity-corrected and over-identified, whereas `batch`
  controls the false discovery rate.
  ([`transport_discriminant()`](https://rodrigozepeda.github.io/tbl.now/reference/transport_discriminant.md)
  keeps its `classification`.)

- [`batch_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_test.md)
  (and
  [`transport_discriminant()`](https://rodrigozepeda.github.io/tbl.now/reference/transport_discriminant.md))
  now infer the calendar `period` from the object’s temporal effects: a
  **day-of-week** effect sets `period = 7`, a **week-of-year** effect
  `period = 52` (see \[add_temporal_effects()\]). A `period` you pass
  still wins, with a note if it disagrees; and if the data is daily with
  no temporal effect, the function suggests `period = 7`.

- The `baseline_method` argument of
  [`batch_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_test.md)
  and
  [`transport_discriminant()`](https://rodrigozepeda.github.io/tbl.now/reference/transport_discriminant.md)
  has been **removed** — the baseline is always the repeated-median
  local line. The running-median (local-constant) alternative had no
  advantage: it reduces to the same fit on a flat series and is biased
  the moment the series trends.

- New `covid_us` dataset: a compact aggregation of the U.S. CDC COVID-19
  Case Surveillance Public Use Data, with both event and report dates in
  2020-2021 (a self-consistent “as of the end of 2021” snapshot), built
  to demonstrate **batch reporting**. Its reporting delay is huge and
  heavily right-skewed — cases were released to CDC in large backlog
  dumps — so
  [`batch_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_test.md)
  and the batch plots recover a clear, real signal (and correctly call
  the biggest December-2021 spikes *surges*, since they land on the
  Omicron wave). Prepared with duckdb from the 14 GB source (see
  `data-raw/covid_us.R`).

- New article, *Finding batch reporting in CDC COVID-19 case
  surveillance data*, written for public-health practitioners with no
  maths. It builds a made-up outbreak with a planted batch to show what
  each plot looks like (including a novel **V reporting triangle** – the
  reporting triangle rotated 45° so a batch is a horizontal slice),
  rehearses on a **real dengue epidemic curve** with simulated
  log-normal reporting and self-planted batches, finds the batches in
  the real `covid_us` data, adds a **wavelet** view (window-inner
  report-vs-event scalograms, via ), and ends with a one-page summary
  table. A new **transport-vs-creation tutorial** plants a hold, a batch
  and a surge in a made-up outbreak and colours each day the same way on
  the reporting timeline and in the creation/transport plane, so a
  reader can trace a bar to its dot and see why a batch goes *up*, a
  surge goes *right*, and a hold drifts *up-and-left*.

- Every plot function now takes **`plotly = TRUE`** to return an
  interactive widget (hover, zoom) instead of a static plot:
  [`plot_reporting_process()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_reporting_process.md),
  [`plot_epidemic_process()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_epidemic_process.md),
  [`plot_reporting_triangle()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_reporting_triangle.md),
  [`plot_delay_profiles()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_profiles.md),
  [`plot_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_drift.md),
  [`plot_transport_discriminant()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_transport_discriminant.md),
  [`plot_reporting_v()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_reporting_v.md),
  [`plot_scalogram()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_scalogram.md),
  [`diagnostic_plot()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnostic_plot.md)
  and
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).
  Needs the (suggested) package.

- New
  [`plot_reporting_v()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_reporting_v.md):
  the reporting **“V”** – the same data as
  [`plot_reporting_triangle()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_reporting_triangle.md)
  (the same event-date x delay cells) rotated 45° so report date runs up
  the page and the data opens into a V (left arm = event date, right arm
  = delay). A batch, a diagonal in the square triangle, becomes a
  horizontal slice. The whole observable triangle is filled (pale-blue
  reported zeros + coloured reports).

- New wavelet **scalograms**, `plot_scalogram(type = "reporting")` and
  `plot_scalogram(type = "epidemic")`, plus the paired
  [`plot_reporting_process()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_reporting_process.md)
  and
  [`plot_epidemic_process()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_epidemic_process.md)
  bar charts. The scalogram splits the count series into fast wiggles
  (short periods) and slow swings (long periods) and shows the energy at
  each: a **batch** lights up as a bright short-period ridge in the
  *reporting* scalogram that the *epidemic* (event) scalogram lacks.
  These use a **window-inner** scalogram (, `border_effects = "INNER"`):
  computed from observed data only, with **no border padding**, so
  nothing is fabricated at the recent (“now”) edge that matters for
  nowcasting. Reporting views are drawn in red, epidemic views in green.
  [`plot_scalogram()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_scalogram.md)
  defaults to the PAUL wavelet (`wname`), which localises a batch more
  sharply; takes a `format` argument for the x-axis date labels (default
  `"%d/%b/%y"`); and paints the region outside the cone of influence
  dark grey. The series is analysed on its own integer time grid, so
  weekly (or monthly) data is handled correctly, and the heat map tiles
  a uniform index relabelled with dates so it stays gapless even for
  long series.

- The conservation monitors — `plot_creation_transport()` (the two
  window scores as stacked panels) together with the cumulative-backlog,
  reporting-lag, dashboard and transport-minus-creation “batch score”
  plots — live in `devel/conservation_extras.R`, kept out of the
  package: clean on large batches but noisy in general. The transport
  diagnostics keep their exported
  [`transport_discriminant()`](https://rodrigozepeda.github.io/tbl.now/reference/transport_discriminant.md)
  /
  [`plot_transport_discriminant()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_transport_discriminant.md).

- [`simulate_batch()`](https://rodrigozepeda.github.io/tbl.now/reference/simulate_batch.md)
  gains a **`held_fraction`** argument: the fraction of each closed
  date’s reports actually held back and released later (default `1`, a
  full closure). With `held_fraction = 0.5`, roughly half of each day’s
  reports are held and half report on time – a realistic partial
  slow-down rather than a total blackout. Supported for `"linelist"` and
  `"count-incidence"` data (a cumulative total cannot be split).

- The default `lookback` for
  [`batch_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_test.md)
  and
  [`transport_discriminant()`](https://rodrigozepeda.github.io/tbl.now/reference/transport_discriminant.md)
  is now **7** (a week of daily reporting) rather than 3.

- The `@details` of the batch functions
  ([`batch_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_test.md),
  [`transport_discriminant()`](https://rodrigozepeda.github.io/tbl.now/reference/transport_discriminant.md),
  [`batch_shape_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_shape_test.md),
  [`simulate_batch()`](https://rodrigozepeda.github.io/tbl.now/reference/simulate_batch.md))
  and the batch plots were trimmed: the formal theorem /
  null-distribution derivations were replaced with concise,
  plain-language explanations.

- New
  [`diagnostic_plot()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnostic_plot.md):
  a gallery of complementary views of the reporting process for spotting
  reporting artefacts (above all *batch reporting*), laid out in **two
  columns**. The five panels are the **reporting process** (reports by
  report date), the **reporting triangle** (event date x delay), the
  per-date **delay profiles**, the **reporting-delay drift**
  ([`plot_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_drift.md)),
  and the **transport discriminant** plane. Each is also its own
  exported function. Choose views with `panels` (a single one is
  returned as a plain plot), and every view is facetted by stratum.
  `by = c("report", "event")` switches the profiles panel; `...`
  (e.g. `period = 7`) is routed to whichever panels accept it.

  - Every panel carries a plain-language, grey **caption** explaining
    what it shows and what the colours mean, and legends are labelled in
    words.
  - The **reporting process** y-axis is capped at the 99th percentile
    only when a *pathological* dump (over 30x the median day,
    e.g. covid’s 1.8M-report day) would otherwise flatten the whole
    series; an ordinary batch spike – the very thing the plot exists to
    show – is left to tower.
  - The **transport discriminant** y-axis is limited to the batch region
    (with default clipping, so points stop at the panel edge) so the
    deep-negative “hold” dates do not squash the confirmed batches; the
    shaded region is now labelled *“Potential batch region”* and each
    confirmed batch gets a bold, unclipped date label.
  - The **reporting triangle** draws a **third axis for report date**:
    evenly spaced dashed diagonals (`report = event + delay`) running
    up-right at 45°, labelled by report date, so event date (x),
    delay (y) and report date are all readable off one plot
    (`plot_reporting_triangle(report_ticks =)`, default 6;
    `mark_batches =` optionally highlights the biggest batch stripes).
    It also distinguishes an *observable reported zero* (muted blue)
    from a *not yet reportable* cell (blank), on the *full calendar*
    event axis.
  - The **delay profiles** draw in a single colour at fixed
    transparency.
  - The **transport discriminant** colours red only the
    [`batch_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_test.md)-confirmed
    batches (BH-corrected), not the raw per-point classification – which
    at level `alpha` painted 10-20% of points batch/surge/hold by
    construction, ignoring multiplicity and the heavy autocorrelation of
    the window statistics. The shaded batch region and the `±z*` lines
    are drawn only as a reference for where a batch would sit.

- New
  [`transport_discriminant()`](https://rodrigozepeda.github.io/tbl.now/reference/transport_discriminant.md):
  exposes the plane behind
  [`batch_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_test.md)’s
  conservation law – for every report date the **deficit** (the
  transport axis: reports the preceding window is missing) and the
  window **discriminant** (the creation axis: the window total relative
  to its baseline), with robust standardised `transport_z` /
  `creation_z` and the same quadrant `classification`. A batch sits
  top-left (a deficit paid the spike, no net creation); a surge sits
  bottom-right. Returned as a `transport_discriminant` tibble and
  plotted by `diagnostic_plot(panels = "transport")`.

- The multi-panel
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  title changed from *“Diagnostic plots”* to **“Automatic plot of
  effects”** (that phrase now titles
  [`diagnostic_plot()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnostic_plot.md)).

- `batch_test(null_model = "auto")` is now **overdispersion-aware**. The
  exact Poisson/Binomial null assumes Poisson counts *and* a baseline
  that captures the mean; real surveillance counts are overdispersed,
  and the conditional transport test is then badly anti-conservative (on
  clean but overdispersed Poisson data it can fake dozens of batches).
  `auto` now reserves the exact null for non-negative counts with no
  detected overdispersion (dispersion `<= 1.5`) and otherwise falls back
  to the dispersion-corrected robust null; signed (count-cumulative)
  increments still always use the robust null. This makes the default
  far more realistic on overdispersed data (e.g. filtered
  `covid_colombia` drops from ~125 flags to ~18; add `period = 7` for
  its weekly reporting cadence to reach ~4). Force the old behaviour
  with `null_model = "poisson"` if you need it.

- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)’s
  **empirical delay distribution** panel now adapts to
  `count-cumulative` data: instead of a histogram of increments it shows
  the *cumulative growth by delay* — boxplots (on a log scale, with a
  dashed reference at `1`) of the ratio of each event date’s cumulative
  count at a delay to its count at the previous delay. Ratios above `1`
  are upward revisions, below `1` downward ones, and they converge to
  `1` as reporting completes, so you can see the cumulative curve
  stabilise. The log scale makes a doubling and a halving symmetric
  about `1`. `linelist` / `count-incidence` data keep the histogram, and
  the panel respects `by_strata`.

- `tbl_now_to_baselinenowcast(delays_unit = )` now defaults to `NULL`
  and is **inferred** from the object’s time units for the `"matrix"`
  format: when the event and report units are equal and either `"days"`
  or `"weeks"`, that unit is used; otherwise the function errors asking
  you to supply `delays_unit` explicitly. (The `"long"` format never
  uses it.)

- Added the `covid_colombia` dataset from `diseasenowcasting` to here.

- Fixed several documentation issues that produced *“could not resolve
  link”* warnings when building the docs (links to internal helpers / to
  the un-declared `trend` package, a `[0, 1]` mis-parsed as a link, and
  a mis-ordered internal roxygen block).

- [`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md)
  now supports `count-cumulative` -\> `count-incidence` by
  **de-accumulating** the series (increment = cumulative total minus the
  previous one within each event date and grouping). Because cumulative
  totals can be revised downward, an increment can be negative. This
  fixes
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  (and the other delay diagnostics) on `count-cumulative` data such as
  FluSight, which previously errored with *“Transformation from
  `data_type` count-cumulative to count-incidence not implemented”*
  (#26).

- Updated `SKILL.md` (the AI-agent usage guide) to cover everything
  added since 0.10.0: reporting-delay
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  panels and the `panels` / `by_strata` selectors,
  [`plot_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_drift.md)
  /
  [`test_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/test_delay_drift.md)
  /
  [`test_delay_changepoint()`](https://rodrigozepeda.github.io/tbl.now/reference/test_delay_changepoint.md),
  the model-free batch detectors
  ([`batch_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_test.md),
  [`batch_shape_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_shape_test.md),
  [`simulate_batch()`](https://rodrigozepeda.github.io/tbl.now/reference/simulate_batch.md)),
  [`get_nth_reported_cases()`](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.md),
  the after-holiday/weekend temporal-effect lags,
  [`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
  / [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
  coercion, and the new `count-cumulative` -\> `count-incidence`
  support.

## tbl.now 0.12.0

### Batch detection, rebuilt around a conservation law

The report-batch detectors were rebuilt on a single, exact principle:
**a batch moves reports along the report axis without creating them**,
so a window of report dates spanning both the lull and the release has
an unchanged total, whereas a genuine epidemic surge inflates it. The
previous heuristic `detect_report_batches()` / `plot_report_batches()`
(multi-signal robust-z, and the model-based conditional scan) are
**removed** and replaced by three model-free,
`r lifecycle::badge("experimental")` functions. Each derives its
mathematics in a **“The mathematics”** section of its help page.

- New
  [`batch_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_test.md)
  returns, per (report date, stratum), the `deficit` (reports missing
  beforehand — sensitive to a batch) and `delta` (the window total minus
  its expected value — sensitive to a real surge), and classifies each
  date as `"batch"`, `"surge"`, `"batch_and_surge"`,
  `"hold_or_deletion"` or `"none"`. The transport (batch) test
  conditions on the window total, so its size does not depend on the
  unknown incidence nor on the quality of the baseline; the baseline
  itself is refit from report dates *outside* each candidate window,
  which makes `delta` invariant to a within-window batch pathwise. It
  handles all data types, including `"count-cumulative"` (signed
  increments), and takes a `period` argument that absorbs a fixed
  reporting schedule (weekends, holidays).
- New
  [`batch_shape_test()`](https://rodrigozepeda.github.io/tbl.now/reference/batch_shape_test.md)
  tests whether a flagged report date drew on unusually *old* event
  dates, by a permutation rank-sum on the reporting delays. It is
  exactly distribution-free whenever incidence is locally log-linear.
- New
  [`simulate_batch()`](https://rodrigozepeda.github.io/tbl.now/reference/simulate_batch.md)
  plants a known batch (a deterministic close-and-release) in a
  `tbl_now`, for validation and teaching.
- New **Batch detection** article, with worked examples on dengue (a
  planted batch), FluSight (count-cumulative), and a weekend reporting
  schedule.

## tbl.now 0.10.1

- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)’s
  reporting-delay calendar panels (`delay_weekday`, `delay_week`,
  `delay_month`) are now **normalized**: each event date’s mean delay is
  divided by the overall mean delay, so `1` marks an average delay and a
  dashed reference line is drawn there. Previously the ungrouped panels
  plotted the raw mean delay while the `by_strata = TRUE` panels were
  already normalized. They now share one scale, matching the case-count
  calendar panels and making the calendar *pattern* comparable across
  strata (y-axis: `"Normalized delay"`).
- [`plot_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_drift.md)’s
  `window` now defaults to **`7` periods regardless of the time unit** —
  7 days for daily data, 7 weeks for weekly data. Previously the default
  was data-dependent (`max(5, n_periods / 20)`), which produced a very
  wide window on long series. Pass `window =` to smooth a specific
  series.
- Internal: replaced the remaining base-R data-frame subsetting and
  column assignment (`df[cond, ]`, `df$col <- ...`) outside the
  converters with the equivalent `dplyr` verbs
  ([`filter()`](https://dplyr.tidyverse.org/reference/filter.html),
  [`select()`](https://dplyr.tidyverse.org/reference/select.html),
  [`slice()`](https://dplyr.tidyverse.org/reference/slice.html),
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)). No
  user-facing behaviour change. The examples and vignettes now likewise
  use
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  rather than `[` (e.g. `dplyr::filter(batches, batch)`).

## tbl.now 0.10.0

- New
  [`get_nth_reported_cases()`](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.md):
  the cumulative cases reported for each event date **within a given
  delay**. `delay = 0` gives the initial snapshot, `delay = 1` adds the
  delay-1 reports, and so on; `delay = Inf` (or the maximum delay)
  matches
  [`get_latest_reported_cases()`](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.md).
  Documented alongside
  [`get_initial_reported_cases()`](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.md)
  and
  [`get_latest_reported_cases()`](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.md).

- **Performance**:
  [`get_latest_reported_cases()`](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.md),
  [`get_initial_reported_cases()`](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.md)
  and
  [`get_nth_reported_cases()`](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.md)
  are substantially faster (~3-4x on the bundled data) — the aggregation
  now runs on a declassed data frame and the `tbl_now` is reconstructed
  once, with identical output.

- The experimental diagnostic functions
  ([`plot_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_drift.md),
  [`test_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/test_delay_drift.md),
  [`test_delay_changepoint()`](https://rodrigozepeda.github.io/tbl.now/reference/test_delay_changepoint.md),
  `detect_report_batches()`, `plot_report_batches()`) now carry a
  lifecycle **experimental** badge.
  [`test_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/test_delay_drift.md)
  and
  [`test_delay_changepoint()`](https://rodrigozepeda.github.io/tbl.now/reference/test_delay_changepoint.md)
  additionally emit a `cli` warning that they are experimental, their
  results are not guaranteed and their interface may change. Flagged
  batches, change points and trend changes are surfaced as **potential**
  (e.g. “potential batches”, “potential change point”).

- New `detect_report_batches()` and `plot_report_batches()` to detect
  **batch reporting** — report dates on which a laboratory releases a
  backlog of many old cases at once. Working on the report-date axis, it
  flags a report date using up to four selectable robust-anomaly signals
  (`volume`, `delay`, `span`, `gap`), AND-ed together. Requiring the
  `delay` (long/dispersed delays) signal alongside `volume` is what
  **distinguishes a batch from an epidemic peak**: a peak also spikes
  the report volume, but its cases keep the normal short delay
  distribution, so its delay score stays low. `detect_report_batches()`
  returns a per-report-date table with the features, robust scores and a
  `batch` flag; `plot_report_batches()` shows the report-volume and
  mean-delay timelines with the flagged dates marked.

- New
  [`test_delay_changepoint()`](https://rodrigozepeda.github.io/tbl.now/reference/test_delay_changepoint.md)
  complements
  [`test_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/test_delay_drift.md):
  where the latter tests for a *gradual* monotonic trend, this tests for
  a **single abrupt change point** in the per-period delay summaries
  using **Pettitt’s** nonparametric test (implemented directly, no extra
  dependency). It reports the estimated change date, the before/after
  level of the statistic, the shift and a `changepoint_detected`
  verdict, per stat (median / mean / IQR / 10-90 spread) and per
  stratum, on mature data only.

- [`plot_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_drift.md)
  gained a `changepoint` argument: set it to `TRUE` to mark the
  estimated change point of the median delay on the fan chart with a
  vertical line.

- New
  [`plot_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_drift.md)
  and
  [`test_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/test_delay_drift.md)
  to answer *“do reporting delay distributions drift over time?”*.

  - [`plot_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_drift.md)
    draws a rolling **fan chart** of the count-weighted delay
    distribution indexed by event date: a solid rolling median, a dashed
    rolling mean, and 25-75% / 10-90% quantile bands. The recent,
    not-yet-fully reported region (after the `level` incompleteness
    cutoff) is shaded grey so the truncation-induced dip is not mistaken
    for drift. Supports `by_strata`.
  - [`test_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/test_delay_drift.md)
    runs an **autocorrelation-robust monotonic-trend test** (Hamed-Rao
    modified Mann-Kendall by default, with Yue-Pilon and block-bootstrap
    options via the new `modifiedmk` *Suggests*) on the per-period delay
    summaries, testing both a location statistic (median/mean) and a
    dispersion statistic (IQR / 10-90 spread), on mature data only.
    Returns a tidy tibble with the Kendall tau, Sen’s slope, p-value and
    a `drift` verdict, per stat and stratum.

- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  gained a `by_strata` argument (default `FALSE`). When `TRUE`, every
  panel is split by stratum: the calendar and delay boxplots become
  dodged boxes (one per stratum, side by side), the epidemic process and
  both periodograms become one coloured line per stratum (no area fill),
  and the delay distribution becomes dodged bars. Boxplots are
  normalized **per stratum** (1 = that stratum’s own average) so the
  calendar pattern is comparable across strata, and strata are coloured
  with a `viridis` scale. A companion `strata` argument chooses which
  columns to group on (defaults to the object’s `strata`; pass a subset
  such as `strata = "gender"` to override).

- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  now draws **reporting-delay** diagnostic panels alongside the
  case-count ones, so you can see *delay effects*: the mean reporting
  delay by day of week / week of year / month (`delay_weekday`,
  `delay_week`, `delay_month`), and a periodogram of the mean-delay
  series (`delay_seasonality`) that reveals periodicity in the delay
  itself. The delay panels are computed on the complete part of the
  series (before the incompleteness line) so recent truncation does not
  bias them.

- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  gained a `panels` argument to choose which panels to draw. It accepts
  the concrete panel keys, or the aliases `"all"` (default),
  `"calendar"` and `"delay_calendar"`. Selecting a single panel returns
  it as a plain `ggplot2` object instead of a `patchwork`. Unknown
  panels error; panels that do not apply to the data’s time unit are
  skipped with a warning.

- New pkgdown article *“One dataset, many nowcasts”* now also
  demonstrates that temporal (delay) effect columns are carried into
  `epinowcast` (`metareference`/`metareport`), `baselinenowcast` (long)
  and `epidist`, with a table clarifying which target formats can hold
  covariates and how each model can use them.

- [`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md)
  gained an **after-holiday** and **after-weekend** effect via the new
  `holiday_lags` and `weekend_lags` arguments. Each takes a non-negative
  integer depth `N`; materialising the spec then adds indicator columns
  `..._holiday_lag_1 … ..._holiday_lag_N` (and likewise
  `..._weekend_lag_k`) that flag dates falling exactly `k` **working
  days** after a holiday / weekend. Working days skip weekends and
  holidays, so the effect lands on the first day(s) back at work —
  designed to capture the rise in cases just after a holiday or weekend.
  `holiday_lags` requires a `holidays` calendar. The columns are picked
  up automatically by every `tbl_now_to_*()` converter (as covariate
  columns) and by `diseasenowcasting::nowcast()`.

- Documented and tested attaching temporal effects to the **report
  date** (in addition to the default event date) via
  `add_temporal_effects(x, spec, date_type = "report_date")`. Event- and
  report-date effects can coexist on the same `tbl_now`; both sets of
  columns (`.event_*` and `.report_*`) are carried through all
  converters.

## tbl.now 0.9.0

- Added
  [`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
  and [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
  methods for `tbl_now` with an opt-in `compute_temporal_effects`
  argument (default `FALSE`). Passing `compute_temporal_effects = TRUE`
  materialises the lazy
  [`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md)
  spec (holidays, Fourier terms, calendar effects) into columns before
  returning a plain `tibble` / `data.frame`; the input `tbl_now` is left
  unchanged. The default stays lazy on purpose, because `dplyr` relies
  on these coercions being cheap, non-materialising declassers
  internally
  (e.g. [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)).
- The `tbl_now_to_*()` converters now carry the (lazy) temporal-effect
  columns (holidays, Fourier seasonal terms, day-of-week / calendar
  effects) into the target format as covariate columns. The spec is
  materialised on demand via
  [`compute_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/compute_temporal_effects.md)
  at conversion time (the input `tbl_now` is left unchanged), and the
  columns are passed to `data.table`, `tsibble`, `baselinenowcast` long
  format, `epidist`, and `epinowcast` (where they appear in the
  observations and `metareference` tables for use in the reference
  module). The `baselinenowcast` reporting-triangle matrix still cannot
  hold them.
- Removed the `%>%` export and changed all the pipes to `|>`
- Refactored `converters.R` for readability (dplyr column operations
  instead of base indexing, full variable names, lintr-clean).
- The `tbl_now_to_*()` converters now keep the `covariates` and
  `is_censored` columns wherever the target format can hold them
  (`data.table`, `tsibble`, `baselinenowcast` long format, `epidist`
  linelist); the fixed modelling objects (`enw_preprocess_data`, the
  reporting-triangle matrix, the EpiNow2 series) still cannot carry
  them.
- Added S3 methods on the other packages’ coercion generics so they
  accept a `tbl_now` directly:
  [`as_epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_linelist_data.html),
  [`as_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.html),
  `as_tsibble()` and
  [`as.data.table()`](https://rdrr.io/pkg/data.table/man/as.data.table.html),
  each wrapping the matching `tbl_now_to_*()`.
- Fixed
  [`tbl_now_to_data_table()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_data_table.md)
  checking for `baselinenowcast` instead of `data.table`, and
  `tbl_now_to_baselinenowcast(format = "long")` no longer requiring
  `baselinenowcast` to be installed.

## tbl.now 0.8.0

- Modified the `update` as the `t_effect` argument was not doing
  anything.
- Fixed bug that errored `complete_zeroes` when `is_censored` was given.
- Removed explicit zeroes from the converters (`tbl_now_from_*`) as they
  are not necessary in `tbl_now`.
- Added
  [`censor_delays_above()`](https://rodrigozepeda.github.io/tbl.now/reference/censor_delays_above.md)
  to flag reports with an implausibly long delay as censored (their
  delay becomes an upper bound).
- Improved documentation and README
- Documented all internal functions with roxygen (`@keywords internal` +
  `@noRd`) and ensured every exported function has a `@return`.
- Homogenized `lifecycle` badges.
- Brought the `censor_delays_above` function from `diseasenowcasting` to
  `tbl_now`.
- [`tbl_now_from_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md)
  now accepts not only the raw long input but also a preprocessed
  `enw_preprocess_data` object or a fitted `epinowcast` object (grouping
  auto-detected), matching the format `epinowcast` uses for summaries
  and plots.
- `tbl_now_to_EpiNow2()` gained a `model` argument:
  `"estimate_infections"` (default, the single `date`/`confirm` series)
  and `"estimate_truncation"` (a list of report-date snapshots, the one
  EpiNow2 model that uses the report dimension). Documentation clarified
  accordingly.
- Fixed two converter
  [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) guards:
  [`tbl_now_to_data_table()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_data_table.md)
  checked for `baselinenowcast` instead of `data.table`, and
  `tbl_now_to_baselinenowcast(format = "long")` no longer requires
  `baselinenowcast` to be installed.

## tbl.now 0.7.5

- Bumped roxygen to version 8.0.0. This also resulted in updated
  documentation.
- Changed
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)’s
  default level to 0.95
- Added tests for converters and pillars.
- Throws warning when converting to `baselinenowcast` if data is
  `"count-cumulative"`.

## tbl.now 0.7.3

- Added the
  [`update_now()`](https://rodrigozepeda.github.io/tbl.now/reference/change.md)
  function to make it more intuitive to update the now.

## tbl.now 0.7.0

- Added an
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  method for `tbl_now` objects that produces a multi-panel diagnostic
  overview: the empirical delay distribution, the observed epidemic
  process with an incompleteness line (controlled by `level`),
  normalized calendar-effect boxplots (cases relative to the overall
  mean), and a periodogram to help choose Fourier `seasons`. Daily data
  shows both a day-of-week and a week-of-year boxplot panel; weekly data
  shows week-of-year. Built on `ggplot2` and `patchwork`. The x-axis
  limits of each panel can be set individually
  (`delay_distribution_xlim`, `event_date_xlim`, `calendar_effect_xlim`,
  `seasonality_xlim`), and holidays from the temporal-effects spec are
  marked with red dots on the epidemic process.
- Added converters to and from other packages, all of the form
  `tbl_now_from_*()` / `tbl_now_to_*()`: `epinowcast`,
  `baselinenowcast`, `EpiNow2` (to only), `epidist`, `data.table` and
  `tsibble`. The `tbl_now_from_*()` functions wrap
  [`as_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md)
  and forward `...` to
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md);
  the `tbl_now_to_*()` functions call into the target package. All
  accept a `verbose` argument that reports the choices made (the
  inferred `now`, data type, units, and column mapping).
- [`as_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md)
  gained methods for the classes produced by `tbl_now_to_*()`
  (`enw_preprocess_data`, `reporting_triangle`, `epidist_linelist_data`,
  `tbl_ts` and `data.table`), so a converted object can be turned
  straight back into a `tbl_now`.
- Documented
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  and the converters in the introduction vignette.

## tbl.now 0.6.4

- Fixed dependency on R \>= 4.2.0
- Update function now defaults the censoring to FALSE if the update is
  censored but the original is not.

## tbl.now 0.6.3

- Added season length to seasons so we can get weekly seasonality.

## tbl.now 0.6.2

- Removed warning when using columns for temporal effects that cascaded
  into `to_count`.
- Changed DESCRIPTION to fix ortographic error and trigger less messages
  of unknown words.

## tbl.now 0.6.1

- Changed links in description of `tidy-select`

## tbl.now 0.6.0

- Changed temporal effects to be lazy (as required by \#17) so that now
  its easier to use `dplyr` functions without compromising them.
- Bumped the deprecated dplyr’s `*_at` functions to use
  [`all_of()`](https://tidyselect.r-lib.org/reference/all_of.html)
- Fixed to no warnings during test.
- Users can now pass the `.delay` column directly (#6) and it will
  recalculate the missing column (i.e. event or report)
- Added `complete_zeroes` to vignette (#13).
