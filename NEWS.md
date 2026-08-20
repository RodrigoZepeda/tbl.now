# tbl.now 0.15.1

* Two new converters, bringing the supported nowcasting back-ends to seven:
  * `tbl_now_to_surveillance()` builds the individual-level line list
    [surveillance::nowcast()] works from, renaming the event and report dates to
    \pkg{surveillance}'s own `dHospital` / `dReport` defaults. `format = "sts"`
    instead returns the observed curve as a `surveillance` `sts` object.
  * `tbl_now_to_nowcaster()` builds the line list
    `nowcaster::nowcasting_inla()` expects, renaming the dates to `date_onset` /
    `date_report`. Note that `nowcasting_inla()` takes those two as *bare*
    column names (tidy-evaluation), not strings.

  Both accept count data as well as line lists, expanding counts back to one row
  per case (de-accumulating first when the data is cumulative). \pkg{surveillance}
  and \pkg{nowcaster} are new `Suggests`; \pkg{nowcaster} is not on CRAN, so the
  covid19br r-universe was added to `Additional_repositories`.
* The nowcasting-models article gained sections for both packages and a closing
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
* Added `censor_delays_above()` to flag reports with an implausibly long delay
as censored (their delay becomes an upper bound).
* Improved documentation and README
* Documented all internal functions with roxygen (`@keywords internal` + `@noRd`)
and ensured every exported function has a `@return`.
* Homogenized `lifecycle` badges. 
* Brought the `censor_delays_above` function from `diseasenowcasting` to `tbl_now`. 
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
