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
* Removed the `%>%` export and changed all the pipes to `|>`
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
