# Changelog

## tbl.now 0.8.1

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
  accept a `tbl_now` directly: `as_epidist_linelist_data()`,
  `as_reporting_triangle()`, `as_tsibble()` and
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
