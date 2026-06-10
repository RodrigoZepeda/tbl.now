# Changelog

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
