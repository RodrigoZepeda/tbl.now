# Package index

## The `tbl_now` class

Create one, check it, and move between the three data types.

- [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
  **\[experimental\]** :

  Create a `tbl_now` object

- [`as_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md)
  **\[experimental\]** :

  Transform an object into a `tbl_now`

- [`validate_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md)
  [`is_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md)
  **\[experimental\]** :

  Check that an object is a valid `tbl_now`

- [`tbl_now_attributes()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_attributes.md)
  **\[stable\]** :

  List what a `tbl_now` was told about itself

- [`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md)
  **\[stable\]** : Convert between linelist and aggregated count data

## Attributes

Read what the object was told about itself, and change it.

- [`get_event_date()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
  [`get_report_date()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
  [`get_strata()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
  [`get_num_strata()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
  [`get_covariates()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
  [`get_num_covariates()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
  [`get_now()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
  [`get_report_units()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
  [`get_event_units()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
  [`get_data_type()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
  [`get_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
  [`get_temporal_effect_cols()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
  [`get_is_censored()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
  [`get_case_count()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
  **\[experimental\]** :

  Read what a `tbl_now` was told about itself

- [`get_latest_reported_cases()`](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.md)
  [`get_initial_reported_cases()`](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.md)
  [`get_nth_reported_cases()`](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.md)
  **\[stable\]** : Reported cases at a chosen point in the reporting
  process

- [`get_confirmation_date()`](https://rodrigozepeda.github.io/tbl.now/reference/confirmation_getters.md)
  [`get_confirmation_type()`](https://rodrigozepeda.github.io/tbl.now/reference/confirmation_getters.md)
  [`get_confirmation_units()`](https://rodrigozepeda.github.io/tbl.now/reference/confirmation_getters.md)
  [`has_confirmation()`](https://rodrigozepeda.github.io/tbl.now/reference/confirmation_getters.md)
  **\[experimental\]** :

  Confirmation attributes of a `tbl_now`

- [`change_now()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
  [`update_now()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
  [`change_event_date()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
  [`change_report_date()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
  [`change_case_count()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
  [`change_is_censored()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
  [`remove_is_censored()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
  [`add_is_censored()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
  [`change_strata()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
  [`remove_strata()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
  [`add_strata()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
  [`remove_all_strata()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
  [`change_covariates()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
  [`remove_covariates()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
  [`add_covariates()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
  [`remove_all_covariates()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
  [`replace_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
  [`remove_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
  **\[stable\]** :

  Set, change and remove the attributes of a `tbl_now`

- [`add_confirmation()`](https://rodrigozepeda.github.io/tbl.now/reference/confirmation_setters.md)
  [`change_confirmation()`](https://rodrigozepeda.github.io/tbl.now/reference/confirmation_setters.md)
  [`remove_confirmation()`](https://rodrigozepeda.github.io/tbl.now/reference/confirmation_setters.md)
  **\[experimental\]** : Attach, change or drop a confirmation process

- [`update(`*`<tbl_now>`*`)`](https://rodrigozepeda.github.io/tbl.now/reference/update.tbl_now.md)
  **\[experimental\]** :

  Append newly arrived data to a `tbl_now`

## Reshaping

Put the data on the grid a model needs.

- [`align_weeks()`](https://rodrigozepeda.github.io/tbl.now/reference/align_weeks.md)
  [`week_2_date()`](https://rodrigozepeda.github.io/tbl.now/reference/align_weeks.md)
  **\[experimental\]** : Put weekly data on a common weekday
- [`complete_zeroes()`](https://rodrigozepeda.github.io/tbl.now/reference/complete_zeroes.md)
  **\[experimental\]** : Fill in the days when nothing was reported
- [`censor_delays_above()`](https://rodrigozepeda.github.io/tbl.now/reference/censor_delays_above.md)
  [`censor_confirmation_delays_above()`](https://rodrigozepeda.github.io/tbl.now/reference/censor_delays_above.md)
  **\[experimental\]** : Treat implausibly long delays as censored
  rather than exact
- [`is_weekday()`](https://rodrigozepeda.github.io/tbl.now/reference/is_weekday.md)
  **\[stable\]** : Is a date a weekday or a weekend?

## Temporal effects

Calendar structure, recorded lazily and materialised on demand.

- [`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md)
  **\[stable\]** : Calendar effects to include in a nowcast

- [`add_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.md)
  [`compute_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.md)
  **\[stable\]** :

  Attach calendar effects to a `tbl_now`, and turn them into columns

## The confirmation process

The optional third date, and the outcomes it carries.

- [`get_latest_confirmed()`](https://rodrigozepeda.github.io/tbl.now/reference/confirmation_counts.md)
  [`get_net_confirmed()`](https://rodrigozepeda.github.io/tbl.now/reference/confirmation_counts.md)
  [`get_nth_confirmed()`](https://rodrigozepeda.github.io/tbl.now/reference/confirmation_counts.md)
  [`get_initial_confirmed()`](https://rodrigozepeda.github.io/tbl.now/reference/confirmation_counts.md)
  **\[experimental\]** : Confirmed, retracted and net counts per event
  date
- [`diagnose_confirmation_delay()`](https://rodrigozepeda.github.io/tbl.now/reference/confirmation_delay.md)
  [`plot_confirmation_delay()`](https://rodrigozepeda.github.io/tbl.now/reference/confirmation_delay.md)
  **\[experimental\]** : Compare confirmation delays between confirmed
  and retracted cases

## Summarising

What is in the data. [`summary()`](https://rdrr.io/r/base/summary.html)
returns a tibble, and every block of it is also a function of its own.

- [`summary(`*`<tbl_now>`*`)`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_summary.md)
  **\[experimental\]** :

  Summarise a `tbl_now`

- [`cases_per_date()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md)
  [`delay_summary()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md)
  [`zero_run_summary()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md)
  [`prop_censored()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md)
  [`prop_confirmation_type()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md)
  [`prop_strata()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md)
  [`prop_covariate_levels()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md)
  [`case_autocorrelation()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md)
  [`date_ranges()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md)
  [`triangle_occupancy()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md)
  [`reporting_completeness()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md)
  [`cumulative_growth()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md)
  **\[experimental\]** :

  Individual blocks of a `tbl_now` summary

## Diagnosing

What is wrong with the data.
[`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
is structural and deterministic; the statistical tests it signposts are
listed after it.

- [`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
  **\[experimental\]** :

  Diagnose a `tbl_now`

- [`diagnose_declarations()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_diagnose_components.md)
  [`diagnose_ordering()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_diagnose_components.md)
  [`diagnose_missing()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_diagnose_components.md)
  [`diagnose_duplicates()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_diagnose_components.md)
  [`diagnose_units()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_diagnose_components.md)
  [`diagnose_negatives()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_diagnose_components.md)
  [`diagnose_now()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_diagnose_components.md)
  [`diagnose_truncation()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_diagnose_components.md)
  [`diagnose_strata()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_diagnose_components.md)
  [`diagnose_signposts()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_diagnose_components.md)
  **\[experimental\]** :

  Individual blocks of a `tbl_now` diagnosis

- [`diagnose_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_drift.md)
  **\[experimental\]** : Test whether the reporting-delay distribution
  drifts over time

- [`diagnose_changepoint()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_changepoint.md)
  **\[experimental\]** : Detect an abrupt change point in the
  reporting-delay distribution

- [`diagnose_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches.md)
  **\[experimental\]** : Screen the report axis for batched reporting

- [`diagnose_batch_shape()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batch_shape.md)
  **\[experimental\]** : Test whether one report date drew from
  unusually old event dates

- [`transport_discriminant()`](https://rodrigozepeda.github.io/tbl.now/reference/transport_discriminant.md)
  **\[experimental\]** : The transport discriminant of a reporting
  series

- [`simulate_batch()`](https://rodrigozepeda.github.io/tbl.now/reference/simulate_batch.md)
  **\[experimental\]** :

  Inject a batch into a `tbl_now` by withholding and then releasing
  reports

## Plots

One grid, or any panel of it on its own.

- [`autoplot(`*`<tbl_now>`*`)`](https://rodrigozepeda.github.io/tbl.now/reference/autoplot.tbl_now.md)
  **\[experimental\]** :

  Diagnostic `autoplot` for a `tbl_now`

- [`diagnostic_plot()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnostic_plot.md)
  **\[experimental\]** : Diagnostic plots of the reporting process

- [`plot_day_of_week_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/calendar_effect_plots.md)
  [`plot_week_of_year_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/calendar_effect_plots.md)
  [`plot_month_of_year_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/calendar_effect_plots.md)
  [`plot_holiday_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/calendar_effect_plots.md)
  [`plot_holiday_lag_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/calendar_effect_plots.md)
  **\[experimental\]** : Calendar effects on the case counts or on the
  reporting delay

- [`plot_confirmation_status()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_confirmation_status.md)
  **\[experimental\]** : How much of each day has been resolved

- [`plot_cycles()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_cycles.md)
  **\[experimental\]** : Periodogram of the case counts or of the
  reporting delay

- [`plot_delay_distribution()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_distribution.md)
  **\[experimental\]** : Empirical distribution of the reporting delay

- [`plot_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_drift.md)
  **\[experimental\]** : Visualise whether the reporting-delay
  distribution drifts over time

- [`plot_delay_profiles()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_profiles.md)
  **\[experimental\]** : Plot the per-date delay profiles

- [`plot_reporting_process()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_epidemic_process.md)
  [`plot_epidemic_process()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_epidemic_process.md)
  **\[experimental\]** : The epidemic process and the reporting process

- [`plot_observed_cases()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_observed_cases.md)
  **\[experimental\]** : Observed epidemic process with the
  incompleteness line

- [`plot_reporting_hexamap()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_reporting_hexamap.md)
  **\[experimental\]** : Plot the reporting triangle as an
  age-period-cohort hexamap

- [`plot_reporting_triangle()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_reporting_triangle.md)
  **\[experimental\]** : Plot the reporting triangle

- [`plot_scalogram()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_scalogram.md)
  **\[experimental\]** : Plot the reporting or epidemic scalogram

- [`plot_transport_discriminant()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_transport_discriminant.md)
  **\[experimental\]** : Plot the transport-discriminant plane

## Fitting nowcasts

One interface over the modelling packages.

- [`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
  **\[experimental\]** :

  Nowcast a `tbl_now` with any supported modelling package

- [`engine()`](https://rodrigozepeda.github.io/tbl.now/reference/engine.md)
  **\[experimental\]** : Specify a nowcasting model and its arguments

- [`engine_diseasenowcasting()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md)
  [`engine_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md)
  [`engine_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md)
  [`engine_nobbs()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md)
  [`engine_surveillance()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md)
  [`engine_epinow2()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md)
  **\[experimental\]** : Engines for the built-in nowcasting packages

- [`example_engine()`](https://rodrigozepeda.github.io/tbl.now/reference/example_engine.md)
  [`nowcast_fit(`*`<example>`*`)`](https://rodrigozepeda.github.io/tbl.now/reference/example_engine.md)
  [`nowcast_tidy(`*`<example>`*`)`](https://rodrigozepeda.github.io/tbl.now/reference/example_engine.md)
  **\[experimental\]** : A toy engine for examples

- [`is_nowcast_engine()`](https://rodrigozepeda.github.io/tbl.now/reference/is_nowcast_engine.md)
  **\[experimental\]** : Is this an engine?

- [`list_nowcast_methods()`](https://rodrigozepeda.github.io/tbl.now/reference/list_nowcast_methods.md)
  **\[experimental\]** : List the available nowcasting methods

- [`nowcast_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_fit.md)
  **\[experimental\]** : Fit a nowcast with one modelling package

- [`nowcast_tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_tidy.md)
  **\[experimental\]** : Standardise a fitted nowcast

- [`tbl_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_nowcast.md)
  **\[experimental\]** :

  A nowcast produced by
  [`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)

- [`is_tbl_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/is_tbl_nowcast.md)
  **\[experimental\]** :

  Is this object a `tbl_nowcast`?

- [`nowcast_quantile_levels()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_quantile_levels.md)
  **\[experimental\]** : Default quantile levels for a nowcast

- [`autoplot.tbl_nowcast`](https://rodrigozepeda.github.io/tbl.now/reference/autoplot.tbl_nowcast.md)
  **\[experimental\]** : Plot a nowcast

- [`as_tibble.tbl_nowcast`](https://rodrigozepeda.github.io/tbl.now/reference/as_tibble.tbl_nowcast.md)
  [`as_tibble_tbl_nowcast`](https://rodrigozepeda.github.io/tbl.now/reference/as_tibble.tbl_nowcast.md)
  **\[experimental\]** :

  Coerce a `tbl_nowcast` into a `tibble`

## Ensembles, backtests and scoring

- [`nowcast_ensemble()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_ensemble.md)
  **\[experimental\]** : Combine several nowcasts into an ensemble

- [`nowcast_weights()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_weights.md)
  **\[experimental\]** : Ensemble weights from a backtest

- [`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md)
  **\[experimental\]** :

  Refit several methods at past `now` dates and score them

- [`score_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/score_nowcast.md)
  [`as_scoringutils()`](https://rodrigozepeda.github.io/tbl.now/reference/score_nowcast.md)
  **\[experimental\]** : Score a nowcast against observed data

## Converters

Out to the modelling packages, and back again.

- [`tbl_now_to_EpiNow2()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_EpiNow2.md)
  [`tbl_now_from_EpiNow2()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_EpiNow2.md)
  **\[experimental\]** :

  Convert between `tbl_now` and EpiNow2

- [`tbl_now_from_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_baselinenowcast.md)
  [`tbl_now_to_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_baselinenowcast.md)
  **\[experimental\]** :

  Convert between `tbl_now` and baselinenowcast

- [`tbl_now_from_data_table()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_data_table.md)
  [`tbl_now_to_data_table()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_data_table.md)
  **\[experimental\]** :

  Convert between `tbl_now` and data.table

- [`tbl_now_from_epidist()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epidist.md)
  [`tbl_now_to_epidist()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epidist.md)
  **\[experimental\]** :

  Convert between `tbl_now` and epidist

- [`tbl_now_from_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md)
  [`tbl_now_to_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md)
  **\[experimental\]** :

  Convert between `tbl_now` and epinowcast

- [`tbl_now_to_nobbs()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_nobbs.md)
  **\[experimental\]** :

  Convert a `tbl_now` into the line list NobBS nowcasts from

- [`tbl_now_to_surveillance()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_surveillance.md)
  **\[experimental\]** :

  Convert a `tbl_now` into the line list surveillance nowcasts from

- [`tbl_now_from_tsibble()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_tsibble.md)
  [`tbl_now_to_tsibble()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_tsibble.md)
  **\[experimental\]** :

  Convert between `tbl_now` and tsibble

- [`as_epidist_linelist_data(`*`<tbl_now>`*`)`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_coercion_methods.md)
  [`as_epidist_aggregate_data(`*`<tbl_now>`*`)`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_coercion_methods.md)
  [`as_reporting_triangle(`*`<tbl_now>`*`)`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_coercion_methods.md)
  [`as_tsibble(`*`<tbl_now>`*`)`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_coercion_methods.md)
  [`as.data.table(`*`<tbl_now>`*`)`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_coercion_methods.md)
  **\[experimental\]** :

  Coerce a `tbl_now` with another package's generic

- [`print(`*`<tbl_now_epinow2_snapshots>`*`)`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinow2_snapshots.md)
  **\[experimental\]** :

  Snapshots of one series, as EpiNow2 estimates truncation from

- [`print(`*`<tbl_now_triangle_list>`*`)`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_triangle_list.md)
  **\[experimental\]** : One reporting triangle per stratum

- [`print(`*`<tbl_now_surveillance_list>`*`)`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_surveillance_list.md)
  **\[experimental\]** :

  One surveillance line list per stratum

- [`get_surveillance_when()`](https://rodrigozepeda.github.io/tbl.now/reference/surveillance_grids.md)
  [`get_surveillance_range()`](https://rodrigozepeda.github.io/tbl.now/reference/surveillance_grids.md)
  **\[experimental\]** :

  The date grids
  [`surveillance::nowcast()`](https://rdrr.io/pkg/surveillance/man/nowcast.html)
  needs

## Tidying model output

Every engine’s result, in one shape.

- [`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
  **\[experimental\]** : Tidy a fitted nowcast into one standard table

- [`tidy.tbl_nowcast`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.tbl_nowcast.md)
  [`tidy_tbl_nowcast`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.tbl_nowcast.md)
  **\[experimental\]** :

  Tidy a nowcast produced by
  [`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
  or
  [`nowcast_ensemble()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_ensemble.md)

- [`tidy(`*`<nowcast_backtest>`*`)`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast_backtest.md)
  **\[experimental\]** :

  Tidy the scores of a
  [`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md)

- [`tidy(`*`<epidist_fit>`*`)`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.epidist_fit.md)
  **\[experimental\]** :

  Tidy the delay distribution from an epidist fit

- [`tidy(`*`<estimate_dist>`*`)`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.estimate_dist.md)
  **\[experimental\]** :

  Tidy the delay distribution from an EpiNow2 `estimate_dist()` fit

## dplyr integration

The methods that keep the class alive inside a pipeline.

- [`as_tibble(`*`<tbl_now>`*`)`](https://rodrigozepeda.github.io/tbl.now/reference/as_tibble.tbl_now.md)
  [`as_tibble(`*`<grouped_tbl_now>`*`)`](https://rodrigozepeda.github.io/tbl.now/reference/as_tibble.tbl_now.md)
  [`as.data.frame(`*`<tbl_now>`*`)`](https://rodrigozepeda.github.io/tbl.now/reference/as_tibble.tbl_now.md)
  [`as.data.frame(`*`<grouped_tbl_now>`*`)`](https://rodrigozepeda.github.io/tbl.now/reference/as_tibble.tbl_now.md)
  **\[experimental\]** :

  Coerce a `tbl_now` to a tibble or a data frame

- [`` `[`( ``*`<tbl_now>`*`)`](https://rodrigozepeda.github.io/tbl.now/reference/assign_tbl.md)
  [`` `[`( ``*`<grouped_tbl_now>`*`)`](https://rodrigozepeda.github.io/tbl.now/reference/assign_tbl.md)
  [`` `names<-`( ``*`<tbl_now>`*`)`](https://rodrigozepeda.github.io/tbl.now/reference/assign_tbl.md)
  [`` `names<-`( ``*`<grouped_tbl_now>`*`)`](https://rodrigozepeda.github.io/tbl.now/reference/assign_tbl.md)
  [`` `$<-`( ``*`<tbl_now>`*`)`](https://rodrigozepeda.github.io/tbl.now/reference/assign_tbl.md)
  [`` `$<-`( ``*`<grouped_tbl_now>`*`)`](https://rodrigozepeda.github.io/tbl.now/reference/assign_tbl.md)
  **\[stable\]** :

  Base R operations on a `tbl_now`

## Datasets

- [`covid_colombia`](https://rodrigozepeda.github.io/tbl.now/reference/covid_colombia.md)
  : COVID-19 Notifications – Colombia 2020-2023
- [`covid_us`](https://rodrigozepeda.github.io/tbl.now/reference/covid_us.md)
  : covid_us: CDC COVID-19 Case Surveillance Public Use Data (2020-2021)
- [`denguedat`](https://rodrigozepeda.github.io/tbl.now/reference/denguedat.md)
  : denguedat: Dengue fever individual-level reporting data from Puerto
  Rico
- [`flusight`](https://rodrigozepeda.github.io/tbl.now/reference/flusight.md)
  : flusight: NHSN Weekly Hospital Respiratory Data from FluSight
- [`hai_bucaramanga`](https://rodrigozepeda.github.io/tbl.now/reference/hai_bucaramanga.md)
  : Healthcare-Associated Infections – Bucaramanga, Colombia 2016-2023
- [`mpoxdat`](https://rodrigozepeda.github.io/tbl.now/reference/mpoxdat.md)
  : mpoxdat: Mpox reporting data from the 2022 New York City outbreak
