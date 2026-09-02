# tbl.now: Tidy Extension for Nowcasting

Provides a 'tbl_now' class for temporal data for performing nowcasts.
The 'tbl_now' allows for easy manipulation of event dates, report dates,
strata and covariates for implementing different nowcasting models while
at the same time permitting ease of use with 'dplyr' functions.

## Details

Surveillance data arrives late. A case that happened on Monday may not
reach the system until Thursday, so the most recent counts always look
lower than they will turn out to be. **Nowcasting** corrects that: it
estimates how many cases have already happened but have not been
reported yet.

`tbl.now` is the tidy scaffolding around that problem. You declare which
columns hold the event date, the report date and anything else that
matters once, and everything else – describing, diagnosing, plotting,
fitting, scoring – follows from that declaration.

## The workflow

1.  **Declare.**
    [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
    turns a `data.frame` into a `tbl_now`, or
    [`as_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md)
    converts an object from another nowcasting package. The result is
    still a `tibble`, so `dplyr` keeps working.

2.  **Describe.**
    [summary()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_summary.md)
    says what is in the data;
    [autoplot()](https://rodrigozepeda.github.io/tbl.now/reference/autoplot.tbl_now.md)
    draws it.

3.  **Diagnose.**
    [`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
    says what is *wrong* with it, and points at the statistical tests
    worth running –
    [`diagnose_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_drift.md)
    for delays that are getting longer,
    [`diagnose_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches.md)
    for backlog releases,
    [`diagnostic_plot()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnostic_plot.md)
    for the reporting process as a picture.

4.  **Reshape.**
    [`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md),
    [`complete_zeroes()`](https://rodrigozepeda.github.io/tbl.now/reference/complete_zeroes.md),
    [`aggregate_time_units()`](https://rodrigozepeda.github.io/tbl.now/reference/aggregate_time_units.md),
    [`align_weeks()`](https://rodrigozepeda.github.io/tbl.now/reference/align_weeks.md)
    and
    [censor_reports()](https://rodrigozepeda.github.io/tbl.now/reference/censoring.md)
    put the data on the grid a model needs.

5.  **Fit.**
    [`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
    takes the data and an
    [`engine()`](https://rodrigozepeda.github.io/tbl.now/reference/engine.md)
    – one interface over epinowcast, baselinenowcast, NobBS, EpiNow2,
    surveillance and diseasenowcasting. Write your own with
    [`nowcast_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_fit.md)
    and
    [`nowcast_tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_tidy.md).

6.  **Check.**
    [`score_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/score_nowcast.md)
    and
    [`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md)
    say whether the nowcast was any good;
    [`nowcast_ensemble()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_ensemble.md)
    combines several.

[`vignette("tbl.now")`](https://rodrigozepeda.github.io/tbl.now/articles/tbl.now.md)
walks through this end to end. The [package
website](https://rodrigozepeda.github.io/tbl.now/) carries longer
articles on the modelling packages, batch reporting, ensembles and
writing your own backend.

## Datasets

Six surveillance datasets ship with the package for experimenting:
[denguedat](https://rodrigozepeda.github.io/tbl.now/reference/denguedat.md),
[mpoxdat](https://rodrigozepeda.github.io/tbl.now/reference/mpoxdat.md),
[flusight](https://rodrigozepeda.github.io/tbl.now/reference/flusight.md),
[covid_colombia](https://rodrigozepeda.github.io/tbl.now/reference/covid_colombia.md),
[covid_us](https://rodrigozepeda.github.io/tbl.now/reference/covid_us.md)
and
[hai_bucaramanga](https://rodrigozepeda.github.io/tbl.now/reference/hai_bucaramanga.md)
– the last deliberately messy, for the diagnostics.

## See also

Useful links:

- <https://rodrigozepeda.github.io/tbl.now/>

## Author

**Maintainer**: Rodrigo Zepeda-Tello <rzepeda17@gmail.com>
([ORCID](https://orcid.org/0000-0003-4471-5270))

Authors:

- Rodrigo Zepeda-Tello <rzepeda17@gmail.com>
  ([ORCID](https://orcid.org/0000-0003-4471-5270))

- Rami Yaari ([ORCID](https://orcid.org/0000-0002-8808-8937))

- Matteo Perini ([ORCID](https://orcid.org/0000-0002-9465-6216))

Other contributors:

- Teresa Yamana ([ORCID](https://orcid.org/0000-0001-8349-3151))
  \[contributor\]

- Jeffrey Shaman \[contributor\]

- Columbia University in the City of New York \[copyright holder,
  funder\]
