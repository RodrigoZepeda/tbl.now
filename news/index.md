# Changelog

## tbl.now 0.29.0

### Breaking: `is_censored` is now `is_censored_report` (#54)

There are two censoring axes now, so the unqualified name had to go. The
old spelling is removed outright, not deprecated:

| was | is |
|----|----|
| `tbl_now(is_censored = )` | `tbl_now(is_censored_report = )` |
| `get_is_censored()` | [`get_is_censored_report()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md) |
| `add_is_censored()`, `change_is_censored()`, `remove_is_censored()` | [`add_is_censored_report()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md), [`change_is_censored_report()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md), [`remove_is_censored_report()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md) |
| `is_censored` attribute | `is_censored_report` attribute |
| `.is_censored` (the column [`censor_delays_above()`](https://rodrigozepeda.github.io/tbl.now/reference/censor_delays_above.md) creates) | `.is_censored_report` |

### New: `is_censored_validation`, the validation-axis censoring flag (#53)

The twin of `is_censored_report`, for models that use censored
validation delays. It marks rows whose time from report to resolution is
a **bound** rather than a measurement.

- `tbl_now(is_censored_validation = )`,
  [`get_is_censored_validation()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md),
  [`add_is_censored_validation()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md),
  [`change_is_censored_validation()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md),
  [`remove_is_censored_validation()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md).
  It requires a `validation_date`: there is no validation delay to bound
  without one.
- The column is protected, is carried through every `dplyr` verb and
  through
  [`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md),
  [`update()`](https://rdrr.io/r/stats/update.html) and
  [`align_weeks()`](https://rodrigozepeda.github.io/tbl.now/reference/align_weeks.md),
  and joins the grouping keys – a censored resolution and an exact one
  on the same `(event, report, outcome)` triple stay two rows rather
  than being summed into one.

#### Breaking: `censor_validation_delays_above()` flags instead of erasing

It used to set the offending rows’ `validation_type` to `"pending"` and
delete their validation date. That was wrong: a case confirmed after 200
days is still a confirmed case, and the object should say so. It now
sets `is_censored_validation` and leaves the date and the outcome alone,
exactly as
[`censor_delays_above()`](https://rodrigozepeda.github.io/tbl.now/reference/censor_delays_above.md)
does on the report axis.
[`get_latest_confirmed()`](https://rodrigozepeda.github.io/tbl.now/reference/validation_counts.md)
therefore still counts those cases.

### New: `validation_levels`, for data not recorded in English (#54)

`validation_type` may hold only `"confirmed"`, `"retracted"`,
`"pending"` or `NA` – that was already enforced, and the error now names
the way out. `tbl_now(validation_levels = )` is that way out: a named
dictionary whose names are the labels in your data and whose values are
the canonical four.

``` r

tbl_now(casos,
  validation_type   = desenlace,
  validation_levels = c(
    confirmado = "confirmed", retractado = "retracted", pendiente = "pending"
  ),
  ...
)
```

The column is rewritten to the canonical values; the dictionary is kept
on the object and read back with
[`get_validation_levels()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md).
A dictionary that would recode a canonical value into a different one is
refused, because it would flip the column on every rebuild.

### Fixed: `change_now()` re-censors instead of erroring (#51)

Moving `now` **backwards** is what
[`change_now()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
is for – it is how a backtest walks through time. On an object carrying
a validation process it aborted for every `now` earlier than the last
validation, which is nearly every historical as-of date.

It now masks validations dated after the new `now`: the validation date
becomes `NA` and the outcome returns to `"pending"`, because a
resolution that has not happened yet is not a resolution.
[`change_now()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
and
[`update_now()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
gain `verbose` to silence the report of how many rows were masked.

### `covid_us` carries a validation process (#52)

No shipped dataset had one, so every example fabricated an outcome by
row position. `covid_us` is rebuilt from the same CDC source with the
two date columns that were being left on the floor, and it now runs
onset -\> positive specimen -\> registration at CDC:

| was | is |
|----|----|
| `cdc_case_earliest_dt`, `cdc_report_dt`, `n` (2020-2021) | `onset_dt`, `pos_spec_dt`, `cdc_report_dt`, `current_status`, `sex`, `n` (2020) |

`cdc_case_earliest_dt` is CDC-derived and equals `onset_dt` for 99.997%
of the rows kept, so it is gone as redundant; `sex` is a stratum, and
`current_status` is the validation outcome – in CDC’s own words, so that
mapping it is a worked example of `validation_levels`. The relationship
between outcome and validation delay is real rather than fabricated:
probable cases are registered a median of 2 days after the specimen,
laboratory-confirmed ones 4 days. CDC does not withdraw cases, so
`"retracted"` does not occur.

## tbl.now 0.28.0

### Breaking: the confirmation process is now the validation process

The optional third date a `tbl_now` can carry is called a **validation**
rather than a confirmation, throughout. The old spelling is gone, not
deprecated – it had not shipped.

| was | is |
|----|----|
| `add_confirmation()`, `change_confirmation()`, `remove_confirmation()` | [`add_validation_date()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md), [`change_validation_date()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md), [`remove_validation_date()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md) |
| `get_confirmation_date()`, `get_confirmation_type()`, `get_confirmation_units()`, `has_confirmation()` | [`get_validation_date()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md), [`get_validation_type()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md), [`get_validation_units()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md), [`has_validation()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md) |
| `confirmation_counts`, `confirmation_delay` | `validation_counts`, `validation_delay` |
| `censor_confirmation_delays_above()`, `diagnose_confirmation_delay()` | [`censor_validation_delays_above()`](https://rodrigozepeda.github.io/tbl.now/reference/censor_delays_above.md), [`diagnose_validation_delay()`](https://rodrigozepeda.github.io/tbl.now/reference/validation_delay.md) |
| `plot_confirmation_delay()`, `plot_confirmation_status()`, `prop_confirmation_type()` | [`plot_validation_delay()`](https://rodrigozepeda.github.io/tbl.now/reference/validation_delay.md), [`plot_validation_status()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_validation_status.md), [`prop_validation_type()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md) |
| `confirmation_date`, `confirmation_type`, `confirmation_units` arguments | `validation_date`, `validation_type`, `validation_units` |
| `.confirmation_num`, `.confirmation_delay` columns | `.validation_num`, `.validation_delay` |
| `axis = "confirmation"` | `axis = "validation"` |
| `"event_to_confirmation"`, `"report_to_confirmation"` | `"event_to_validation"`, `"report_to_validation"` |

The **outcome values are unchanged**: a case is still `"confirmed"`,
`"retracted"` or `"pending"`. Validation is what the process does;
confirmed is one of the things it can conclude.

`diseasenowcasting::confirmation_process()` is that package’s name and
is untouched – `model(confirmation = confirmation_process())` still
reads exactly as it did.

### Documentation: fewer, fuller reference pages

- The validation getters now live on
  [`?nowcast_data_getters`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md),
  next to
  [`get_event_date()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md),
  and the validation setters on
  [`?add`](https://rodrigozepeda.github.io/tbl.now/reference/add.md),
  next to
  [`change_event_date()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md).
  Someone asking “what did this object record, and how do I change it”
  now finds every answer on one page instead of four.
- *Describing and diagnosing a tbl_now* and *Diagnosing reporting
  batches* are now **one article**, [*Diagnosing a
  tbl_now*](https://rodrigozepeda.github.io/tbl.now/articles/diagnosing-a-tbl-now.html),
  running structure-first: what is in the data
  ([`summary()`](https://rdrr.io/r/base/summary.html)), what is
  structurally wrong with it
  ([`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)),
  and then the statistical tests
  [`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
  signposts but refuses to run.
- The attribute diagrams in the README now appear on the pkgdown site.
  They lived in `inst/figures/`, which pkgdown does not copy;
  `man/figures/` is the directory it publishes, and GitHub renders it
  just as happily.
- [`summary()`](https://rdrr.io/r/base/summary.html)’s `"completeness"`
  and `"growth"` rows are distributions over event dates, so they
  populate `mean`/`sd`/the quantiles (and, for completeness, `prop`) and
  leave the scalar `value` column empty. The documented examples
  selected `value` and got a column of `NA`; they now select the columns
  that carry the answer.

### Fixed: `baselinenowcast` on a snapshot (“as of”) series

A snapshot stream restates the whole history in every snapshot, so its
delay axis is as long as the series itself and the reporting triangle
comes out square. `baselinenowcast` needs more reference dates than
delay columns – it spends `max_delay` of them estimating the delay
distribution and keeps two back for the uncertainty model – so it
refused, with a message about reference-time arithmetic that mentioned
neither the delay axis nor anything to do about it. Three of the six
shipped datasets are that shape.

- [`engine_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md)
  gains **`max_delay`**, the number of delay periods to keep, forwarded
  to
  [`tbl_now_to_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_baselinenowcast.md).
  [`?run_nowcast`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
  already documented it (“`max_delay` caps the triangle’s width”); what
  actually happened is that it fell into `...` and reached the modelling
  call, which has no such argument and ignored it.

- A triangle too wide to fit is now refused by `tbl.now`, naming the
  delay axis and a concrete cap – the delay covering 99% of the reported
  cases:

  ``` r

  run_nowcast(x, engine_baselinenowcast(max_delay = 21))
  ```

Note that a snapshot series must be **declared**
`data_type = "count-cumulative"`. `infer_data_type()` reads a single
downward revision as incidence, by design, and a revised running total
has them; left to the inference, every delay carries a whole period’s
count instead of an increment and nothing downstream can tell.

### New: `diagnose()` and `summary()` print as reports

Both still return the tibbles they always returned, and every `dplyr`
verb still works on them. What changed is what you see when you print
one.

- [`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
  – and each of its blocks – prints the errors, warnings and notes in
  full, each with its hint, and counts the checks that passed, that were
  deliberately not run, and that could not be assessed.
  `print(x, all = TRUE)` spells those out too.
- [`summary()`](https://rdrr.io/r/base/summary.html) – and each of its
  blocks – prints one block per component, dropping the columns that
  component does not populate. The schema is wide because it holds every
  block at once; no block fills more than a handful of it.
- [`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
  gives the plain table back in both cases.

### New: a nowcast prints its value at the `now` edge

[`print()`](https://rdrr.io/r/base/print.html) on a `tbl_nowcast` now
leads with the number it was fitted to produce – the estimate and
interval at the last event date it covers, one line per stratum – before
the quantile table, which starts at the oldest event date.

## tbl.now 0.27.0

### Breaking: a nowcast is specified with an `engine()`

[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
and
[`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md)
used to take a method **name** plus a `...` (and, for the backtest, a
`method_args` list of lists keyed by label). Both failed the same silent
way: an argument that missed its backend simply vanished, and you got a
fitted model at its default with nothing on the object to say so.

An **engine** is one modelling package plus every argument it needs:

``` r

run_nowcast(x, engine_nobbs(max_D = 10, moving_window = 64))

nowcast_backtest(x,
  engine_baselinenowcast(draws = 1000),
  engine_nobbs(max_D = 10),
  now_dates = dates, seed = 20260824
)
```

- One constructor per supported package –
  [`engine_diseasenowcasting()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md),
  [`engine_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md),
  [`engine_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md),
  [`engine_nobbs()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md),
  [`engine_surveillance()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md),
  [`engine_epinow2()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md)
  – each **naming** that package’s own arguments, so they are visible in
  the signature and a typo is an error at the call rather than a default
  nobody notices. `...` still carries anything a named argument does not
  cover.
- `engine(method, ...)` is the general constructor and works for any
  registered method, including a backend you wrote yourself.
- **The data and `verbose` are the only arguments outside the engine.**
  `quantile_levels` moved onto it, because for `NobBS` it is a
  *fit-time* model argument (it lands in `specs$quantiles`, and NobBS
  keeps no draws, so a level it was never asked for cannot be recovered)
  rather than a way of summarising afterwards.
- `nowcast_backtest(x, ...)` now takes the engines **variadically**, or
  as one list. `methods` and `method_args` are gone. An engine’s `label`
  is its name in the result; labels must be unique, and every engine
  must report the **same** quantile levels – the WIS averages over the
  levels reported, so mismatched engines are not scoring the same
  quantity.
- `nowcast_method()` is **removed**. The engine is the object
  [`nowcast_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_fit.md)
  and
  [`nowcast_tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_tidy.md)
  dispatch on, so an existing backend needs no change; write
  `engine("mymodel")` where you wrote `nowcast_method("mymodel")`.
- A bare method string is an error that names the constructor to use.

### New: `min_date`, per engine

Every engine takes `min_date`, saying how much history to fit on:

| `min_date` | means |
|----|----|
| `NULL` (default) | the whole series |
| a `Date` | keep event dates on or after it |
| a number | keep the last *n* periods before `now`, in the object’s own units |

It is per engine on purpose. `baselinenowcast` and `diseasenowcasting`
take a long series in their stride, while `epinowcast` and `EpiNow2`
scale with the number of reference dates and are best given a window –
so one global
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html) over all
of them was the wrong tool.

Prefer the **number** inside a
[`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md):
`now` moves between fits, so a fixed calendar cut makes the fitted
window grow as the backtest walks forward and the last fit is trained on
more data than the first.

`min_date` trims the **event axis**, not `now`, and the trimmed object
is what the result carries – so
[`score_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/score_nowcast.md)
and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)’s
reported counts describe the series the model was actually shown.

### Breaking: `score_nowcast()` / `as_scoringutils()` take a `tbl_now` as `truth`

`observed_col` is **removed**, and a plain data frame is no longer
accepted. The `tbl_now` already knows which column holds the observed
counts – it is
[`get_case_count()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md),
or the count
[`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md)
produces from a line list – so naming it was a burden on the caller and
the old default (*“the last column that is neither the event date nor a
stratum”*) was a guess that could mis-score silently.

``` r

score_nowcast(nc, truth = dengue)     # the FULL tbl_now, line list or counts
as_scoringutils(nc, truth = dengue)
```

### Breaking: argument names made consistent

A documentation audit read every exported function and found the same
argument wearing different names in different places. 116 of the 148
exports already took `x` first; these were the exceptions.

- **[`nowcast_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_fit.md)
  and
  [`nowcast_tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_tidy.md)
  take `engine`, not `method`.** This is the one that affects code
  outside the package: if you wrote a backend, rename the first argument
  of your methods.

  ``` r

  # before
  nowcast_fit.mymodel  <- function(method, x, ..., quantile_levels, verbose) { }
  nowcast_tidy.mymodel <- function(method, fit, x, ..., quantile_levels) { }

  # after
  nowcast_fit.mymodel  <- function(engine, x, ..., quantile_levels, verbose) { }
  nowcast_tidy.mymodel <- function(engine, fit, x, ..., quantile_levels) { }
  ```

  What arrives has always been the engine –
  [`engine()`](https://rodrigozepeda.github.io/tbl.now/reference/engine.md)’s
  own documentation defines an engine as “the object
  [`nowcast_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_fit.md)
  and
  [`nowcast_tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_tidy.md)
  dispatch on” – and the old name was left over from the removed
  `nowcast_method()`. The argument is only a dispatch handle, so no
  method body needed changing; `R CMD check`’s S3 consistency check will
  flag yours until you rename it.

  `engine(method = )` and
  [`list_nowcast_methods()`](https://rodrigozepeda.github.io/tbl.now/reference/list_nowcast_methods.md)
  **keep** “method”, where it correctly means the *name* of a backend
  rather than a configured engine.

- **`data` becomes `x`** in
  [`diagnose_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches.md),
  [`diagnose_batch_shape()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batch_shape.md),
  [`simulate_batch()`](https://rodrigozepeda.github.io/tbl.now/reference/simulate_batch.md),
  [`transport_discriminant()`](https://rodrigozepeda.github.io/tbl.now/reference/transport_discriminant.md),
  [`censor_delays_above()`](https://rodrigozepeda.github.io/tbl.now/reference/censor_delays_above.md)
  and `censor_confirmation_delays_above()`. Positional calls are
  unaffected. Two internal helpers also named `data` in their error
  messages, so `diagnose_batches(x = <not a tbl_now>)` used to complain
  about an argument that did not exist.

- **`quiet` becomes `verbose`** in
  [`censor_delays_above()`](https://rodrigozepeda.github.io/tbl.now/reference/censor_delays_above.md)
  and `censor_confirmation_delays_above()`, with the sense inverted and
  defaulting to `TRUE`, matching the twenty other functions that control
  messaging this way. Write `verbose = FALSE` where you wrote
  `quiet = TRUE`.

  The converters that carry **both** `verbose` and `quiet` keep both:
  they are different channels – `verbose` is the conversion summary,
  `quiet` is the lossy-conversion warning – and the documentation now
  says so.

### Breaking: `align_weeks()` numbers weekdays the ISO way

`align_weeks(align_on_day = )` counted weekdays from Sunday while
`is_weekday(weekend_days = )` counted them from Monday.
[`align_weeks()`](https://rodrigozepeda.github.io/tbl.now/reference/align_weeks.md)
now uses ISO numbering too – **1 = Monday … 7 = Sunday** – so the two
agree.
[`is_weekday()`](https://rodrigozepeda.github.io/tbl.now/reference/is_weekday.md)
is unchanged.

**The default is unchanged.** It becomes `7`, which is still Sunday, so
`align_weeks(x)` – and `tbl_now(..., align_weeks = TRUE)`, which is
where nearly everyone meets it – behaves exactly as before. Only an
explicit `align_on_day` changes meaning, and the migration is to
subtract one, wrapping `1` to `7`:

| you wrote | you meant | now write |
|-----------|-----------|-----------|
| `1`       | Sunday    | `7`       |
| `2`       | Monday    | `1`       |
| `3`       | Tuesday   | `2`       |
| …         | …         | …         |
| `7`       | Saturday  | `6`       |

### New: `example_engine()`, a toy engine for examples

Every real engine needs its modelling package, so every example that
fitted a nowcast sat inside `\donttest{}` behind a
[`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) guard – and
none of them ran on a default check.
[`example_engine()`](https://rodrigozepeda.github.io/tbl.now/reference/example_engine.md)
needs nothing, is deterministic, and returns in milliseconds, so the
examples for
[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md),
[`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md),
[`nowcast_weights()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_weights.md),
[`score_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/score_nowcast.md)
and
[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
on a backtest now show real output.

It is not a nowcasting method. It ignores the reporting delay entirely –
reporting the counts that have arrived and putting a `spread`-wide band
around them – so it under-predicts recent dates by construction. That is
useful to *see* and useless to rely on; the examples say so. Its source
is also the shortest complete
[`nowcast_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_fit.md)
/
[`nowcast_tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_tidy.md)
pair in the package, if you are writing a backend.

### New: `tbl_now()` warns on misspelled argument names

[`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
keeps unmatched `...` names as user metadata, which meant a typo in a
real argument name was accepted in silence. `case_col = "n"` set a
useless attribute and left count data typed as a line list – as it had
been doing in one of this package’s own examples.

Names close enough to a real argument to be a typo now warn and name the
intended one. Deliberate metadata (`data_source`, `citation`,
`population`) stays silent: a match needs a shared first letter and an
edit distance under a third of the longer name, which is what keeps
`source` from being read as a misspelling of `force`.

### `autoplot()` on a nowcast draws the reported counts as columns

The cases reported so far were points floating in the middle of the fan,
which reads as a second estimate. They are now grey **columns** under
it, so they read as a count measured from zero and the correction the
nowcast applies is the visible gap between the top of a bar and the
band. The bars are one period wide, taken from
[`get_event_units()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md).

### `EpiNow2` keeps its draws

[`nowcast_tidy.EpiNow2()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_tidy.md)
now reads the posterior samples with
`EpiNow2::get_predictions(format = "sample")` instead of the fit’s
`lower_<pct>`/`upper_<pct>` summary. Before, EpiNow2 could report only a
median and the two tails of whatever `CrIs` it happened to be fitted
with – three levels – so `quantile_levels` could not be honoured,
`tidy(probs =)` was an error, and it could not join a
`type = "linear_pool"` ensemble. It now does all three. The summary path
remains as a fallback for a fit
[`get_predictions()`](https://epiforecasts.io/EpiNow2/reference/get_predictions.html)
cannot read.

This has a visible knock-on: an ensemble containing EpiNow2 now shares
all nine of
[`nowcast_quantile_levels()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_quantile_levels.md)
rather than collapsing to three.

### Performance: `tbl_now()` and every `dplyr` verb on one

No behaviour changed, but the class got substantially cheaper.
[`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
is about **3x faster** and
[`validate_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md)
– which runs on every `dplyr` verb via `tbl_now_reconstruct()` – about
**4x**.

Almost all of the cost was building findings that were then discarded.
[`validate_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md)
reports at `floor = "note"`, so on a clean object it formatted eleven
`cli` messages and showed one; formatting is the expensive part (a hint
interpolating a vector of row numbers costs ~15 ms), and each finding
also built its own one-row tibble (~2 ms).

- `.diagnose_text()` now returns a **template** rather than a formatted
  string, and `.diagnose_finalise()` filters by the reporting floor
  *before* formatting, so only a finding somebody will read is paid for.
- Findings are plain lists until `.diagnose_finalise()` assembles the
  one tibble the caller sees.

[`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
returns exactly the same tibble, and
[`validate_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md)
the same conditions.

### Documentation

Every reference page was read once, function by function, for an
audience of public-health practitioners first and statisticians second.

- **Eleven defects**, most of them found by running examples that had
  never been run.
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
  documented two attributes that do not exist (`repot_num` – a typo –
  and `event_num`) and omitted four that do. The `align_weeks` example
  passed `case_col =`, which `...` swallowed, building count data as a
  line list. The `change` example referenced an undefined object that
  only survived because R never forced the promise.
  [`update()`](https://rdrr.io/r/stats/update.html)’s example built from
  the whole dataset and then “updated” it with rows it already held.

- **Fifteen pages shipped with an empty Description.** A block opening
  with a bare `` `r lifecycle::badge()` `` paragraph gets that badge as
  its *entire* `@description`, pushing the prose into Details;
  [`?diagnose_drift`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_drift.md)
  and fourteen others showed a badge and nothing else, in the help
  viewer and in the reference index.

- **Article links.** `vignettes/articles` is `.Rbuildignore`d, so
  `vignette("nowcasting-models")` and
  `vignette("custom-nowcast-models")` resolved to nothing in an
  installed package. All article references now use URLs.

- **Ten pages merged into five**, with aliases preserved so
  [`?change`](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
  and existing links still resolve: `change` and `remove` onto `add`;
  `plot_reporting_process` onto `plot_epidemic_process`; `names_tbl_now`
  and `money_tbl_now` onto `assign_tbl`; `as_scoringutils` onto
  `score_nowcast`; `censor_confirmation_delays_above` onto
  `censor_delays_above`; `is_tbl_now` onto `validate_tbl_now`;
  `week_2_date` onto `align_weeks`; `compute_temporal_effects` onto
  `add_temporal_effects`.

- Every exported topic now has `@seealso`, `@return` and a runnable
  example; every internal function carries `@noRd`. Both
  `@examplesIf FALSE` blocks are gone, and nothing in `man/` contains
  `if (FALSE)` or `\dontrun{}`.

- [`?tbl.now`](https://rodrigozepeda.github.io/tbl.now/reference/tbl.now-package.md)
  was the DESCRIPTION text and nothing else. It now lays out the
  workflow – declare, describe, diagnose, reshape, fit, check – with a
  link into each step.

- Three slow examples trimmed: `align_weeks` ran the whole 452,567-row
  FluSight table (15.4s to 1.5s), `tbl_now_summary` computed
  [`summary()`](https://rdrr.io/r/base/summary.html) four times over,
  and both Stan examples fitted on twenty years of dengue data.

- `vignette("ensemble-nowcasting")` gains a figure of **the ensemble
  against each of its members**, and a section on `min_date` explaining
  why the engines are not all shown the same data.

- `data-raw/ensemble_comparison.R` fits both Stan back-ends with
  **approximate inference** (`epinowcast` through
  [`enw_pathfinder()`](https://package.epinowcast.org/reference/enw_pathfinder.html),
  `EpiNow2` through `stan_opts(method = "pathfinder")`), so the article
  rebuilds in minutes rather than overnight. The article says so, so no
  member’s band is mistaken for that package’s tuned answer.

- It also **no longer fits three epidemics.** It scored every member on
  mpox and covid as well as dengue and cached the result as `forecasts`;
  no chunk in the article ever read that table, and it was roughly two
  thirds of the run time.

- `DEVELOPMENT_SKILL.md` records why the CRAN test path cannot be
  measured with
  [`testthat::test_local()`](https://testthat.r-lib.org/reference/test_package.html),
  and `devel/TEST_SPEEDUP_BRIEF.md` is a standalone brief on the suite’s
  runtime with measured per-file timings.

## tbl.now 0.26.0

### One `surveillance` line list per stratum

[`tbl_now_to_surveillance()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_surveillance.md)
gains `format = "linelist_list"`, which returns one line list **per
stratum** as a `tbl_now_surveillance_list` instead of one frame with a
pasted `strata` column.
[`surveillance::nowcast()`](https://rdrr.io/pkg/surveillance/man/nowcast.html)
has no strata argument, so a stratified analysis is one fit per stratum,
and the split no longer has to be done by hand:

``` r

pieces <- tbl_now_to_surveillance(x, format = "linelist_list")
lapply(pieces, function(piece) surveillance::nowcast(data = piece, ...))
```

It mirrors `tbl_now_to_baselinenowcast(format = "triangle_list")`
throughout: the result is a **plain list**, so
[`lapply()`](https://rdrr.io/r/base/lapply.html), `[[` and friends work
unchanged; it is length one and named `"all"` when the object declares
no strata, so the return type never depends on whether strata happen to
be attached; it prints what it is; and
[`as_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md)
binds it back into a `tbl_now`, restoring the original date-column
names, the strata and the covariates. Count input comes back as a
`"linelist"` – one row per case, totals unchanged – because that is what
a `surveillance` line list holds.

`format = "linelist"` remains the default and is unchanged.

### Documentation

- The `surveillance` and `NobBS` sections of
  `vignette("nowcasting-models")` now say that the credible interval
  **is** in their figures and is simply too narrow to see: the median
  band over the plotted window is under 1% of the estimate for both,
  against 37% for `epinowcast`. The numbers quoted are computed from the
  cached
  [`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
  tables rather than typed.
- `EpiNow2` gained the nowcast-vs-truth figure every other engine’s
  section already had.
- The `surveillance` section fits its strata through the new
  `format = "linelist_list"`.

## tbl.now 0.25.0

### A vignette on writing your own back-end

`vignette("custom-nowcast-models")` is the full account of the
[`nowcast_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_fit.md)
/
[`nowcast_tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_tidy.md)
contract: what a method may assume about the `tbl_now` it is handed (get
the column names from the getters, work on `.event_num`/`.delay` rather
than the calendar, run the grid to
[`get_now()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md),
remember that a line list cannot hold a zero), how to reuse the
`tbl_now_to_*()` converters and
[`as_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md)
instead of reshaping by hand, and what shipping a back-end in a package
involves.

The worked example is a **delay-ratio nowcast**: for each delay it takes
the median of the factor by which past mature event dates grew from that
delay to their eventual total, and applies the empirical quantiles of
that factor to the counts reported so far. It needs no modelling
package, so the article runs every line of its own code – including the
scoring, the backtest and the ensemble – and it is written twice, once
returning `predictions` and once returning `draws`, to show both
branches of the contract.

`vignette("ensemble-nowcasting")`’s section 4 now points here instead of
carrying its own smaller version of the same material.

### Bug fixes

- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  on a `tbl_nowcast` drew **only the 50% band**. The tails of each
  central interval were matched to the requested width by exact
  equality, and `(1 - (1 - 2 * 0.05)) / 2` is not `0.05`, so every other
  band came out as an `NA` ribbon and was silently dropped by `ggplot2`.
  The default nine quantile levels now draw all four bands, and
  `levels =` is matched with a tolerance too.

### Documentation

- [`?nowcast_tidy`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_tidy.md)
  said its `...` was “available to your own” methods. It is not:
  [`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
  forwards the user’s `...` to
  [`nowcast_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_fit.md)
  only, so anything the tidying step needs has to travel inside the fit
  object. Both help pages now say so.

## tbl.now 0.24.0

### `diagnose()`: a structural health check

[`summary()`](https://rdrr.io/r/base/summary.html) describes a
`tbl_now`;
[`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
looks for what is **wrong** with it. One row is one finding, sorted
worst first, and the offending row indices come with it:

``` r

findings <- diagnose(dengue_now)
findings |> dplyr::filter(status <= "note")

bad <- findings |> dplyr::filter(check == "ordering")
dengue_now[bad$rows[[1]], ]
```

Ten checks: `declarations` (attribute types, the columns they name, role
collisions, columns the object was never told about, temporal effects
added but never materialised), `ordering`
(`event <= report <= confirmation`, including the transitive leg that a
missing `report_date` would otherwise hide), `missing`, `duplicates`,
`units`, `negatives`, `now`, `truncation`, `strata` and `signposts`.
Each is also an exported function of its own – see
[`?nowcast_diagnose_components`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_diagnose_components.md)
– and `diagnose(x)` is exactly the
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
of them.

`status` is an **ordered factor**, worst first, which is why the tibble
sorts itself and why `status <= "note"` reads as “anything worth acting
on”: `error` \> `warning` \> `note` \> `ok` \> `not_run` \> `skipped`.

Four decisions worth knowing about:

- **It runs no statistical test, ever.** Whether the reporting delay
  drifts, and whether reports arrive in batches, are statements about a
  *distribution*. Answering them means choosing a method, a maturity
  window and a multiplicity correction, and
  [`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
  has no business choosing those on your behalf. It emits `not_run` rows
  carrying the call instead – `diagnose_drift(x, axis =)` and
  `diagnose_batches(x, axis =)`.
- **Reporting outages are deliberately not detected.** A `tbl_now` does
  not carry the zeroes, so an absent row means “nothing was reported”
  and a quiet Sunday is structurally identical to a three-week outage.
  Telling them apart requires asking whether a run of zero-arrival dates
  is improbably long, which is a test. The descriptive answer is
  [`zero_run_summary()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md);
  the inferential one is
  [`diagnose_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches.md).
- **An `NA` count is reported neutrally.** In a reporting triangle it
  means *not yet observed* – correct data, and the thing that tells a
  nowcast the cell is still open – so
  [`diagnose_missing()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_diagnose_components.md)
  counts it without calling it a defect. An `NA` *date* is a different
  matter and stays a warning.
- **[`diagnose_strata()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_diagnose_components.md)
  uses no thresholds.** “Too small to fit separately” depends on the
  engine and on the epidemic, so it names the extremes – the smallest
  stratum, its case count and its share; the sparsest stratum and how
  much of the event grid it leaves empty – and lets you judge.

### `validate_tbl_now()` is the same engine, presented as conditions

[`validate_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md)
no longer has a check list of its own. It calls the findings engine and
re-emits the result as the `cli` conditions it has always emitted: it
aborts on the `error`s and warns about the `warning`s. One
implementation, two presentations.

What that changes for you:

- **[`validate_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md)
  now warns when a confirmation precedes its report.** That check
  existed, but only ran at construction, so an object that acquired the
  problem later never mentioned it again.
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
  no longer runs it separately, so it warns once rather than twice.
- Everything else aborts and warns exactly as before, including
  `warn_non_uniqueness`, which stays `FALSE` there.
  [`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
  defaults it `TRUE`.
- A `note` is never emitted as a warning.
  [`validate_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md)
  runs inside every `dplyr` verb, and turning a
  [`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
  observation into a warning there would make construction noisy for
  data the class has always accepted.
- **One warning was reworded.** The missing-date warning said “*N* rows
  have NULL or NA values in column `event_date = "event_date"`” – it
  printed the literal string rather than the column, and a column cannot
  hold `NULL`. It now reads “*N* rows have NA values in the event_date
  column `"onset_week"`”.

### Breaking: the statistical tests take the `diagnose_` prefix

The five tests are named for what they are for rather than for the fact
that they are tests. **The old names are gone**, not deprecated:

| was | is now |
|----|----|
| `test_delay_drift()` | [`diagnose_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_drift.md) |
| `test_delay_changepoint()` | [`diagnose_changepoint()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_changepoint.md) |
| `test_confirmation_delay()` | `diagnose_confirmation_delay()` |
| `batch_test()` | [`diagnose_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches.md) |
| `batch_shape_test()` | [`diagnose_batch_shape()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batch_shape.md) |

The S3 class `batch_test`, and with it `print.batch_test()`, is renamed
to `diagnose_batches` to match.

### Documentation and website

[`summary()`](https://rdrr.io/r/base/summary.html) and
[`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
are now documented where people actually meet the package:

- **A new article**, *Describing and diagnosing a `tbl_now`*, treats the
  two as one workflow: what the schema means, what the six statuses
  mean, why `skipped` is not `ok`, and why
  [`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
  refuses to run a statistical test.
- **The worked example article is restructured.** It now builds the
  `tbl_now` *before* cleaning and lets
  [`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
  report the defects, rather than checking for them by hand and hoping
  the list was complete. The hand-written cleaning is still there — it
  is now the *fix* for what was reported, and it keeps the one check
  [`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
  deliberately will not do for a line list (deduplicating on a record
  id).
- **The README and the introductory vignette** gain a compact section on
  each.
- **The reference index is now explicit.** `_pkgdown.yml` gained a
  `reference:` section grouping every exported topic, so
  [`summary()`](https://rdrr.io/r/base/summary.html),
  [`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
  and their components are findable rather than buried in one
  alphabetical list. Note for contributors: `pkgdown` now **fails the
  build** on an exported topic that is not listed.
  [`pkgdown::check_pkgdown()`](https://pkgdown.r-lib.org/reference/check_pkgdown.html)
  catches it without building the site.

#### Fixed: the light/dark switch never rendered

`template: light-switch: true` was set and `lightswitch.js` was being
loaded, but the site had no toggle. The control is a navbar
**component**, and `_pkgdown.yml` named an explicit
`navbar: structure: right:` that replaced pkgdown’s default
`[search, github, lightswitch]` without listing it. The script loaded,
the button did not exist, and nothing errored. `lightswitch` is now
listed explicitly.

## tbl.now 0.23.0

### `summary()` describes the object the way a nowcaster reads it

[`summary()`](https://rdrr.io/r/base/summary.html) on a `tbl_now` now
returns a tibble rather than the column-by-column listing
[`summary.data.frame()`](https://rdrr.io/r/base/summary.html) produces,
which said nothing about the structure the class exists to carry. One
row is one statistic of one quantity of one stratum:

``` r

summary(dengue_now) |> dplyr::filter(component == "delay")
```

It covers the case counts on each of the object’s time axes (event,
report and, where there is one, confirmation), the delay distributions
between them, the lengths of the runs of zero dates, the compositional
shares (censored, per confirmation outcome, per stratum, per categorical
covariate level), the lag-1 autocorrelation of each series, the
reporting-completeness curve, the totals, the date ranges and `now`, and
how full the reporting triangle is.

Three decisions worth knowing about:

- **The date grids run to `now`, not to the last row present.** “Cases
  per event date” is a statement about a calendar; a date with no rows
  is a zero, not an absence. This is what makes `prop_zero` and the
  zero-run lengths mean anything, and it is why a **line list** – which
  cannot represent a zero – summarises to exactly the same numbers as
  its counts. The grid is *global*, so a stratum whose cases start late
  shows its leading zeros and the strata stay comparable. So does the
  triangle-occupancy denominator.
- **Quantiles are the inverse-ECDF (type 1) estimator**, not
  [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html)’s
  default: `q50` is the smallest value whose cumulative weight reaches
  `0.5`. This is the estimator
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  and `test_delay_drift()` already use, so the table and the figures
  agree, and it always returns a delay that was actually observed. The
  mean and standard deviation are the ordinary case-weighted ones, equal
  to expanding the counts to one row per case.
- **Not-yet-observed cells are dropped.** An `NA` count means the cell
  has not been observed yet, unlike a `0`, which was observed and was
  zero. Those rows carry no cases and are excluded, rather than turning
  every total they touch into `NA` – which is what `flusight` did to an
  earlier draft. The `"unobserved_cells"` coverage row says how many
  were dropped.
- **`count-cumulative` data gets no delay rows.** A cumulative total is
  not additive across delays, so a case-weighted delay distribution
  would be meaningless;
  [`delay_summary()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md)
  refuses it outright and points at
  [`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md).
  The new `"growth"` rows take its place, giving the ratio of each event
  date’s running total from one delay to the next.

### Every block of the summary is its own function

[`summary()`](https://rdrr.io/r/base/summary.html) is exactly the
[`bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html) of
these, and each returns the same schema, so they stack:

[`cases_per_date()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md),
[`delay_summary()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md),
[`zero_run_summary()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md),
[`prop_censored()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md),
`prop_confirmation_type()`,
[`prop_strata()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md),
[`prop_covariate_levels()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md),
[`case_autocorrelation()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md),
[`date_ranges()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md),
[`triangle_occupancy()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md),
[`reporting_completeness()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md)
and
[`cumulative_growth()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md).

[`delay_summary()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md)
names the three delays explicitly – `"event_to_report"`,
`"event_to_confirmation"` and `"report_to_confirmation"` – because the
first two are measured from the event and the last is the laboratory’s
own turnaround, measured from the report, and confusing them is a
documented hazard.

### Internal

One date-grid helper replaces three inlined copies of the same
`seq(from, to, by = <units>)` logic, including the one in
[`complete_zeroes()`](https://rodrigozepeda.github.io/tbl.now/reference/complete_zeroes.md)
that only knew about days and weeks.

## tbl.now 0.22.0

### The back-ends that stratify by ONE column

[`NobBS::NobBS.strat()`](https://rdrr.io/pkg/NobBS/man/NobBS.strat.html)
takes a single `strata` column name,
[`EpiNow2::regional_epinow()`](https://epiforecasts.io/EpiNow2/reference/regional_epinow.html)
a single `region`, and
[`surveillance::nowcast()`](https://rdrr.io/pkg/surveillance/man/nowcast.html)
takes no strata argument at all. A `tbl_now` may declare several
stratifying columns, and their interaction – “nowcast each observed
combination separately” – is exactly one stratum to those back-ends. The
converters now build that column, so there is an argument to write:

- [`tbl_now_to_nobbs()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_nobbs.md)
  and
  [`tbl_now_to_surveillance()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_surveillance.md)
  gain `strata_col` (default `"strata"`) and `strata_sep` (default
  `" | "`). The declared strata are pasted into that one column, which
  `NobBS.strat(strata = "strata")` takes directly and which
  [`split()`](https://rdrr.io/r/base/split.html) splits a `surveillance`
  line list on. The original columns ride along unchanged, and
  `strata_col = NULL` opts out.
- Pasting is refused rather than fudged when a **stratum value already
  contains the separator**: the label could not be split back apart, and
  a nowcast silently attached to the wrong stratum is worse than a
  failed conversion. The error names `strata_sep`.
  `tbl_now_to_EpiNow2(target = "regional_epinow")` gained the same
  check, which it did not have.
- Writing into an existing column is refused too, so a declared
  covariate called `strata` is not overwritten.

Previously
[`tbl_now_to_nobbs()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_nobbs.md)
handed back the strata as ordinary columns and nothing else, so there
was no way to call
[`NobBS.strat()`](https://rdrr.io/pkg/NobBS/man/NobBS.strat.html) on a
multiply stratified object at all. `run_nowcast(x, "NobBS")` had its own
copy of the pasting logic; it now uses the converter’s column, so the
two cannot disagree.

[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
also learned the last per-stratum shape it did not know: a list of
`stsNC` fits, which is what
[`split()`](https://rdrr.io/r/base/split.html)-ing a `surveillance` line
list and looping produces.

### `tidy()` returns the quantiles a NobBS fit was asked for

`NobBS` keeps no draws, so `tidy(fit, probs = ...)` refused every
`probs` outright. But
`NobBS(specs = list(quantiles = c(0.1, 0.5, 0.9)))` computes those
levels at fit time and puts them in `estimates` – reading them back is a
lookup, not an approximation, and refusing it made the documented
workflow (“ask at fit time, then request them with `probs`”) impossible
to complete.

[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
now returns them. A level the fit was **not** asked for still aborts,
because that one really is unrecoverable, and the message now names the
missing levels and the `specs = list(quantiles = ...)` call that would
have produced them.

### The two date grids `surveillance::nowcast()` needs

- `get_surveillance_when(x, length = 30)` – the dates to estimate, the
  most recent `length` steps ending exactly at
  [`get_now()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md).
- `get_surveillance_range(x)` – the whole time axis, passed as
  `control$dRange`.

Both read the step off the object’s own event units and abort on a
`"numeric"` grid rather than anchoring integer indices at the 1970
epoch. `dRange` matters more than it looks: left to itself
[`nowcast()`](https://rdrr.io/pkg/surveillance/man/nowcast.html) infers
the axis from the line list it was handed, and **a line list cannot
express a zero** – the quiet days at the `now` edge have no rows, so the
inferred axis stops short of exactly the days being nowcast.

### The article now runs the code it shows

`vignettes/articles/nowcasting-models.Rmd` displayed cached results next
to code that a separate script, `data-raw/nowcast_comparison.R`, kept
its own copy of. The two drifted, invisibly, because the article never
ran what it printed.

`data-raw/nowcast_models_precompute.R` replaces it: it
[`knitr::purl()`](https://rdrr.io/pkg/knitr/man/knit.html)s the article,
runs the article’s own chunks with the fits live, and reads the
displayed objects back out by name. The code that produced every number
is now literally the code printed above it. Renaming an object in the
article stops the script with a list of what is missing instead of
quietly saving a shorter file.

Fixed along the way, all of it drift the old arrangement hid:

- the Summary figure showed an unnamed grey `NA` line, because `EpiNow2`
  had no entry in the figure’s colour scale and the factor dropped it to
  `NA`;
- two chunk labels were duplicated and two chunks called
  [`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
  on objects the article never created, so the article could not be
  knitted at all;
- the `EpiNow2` delay section tidied a `dist_fit` that was never fitted,
  and the `epinowcast` seasonal fit was never assigned to a name;
- [`regional_epinow()`](https://epiforecasts.io/EpiNow2/reference/regional_epinow.html)
  was called without `truncation`, which is the one argument that makes
  it a nowcast – the same trap the pooled section spends a warning box
  on;
- `epidist`’s **marginal** model is used, now that it compiles. It reads
  the aggregated weights the converter produces instead of expanding
  6.1M cases back to one row each, which is why the latent model was
  there;
- the `epinowcast` sections filtered to **two years** of daily reference
  dates while every other engine used 60 days, and the article claimed
  that “keeps the Stan fit tractable”. It does not: one chain spent
  **six hours** in a bad region of the posterior while the other chain
  of the same fit finished in sixteen minutes. The cached numbers had
  come from a 180-day fit that took six minutes, so the article had
  never run its own window. It is 180 days now, with the discrepancy
  explained in the text;
- the `epinowcast` fits were **unseeded** –
  [`epinowcast()`](https://package.epinowcast.org/reference/epinowcast.html)
  does not take R’s [`set.seed()`](https://rdrr.io/r/base/Random.html),
  so Stan drew its own each run and the same fit took 41 minutes once
  and six hours the next. Both now pass `seed` through
  [`enw_fit_opts()`](https://package.epinowcast.org/reference/enw_fit_opts.html).

## tbl.now 0.21.0

### The confirmation process

A `tbl_now` can now carry a **third** date. Influenza is the picture to
keep in mind: symptoms begin (the event), the patient visits a doctor
(the report), and days later a swab comes back positive (the
confirmation) or negative (a *retraction* – reported, but not a case
after all). The assumed timeline is
`event <= report <= confirmation <= now`.

- [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
  gains `confirmation_date`, `confirmation_type` and
  `confirmation_units`. `confirmation_type` takes `"confirmed"`,
  `"retracted"`, `"pending"` or `NA`; **pending** means reported and
  still waiting, so it has no confirmation date, which is a different
  thing from a result you never recorded (`NA`). Two columns are
  derived: `.confirmation_num` (on the same numeric grid as the other
  dates) and `.confirmation_delay`, the laboratory’s turnaround,
  measured **from the report**.
- `add_confirmation()`, `change_confirmation()`,
  `remove_confirmation()`, `get_confirmation_date()`,
  `get_confirmation_type()`, `get_confirmation_units()` and
  `has_confirmation()`.
- A date with no type warns rather than guessing: a date alone cannot
  say whether the case was confirmed or retracted. A confirmation before
  its own report warns too.
- `now` is confirmation-aware. A result issued on a date means the
  system was still being observed then, so `now` is never earlier than
  the last confirmation, and setting one earlier is an error.
- The confirmation columns survive `dplyr` verbs,
  [`update()`](https://rdrr.io/r/stats/update.html),
  [`align_weeks()`](https://rodrigozepeda.github.io/tbl.now/reference/align_weeks.md)
  (which now aligns all three dates) and
  [`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md)
  (which groups by the confirmation, so a case is never summed together
  with its own retraction).
- The print footer gains a confirmation line: the column, its units, and
  how many cases are resolved.

#### Counting when cases can be undone

[`get_latest_confirmed()`](https://rodrigozepeda.github.io/tbl.now/reference/validation_counts.md),
[`get_net_confirmed()`](https://rodrigozepeda.github.io/tbl.now/reference/validation_counts.md)
(confirmed minus retracted), `get_nth_confirmed(x, delay)` and
[`get_initial_confirmed()`](https://rodrigozepeda.github.io/tbl.now/reference/validation_counts.md)
– the confirmation mirrors of the report-axis getters.
`censor_confirmation_delays_above()` returns implausibly long
confirmations to `"pending"`, which is what they really were.

#### Diagnostics on the confirmation axis

A laboratory clearing a backlog looks exactly like a surveillance system
clearing its inbox, so rather than duplicate every diagnostic, they take
an `axis = c("report", "confirmation")` argument: `batch_test()`,
`batch_screen()`, `batch_shape_test()`,
[`transport_discriminant()`](https://rodrigozepeda.github.io/tbl.now/reference/transport_discriminant.md),
[`plot_reporting_process()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_epidemic_process.md),
[`plot_epidemic_process()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_epidemic_process.md),
[`plot_reporting_triangle()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_reporting_triangle.md),
[`plot_delay_profiles()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_profiles.md),
[`plot_reporting_hexamap()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_reporting_hexamap.md),
[`plot_scalogram()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_scalogram.md),
[`plot_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_drift.md),
`test_delay_drift()`, `test_delay_changepoint()` and
[`diagnostic_plot()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnostic_plot.md).

On the confirmation axis, delays are still measured **from the event**,
so the two axes are directly comparable and the gap between them is the
time the laboratory adds. Cases still `"pending"` are excluded –
counting them would invent an arrival on a date they do not have.

New in their own right: `plot_confirmation_status()` (the confirmed /
retracted / pending shares over time), and `test_confirmation_delay()` /
`plot_confirmation_delay()`, which ask whether retractions come back
faster than confirmations – a laboratory that rules cases out sooner
than it confirms them biases any nowcast that treats the two alike.

### Other changes

- **Calendar temporal effects are now factors.** `day_of_week`,
  `day_of_month`, `month_of_year` and `week_of_year` are `factor`s with
  their full level sets (all seven weekdays, 1-31, 1-12, 1-52) rather
  than character or numeric columns, so a model gets dummy coding rather
  than treating “Tuesday” as twice “Monday”, and a level absent from a
  stratum still exists. `weekend` stays 0/1 and the Fourier `seasons`
  stay numeric, as both are already correctly numeric.
- **Fixed:** the non-uniqueness warning fired on every
  confirmed/retracted pair. A case and its own retraction share an
  (event, report) combination and are still two different rows; the
  confirmation columns are now part of the key.
- `run_nowcast(x, "diseasenowcasting")` passes straight through to
  `diseasenowcasting::nowcast()`. The confirmation process belongs to
  that package’s `model()`, not to `tbl.now`, so pass it there.

## tbl.now 0.20.0

### Bugs found by the new engine test suite

Every one of these was found by writing the tests, not before:

- **`count-cumulative` data failed on `diseasenowcasting` for want of a
  confirmation process.** `diseasenowcasting::nowcast()` auto-detects
  cumulative data and switches to the signed-increment Skellam / SkNB
  likelihood, but that likelihood needs a `confirmation_process()` – the
  retraction side of a stream that can revise **down** – and `model()`’s
  default is `no_confirmation()`. Without one the fit reports “Joint fit
  failed to converge for all init attempts”. Pass one through, as
  `run_nowcast(x, "diseasenowcasting", model = model(confirmation = confirmation_process()))`.

  De-accumulating to incidence first would also “work”, and is wrong: it
  discards the downward revisions the cumulative likelihood exists to
  model.

- **A censored report’s window started before its own event date.** For
  `is_censored` rows, `.delay_censoring_windows()` bounded the secondary
  window below by the *earliest event in the data* rather than by that
  row’s event, so every censored row implied a possibly-negative delay,
  and the zero-width guard pushed one strictly negative. refuses it
  outright (“Assertion on `data$stime_lwr` failed: not \>= 0”) and
  [`EpiNow2::estimate_dist()`](https://epiforecasts.io/EpiNow2/reference/estimate_dist.html)
  would have fitted a delay distribution with mass below zero. The
  window is now `[event_date, report_date]`, and the zero-width guard
  widens **upward**.

- **`as_tbl_now(x, verbose = )` failed on two classes.**
  [`as_tbl_now.tbl_now_triangle_list()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md)
  and
  [`as_tbl_now.tbl_now_epinow2_snapshots()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md)
  passed `verbose` both explicitly and through `...`: “formal argument
  ‘verbose’ matched by multiple actual arguments”. Both now default it
  into the dots, so the caller still wins.

- **`verbose = FALSE` was suppressing warnings, not just chatter.**
  `.quietly_if()` wrapped every backend in
  [`suppressWarnings()`](https://rdrr.io/r/base/warning.html), which hid
  exactly the messages that say what the model actually saw – strata
  pooled, a censoring flag collapsed, covariates dropped. It now
  suppresses **messages only**. This is the same failure mode
  DEVELOPMENT_SKILL section 9 records for `run_engine()`.

- **`diseasenowcasting` and `NobBS` were pooling multi-column strata
  needlessly.** `diseasenowcasting` models any number of strata and
  labels each combination `"F|N"`; `tbl.now` only ever read the
  one-column case and pooled otherwise.
  [`NobBS.strat()`](https://rdrr.io/pkg/NobBS/man/NobBS.strat.html)
  takes one column, so several are now joined into their interaction and
  split back apart. Both take **any** number of strata, and
  [`?run_nowcast`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)’s
  table says so.

### Covariates and censoring are no longer dropped in silence

- [`tbl_now_to_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_baselinenowcast.md)
  (matrix and triangle formats),
  [`tbl_now_to_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md)
  and
  [`tbl_now_to_EpiNow2()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_EpiNow2.md)
  now **warn** when declared covariates cannot be carried, naming them
  and saying what to do instead. Materialised temporal-effect columns
  count as covariates: they are the case where somebody asked for an
  effect and would otherwise never learn it was ignored.
- The censoring collapse already warned; it is now *reachable* through
  `run_nowcast(verbose = FALSE)` because of the `.quietly_if()` fix
  above.

### `nowcast_truth()` removed

Dropped entirely rather than kept internal. It was
[`get_latest_reported_cases()`](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.md)
reshaped.
[`score_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/score_nowcast.md)
and
[`as_scoringutils()`](https://rodrigozepeda.github.io/tbl.now/reference/score_nowcast.md)
take the `tbl_now` itself as `truth`.

### `covidat` removed

`covid_us` is kept: it is the only shipped dataset that actually
exhibits backlog dumps, which `vignette("batch-reporting")` is about.
Measured against a 15-day rolling baseline, `covid_us` has 21 report
days above 2x and 5 above 3x; `covid_colombia` has one above 2x and none
above 3x.

### New tests

All [`skip_on_cran()`](https://testthat.r-lib.org/reference/skip.html),
all on **synthetic fixtures** built by `tests/testthat/helper-engines.R`
rather than on shipped data, so one axis can be varied at a time:

- `test-engines-matrix.R` – 24 real fits per fast engine ({0,2
  covariates} x {0,2 strata} x {days, weeks} x the three data types),
  plus numeric-grid refusals, weekly-grid preservation, strata labelling
  for 0/1/2 columns, counts-are-cases, and
  [`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
  against the hand-written call.
- `test-engines-covariates.R` – used, or complained about, per
  converter.
- `test-engines-censoring.R` – used, or announced, per converter.
- `test-converter-roundtrip-all.R` – a registry of every
  `tbl_now_to_*()` shape and whether
  [`as_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md)
  brings it back; **fails when a converter is missing from it**.
- `test-coercion-methods.R` – every converter must expose the target
  package’s own coercion generic
  ([`as_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.html),
  `as_tsibble()`, …) as a thin wrapper, or record why that package has
  none. It re-checks the “has none” claims against the installed
  package, so we find out if one gains a verb.

### Articles

- `vignette("nowcasting-models")`: **EpiNow2 is now a two-step fit.**
  Given only a reporting delay it does not nowcast at all – its median
  stayed flat and sat below the already-reported count. `delays` says
  how infections become reports; it does not say the newest days are
  incomplete. Only `truncation` does, and that is what the report
  dimension of a `tbl_now` measures. Step 1 fits it with
  [`estimate_truncation()`](https://epiforecasts.io/EpiNow2/reference/estimate_truncation.html),
  step 2 passes it as
  [`trunc_opts()`](https://epiforecasts.io/EpiNow2/reference/trunc_opts.html).
  Over the last seven days – about 50% complete – the fit now sits below
  the reported count on 4 of 21 stratum-days instead of most of them.
- `vignette("ensemble-nowcasting")`: the three-epidemic comparison is
  removed; the article is now about how to use ensembles.

### DEVELOPMENT_SKILL

- A pre-flight grep before writing any new function, with the two
  questions that decide whether it should exist, and the two times this
  package got it wrong.
- The target package’s own coercion verb is now part of “writing a
  converter”.

## tbl.now 0.19.0

### Converters no longer make you aggregate first

`covid_colombia` carries `sex`. An object built without `strata = sex`
therefore has **two rows per `(notification_date, diagnosis_date)`
cell**, and a reporting triangle, a `tsibble` key and an epinowcast
observation table each have exactly one slot per cell. Until now that
meant
[`tbl_now_to_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_baselinenowcast.md)
aborted (“duplicate `reference_date` and `report_date` combinations”)
and
[`tbl_now_to_tsibble()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_tsibble.md)
aborted (“a valid tsibble must have distinct rows”), and you had to
`group_by() |> summarise()` before converting.

Both now pool undeclared columns for you, as
[`tbl_now_to_nobbs()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_nobbs.md),
[`tbl_now_to_surveillance()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_surveillance.md),
[`tbl_now_to_EpiNow2()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_EpiNow2.md),
[`tbl_now_to_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md)
and
[`tbl_now_to_data_table()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_data_table.md)
already did. The pooling is
[`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md),
so case totals are preserved exactly, and it is reported under
`verbose = TRUE`:

    i `tbl_now_to_baselinenowcast()`: pooled over 1 undeclared column ("sex");
      18195 rows -> 10129.
    i Declare it with `add_strata()` to nowcast it separately.

Line lists are left alone: one row is already one case there, and
collapsing would destroy the individual records the target package is
being handed.

### The non-uniqueness warning now names the culprit

It used to say “Consider using
[`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md)
to aggregate the data or
[`distinct()`](https://dplyr.tidyverse.org/reference/distinct.html) to
remove repeated observations”. The
[`distinct()`](https://dplyr.tidyverse.org/reference/distinct.html) half
is wrong whenever the cause is an undeclared column – those rows **are**
distinct, they differ in `sex` – so it sends you in a circle, and on
data with genuine repeats it silently deletes cases. The warning now
inspects the object and says which:

- undeclared columns: names them, and points at `strata =` or
  [`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md),
  adding that the converters pool them for you so this is information
  rather than a fault;
- genuine duplicate rows: says so, and *then* recommends
  [`distinct()`](https://dplyr.tidyverse.org/reference/distinct.html).

### `tbl_now_to_baselinenowcast(max_delay = )`

A cap on the delay axis, counted exactly as
[`tbl_now_to_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md)
counts it – `max_delay = 30` keeps delays `0` to `29`, giving a
30-column triangle – so the same number means the same triangle in both.
`NULL` (default) keeps every delay, which is the previous behaviour.
This replaces the `filter(.delay <= 30) |>` idiom the docs used to
recommend.

### `nowcast_truth()` is now internal

It was
[`get_latest_reported_cases()`](https://rodrigozepeda.github.io/tbl.now/reference/get_latest_first.md)
with the class stripped, undeclared columns summed away and the count
renamed `.observed` – the values were identical. A second public name
for that is a second thing to learn for no gain.

[`score_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/score_nowcast.md)
and
[`as_scoringutils()`](https://rodrigozepeda.github.io/tbl.now/reference/score_nowcast.md)
now accept the **`tbl_now` itself** as `truth` and do the reshaping
internally, which is shorter than what it replaces:

``` r

score_nowcast(nowcast, truth = dengue)          # was: truth = nowcast_truth(dengue)
```

A data frame of observed counts still works, as does `NULL`.

### `?run_nowcast` says what the models actually are

Three new sections, because “it calls the package with its defaults” is
not enough to read the output:

- **Strata** – a table of how many each backend can model and how.
  `baselinenowcast`, `surveillance`, `EpiNow2` and `epinowcast` take any
  number; `diseasenowcasting` and `NobBS` take exactly one and **pool
  with a warning** beyond that, because the single array dimension they
  return cannot be split back into two columns.
- **Temporal effects** – the converters materialise them into columns,
  but only `diseasenowcasting` uses them automatically. `epinowcast`
  needs them named in a module formula; every other backend carries them
  and ignores them.
- **Censored delays** – collapsed with a warning by every backend that
  goes through a converter; `diseasenowcasting` receives the flag
  intact.

And a section on how each engine’s default model is specified, with the
two that most need saying out loud:

- **`epinowcast`** defaults to a per-day random effect on the growth
  rate (a random walk in all but name), a single time-constant lognormal
  reporting delay, and no day-of-week report effect.
- **`EpiNow2`** defaults to `delays = delay_opts()`, which is `Fixed(0)`
  – **no reporting delay at all** – and `generation_time = gt_opts()`,
  which is `Fixed(1)`. Those defaults describe a process with nothing to
  nowcast, so supply the epidemiology yourself. It also models with a
  Gaussian process rather than a random walk.

### Article fixes

- **`vignette("nowcasting-models")` now cuts at 2021-04-01**, on the
  rising limb of Colombia’s third wave, instead of 2023-03-03 where the
  epidemic had subsided and a nowcast had nothing to correct. The
  line-list engines trim to 60 days (278,000 rows; NobBS 24s,
  surveillance 6s measured) rather than 180.
- **The per-package figures were drawing the wrong quantity.**
  [`geom_col()`](https://ggplot2.tidyverse.org/reference/geom_bar.html)
  was given `width = 5.5` on a **daily** series, so each bar overlapped
  its neighbours and ggplot2 stacked the overlaps: the grey “reported by
  now” bars showed sums of about six days. The summary figure at the end
  of the article used `width = 0.8` and was correct, which is why the
  two disagreed. All the panels now use `width = 0.8`.
- **The stratified `NobBS` example handed it count rows**, which is the
  exact mistake the article’s own warning box forbids two screens
  earlier – it counts rows, so it was nowcasting counts as cases. It now
  goes through
  [`tbl_now_to_nobbs()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_nobbs.md)
  like the unstratified example.
- The stratified `surveillance` example converted the **whole** series
  (2.3M cases) at build time and used `N.tInf.max = 1000` against
  per-stratum daily counts of ~4,000, which silently truncates the
  posterior. It now trims first and uses the same settings as the
  unstratified fit.
- `vignette("ensemble-nowcasting")` gains an **experimental badge and
  note** at the top, and a figure showing each member’s median against
  the ensemble and the eventual truth.

## tbl.now 0.18.0

### New: one call per model, and ensembles

Until now `tbl.now` prepared data for six nowcasting packages and
normalised what they returned, but running several of them still meant
six different calls and six different result shapes to reconcile by
hand. This release adds the layer that removes that bookkeeping.

- **`run_nowcast(x, method)`** fits any supported package and always
  returns a **`tbl_nowcast`**: an S7 object holding the predictions as
  one row per (event date, stratum, quantile level), plus the draws
  where the backend has them, plus the backend’s own untouched fit.
  Backends ship for `"diseasenowcasting"`, `"baselinenowcast"`,
  `"epinowcast"`, `"NobBS"`, `"surveillance"` and `"EpiNow2"`, each
  feeding its package through the matching `tbl_now_to_*()` converter
  rather than building the input by hand.

  It is called
  [`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
  and not
  [`nowcast()`](https://rdrr.io/pkg/surveillance/man/nowcast.html)
  because exports
  [`nowcast()`](https://rdrr.io/pkg/surveillance/man/nowcast.html);
  keeping the names distinct means both can be attached at once.

- **[`nowcast_ensemble()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_ensemble.md)**
  combines several of them, either by averaging their quantiles level by
  level (`type = "quantile"`, vincentization – narrower) or by pooling
  their draws into a mixture (`type = "linear_pool"` – wider, and
  refused outright when a member has no draws, rather than silently
  dropping it).

- **[`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md)**,
  **[`score_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/score_nowcast.md)**
  and
  **[`nowcast_weights()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_weights.md)**
  score models retrospectively and turn those scores into ensemble
  weights (`"inverse_score"`, `"optim"` or `"equal"`).
  [`as_scoringutils()`](https://rodrigozepeda.github.io/tbl.now/reference/score_nowcast.md)
  hands the same object to for its full score suite.

- **[`nowcast_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_fit.md)
  /
  [`nowcast_tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_tidy.md)**
  are the extension point: two S3 methods, in any package, and
  [`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
  knows about your model. See `vignette("ensemble-nowcasting")`.

- **[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)**
  for a `tbl_nowcast` draws a fan chart, in the palette’s green – a
  nowcast estimates the epidemic process, not the reporting one.

### New: `tidy()` for a nowcast and for a backtest

[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
already worked on every raw engine fit. It now also works on what
[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
and
[`nowcast_ensemble()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_ensemble.md)
return, which is the way round it should always have been.

- **[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
  on a `tbl_nowcast`** returns the package’s standard frame –
  `event_date`, `stratum`, `estimate`, `conf.low`, `conf.high`, `level`,
  `engine`, plus `q*` columns for `probs`. `engine` is the method (or
  the ensemble’s name); `level` is the width of the **widest symmetric
  pair of quantile levels the object actually carries**, and is `NA`,
  with `NA` bounds, when no symmetric pair exists. A guessed 0.95 there
  would defeat the one column that exists to stop a 90% band being
  compared with a 95% one.

  `probs` is honoured only when the nowcast carries draws, and errors
  otherwise: a quantile-only nowcast cannot produce a level it was not
  summarised at.

  Registered in `.onLoad()`, because `tbl_nowcast` is S7 and
  `tidy.tbl.now::tbl_nowcast` is not a writable S3 method name.

- **[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
  on a `nowcast_backtest`** gives one row per (method, `now` date,
  target), with the internal dot-prefixed columns traded for ordinary
  ones.

### New: reproducible backtests

[`nowcast_backtest()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_backtest.md)
gains a **`seed`** argument. When given, the RNG is seeded immediately
before each fit, from the seed and the method and date that fit is for.
One [`set.seed()`](https://rdrr.io/r/base/Random.html) before the whole
backtest only pins anything if every method draws the same random
numbers in the same order – which stops being true the moment a method
is dropped or one date is refitted. This is the same lesson
`data-raw/nowcast_comparison.R` already records.

`nowcast_weights(type = "optim")` now falls back to equal weights, with
a warning, when the optimiser does not converge on a usable point. It
used to return `NA` weights, which do not fail until much later inside
[`nowcast_ensemble()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_ensemble.md),
as an all-`NA` nowcast that reads like a modelling problem rather than
an optimisation one.

### Removed: the `nowcaster` backend

`nowcaster` was dropped in 0.16.0 along with its converters, for the
reasons recorded there. The
[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
backend for it is not shipped: it called `tbl_now_to_nowcaster()` and
`get_nowcaster_strata()`, which no longer exist. Neither `nowcaster` nor
`INLA` is reintroduced to `DESCRIPTION`.

### Other

- `scoringutils` added to `Suggests` (CRAN, so no
  `Additional_repositories` entry is needed). is deliberately **not**
  added: it is GitHub-only and sits in no repository
  `R CMD check --as-cran` can resolve, so declaring it would trade an
  undeclared-import warning for a CRAN-incoming NOTE about a dependency
  that cannot be found.
  [`nowcast_fit.diseasenowcasting()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_fit.md)
  therefore looks its entry point up with
  [`getExportedValue()`](https://rdrr.io/r/base/ns-reflect.html) after
  `.need_pkg()` has confirmed the package is installed, rather than
  writing a literal `diseasenowcasting::`.
- `LICENSE` / `LICENSE.md` copyright year updated to 2026. The
  hand-rolled `.wis()` is now cross-checked against it in the test
  suite: two implementations agreeing is worth more than either alone.
- New article, `vignette("ensemble-nowcasting")`, with the fits
  precomputed by `data-raw/ensemble_comparison.R` so the build never
  fits anything. It reports WIS per model and per ensemble across three
  epidemics, and answers “does the ensemble beat its best member?” and
  “does performance weighting beat equal weighting?” from the cached
  numbers rather than by assertion.
- `vignette("nowcasting-models")` gains a
  [`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md)
  column in its package table, and a pointer to the new article.

## tbl.now 0.17.0

### New: support

[`tbl_now_to_EpiNow2()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_EpiNow2.md)
and
[`tbl_now_from_EpiNow2()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_EpiNow2.md),
against **EpiNow2 1.9.0** (now the minimum in `Suggests`). EpiNow2 takes
four different input shapes, one per entry point, so `target` names the
function the result is passed to and it can be handed over unchanged:

- **`"estimate_infections"`** (default) – `data.frame(date, confirm)`,
  the series as known at
  [`get_now()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md).
  Also what
  [`epinow()`](https://epiforecasts.io/EpiNow2/reference/epinow.html)
  takes.
- **`"regional_epinow"`** – the same plus a `region` column built from
  the object’s strata (`" | "`-joined for several, matching the
  `triangle_list` convention). The other targets pool strata with a
  warning.
- **`"estimate_truncation"`** – a `tbl_now_epinow2_snapshots` list, one
  `date`/`confirm` snapshot per report date. This is the one EpiNow2
  model that uses the report dimension a `tbl_now` exists to carry.
- **`"estimate_dist"`** – the interval-censored frame
  [`EpiNow2::estimate_dist()`](https://epiforecasts.io/EpiNow2/reference/estimate_dist.html)
  fits a delay distribution to. **New in EpiNow2 1.9.0**, and it
  documents the schema exactly, so it shares
  `.delay_censoring_windows()` with
  [`tbl_now_to_epidist()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epidist.md)
  rather than growing a second copy.

Three things worth knowing:

- **EpiNow2 models a daily process and has no `timestep`.** As of 1.9.0
  there is no `timestep`, `interval` or `period` argument on any entry
  point (all four formals checked), so a weekly series passed as one row
  per week is read as one row per **day** – no error, just an epidemic
  seven times too fast. The converter lays it on the daily grid with
  EpiNow2’s own `accumulate` column instead – built by
  \[EpiNow2::fill_missing()\] rather than by hand, because a hand-rolled
  version put each period’s count on the period’s *last* day where
  [`fill_missing()`](https://epiforecasts.io/EpiNow2/reference/fill_missing.html)
  leaves it on the date given, shifting every weekly fit six days with
  no error. Units coarser than a week, and the `"numeric"` grid, are
  refused by name rather than approximated.

  `initial_accumulate` is passed explicitly rather than inferred: with
  `by`, EpiNow2 1.9.0’s inference drops each group’s first observation
  (a two-region weekly series of 336/167 cases came back as 295/147).
  Single-series inference is unaffected.

- **The snapshot form has a real inverse.** Snapshot *k* is the series
  as known at report date *k*, so differencing consecutive snapshots
  recovers `count-incidence` exactly. `tbl_now_epinow2_snapshots`
  carries the report dates so
  [`as_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md)
  can do it; a bare list needs `report_dates`. Verified against
  [`EpiNow2::example_truncated`](https://epiforecasts.io/EpiNow2/reference/example_truncated.html),
  which round-trips to the case for the case. (The commented-out draft
  of this converter asserted no inverse was possible. For a single
  series that is true; for snapshots it is not.)

- **[`estimate_secondary()`](https://epiforecasts.io/EpiNow2/reference/estimate_secondary.html)
  and
  [`estimate_delay()`](https://epiforecasts.io/EpiNow2/reference/estimate_delay.html)
  get no target.** The first models two data streams against each other
  and one `tbl_now` is one stream; the second is superseded by
  [`estimate_dist()`](https://epiforecasts.io/EpiNow2/reference/estimate_dist.html)
  by EpiNow2’s own help and throws away the censoring a `tbl_now`
  carries.

**`obs_date` and the censoring windows are different quantities**, and
the converter now treats them as such. `[sdate_lwr, sdate_upr)` brackets
*when the report happened* – at weekly resolution `[W, W + 7)`, a
half-open interval whose upper bound is the end of that week, not a
claim that anything happened on day `W + 7`. `obs_date` is *when
observation stopped*, which
[`estimate_dist()`](https://epiforecasts.io/EpiNow2/reference/estimate_dist.html)
asserts is `>= sdate_upr` on every row. A `tbl_now`’s `now` **labels a
period**, so the instant observation stopped is the end of it:
`obs_date = now + w`. That makes the assertion hold by construction, and
nothing is observed after it. Clamping the windows at `now` instead was
tried and rejected – it moves reports in the final period into an
earlier one, which the epidist round-trip test caught.

The `nowcasting-models` article now covers across all three strata, with
its results precomputed into `nowcast-comparison.rds` like every other
engine. Two caveats are stated in the article itself: the delay
distributions are ’s shipped examples rather than distributions fitted
to the Colombian data, and sampling is lighter than the default (500
draws, 250 warmup, 2 chains) because it is much the slowest engine in
the comparison.

`data-raw/nowcast_comparison.R` now takes engine names
(`Rscript data-raw/nowcast_comparison.R EpiNow2`) and merges them into
the existing file, leaving every other engine’s rows and recorded
timings alone; with no arguments it rebuilds everything as before. This
replaces a second script that re-created the setup by parsing the first
one.

Two correctness fixes came out of that. Every engine is now seeded per
`(engine, stratum)` immediately before its fit, rather than relying on a
single [`set.seed()`](https://rdrr.io/r/base/Random.html) at the top of
the script – which only pins results if every engine consumes the same
random numbers in the same order, and so does not survive refitting a
subset. Refitting `baselinenowcast` alone had been silently changing its
estimates, and one EpiNow2 fit produced a stratum whose upper credible
bound sat at `1e8` for all 181 days and would not reproduce. Both now
refit to `max abs diff == 0`. The script also refuses to cache any fit
whose scale exceeds 100x the observed maximum for its stratum, since an
unconverged Stan or INLA fit returns numbers rather than an error.

[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
gained methods for `estimate_infections`, `epinow`,
`estimate_truncation` and `estimate_dist`, plus a `regional_epinow`
branch in
[`tidy.list()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
giving one block per region.

[`tidy.estimate_dist()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.estimate_dist.md)
reports the fitted distribution’s **`mean` and `sd`** alongside its
parameters, so its output is directly comparable with
[`tidy.epidist_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.epidist_fit.md).
They are derived from the **distribution**, not from the family’s
algebra: each draw’s parameters go back into the fit’s own `dist_spec`
and through \[EpiNow2::discretise()\], which knows the families, and the
moments follow by summation over the PMF. Nothing in this package names
a distribution, so a family adds later works as soon as
[`discretise()`](https://epiforecasts.io/EpiNow2/reference/discretise.html)
supports it. Against the closed forms the mean is exact and the sd runs
about 1% high – the variance a discrete grid adds – so expect a
difference of that order against , which reports continuous-distribution
moments.

It also honours `probs` and takes a `level` argument, matching
[`tidy.epidist_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.epidist_fit.md).
(An earlier draft rejected `probs` with a message claiming the engine
keeps no draws. It does: `summary.estimate_dist()` reads them.)

`tbl_now_to_EpiNow2(target = "estimate_dist")` warns when it pools
strata –
[`estimate_dist()`](https://epiforecasts.io/EpiNow2/reference/estimate_dist.html)
has no grouping argument, so it fits one distribution to everything –
and warns when a large share of delays are exactly zero, since a
lognormal has zero density there and will inflate its variance rather
than fail. The message points at the families that do have positive
density at zero (`"exp"`, or `"gamma"`/`"weibull"` with shape below 1)
rather than at a constant shift, which would silently bias every
parameter.

Two more points of care:

- `level` is read off the `lower_<pct>`/`upper_<pct>` column names,
  because EpiNow2’s `CrIs` is a user argument – a fit made with
  `CrIs = c(0.5, 0.95)` has no `lower_90` at all, and hard-coding `0.90`
  would report a width the fit never produced.
- [`tidy.estimate_dist()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.estimate_dist.md)
  returns the **delay** schema (`term`, `estimate`, …), not the nowcast
  one – the second instance of the documented exception alongside
  [`tidy.epidist_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.epidist_fit.md).
  Note that [`summary()`](https://rdrr.io/r/base/summary.html)’s
  `mean`/`sd` *columns* are the posterior mean and sd of each
  **parameter**, while the `mean`/`sd` *rows* this method reports are
  the **delay distribution’s** moments. Same words, different
  quantities.

`.epidist_drop_unusable_counts()` is now `.drop_unusable_counts()` and
shared:
[`EpiNow2::estimate_dist()`](https://epiforecasts.io/EpiNow2/reference/estimate_dist.html)
asserts `n >= 1` with the identical message epidist uses, so the same
filter applies to both.

### Audit of the converters and `tidy()` against the target packages’ own docs

Every claim the converters and
[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
methods make about `diseasenowcasting`, `baselinenowcast`, `epinowcast`,
`epidist`, `NobBS`, `surveillance`, `tsibble` and `data.table` was
re-checked against those packages’ installed help pages and source. Five
defects came out of it, all of them cases where the code was silently
*plausible* rather than wrong-looking.

- **[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
  no longer pools strata under `"all"`.**
  [`tidy.nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
  documents `stratum` as `"all"` *when the fit is unstratified*, so
  `(stratum, event_date)` is meant to be a unique key. Two methods broke
  that:

  - **[`tidy.epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)**
    read `summary(fit, type = "nowcast")` and ignored both `.group` and
    the `by` columns sitting beside it. A real two-group fit
    (`by = "age_group"` on `germany_covid19_hosp`, age groups `00+` and
    `80+`) came back as 20 rows all labelled `"all"`, with every one of
    its 10 reference dates duplicated. It now emits one block per `by`
    group, and several grouping columns are pasted `" | "`-separated,
    matching `tbl_now_to_baselinenowcast(format = "triangle_list")`.
  - **[`tidy.list()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)**
    recognised a
    [`NobBS::NobBS.strat()`](https://rdrr.io/pkg/NobBS/man/NobBS.strat.html)
    fit – it has the `estimates`/`onset_date` shape the detector looks
    for – but ignored the `stratum` column that
    [`NobBS.strat()`](https://rdrr.io/pkg/NobBS/man/NobBS.strat.html)
    puts there. A two-stratum fit on `denguedat` returned 44 rows
    labelled `"all"`, 22 of them duplicate keys. It now reads `stratum`
    when present.

  The `probs` path for `epinowcast` was mispaired in the same way: it
  split the posterior samples on `reference_date` alone, so on a
  stratified fit each date’s quantiles went to whichever stratum
  [`split()`](https://rdrr.io/r/base/split.html) sorted first. The split
  is now keyed on `(stratum, reference_date)` and indexed by the
  summary’s own rows.

- **[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
  no longer invents an interval for a `baselinenowcast` point fit.**
  `baselinenowcast(output_type = "point")` returns one value per
  reference date and stamps `output_type = "point"` on the result.
  [`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
  ignored that column and took the 2.5%/97.5% quantiles of a single
  number, reporting `conf.low == conf.high == estimate` with
  `level = 0.95` – a zero-width 95% band. It now returns `NA` bounds and
  `NA` `level`, and refuses `probs` rather than returning the point
  estimate under a quantile’s name.

- **[`tbl_now_to_nobbs()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_nobbs.md)
  prints the `units` string NobBS accepts.** Its verbose summary printed
  the object’s own `"weeks"`, but
  [`NobBS::NobBS()`](https://rdrr.io/pkg/NobBS/man/NobBS.html) documents
  `units` as `"1 day"` or `"1 week"`; pasting `"weeks"` into the call
  produces `-Inf`/`Inf` warnings from
  [`seq()`](https://rdrr.io/r/base/seq.html) and then an opaque
  `replacement has 1 row, data has 0`. It now prints `"1 week"`, and
  aborts up front for any grid NobBS cannot model.

- **The line-list back-ends no longer fabricate 1970 dates from a
  `numeric` grid.**
  [`tbl_now_to_nobbs()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_nobbs.md)
  and
  [`tbl_now_to_surveillance()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_surveillance.md)
  both coerced the event and report columns with
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html). On a
  `numeric`-unit `tbl_now` those columns are integer indices, so index 1
  became 1970-01-02 and the conversion succeeded, silently, with a line
  list of invented dates. Both now abort naming the units, as
  [`tbl_now_to_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_baselinenowcast.md)
  and
  [`tbl_now_to_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md)
  already did.
  [`tbl_now_to_surveillance()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_surveillance.md)
  also gained `"years"` -\> `"1 year"`; it previously fell through to
  `"1 week"`.

The remaining findings were addressed too:

- **A negative delay now warns instead of silently losing cases.** A
  reporting triangle is indexed by delay from 0, so a report that
  arrived *before* its event has no cell: 10 cases in gave a triangle
  summing to 9, with the affected cell reading `0` – an *observed* zero
  – rather than `NA`. Both triangle formats and
  [`tbl_now_to_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md)
  now warn, naming how many rows and cases go and how to filter them
  yourself. `format = "long"` has no delay axis, keeps them, and stays
  quiet.

- **[`tbl_now_to_epidist()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epidist.md)
  accepts `count-cumulative` data.** epidist asserts `n >= 1`, and
  de-accumulating a cumulative series produces a `0` wherever a report
  added nothing and a negative on any downward revision – so the
  conversion died on epidist’s own `Assertion on 'data$n' failed` for
  essentially any real cumulative input, and for plain incidence data
  that had been through
  [`complete_zeroes()`](https://rodrigozepeda.github.io/tbl.now/reference/complete_zeroes.md).
  Rows carrying no case are now dropped before the epidist object is
  built: a zero contributes nothing to a delay distribution, so that is
  lossless and only reported under `verbose = TRUE`; a negative discards
  a revision, so it **warns**; and if nothing usable is left the
  converter aborts saying why. `flusight` – the one `error` cell in the
  article’s converter matrix – now converts.

- **[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
  handles a per-stratum list of `baselinenowcast` fits.**
  [`?tbl_now_triangle_list`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_triangle_list.md)
  recommends `lapply(triangles, baselinenowcast::baselinenowcast)`, and
  [`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
  on the result used to error and suggest `engine = "NobBS"`. A list
  whose elements are all `baselinenowcast_df` is now recognised: each is
  tidied and labelled with its list name (or its position, when the list
  is unnamed), giving the same one-block-per-stratum table the natively
  stratified engines return. `probs` passes through.

- **`DEVELOPMENT_SKILL.md` section 2 corrected.** It claimed
  [`tbl_now_to_surveillance()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_surveillance.md)
  sets `control$dRange`. It does not, and its own help page says so:
  `now` and the delay unit are deliberately left to the caller, because
  the converter cannot know which window you mean to fit.

### Behaviour changes

- **[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
  reports `level = NA` for a fit** instead of `0.95`.
  [`NobBS()`](https://rdrr.io/pkg/NobBS/man/NobBS.html)’s
  `lower`/`upper` come from `specs$conf`, and its return value is
  `list(estimates, estimates.inflated, nowcast.post.samps, params.post)`
  – no `specs`, so the width is genuinely unrecoverable from the fit. A
  guessed default is worse than `NA` in the one column that exists to
  stop widths being compared blindly. Pass `tidy(fit, level = 0.95)` to
  fill it in. The assertion in `test-tidy.R` that recorded the old
  behaviour was updated.

- **[`tidy.epidist_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.epidist_fit.md)
  warns on a delay model with covariates.**
  [`epidist::predict_delay_parameters()`](https://epidist.epinowcast.org/reference/predict_delay_parameters.html)
  returns one row per draw *and* observation, and the reported quantiles
  pool over both. For `mu ~ 1` every observation shares the draw’s
  value, so that is exactly the posterior interval; with covariates in
  the delay model the interval is a *mixture across covariate levels*,
  which the docs described simply as “Posterior median”. The method now
  detects a parameter that varies within a single draw and says so,
  pointing at `newdata` for a specific covariate combination. The
  numbers are unchanged – only the silence is.

### Tests

- New `test-tidy-strata.R`: stratified
  [`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
  for `epinowcast` and
  [`NobBS.strat()`](https://rdrr.io/pkg/NobBS/man/NobBS.strat.html),
  quantile-to-stratum alignment, the point-fit interval, the per-stratum
  list of `baselinenowcast_df` fits, and the `level` argument. The fits
  are mocked from the shape of real ones, so the file needs neither
  cmdstan nor JAGS.

- New `test-converter-grids.R`: the `numeric` grid across every
  converter, zero / negative / very long delays, gaps in the event grid,
  a trailing event period with no reports under each `complete` setting,
  the negative-delay warning, and epidist’s `n >= 1` filtering
  (including `flusight`).

- New `test-converter-strata-shapes.R`: several stratifying columns, a
  factor level with no rows, and label-to-value pairing when the data
  order is not alphabetical.

- `test-converter-censoring.R` now covers
  [`tbl_now_to_nobbs()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_nobbs.md),
  which the converter loop skipped because its package name is not its
  suffix.

- **Removed support**: `tbl_now_to_nowcaster()`,
  `get_nowcaster_strata()`, the
  [`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
  branch for its fits, and its sections in the articles are all gone.
  The converter worked, but the package around it demanded enough
  special-casing that keeping it cost more than it returned:

  - **`Dmax` and `wdw` are counted in weeks, whatever grid you hand
    it.** On a daily series, values chosen as days are silently read as
    weeks: `Dmax = 30`, `wdw = 120` asked for a 30-week horizon over a
    2.3-year window, which ran for **45 minutes** and had INLA reporting
    the fit diverging. The same fit with week-scaled values took **24
    seconds**.
  - **It returns weekly estimates from daily data**, so its numbers are
    weekly *totals* while every other engine reports daily counts –
    roughly 6x larger on the same axis, and not comparable without
    re-gridding. Its label is the week *start*.
  - **`age_col` must be numeric** even though the help calls it a
    stratum column: a character column errors inside
    [`cut()`](https://rdrr.io/r/base/cut.html), and a character
    `bins_age` trips an `if (bins_age == "SI-PNI")` comparison against a
    vector. The converter existed largely to encode strata into codes
    and hand back the matching breaks.
  - **Results come back as those codes, not labels**, so a tidied
    stratified fit reported `stratum` values of `"1"` and `"2"` rather
    than the levels.
  - **It takes its maximum observable time from the last event date, not
    the last report**, so cutting the series anywhere except where
    `max(onset) == max(report)` made it NA-mask genuinely observed cells
    and nowcast *below* what had already been reported.
  - It needs **R-INLA**, and was itself installable only from GitHub.

  That last point has a side benefit: was the only entry in `Remotes:`,
  so removing it drops that field entirely – and with it the reason the
  package could not be submitted to CRAN as-is.

- **New
  [`tbl_now_to_nobbs()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_nobbs.md)**,
  filling a real gap. counts *rows*, so handing it `count-incidence`
  data was silently wrong: a table of 1,174 rows carrying 50,160 cases
  was nowcast as 1,174 cases, with no error. The converter expands
  counts to one row per case first. The articles previously recommended
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html), which
  is correct only for a line list.

- **Fixed the pkgdown build on CI.** The shared “Learning more” fragment
  was pulled in with a relative child path (`../../man/fragments/...`).
  renders into an intermediates directory under
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) and copies
  relative resources alongside it, so a path containing `../..` escapes
  that directory – harmless where
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) is deep, fatal on
  CI where it sits two levels from the filesystem root
  (`cannot create file '/tmp/RtmpXXXX/../../man/...'`). The fragment
  moved to `inst/fragments/` and every caller now locates it with
  [`system.file()`](https://rdrr.io/r/base/system.file.html), which is
  path-independent and also works under
  [`pkgload::load_all()`](https://pkgload.r-lib.org/reference/load_all.html).

- **Dependency fixes for CI.** `almanac` is used by the package but was
  declared nowhere – the `Remotes:` entry for it was inert, since
  `Remotes` only says *where* to fetch an already-declared dependency.
  It is now in `Suggests`, with its r-universe added to
  `Additional_repositories` (it was archived from CRAN). `nowcaster` was
  declared but unobtainable from any configured repository; it briefly
  gained a `Remotes:` entry, and was then dropped altogether (above).

- **[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
  on a fit now works directly.** From 2.1.0 that package re-exports the
  shared `generics` generic and ships its own method, so `tidy(fit)`
  returns the standard nowcast table. `tbl.now` now registers its own
  method for `diseasenowcasting::nowcast_prediction` **only when the
  package does not supply one**, so older versions keep working and
  newer ones are not overridden. The article calls plain `tidy(dnc_fit)`
  again.

- **Article fixes so the code on the page reproduces the output shown.**
  Three places displayed results the printed code could not produce: the
  section tidied the *fit* rather than `predict(fit)`, and the section
  hid the trailing-row trim that keeps the final week from exploding.
  All three now match the precompute.

- **Documented two
  [`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
  masking hazards.**
  [`library(diseasenowcasting)`](https://rdrr.io/r/base/library.html)
  attaches its own
  [`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
  generic, and [`library(broom)`](https://broom.tidymodels.org/)
  overwrites `tbl.now`’s
  [`tidy.list()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
  method (which fits dispatch on). Neither errors; both silently return
  a different table.
  [`tbl.now::tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
  disambiguates.

- **New
  [`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
  method for fits.** is the one supported package that does not nowcast
  – it estimates the reporting-delay distribution – so
  [`tidy.epidist_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.epidist_fit.md)
  returns a *delay-shaped* table (`term`, `estimate`, `conf.low`,
  `conf.high`, `level`, `engine`) with one row per distribution
  parameter, rather than forcing a delay fit into the per-event-date
  nowcast schema. `probs` works, because the fit keeps its draws. Note
  that
  [`epidist()`](https://epidist.epinowcast.org/reference/epidist.html)
  returns `c("brmsfit", "epidist_fit")` in that order, so a loaded wins
  dispatch; call
  [`tidy.epidist_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.epidist_fit.md)
  explicitly if that matters.

- **[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
  now returns ’s credible interval**, which it previously discarded.
  [`surveillance::nowcast()`](https://rdrr.io/pkg/surveillance/man/nowcast.html)
  stores a prediction interval in the returned object’s `pi` slot at the
  width `control$alpha` names (95% by default), but the method
  hard-coded `conf.low`, `conf.high` and `level` to `NA`, so
  surveillance was the one engine that appeared to report no
  uncertainty. Reaching for the JAGS-backed
  `bayes.trunc`/`bayes.trunc.ddcp` methods was never needed to get an
  interval.

- **Censored delays no longer break the converters.** A censoring
  indicator that is a property of the *case* rather than of the delay –
  an administrative “this date is only an upper bound” mark, say –
  splits one `(event_date, report_date)` cell into a censored and an
  uncensored row. A reporting triangle has one slot per cell, so
  [`tbl_now_to_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_baselinenowcast.md)
  and
  [`tbl_now_to_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md)
  aborted on duplicate cells, and the converters that expand back to a
  line list picked the flag up as an unrequested stratifier. The
  censoring dimension is now collapsed before the conversion, and each
  route warns:

  - **count data**: counts are summed over the flag, so case totals are
    unchanged;
  - **line lists**: the column is dropped, leaving one row per case.

  [`tbl_now_to_epidist()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epidist.md)
  is deliberately exempt: estimating a delay distribution is the one job
  that can use the flag.

- [`tbl_now_to_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_baselinenowcast.md)
  now handles a **line list on its own**. It already aggregated to
  incidence; it now also completes the zero periods out to the `now`
  (new `complete = TRUE` argument). A reporting triangle is a
  rectangular grid, and an event period with no reports has no rows, so
  the triangle used to stop short unless you remembered
  `to_count() |> complete_zeroes()` first. Linelist and count-incidence
  input now produce an **identical** triangle.

- [`tbl_now_to_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_baselinenowcast.md)
  also accepts **`count-cumulative`** data, which it used to refuse.
  De-accumulating produces negative increments wherever a total was
  revised downward, and ships
  [`preprocess_negative_values()`](https://baselinenowcast.epinowcast.org/reference/preprocess_negative_values.html)
  for exactly that; the converter applies it and warns.
  `negatives = "error"` restores the old refusal.

- **Bug fix:
  [`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
  ignored the strata of a `diseasenowcasting` fit.** A stratified fit
  reports `strata_draws` (draws x event times x stratum), but the method
  read only the pooled `draws`, so every row came back with
  `stratum = "all"` even when the fit itself said “2 strata”. It now
  returns one block per stratum, matching what the other engines do.

- Two new test files worth naming, because they exist to stop silent
  regressions:

  - `test-converter-equivalence.R` – every converter accepts line-list
    input, and the triangle/preprocessing targets give the *same* result
    from a line list as from the equivalent count-incidence object.
  - `test-converter-datasets.R` – every converter against every dataset
    the package ships. This is the testthat counterpart of the article’s
    matrix: the article documents, this one fails.

- Website: fixed a regression that drew an empty scrollbar track (“a
  rectangle”) under every code chunk. The no-wrap rules have to apply to
  `code` as well as its container, but `overflow-x` must apply ONLY to
  the container – setting it on the inner `<code>` too made each one a
  second scroll context that reserved a gutter. Figure captions are now
  centred, smaller and grey.

- The nowcasting-models article now **shows the real output of every fit
  and every
  [`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
  call**. The fits are far too slow to run on each build, so
  `data-raw/nowcast_comparison.R` captures what each one prints and what
  [`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
  returns for it, and the article replays that. Each section pairs a
  copy-pasteable `tidy(fit)` (shown, not run) with a hidden `head(5)`
  whose output appears beneath it, so nothing in the visible code has to
  be trimmed for display. The ad-hoc result extraction each section used
  to do – pulling `$estimates` out of NobBS, building a
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html) from
  [`epoch()`](https://rdrr.io/pkg/surveillance/man/stsSlots.html) and
  [`upperbound()`](https://rdrr.io/pkg/surveillance/man/stsSlots.html)
  for surveillance – is gone; every section now uses
  [`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md).

- New article section running **every converter, plus a nowcast, against
  every dataset the package ships** (`data-raw/converter_matrix.R`),
  recording which combinations work and explaining the ones that do not:
  `count-cumulative` cannot become a reporting triangle without
  inventing negative increments, `epidist` has no individual delays to
  censor in a cumulative series, and a `tsibble` needs a unique
  index/key. Each attempt is time-limited so the matrix is reproducible.

- The article states plainly that each modelling package is a **separate
  install** that `tbl.now` does not pull in, with the commands for the
  ones that are not on CRAN and the note that Stan, JAGS and R-INLA are
  software outside R.

- `SKILL.md` documents
  [`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md),
  the `surveillance` converter, `format = "triangle_list"`, the new
  [`complete_zeroes()`](https://rodrigozepeda.github.io/tbl.now/reference/complete_zeroes.md)
  behaviour, and the zero-period pitfalls.

- Every package section in that article now shows how to recover its
  predictions with
  [`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md).

- The comparison precompute falls back gracefully when rejects a
  triangle whose most recent reference times are all zero. A thin
  stratum can hit that after the zero weeks are completed out to `now` –
  here the female series has no case in the final week even though the
  pooled series does – and the fallback completes only as far as the
  last week holding a case, costing that stratum one week rather than
  the whole nowcast.

- New
  **[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)**
  methods, one shape of answer whatever engine produced the fit. The
  converters normalise what goes *into* a nowcasting package;
  [`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
  normalises what comes back out. It returns `event_date`, `stratum`,
  `estimate`, `conf.low`, `conf.high`, `level` and `engine` for fits
  from , , , and .

  - `probs` adds one column per requested quantile, named after the
    probability (`q5`, `q50`, `q2.5`, …). Only the engines that keep
    draws (, , ) can honour it; the others error rather than return an
    approximation dressed up as a quantile.
  - `level` records the width each engine’s interval **actually** has –
    reports a 90% band by default while the others report 95%, and
    without it the two get compared as if they were the same.
  - returns an *unclassed* list, so it is told apart by structure, with
    an `engine` argument to override.
  - [`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
    deliberately does **not** re-grid: packages that bin onto their own
    week starts keep them, because snapping would hide a real
    difference.
  - The generic comes from (a new, dependency-free `Imports`), so it
    composes with rather than masking it.

- The nowcasting-models article now cuts at the **second week of July
  2002** rather than the turn of the year: a December cut lands on the
  holiday reporting slump, which says more about December than about the
  models. Section headings are now just the package name, each package’s
  figure carries a caption instead of a heading, each gains a *Simple
  nowcast* heading, and packages needing an external backend (Stan,
  JAGS, R-INLA) carry a coloured requirement callout. The overview table
  gained an *Additional requirements* column.

- Website: `.alert-warning` callouts now use the attenuated red the
  plots use for intervals, and code blocks are pinned to scroll sideways
  rather than wrap – pkgdown’s `white-space: pre-wrap` on `code` inside
  `pre` was overriding the bare `pre` rule and folding long lines onto a
  second line.

- Print methods now write to **stdout** instead of emitting messages.
  `print.batch_test()`, `print.transport_discriminant()`,
  [`print.tbl_now_triangle_list()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_triangle_list.md)
  and the `temporal_effects` print method used the `cli_*()` family,
  whose output is a *message* – so it vanished under `message = FALSE`,
  [`sink()`](https://rdrr.io/r/base/sink.html) or
  [`capture.output()`](https://rdrr.io/r/utils/capture.output.html),
  which is exactly where a print method is expected to work. They now
  use cli’s `cat_*()` family. The matching tests were switched from
  [`cli::cli_fmt()`](https://cli.r-lib.org/reference/cli_fmt.html) to
  [`capture.output()`](https://rdrr.io/r/utils/capture.output.html).

- The `epinowcast` section filtered on a hard-coded `2008-12-20`, left
  over from the old window; against the new data that matched **zero
  rows** and aborted the build. It now trims relative to the series,
  using `tbl_now_to_epinowcast(preprocess = FALSE)` followed by
  [`enw_filter_reference_dates()`](https://package.epinowcast.org/reference/enw_filter_reference_dates.html)
  and
  [`enw_preprocess_data()`](https://package.epinowcast.org/reference/enw_preprocess_data.html).

- [`tbl_now_to_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_baselinenowcast.md)
  gained `format = "triangle_list"`: one reporting triangle **per
  stratum**, instead of pooling them into a single matrix. Unlike
  splitting the long format by hand it takes the delay unit and the
  strata off the object, so neither has to be restated. With no strata
  attached the result is still a list — of length one, named `"all"` —
  so the return type never depends on whether strata happen to be
  present.

- The result is a thin `tbl_now_triangle_list` class: still an ordinary
  list, so [`lapply()`](https://rdrr.io/r/base/lapply.html) and `[[`
  work as before, but with a
  [`print()`](https://rdrr.io/r/base/print.html) method. The class earns
  its place as a guard: ’s
  [`estimate_and_apply_delays()`](https://baselinenowcast.epinowcast.org/reference/estimate_and_apply_delays.html)
  also takes a list of triangles, but *retrospective snapshots of one
  series* rather than one per stratum, and would silently accept a
  per-stratum list and treat the strata as points in time.

- [`as_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md)
  gained a method for `tbl_now_triangle_list`, rebuilding a
  `count-incidence` `tbl_now` with the strata recoded onto their column.
  The strata **values** are stored on the object rather than parsed back
  out of the element names, so a stratum containing the name separator
  still round-trips.

- **Bug fix:
  [`as_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/as_tbl_now.md)
  aborted on any weekly reporting triangle.**
  [`tbl_now_from_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_baselinenowcast.md)
  ignored the triangle’s own `delays_unit` attribute and read the delay
  columns as *days*, so a weekly triangle produced daily report dates
  against weekly event dates and unit inference contradicted itself
  (“report_units must be coarser than or equal to event_units”). Both
  directions now default `delays_unit = NULL` and resolve it from the
  attribute; an explicit value still wins.

- **Bug fix in
  [`complete_zeroes()`](https://rodrigozepeda.github.io/tbl.now/reference/complete_zeroes.md):
  it was silently deleting real cases.** The closing “don’t look into
  the future” filter compared with `<` rather than `<=`, so every row
  reported on the final report date was dropped — in the function’s own
  documented example, 5 of 55 cases vanished. A function whose job is to
  *add* zeroes was removing data at the boundary.

- [`complete_zeroes()`](https://rodrigozepeda.github.io/tbl.now/reference/complete_zeroes.md)
  now completes out to the object’s `now`, not merely to the last event
  date present in the data, and gained an `until` argument to complete
  to a specific date instead. An event date with no reports at all does
  not appear in the data, so the old behaviour left a gap exactly at the
  `now` edge — where nowcasting matters. A supplied `until` never
  truncates below the data. The line-list error message now explains why
  a line list cannot hold a zero week and points at
  [`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md).

- [`plot_reporting_hexamap()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_reporting_hexamap.md)’s
  `max_cells` is now a real bound. It previously took the delay at
  position `max_cells` and kept every cell sharing that delay, so a wide
  band at the cut overshot the documented cap.

- The nowcasting-models comparison now runs to the `now` for every
  engine. `baselinenowcast` gets there via
  [`complete_zeroes()`](https://rodrigozepeda.github.io/tbl.now/reference/complete_zeroes.md);
  `surveillance` cannot (a zero-count row expands to zero line-list
  rows, so padding evaporates) and is instead given its grid directly
  through `control$dRange`. The article explains both, including that
  forcing `surveillance` to estimate a period with no observations is
  unstable on stratified data.

- The nowcasting-models article now builds its `tbl_now` from the
  **whole** `denguedat` series (52,987 cases over 1,091 weeks) instead
  of a pre-filtered two-year window. Every converter runs on the full
  object in a few seconds; where a package needs a shorter series to
  fit, the article now uses **that package’s own argument** rather than
  subsetting the data first — `moving_window` in `NobBS` (which is what
  takes the full-series fit from impractical to about six seconds) and
  `when` in `surveillance`. `diseasenowcasting` (~12 s) and
  `baselinenowcast` (~10 s) take all 1,091 weeks as they are.
  `epinowcast` is the one engine with no such argument, since the
  reporting triangle is already built by the time you hold a
  preprocessed object; the article shows
  `tbl_now_to_epinowcast(preprocess = FALSE)` followed by
  [`enw_filter_reference_dates()`](https://package.epinowcast.org/reference/enw_filter_reference_dates.html)
  and
  [`enw_preprocess_data()`](https://package.epinowcast.org/reference/enw_preprocess_data.html),
  and prints the full and trimmed objects side by side. Each section
  states which it is doing.

- Website: `.alert-info` callouts (pandoc `::: {.alert .alert-info}`
  fenced divs) are restyled from the Bootstrap default blue into the
  package’s sage green, with a darker green left rule and heading
  colour.

- The nowcasting-models article’s `baselinenowcast` fit referred to a
  `dengue_triangle2` object that no longer existed; it now uses
  `dengue_triangle`.

- The example article was rewritten from scratch around the new
  `hai_bucaramanga` dataset and is now a full **end-to-end tutorial**:
  cleaning a messy surveillance extract with `dplyr` + `tbl.now`
  (duplicate records, missing dates, and reports dated before the event
  they describe), reading the data with
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  and the standalone `plot_*()` diagnostics, testing the reporting delay
  for drift and change points, attaching only the temporal effects the
  data justifies, and finally nowcasting with `diseasenowcasting` and
  five other engines. Each modelling choice at the end is traced back to
  a diagnostic at the beginning. It moved from `vignettes/` to
  `vignettes/articles/` (the pkgdown URL is unchanged) because
  `diseasenowcasting` is not on CRAN and so cannot be fitted while
  building a shipped vignette.

- New converters for further back-ends:

  - [`tbl_now_to_surveillance()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_surveillance.md)
    builds the individual-level line list \[surveillance::nowcast()\]
    works from, renaming the event and report dates to ’s own
    `dHospital` / `dReport` defaults. `format = "sts"` instead returns
    the observed curve as a `surveillance` `sts` object.

  It accepts count data as well as line lists, expanding counts back to
  one row per case (de-accumulating first when the data is cumulative).
  is a new `Suggests`.

- The nowcasting-models article gained a section for it and a closing
  **comparison of every engine on one set of axes** — one plot for the
  unstratified object and one faceted by stratum, with a colour per
  package, the incomplete data each engine actually saw, and the counts
  those weeks eventually reached. The comparison deliberately uses an
  earlier 2002-2003 window rather than the article’s main `dengue_now`,
  because the latter runs to the end of `denguedat` and so has no ground
  truth to check against. The fits are precomputed by
  `data-raw/nowcast_comparison.R` and read from a saved file, so editing
  the prose no longer re-runs Stan, JAGS and INLA.

- `flusight` no longer ships duplicate rows (#25). The upstream FluSight
  `time-series.csv` contains 39,139 exact duplicates, which forced every
  example to open with a
  [`distinct()`](https://dplyr.tidyverse.org/reference/distinct.html)
  call; the dataset now goes from 491,706 to 452,567 rows. The removal
  is lossless — every repeated (`as_of`, `target_end_date`,
  `location_name`) key carried an identical `observation`, with no
  conflicting values — so that triple is now a unique key. The help page
  documents the change, and the FluSight example vignette drops the
  de-duplication step.

- New dataset **`hai_bucaramanga`**: 1,423 healthcare-associated
  infections (IAAS) notified in Bucaramanga, Colombia, 2016-2023, from
  the Colombian open data portal. Column names and categorical values
  are translated from Spanish. It is a deliberately *unpolished* extract
  and its help page documents the defects in detail — a `1900-01-01`
  missing-date sentinel, 88 negative reporting delays, 100 exact
  duplicate records, and a strongly bimodal delay (3-day median, 92-day
  90th percentile) — which makes it a realistic exercise for the delay
  diagnostics rather than a clean modelling example.

- `test_delay_drift()` and `test_delay_changepoint()` now document
  **every column of their output**, plus new *Interpreting the result*
  sections. The `test_delay_drift()` help gained a *Choosing a method*
  section explaining why `"hamed-rao"` is the default (deterministic, no
  AR(1) assumption, effectively instant) and when to cross-check with
  `"block-bootstrap"`, which is robust to weekly periodicity but
  stochastic and thousands of times slower.

- The Get Started vignette now opens the nowcasting problem with a
  **figure** showing observed-to-date cases, the reports still in
  transit, and the nowcast of the eventual total.

- The “Learning more” links live in a single
  `man/fragments/learning-more.Rmd` and are included in the README and
  at the end of every vignette and article, so they only have to be
  edited in one place.

- Website: the “Articles” navbar dropdown was rendering near-black with
  grey text because the styling targeted `.submenu`, which Bootstrap 5
  does not use; it now targets `.dropdown-menu` and matches the pale red
  of the package plots. `pkgdown/extra.css` is also no longer listed
  under `includes: in_header:`, which was pasting raw CSS into `<head>`
  where it was ignored.

- README code blocks no longer wrap mid-tibble: printed output was being
  split into stacked column blocks by R itself, which no stylesheet
  could undo.

## tbl.now 0.15.0

- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  panels are now consistently **colour-coded by process**: red for
  everything reporting-related (the delay distribution, the delay
  calendar/holiday effects, the delay periodogram) and green for the
  epidemic (event-date) process (the observed cases and their
  calendar/holiday effects). This matches the colours the standalone
  diagnostic plots
  ([`plot_reporting_process()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_epidemic_process.md)
  /
  [`plot_epidemic_process()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_epidemic_process.md),
  [`plot_scalogram()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_scalogram.md),
  …) already used, so a panel and its standalone twin read the same.
- Every
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  panel now says **which process it describes** in its subtitle — either
  “Reporting delay process” or “Epidemic (event-date) process” —
  replacing the per-panel explanatory subtitles. A single panel
  therefore reads on its own.
- The two periodogram panels are renamed **“Cycles (periodogram)”**
  (previously “Seasonality” / “Delay periodicity”).
- Every
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  panel now has a standalone `plot_*()` twin that draws just that panel
  (identical data, colours and subtitle):
  [`plot_day_of_week_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/calendar_effect_plots.md),
  [`plot_week_of_year_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/calendar_effect_plots.md),
  [`plot_month_of_year_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/calendar_effect_plots.md),
  [`plot_holiday_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/calendar_effect_plots.md),
  [`plot_holiday_lag_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/calendar_effect_plots.md)
  (each taking `type = "epidemic"` or `type = "report"`), plus
  [`plot_cycles()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_cycles.md),
  [`plot_delay_distribution()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_distribution.md)
  and
  [`plot_observed_cases()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_observed_cases.md).
  Use
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  for the grid and a `plot_*()` for one effect on its own.
- The day-of-week, week-of-year, month-of-year, holiday and
  weekend/holiday-lag panels gained a `measure` argument (in
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  and every `plot_*()` twin). `measure = "normalized"` (the default) is
  the existing view — each value divided by its overall mean, 1 =
  average. `measure = "percent"` instead shows the **share of cases**
  falling in each group with its IQR (e.g. “10% of cases at the weekend
  versus 90% on weekdays”); the reporting version shares out the reports
  by report date. Percentages need `Date` event/report columns.
- Vignettes: the Get Started guide documents the `plot_*()` twins and
  the `measure` argument, and marks the “Holiday effects”, “Do delay
  distributions drift over time?” and “Detecting batch reporting”
  sections as AI-written, pointing readers to the human-written
  batch-reporting article. The FluSight example analysis is flagged as a
  work in progress.

## tbl.now 0.14.1

- Strata are now carried into the model converters that can use them.
  [`tbl_now_to_epidist()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epidist.md)
  keeps the strata as data columns (usable as covariates in an epidist
  formula), and `tbl_now_to_baselinenowcast(format = "long")` keeps them
  so you can build one reporting triangle per stratum. A single
  reporting-triangle **matrix** has no strata dimension, so
  `format = "matrix"` now **pools** the strata with a warning instead of
  erroring on duplicate cells.
  [`tbl_now_to_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md)
  already passed strata as its grouping (`by`).

- The nowcasting-models article was restructured: each package is now
  shown **bare** (from `dengue_now`) and then **enriched** — one
  `dengue_seasonal` object carrying a stratum and temporal effects flows
  through every converter — so the separate “Carrying delay effects into
  each model” section is gone. It adds a worked **per-stratum**
  `baselinenowcast` loop (one triangle per stratum). The workflow also
  had a bug: it used the plural
  [`estimate_and_apply_delays()`](https://baselinenowcast.epinowcast.org/reference/estimate_and_apply_delays.html)
  (which expects a *list* of retrospective triangles) on a single
  triangle; it now uses the one-call
  [`baselinenowcast()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.html)
  wrapper for samples and notes the singular
  [`estimate_and_apply_delay()`](https://baselinenowcast.epinowcast.org/reference/estimate_and_apply_delay.html)
  for a point nowcast.

- New
  [`plot_reporting_hexamap()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_reporting_hexamap.md):
  draws the reporting triangle as an age-period-cohort **hexamap**
  (Jalal and Burke, 2020). Event date, report date and delay are the
  cohort, period and age (`report = event + delay`); each cell is a
  hexagon coloured by its report count, and a **batch** — a single
  report date — reads as a clean **vertical stripe**. The number of
  hexagons is bounded by a `max_cells` safety cap (the delay axis is
  auto-capped, with a message, rather than drawing an unbounded map).
  Replaces the reporting-V panel in the batch article.

- Bug fix for issue \#33:
  `autoplot(x, strata = "race", by_strata = TRUE)` no longer errors with
  a strata passed as column name.

- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  gained four **holiday panels**, which describe the attached
  [`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md)
  spec rather than the event unit:

  - `"calendar_holiday"` / `"delay_holiday"` — normalized cases / mean
    reporting delay by **day type**. The categories follow the spec: a
    `holidays` calendar plus `weekend = TRUE` gives
    `Weekday`/`Weekend`/`Holiday`, a calendar alone gives
    `Non-holiday`/`Holiday`, and a weekend effect alone gives
    `Weekday`/`Weekend`. A holiday falling on a weekend counts as a
    holiday.
  - `"calendar_holiday_lag"` / `"delay_holiday_lag"` — the same, by
    **position relative to the nearest holiday** (`"2 before"`,
    `"1 before"`, `"Holiday"`, `"1 after"`, …, plus `"Other"` as the
    reference), as asked for by `holiday_lags`. These show exactly the
    days the `..._holiday_lag_k` / `..._holiday_lead_k` columns flag, so
    you can check a lag is worth modelling before you model it. A date
    that is both after one holiday and before the next is attributed to
    the nearer one, ties going to the “after” side.

- Bug fix:
  [`tbl_now_to_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md)
  now passes a `timestep` to , inferred from the object’s report units
  (`"days"` -\> `"day"`, `"weeks"` -\> `"week"`) and overridable with
  the new `timestep` argument. It previously left on its `"day"` default
  whatever the data, so **weekly** data was laid out on a daily grid.

- Bug fix:
  [`tbl_now_to_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md)
  now derives the temporal-effect covariates on ’s completed date grid
  instead of carrying them through
  [`enw_complete_dates()`](https://package.epinowcast.org/reference/enw_complete_dates.html).
  That function fills the (reference, report) grid and extends the
  reference axis into the nowcast horizon, but sets every non-schema
  column to `NA` on the rows it adds — so the covariates previously
  survived only on the original rows. Becasue the effects are functions
  of a date alone, they are n ow re-derived from the completed grid and
  cover every row, including the recent horizon dates a nowcast has to
  predict.

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

- Bug fix: `batch_shape_test()` no longer errors (“missing value where
  TRUE/FALSE needed”) on large count data. The standardised rank-sum
  expands counts to one value per item, so the group sizes could exceed
  the 32-bit integer range and their product overflowed to `NA`; the
  group sizes are now computed as doubles.

- `batch_test()` now returns a **lean, Benjamini-Hochberg-only** result:
  `report_date`, `stratum`, `reported`, `baseline`, `deficit`, `delta`,
  `p_transport`, `p_transport_bh` and the `batch` flag, each documented
  under `?batch_test`. The raw per-point `classification` column (and
  the `p_creation`/`p_deletion`/scale columns behind it) has been
  dropped: it was not multiplicity-corrected and over-identified,
  whereas `batch` controls the false discovery rate.
  ([`transport_discriminant()`](https://rodrigozepeda.github.io/tbl.now/reference/transport_discriminant.md)
  keeps its `classification`.)

- `batch_test()` (and
  [`transport_discriminant()`](https://rodrigozepeda.github.io/tbl.now/reference/transport_discriminant.md))
  now infer the calendar `period` from the object’s temporal effects: a
  **day-of-week** effect sets `period = 7`, a **week-of-year** effect
  `period = 52` (see \[add_temporal_effects()\]). A `period` you pass
  still wins, with a note if it disagrees; and if the data is daily with
  no temporal effect, the function suggests `period = 7`.

- The `baseline_method` argument of `batch_test()` and
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
  dumps — so `batch_test()` and the batch plots recover a clear, real
  signal (and correctly call the biggest December-2021 spikes *surges*,
  since they land on the Omicron wave). Prepared with duckdb from the 14
  GB source (see `data-raw/covid_us.R`).

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
  [`plot_reporting_process()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_epidemic_process.md),
  [`plot_epidemic_process()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_epidemic_process.md),
  [`plot_reporting_triangle()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_reporting_triangle.md),
  [`plot_delay_profiles()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_profiles.md),
  [`plot_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_drift.md),
  [`plot_transport_discriminant()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_transport_discriminant.md),
  `plot_reporting_v()`,
  [`plot_scalogram()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_scalogram.md),
  [`diagnostic_plot()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnostic_plot.md)
  and
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).
  Needs the (suggested) package.

- New `plot_reporting_v()`: the reporting **“V”** – the same data as
  [`plot_reporting_triangle()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_reporting_triangle.md)
  (the same event-date x delay cells) rotated 45° so report date runs up
  the page and the data opens into a V (left arm = event date, right arm
  = delay). A batch, a diagonal in the square triangle, becomes a
  horizontal slice. The whole observable triangle is filled (pale-blue
  reported zeros + coloured reports).

- New wavelet **scalograms**, `plot_scalogram(type = "reporting")` and
  `plot_scalogram(type = "epidemic")`, plus the paired
  [`plot_reporting_process()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_epidemic_process.md)
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

- The default `lookback` for `batch_test()` and
  [`transport_discriminant()`](https://rodrigozepeda.github.io/tbl.now/reference/transport_discriminant.md)
  is now **7** (a week of daily reporting) rather than 3.

- The `@details` of the batch functions (`batch_test()`,
  [`transport_discriminant()`](https://rodrigozepeda.github.io/tbl.now/reference/transport_discriminant.md),
  `batch_shape_test()`,
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
    `batch_test()`-confirmed batches (BH-corrected), not the raw
    per-point classification – which at level `alpha` painted 10-20% of
    points batch/surge/hold by construction, ignoring multiplicity and
    the heavy autocorrelation of the window statistics. The shaded batch
    region and the `±z*` lines are drawn only as a reference for where a
    batch would sit.

- New
  [`transport_discriminant()`](https://rodrigozepeda.github.io/tbl.now/reference/transport_discriminant.md):
  exposes the plane behind `batch_test()`’s conservation law – for every
  report date the **deficit** (the transport axis: reports the preceding
  window is missing) and the window **discriminant** (the creation axis:
  the window total relative to its baseline), with robust standardised
  `transport_z` / `creation_z` and the same quadrant `classification`. A
  batch sits top-left (a deficit paid the spike, no net creation); a
  surge sits bottom-right. Returned as a `transport_discriminant` tibble
  and plotted by `diagnostic_plot(panels = "transport")`.

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
  / `test_delay_drift()` / `test_delay_changepoint()`, the model-free
  batch detectors (`batch_test()`, `batch_shape_test()`,
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

- New `batch_test()` returns, per (report date, stratum), the `deficit`
  (reports missing beforehand — sensitive to a batch) and `delta` (the
  window total minus its expected value — sensitive to a real surge),
  and classifies each date as `"batch"`, `"surge"`, `"batch_and_surge"`,
  `"hold_or_deletion"` or `"none"`. The transport (batch) test
  conditions on the window total, so its size does not depend on the
  unknown incidence nor on the quality of the baseline; the baseline
  itself is refit from report dates *outside* each candidate window,
  which makes `delta` invariant to a within-window batch pathwise. It
  handles all data types, including `"count-cumulative"` (signed
  increments), and takes a `period` argument that absorbs a fixed
  reporting schedule (weekends, holidays).
- New `batch_shape_test()` tests whether a flagged report date drew on
  unusually *old* event dates, by a permutation rank-sum on the
  reporting delays. It is exactly distribution-free whenever incidence
  is locally log-linear.
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
  `test_delay_drift()`, `test_delay_changepoint()`,
  `detect_report_batches()`, `plot_report_batches()`) now carry a
  lifecycle **experimental** badge. `test_delay_drift()` and
  `test_delay_changepoint()` additionally emit a `cli` warning that they
  are experimental, their results are not guaranteed and their interface
  may change. Flagged batches, change points and trend changes are
  surfaced as **potential** (e.g. “potential batches”, “potential change
  point”).

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

- New `test_delay_changepoint()` complements `test_delay_drift()`: where
  the latter tests for a *gradual* monotonic trend, this tests for a
  **single abrupt change point** in the per-period delay summaries using
  **Pettitt’s** nonparametric test (implemented directly, no extra
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
  and `test_delay_drift()` to answer *“do reporting delay distributions
  drift over time?”*.

  - [`plot_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_drift.md)
    draws a rolling **fan chart** of the count-weighted delay
    distribution indexed by event date: a solid rolling median, a dashed
    rolling mean, and 25-75% / 10-90% quantile bands. The recent,
    not-yet-fully reported region (after the `level` incompleteness
    cutoff) is shaded grey so the truncation-induced dip is not mistaken
    for drift. Supports `by_strata`.
  - `test_delay_drift()` runs an **autocorrelation-robust
    monotonic-trend test** (Hamed-Rao modified Mann-Kendall by default,
    with Yue-Pilon and block-bootstrap options via the new `modifiedmk`
    *Suggests*) on the per-period delay summaries, testing both a
    location statistic (median/mean) and a dispersion statistic (IQR /
    10-90 spread), on mature data only. Returns a tidy tibble with the
    Kendall tau, Sen’s slope, p-value and a `drift` verdict, per stat
    and stratum.

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
  [`compute_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.md)
  at conversion time (the input `tbl_now` is left unchanged), and the
  columns are passed to `data.table`, `tsibble`, `baselinenowcast` long
  format, `epidist`, and `epinowcast` (where they appear in the
  observations and `metareference` tables for use in the reference
  module). The `baselinenowcast` reporting-triangle matrix still cannot
  hold them.
- Removed the `|>` export and changed all the pipes to `|>`
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
- [`tbl_now_to_EpiNow2()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_EpiNow2.md)
  gained a `model` argument: `"estimate_infections"` (default, the
  single `date`/`confirm` series) and `"estimate_truncation"` (a list of
  report-date snapshots, the one EpiNow2 model that uses the report
  dimension). Documentation clarified accordingly.
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
  [`update_now()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
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
