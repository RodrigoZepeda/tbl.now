# Documentation audit (v0.27.0)

Audience: public-health practitioners first, statisticians second. Plain language in
`@description`; anything technical goes in a `@section Statistical details:` or `@details`.

## Conventions adopted

1. **Vignette links.** Only `vignettes/tbl.now.Rmd` is a real vignette. Everything in
   `vignettes/articles/` is `.Rbuildignore`d and is a *pkgdown article*, so
   `vignette("nowcasting-models")` FAILS in an installed package. Link articles by URL:

   `[the *One dataset, many nowcasts* article](https://rodrigozepeda.github.io/tbl.now/articles/nowcasting-models.html)`

   Article slugs: `batch-reporting`, `custom-nowcast-models`, `describing-and-diagnosing`,
   `ensemble-nowcasting`, `example`, `nowcasting-models`.

2. **Every exported topic** gets `@return` and `@seealso`, and at least one runnable example.
3. **`\donttest{}`** is allowed for genuine model fits only; no `\dontrun{}`, no `if (FALSE)`.
4. **Internal helpers** carry `@noRd`.

## Findings log

(filled in per section)

### Section 1 — The `tbl_now` class

Fixed:
* `tbl_now()` *Attributes* section documented two attributes that do not exist
  (`repot_num` — a typo — and `event_num`) and omitted four that do
  (`confirmation_date`, `confirmation_type`, `confirmation_units`,
  `computed_temporal_effect_cols`). Corrected against the live object.
* `is_tbl_now()` folded onto the `validate_tbl_now` page ("check this object",
  quiet vs loud). `_pkgdown.yml` updated.
* Three long-standing roxygen warnings cleared: a `@noRd` block followed by text,
  and two `\link{}`s to undocumented internals.
* **Bug:** `.batch_report_increments()`'s roxygen block was stranded above
  `.batch_confirmation_axis()` in `R/batch_screen.R`; the documented function had
  no block at all. Moved.

Deferred to the Converters pass:
* **Inconsistency (tidy-select).** `as_tbl_now()` documents `event_date` /
  `report_date` as tidy-select, but `as_tbl_now.tbl_ts()` ->
  `tbl_now_from_tsibble()` requires character strings (`setdiff()` and
  `.build_tbl_now()` use the value directly). Docs made accurate for now; decide
  in the converter sweep whether every `tbl_now_from_*()` should take tidy-select.

### Section 2 — Attributes

Fixed:
* `add` / `change` / `remove` merged onto **one** page ("Set, change and remove the
  attributes of a `tbl_now`"). You cannot understand `add_strata()` without
  `change_strata()` beside it; the three pages were cross-referencing each other in
  a loop. `\alias{change}` and `\alias{remove}` keep `?change` and old links working.
* **Bug in the `change` example:** `add_temporal_effects(disease_data, t_effects = ...)`
  passed an undefined object `disease_data` into `overwrite=` by position. It only
  ran because R never forced it. Removed.
* The `change` examples contained a **duplicated** "Change covariates" block, and the
  `remove` examples had comments that said "Add strata" above `remove_strata()`.
* **Bug in the `update.tbl_now` example:** built `initial_tbl` from the *whole*
  `denguedat` and then "updated" it with rows it already contained, so the example
  demonstrated nothing. Now splits 1:500 / 501:1000 as the prose claims.
* `@seealso [temporal_effects()] [add] [remove] [change]` (four links run together with
  no separators, in four places) replaced with prose lists saying what each is for.
* `nowcast_data_getters`: per-function `@return` (was one vague line for 14
  functions), and the `NULL`-vs-`0` distinction between `get_strata()` and
  `get_num_strata()` now stated. Examples cover the three getters that had none.
* `confirmation_getters` example only ever showed the getters returning `NULL`; it now
  attaches a confirmation and shows them returning something. `confirmation_setters`
  now exercises `change_confirmation()` and the `confirmation_type` column.

### Section 3 — Reshaping

Fixed:
* **Bug in the `align_weeks` example:** it passed `case_col = "observation"` to
  `tbl_now()`. There is no such argument — `tbl_now()`'s is `case_count` — so the
  value was swallowed by `...`, stored as a stray attribute, and the count data was
  built as a **linelist**. See the open question on `...` below.
* `align_weeks` `@details` ended mid-sentence ("...(if applied to a `tbl_now`) or").
* The `align_weeks` example indexed `flutbl[413484, ".delay"]` — a magic row number —
  to make its point. Replaced with the share of non-integer delays, which states the
  point directly. Also subset to one state: **15.4s -> 1.5s**.
* `week_2_date` folded onto the `align_weeks` page; `censor_confirmation_delays_above`
  folded onto `censor_delays_above` (exact counterparts).
* `complete_zeroes` and `is_weekday` rewritten around why a practitioner would reach
  for them.

**Naming fixes applied** (see the consistency survey below):
* `censor_delays_above(data=)` and `censor_confirmation_delays_above(data=)` -> `x`.
* `censor_delays_above(quiet=)` and `censor_confirmation_delays_above(quiet=)` ->
  `verbose=`, default `TRUE`. Tests updated.

**Conflict documented, not silently changed:**
* `align_weeks(align_on_day=)` numbers weekdays from **Sunday** (`lubridate::wday()`
  default); `is_weekday(weekend_days=)` numbers them from **Monday**
  (`week_start = 1`). Both are now documented explicitly and cross-warn in a shared
  "A warning about weekday numbers" section. Changing either silently would shift
  existing user results by a day with no error, so this is left as a decision for the
  maintainer.

## Parameter-consistency survey (whole package, 148 exported functions)

First argument: `x` in 116. Deviations and the verdict on each:

| Function(s) | First arg | Verdict |
|---|---|---|
| `tbl_now()`, `tbl_now_from_*()` (6) | `data` | **Keep** — input is not a `tbl_now` yet. |
| `align_weeks()`, `week_2_date()` | `.data` | **Keep** — tidyverse convention for data-frame verbs; consistent with each other. |
| `as_tbl_now()`, `autoplot()` | `object` | **Keep** — fixed by the S3 generics. |
| `is_weekday()` | `date` | **Keep** — takes a Date, not a `tbl_now`. |
| `list_nowcast_methods()`, `nowcast_weights()`, `engine_*()`, `nowcast_ensemble()` | various | **Keep** — do not take a `tbl_now`. |
| `censor_delays_above()`, `censor_confirmation_delays_above()` | `data` | **FIXED -> `x`** |
| `diagnose_batches()`, `diagnose_batch_shape()`, `simulate_batch()`, `transport_discriminant()` | `data` | **TO FIX -> `x`** (Diagnosing section) |
| `engine()`, `nowcast_fit()`, `nowcast_tidy()` | `method` | **TO FIX -> `engine`** (Fitting section) — leftover from the removed `nowcast_method()`. |

Other shared names:
* `verbose` (20 functions) vs `quiet` (5). The three converters that have **both**
  use them for different channels (`verbose` = the conversion summary, `quiet` = the
  lossy-conversion warning); document that distinction rather than merge them.
* `axis` (16), `max_delay` (9), `alpha` (4), `by` (7) are used consistently.
* `level` (3, credible-interval width) vs `quantile_levels` (10, the vector of
  quantiles). Different things; names are fine but must be distinguished in prose.

## Open question for the maintainer
`tbl_now(...)` stores unmatched arguments as attributes, so a **misspelled argument
name is accepted silently** — that is how `case_col = "observation"` slipped into a
shipped example and mis-typed the data. Worth considering a warning for unknown names
that are near-misses of real ones. Behaviour change, so not done in this pass.

### Sections 6-7 — Summarising and Diagnosing

Cross-cutting fix (13 blocks, all sections): a roxygen block that opens with a bare
`r lifecycle::badge()` paragraph gets that badge as its **entire** `@description`, with
the real prose falling through to Details. Every affected page shipped with an empty
Description in `?fn` and in the pkgdown index. Lead paragraph promoted to
`@description`, remainder tagged `@details`. None remain.

Fixed:
* `case_autocorrelation(ndata, lag = 1)` in the shipped example — the argument is
  `lags`. It worked only through R's partial matching.
* `nowcast_summary_components` exemplified 8 of its 12 functions;
  `nowcast_diagnose_components` 6 of its 10. Both now cover all of them, grouped by
  the question they answer.
* `library(tbl.now)` / `data(..., package = "tbl.now")` boilerplate removed from three
  batch examples — the package is already attached when its own examples run.
* `simulate_batch`'s example planted a batch and then showed nothing. It now shows
  that no cases are lost, and that `diagnose_batches()` recovers the planted date —
  which is the whole reason the function exists.
* `tbl_now_summary` computed `summary()` four times over: **4.5s -> 2.9s**.

**Naming fixes applied:** `diagnose_batches()`, `diagnose_batch_shape()`,
`simulate_batch()`, `transport_discriminant()` take `x` rather than `data`. Two
internal helpers (`.batch_check_tbl_now()`, `.batch_confirmation_axis()`) hardcoded
`{.arg data}` in their error messages, so a user calling `diagnose_batches(x = ...)`
was told that `data` must be a `tbl_now`. Both renamed. 108 batch tests pass.

Full test suite after the weekday-numbering and near-miss-warning changes: **0
failures, 0 errors**.

### Section 8 — Plots

* `plot_reporting_process()` and `plot_epidemic_process()` merged onto one page.
  Identical signatures, mirror images: the entire diagnostic idea is *comparing* them
  (a spike in the reporting process with nothing under it in the epidemic process is a
  batch), and that is invisible when they sit on separate pages.
* `autoplot.tbl_now` had **no `@seealso` at all** — the entry point to the whole
  plotting side of the package, with no way out of it.
* The other 13 plot pages had `@seealso` blocks that were bare link lists
  (`[diagnostic_plot()].`). Rewritten so each link says what it is for, and so the two
  galleries — `autoplot()` for the case counts, `diagnostic_plot()` for the reporting
  process — are named as such from every panel.
* `plot_reporting_triangle`'s example now says what the blank upper-right wedge is
  (the future, which is what a nowcast fills in) rather than just drawing it.

All 14 plot examples run in 21.6s combined.

### Sections 9-14 — Fitting, Ensembles, Converters, Tidying, dplyr, Datasets

**Naming fixes applied:**
* `nowcast_fit(method=)` / `nowcast_tidy(method=)` -> **`engine=`**. The docs admitted
  the problem in a parenthesis ("the argument is called `method` for historical
  reasons"): `engine()`'s own page defines an engine as "the object `nowcast_fit()` and
  `nowcast_tidy()` dispatch on", and that is what arrives. Renamed in the 2 generics,
  the 14 built-in methods, the tests and the custom-backends article. No method body
  used the name -- it was a pure dispatch placeholder. `tools::checkS3methods()` is
  clean and the extension point still works end to end.
* `engine(method=)` and `list_nowcast_methods()` **keep** "method": in the package's
  vocabulary a *method* is the name of a backend and an *engine* is that name plus its
  arguments. That distinction is coherent and worth preserving.
* `tbl_now_from_data_table()` and `tbl_now_from_tsibble()` now accept **tidy-select**
  (a bare column name) as well as strings, matching `tbl_now()` and the rest of the
  package. `as_tbl_now.data.table()` and `as_tbl_now.tbl_ts()` were evaluating the
  promise instead of forwarding it, so bare names failed there too; both now embrace.
  Strings keep working -- tidy-select accepts them -- so nothing breaks.
* The three converters that carry **both** `verbose` and `quiet` now say what the
  difference is (`verbose` = the conversion summary, `quiet` = the lossy-conversion
  warning) rather than leaving the reader to guess.

**Examples that could not run.** The scoring and fitting half of the package had *no*
executable documentation: `run_nowcast()`, `nowcast_backtest()`, `nowcast_weights()`,
`score_nowcast()` and `as_scoringutils()` all had their only examples inside
`\donttest{}` behind `requireNamespace()`. Each now leads with a runnable example --
scoring is demonstrated by building the nowcast and its truth by hand, and fitting by a
five-line `carry_forward` engine defined in the example itself, which doubles as
documentation of the extension point. The real-model versions are kept underneath.

**`if (FALSE)` eliminated.** `tidy.epidist_fit` and `tidy.nowcast_backtest` used
`@examplesIf FALSE`, which renders as `\dontshow{if (FALSE) ...}`. The backtest example
is now genuinely runnable; the epidist one guards on the package and wraps only the
Stan fit in `\donttest{}`. No `if (FALSE)` remains anywhere in `man/`.

**Merges:** `as_scoringutils` -> `score_nowcast`; `plot_reporting_process` ->
`plot_epidemic_process`; `names_tbl_now` and `money_tbl_now` -> `assign_tbl` (three
pages describing three operators that all do one thing: keep the class alive).

**Datasets:** `denguedat` and `mpoxdat` had `\format{A data frame.}` and an example
that was only `data(denguedat)`. All six now describe every column with its type, and
show the data becoming a `tbl_now`. `mpoxdat` described itself as "line-list data with
each row representing case counts" -- it is count data. Two "Its a" -> "It is a".
Keywords standardised to `datasets` (they were `dengue`, `mpox`, `flu influenza`,
`covid`).

Note: dataset pages use `\format`, not `\value` -- `\value` is for functions, and
`R CMD check`'s value check skips `\docType{data}`.

## Final verification

`R CMD check --as-cran --no-manual` on the finished tree:

```
Status: OK
Duration: 19m 36.2s
0 errors | 0 warnings | 0 notes
```

Includes the full test suite (OK), all examples on the default path (77s) and with
`--run-donttest` (219s), Rd files / metadata / line widths / cross-references,
S3 generic-method consistency after the `method` -> `engine` rename, missing
documentation entries, code/documentation mismatches, `\usage` sections and Rd
contents -- all OK.

## ISO weekday change — impact verification

`align_weeks(align_on_day =)` was renumbered from Sunday-first to ISO (1 = Monday).
Checked that nothing outside the tests moved.

**1. Who passes `align_on_day` at all.** A grep over the whole repo (excluding
`docs/`): only `tests/testthat/test-tbl_now_align_week.R`, which was updated with the
change. No vignette, article, `data-raw/` script, README or R source passes it. The
two indirect users go through the default:

* `vignettes/tbl.now.Rmd:1216` — `align_weeks(df, date_col = date)`
* `data-raw/converter_matrix.R:75` — `tbl_now(..., align_weeks = TRUE)`

**2. Byte-comparison of every default path.** A worktree at `8d7a35b~1` (pre-change)
and the current tree were run over the same inputs and the results compared with
`identical()`:

| path | n | result |
|---|---|---|
| `align_weeks(df, date_col=)`, the vignette's call | 401 | identical |
| `type = "epi"` / `type = "iso"` | 401 each | identical |
| `align_weeks(<tbl_now>)` — event, report, `.delay`, `now` | 4000 | identical |
| `tbl_now(align_weeks = TRUE)` on FluSight Texas | 8539 | identical |
| confirmation-date alignment | 42 | identical |
| `week_2_date()` | 52 | identical |

**3. The vignette's own chunk**, run verbatim under both versions, produces the same
three rows and the same `Sunday` labels. Its prose ("Align to Sundays") still holds.

**4. `R CMD check`** re-built the vignette outputs: OK.

**5. `is_weekday()` was not changed** — it already numbered from Monday. No vignette,
README or `data-raw/` script passes numeric `weekend_days`; the one vignette call
(`tbl.now.Rmd:978`) uses the character default.

**Migration for an explicit caller:** subtract one, wrapping 1 to 7.
Old `1`(Sun)→`7`, `2`(Mon)→`1`, `3`(Tue)→`2`, ... `7`(Sat)→`6`.

## A flaky test worth knowing about

`R CMD check` on the final tree once came back `1 ERROR`:

```
-- Failure ('test-engines-matrix.R:153:7'): all 24 shapes are handled: epinowcast --
- "epinowcast / weeks / count-incidence / 2 strata / 0 cov: External command failed
   with exit code 2. This can happen when the disk is full in the temporary
   directory (...). See ?fread for the tmpdir argument."
[ FAIL 1 | WARN 57 | SKIP 3 | PASS 3864 ]
```

It is **not** a code regression:

* the error is `data.table::fread` failing to run an external command, and its own
  message names a full temp directory as the cause;
* repeated concurrent `R CMD check` runs had left **3.9 GB** in
  `/var/folders/.../T`; after clearing it, `test_file("test-engines-matrix.R")`
  passes **71/71 with zero** "External command failed" occurrences;
* the same suite, with the same converter code, passed `[23m/13m] OK` in the
  previous full check. The only code added between the two runs is
  `R/example_engine.R`, which cannot reach `tbl_now_to_epinowcast()`;
* the whole file is `skip_on_cran()`, so it never runs on CRAN at all.

Worth remembering when a `data.table`-backed engine test fails for no reason: clear
the temp directory before believing it.

### Re-run on a clean temp directory

```
Status: OK
Duration: 20m 25.3s
0 errors | 0 warnings | 0 notes
```

examples 77s OK · `--run-donttest` 216s OK · tests `[24m/14m]` OK · vignettes
re-built OK. No per-example timing table, i.e. **no example exceeds 5s** — the run
before the Stan examples were trimmed named four, the worst at 87s.

## Removing `\donttest{}` entirely (2026-08-31)

All eight blocks are gone. Nothing in `man/` contains `\donttest`, `\dontrun` or
`if (FALSE)`; verified per-file across all 88 Rd pages.

Five were wrapped only for speed and cost under a second each once unwrapped: the two
plot galleries and the three `baselinenowcast` fits, which already sat behind
`requireNamespace()` guards. Three needed work:

| topic | before | after | how |
|---|---|---|---|
| `tbl_now_epinowcast` | 16.0s CPU | 3.6s CPU | later reference-date window; `setDTthreads(2)` |
| `tidy.estimate_dist` | 8.1s | 1.1s | `stan_opts(samples = 100, chains = 1)`, `try()`-guarded |
| `tidy.epidist_fit` | 87s | ~59s | `chains = 1, iter = 200`, `try()`-guarded |

**`tidy.epidist_fit` cannot be made fast.** 62.0s on 5,426 rows against 63.6s on 305:
the cost is fixed Stan setup, not data, and a second fit in the same session costs the
same, so it is not compilation caching either. `epidist` is not on CRAN, so the
`@examplesIf` guard means CRAN never runs it; it costs only machines that installed
epidist deliberately. It is the one example over 5s.

Unwrapping pushed four examples over the threshold; they were tuned back by narrowing
data windows and, for `autoplot`, drawing two of the five demonstrations as single
panels rather than full galleries.

### What `R CMD check` measures

The timing table trips on **user + system OR elapsed** > 5s. `tbl_now_epinowcast` read
16.0s CPU against 3.0s elapsed purely because data.table used every core. Capping
threads at 2 -- which CRAN asks for anyway -- fixed it without touching the data.

## The CI failure: a missing Stan backend is not a broken engine

`test-engines-matrix.R` did `skip_if_not_installed(engine)`, which passes on CI because
`epinowcast` *is* installed -- but `cmdstanr` is not, so all 24 shapes failed with
"cmdstanr is required but not installed" and one assertion emitted eighteen lines that
said nothing about shapes.

`available_engines()` now means installed **and** runnable, and
`engine_backend_available()` checks for a *built CmdStan*, not merely the `cmdstanr`
package -- the R package installs happily without one and the fit fails just the same.
Verified by hiding `cmdstanr`: epinowcast reports unavailable, the others do not.

## checktor

Everything passes except one false positive:

* `diagnose_package_size` reports **14232 MB**. That is the working directory, which
  contains `devel/COVID-19_Case_Surveillance_Public_Use_Data_20260710.csv` -- the 14 GB
  CDC source for `covid_us`, both `.gitignore`d and `.Rbuildignore`d. The **built
  tarball is 1.3 MB**.
* `diagnose_unexported_example_ns` errors inside checktor itself:
  `invalid regular expression '(?<!:)\b[\.tbl_now\s*\('`. checktor builds that pattern
  from the package name and does not escape our dot-prefixed internals. Not ours to fix.

`diagnose_suggested_in_examples` caught a **real bug this audit introduced**: the
container-class examples called `tbl_now_to_baselinenowcast()`,
`tbl_now_to_surveillance()` and `tbl_now_to_EpiNow2()`, all of which call `.need_pkg()`,
with no guard. `R CMD check` missed it because all three packages are installed here.
Now `@examplesIf`-guarded. checktor found two of the three; the third had the same bug
but a comment that did not use `::` notation.

`diagnose_commented_examples` reads `#` at the start of an example line as commented-out
code and `##` as prose -- the base R convention, which `?lm` follows. The audit's prose
comments used `#`, taking the count from 15 files to 36. Converted 67 lines across 26
files inside `@examples` blocks; no prose changed, only the marker.

### Final verification (2026-08-31)

`R CMD check --as-cran --no-manual` on the finished tree:

```
Status: OK
Duration: 18m 51.5s
0 errors | 0 warnings | 0 notes
```

examples `[145s/146s]` OK · tests `[25m/14m]` OK · vignettes re-built `[29s]` OK.

Example timing table: **one** entry, `tidy.epidist_fit` at 60.5s (see above -- it is
irreducible, and CRAN does not run it because `epidist` is not on CRAN). Before this
pass the table named four, up to 87s.

`checktor`: 18 diagnostics, 17 pass. The one that does not is `diagnose_package_size`,
which measures the source directory (14 GB of gitignored CDC source data in `devel/`)
rather than the built tarball (**1.3 MB**). `diagnose_documentation_issues` is clean on
every count except its own `unexported_example_ns` regex bug.
