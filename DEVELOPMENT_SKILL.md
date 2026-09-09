---
name: tbl-now-development
description: Develop and maintain the tbl.now R package, including tbl_now invariants, dplyr behavior, converters, cross-engine results, diagnostics, documentation, and tests. Use only for changes to tbl.now itself; use SKILL.md for package usage.
---

# Develop `tbl.now`

Read `SKILL.md` first for the user-facing contract. This guide contains the
non-obvious invariants needed when changing the package.

`tbl.now` owns:

- declaration, validation, manipulation, and diagnostics of `tbl_now` data;
- conversion to and from other nowcasting packages;
- the common `tbl_nowcast` result, cross-engine fitting, retrospective
  evaluation, scoring, and ensembling.

It does not own an engine's statistical model or native fit diagnostics.
`diseasenowcasting`, for example, owns its model components and RTMB fit while
returning the shared `tbl_nowcast` result.

## Start from the repository

Before adding an exported function, search the current namespace and source by
both verb and noun. Prefer extending an existing function when the new behavior
is the same concept with another argument.

```sh
rg '^export' NAMESPACE
rg '^[A-Za-z.][A-Za-z0-9._]*\s*<-\s*function' R
rg -n 'concept|synonym' R tests/testthat vignettes
```

Inspect the implementation, tests, roxygen, `NEWS.md`, and `_pkgdown.yml` for
the affected surface before editing. Do not treat this guide as a substitute for
the code.

## Preserve the `tbl_now` contract

A `tbl_now` is a tibble with declared roles and derived columns. It must remain
usable in dplyr pipelines.

The three data types are:

- `linelist`: one case per row, no count column;
- `count-incidence`: newly reported count per cell;
- `count-cumulative`: cumulative level as of each report date.

Required behavior:

- A line list cannot encode a zero period.
- Cumulative data are not additive. De-accumulation may produce negative
  increments after downward revisions.
- `to_count()` cannot reconstruct a line list and pools undeclared columns.
- Preserve `NA` as unobserved and `0` as observed zero.
- Time grids used for summaries or modelling extend to `now`, not merely the
  latest row, and should be global across strata unless the API says otherwise.

Use getters inside package code; do not read public metadata with raw `attr()`.
Getters for roles return column names, not vectors.

When adding an attribute:

1. Add it to the required/rebuild machinery where appropriate.
2. Add an exported, documented, tested getter.
3. Decide whether its named column is protected.
4. Audit every manual rebuild, including `do.call(tbl_now, ...)`, dplyr methods,
   `update()`, `align_weeks()`, and converters.
5. Test objects with no strata, multiple strata, lazy effects, censoring, and no
   revision axis as applicable.

Protected user columns include declared dates, case count, revision type, and
censoring flags. Generated protected columns include `.event_num`,
`.report_num`, `.delay`, and the optional `.revision_num` and
`.revision_delay`. Removing or renaming a generated protected column must demote
with `.demote_to_tibble()` and remove all `tbl_now` attributes while preserving
unrelated user metadata.

For internal reshaping, strip the class and rebuild with the established helpers
such as `.strip_tbl_now()`, `.tbl_now_rebuild()`, and
`.revision_rebuild_args()` rather than reproducing constructor logic.

## dplyr and grouping

Every exported function accepting a `tbl_now` must deliberately handle a
`grouped_tbl_now`. Add a grouped test that checks:

- the call does not abort;
- grouping is preserved or deliberately removed;
- the result agrees with the ungrouped calculation.

`summarise()` and `reframe()` may return a plain tibble when the result no longer
satisfies the `tbl_now` contract. `rowwise()` deliberately demotes. Locale can
affect row ordering: use dplyr ordering or `order(..., method = "radix")` where
the C-locale behavior matters. Day and month names also vary with `LC_TIME`:
do not hardcode localized names or compare calendar values through
`weekdays()`, `months()`, or labelled factors in tests and internal logic. Use
numeric calendar fields such as `lubridate::wday()` and `lubridate::month()`.
Fixed-language labels are appropriate only when they are an intentional,
documented part of the public or backend contract; test their calendar mapping
with numeric positions rather than the session locale's translated names.

## Revisions and censoring

The optional third axis has the chronology event <= report <= revision <=
`now`. `.revision_delay` is report-to-revision time. Code must work when
`has_revision(x)` is false.

Canonical revision outcomes are `"confirmed"`, `"retracted"`, `"pending"`,
and `NA`. `revision_levels` is the only recoding route and must remain
idempotent because rebuilds reapply it. Moving `now` backward masks future
revisions back to pending; it must not reveal future information in a backtest.

There are two independent censoring flags. Each says an axis date is a bound,
not an exact arrival. Preserve the case and outcome. Only analyses of arrival
timing, such as batch detection on that axis, should drop censored rows. Keep
`.batch_report_increments()` conservative by default; its callers choose whether
to exclude censored arrivals.

If an external format cannot represent per-case censoring within a cell, call
`.tbl_now_collapse_censoring()` and warn. Count data are summed over the flag;
line lists drop the flag without dropping cases. `tbl_now_to_epidist()` is the
intentional exception because a delay model can consume censoring.

## Temporal effects

Temporal effects are lazy specifications. `add_temporal_effects()` records them;
`compute_temporal_effects()` materializes columns. Dplyr methods preserve the
spec without computing it. Converters materialize only when required.

When time units are aggregated, coarsen compatible specs, rescale Fourier
periods, drop incompatible effects, and remove stale materialized columns.
Holiday calendars are optional `almanac` inputs and must not become a hidden
hard dependency.

## Converters and engines

A converter is complete only when the target format's capabilities have been
handled explicitly:

- outbound `tbl_now_to_<package>()`;
- inbound `tbl_now_from_<package>()` when a meaningful inverse exists;
- an `as_tbl_now()` method for the foreign class;
- the target package's own coercion generic as a thin wrapper, or a recorded
  reason it has none in the converter registry test;
- round-trip tests, including documented losses;
- grouped, stratified, censored, and relevant data-type tests;
- a `tidy()` method when the package returns nowcasts.

Use `.need_pkg()` for optional packages, `.warn_lossy_conversion()` for lost
semantics, `.pool_undeclared()` where the target cannot carry extra dimensions,
and `.tbl_now_collapse_censoring()` where needed.

Converters and engine adapters must:

- accept line lists and incidence counts at minimum, unless the target truly
  cannot;
- complete count grids to `now` internally when required;
- test a fixture whose final event periods have no reports;
- keep strata shapes and names intact;
- preserve integer delay units and state any lossy change;
- exercise the destination package's actual entry point, not only inspect the
  converted object.

Do not suppress backend warnings wholesale. Suppress routine messages only;
fit failures and convergence warnings are part of the result's credibility.
Seed stochastic fits per `(engine label, now date)`, immediately before each
fit, so results do not depend on execution order.

The common result schema is:

- predictions: event date, strata, `.quantile_level`, `.value`;
- draws when available: event date, strata, `.draw`, `.value`;
- method, fit, `now`, source data, call, and metadata.

Backend-specific operations use the untouched native fit. Generic `tidy()`,
plotting, scoring, forecast coercion, and ensembling use the common result.
Never fabricate arbitrary quantiles for a backend without draws.

S7 class names can contain `::`, which S3 method names cannot represent. Use
the existing `.onLoad()` registration pattern where required. Re-export
`generics::tidy`; do not create a competing generic.

## Summaries and diagnostics

`validate_tbl_now()` and `diagnose()` share `.tbl_now_findings()`. Add structural
checks there so construction and diagnosis cannot diverge. Keep cheap class and
metadata checks in `is_tbl_now()`; do not move data scans into it.

Findings distinguish:

- `ok`: the check ran and passed;
- `skipped`: the check could not apply;
- `note`: useful information that should not make construction noisy;
- `warning` / `error`: data problems at their established severity.

Do not promote findings casually: validation runs during construction and many
rebuilds. Statistical tests with tuning choices or optional dependencies belong
in explicit functions such as `diagnose_drift()` or `diagnose_batches()`, not in
the structural `diagnose()` report.

All `summary()` components share one schema; all diagnosis components share
another. Return the established columns and meanings so `bind_rows()` remains
valid. Reuse the package's quantile, maturity, grid, and stratum-label helpers so
plots and reports agree. Unreviewed statistics stay out of default reports and
retain their warning at every call.

## Plots and print methods

Use `tbl_now_palette()` by semantic role. Do not add hex values outside
`R/palette.R`. Validate palettes and size controls with the existing helpers;
`size` and `linewidth` are multipliers, not replacements for every geom value.

Anything emitted by a `print()` method must go to stdout via `cli::cat_*()`.
Register base print methods with `@exportS3Method base::print`, and test
auto-printing with `capture.output(x)`. After documentation, verify NAMESPACE
contains `S3method(base::print, ...)`, not a method on a shadowed generic.

## Style, dependencies, and documentation

- Use the native pipe `|>`, snake case, `<-`, and tidyverse style.
- Prefer vectorized dplyr/tidyr code for data frames; use base R where it is
  clearer for matrices, S3/S7 plumbing, or tight internals.
- Match existing `function(x)` style in `R/`; concise lambda syntax is fine in
  vignettes where already used.
- Internal helpers are dot-prefixed and documented with `@noRd`.
- Use `cli` for messages and warnings; no emoji in R output.
- Document with roxygen. Never hand-edit `NAMESPACE` or `man/*.Rd`.
- Update `NEWS.md` for user-visible changes and `SKILL.md` when the public
  workflow changes.
- Add every exported topic to the explicit reference index in `_pkgdown.yml`.
- Keep runnable examples runnable; do not hide failures with `\dontrun{}`,
  `\donttest{}`, `if (FALSE)`, or warning suppression.
- Prefer CRAN dependencies. Optional integrations belong in `Suggests`, behind
  `requireNamespace()`, with repository metadata only when unavoidable.

`pkgdown` renders from the installed package, so install current source before
debugging site output. Use stable source-relative resources; do not use knitr
child paths containing `../`.

## Verification

Run the narrowest relevant tests while iterating, then the full package checks
when the change warrants them:

```r
devtools::test(filter = "relevant-file")
devtools::document()
devtools::test()
devtools::run_examples(run_donttest = TRUE, run_dontrun = TRUE)
devtools::check()
pkgdown::check_pkgdown()
checktor::checkup()
```

Before finishing, confirm that:

- new behavior has regression tests, including grouping and each relevant data
  type, strata, revision, censoring, and `now` edge;
- user-facing docs, `NEWS.md`, and the usage skill agree with the code;
- generated documentation and NAMESPACE are current;
- no unrelated file was reformatted;
- `R CMD check` and CRAN-oriented checks are clean, or any unrun/failed check is
  reported precisely.
