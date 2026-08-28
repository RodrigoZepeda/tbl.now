# `tbl.now` — development guide

This file teaches an LLM (or a new human contributor) how to **develop** the
`tbl.now` package. It is not a user manual: `SKILL.md` explains how to *use* the
package, and the vignettes explain how to *apply* it. Read this before changing
code.

---

## 1. What the package is

`tbl.now` extends `tibble` into a class that stores **epidemiological nowcasting
data**: case data where every observation carries two dates — when the event
happened and when it was reported. The gap between them is the **reporting
delay**, and the whole package exists to keep that structure honest, queryable
and convertible.

A `tbl_now` is a tibble plus attributes. It is deliberately *not* an S4/R6
object: it must keep working inside a `dplyr` pipeline.

The package does **not** fit nowcast models itself. It prepares data for the
packages that do (`diseasenowcasting`, `baselinenowcast`, `epinowcast`, `NobBS`,
`surveillance`, `nowcaster`, `epidist`) and normalises what they return.

---

## 2. The three data types

Every `tbl_now` has exactly one `data_type`, and almost every bug in this package
traces back to code that assumed the wrong one. Check with `get_data_type(x)`.

| `data_type` | one row is | case column | can convert to |
|---|---|---|---|
| `"linelist"` | **one case** | none (`get_case_count()` is `NULL`) | both count types |
| `"count-incidence"` | cases *newly reported* for an (event, report) pair | required | `count-cumulative` |
| `"count-cumulative"` | cases reported **so far** for that event as of that report | required | `count-incidence` |

Rules that follow from this, and that you must not break:

* **A line list cannot represent a zero.** An event date with no reports has no
  rows at all. Any code that builds a time grid from the rows it was handed will
  silently stop short of the `now`. `complete_zeroes()` fixes this for count
  data. For the line-list engines the grid is the **caller's** job:
  `tbl_now_to_surveillance()` deliberately does *not* bake it in — it returns a
  line list and leaves `now` and `control$dRange` to you, because the object
  already carries them (`get_now()`, `get_event_units()`) and the converter
  cannot know which window you mean to fit. Pass them at the call site, as the
  `surveillance` section of `vignettes/articles/nowcasting-models.Rmd` does.
* **`count-cumulative` is not additive.** Never `sum()` it across delays. To get
  increments use `to_count(x, to = "count-incidence")`, which de-accumulates —
  and can produce **negative** values when a total was revised downward.
  Downstream code must handle negatives or refuse them explicitly, never assume
  they cannot happen.
* **`to_count()` cannot rebuild a line list.** Aggregation is one-way.
* **`to_count()` sums over undeclared columns.** A column that is neither
  declared (strata/covariates) nor protected is silently pooled away. If a test
  shows "impossible" spread in a boxplot, look for an undeclared column before
  suspecting the plot.

---

## 3. Attributes and getters

### The attributes a `tbl_now` carries

| attribute | holds | getter |
|---|---|---|
| `event_date` | column name of the event date | `get_event_date()` |
| `report_date` | column name of the report date | `get_report_date()` |
| `data_type` | one of the three above | `get_data_type()` |
| `case_count` | column name of the count (count types only) | `get_case_count()` |
| `strata` | character vector of stratifying columns | `get_strata()`, `get_num_strata()` |
| `covariates` | character vector of covariate columns | `get_covariates()`, `get_num_covariates()` |
| `is_censored` | column name of the censoring flag, or `NULL` | `get_is_censored()` |
| `now` | the "as of" date | `get_now()` |
| `event_units` | `"days"`, `"weeks"`, `"months"`, `"years"`, `"numeric"` | `get_event_units()` |
| `report_units` | same | `get_report_units()` |
| `temporal_effects` | **lazy** effect specs | `get_temporal_effects()` |
| `computed_temporal_effect_cols` | materialised effect column names | `get_temporal_effect_cols()` |
| `confirmation_date` | column name of the confirmation date, or `NULL` | `get_confirmation_date()`, `has_confirmation()` |
| `confirmation_type` | column name of the outcome, or `NULL` | `get_confirmation_type()` |
| `confirmation_units` | same set as `report_units` | `get_confirmation_units()` |

### The rules

1. **Always use the getter. Never `attr(x, "...")`.** Attribute names are an
   implementation detail; the getters are the contract. This applies to your own
   code inside the package just as much as to user code.
2. **If you add an attribute, you add a getter in the same commit.** No
   exceptions. Export it, document it under the `nowcast_data_getters` topic,
   and test it. An attribute without a getter is a private field the user will
   end up reading by hand — which is exactly the coupling the getters exist to
   prevent.
3. **A getter returns the smallest useful thing.** `get_nowcaster_strata()`
   returns the `bins_age` vector, not a list to dig through. Prefer the value a
   caller passes straight to the next function.
4. **Getters return column *names*, not data.** `get_event_date(x)` is a string.
   Get the values with `x[[get_event_date(x)]]`.
5. **Temporal effects are lazy.** `get_temporal_effects()` returns *specs*;
   the columns do not exist until `compute_temporal_effects()` runs. Converters
   call `.materialize_temporal_effects()` for this. Calling
   `compute_temporal_effects()` before `add_temporal_effects()` is a no-op.

### The confirmation process (the optional third date)

`event <= report <= confirmation <= now`. It is **optional**: most objects have
no confirmation, so every code path must work when `has_confirmation(x)` is
`FALSE`, and `.confirmation_group_cols(x)` returns `character(0)` there so it can
be spliced into a grouping unconditionally.

1. **Every rebuild must carry it.** `do.call(tbl_now, c(list(...), .confirmation_rebuild_args(x, data)))`.
   This is the same silent-drop hazard as strata: `summarise()`, `group_by()`,
   `ungroup()`, `update()` and `align_weeks()` each rebuild by hand, and each one
   dropped the confirmation until it was spliced in. **Grep for
   `do.call(tbl_now` before adding an attribute** and fix every site.
2. **`"pending"` means no date, not a missing date.** A pending case is reported
   and still waiting. Anything that counts arrivals on the confirmation axis must
   drop pending rows -- counting them invents an arrival on a date they do not
   have.
3. **Two different delays.** `.confirmation_delay` is the laboratory's turnaround,
   measured **from the report**. The `axis = "confirmation"` diagnostics measure
   **from the event**, so the two axes are comparable. Do not mix them up.
4. **Do not duplicate a diagnostic for the new axis.** Add
   `axis = c("report", "confirmation")` and forward it to
   `.batch_report_increments()` / `.batch_registration()`, which is where the
   axis is actually swapped. One switch point, no parallel implementations.
5. **`now` is confirmation-aware.** `infer_now()` takes the max over both, and
   setting `now` before the last confirmation is an error.

### Protected columns

`get_protected_cols()` = the user-given ones (event/report dates, censoring
indicator, case count) plus the generated ones (`.event_num`, `.report_num`,
`.delay`). Removing any of them **downgrades the object to a plain tibble**,
with a warning. This is why `dplyr::summarise()` on a `tbl_now` usually returns
a tibble: it drops the protected columns.

When you need to reshape inside the package, operate on
`.strip_tbl_now(x)` (a bare data frame) and rebuild the attributes, rather than
fighting the dplyr methods. See `.tbl_now_collapse_censoring()` for the pattern.

---

## 4. Function-choice hierarchy

When writing data-manipulation code **inside this package**, prefer in this
order:

1. **`tbl.now`'s own functions** — `to_count()`, `complete_zeroes()`,
   `align_weeks()`, `get_*()`, `add_*()` / `change_*()` / `remove_*()`.
   They know about the attributes and keep the object valid.
2. **`dplyr` / `tidyr` / `tibble`** — for anything the package does not already
   express. `dplyr` verbs have `tbl_now` methods and mostly preserve the class.
3. **base R** — last resort, for things the tidyverse genuinely does not cover
   (`switch()`, `vapply()`, `seq()`, matrix work, S3 plumbing).

Rewriting a `tbl.now` helper's logic by hand is a bug waiting to happen: the
helper handles the data types, the units and the `now` edge, and your inline
version will handle whichever one you were thinking about that day.

### Before you write a new function, prove it does not exist

This has gone wrong twice, in the same shape both times, and neither time did a
test catch it -- the duplicate had a *different body*, so nothing mechanical
noticed.

* `nowcast_truth()` was written, exported and documented before anyone noticed
  it was `get_latest_reported_cases()` with the class stripped and the count
  column renamed. It shipped as a second public name for one idea.
* `.strata_from_labels()` and `.split_strata_labels()` were written a few hours
  apart, in the same file, by the same author, to split the same `" | "` label.

So the rule is a **pre-flight grep, and you say in the commit what you grepped
for**:

```bash
# 1. Does a getter already answer this question?
grep -n "^get_" R/getters.R

# 2. Does a helper already do this? Search the VERB, not your name for it.
grep -rn "split\|paste.*sep\|pool\|aggregate" R/ | grep "<- function"

# 3. Is there already an exported function in this area?
grep -n "^export" NAMESPACE | grep -i "<the noun you are about to use>"
```

Two questions decide it:

1. **Could an existing function answer this with different arguments?** Then add
   the argument. `score_nowcast(truth = )` learning to accept a `tbl_now` was
   the right fix; a `nowcast_truth()` to produce that input was not.
2. **Is the new thing a reshaping of an existing thing?** Then it is internal at
   most, and its documentation must name what it wraps. A reshaping is not a
   concept, and the user should not have to learn it as one.

When you genuinely need both, there is **one implementation** and the other
calls it. Two functions that split the same string are one bug waiting to be
fixed in one place only.

---

## 5. Style

* **Pipe:** native `|>`. Not `%>%`. The package does not depend on `magrittr`.
* **Style guide:** the [tidyverse style guide](https://style.tidyverse.org).
  Snake case, `<-` for assignment, spaces around infix operators. Aim for 80
  characters, but this is a target rather than a rule the codebase already
  meets — do not reflow unrelated lines to chase it.
* **Anonymous functions:** package code uses `function(x) ...` almost
  everywhere (~400 occurrences against a single `\(x)`), so match that in `R/`.
  The shorthand `\(x)` is used in the **vignettes**, inside pipes, where it
  reads better. Consistency with the surrounding file beats personal preference
  either way.
* **Documentation:** `roxygen2` for everything. Regenerate with
  `devtools::document()` — never hand-edit `man/*.Rd` or `NAMESPACE`.
* **Internal helpers** are `.`-prefixed and `@noRd`.
* **Messages** use `cli`. But see the pitfall in §9: `cli_*` writes to the
  *message* stream, which is invisible in a knitr chunk with `message=FALSE`.
  Anything a `print()` method emits must use `cli::cat_line()` / `cat_rule()` /
  `cat_bullet()`.
* **Comments explain *why*, not *what*.** Match the density of the surrounding
  file. A comment restating the code is noise; a comment recording the
  non-obvious constraint that forced the code is the point.
* **No emoji in R code or console output.** (`SKILL.md`'s capability tables do
  use `✅`/`❌` as status markers; that is a deliberate local convention for those
  tables, not licence to use emoji elsewhere.)

---

## 6. Colours

The default palette is **the `tbl.now` palette**, `.tbl_now_palette()`. Do not
introduce new hex codes in plotting code; take them from the palette.

The package has one visual grammar, and every plot must obey it:

* **Red** (`accent_red`, `light_red`) = the **reporting** process — report dates,
  delays, anything about *when we found out*.
* **Green** (`primary_green`, `light_green`, `dark_green`) = the **epidemic**
  process — event dates, case counts, anything about *what happened*.

A new panel that draws delays in green is wrong even if it looks nice. The
website CSS (`pkgdown/extra.css`) follows the same palette, which is why
`.alert-info` is green (event-date process) and `.alert-warning` is the
attenuated red used for intervals.

---

## 7. Writing a converter

Converters are the package's main external surface, and they have the most
rules. `R/converters.R` holds all of them.

### The full round trip is the deliverable

A converter is not finished when `tbl_now -> other package` works. You must do:

```
tbl_now  ->  other package  ->  tbl_now
```

Concretely, that means **all** of:

1. **`tbl_now_to_<pkg>()`** — the outbound converter.
2. **`tbl_now_from_<pkg>()`** — the inbound one, *where the package has an object
   that can carry the information back*. Some packages only consume data
   (`nowcaster`, `surveillance` take a plain line list), and then there is
   nothing to convert back from; say so in the docs rather than inventing a
   half-faithful inverse.
3. **An `as_tbl_now()` method** for the foreign class, so the generic works.
   Register it as `S3method(as_tbl_now, <foreign_class>)`.
4. **A round-trip test** asserting that converting out and back preserves the
   data. Where the round trip is lossy, test *what* survives and document what
   does not.

### The package's own coercion verb is part of the job

Someone fluent in `baselinenowcast` will reach for `as_reporting_triangle()`,
not for `tbl_now_to_baselinenowcast()`. Where the target package exposes a
coercion **generic**, register a `tbl_now` method on it, as a thin wrapper
around your converter:

```r
#' @rdname tbl_now_coercion_methods
#' @exportS3Method baselinenowcast::as_reporting_triangle
as_reporting_triangle.tbl_now <- function(data, ..., verbose = FALSE) {
  tbl_now_to_baselinenowcast(data, format = "matrix", ..., verbose = verbose)
}
```

A wrapper, never a second implementation, and quiet by default (`verbose =
FALSE`) because coercion is an idiom rather than a conversion you are watching.

Some packages have no such generic -- `epinowcast`, `EpiNow2` and `NobBS`
export none at all, and `surveillance`'s `as.epidata()` is a different concept.
That is fine, but it has to be *recorded*: `tests/testthat/test-coercion-methods.R`
holds a registry of every `tbl_now_to_*()` with either its generic or a written
reason there is none, and it **fails when a converter is missing from it**. It
also re-checks the "no generic" claims against the installed package, so if one
of them gains a coercion verb we find out.

### `tidy()` is part of the job

If the package **returns a nowcast**, you must also write a `tidy()` method
producing the standard frame: `event_date`, `stratum`, `estimate`, `conf.low`,
`conf.high`, `level`, `engine` (plus `q*` columns when the engine keeps draws).
Users must not have to learn six output formats.

The single exception is a package that returns **only a delay distribution**, not
per-date case estimates. `epidist` is the example: `tidy.epidist_fit()` returns a
*delay-shaped* table (`term`, `estimate`, `conf.low`, `conf.high`, `level`,
`engine`) instead, and that is correct — do not force a delay fit into the
nowcast schema.

Notes:

* Register with `@exportS3Method generics::tidy`. The generic comes from
  `generics`, so `tbl.now` composes with `broom` rather than masking it.
* If the foreign class name contains `::` (S7 classes do), an S3 method name
  cannot express it. Register it manually in `.onLoad()` — see
  `tidy_nowcast_prediction` in `R/zzz.R`.
* Check the **class order** of the foreign object. `epidist()` returns
  `c("brmsfit", "epidist_fit")`, so `broom.mixed`'s `tidy.brmsfit()` wins
  dispatch when loaded. Document that rather than pretending it cannot happen.
* Engines that keep draws can honour `probs`; those that do not must **error**
  via `.reject_probs()`, never silently approximate. Where the engine can compute
  quantiles at fit time (NobBS's `specs$quantiles`), document how.

### Censored delays

**Be careful with the censoring indicator.** A flag that is a property of the
*case* rather than of the delay puts a censored and an uncensored row in the same
`(event_date, report_date)` cell. A reporting triangle has one slot per cell, so
the extra dimension breaks the conversion outright.

Every converter that ends in a triangle or a keyed table must call
`.tbl_now_collapse_censoring(x, "<fn name>")` in its prologue, right after
`.assert_tbl_now()`. That helper:

* **count data** — sums the counts over the flag (totals unchanged), and warns;
* **line lists** — drops the column (one row per case, unchanged), and warns.

`tbl_now_to_epidist()` is the deliberate exception: estimating a delay
distribution is the one job that can *use* censoring, so it keeps the flag.

Note the two kinds of flag, because they behave differently and only one causes
trouble: one derived from the delay (`censor_delays_above()`) is constant within
a cell and harmless; a per-case administrative flag varies within a cell and
breaks things. Test with the second kind or your test proves nothing.

### Other converter rules

* **Warn about lossy conversions** with `.warn_lossy_conversion()`.
* **Check the package is installed** with `.need_pkg()`, and give the real
  install command (`nowcaster` is GitHub-only).
* **Accept line lists.** At minimum a converter must work with `linelist` and
  `count-incidence` input. If the target needs incidence counts completed to the
  `now`, do it *inside* the converter (`complete = "auto"`), so the user does not
  have to remember `to_count() |> complete_zeroes()`.
* **Respect `NA` vs `0`.** In a reporting triangle an `NA` cell means *not yet
  observed*; a `0` means *observed, and it was zero*. Completing count data that
  the user did not ask to complete destroys that distinction. This is why
  `complete` defaults to `"auto"` (line lists only) rather than `TRUE`.
* **Resolve units from the attribute**, never assume days. A weekly triangle
  whose `delays_unit` you ignore will read its delays as days and abort.
* **Ask how the target stratifies, not whether it can.** Three shapes exist and
  they need different things:
  * *many columns* (`epinowcast`'s `by`, a `tsibble` key, an `epidist` formula):
    hand over the columns themselves. Pasting them would be the bug.
  * *one column* (`NobBS::NobBS.strat(strata=)`,
    `EpiNow2::regional_epinow(region)`): the converter must **build** that
    column — `.add_strata_column()` — with a user-settable `strata_col` /
    `strata_sep`. Carrying the originals along is not enough: without the pasted
    column there is no argument the user can write, and a multiply-stratified
    object simply cannot be fitted.
  * *none at all* (`surveillance::nowcast()`): still build the column, because
    the answer is `split()` and that needs something to split on.

  Whenever you paste, refuse the paste if a stratum **value** contains the
  separator (`.paste_strata_labels()` does this): `tidy()` splits the label back
  into columns, and an ambiguous split attaches a nowcast to the wrong stratum
  with no error. Refuse to overwrite an existing column too.

  A test that only asserts "the strata columns are present in the output" does
  **not** cover this — that test existed and the bug shipped anyway. Assert that
  the target's own stratified entry point can be *called*, and that splitting on
  the result reproduces each stratum's total.

### Document it in the vignette

Every converter gets **its own section** in
`vignettes/articles/nowcasting-models.Rmd`, and that section must show **both**:

1. the **plain** conversion and fit (`dengue_now`), and
2. the **stratified** one, with temporal effects (`dengue_seasonal`) — including
   how that package expresses strata, or an honest statement that it cannot and
   what to do instead (`surveillance` needs one fit per stratum).

Follow the section pattern already in the file:

* a **visible** `tidy()` call, `eval=FALSE`, copy-pasteable, with no `head()` or
  `tail()` wrapper;
* a **hidden** `echo=FALSE` chunk rendering the cached result, so the output
  appears to come from the visible call.

**The precompute runs the article, it does not restate it.**
`data-raw/nowcast_models_precompute.R` `knitr::purl()`s
`nowcasting-models.Rmd`, sources the extracted chunks with
`TBL_NOW_RUN_MODELS=true`, and reads the fits back out **by name** into
`vignettes/articles/nowcast-comparison.rds`. `run_models` is `FALSE` during a
normal build, so the article never fits anything and shows the cached results
instead.

This replaced a script that kept its own copy of every fit next to a comment
asking whoever edited one to remember the other. They drifted, silently, because
the article never ran what it printed. Do not reintroduce that: **add your
engine to the article**, then add its object name to `DISPLAYED` / `PANELS` in
the precompute and re-run it.

Consequences to respect when you edit the article:

* a chunk that only *displays* a cached result (`show_fit()`, `show_tidy()`,
  `nowcast_panels()`, the figures) must be marked `purl = FALSE`, or the
  precompute will try to run it before the file it reads exists;
* every object the precompute names must actually be **assigned** in a chunk.
  An `eval = FALSE` chunk still gets purled and run, so illustrative code that
  cannot run needs `purl = FALSE` too;
* every package that appears in `nowcasts` needs an entry in `engine_colours`.
  The Summary figure turns `package` into a factor with those levels, so a
  missing one becomes a grey unnamed `NA` line with no error;
* chunk labels must be unique — a duplicate stops `knit()` and `purl()` alike.

Also add a row to the package table at the top of the article, and to the
converter matrix in `data-raw/converter_matrix.R`, which runs every converter
against every shipped dataset.

---

## 8. Testing

* Run with `NOT_CRAN=true`, or 424 tests silently skip:
  ```r
  NOT_CRAN=true Rscript -e 'devtools::load_all("."); testthat::test_local()'
  ```
* **You cannot measure the CRAN path with `test_local()`, and the mistake is
  invisible.** `testthat::test_local()` calls `local_assume_not_on_cran()`, which
  sets `NOT_CRAN = "true"` whenever the variable is empty; `devtools::check()`
  sets it too (`formals(devtools::check)$env_vars`). So `env -u NOT_CRAN Rscript
  -e 'testthat::test_local()'` runs **every** `skip_on_cran()` test anyway. Use
  `test_check()` -- what `tests/testthat.R` runs under `R CMD check` -- and
  assert you are on the path you think you are:
  ```r
  library(testthat); library(tbl.now)
  stopifnot(testthat:::on_cran())
  setwd("tests"); test_check("tbl.now")
  ```
  The two paths are not close: at 0.27.0 the CRAN path was **6.1 min** (1334
  tests, 424 skipped) and the full path **30.8 min**. Optimising the wrong one
  wastes a day. See `devel/TEST_SPEEDUP_BRIEF.md`.
* Guard optional packages with `skip_if_not_installed()`, and heavy work with
  `skip_on_cran()`.
* **Test the failure you actually fixed.** A test that exercises the harmless
  variant of a bug (delay-derived censoring instead of per-case) passes both
  before and after the fix and proves nothing.
* Prefer mocking to fitting. A `tidy()` method can be tested against
  `local_mocked_bindings()` instead of compiling a Stan model; verify against a
  real fit once, by hand, and keep the suite fast and deterministic.
* When behaviour changes deliberately, **update the tests that asserted the old
  behaviour** and say so in `NEWS.md`. Do not leave a stale assertion for someone
  else to trip over — that is how `test-autoplot.R` ended up asserting a default
  that had been changed weeks earlier.

### Why the suite was slow, and what actually fixed it

The suite's profile is **flat** — no file is more than 8% of the CRAN path — so
there was no one test to fix. The cost was spread over roughly 780 `tbl_now()`
constructions, and it was not in the data handling. It was in
`validate_tbl_now()`, which runs on every `dplyr` verb, and inside that in
**formatting messages nobody reads**:

* `cli::format_inline()` is not cheap: about 0.5 ms for a static string, 3.5 ms
  once `{.val {x}}` is involved, and **15 ms** for a hint that interpolates a
  vector of row numbers. It calls `isatty()` every time, which alone was 28% of
  a `tbl_now()` call.
* A findings block builds the "you have a problem" message *and* the "all clear"
  message whether or not the check found anything. Validating a clean object
  formatted **eleven messages and reported one** — the other ten were dropped by
  the status floor before anyone could read them.
* A `dplyr::tibble()` costs about 2 ms to construct. One per finding, ten
  findings, on every verb. `dplyr::arrange()` on a ten-row tibble costs 6 ms;
  `order(..., method = "radix")` does the same job in 0.11 ms.

So `.diagnose_text()` no longer returns a string: it returns a **template**, and
`.diagnose_finalise()` filters by the floor first and formats only the survivors.
Findings are plain lists (`.diagnose_row()`), assembled into the one tibble the
caller sees at the very end. Together: `tbl_now()` −68%, `validate_tbl_now()`
−76%, and the CRAN test path from 311.6 s to 152.4 s — **half** — with no test
removed and coverage slightly up. See `devel/TEST_SPEEDUP_BRIEF.md` for the
full before/after and for the `diseasenowcasting` levers that turned out to
make things worse.

Two things to know before touching that code again:

* **Deferring changes *when* a template is read.** Most of these are built in a
  `for` loop over columns, axes or strata, so a template that read the live
  frame would report the loop variable's LAST value in every row.
  `.diagnose_text()` therefore `rlang::env_clone()`s its `.envir`; the clone
  costs 0.01 ms and does not force promises. If you write a new finding, you get
  this for free — but do not "optimise" the clone away.
* **`paste()` on a `.diagnose_text()` forces it**, which is the cost the whole
  design avoids. Use `.diagnose_join()`.

### Engine tests: assert the CALL, not the fit

`test-engines-matrix.R` used to fit a real model in every cell of a 24-shape
grid, per engine, and `diseasenowcasting` alone took 956 s of it — 52% of the
whole suite — for assertions that only ever checked plumbing: no error, the
right strata, the right combinations, monotone quantiles. None of that needs a
good fit, or any fit.

So the grid is now a **dry-run tier** (`engine_dry_args()` in
`helper-engines.R`): `diseasenowcasting` simulates from the prior
(`prior_only = TRUE`), `baselinenowcast` draws 10 samples, `NobBS` runs a short
JAGS chain, and `epinowcast` and `EpiNow2` drop their likelihood
(`enw_fit_opts(likelihood = FALSE)`, `obs_opts(likelihood = FALSE)`). The file
went 798 s -> 432 s while going from **three engines to all six** — the other
three had only ever been excluded for being slow to fit. Reach for a dry run
before tuning a sampler: every "make the fit smaller" knob on
`diseasenowcasting` made it **slower** (see `devel/TEST_SPEEDUP_BRIEF.md`),
because fewer basis functions break convergence and the fitter retries.

**Measure before concluding a package is too slow to test.** An earlier pass
here ruled `epinowcast` out at "~100 s per shape". It is really ~90 s of Stan
compilation **once per R process** plus 1.5 s per fit, so its full 24 shapes
cost 153 s and dropping half of them would have saved 50. Never read the first
fit in a session as the cost of a fit.

Two rules if you add to that file:

* **A dry run must still be a model, not an absent one.** `prior_only = TRUE`
  returns all-`NA` draws for `count-cumulative` — silently, with the shape still
  correct — so every shape assertion would pass on nothing. Assert non-NA
  values, and where a package genuinely cannot produce them, assert the NA-ness
  explicitly so an upstream fix shows up as a failure rather than passing
  unnoticed.
* **Keep one real fit per data type.** `"each data type is either modelled or
  refused with a reason"` is that tier, and it deliberately pays ~24 s for the
  `count-cumulative` fit: that is the shape needing `confirmation_process()`,
  without which the fit reports "Joint fit failed to converge for all init
  attempts".

And one trap, now fixed but worth knowing: `engine_for()` used to combine
overrides with `engine_args()` through `utils::modifyList()`, which **recurses
into list-valued arguments**. Overriding one merged it with the default rather
than replacing it, so `fit = enw_fit_opts(sampler = enw_pathfinder)` kept
`epinowcast`'s default `chains`/`iter_sampling` and the pathfinder then refused
them as unused arguments — a failure that reads as "the sampler is broken"
rather than "the override did not take". `engine_for()` now assigns overrides
directly. Exported `engine()` stores `list(...)` unmerged and never had the
problem.

---

## 9. Pitfalls that have already bitten

* **`cli_*` vs `cat_*`.** `cli::cli_inform()` writes to the *message* stream.
  Inside a knitr chunk with `message=FALSE` it vanishes. `print()` methods must
  use `cli::cat_line()` / `cat_rule()` / `cat_bullet()`.
* **`\\` line continuations render literally** inside `cli::format_inline()`.
  Use `paste0()`.
* **`local_mocked_s3_method()` DELETES the real method when the owning package
  is not attached.** It restores whatever it found in the S3 methods table on
  exit; with the package merely loaded it finds nothing, and faithfully restores
  nothing — unregistering the real method for the rest of the R session.
  `test-converter-epinow2.R` did this to
  `EpiNow2::get_predictions.estimate_infections`, and nothing noticed until the
  engine matrix began fitting EpiNow2 for real: all 24 of its shapes died with
  "no applicable method for 'get_predictions'", in a file that passed on its
  own. `withr::local_package("EpiNow2")` before the mock fixes it — attached,
  the method is visible, so it is found and put back. Only bites generics OWNED
  by the package: mocking a base generic like `summary()` is safe, because the
  mock goes in base's table and the package's own entry is never touched.
* **A test file that passes alone is not isolated.** That leak was invisible for
  as long as nothing downstream used the thing it broke. When adding a test that
  really calls a suggested package, run it after the files that sort before it,
  not just on its own:
  ```r
  for (f in earlier_files) test_file(f); test_file("the-new-one.R")
  ```
* **A findings message is a template, not a string.** `.diagnose_text()` returns
  a deferred object so that a finding below the reporting floor is never
  formatted. `paste()` on one forces it (and returns something useless);
  `.diagnose_join()` is the concatenation that stays lazy. See §8.
* **`dplyr::arrange()` sorts in the C locale, `order()` in the session's.**
  Swapping one for the other to save time silently changes row order for
  anybody outside an ASCII locale. `order(..., method = "radix")` is the
  match.
* **S7 classes break S3 method naming.** `class()` is
  `"pkg::thing"`, and `::` cannot appear in a method name. Register in
  `.onLoad()`.
* **`fileEncoding = "UTF-8"`** truncates a `read.csv()` at the first non-ASCII
  byte on some platforms. Use `encoding = "UTF-8"`.
* **Do not poll for a background job at all.** The harness notifies you when a
  backgrounded command exits; a watch loop adds nothing and can outlive the work
  by hours. `while pgrep -f "job.R"; do sleep 30; done` never terminates, because
  the loop's *own* command line contains `job.R`, so `pgrep` matches itself. The
  `[j]ob.R` bracket trick does not save you either: any *second* watcher carrying
  the same string keeps every one of them alive. If you truly must poll, match on
  a PID you captured, never on a name. And when cleaning up, enumerate what you
  actually spawned -- grepping for one loop syntax silently leaves the others
  running.
* **pkgdown builds against the *installed* package, not the source.** A
  `devtools::load_all()` session will show a fixed converter while the rendered
  site still shows the old behaviour. `R CMD INSTALL .` before building docs.
* **`pkgdown::build_article()` does not re-copy `pkgdown/extra.css`.** CSS edits
  need `pkgdown::init_site()` (or a full `build_site()`) or you will be looking
  at a stale stylesheet and debugging a fixed bug.
* **Never point a knitr `child =` at a path containing `../`.** \pkg{rmarkdown}
  renders into an intermediates directory under `tempdir()` and copies relative
  resources alongside it, so `../..` escapes that directory. It appears to work
  on macOS, where `tempdir()` is deep enough that the escape lands somewhere
  writable (it silently creates stray directories there), and fails on CI, where
  `tempdir()` is two levels from `/`. Put shared fragments in `inst/` and load
  them with `system.file()`, which is path-independent and is shimmed by
  `pkgload::load_all()` to find the source tree.
* **A local render is not proof when paths are involved.** Reproduce with the
  same `TMPDIR` CI uses (`TMPDIR=/tmp Rscript -e '...'`) before concluding a
  path bug is fixed -- or unfixed.
* **Weekly data with fractional `.delay`** means the two date columns are on
  different weekday grids. Use `align_weeks()`; do not round.
* **A package that declares its own `tidy()` generic silently breaks every other
  package's methods.** The broom convention is to re-export `generics::tidy`, so
  all methods land on one generic; declaring a fresh `UseMethod("tidy")` makes
  methods registered on the `generics` one invisible, with no error — just a
  different table. `diseasenowcasting` did this before 2.1.0. `broom` still
  overwrites `tbl.now`'s `tidy.list()` method (the one `NobBS` and `nowcaster`
  need). Write `tbl.now::tidy()` in article code where another package is
  attached, and re-export the generic in any package you write.
* **Register a method for another package's class only if that package has not.**
  `.onLoad()` checks `getS3method(..., optional = TRUE)` before registering the
  `diseasenowcasting` method, so tbl.now fills the gap on old versions without
  overriding the method newer ones ship.
* **Never edit a script a background job is reading.** `Rscript` streams the
  file as it evaluates, so an edit mid-run corrupts its read position and
  produces a parse error at a line that is perfectly valid on disk. Copy the
  script somewhere frozen and run that copy if you need to keep editing.
* **A vignette that displays cached results must not have TWO sources of
  truth.** `nowcasting-models.Rmd` used to show one call and cache the output of
  another, kept in step by a comment asking whoever edited one to remember the
  other. Nobody did, and nothing caught it: the article never runs what it
  prints, so the drift is invisible until a reader copies a line off the page.
  The fix is structural, not a reminder -- purl the article and run *its* code
  (§7). If you find yourself writing "keep this in step with", you are building
  the bug.
* **A test can assert the right fact and still miss the bug.** "The strata
  columns are present in the converter's output" was true, passing, and useless:
  `NobBS.strat()` needs *one* column and there was none, so a multiply
  stratified object could not be fitted at all. Assert that the destination's
  own entry point can be **called** with what the converter returns.
* **A Stan fit that does not converge does not error -- it returns numbers.**
  `run_engine()` wraps every fit in `suppressWarnings(suppressMessages(...))`, so
  divergent-transition and low-ESS warnings never reach the log and a broken fit
  records as `ok`. One EpiNow2 fit was cached with its upper credible bound at
  `1e8` for all 181 days of a stratum whose observed maximum was 842. Nothing
  caught it until the numbers were read. Look at the numbers, and keep a
  plausibility guard on anything written to a cache.
* **One `set.seed()` at the top of a precompute script is not reproducibility.**
  It only pins anything if every engine consumes the same random numbers in the
  same order, so refitting a SUBSET lands somewhere different from a full run.
  This bit twice: it is how the `1e8` EpiNow2 fit above appeared and then refused
  to reappear, and refitting `baselinenowcast` alone silently changed its
  estimates, because its bootstrap resamples off the same RNG. It is not a Stan
  problem -- it is every stochastic engine.

  Seed per `(engine, stratum)` **immediately before each fit**, so a fit depends
  only on which fit it is, not on what ran before it or whether anything did.
  `nowcast_models_precompute.R` seeds once before the run; both engines then reproduce
  to `max abs diff == 0` with the RNG deliberately perturbed in between.

* **A uniqueness key that omits an attribute reports real rows as duplicates.**
  `validate_tbl_now()`'s non-uniqueness warning built its `distinct()` key from
  the dates, strata, covariates and effect columns. When confirmation arrived, a
  case and its own retraction -- same event, same report, opposite outcome --
  came out as "exact duplicates", advising `dplyr::distinct()`, which would have
  **deleted the retraction**. Any column that legitimately distinguishes two rows
  belongs in that key.
* **`.quietly_if()`/`suppressWarnings()` around a backend hides the warnings that
  matter.** Suppress messages only. (Recorded twice now: `run_engine()` and
  `nowcast_fit()`.)

---

## 10. Definition of done

Before calling a change finished:

- [ ] `devtools::document()` run; `NAMESPACE` and `man/` regenerated, not hand-edited.
- [ ] `NOT_CRAN=true` test suite passes; new behaviour has new tests.
- [ ] Any new attribute has an exported, documented, tested getter.
- [ ] A new converter has: `to`, `from` (where meaningful), an `as_tbl_now()`
      method, the target package's own coercion generic (or an entry in the
      registry in `test-coercion-methods.R` saying why there is none), a
      round-trip test, a `tidy()` method (unless it returns only delays), a
      censoring-collapse call, a `.pool_undeclared()` call, and a vignette
      section covering both the plain and the stratified case.
- [ ] **No new function duplicates an existing one** -- see the pre-flight grep
      in §4, and say in the commit message what you checked against.
- [ ] `NEWS.md` updated for anything user-visible, including deliberate
      behaviour changes.
- [ ] `SKILL.md` updated if the *user-facing* API changed.
- [ ] **Every new exported topic added to `reference:` in `_pkgdown.yml`.**
      The index is explicit, so `pkgdown` **fails the build** on a topic that is
      not listed. Check with `pkgdown::check_pkgdown()`, which is fast and needs
      no site build.
- [ ] A new check or statistic goes **into the findings engine**, not beside it
      — see §11.
- [ ] Plots use `.tbl_now_palette()` and the red/green grammar.
- [ ] **`R CMD check` clean** — the test suite passing is not the same check.
- [ ] **`checktor::checkup()` clean** — the extra CRAN-submission checks.

### The two checks, and why the suite does not cover them

`testthat` runs your code. `R CMD check` checks the *package*: the things that
only break for someone installing it fresh.

```r
devtools::check()                 # or: R CMD check tbl.now_*.tar.gz
checktor::checkup()               # extra CRAN-submission checks
```

The one that keeps recurring here is **undeclared packages**:

```
'library' or 'require' call not declared from: 'NobBS'
```

Every package `library()`d in a vignette must be in `DESCRIPTION`, even when the
chunk is `eval = FALSE` — the check reads the source, not the evaluated output.
The suite never sees this, because the vignettes do not run under `testthat`.

For a package **not on CRAN**, adding it to `Suggests` alone is not enough:
`Suggests` is resolved against the configured repositories, so it also needs an
entry under `Additional_repositories` (this is how `epinowcast` is handled) or
the vignette must stop attaching it and use `pkg::fun()` behind
`requireNamespace()` instead.

---

## 11. The two report families (`summary()` and `diagnose()`)

`R/summary.R` and `R/diagnose.R` answer the two questions a user has about a new
object: **what is in it** and **what is wrong with it**. They are built the same
way, and the rules below are what keep them from drifting apart.

### One engine, two presentations

`validate_tbl_now()` and `diagnose()` are **the same code**.
`.tbl_now_findings()` computes the findings; `diagnose()` returns them as a
tibble, and `validate_tbl_now()` re-emits the `error` rows as an abort and the
`warning` rows as warnings.

This is the rule that matters: **a new check goes in the engine.** Adding one to
`validate_tbl_now()` directly means `diagnose()` cannot see it, and adding it to
`diagnose()` directly means construction stops complaining about it. Both
failures are silent. Before the engine existed, the ordering and duplicate
checks lived only in `validate_tbl_now()` as `cli` warnings, which is why they
could not be tested on their value — only on whether a warning was thrown.

Two constraints on the engine itself:

* **Keep the pre-flight stage.** Missing attributes short-circuit before the
  findings stage, because every later check assumes the attributes exist. Do not
  flatten the two passes into one.
* **Never promote a `note` to a `warning`.** `validate_tbl_now()` runs on every
  `dplyr` verb. A new warning there turns a quiet construction into a noisy one
  for data that has always been accepted, for every existing user at once.
  `note` exists precisely so `diagnose()` can say more than `validate` does.

### The schema is the contract

Every function in each family returns the same columns, so `summary()` and
`diagnose()` are literally the `bind_rows()` of their components — and there is
a test asserting exactly that. When you add a block:

* return the **full schema**, and let blocks **omit** columns they do not
  populate. `bind_rows()` then infers each column's type. Handing it a logical
  `NA` where a `Date` belongs is an error, not a coercion — which is why
  `date_min`/`date_max` appear only when a coverage row is present.
* put the *subset being described* in `stratum` and the *category* in
  `quantity`. `"confirmation_type = confirmed"` is a quantity; `"Female"` is a
  stratum. Mixing them makes a compositional row impossible to interpret.

### `skipped` is not `ok`, and the difference is load-bearing

`ok` means the check ran and found nothing. `skipped` means it could not run —
no confirmation process, the wrong data type, an optional package absent. A
check that cannot be performed must never be reported as a pass; silence that
reads as approval is the main way a health check misleads.

The example that keeps coming up: **`diagnose()` refuses to look for duplicates
in a line list**, because one row is one case, so two identical rows are two
infections. That is a `skipped` with a reason, not an `ok`, and not a bug.

### Reuse the estimators, do not add new ones

Both families reuse what the plots already use — `.tbl_now_weighted_quantile()`
for every quantile, `.tbl_now_maturity_threshold()` for "how recent is too
recent", `.tbl_now_strata_label()` for stratum labels,
`.tbl_now_date_seq()`/`.tbl_now_units_between()` for grids. This is not
tidiness: a second median estimator means `summary()` and `autoplot()` print
different numbers for the same data, and nothing fails to tell you.

The consequence to accept: quantiles are **inverse-ECDF (type 1)**, so `q50` on
an even number of observations is the upper middle value rather than the
average. It is documented in a `@note` on both families. Changing it means
changing the plots too.

### Statistical tests live outside `diagnose()`

`diagnose()` is structural and deterministic: no test, no seed, no optional
package, no runtime that depends on the data size. Drift and batching are
statements about a distribution, and answering them means choosing a method, a
window and a multiplicity correction — decisions a health check has no business
making silently.

They come back as `not_run` **signposts** carrying the call that answers them
(`diagnose_drift()`, `diagnose_batches()`). If you add a statistical diagnostic,
add a signpost row for it; do not wire it into `diagnose()`.

### Grids run to `now`, globally

Every date grid in `summary()` runs from the earliest observed date on that axis
to `get_now()`, stepping in that axis's units — not to the last row present.
A date with no rows is a **zero**, not an absence.

Two things follow, and both are asserted in the tests:

* a **line list summarises to exactly the same numbers as its counts**, which is
  only true because the grid does not come from the rows;
* the grid is **global**, not per-stratum, so a stratum whose cases start late
  shows its leading zeros and the strata stay comparable. The
  triangle-occupancy denominator uses the global widest delay for the same
  reason — it was per-stratum first, and it flattered any stratum whose cases
  all arrived same-day.

### `NA` counts are dropped, visibly

In count data an `NA` count means *not yet observed*; a `0` means *observed, and
it was zero*. `summary()` drops the `NA`s and counts them in the
`unobserved_cells` coverage row, so the drop is visible rather than silent.
This was found by running against `flusight`: one `NA` turned every total in the
table into `NA`.
