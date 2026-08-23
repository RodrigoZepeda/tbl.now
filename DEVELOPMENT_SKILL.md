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
  data; for line-list engines you must pass the grid explicitly (this is why
  `tbl_now_to_surveillance()` sets `control$dRange`).
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

Fits are precomputed by `data-raw/nowcast_comparison.R` into
`vignettes/articles/nowcast-comparison.rds`; `run_models` is `FALSE` in the
article so the build never fits anything. Add your engine there, then re-run the
script.

Also add a row to the package table at the top of the article, and to the
converter matrix in `data-raw/converter_matrix.R`, which runs every converter
against every shipped dataset.

---

## 8. Testing

* Run with `NOT_CRAN=true`, or roughly 780 tests silently skip:
  ```r
  NOT_CRAN=true Rscript -e 'devtools::load_all("."); testthat::test_local()'
  ```
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

---

## 9. Pitfalls that have already bitten

* **`cli_*` vs `cat_*`.** `cli::cli_inform()` writes to the *message* stream.
  Inside a knitr chunk with `message=FALSE` it vanishes. `print()` methods must
  use `cli::cat_line()` / `cat_rule()` / `cat_bullet()`.
* **`\\` line continuations render literally** inside `cli::format_inline()`.
  Use `paste0()`.
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
* **A vignette that displays cached results has TWO sources of truth.** The code
  on the page and the script that produced the cache must agree, or the reader
  cannot reproduce what they see. Whenever you change a fitting call in one,
  change it in the other, and re-run the precompute.

---

## 10. Definition of done

Before calling a change finished:

- [ ] `devtools::document()` run; `NAMESPACE` and `man/` regenerated, not hand-edited.
- [ ] `NOT_CRAN=true` test suite passes; new behaviour has new tests.
- [ ] Any new attribute has an exported, documented, tested getter.
- [ ] A new converter has: `to`, `from` (where meaningful), an `as_tbl_now()`
      method, a round-trip test, a `tidy()` method (unless it returns only
      delays), a censoring-collapse call, and a vignette section covering both
      the plain and the stratified case.
- [ ] `NEWS.md` updated for anything user-visible, including deliberate
      behaviour changes.
- [ ] `SKILL.md` updated if the *user-facing* API changed.
- [ ] Plots use `.tbl_now_palette()` and the red/green grammar.
