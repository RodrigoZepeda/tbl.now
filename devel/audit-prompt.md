# Audit prompt — `tbl.now` converters and `tidy()` methods

Copy everything below the line into a fresh chat, from the repository root.

---

You are auditing the R package `tbl.now` (repo root is the working directory).
Read `DEVELOPMENT_SKILL.md` first — it states the package's rules — and
`SKILL.md` for the user-facing API. Do **not** take either at face value: they
are documentation, and part of your job is finding where they and the code
disagree.

## What the package does

`tbl.now` holds nowcasting data (every observation has an **event date** and a
**report date**) and does two jobs for seven external nowcasting packages:

* **converters** — `tbl_now_to_<pkg>()` / `tbl_now_from_<pkg>()` / `as_tbl_now()`
  methods, which reshape a `tbl_now` into what each package expects;
* **`tidy()` methods** — which normalise what each package returns into one
  table.

Both are in `R/converters.R` and `R/tidy.R`.

## Task 1 — verify against each package's own documentation

**This is the core of the audit, and it must not be done from memory.** For
every package the converters and `tidy()` methods touch —

`diseasenowcasting`, `baselinenowcast`, `epinowcast`, `epidist`, `NobBS`,
`surveillance`, `nowcaster`, `tsibble`, `data.table`

— open that package's **actual help pages and source** in this environment and
check the claims the code makes about it. Useful moves:

```r
help(package = "<pkg>")                       # index of topics
?<pkg>::<function>                            # the specific page
args(<pkg>::<function>)                       # formals, incl. defaults
tools::Rd_db("<pkg>")                         # Rd sources, greppable
methods(class = "<class>"); getS3method(...)  # what dispatch really finds
utils::str(fit); S7::props(obj)               # actual object structure
```

For each converter and each `tidy()` method, establish:

1. **Argument names, order and defaults** the target function really has —
   including whether an argument takes a bare name, a string, or a vector. (A
   real example of getting this wrong: `epidist::as_epidist_linelist_data()`
   takes column names as **strings**; passing bare names errors.)
2. **The exact shape the target expects** — column names, types, whether counts
   must be cumulative or incident, whether delays may be zero or negative,
   whether it needs a complete rectangular grid or tolerates gaps.
3. **The exact shape the target returns**, and whether `tidy()` reads the right
   slot. Check the *stratified* case separately: a pooled slot and a
   per-stratum slot often both exist, and reading the pooled one silently
   returns one block labelled `"all"` while the fit reports several strata.
4. **Units and grids.** Does the target think in days, weeks, or index numbers?
   Does `tbl.now` convert correctly, or assume days?
5. **Semantics that survive the round trip.** In a reporting triangle `NA` means
   *not yet observed* and `0` means *observed and zero*. Find any path that
   conflates them.
6. **Interval semantics.** `tidy()` reports a `level` column. Confirm the value
   matches what the engine actually produced (e.g. a q5–q95 band is 90%, not
   95%) and that the bounds come from the right slot.

Report each finding as: what the code assumes → what the help page says →
what breaks, with a reproducible snippet.

## Task 2 — build a scenario matrix of tests

Existing tests are in `tests/testthat/`. Read them first — several files already
cover parts of this (`test-converters.R`, `test-converter-equivalence.R`,
`test-converter-datasets.R`, `test-converter-censoring.R`, `test-tidy.R`) — and
**do not duplicate** what is there. Add what is missing.

Cross these dimensions and cover the combinations that are meaningful:

| dimension | levels |
|---|---|
| temporal grid | **daily**, **weekly** (and a `numeric` grid if supported) |
| data type | **linelist**, **count-incidence**, **count-cumulative** |
| strata | **none**, **one**, **several** (and a stratum with an empty level) |
| censoring | **absent**, **delay-derived** (`censor_delays_above()`), **per-case flag** |
| delays | all ≥ 0; some zero; **negative** (report before event); very long |
| completeness | dense grid; **gaps**; a trailing event period with no reports |
| covariates / temporal effects | absent; present (lazy and materialised) |

Rules for these tests:

* **Test the failure that actually bites.** The two censoring kinds behave
  differently: a delay-derived flag is constant within an `(event, report)` cell
  and harmless; a **per-case** flag varies within a cell and breaks uniqueness.
  A test using only the first kind passes before and after the fix and proves
  nothing.
* **Assert on content, not just absence of error.** Case totals preserved,
  `NA`-vs-`0` preserved, strata labels preserved and correctly *paired* (label
  order is often data order, not alphabetical — a mispairing silently swaps
  strata).
* **Round-trip where a round trip exists**: `tbl_now → pkg → tbl_now`. Where it
  is lossy, assert *what* survives and document what does not.
* Guard optional packages with `skip_if_not_installed()`, heavy work with
  `skip_on_cran()`. Prefer `testthat::local_mocked_bindings()` over fitting a
  Stan/JAGS/INLA model; a slow flaky suite is worse than a fast focused one.
* Use the shipped datasets for realism (`denguedat` weekly line list;
  `covid_us`, `covid_colombia`, `covidat` daily counts; `flusight`
  count-cumulative; `hai_bucaramanga` deliberately messy; `mpoxdat`).

Run with `NOT_CRAN=true`, or roughly 780 tests silently skip:

```r
NOT_CRAN=true Rscript -e 'devtools::load_all("."); testthat::test_local()'
```

## Ground rules

* **Verify, do not assume.** Run the code. If you claim a converter is wrong,
  show the failing call and its output. If you claim it is right, say what you
  ran.
* **Report honestly.** If a check was not run, say so rather than implying it
  passed. Distinguish "confirmed bug" from "suspicious, unverified".
* Do not "fix" behaviour that is deliberate — read the comments, which usually
  record why. Flag disagreements instead of silently changing them.
* Prefer `tbl.now`'s own functions, then dplyr/tidyr, then base R.
* Native pipe `|>`, roxygen2, tidyverse style.

Deliverable: a written report of findings ranked by severity, plus the new test
files, with the full suite passing.
