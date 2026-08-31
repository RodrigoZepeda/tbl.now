# `diagnose()` — implementation plan

Written 2026-08-26, to be executed in a fresh conversation. Companion to the
`summary()` work that landed in `R/summary.R` (v0.23.0). Read
`DEVELOPMENT_SKILL.md` first; this plan assumes its rules, especially §4
(prove the function does not already exist) and §10 (definition of done).

---

## 0. The one-paragraph version

`diagnose()` is a **structural, deterministic** health check on a `tbl_now`,
returning a findings tibble sorted worst-first. It runs **no statistical
tests**. Most of the checks the user asked for already exist inside
`validate_tbl_now()` as `cli` warnings, so the core of this work is a
refactor: extract a shared findings engine that `validate_tbl_now()` formats
into messages and `diagnose()` returns as data. Everything else is new but
small.

---

## 1. Decisions already made (do not relitigate)

| decision | choice |
|---|---|
| Relationship to `validate_tbl_now()` | **Extract a shared findings engine.** One implementation, two presentations. |
| Statistical tests (drift, batches) | **Never run inside `diagnose()`.** Emit a signpost row instead. |
| Naming | **`diagnose_*` prefix.** Avoids the head-on collision with the eight existing `check_*` internals in `R/checkers.R`. |
| Extras to include | Undeclared columns + uncomputed effects; right-truncation severity; stratum viability + pending backlog. |
| Extras rejected | Reporting-outage detection (see §2), weekday/seasonal profile. |
| Per-check functions | Yes, each block callable on its own, same as the `summary()` family. |

The user knowingly accepted that choosing "structural only, ever" turns their
requests 3, 4 and 5 (delay drift, confirmation drift, batches) into
**signposts rather than answers**. §6 makes those signposts as useful as
possible. Do not quietly re-add the tests.

---

## 2. Answer to the open question about outages

> *"For the outages that would be with NA's right? Cause otherwise the main
> idea of the tbl_now is to not carry the zeroes (until you complete_zeroes)."*

**Correct, and it kills the check as I originally proposed it.** In a
`tbl_now` an absent row means "nothing was reported for that date", and a
quiet Sunday is structurally *identical* to a three-week system outage. There
is nothing to detect structurally, because the object deliberately does not
carry the zeroes. Distinguishing the two requires asking whether a run of
zero-arrival dates is improbably long given the surrounding arrival rate —
which is a statistical test, and therefore excluded by the decision in §1.

So the plan **drops outage detection**. What survives of the idea, split
across three places that already exist or are already planned:

1. **`NA` counts are a different thing entirely, and they are structural.**
   In count data an `NA` count means *not yet observed* — genuinely carried,
   genuinely distinguishable from an absent row. (This is the distinction that
   produced the `flusight` bug during the `summary()` work: one `NA` turned
   every total in the table into `NA`.) `diagnose_missing()` reports these,
   and must label them **neutrally** — an `NA` cell in a reporting triangle is
   correct data, not a defect.
2. **The descriptive answer already shipped**: `zero_run_summary()` in
   `summary()` gives the distribution of consecutive zero-date run lengths, on
   the completed grid.
3. **The inferential answer already exists**: `batch_test()` finds the spikes
   that typically follow an outage.

---

## 3. Architecture

### 3.1 The findings engine

```
R/diagnose.R
  .tbl_now_findings(x, checks = NULL, ...)   # internal, returns the tibble
  diagnose(x, ...)                           # generic + tbl_now method
  diagnose_*()                               # one per block, all exported
```

`validate_tbl_now()` in `R/dplyr_generics.R` is rewritten to consume
`.tbl_now_findings()` and re-emit it as the `cli` messages it emits today.

**Sequencing constraint that must not be broken.** `validate_tbl_now()`
currently aborts early when a required attribute is missing, and every check
after that point assumes the attributes exist. The engine must therefore keep
a **pre-flight stage** (class, required attributes, columns present) that
short-circuits, before the findings stage. Do not flatten these into one pass.

### 3.2 Severity

`status` is an **ordered factor**, worst first, so the tibble sorts itself and
"easy to see" comes for free:

```
error > warning > note > ok > not_run > skipped
```

* `error` — `validate_tbl_now()` aborts on it.
* `warning` — `validate_tbl_now()` warns on it.
* `note` / `ok` — `diagnose()` only; invisible to `validate_tbl_now()`.
* `not_run` — a signpost (§6).
* `skipped` — could not be assessed (no confirmation process, wrong data type).

The engine gains rows `validate_tbl_now()` must *not* emit. Gate with a
`severity_floor` argument, or have `validate_tbl_now()` filter to
`c("error", "warning")`. The second is simpler and keeps the filtering
decision in one visible place.

### 3.3 Output schema

One row per (check, scope, stratum). Proposed columns:

| column | meaning |
|---|---|
| `check` | slug, e.g. `"date_ordering"`, `"missing"`, `"duplicates"` |
| `scope` | what the row is about: a column name, an axis, `"all"` |
| `stratum` | stratum label, or `"all"` |
| `status` | the ordered factor above |
| `n_affected` | offending rows |
| `n_total` | rows considered |
| `prop` | `n_affected / n_total` |
| `message` | one human sentence, already formatted |
| `rows` | list-column of offending row indices |

**Assumption to confirm:** `rows` is included. It makes findings actionable
(`x[result$rows[[1]], ]` goes straight to the bad rows) at the cost of a
list-column in a table the user asked to be "easy to see". It is trivial to
drop; say so if it should not be there.

Unlike the `summary()` schema, columns are **not** forced to a fixed set —
there is no distributional/compositional/scalar mixing here, so every column
is populated by every row.

---

## 4. Check inventory

Ten requests, mapped to what exists. **Reuse column is the important one.**

| # | request | function | reuse / new |
|---|---|---|---|
| 1 | `event <= report <= confirmation` | `diagnose_ordering()` | **Reuse.** `validate_tbl_now()` already checks `report < event`; `.check_confirmation_ordering()` (R/confirmation.R:126) already checks `confirmation < report`. Both currently warn and are internal. Move into the engine. Add the missing transitive case (`confirmation < event` with `report` `NA`). |
| 2 | NAs per stratum/covariate/date/count | `diagnose_missing()` | **Partly reuse.** `validate_tbl_now()` covers `NA` in the two dates. New: counts, confirmation date/type, censoring flag, each stratum, each covariate. Label `NA` counts neutrally (§2). |
| 3 | drift in the delay | — | **Signpost** to `test_delay_drift(axis = "report")`. |
| 4 | drift in the confirmation delay | — | **Signpost** to `test_delay_drift(axis = "confirmation")`. |
| 5 | batches in report or confirmation | — | **Signpost** to `batch_test(axis = )` / `batch_shape_test()`. |
| 6 | negative changes in cumulative | `diagnose_negatives()` | **New.** `to_count(x, "count-incidence")` de-accumulates; count and size the negative increments. Also flag negative counts in `count-incidence`. `skipped` for line lists. |
| 7 | repeated rows on the full key | `diagnose_duplicates()` | **Reuse.** `validate_tbl_now(warn_non_uniqueness = TRUE)` already builds exactly this key (and R/dplyr_generics.R:332 records why the confirmation columns belong in it). Note it is **off by default** there; `diagnose()` should default it **on**. |
| 8 | units mismatch | `diagnose_units()` | **New**, three sub-checks — see §5. |
| 9 | anything after `now` | `diagnose_now()` | **Reuse.** `validate_tbl_now()` already errors on confirmation `> now` and warns on `now < max(report)`. Add event/report `> now` as explicit rows. |
| 10 | gap from last observation to `now` | `diagnose_now()` | **Reuse.** Already computed: `triangle_occupancy()` emits `now_gap_event` / `now_gap_report` via `.tbl_now_units_between()`. Do not recompute. |

---

## 5. `diagnose_units()`, spelled out

"Units between event, delay and confirmation don't match" is three different
failures. Report them as three rows:

1. **Declared attributes disagree.** `get_event_units()` vs
   `get_report_units()` vs `get_confirmation_units()`. Note that
   `time_cols_to_numeric()` already *enforces* report-coarser-than-event and
   the numeric all-or-nothing rule at construction, so this row is mostly
   reachable via `change_*()`. Confirmation units are **not** currently
   cross-checked against the other two — that is a real gap.
2. **Declared units disagree with the empirical grid.** Declared `"weeks"` but
   the dates land on several different weekdays. This is the §9 pitfall
   ("weekly data with fractional `.delay`") and the fix is `align_weeks()`,
   which the message must name.
3. **Fractional `.delay`.** The direct symptom of (2), and the one that
   actually breaks converters. Count the rows.

---

## 6. The signposts

Rows with `status = "not_run"`, carrying a copy-pasteable call in `message`:

```
check           scope         status   message
delay_drift     report        not_run  Run: test_delay_drift(x, axis = "report")
delay_drift     confirmation  not_run  Run: test_delay_drift(x, axis = "confirmation")
batches         report        not_run  Run: batch_test(x, axis = "report")
batches         confirmation  not_run  Run: batch_test(x, axis = "confirmation")
```

Suppress the confirmation rows when `has_confirmation(x)` is `FALSE` (emit
`skipped`, not `not_run`). Suppress the drift rows when `modifiedmk` is not
installed, with a `skipped` row naming the install command.

---

## 7. The three extras

* **`diagnose_declarations()`** — undeclared columns via the existing
  `.undeclared_cols()` (R/converters.R:1057), plus temporal effects that were
  added but never materialised (`get_temporal_effects()` non-empty while
  `get_temporal_effect_cols()` is empty). Both are silent-bug sources the
  development guide already documents.
* **`diagnose_truncation()`** — how many recent event dates are still immature,
  and the share of their eventual total likely still missing. **Reuse**
  `.tbl_now_maturity_threshold()` (R/delay_drift.R:84), which `autoplot()` and
  `reporting_completeness()` already share. Do not introduce a second maturity
  rule.
* **`diagnose_strata()`** — strata too small or too sparse to fit separately,
  plus confirmations still pending well past the typical turnaround.
  Thresholds must be arguments with documented defaults, not magic numbers.

---

## 8. Risk register

* **The `validate_tbl_now()` refactor is the whole risk of this change.**
  52 test references across 7 files, 7 internal callers in
  `R/adders_changers_and_removers.R`. **No snapshot tests**, so wording is
  asserted by `expect_warning()` regex — verify each one still matches rather
  than assuming.
* **Preserve the abort/warn split exactly.** Anything that aborts today must
  still abort; anything that warns must still warn. A finding promoted from
  warning to error breaks `add_*()`/`change_*()` for users with imperfect data.
* **Keep `warn_non_uniqueness = FALSE` as `validate_tbl_now()`'s default**
  even though `diagnose()` defaults it on. Flipping it turns a quiet
  construction into a noisy one for every existing user.
* **Do not add a `print()` method without checking the S3 shadowing trap**
  (`S7::method(print, ...)` shadows `base::print` in the namespace; use
  `@exportS3Method base::print`). A plain sorted tibble may be enough.

---

## 9. Test plan

Mirror `test-summary.R`: a small fixture with defects **planted deliberately**,
one per check, hand-counted in a header comment.

* One fixture per defect class, so a check that silently stops working fails
  exactly one test.
* **Assert the failure that was actually planted**, per §8 of the development
  guide — a duplicate-row test using rows that differ in an *undeclared*
  column passes before and after any fix and proves nothing.
* Assert `validate_tbl_now()`'s behaviour is **unchanged** by the refactor:
  same conditions, same classes, same wording, for both a clean and a dirty
  object.
* Assert `diagnose(x)` is the `bind_rows()` of the `diagnose_*()` components,
  the same property `summary()` guarantees.
* Assert a clean object produces all-`ok` rows and no warnings.

---

## 10. Definition of done

- [ ] `devtools::document()`; `NAMESPACE` and `man/` regenerated.
- [ ] `NOT_CRAN=true` suite passes; current baseline is **3621 pass / 0 fail / 2 skip**.
- [ ] `R CMD check --as-cran` still **1 NOTE, 0 WARNINGs**.
- [ ] No new function duplicates an existing one; say in the commit what was
      grepped for (§4).
- [ ] `NEWS.md` and `SKILL.md` updated.
- [ ] `_pkgdown.yml` gains a `diagnose` section next to the summary one.

---

## 11. Questions and answers for the new conversation

1. **Should `rows` (the list-column of offending indices) be in the schema?**
   YES
2. **Should `diagnose_drift()` / `diagnose_batches()` exist as explicit
   adapters** that map `test_delay_drift()` / `batch_test()` output into the
   diagnose schema — callable by hand, never by `diagnose()`? 
   RENAME THE TEST ONES DO `diagnose_drift()` / `diagnose_batches()`
3. **Should `diagnose()` gain a `strata` / `by_strata` pair** like the summary
   family, or are findings inherently whole-object? Some checks are naturally
   per-stratum (viability, missingness), others are not (units, declarations).
   DO THE ONES THAT ARE NATURALLY PER STRATA PER STRATE THE OTHERS PER WHOLE OBJECT
4. **Thresholds for `diagnose_strata()`** — minimum cases and maximum zero
   share for a stratum to be considered fittable. Needs a defensible default.
   LET'S NOT USE A THRESHOLD. LET US INSTEAD SAY SOMETHING LIKE
   "THE STRATA WITH THE LEAST AMOUNT OF CASES IS yyy WITH XXX CASES REPRESENTING KKK%"
   OR SOMETHING SIMILAR. 