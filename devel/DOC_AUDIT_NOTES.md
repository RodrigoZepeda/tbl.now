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
