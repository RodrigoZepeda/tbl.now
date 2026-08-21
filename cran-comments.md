#v 0.15.0 

This is a resubmission. It addresses the following comments from the CRAN
reviewer:

## 1. Missing references in the DESCRIPTION field

> If there are references describing the methods in your package, please
> add these in the description field of your DESCRIPTION file...

The package does not introduce a new method that needs a top-level citation.
The handful of diagnostic functions are already
cited via `@references` on those specific help pages.

## 2. Examples for unexported functions

> You have examples for unexported functions. Please either omit these
> examples or export these functions.

These examples have been omitted from the documentation using the `@noRd` tag

### Second round: `censor_delays_above()` in `compute_temporal_effects.Rd`,
### `tbl_now_attributes.Rd`, `tbl_now_coerce.Rd`

Thank you — this one took some digging, because the named function was a red
herring. `censor_delays_above()` *is* exported (`NAMESPACE` line 65) and is
documented on its own `censor_delays_above.Rd`, not on any of the three pages
listed. The same mismatch occurred in the previous round, where
`as_tbl_now.tbl_now()` was reported "in `check_bool.Rd`" although that page's
example only ever called `check_bool()`. We therefore treated the **file list**
as the signal and audited it directly.

Of the three pages, exactly one was genuinely at fault: **`tbl_now_coerce.Rd`**.
It was generated from a roxygen `@name tbl_now_coerce` topic on a `NULL` block,
so the page's `\name` referred to **no object at all**, and its only aliases were
four S3 methods registered with `S3method()` and therefore unexported. The page
carried `\examples` while nothing on it was exported. The other two pages
(`compute_temporal_effects.Rd`, `tbl_now_attributes.Rd`) each document a single
exported function and needed no change.

Fixes applied:

* The coercion page is now named after a real object (`@name as_tibble.tbl_now`
  instead of the phantom `tbl_now_coerce`), making it structurally identical to
  `autoplot.tbl_now.Rd` / `update.tbl_now.Rd` / `print.Rd` — S3-method pages that
  passed both previous reviews. No exports changed.
* We then audited **all 308 functions** in `R/`. 133 carry `@export` /
  `@exportS3Method`, 158 carry `@noRd`, and 3 dot-prefixed helpers have no
  roxygen block (so generate no page). The remaining **15** had only
  `@keywords internal` — they generated Rd pages for unexported functions — and
  have now been given an explicit `@noRd`, removing those 15 pages. None of them
  had examples, and we verified no surviving page cross-references them.
* Result: **no Rd page in the package now has `\examples` while nothing on the
  page is exported.**

A second page had the same defect and is fixed too: **`print.Rd`**. It came from
`@name print` on a `NULL` block documenting an S7 method, so it produced a page
aliased to the bare name **`print`** — squatting the base generic's `?print`
topic — with examples and nothing exported. (The `@export` on an
`S7::method()<-` assignment produces no NAMESPACE entry at all; registration is
done by `S7::methods_register()` in `.onLoad()`.) That method is now `@noRd`,
and its examples were replaced by unit tests so the behaviour stays covered.

To keep this from recurring we added a maintainer script,
`devel/check_unexported_examples.R` (not shipped; `devel/` is in
`.Rbuildignore`). A page is treated as safe only if it *both* carries
`\method{generic}{class}` markup *and* has a `\name` that resolves to a real
object; either criterion alone was insufficient. We validated the script by
running it against the two previously rejected states: it reproduces the genuine
findings (`check_bool.Rd`, `infer_now.Rd`, `infer_units_one_column.Rd`,
`tbl_now_coerce.Rd`) and is clean on the current tree.

## 3. `\dontrun{}` used where not necessary

> `\dontrun{}` should only be used if the example really cannot be
> executed... Please replace `\dontrun` with `\donttest`... Please unwrap
> the examples if they are executable in < 5 sec.

We unwrapped them

## 4. `if(FALSE){}` in examples

> Some code lines in examples are wrapped in `if(FALSE){}`. Please never do
> that.

We use `try()` to catch the intentional error instead of the `if(FALSE){}` now:

```r
# Validate with errors (wrapped in try() since this intentionally errors)
try(validate_tbl_now(data.frame(x = 1:3)))
```

## 5. win-builder pre-test NOTE (new submission)

The automated win-builder pre-test flagged one NOTE on both Windows and
Debian:

* "Possibly misspelled words in DESCRIPTION" (Nowcasting/nowcasting/nowcasts)
  — these are standard epidemiological terminology, not typos.
* "Suggests or Enhances not in mainstream repositories: epidist,
  epinowcast" — both are already declared with `Additional_repositories`
  pointing at `https://epinowcast.r-universe.dev`, which the check itself
  confirms as available.

Neither needs a code change. Windows additionally reported "Overall
checktime 14 min > 10 min", driven mostly by `testthat.R` (531s). We added
`skip_on_cran()` to the slower, more exhaustive/edge-case tests, while keeping
at least one CRAN-visible test or example exercising every exported function
(verified programmatically against `NAMESPACE`). `testthat.R` now runs in
**171s** under real CRAN conditions.

## R CMD check results

0 errors | 0 warnings | 1 note

Checked with `R CMD check --as-cran` on a tarball built with vignettes, with
`NOT_CRAN` unset so `skip_on_cran()` applies exactly as it will on CRAN. (Note
for other maintainers reading this: `devtools::check()` forces `NOT_CRAN=true`
internally, which disables `skip_on_cran()` and hides the real check time.)

The remaining NOTE is the routine new-submission one:

* "New submission"
* "Suggests or Enhances not in mainstream repositories: epidist, epinowcast",
  where the same NOTE confirms both resolve via the declared
  `Additional_repositories`.
