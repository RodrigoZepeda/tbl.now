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
`skip_on_cran()` to the slower, more exhaustive/edge-case tests.
