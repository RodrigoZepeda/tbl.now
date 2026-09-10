# v1.0.0

New submission.

## Test environments

* local macOS 15.7.3 (x86_64-apple-darwin20), R 4.5.3 -- `R CMD check --as-cran`
  on a tarball built with vignettes, with `NOT_CRAN` **unset** so
  `skip_on_cran()` applies exactly as it will on CRAN.
* GitHub Actions (`r-lib/actions`): ubuntu-latest (release, devel, oldrel-1),
  macOS-latest (release), windows-latest (release).
* win-builder (devel and release).

A note for other maintainers reading this: `devtools::check()` and
`testthat::test_local()` both force `NOT_CRAN=true` internally, which disables
`skip_on_cran()` and measures the wrong code path. The timings below come from
`R CMD check` with the variable unset.

## R CMD check results

0 errors | 0 warnings | 1 note

(The local run reports a second NOTE, "checking for future file timestamps ...
unable to verify current time". That is the check being unable to reach
`worldclockapi.com` from this machine, not a property of the package.)

The remaining NOTE is the routine new-submission one:

```
New submission

Suggests or Enhances not in mainstream repositories:
  almanac, diseasenowcasting, epidist, epinowcast
Availability using Additional_repositories specification:
  almanac             yes   https://davisvaughan.r-universe.dev
  diseasenowcasting   yes   https://rodrigozepeda.r-universe.dev
  epidist             yes   https://epinowcast.r-universe.dev
  epinowcast          yes   https://epinowcast.r-universe.dev
```

All four resolve through the declared `Additional_repositories`, as the NOTE
itself confirms. `epidist` and `epinowcast` (and `epinowcast`'s dependency
`primarycensored`) are published by the epinowcast project's r-universe;
`almanac` was archived from CRAN and is served from its author's r-universe;
`diseasenowcasting` is published from the maintainer's own r-universe. Every one
of them is optional: each is used strictly behind `requireNamespace()`, each
example that needs one is guarded with `@examplesIf`, and the package builds,
checks, and works fully without any of them installed.

## Notes on the previous submission round

The reviewer comments from the v0.16.0 round -- references in `DESCRIPTION`,
examples for unexported functions, `\dontrun{}`, and `if (FALSE) {}` in examples
-- were all addressed at the time and remain addressed. There are no
`\dontrun{}` or `if (FALSE) {}` blocks in the examples, and every documented
example belongs to an exported function.

Windows previously reported "Overall checktime 14 min > 10 min", driven mostly
by `testthat.R` (531s). The slower, exhaustive and edge-case tests carry
`skip_on_cran()`, while at least one CRAN-visible test or example still
exercises every exported function (verified programmatically against
`NAMESPACE`). Under real CRAN conditions `testthat.R` now runs in well under a
minute.

## Documentation URLs

Two links in `README.md`, `NEWS.md` and `vignette("tbl.now")` point at articles
on the package website:

* `https://rodrigozepeda.github.io/tbl.now/articles/batches.html`
* `https://rodrigozepeda.github.io/tbl.now/articles/more-on-tbl-now.html`

Both articles are new in 1.0.0. The website is rebuilt and deployed from the
default branch, so these pages go live with the 1.0.0 site deploy that precedes
this submission; if a pre-test still reports them as 404, the deploy had not yet
completed and no source change is needed.
