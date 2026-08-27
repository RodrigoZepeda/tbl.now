# Brief: make `tbl.now`'s test suite faster

> **You are picking this up cold.** Read `DEVELOPMENT_SKILL.md` first (how to
> develop the package) and `SKILL.md` (how to use it). This file is the brief for
> **one job**: cut the test suite's runtime without losing coverage.
>
> Written 2026-08-27 against `tbl.now` 0.27.0, branch `ensemble-restore`,
> on an x86_64 macOS box. All timings below are **measured**, not estimated.

---

## 0. Read this before you touch anything

The number that started this job was `R CMD check`'s
`checking tests ... [28m/25m]`, which reads like an instant CRAN rejection.

**It is not the number CRAN sees.** The suite calls `skip_on_cran()` 424 times,
and *two separate things* turn those skips off behind your back:

| what | does it force `NOT_CRAN=true`? | why |
|---|---|---|
| `devtools::check()` | **yes** | its `env_vars` default is `c(NOT_CRAN = "true")` |
| `testthat::test_local()` | **yes** | it calls `testthat:::local_assume_not_on_cran()`, which sets `NOT_CRAN="true"` whenever the variable is empty |
| `testthat::test_dir()` | no | |
| `testthat::test_check()` | no | this is what `tests/testthat.R` runs under `R CMD check` |

So `env -u NOT_CRAN Rscript -e 'testthat::test_local()'` **does not measure the
CRAN path** — `test_local()` puts the variable back. I lost two ~30-minute runs
to this before spotting it. Check with `testthat:::on_cran()`, which must be
`TRUE`.

### The measured truth

```
             tests   skipped   test time   wall
CRAN path     1334       424       354 s   6.06 min     <- what CRAN runs
NOT_CRAN=true 1389         3      1848 s   30.8 min     <- what devtools::check() runs
```

**The CRAN suite is 6 minutes, not 30.** That is not a rejection, but it is not
comfortable either: CRAN wants the *whole* check under about 10 minutes, and on
this machine 0.27.0 came to 30m39s in total, of which

| stage | `NOT_CRAN=true` | CRAN path (est.) |
|---|---|---|
| `checking examples` | 95 s | 95 s |
| `checking examples with --run-donttest` | 150 s | 150 s |
| `checking tests` | 28 min | **~6 min** |
| `checking re-building of vignette outputs` | 39 s | 39 s |
| **total** | 30m 39s | **~11 min** |

So the real target is roughly **halving the CRAN-path test time**, from 6 minutes
to under 3, which brings the whole check comfortably inside 10. 

### The reproducible measurement

```r
# devel/measure_tests.R -- write this, it is worth keeping
library(testthat); library(tbl.now)
stopifnot(testthat:::on_cran())          # or you are measuring the wrong thing
setwd("tests")
res <- test_check("tbl.now", reporter = ListReporter$new(), stop_on_failure = FALSE)
df  <- as.data.frame(res)
saveRDS(df, "../cran_path.rds")
```

```bash
env -u NOT_CRAN Rscript devel/measure_tests.R      # CRAN path
NOT_CRAN=true   Rscript devel/measure_tests.R      # everything
```

`df` has one row per test with `file`, `test`, `real` (seconds) and `skipped`.
Aggregate with `aggregate(real ~ file, df, sum)`.

---

## 1. Where the time actually goes

### CRAN path — 354 s of test time (this is the one to optimise)

| file | secs | % | tests | skipped |
|---|---|---|---|---|
| `test-summary.R` | 27.5 | 7.8% | 43 | 0 |
| `test-diagnose.R` | 26.6 | 7.5% | 41 | 1 |
| `test-tidy.R` | 20.8 | 5.9% | 13 | 0 |
| `test-tbl_now.R` | 19.8 | 5.6% | 12 | 0 |
| `test-nowcast_score.R` | 18.1 | 5.1% | 22 | 0 |
| `test-confirmation.R` | 16.3 | 4.6% | 34 | 0 |
| `test-converter-censoring.R` | 15.9 | 4.5% | 19 | 0 |
| `test-converters.R` | 15.2 | 4.3% | 114 | 85 |
| `test-converter-equivalence.R` | 14.5 | 4.1% | 9 | 0 |
| `test-converter-epinow2.R` | 14.1 | 4.0% | 28 | 0 |
| `test-coverage_gaps.R` | 11.2 | 3.2% | 67 | 0 |
| `test-as_tbl_now.R` | 10.6 | 3.0% | 38 | 0 |
| `test-diagnostic_plot.R` | 10.0 | 2.8% | 11 | 0 |
| `test-report.R` | 8.6 | 2.4% | 30 | 0 |
| `test-converter-grids.R` | 8.3 | 2.4% | 15 | 2 |

**The CRAN path is flat.** No single file is more than 8%, and the top 15 come to
about 60%. There is no one thing to fix — this is death by a thousand
`tbl_now()` constructions (there are **780** of them across the test files, each
running `validate_tbl_now()`, unit inference, type inference and three derived
columns).

Slowest individual CRAN-path tests:

```
 16.6s  test-tbl_now.R      tbl_now throws warning when repeated rows
  6.9s  test-tidy.R         tidy() falls back to NA bounds when there is no pi slot
  6.1s  test-tidy.R         tidy() reports surveillance's interval width from ...
  5.0s  test-tidy.R         tidy() reads surveillance's prediction interval
  4.2s  test-autoplot.R     the plot_* twins draw the same panel as autoplot()
  3.6s  test-update.R       update keeps everything similar when nothing new is added
```

**`test-tbl_now.R:"tbl_now throws warning when repeated rows"` at 16.6 s is 4.7%
of the entire CRAN suite in one test.** Start there — it is almost certainly
building something far larger than the assertion needs.

### `NOT_CRAN=true` path — 1848 s, and one test is half of it

| file | secs | % | tests |
|---|---|---|---|
| **`test-engines-matrix.R`** | **1083.4** | **58.7%** | 10 |
| `test-converters.R` | 102.1 | 5.5% | 114 |
| `test-delay_drift.R` | 67.3 | 3.6% | 16 |
| `test-converter-datasets.R` | 47.2 | 2.6% | 8 |
| `test-autoplot.R` | 45.9 | 2.5% | 49 |

and inside that one file:

```
 955.7s  all 24 shapes really fit: diseasenowcasting     <- 52% of the WHOLE suite
  43.7s  all 24 shapes really fit: surveillance
  36.9s  each data type is either modelled or refused with a reason
  31.9s  all 24 shapes really fit: baselinenowcast
   9.2s  every engine models 0, 1 and 2 strata and labels them correctly
```

**One `test_that()` block is 16 minutes.** `REAL_SHAPES` is
`{days, weeks} x {3 data types} x {0,2 strata} x {0,2 covariates}` = 24 shapes,
each genuinely fitted. `baselinenowcast` does all 24 in 32 s and `surveillance`
in 44 s; `diseasenowcasting` takes 956 s — about **40 s per fit**. It is a Stan
model, and `helper-engines.R::engine_args()` sets only `n_draws = 100`, which
controls the *output* draws, not the sampler.

Things to try, in order:
1. `diseasenowcasting::nowcast()` has a `type` argument
   Pass `"one_stage"` should be faster if it converges.
2. Pass sampler control through `...` (fewer iterations/chains), the way
   `epinowcast` already gets `chains = 1, iter_sampling = 100` in the same helper. Try
   `n_draws = 100L`, `delay_window = 15`.
3. Shrink `n_periods = 30L` in the fixture.
4. Never cut the cross product **for any engine** .

This does not affect CRAN. It affects every developer running the suite, and it
is why a local full run is a coffee break.

---

## 2. Redundant tests: where to look

I did not audit these. Ranked by how likely I think the overlap is.

* **`test-temporal_effects.R` (882 lines) + `test-temporal_effects_2.R` (629).**
  A `_2` file is usually one nobody wanted to merge. 1,511 lines on one feature.
* **`test-converters.R` (1940 lines, 121 tests) vs the eight focused
  `test-converter-*.R` files** (`-censoring`, `-datasets`, `-epinow2`,
  `-equivalence`, `-grids`, `-pooling`, `-roundtrip-all`, `-strata-shapes`).
  Those were split out later; check whether the originals were deleted from
  `test-converters.R` or just left in place. Note `test-converters.R` skips 85 of
  its 114 tests on CRAN, so most of its cost is on the developer path.
* **`test-check.R` (554) vs `test-diagnose.R` (654).** `DEVELOPMENT_SKILL.md` §11
  says `validate_tbl_now()` and `diagnose()` are *the same engine* with two
  presentations. If both files assert the same findings on the same fixtures, one
  should assert only the presentation.
* **`test-changers.R` (101 tests) + `test-update.R` (66).** Both large, both
  exercising attribute round-trips; look for the same add/change/remove cycle
  repeated per attribute in both.
* **`test-summary.R` (27.5 s) and `test-diagnose.R` (26.6 s) are the two most
  expensive CRAN-path files.** They share an architecture (one function per
  block, all returning the same schema, `bind_rows()`-ed). If each block is
  tested against a freshly built fixture, one shared fixture would pay for
  itself twice over.

**A caution before deleting anything.** `DEVELOPMENT_SKILL.md` §9 records several
bugs that shipped *because* a test asserted the right fact about the wrong thing.
Two tests that look duplicated are sometimes covering the harmless and the
harmful variant of one bug — the per-case vs delay-derived censoring flag is the
documented example, and a test that only exercises the harmless variant passes
before and after the fix and proves nothing. Read what each one is *for*.

---

## 3. Cheap structural wins

1. **Memoise the shared fixtures.** 780 `tbl_now()` calls. The helper builders
   (`engine_fixture()` in `helper-engines.R`; `score_tbl_now()`,
   `truth_tbl_now()` in `helper-nowcast.R`) are called 15 / 10 / a few times and
   rebuilt every time. A cache keyed on the arguments is ~10 lines and touches no
   test. **Only if the fixture is treated as read-only** — several tests mutate
   theirs, so return a copy or memoise selectively.
2. **`test-tbl_now.R`'s 16.6 s duplicate-row warning test.** One test, 4.7% of
   CRAN. Look at what it builds.
3. **`test-tidy.R`: three `surveillance` interval tests at 6.9 + 6.1 + 5.0 s.**
   Eighteen seconds, 5% of CRAN, on three assertions about the same `pi` slot.
   One fitted `stsNC` shared across all three (or a stored fixture) would collapse
   this.
4. **`test-autoplot.R`: check whether any test forces a *render*.** Building a
   ggplot is cheap; `ggplot_build()`, `print()` and `ggsave()` are not.
5. **`denguedat` is a ~53,000-row line list**, used in 23 test files. Any test
   that hands it to a line-list backend (`tbl_now_to_nobbs()`,
   `tbl_now_to_surveillance()`) expands counts to one row per case. Check they
   trim first; one that does not is doing 50× the work.

---

## 4. Constraints you must not break

* **`R CMD check --as-cran` stays at 0 errors / 0 warnings / 0 notes.** It was
  clean at 0.27.0.
* **Take a coverage baseline before you start**, and do not end below it:
  ```r
  covr::package_coverage()
  ```
* **The suite must pass with no modelling package installed.** Every backend is a
  `Suggests`.
* **Work with `NOT_CRAN=true`** or you will not see the tests you are changing:
  ```bash
  NOT_CRAN=true Rscript -e 'devtools::load_all("."); testthat::test_local()'
  ```
* **Moving a test behind `skip_on_cran()` is a legitimate CRAN speed-up.
  Deleting it is a coverage loss.** A skipped test still runs in CI and locally.
* **Do not fit a real Stan/JAGS model in a new test.** Nothing in the suite does
  at full size today; the Stan paths are `local_mocked_bindings()`-ed (see
  `test-converter-epinow2.R:654`). `DEVELOPMENT_SKILL.md` §8: "prefer mocking to
  fitting".
* Never use `if(FALSE)` in an example neither `\donttest{}` or `\dontrun{}`. Change
them to `try` if you have to. 

---

## 5. What the package is, and the bits you will touch

`tbl.now` extends `tibble` into `tbl_now`: nowcasting data where every row
carries **two dates** — when the event happened and when it was reported. It
does not fit models. It prepares data for the packages that do
(`diseasenowcasting`, `baselinenowcast`, `epinowcast`, `NobBS`, `surveillance`,
`EpiNow2`, `epidist`) through `tbl_now_to_*()` converters, normalises what they
return through `tidy()`, and drives them through `run_nowcast(x, engine_*())`.

**0.27.0 changed the nowcasting front door**, and the tests were rewritten with
it. If you see the old spelling anywhere, it is stale:

* `run_nowcast(x, engine_nobbs(max_D = 10))` — the data and `verbose` are the
  only arguments outside the engine. `nowcast_method()` no longer exists;
  `engine()` is the S3 dispatch object.
* `nowcast_backtest(x, engine1, engine2, ..., now_dates =, seed =)` is variadic.
  `methods` / `method_args` are gone.
* `score_nowcast()` / `as_scoringutils()` take a **`tbl_now`** as `truth`;
  `observed_col` is gone.
* Engines carry `min_date` and `quantile_levels`.

Test helpers you will live in:

* `tests/testthat/helper-engines.R` — `engine_fixture()`, `engine_args()`,
  `engine_for()`, `try_run_nowcast()`, `available_engines()`, and `ENGINE_SPEC`,
  a table of what each backend can do. **`ENGINE_SPEC` has a `fast` flag and
  `available_engines(fast_only = TRUE)` uses it** — that mechanism exists and may
  be under-used.
* `tests/testthat/helper-nowcast.R` — the `scoretoy` backend (needs no modelling
  package), `score_tbl_now()`, `truth_tbl_now()`.

---

## 6. Definition of done

- [ ] CRAN-path test time measured **before and after**, with
      `testthat:::on_cran()` asserted `TRUE` in both runs.
- [ ] The `NOT_CRAN=true` path measured too — that is the developer experience,
      and it is currently 30 minutes.
- [ ] `R CMD check --as-cran` still 0/0/0.
- [ ] `covr::package_coverage()` not below the baseline taken first.
- [ ] No test deleted without a sentence saying what covered it instead.
- [ ] `NEWS.md` updated only if test-visible behaviour changed (it should not
      have).
- [ ] Anything learned about *why* something was slow written into
      `DEVELOPMENT_SKILL.md` §8, and the `NOT_CRAN` trap in §9 — it cost this
      session two wasted runs and it will cost the next one the same.
