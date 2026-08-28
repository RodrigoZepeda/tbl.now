# Brief: make `tbl.now`'s test suite faster

> ## DONE — 2026-08-28. Outcome below; the brief as written is kept underneath.
>
> | | before | after | |
> |---|---|---|---|
> | CRAN test path | 311.6 s | **152.4 s** | **−51%** |
> | CRAN wall (`test_check`) | 318.1 s | **159.1 s** | 5.3 min -> **2.7 min** |
> | `R CMD check` tests stage | — | 158 s | whole check now ~7 min |
> | Developer path (`NOT_CRAN=true`) | 1404.4 s | **1066.7 s** | **−24%**, 23.5 -> **17.9 min** |
> | `covr::package_coverage()` | 78.30% | **78.38%** | up |
> | `R CMD check --as-cran` | 0 errors, 0 warnings | 0 errors, 0 warnings | unchanged |
>
> Both paths measured with `testthat:::on_cran()` asserted, 0 failures, and the
> same test counts as before (CRAN 1334/424 skipped; full 1389/3 skipped).
>
> **Both "before" columns were re-measured here, in a worktree at the unmodified
> commit — do not compare against the numbers further down this brief.** That
> session's machine was slower: it recorded the CRAN path at 354 s where it
> measures 311.6 s now, and the full path at 1848 s where it measures 1404.4 s.
> Quoting the old baseline against a new run would have claimed −42% on the
> developer path instead of the real −24%. Measure both ends yourself.

> The developer path improves less than the CRAN one because
> `test-engines-matrix.R` — 798 s of its 1404 s, and Stan fits, not class
> overhead — barely moves (798 s -> 727 s). Everything else roughly halves:
> `test-converters.R` 63.9 -> 39.9, `test-update.R` 32.3 -> 12.4,
> `test-changers.R` 27.4 -> 11.5, `test-summary.R` 21.2 -> 8.9.
>
> **No test was deleted, and the 24-shape cross product is untouched.**
>
> ### What was actually slow: the class, not any one test
>
> The flat profile was the clue. The cost was not in any test but in
> `validate_tbl_now()`, which runs on every `dplyr` verb, and inside it in
> **formatting messages nobody reads**. Validating a clean object formatted
> ELEVEN `cli` messages and reported ONE; `cli::format_inline()` costs ~0.5 ms
> for a static string, ~3.5 ms with `{.val {x}}`, and **~15 ms** for a hint
> interpolating a vector of row numbers (`isatty()` alone was 28% of a
> `tbl_now()` call). Each finding also built its own one-row `dplyr::tibble()`
> (~2 ms), and `dplyr::arrange()` on the resulting ten rows cost 6 ms where
> `order(..., method = "radix")` costs 0.11 ms.
>
> Fixed in `R/diagnose.R`: `.diagnose_text()` returns a **template** rather than
> a string, `.diagnose_finalise()` filters by the reporting floor **before**
> formatting, and findings stay plain lists until the one tibble the caller sees
> is assembled. `tbl_now()` **−68%**, `validate_tbl_now()` **−76%** — a user-facing
> win, not just a test-suite one. Behaviour is unchanged: `diagnose()`,
> `summary()` and `validate_tbl_now()`'s condition stream were captured across 5
> problem frames x 2 data types x 2 strata settings plus broken objects and
> compared against the unmodified package — **70/70 identical**, plus 4 targeted
> cases for the `.diagnose_join()` paths.
>
> **The trap in that change** (see `DEVELOPMENT_SKILL.md` §8): most templates are
> built inside a `for` loop over columns, axes or strata, so a deferred template
> reading the live frame would report the loop variable's LAST value in every
> row. `.diagnose_text()` therefore `rlang::env_clone()`s its `.envir`.
>
> ### The two test-level fixes (§3 of this brief)
>
> * `test-tbl_now.R` "throws warning when repeated rows" fed all 452,000 rows of
>   `flusight` through `tbl_now()` twice. Three locations prove the same thing:
>   **12.0 s -> 0.69 s**, both asserted warnings verified to still fire.
> * `test-tidy.R`'s three `surveillance` tests each ran their own MCMC.
>   One shared fit: **18 s -> ~6 s**. The test that empties the `pi` slot
>   rebinds its own copy, so the cache is safe.
>
> ### §1's `diseasenowcasting` suggestions were tried and are WRONG
>
> Do not repeat them. The 956 s is **entirely the 8 `count-cumulative` shapes**
> (one is 136 s), and the driver is the `confirmation_process()` that
> `engine_args()` injects — not the delay window. Measured on the fixture:
>
> ```
> current (n_draws = 100)                 24 s      <- weeks/count-cumulative
> + delay_window = 4                      29 s      no help; on other shapes, none
> + K = 5                                140 s      SIX TIMES WORSE
> + one_stage + K = 5                    275 s
> + one_stage + K = 5 + delay_window = 4 1447 s
> type = "one_stage" alone                23 s      -15%, the only real gain
> ```
>
> Fewer basis functions break convergence and the fitter retries, so every
> "make it smaller" lever costs more than it saves. `n_periods` was already 30.
> `test-engines-matrix.R` still fell 798 s -> 727 s on the class speedup alone,
> but it is now 68% of the developer path and the only thing left worth
> attacking there.
>
> ### Still on the table, not taken
>
> `Config/testthat/parallel: false` in `DESCRIPTION`. Turning it on would help
> the developer path, but the floor is the slowest single FILE, and
> `test-engines-matrix.R` is 727 s of the 1067 s — so it would need that file
> split per engine to pay off, and a parallel suite's flakiness cannot be judged
> from one green run. Left alone deliberately; it is the obvious next move if
> the developer path matters more than it does today.
>
> `devel/measure_tests.R` is the reproducible measurement this brief asked for.

---


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
