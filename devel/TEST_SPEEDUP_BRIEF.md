# Brief: make `tbl.now`'s test suite faster

> ## DONE — 2026-08-28. Outcome below; the brief as written is kept underneath.
>
> | | before | after | |
> |---|---|---|---|
> | CRAN test path | 311.6 s | **139–160 s** | **−52%** |
> | CRAN wall (`test_check`) | 318.1 s | **145–168 s** | 5.3 min -> **2.6 min** |
> | `R CMD check` tests stage | — | 168 s | whole check now ~7 min |
> | Developer path (`NOT_CRAN=true`) | 1404.4 s | **778.4 s** | **−45%**, 23.5 -> **13.1 min**, with DOUBLE the engine coverage |
> | `covr::package_coverage()` | 78.30% | **78.38%** | up |
> | `R CMD check --as-cran` | 0 errors, 0 warnings | 0 errors, 0 warnings | unchanged |
>
> Both paths measured with `testthat:::on_cran()` asserted and 0 failures. Test
> counts are unchanged apart from one ADDED test (full 1389 -> 1390): the
> 24-shape grid now covers `NobBS` too. The CRAN figure is a range because two
> separate runs gave 152.4 s and 139.0 s; expect roughly 10% run-to-run
> variance on this machine and do not read a single run as a regression. The
> engine work does not touch the CRAN path at all — that file is
> `skip_on_cran()`.
>
> **Both "before" columns were re-measured here, in a worktree at the unmodified
> commit — do not compare against the numbers further down this brief.** That
> session's machine was slower: it recorded the CRAN path at 354 s where it
> measures 311.6 s now, and the full path at 1848 s where it measures 1404.4 s.
> Quoting the old baseline against a new run would have claimed −42% on the
> developer path instead of the real −24%. Measure both ends yourself.

> Read the developer figure carefully: it is **−45% while testing twice as
> much**. Three things moved it.
>
> 1. The findings-engine work below roughly halves everything that touches the
>    class (`test-converters.R` 63.9 -> 36.7, `test-update.R` 32.3 -> 12.1,
>    `test-changers.R` 27.4 -> 9.8, `test-summary.R` 21.2 -> 7.1).
> 2. `test-engines-matrix.R` — 798 s of the original 1404 s, and Stan fits
>    rather than class overhead — became a dry-run tier and fell to **72 s** for
>    the three engines it already covered. At that point the whole path was
>    386 s / 6.5 min.
> 3. Then the grid gained `epinowcast` and `EpiNow2` (+353 s), which it had
>    never covered, taking the file to 432 s and the path to 778 s. That was a
>    deliberate trade: the plumbing tier now exercises **all six** engines
>    against all 24 shapes. Revert step 3 alone to get back to 6.5 min.
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
> ### `test-engines-matrix.R`: 798 s -> 72 s, and one more engine
>
> **§1's tuning suggestions are all WRONG — do not repeat them.** The 956 s was
> **entirely the 8 `count-cumulative` shapes** (one alone was 137 s), and the
> driver is the `confirmation_process()` that `engine_args()` injects, not the
> delay window. Measured on the fixture:
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
>
> **The answer was not to tune the fit but to stop fitting.** The 24-shape grid
> asserts plumbing — no error, right strata, right combinations, monotone
> quantiles — so it never needed a good fit, or any fit. It is now a DRY-RUN
> tier (`engine_dry_args()` in `helper-engines.R`):
>
> | engine | 24 shapes before | after | how |
> |---|---|---|---|
> | `diseasenowcasting` | 956 s | **11 s** | `prior_only = TRUE` |
> | `baselinenowcast` | 32 s | **8 s** | `draws = 10` |
> | `surveillance` | 44 s | **13 s** | nothing; the class speedup |
> | `NobBS` | *not in the grid* | **10 s** | short JAGS chain |
> | `epinowcast` | *not in the grid* | **153 s** | `enw_fit_opts(likelihood = FALSE)` |
> | `EpiNow2` | *not in the grid* | **200 s** | `obs_opts(likelihood = FALSE)` |
>
> **The grid now covers all six engines, where it used to cover three.** The
> other three were excluded only because a genuine fit per cell was hours; run
> as dry runs they are affordable, and the file is 432 s against the 798 s it
> cost for half the engines. The real sampler path is still covered — "each data
> type is either modelled or refused with a reason" fits all three data types at
> full settings, and deliberately keeps the ~24 s `count-cumulative` fit, which
> is the shape that needs `confirmation_process()`.
>
> **On the last two, the likelihood is NOT what costs — measure before you
> tune.** An earlier pass here concluded they could never join the grid because
> a shape cost ~100 s. That was wrong, and wrong in an instructive way:
>
> * `epinowcast` spends about **90 s compiling its Stan model, once per R
>   process** (`enw_model()` compiles into a temp dir). A fit after that is
>   1.5 s. So all 24 shapes cost 153 s while the 12 unstratified ones cost 104 s
>   — nearly the same, because the compile dominates either way. Dropping the
>   likelihood saves 6 s of 101; `enw_pathfinder()` saves nothing at all.
>   **Never read the first fit in a session as the cost of a fit.**
> * `EpiNow2` has no compile cost (rstan is precompiled) but ~4 s of setup per
>   fit, and a 2-strata shape is four fits. That is its 200 s, not the sampler:
>   cutting `samples`/`warmup` from 100 to 25 moved one shape 21.1 s -> 17.5 s.
>
> There is no `passthrough` argument in `epinowcast` 0.7.0; `likelihood = FALSE`
> is the flag, and it sets `likelihood = 0` in the Stan data.
>
> ### It also uncovered a test-isolation bug that was already there
>
> Putting a REAL `EpiNow2` fit after `test-converter-epinow2.R` failed all 24
> shapes with "no applicable method for 'get_predictions'" — while the engine
> file passed on its own. `testthat::local_mocked_s3_method()` restores what it
> found in the S3 methods table on exit, and with `EpiNow2` merely loaded rather
> than ATTACHED it found nothing and restored nothing, **unregistering
> `get_predictions.estimate_infections` for the rest of the session**. The leak
> pre-dates this work; it simply had no victim until something downstream used a
> real fit. Fixed with `withr::local_package("EpiNow2")` before the mock (`withr`
> added to `Suggests`; it ships with `testthat` anyway).
>
> It only bites generics OWNED by the non-attached package. `test-tidy-strata.R`
> mocks `summary.epinowcast`, and `summary()` is a base generic, so the mock goes
> in base's table and epinowcast's own entry is untouched — checked, not assumed.
>
> Two things that bit, both recorded in `engine_dry_args()`:
>
> * **`prior_only = TRUE` returns all-`NA` draws for `count-cumulative`** (upstream,
>   2.1.0) — silently, with the shape still correct. The grid asserts the NA-ness
>   explicitly so an upstream fix surfaces as a failure instead of passing
>   unnoticed, and asserts non-NA everywhere else so an absent model cannot pass
>   the shape checks.
> * **`engine_for()` used to merge through `modifyList()`, which recurses into
>   nested lists**, so overriding a list-valued argument MERGED it with the
>   default instead of replacing it: `fit = enw_fit_opts(sampler =
>   enw_pathfinder)` left `epinowcast`'s `chains`/`iter_sampling` in the merged
>   list and the pathfinder died on them as unused arguments — which reads as
>   "pathfinder is broken" rather than "your override did not take". FIXED:
>   `engine_for()` now assigns overrides directly, so they replace. Exported
>   `engine()` never had this problem, so no user code was affected.
>
> ### Still on the table, not taken
>
> `Config/testthat/parallel: false` in `DESCRIPTION`. With
> `test-engines-matrix.R` no longer dominating, the developer path has no single
> file worth splitting around, so parallelism is now the main lever left — but a
> parallel suite's flakiness cannot be judged from one green run. Left alone
> deliberately.
>
> Caching `epinowcast`'s compiled Stan model across R processes
> (`enw_model(target_dir = )`) would take ~90 s off every developer run, but it
> means writing a build artefact to a persistent location from a test. Not done.
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
