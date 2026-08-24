# Plan: ensemble nowcasts

**For a new conversation.** Read `DEVELOPMENT_SKILL.md` first — it states the
package's rules and this plan assumes them. `SKILL.md` is the user-facing API.

**Goal:** a user can fit several nowcasting models from one `tbl_now`, combine
them into an ensemble with calibrated prediction intervals, `tidy()` the result
like any other fit, check the ensemble actually helps, and read a vignette that
shows the whole thing.

---

## 0. Read this first: the work already exists, in a git stash

Nothing was ever committed on the `ensemble` branch — that branch's tip is
`69d2a36`, at v0.13.x, and carries **none** of this. The work is in
**`stash@{0}`** (`On ensemble: !!GitHub_Desktop<ensemble>`, 2026-08-24).

```bash
git stash list                       # confirm stash@{0} is still there
git stash show --name-only 'stash@{0}'
```

**Do not `git stash pop` it.** The working tree is at 0.16.0 with a large
uncommitted diff, and the stash was taken against 0.13.x — popping will conflict
badly and can lose both. Extract to a scratch directory and copy in file by file:

```bash
mkdir -p /tmp/ens && cd /Users/rodzepeda/Documents/tbl.now
git stash show --name-only 'stash@{0}' | grep -vE '_cache/' > /tmp/ens/files.txt
while read -r f; do
  mkdir -p "/tmp/ens/$(dirname "$f")"; git show "stash@{0}:$f" > "/tmp/ens/$f"
done < /tmp/ens/files.txt
```

**First task of the new session: get this committed**, on a branch off the
current one, before changing anything. It has survived two stash/restore cycles
already and is one `git stash drop` from gone.

### What is in it

2272 lines of R, 41 tests, a 541-line vignette, and a cached
`ensemble-comparison.rds`.

| file | lines | what |
|---|---|---|
| `R/nowcast_class.R` | 291 | `tbl_nowcast` S7 class, `nowcast_quantile_levels()`, `is_tbl_nowcast()`, draws↔quantile helpers |
| `R/run_nowcast.R` | 393 | `run_nowcast()`, the `nowcast_fit()` / `nowcast_tidy()` extension generics, `nowcast_method()`, `list_nowcast_methods()` |
| `R/nowcast_methods.R` | 451 | backends: diseasenowcasting, baselinenowcast, epinowcast, nowcaster, NobBS |
| `R/nowcast_ensemble.R` | 418 | `nowcast_ensemble()` — quantile averaging and linear pool, weight resolution |
| `R/nowcast_score.R` | 591 | WIS, coverage, `nowcast_truth()`, `score_nowcast()`, `nowcast_backtest()`, `nowcast_weights()`, `as_scoringutils()` |
| `R/nowcast_plot.R` | 128 | `autoplot()` fan chart for `tbl_nowcast` |
| 4 test files | 739 | 41 `test_that()` blocks |
| `vignettes/articles/ensemble-nowcasting.Rmd` | 541 | 5 sections incl. a three-epidemic comparison |

The architecture is sound and worth keeping:

* **`tbl_nowcast`** (S7) holds `predictions` (long: event date, strata,
  `.quantile_level`, `.value`), optional `draws`, plus `method`, `fit`, `now`,
  `event_date`, `strata`, `data`, `call`, `metadata`. Validator checks the
  required columns and that `.quantile_level` is strictly inside (0, 1).
* **`run_nowcast(x, method, ...)`** is the one user-facing entry point. Named
  `run_nowcast()` not `nowcast()` deliberately, because diseasenowcasting exports
  `nowcast()` and both should be attachable at once.
* **`nowcast_fit()` / `nowcast_tidy()`** are S3 extension generics — the hook a
  backend author implements. They are *not* `generics::tidy()`; see §2.
* **`nowcast_ensemble(..., type = c("quantile", "linear_pool"))`** with
  `weights = "equal"` or performance weights from a backtest.

---

## 1. What must change before it will even load

The stash predates a lot. Reconcile against the current branch **first**, before
adding anything:

### 1.1 `nowcaster` is gone — remove that backend

`nowcast_fit.nowcaster()` / `nowcast_tidy.nowcaster()` call
`tbl_now_to_nowcaster()` and `get_nowcaster_strata()`, both **removed in 0.16.0**.
The stash's `DESCRIPTION` also re-adds `INLA`, `nowcaster` and two extra
repositories — which would undo the CRAN-readiness work (`Remotes:` was dropped
precisely so the package could be submitted). See the 0.16.0 `NEWS.md` entry for
the six reasons nowcaster was dropped.

**Delete the nowcaster backend.** Do not re-add the dependency.

### 1.2 Backends that must be added

* **`NobBS`** — the stash has a backend, but it builds its line list by hand
  (`data = as.data.frame(x)`). `tbl_now_to_nobbs()` now exists and expands counts
  to one row per case; the hand-rolled version silently nowcasts 1,174 rows as
  1,174 cases when the data carries 50,160. **Use the converter.**
* **`surveillance`** — no backend at all. `tbl_now_to_surveillance()` exists.
* **`EpiNow2`** — no backend (EpiNow2 was a commented-out converter then). It now
  has four targets; `estimate_infections` is the one that belongs here.

### 1.3 Reuse the `tidy()` methods, do not re-derive

`nowcast_tidy.*` in the stash re-implements per-backend summarisation that
`R/tidy.R` now does properly. An audit in 0.16.0 found and fixed real defects in
exactly that logic — check `devel/converter-audit.md` before copying anything:

* `tidy.epinowcast()` pooled every `by` group under `stratum = "all"`;
* the NobBS branch ignored `NobBS.strat()`'s `stratum` column;
* `tidy.baselinenowcast_df()` reported a zero-width band as 95%;
* `level` must be **read** from the fit, never assumed.

Wherever a `nowcast_tidy.<backend>()` method duplicates a `tidy.<class>()` method,
call the latter. DEVELOPMENT_SKILL §4: rewriting a helper by hand is a bug
waiting to happen.

### 1.4 Housekeeping

* `.Rbuildignore`, `NEWS.md`, `DESCRIPTION`, `NAMESPACE` in the stash are from
  0.13.x — take only the deltas you need (`scoringutils` in `Suggests` is wanted).
* The stash also carries unrelated `R/batch_screen.R` and
  `R/transport_discriminant.R` changes and a `_cache/` directory. **Do not copy
  those** — they are older than the current versions.
* `vignettes/references.bib` — check whether the current branch already has it.

---

## 2. The `tidy()` gap — this is the main missing piece

**There is no `generics::tidy()` method for `tbl_nowcast`.** `nowcast_tidy()` is
the *backend author's* hook, not the user's verb. So today a user can `run_nowcast()`
and `nowcast_ensemble()` but cannot `tidy()` the result, while they *can* `tidy()`
every raw engine fit. That is backwards.

Implement **`tidy.tbl_nowcast()`** returning the package's standard nowcast frame:
`event_date`, `stratum`, `estimate`, `conf.low`, `conf.high`, `level`, `engine`
(plus `q*` columns for `probs`).

Points of care, all of them lessons from the 0.16.0 audit:

* **S7 registration.** `class(x)` is `"tbl.now::tbl_nowcast"` and a `::` cannot
  appear in an S3 method name. Register in `.onLoad()`, as
  `tidy_nowcast_prediction` already is — see `R/zzz.R` and DEVELOPMENT_SKILL §7.
* **`engine`** is the `method` property — `"baselinenowcast"`, or `"ensemble"`
  (or the `name` given) for a combined nowcast.
* **`level`** must come from the quantile levels actually stored. A
  `tbl_nowcast` holds arbitrary `.quantile_level`s, so pick the widest symmetric
  pair present and derive `level` from it. Do **not** hard-code 0.95. If no
  symmetric pair exists, `level` is `NA` and the bounds are `NA` — a guessed width
  in that column defeats the column's purpose.
* **`stratum`** is `"all"` only when `strata` is `character(0)`. With several
  strata columns, paste them `" | "`-separated — the convention now used by
  `triangle_list` names, `.epinowcast_stratum()` and `.epinow2_region()`.
  `(stratum, event_date)` must be a unique key.
* **`probs`** is honourable when `@draws` is non-`NULL` and must **error** via
  `.reject_probs()` when it is not.

Also add **`tidy()` for the backtest**: `tidy.nowcast_backtest()` giving one row
per (method, now date, target) with its scores, so results go straight into
ggplot2 or `dplyr`.

---

## 3. Ensemble predictions and intervals

Mostly written; what it needs is scrutiny and tests.

### 3.1 The two combination rules

* **`type = "quantile"`** — vincentization: average the members' *values* at each
  quantile level. Narrower than the members. Needs no draws.
* **`type = "linear_pool"`** — average the *distributions*: pool draws in
  proportion to the weights, then re-quantile. Wider, and usually better
  calibrated. Needs draws, so it must fail clearly for a draws-free member.

Both must be checked for the property that actually matters:

* **quantiles are monotone in the level** after combination (a crossing means
  the interpolation is wrong);
* the ensemble sits **between** its members at every level for equal weights;
* unequal weights move it **towards the heavier member**, monotonically;
* combining a member with itself returns that member (idempotence) — a cheap,
  strong invariant that catches most averaging bugs.

### 3.2 Weights

`nowcast_weights(backtest, type = c("inverse_score", "optim", "equal"))` exists.
Check:

* weights **sum to 1** and are non-negative for every type;
* `inverse_score` gives the lower-WIS method more weight;
* the `optim` path is deterministic given a seed, and degrades to `equal` when
  the optimiser fails rather than returning `NA`;
* a member scoring WIS = 0 does not produce `Inf`/`NaN` weights (the code has a
  guard — test it).

### 3.3 Calibration is the real deliverable

An ensemble whose intervals are wrong is worse than no ensemble. Add an
**empirical coverage** check: over a backtest, the fraction of truths inside the
nominal *q*% interval should be near *q*. Assert loosely (it is stochastic) but
assert it — with a fixed seed, on simulated data where the truth is known.

Consider `simulate_batch()` / the shipped datasets for this.

---

## 4. Validating that it works

The three-epidemic comparison in the existing vignette is the right shape: for
each epidemic, snapshot at `now`, fit each member, ensemble, score everything
against the eventual truth. Reuse it.

Add, because a user will ask:

* **Does the ensemble beat its best member?** Report WIS per method and for the
  ensemble. It does not have to win — but the article must show the answer
  honestly rather than assert a benefit.
* **Does it beat the equal-weighted version?** If performance weights do not
  help on these data, say so.

`as_scoringutils()` exists; `scoringutils` is the reference implementation for
WIS and coverage. **Cross-check the hand-rolled `.wis()` against it** on the same
data — two implementations agreeing is worth more than either alone. That check
belongs in the test suite, guarded by `skip_if_not_installed("scoringutils")`.

---

## 5. Tests — use testthat, and design them

Follow DEVELOPMENT_SKILL §8. Run with `NOT_CRAN=true` or ~780 tests silently
skip:

```bash
NOT_CRAN=true Rscript -e 'devtools::load_all("."); testthat::test_local()'
```

The suite is currently **40 files, ~2620 passing, 0 failures**. Keep it there.

**Design the tests, do not just add them.** The rules that have already paid off
in this package:

* **Test the failure that actually bites.** A test that passes before and after
  the fix proves nothing. For ensembles the failures that bite are: strata pooled
  under one label, quantiles crossing after combination, weights that do not sum
  to 1, and a member silently dropped for having different quantile levels.
* **Assert on content, not absence of error.** Case totals preserved; strata
  labels preserved *and correctly paired* (use members whose values differ by an
  order of magnitude, so a mispairing is arithmetically unmistakable — that is
  what caught two `tidy()` bugs in the audit); `level` matching the quantiles
  actually stored.
* **Prefer mocking to fitting.** Every backend needs Stan, JAGS or INLA. Build
  `tbl_nowcast` objects directly — the constructor is exported for exactly this —
  and use `testthat::local_mocked_bindings()` / `local_mocked_s3_method()` for the
  backend seams. Verify against one real fit by hand, once, and keep the suite
  fast and deterministic. The existing `test-nowcast_ensemble.R` already does this
  with a `toy()` helper; extend that pattern.
* **`skip_if_not_installed()`** for every optional backend; **`skip_on_cran()`**
  for anything heavy.
* Fix the seed anywhere a fit or an optimiser is involved. `nowcast()` in
  diseasenowcasting defaults to `seed = sample.int(...)`; an unseeded call makes a
  test flaky and a benchmark unreproducible.

Suggested files (four exist; extend rather than duplicate):

| file | covers |
|---|---|
| `test-nowcast_class.R` | the S7 validator, draws↔quantile round trip, `is_tbl_nowcast()` |
| `test-run_nowcast.R` | dispatch, `quantile_levels`, error when a backend has `nowcast_fit` but no `nowcast_tidy` |
| `test-nowcast_backends.R` | one block per backend, mocked; that each reuses its `tbl_now_to_*()` converter |
| `test-nowcast_ensemble.R` | the invariants in §3.1, weight validation, incompatible members |
| `test-nowcast_score.R` | WIS/coverage vs `scoringutils`, backtest snapshots, weight properties |
| `test-tidy-nowcast.R` **(new)** | `tidy.tbl_nowcast()`: the standard contract, `level` read not assumed, strata pairing, `probs` honoured with draws and refused without |

---

## 6. The vignette

`vignettes/articles/ensemble-nowcasts.Rmd` (the stash calls it
`ensemble-nowcasting.Rmd` — keep whichever, but be consistent with `_pkgdown.yml`).

The stash's outline is good and should be kept:

1. Why ensemble at all
2. One call per model (`run_nowcast()`)
3. Scoring a nowcast
4. Ensembles — the two combination rules, and weights
5. Does the ensemble help? Three epidemics
6. Adding your own model (`nowcast_fit()` / `nowcast_tidy()`)

Follow the article conventions in `DEVELOPMENT_SKILL.md` §7:

* fits precomputed into `vignettes/articles/ensemble-comparison.rds` by a script
  in `data-raw/`, with `run_models <- FALSE` in the article so the build never
  fits anything;
* a **visible** `tidy()` call (`eval=FALSE`, copy-pasteable, no `head()`) and a
  **hidden** `echo=FALSE` chunk rendering the cached result;
* **both** the plain and the stratified case;
* plots use `.tbl_now_palette()` and the red/green grammar — **green** for the
  epidemic process (case counts, event dates), **red** for the reporting process.
  An ensemble fan chart is the epidemic process: green.
* the cache and the article are two sources of truth — change one, change both,
  and re-run the precompute.

Add a row to the package table in `nowcasting-models.Rmd` pointing at the new
article, and register the vignette in `_pkgdown.yml`.

---

## 7. Definition of done

From DEVELOPMENT_SKILL §10, plus what this feature needs:

- [ ] The stash is **committed** on a branch before anything else.
- [ ] nowcaster backend removed; no `INLA`/`Remotes:` reintroduced.
- [ ] Backends for NobBS (via `tbl_now_to_nobbs()`), surveillance and EpiNow2.
- [ ] No `nowcast_tidy.<backend>()` re-derives what `tidy.<class>()` already does.
- [ ] `tidy.tbl_nowcast()` registered in `.onLoad()`, returning the standard frame.
- [ ] `tidy.nowcast_backtest()`.
- [ ] Ensemble invariants tested: monotone quantiles, between-members,
      weight-monotonicity, idempotence.
- [ ] Empirical coverage checked on simulated data with a fixed seed.
- [ ] `.wis()` cross-checked against `scoringutils`.
- [ ] `devtools::document()` run; `NAMESPACE` and `man/` regenerated, never
      hand-edited.
- [ ] `NOT_CRAN=true` suite passes; new behaviour has new tests.
- [ ] `NEWS.md` and `SKILL.md` updated.
- [ ] The vignette builds with `run_models = FALSE`.

---

## 8. Open questions for the maintainer

1. **Where does `run_nowcast()` sit relative to the converters?** It is a second
   front door — `tbl_now_to_epinowcast()` then `epinowcast()` then `tidy()`, versus
   `run_nowcast(x, "epinowcast")`. Both should keep working; the vignettes should
   say when to reach for which.
2. **Should `nowcast_ensemble()` accept raw engine fits**, not just
   `tbl_nowcast` objects? A user holding an `epinowcast` fit and a
   `baselinenowcast_df` would reasonably expect to combine them. That means
   promoting via `tidy()` internally — pleasant, but it makes the quantile levels
   whatever `tidy()` produced rather than what was asked for.
3. **`nowcast_quantile_levels()` default** — the stash sets one. Confirm it
   matches what the forecasting hubs use, since that is what people will compare
   against.
