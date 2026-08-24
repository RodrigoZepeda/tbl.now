# Verification brief: is the \pkg{EpiNow2} integration actually correct?

**For a fresh session.** You are auditing work you did not do. Everything you need
is below — do not assume the implementer was right about anything, including the
claims in this file. `devel/epinow2-integration-plan.md` records what was
*intended*; this brief is about whether the code *does* it.

**Environment:** repo root is the working directory, branch `vignettes`. EpiNow2
**1.9.0**, epidist 0.4.0, cmdstan 2.39 at `~/.cmdstan/cmdstan-2.39.0` — all
installed and working. Run tests with `NOT_CRAN=true` or ~780 silently skip:

```bash
NOT_CRAN=true Rscript -e 'devtools::load_all("."); testthat::test_local()'
```

Read `DEVELOPMENT_SKILL.md` first — it states the package's rules — and treat it
as fallible: the converter audit that preceded this work found a claim in it that
the code contradicted.

---

## What was built

| Thing | Where |
|---|---|
| `tbl_now_to_EpiNow2(x, target = …)`, four targets | `R/converters.R` |
| `tbl_now_from_EpiNow2()`, `as_tbl_now.tbl_now_epinow2_snapshots()` | `R/converters.R` |
| `tbl_now_epinow2_snapshots` thin class + `print()` | `R/converters.R` |
| `.epinow2_grid()`, `.epinow2_step_days()`, `.epinow2_region()`, `.epinow2_snapshots()`, `.epinow2_dist_data()`, `.epinow2_series_data()` | `R/converters.R` |
| `.delay_censoring_windows()` — shared with `tbl_now_to_epidist()` | `R/converters.R` |
| `.drop_unusable_counts()` — shared, renamed from `.epidist_drop_unusable_counts()` | `R/converters.R` |
| `tidy.estimate_infections/.epinow/.estimate_truncation/.estimate_dist`, `.epinow2_interval()`, `regional_epinow` branch of `tidy.list()` | `R/tidy.R` |
| Tests | `tests/testthat/test-converter-epinow2.R` (81 expectations) |

The four targets:

| `target` | returns | EpiNow2 function |
|---|---|---|
| `"estimate_infections"` (default) | `data.frame(date, confirm[, accumulate])` | `estimate_infections()`, `epinow()` |
| `"regional_epinow"` | the same + `region` | `regional_epinow()` |
| `"estimate_truncation"` | `tbl_now_epinow2_snapshots` (a list) | `estimate_truncation()` |
| `"estimate_dist"` | `data.frame(pdate_lwr, pdate_upr, sdate_lwr, sdate_upr, obs_date[, n])` | `estimate_dist()` |

---

## Priority 1 — the two things most likely to be wrong

### 1.1 The daily grid and `accumulate`

**Why it matters.** EpiNow2 models a **daily** process and has no `timestep`,
`interval` or `period` argument on any entry point. A weekly series passed as one
row per week is read as one row per **day** — no error, an epidemic seven times
too fast. `.epinow2_grid()` lays it on the daily grid with EpiNow2's `accumulate`
column instead.

**This was checked before the brief was finished, and the first version was
wrong.** Recorded here because the residue matters.

A hand-rolled grid put each period's count on the period's **last** day.
`EpiNow2::fill_missing()` leaves it on the date the caller gave and back-fills the
days *before* it — same `accumulate` pattern, dates shifted by six, every weekly
fit misaligned with no error. It also wrote `0` on filler days where
`fill_missing()` writes `NA`. `.epinow2_grid()` now **delegates to
`fill_missing()`**, and `test-converter-epinow2.R` pins the two as identical.

A second, nastier one: with `by`, EpiNow2 1.9.0's inferred `initial_accumulate`
**drops each group's first observation** — 336/167 cases became 295/147, grid
starting the day after the first report. Passing `initial_accumulate` explicitly
fixes it. A regular synthetic series does **not** reproduce this; it took real
data. There is a regression test.

Still open, and now the top item here:

1. **Does EpiNow2 read `accumulate` at all for `estimate_truncation()`?** The
   converter emits it on those snapshots. `?estimate_truncation` documents only
   `date` and `confirm`. If the column is ignored there, each snapshot is a daily
   frame that is `NA` six days in seven, and the truncation estimate may be
   nonsense — or the model may reject the `NA`s outright. **Fit it and see.** This
   is now the single highest-value check in this brief.
2. Is the `by`-path `initial_accumulate` behaviour an EpiNow2 bug worth reporting
   upstream? Reproduce it minimally against `fill_missing()` alone and decide.
3. Confirm the `NA` filler values are what `estimate_infections()` wants. The docs
   say "If all entries of `confirm` are missing (`NA`) the returned estimates will
   represent the prior" — that is about *all* entries, but check partial-`NA`
   behaviour is what we think.

### 1.2 `obs_date` on the `estimate_dist` target — settled, but check the reasoning

`estimate_dist()` asserts `obs_date >= sdate_upr` on **every** row, and a real fit
failed on it (`5 observation(s) have obs_date earlier than sdate_upr`).

The resolution rests on the two being different quantities:

* `[sdate_lwr, sdate_upr)` brackets **when the report happened**. At weekly
  resolution that is `[W, W + 7)` — a half-open interval whose upper bound is the
  *end of that week*, not a claim that anything happened on day `W + 7`.
* `obs_date` is **when observation stopped**, an instant, used for right
  truncation.

A `tbl_now`'s `now` *labels a period*, so the instant observation stopped is the
end of it: **`obs_date = now + w`**. The assertion then holds by construction and
nothing is observed after it.

Clamping the windows at `now` was tried and reverted: it moves reports in the
final period into an earlier period, and the epidist round-trip test caught it.

**What to check:** that `w` is the right width for every unit (`.epidist_window_days()`),
and that `obs_date = now + w` is what \pkg{EpiNow2} means by an observation date
rather than `now + w - 1` — an off-by-one here shifts the truncation correction by
one period and nothing would flag it.

---

## Priority 2 — conservation and round trips

These are assertions the test file already makes. **Re-derive them independently**
rather than reading the tests and nodding.

1. **`sum(confirm)` equals the case total** of `get_latest_reported_cases(x)`, for
   a line list, `count-incidence` and `count-cumulative` input.
2. **Per region**, the same, with regions of very different sizes so a mispaired
   label is arithmetically obvious. (The converter audit found two `tidy()`
   methods that pooled strata under one label; assume nothing.)
3. **Snapshot *k* equals the series as known at report date *k*.** Cross-check
   against `get_nth_reported_cases()`, not against the converter's own snapshots.
4. **Round trip.** `tbl_now → snapshots → as_tbl_now()`: every case back, and
   **no negative delays**. Since the grid now comes from `fill_missing()`, each
   observation stays on the date it was given, so the inverse just drops the
   `accumulate` rows — no date arithmetic. Two traps that already bit here and are
   worth re-checking: `data[ord]` on the classed list **strips every attribute**,
   so all attribute reads must happen before any subsetting; and the snapshots
   must be differenced in report-date order, not list order.
5. **`EpiNow2::example_truncated`.** The strongest check available, because
   neither end is ours. Confirm the report dates the test derives
   (`max(s$date)` per snapshot) are actually right — is the last date of a
   snapshot really when it was taken? Consider whether EpiNow2 documents this
   anywhere; if not, say the test rests on an assumption.

---

## Priority 3 — `tidy()`

1. **`level` is read, not assumed.** `.epinow2_interval()` picks the widest
   `lower_<pct>`/`upper_<pct>` pair present and derives `level` from the column
   name. Fit with `CrIs = c(0.5, 0.95)` and confirm `level == 0.95`; with the
   default confirm `0.90`. Confirm `NA` when there are no interval columns.
2. **`tidy.estimate_dist()`'s `mean`/`sd` rows are the DELAY distribution's
   moments**, derived per draw via `fix_parameters()` → `discretise()` → PMF, so
   no family is ever named in our code. Verify: (a) the moments match the closed
   forms for each of the five families (mean should be exact, sd ~1% high from
   binning); (b) the parameter order in `.epinow2_delay_draws()` really matches
   the draws columns for a **gamma** fit, where the family's order (`shape`,
   `rate`) differs from alphabetical — a silent transposition here would be
   invisible; (c) it degrades gracefully when `discretise()` fails.
   Do **not** confuse these with `summary()`'s `mean`/`sd` columns, which are the
   posterior mean and sd of each *parameter*; `tidy.epidist_fit()` reports
   `mean`/`sd` as *rows* meaning the
   fitted *distribution*'s mean and sd. Confirm the two `tidy()` methods cannot be
   confused, and consider whether the naming collision deserves more than a doc
   note.
3. **`tidy.epinow()`** reads `x$estimates %||% x`. **This is unverified** — no
   `epinow()` fit was run. Check `?epinow` and the object's real structure; if
   `$estimates` is not where the `estimate_infections` fit lives, this method is
   broken and its test (which mocks `get_predictions`) would not catch it.
4. **The `regional_epinow` detector** keys on `x$regional` being a non-empty list.
   Confirm that is what `regional_epinow()` actually returns — `?regional_epinow`
   says "a list of output stratified at the top level into regional output and
   across region output summary output", which is suggestive but not proof. **No
   real `regional_epinow()` fit was run.**

---

## What was NOT verified — start here

Stated plainly so it is not mistaken for tested ground:

| Claim | Status |
|---|---|
| `estimate_dist` target → real fit → `tidy()` | **verified**, real cmdstan fit, `attr(summary, "n_obs") == sum(n) == 450` |
| `example_truncated` round trip | **verified**, 172,832 cases recovered exactly |
| All four targets produce the documented columns | **verified** by tests |
| `estimate_infections()` accepts the output and fits sensibly | **NOT RUN** |
| `regional_epinow()` accepts the output | **NOT RUN** |
| `estimate_truncation()` accepts the snapshots and fits | **NOT RUN** |
| `epinow()` object structure for `tidy.epinow()` | **NOT RUN** |
| The `accumulate` layout matches `fill_missing()` | **verified** — it did not, and was fixed; now pinned by test |
| `estimate_truncation()` reads `accumulate` at all | **NOT CHECKED** — see 1.1 |

The first four are one script away and are the point of this session. Each needs
cmdstan and takes minutes, so run them in the background and do the reading while
they compile.

**Suggested fits**, smallest useful:

```r
x <- tbl_now(denguedat_slice, event_date = "onset_week",
             report_date = "report_week", data_type = "linelist")

# 1. estimate_infections -- does the accumulate grid fit sensibly?
d <- tbl_now_to_EpiNow2(x, quiet = TRUE)
EpiNow2::estimate_infections(d, generation_time = gt_opts(...), ...)

# 2. the control: hand-build the daily frame yourself and compare
#    (this is what proves 1.1)

# 3. estimate_truncation
EpiNow2::estimate_truncation(
  tbl_now_to_EpiNow2(x, target = "estimate_truncation", quiet = TRUE)
)

# 4. regional_epinow, two regions
```

---

## The cross-package check worth doing

`tbl_now_to_epidist()` → `epidist::epidist()` and
`tbl_now_to_EpiNow2(target = "estimate_dist")` → `EpiNow2::estimate_dist()` fit
**the same delay distribution to the same `tbl_now`** through two different Stan
models in two different packages, from a shared window builder.

They should agree within uncertainty. A large disagreement means the censoring
windows are wrong in a way neither package will tell you about, and no internal
test can catch. This is the strongest correctness signal available and it exists
only because both converters now share `.delay_censoring_windows()`.

Note `?tbl_now_epidist` records that `epidist::as_epidist_marginal_model()` fails
at Stan compilation with epidist 0.4.0 / primarycensored 1.5.1 — use the latent
model, on a short window, or check whether that is still true.

---

## Ground rules

* **Verify, do not assume.** Run the code. If you claim something is wrong, show
  the failing call and its output. If you claim it is right, say what you ran.
* **Report honestly.** If a check was not run, say so rather than implying it
  passed. Distinguish "confirmed bug" from "suspicious, unverified".
* Do not "fix" behaviour that is deliberate — the comments record why. Flag
  disagreements instead of silently changing them.
* The suite must still pass: `NOT_CRAN=true`, 40 files, currently 0 failures.
* If you change converter behaviour, `data-raw/converter_matrix.R` now has three
  EpiNow2 rows and its cached `.rds` will be stale — regenerate it (~35 min,
  dominated by one `diseasenowcasting` cell) or say it is stale.

**Deliverable:** findings ranked by severity, each with a reproducible snippet;
any fixes with tests; the suite passing; and an explicit list of what you did not
check.
