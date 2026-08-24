# Audit: converters and `tidy()` against the target packages' own documentation

**Date:** 2026-08-24 · **Branch:** `vignettes` · **Package version:** 0.16.0

Scope: `R/converters.R` and `R/tidy.R`, checked against the *installed* help
pages and source of `diseasenowcasting` 2.1.0, `baselinenowcast` 0.2.0,
`epinowcast` 0.7.0, `epidist` 0.4.0, `NobBS` 1.1.1, `surveillance` 1.26.1,
`tsibble` 1.2.0 and `data.table` 1.18.4.

Every claim below was executed. Where a real fit was needed and possible, one was
produced: a two-group `epinowcast` fit through cmdstan 2.39, a two-stratum
`NobBS.strat()` fit through JAGS, and a `surveillance::nowcast()` fit. What was
*not* run is stated as such.

`tbl_now_to_EpiNow2()` is commented out in `R/converters.R` and was not audited.
`diseasenowcasting` has no converter (it consumes a `tbl_now` directly), and from
2.1.0 it registers its own `tidy()` method, so `tbl.now`'s
`tidy_nowcast_prediction()` is an unreachable fallback for older versions —
see finding 11.

---

## Summary

| # | Severity | Finding | Status |
|---|---|---|---|
| 1 | **High** | `tidy.epinowcast()` pools every `by` group under `stratum = "all"` | **fixed** |
| 2 | **High** | `tidy.list()` ignores `NobBS.strat()`'s `stratum` column | **fixed** |
| 3 | **High** | `tidy.baselinenowcast_df()` quotes a zero-width band as 95% on a point fit | **fixed** |
| 4 | Medium | `tbl_now_to_nobbs()` / `tbl_now_to_surveillance()` fabricate 1970 dates from a `numeric` grid | **fixed** |
| 5 | Medium | A negative delay silently loses cases in the triangle back-ends, as an *observed* zero | **fixed** (warns) |
| 6 | Medium | `tbl_now_to_epidist()` on `count-cumulative` data dies on an upstream assertion | **fixed** |
| 7 | Low | `tbl_now_to_nobbs(verbose = TRUE)` prints a `units` value `NobBS()` rejects | **fixed** |
| 8 | Low | `tidy()` has no path for the per-stratum list of fits the docs recommend building | **fixed** |
| 9 | Low | `DEVELOPMENT_SKILL.md` claims `tbl_now_to_surveillance()` sets `control$dRange`; it does not | **fixed** |
| 10 | Low | `level = 0.95` for NobBS is an unrecoverable assumption, previously undocumented | **fixed** (now `NA`, settable) |
| 11 | Low | `tidy.epidist_fit()` pools quantiles over draws *and* observations | **fixed** (warns) |

All eleven were addressed. Findings 1–4 and 7 were fixed in the first pass,
because each contradicts the package's *own* documented contract. Findings 5, 6,
8, 9, 10 and 11 needed a design decision, which the maintainer made; the
resolution is recorded under each finding below. All changes are in `NEWS.md`,
and two are deliberate **behaviour changes**: NobBS's `level` and the new
warnings.

---

## 1. `tidy.epinowcast()` pools every `by` group under `stratum = "all"` — High

**What the code assumed.** That `summary(fit, type = "nowcast")` is one block, so
a single `stratum` label suffices. `tidy.epinowcast()` passed no `stratum`
argument at all, taking `.tidy_nowcast_frame()`'s `"all"` default.

**What the package actually does.** `?summary.epinowcast` returns
`enw_nowcast_summary()`, whose source ends with

```r
data.table::setorderv(nowcast, c(".group", "reference_date"))
```

— i.e. one block per `.group`, with the grouping columns alongside. The summary
carries `.group`, `age_group` *and* `location`.

**What breaks.** On a real fit with `by = "age_group"` (age groups `00+` and
`80+`, `germany_covid19_hosp`, `max_delay = 10`, 10 reference dates):

```r
fit <- epinowcast::epinowcast(pobs)      # pobs built with by = "age_group"
out <- tidy(fit)
unique(out$stratum)                      # "all"
nrow(out)                                # 20
sum(duplicated(out[, c("stratum", "event_date")]))   # 10
```

Ten of twenty rows are duplicate keys. A downstream `left_join()` on
`(event_date, stratum)` fans out; a `dplyr::filter(stratum == "x")` finds
nothing; a plot draws two overlaid series as one. `tidy_nowcast_prediction()`
for `diseasenowcasting` in the same file does emit one block per stratum, so the
contract was inconsistent across engines — and `?tidy.nowcast` says `"all"` means
*"the fit is unstratified"*.

**Secondary defect on the same path.** With `probs`, the samples were split on
`reference_date` alone:

```r
grouped <- split(samples$sample, as.Date(samples$reference_date))
```

`split()` sorts by date; the summary is ordered by `.group` then date. On a
stratified fit each date's quantiles were therefore handed to whichever stratum
sorted first — a silent swap, not an error.

**Fix.** New internal `.epinowcast_stratum()` reads the labels from the fit's own
`by` columns (`" | "`-joined for several, matching the `triangle_list`
convention), and the `probs` split is keyed on `(stratum, reference_date)` and
indexed by the summary's own rows. After the fix, on the same fit:

```
strata: 00+, 80+   rows: 20   duplicated keys: 0
q5 == conf.low: TRUE           # epinowcast's own q5 reproduced per row
estimates match the per-stratum summary rows: 20/20
```

`enw_example("nowcast")` (ungrouped, `by = list(NULL)`) still returns `"all"`.

---

## 2. `tidy.list()` ignores `NobBS.strat()`'s `stratum` column — High

**What the code assumed.** That a list with an `estimates` element carrying
`onset_date` is a plain `NobBS()` fit — one pooled series.

**What the package actually does.** `NobBS::NobBS.strat()` exists, takes a
`strata` argument, and builds `estimates` as an array with a
**`stratum`** dimension name, flattened to a data frame:

```r
estimates <- array(NA, dim = c(now.T, 4, S),
                   dimnames = list(NULL, c("estimate","lower","upper","stratum"), strat))
```

The result has columns `estimate, lower, upper, stratum, q_0.025 … q_0.975,
stratum.1, onset_date, n.reported`. `.tidy_detect_engine()` matches it — it has
`estimates` and `onset_date` — and the NobBS branch then read only
`estimate`/`lower`/`upper`/`onset_date`.

**What breaks.** A real `NobBS.strat()` fit through JAGS on `denguedat` split
into two strata:

```r
fit <- NobBS::NobBS.strat(ll, now, units = "1 week",
                          onset_date = "onset_date", report_date = "report_date",
                          strata = "grp")
unique(fit$estimates$stratum)     # "odd" "even"
out <- tidy(fit)
unique(out$stratum)               # "all"
nrow(out)                         # 44
sum(duplicated(out[, c("stratum","event_date")]))   # 22
```

**Fix.** The branch reads `est$stratum` when the column is present and falls back
to `"all"` otherwise. After the fix: `strata: even, odd`, 44 rows, 0 duplicate
keys. An unstratified `NobBS()` fit is unchanged. Note `stratum.1` (a duplicate
`cbind()` artefact of `NobBS.strat()`) is *not* used.

---

## 3. `tidy.baselinenowcast_df()` fabricates a 95% band on a point fit — High

**What the code assumed.** That a `baselinenowcast_df` always holds draws, so the
2.5% and 97.5% quantiles of each reference date are an interval.

**What the package actually does.** `?baselinenowcast` documents
`output_type = c("samples", "point")`, and `new_reporting_triangle`'s sibling
`new_baselinenowcast_df()` asserts and stamps the choice:

```r
assert_choice(output_type, choices = c("samples", "point"))
baselinenowcast_df$output_type <- output_type
```

A `"point"` fit holds exactly one row per reference date.

**What breaks.** Before the fix:

```r
fit <- baselinenowcast::baselinenowcast(
  baselinenowcast::example_reporting_triangle, output_type = "point")
tidy(fit)
#>   event_date stratum estimate conf.low conf.high level          engine
#> 1 2024-01-01     all 197.0000 197.0000  197.0000  0.95 baselinenowcast
#> 4 2024-01-04     all 180.5919 180.5919  180.5919  0.95 baselinenowcast
```

`conf.low == conf.high == estimate` with `level = 0.95`. Any scoring or
comparison code that trusts `level` — which is exactly what `level` exists for —
treats a point estimate as a perfectly precise 95% interval. `probs` "worked"
too, returning the point estimate under a quantile's name, which is the
"approximation dressed up as a quantile" `.reject_probs()` was written to
prevent.

**Fix.** The `output_type` column is read: a point fit gets `NA` bounds and `NA`
`level`, and `probs` aborts pointing at `output_type = "samples"`. A samples fit
is unchanged (`level = 0.95`, real bounds).

---

## 4. A `numeric` grid becomes 1970 dates in the line-list back-ends — Medium

**What the code assumed.** That the event and report columns are `Date`s, so
`as.Date()` is a harmless idempotent coercion. Both
`tbl_now_to_nobbs()` and `tbl_now_to_surveillance()` did:

```r
linelist[[event_col]]  <- as.Date(linelist[[event_col]])
linelist[[report_col]] <- as.Date(linelist[[report_col]])
```

**What the attribute says.** `get_event_units()` may be `"numeric"`, in which
case the two columns are integer *indices* — `tbl_now()` requires them to be
integers, not dates. `as.Date(1L)` uses the 1970-01-01 origin.

**What breaks.** Before the fix:

```r
x <- tbl_now(data.frame(ev = c(1L,1L,2L,3L), rp = c(1L,2L,2L,3L), n = c(2,1,3,5)),
             event_date = "ev", report_date = "rp", case_count = "n",
             data_type = "count-incidence", verbose = FALSE)
head(tbl_now_to_nobbs(x, verbose = FALSE), 2)
#>   onset_date report_date
#> 1 1970-01-02  1970-01-02
#> 2 1970-01-02  1970-01-03
```

No error, no warning. The delays are right (index differences equal day
differences), so a fit succeeds and every `event_date` in the tidied output is a
1970 date. This is precisely what DEVELOPMENT_SKILL.md §7 forbids: *"Resolve
units from the attribute, never assume days."* `tbl_now_to_baselinenowcast()` and
`tbl_now_to_epinowcast()` already refused a numeric grid by name, so the
behaviour was also inconsistent between converters.

**Fix.** Two helpers, mirroring the existing `.epinowcast_timestep()` and
`.baselinenowcast_delays_unit()`:

* `.nobbs_units()` — `days` → `"1 day"`, `weeks` → `"1 week"`, otherwise abort.
  (`?NobBS` : *"units — Time scale of reporting. Options: "1 day", "1 week""*.)
* `.surveillance_aggregate_by()` — `days`/`weeks`/`months`/`years` mapped to the
  strings `linelist2sts()` accepts, abort on `numeric`. Resolved **before** the
  line list is expanded, so the abort is fast; an explicit `aggregate_by`
  argument still overrides it.

This also fixed a smaller bug: `years` previously fell through the `switch()`
default to `"1 week"`, even though `linelist2sts()` accepts `"1 year"`.

---

## 5. A negative delay silently loses cases in the triangle back-ends — Medium

**Fixed:** the converters now warn.

`tbl_now()` accepts a report before its event (it warns once, at construction:
`1 row(s) have a report_date before event_date`), and `.delay` goes negative. A
reporting triangle's delay axis starts at 0, so the cell has nowhere to go.

```r
x <- tbl_now(data.frame(ev = as.Date("2024-01-05") + c(0,1,2,3),
                        rp = as.Date("2024-01-05") + c(-2,1,4,3),
                        n  = c(1,2,3,4)),
             event_date = "ev", report_date = "rp", case_count = "n",
             data_type = "count-incidence", verbose = FALSE)
tri <- tbl_now_to_baselinenowcast(x, verbose = FALSE)
sum(tri, na.rm = TRUE)   # 9   -- 10 cases went in
tri[1, 1]                # 0   -- rendered as an OBSERVED zero, not NA
```

Two things are wrong at once: a case disappears with no warning from the
converter, and the affected cell reads `0` rather than `NA`, so the
observed-zero / not-yet-observed distinction the round-trip machinery works hard
to preserve is broken at exactly the cell that lost data.
`tbl_now_to_epinowcast()` drops it the same way (the completed grid's `confirm`
sums to 9). `tbl_now_to_nobbs()` and `tbl_now_to_surveillance()` count rows and
keep all 10. `tbl_now_to_epidist()` refuses, via epidist's own assertion
(`Element 1 is not >= 0`).

**Fix.** `.warn_negative_delays()` runs in both triangle formats of
`tbl_now_to_baselinenowcast()` and in `tbl_now_to_epinowcast()` (before the
cumulative coercion, which would otherwise have removed the evidence). It names
the rows, the cases, and the earliest delay, says the cell will read `0`, and
gives the filter:

```
1 row, carrying 1 case, has a negative delay (earliest -2).
! `tbl_now_to_baselinenowcast()` indexes by delay from 0, so it cannot be
  represented and is dropped.
i The affected cell then reads "0", which is indistinguishable from an observed zero.
i Filter them out yourself to choose what happens, e.g. `dplyr::filter(x, .delay >= 0)`.
```

`format = "long"` has no delay axis, keeps the row, and stays quiet — a warning
there would be crying wolf. Non-negative data is silent. The counts in the
message are data-type aware: rows for `count-cumulative` (a cumulative total is
not a number of cases), cases for the other two.

---

## 6. `tbl_now_to_epidist()` cannot take `count-cumulative` data — Medium

**Fixed:** the unusable rows are dropped inside the converter.

`tbl_now_to_epidist()` de-accumulates cumulative input before building `n`:

```r
if (format == "aggregate" && data_type == "count-cumulative") {
  x <- to_count(x, to = "count-incidence")
}
```

epidist asserts `n >= 1`. De-accumulation produces a `0` whenever a report added
nothing — which happens in essentially every real cumulative series — and a
negative on any downward revision.

```r
tbl_now_to_epidist(x_cumulative, verbose = FALSE)
#> Error: Assertion on 'data$n' failed: Element 6 is not >= 1.
```

This was already the only `error` cell for `flusight` in the shipped converter
matrix (`vignettes/articles/converter-matrix.rds`). Note it is not only a
*cumulative* problem: plain `count-incidence` data that has been through
`complete_zeroes()` carries zeros too, and would have failed identically.

**Fix.** `.epidist_drop_unusable_counts()` runs for every `aggregate` conversion,
after any de-accumulation:

* **zeros and missing counts** are dropped. A cell that carries no case
  contributes nothing to a delay distribution, so this is lossless; it is
  reported only under `verbose = TRUE`.
* **negatives** are dropped with a **warning** — a downward revision is real
  information being discarded, and the user should know.
* if nothing usable is left, the converter **aborts** naming the cause, instead
  of letting `Assertion on 'data$n' failed` through.

`flusight` now converts: 254 rows, 15,851 cases, every `n >= 1`. The converter
matrix was regenerated so the article and the code agree.

---

## 7. `tbl_now_to_nobbs()` printed a `units` value NobBS rejects — Low, fixed

The verbose summary interpolated the object's own units:

```r
cli::cli_li("{.arg units} <- {.val {get_event_units(x)}}")   # -> "weeks"
```

`?NobBS` documents `units` as `"1 day"` or `"1 week"`. Pasting `"weeks"` into the
call does not fail cleanly:

```
Warning: In max((moving_window - 1) * unit.num, (now.T - 1) * unit.num) :
  no non-missing arguments to max; returning -Inf
Error: replacement has 1 row, data has 0
```

Every other converter's verbose block prints values that can be pasted straight
into the target call (`dEventCol`, `aggregate.by = "1 week"`, `timestep`,
`max_delay`). Now prints `"1 week"`.

---

## 8. `tidy()` cannot tidy a per-stratum list of fits — Low, fixed

`?tbl_now_triangle_list` tells the reader:

```r
triangles <- tbl_now_to_baselinenowcast(x, format = "triangle_list")
lapply(triangles, baselinenowcast::baselinenowcast)
```

`tidy()` on the result:

```
Error: Don't know how to `tidy()` this list.
i Supply `engine` explicitly, e.g. `tidy(x, engine = "NobBS")`.
```

The workflow the docs recommend produced something the output-normalising layer
could not consume, and the error steered the reader towards the wrong package.

**Fix.** `.tidy_detect_engine()` now recognises a list whose elements are *all*
`baselinenowcast_df` and returns `"baselinenowcast"`; `tidy.list()` tidies each
element and labels it with its list name, or with its position when the list is
unnamed. `probs` passes through. A list with only *some* such elements is still
refused, and the error message now names both shapes it does understand rather
than only NobBS.

---

## 9. `DEVELOPMENT_SKILL.md` disagrees with the code on `dRange` — Low, fixed

Section 2 states:

> for line-list engines you must pass the grid explicitly (this is why
> `tbl_now_to_surveillance()` sets `control$dRange`)

It does not. `tbl_now_to_surveillance()` returns a line list (or an `sts`) and its
own help page says the opposite — *"`now` and the delay unit are **not** baked
into the result: pass them from the object"*. The article
(`vignettes/articles/nowcasting-models.Rmd`) passes `dRange` by hand, which is
the actual contract. `grep -rn dRange R/` finds only a comment.

**Fix.** Section 2 now states the real rule — the grid is the *caller's* job for
the line-list engines, because the object already carries `now` and the units and
the converter cannot know which window you mean to fit — and points at the
article's `surveillance` section as the worked example.

---

## 10. `level = 0.95` for NobBS is an unrecoverable assumption — Low, fixed

`NobBS()` computes `lower`/`upper` from `specs$conf`:

```r
probs = c((1 - specs$conf)/2, 1 - ((1 - specs$conf)/2))
```

`conf` defaults to `0.95`, but its return value is
`list(estimates, estimates.inflated, nowcast.post.samps, params.post)` — `specs`
is not among them, so `tidy()` genuinely cannot recover the width.

**Fix (behaviour change).** `level` is now `NA_real_` for a NobBS fit rather than
a guessed `0.95`, and `tidy()` gained a `level` argument so the caller can supply
what they actually asked for:

```r
tidy(fit)$level              # NA
tidy(fit, level = 0.95)$level  # 0.95
```

A guessed default is worse than `NA` in the one column that exists to stop widths
being compared blindly — the same reasoning as finding 3. The assertion in
`test-tidy.R` that recorded the old behaviour was updated, and the change is in
`NEWS.md`.

Two related points, both fine as they stand: `.reject_probs()`'s message says
NobBS "does not keep posterior draws", which is accurate *per date* — NobBS does
return `nowcast.post.samps`, but only for the nowcast date, not per reference
date. And NobBS's `estimates` does carry `q_0.025 … q_0.975` columns from
`specs$quantiles`; `?tidy.nowcast` already points users at that as the way to get
quantiles from NobBS.

---

## 11. `tidy.epidist_fit()` pools over draws *and* observations — Low, fixed

`epidist::predict_delay_parameters()` returns one row per
**draw × observation**:

```r
samples_df <- expand.grid(draw = seq_len(nrow(lp_mu)), index = seq_len(ncol(lp_mu)))
```

`tidy.epidist_fit()` takes `stats::quantile()` over the whole column. For an
intercept-only delay model (`mu ~ 1`) every observation shares the draw's value,
so the interval is exactly the posterior interval. For a fit with covariates in
the delay model — the case the method's own `newdata` argument exists for
(`formula = mu ~ 1 + gender`) — the reported interval is a *mixture* across
covariate levels, not a posterior interval for a parameter. The docs describe
`estimate` as "Posterior median" without noting this.

Verified by source reading, **not** by fitting an epidist model with covariates
(that needs a Stan compile). Also by source reading: `add_mean_sd()` is already
applied inside `predict_delay_parameters()`, so `tidy.epidist_fit()`'s second
call is redundant — idempotent for the shipped families, so harmless.

**Fix.** The numbers are right for what they are; what was missing was saying
what they are. `.warn_epidist_mixture()` checks a **single draw**: if a parameter
takes more than one value within one draw it is a function of the data, so the
pooled quantile is a mixture. When that happens `tidy()` warns, names the varying
parameters, and points at `newdata`:

> This delay model has covariates: `mu` takes a different value for different
> observations. `estimate` and the interval pool over draws *and* observations,
> so they describe the mixture across covariate levels, not one level's
> posterior. Pass `newdata` with the covariate combination you want.

Checking one draw keeps this `O(n_obs)` rather than `O(n_draws × n_obs)`. An
intercept-only fit is unaffected and silent.

---

## Checks that came out clean

These were run and found correct; they are listed so the report says what was
covered, not only what failed.

* **`tidy.stsNC()`.** `control$alpha` is `0.05` by default per `?nowcast`, and
  the fitted object really does carry it in `@control$alpha`; `1 - alpha` is
  right. `?nowcast` describes a **univariate** series, so `pi[keep, 1L, 1L]`'s
  hard-coded unit index is safe. Verified on a real `bayes.notrunc.bnb` fit:
  `dim(pi) = 655 x 1 x 2`, `dimnames(pi)[[3]] = c("2.5%","97.5%")`, and the six
  non-`NA` `upperbound()` rows line up with the six non-`NA` `pi` rows, giving
  `conf.low <= estimate <= conf.high` throughout.
* **`epinowcast`'s `level = 0.90`.** `?enw_nowcast_summary` documents
  `probs = c(0.05, 0.2, 0.35, 0.5, 0.65, 0.8, 0.95)`, so the `q5`–`q95` band the
  method reads is 90%, not 95%. The comment in the code is correct.
* **`epidist` string arguments.** `as_epidist_linelist_data.data.frame()` and
  `as_epidist_aggregate_data.data.frame()` take `pdate_lwr`, `sdate_lwr`,
  `pdate_upr`, `sdate_upr`, `n` as **strings**; `tbl_now_to_epidist()` passes
  strings. `tidy.epidist_fit()`'s `bookkeeping` list covers exactly the two
  non-parameter columns `predict_delay_parameters()` emits (`draw`, `index`).
* **Strata label ↔ value pairing in `triangle_list`.** The mispairing hazard is
  handled: `strata_values` is built from the same `split()` groups as the names,
  so the two cannot diverge. Confirmed with two stratifying columns whose data
  order is the reverse of alphabetical and counts an order of magnitude apart:
  `alpha | x = 15`, `alpha | y = 600`, `zulu | x = 6`, `zulu | y = 60`, all
  correct and all surviving the round trip through `as_tbl_now()` — including the
  factor class.
* **A factor level with no rows** is dropped from the split rather than becoming
  an empty triangle or an `NA` label, and does not add a group to
  `epinowcast`'s `by`.
* **NA vs 0 in the triangle round trip**, daily and weekly. `NA` cells with
  `report_date <= now` survive as `count = NA` rows; the not-yet-observable
  corner is dropped and regenerated from `now`. On weekly data the delay axis is
  read in weeks (`delays_unit = "weeks"`), not days.
* **`as_reporting_triangle.data.frame()`'s single-stratum requirement** — *"there
  can be no repeated reference dates and report dates"* — matches the pooling
  `format = "matrix"` performs, and the pooled total is preserved
  (`NA` only where every stratum is `NA`).
* **`enw_complete_dates()` / `enw_preprocess_data()` argument names and the
  `timestep`/`max_delay` pairing** match `tbl_now_to_epinowcast()`'s usage; both
  calls are given the same `timestep`, which is what the comment says they must
  be.
* **`tsibble`.** `as_tsibble(index=, key=)` is right; the event date is the
  index, the report date plus strata the key, so index/key is unique. Several
  strata round-trip through `tbl_now_from_tsibble()`.
* **`.tbl_now_collapse_censoring()`** with a genuine **per-case** flag (varying
  within an `(event, report)` cell, not derived from the delay): counts are
  summed and the total is unchanged; a line list keeps one row per case. Already
  covered for four converters; `tbl_now_to_nobbs()` was missing and is now
  covered.
* **`tidy_nowcast_prediction()`'s S7 property names** (`event_dates`,
  `strata_draws`, `strata_levels`, `draws`) match what
  `diseasenowcasting`'s own method reads. Since 2.1.0 the package registers that
  method itself and `.onLoad()` correctly stands down, so `tbl.now`'s copy is
  reachable only on older versions — it was not exercised here.

---

## New tests

| File | Dimensions crossed |
|---|---|
| `test-tidy-strata.R` | strata (none / one / several) × engine (`epinowcast`, `NobBS`, `NobBS.strat`, `baselinenowcast`) × interval provenance (samples / point / absent / caller-supplied); the per-stratum list of fits; the `level` argument. Fits mocked from real shapes — no cmdstan, no JAGS. |
| `test-converter-grids.R` | grid (daily / weekly / numeric) × delays (zero / negative / very long) × completeness (dense / gap / trailing empty period) × `complete` (`"auto"` / `TRUE` / `FALSE`) × data type (linelist / count-incidence); plus the negative-delay warning and epidist's `n >= 1` filtering, including `flusight` |
| `test-converter-strata-shapes.R` | several stratifying columns, a level with no rows, non-alphabetical data order, across `triangle_list` / matrix pooling / `epinowcast` / `tsibble` / `epidist` / `NobBS` / `surveillance` / `data.table` |
| `test-converter-censoring.R` (appended) | per-case censoring flag × `tbl_now_to_nobbs()`, the one converter the existing loop skipped |

Deliberately **not** duplicated: the linelist ↔ count-incidence equivalence,
count-cumulative de-accumulation, the NA-vs-0 matrix round trip, the weekly
`timestep` inference, temporal-effect materialisation, and the per-dataset
regression matrix are all already covered in `test-converter-equivalence.R`,
`test-converter-datasets.R`, `test-converters.R` and `test-tidy.R`.
