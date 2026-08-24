# Plan: integrating \pkg{EpiNow2}

**Status:** proposal. **Checked against:** EpiNow2 **1.9.0**, installed. Every
signature, column name and class below was read from the installed help pages and
source, and the two claims that could be executed were executed (§1.1, §1.2).

> **Revised for 1.9.0.** The first draft of this plan was written against 1.8.0.
> One change in 1.9.0 rewrites the most important part of it — see §0.

There is already a **commented-out** `tbl_now_to_EpiNow2()` in `R/converters.R`.
It is not exported and not in `NAMESPACE`, so nothing depends on it; this plan
supersedes it.

---

## 0. What changed in 1.9.0

Diffed against the exports and signatures recorded during the 1.8.0 pass.

| | 1.8.0 | 1.9.0 |
|---|---|---|
| **`estimate_dist()`** | **absent** — not exported, not in the namespace | **exported.** Fits a delay distribution with double interval censoring and right truncation, via vendored \pkg{primarycensored} Stan functions |
| `estimate_delay()` | the only delay route | still present, but its help now says *"`estimate_dist()` for the recommended replacement"* |
| `estimate_truncation()` | `data, truncation, stan, CrIs, …` | gained `obs = obs_opts()` and `noise = Normal(0, 1)`; **`data` shape unchanged** |
| `regional_epinow()` | had `horizon` | `horizon` gone, folded into `forecast = forecast_opts()`; **`data` shape unchanged** |
| class hierarchy | flat | new base class `<epinowfit>`, carrying `get_parameters()` and `print()` |
| non-daily data | `accumulate` column only | **unchanged** — there is still no `timestep`/`interval` argument anywhere |

The first row is the one that matters. When this plan was written against 1.8.0 it
had to say:

> The `model_overview` article says `estimate_dist()` handles "linelist data…".
> **`estimate_dist` does not exist in EpiNow2 1.8.0.** … So the honest position
> is: no EpiNow2 count entry point accepts a line list.

That is now out of date. The article was ahead of the release, and 1.9.0 shipped
the function.

### 0.1 The consequence: the delay path is nearly free

`estimate_dist()`'s `data` is documented as:

* `pdate_lwr` (required), `pdate_upr` (optional, default `pdate_lwr + 1`)
* `sdate_lwr` (required), `sdate_upr` (optional, default `sdate_lwr + 1`)
* `obs_date` (optional, default `max(sdate_upr)`)
* `n` (optional, observation count/weight, default 1)

**That is the \pkg{epidist} schema, exactly.** `tbl_now_to_epidist()` already
builds all of it. Verified end to end, not by inspection:

```r
x  <- tbl_now(denguedat_slice, event_date = "onset_week",
              report_date = "report_week", data_type = "linelist")
ed <- tbl_now_to_epidist(x, verbose = FALSE)
names(ed)
#> ptime_lwr ptime_upr stime_lwr stime_upr obs_time
#> pdate_lwr pdate_upr sdate_lwr sdate_upr obs_date

EpiNow2::estimate_dist(as.data.frame(ed)[1:60, ],
  stan = stan_opts(chains = 1, samples = 20, warmup = 20))
#> FIT COMPLETED
```

and with counts, `n` and all:

```r
ed <- tbl_now_to_epidist(to_count(x, "count-incidence"), verbose = FALSE)
fit <- EpiNow2::estimate_dist(as.data.frame(ed)[1:40, ], stan = ...)
attr(summary(fit), "n_obs")   #> 274   == sum(ed$n[1:40])
```

So the EpiNow2 delay branch needs **no new reshaping logic** — only that the
frame-building be factored out of `tbl_now_to_epidist()` so
`tbl_now_to_EpiNow2()` can call it without `.need_pkg("epidist")`. Requiring
epidist to be installed in order to feed EpiNow2 would be absurd.

### 0.2 And it inherits the `n >= 1` problem verbatim

`estimate_dist()` rejects a zero weight with the *identical* assertion epidist
uses:

```r
z <- ed; z$n[1:5] <- 0
EpiNow2::estimate_dist(z, stan = ...)
#> Error: Assertion on 'n' failed: Element 1 is not >= 1.
```

This is finding 6 of the converter audit, in a second package. The
`.epidist_drop_unusable_counts()` helper written for that fix applies unchanged —
it should be renamed to something target-neutral (`.drop_unusable_counts()`) and
shared, not duplicated.

---

## 1. What EpiNow2 1.9.0 takes

Five distinct input shapes. They are genuinely different objects, so a single
return type is not available.

| Entry point | `data` shape | Verified from |
|---|---|---|
| `estimate_infections()`, `epinow()` | `data.frame(date, confirm[, accumulate])` — **one** series | `?estimate_infections` |
| `regional_epinow()` | `data.frame(date, confirm, region)` | `?regional_epinow` |
| `estimate_truncation()` | a **list** of `data.frame(date, confirm)` snapshots, each "a complete vector of dates" | `?estimate_truncation` |
| **`estimate_dist()`** | `data.frame(pdate_lwr, pdate_upr, sdate_lwr, sdate_upr[, obs_date][, n])` | `?estimate_dist`, and §0.1 |
| `estimate_delay()` | integer vector of delays — **superseded**, ignores censoring and truncation | `?estimate_delay` |

`estimate_secondary()` takes `data.frame(date, primary, secondary)` — two
*different* data streams (cases and deaths, say). One `tbl_now` is one stream, so
it is **out of scope**; say so in the docs rather than invent a mapping.

`estimate_delay()` should **not** get a target. It is superseded by
`estimate_dist()`, it throws away the censoring information a `tbl_now` carries,
and a user who wants it can write `x$.delay` themselves. Document that, rather
than build a converter for the worse of two routes.

### 1.1 Line list vs counts

Both work, on every target, but by different mechanisms:

* `estimate_infections` / `regional_epinow` / `estimate_truncation` count **dates**,
  so a line list is aggregated with `to_count()` inside the converter — the user
  must not have to remember, exactly as `tbl_now_to_epinowcast()` already does.
* `estimate_dist` counts **cases**: a line list goes one row per case, and count
  data goes through the `n` weight column. Confirmed by fit in §0.1.

---

## 2. Shape of the API

```r
tbl_now_to_EpiNow2(
  x, ...,
  target = c("estimate_infections", "regional_epinow",
             "estimate_truncation", "estimate_dist"),
  snapshots = NULL,     # estimate_truncation only: how many, latest first
  accumulate = "auto",  # non-daily -> daily grid + `accumulate` column
  verbose = TRUE, quiet = FALSE
)
```

`target` is named for **the EpiNow2 function the result is passed to**, so the
value documents itself and the return type is predictable from the call.

**On "maybe just return a data.frame and let `tidy()` be nice".** For three of the
four targets that is exactly what happens — a plain `data.frame` with the columns
EpiNow2 names. It cannot be all four: `estimate_truncation()` documents its `data`
as *a list of data.frames*. Returning a data frame there would mean the user
reshapes it themselves, which is the work the converter exists to do. The rule the
package already follows (`tbl_now_to_nobbs()` returns the line list `NobBS()`
takes) is the right one: **return what the named function accepts**.

| `target` | returns |
|---|---|
| `"estimate_infections"` | `data.frame(date, confirm[, accumulate])` |
| `"regional_epinow"` | `data.frame(date, confirm, region[, accumulate])` |
| `"estimate_truncation"` | `tbl_now_epinow2_snapshots` — a **list** of `data.frame(date, confirm)`, plus the metadata needed to invert it |
| `"estimate_dist"` | `data.frame(pdate_lwr, pdate_upr, sdate_lwr, sdate_upr, obs_date[, n])` |

### 2.1 `tbl_now_from_EpiNow2()` — and why it can exist

The commented-out code claims there is "intentionally **no**
`tbl_now_from_EpiNow2()`: neither a single time series nor a set of snapshots can,
in general, reconstruct the full event/report structure". **Half of that is
wrong.** A single series cannot. A set of snapshots *is* the reporting triangle in
another basis: snapshot `k` is the series as known at report date `k`, so

```
increment(event = d, report = r_k) = confirm_k[d] - confirm_{k-1}[d]
```

recovers `count-incidence` exactly, provided the snapshot report dates are known.
They are not carried by a bare list of `date`/`confirm` frames — which is why the
outbound object should be a **thin classed list** carrying them, the same pattern
as `tbl_now_triangle_list`:

```r
structure(snapshots,
  class = "tbl_now_epinow2_snapshots",
  report_dates = <Date vector, one per element>,
  now = get_now(x), event_col = ..., report_col = ..., strata_cols = ...)
```

A classed list is still a list, so `estimate_truncation()` accepts it unchanged.
`as_tbl_now.tbl_now_epinow2_snapshots()` differences it back. `print()` should say
what it is, for the same reason `tbl_now_triangle_list`'s does.

For a plain list with no attribute, `tbl_now_from_EpiNow2(x, report_dates = ...)`
takes them as an argument, so `EpiNow2::example_truncated` converts too. That is
the fixture the round-trip test should use (§5).

---

## 3. The five things most likely to be wrong

Ranked by how quietly they fail. Each is a lesson from the converter audit.

### 3.1 EpiNow2 models a **daily** process — still the big one

**Unchanged in 1.9.0**: there is no `timestep`, `interval`, `freq` or `period`
argument on `estimate_infections()`, `epinow()`, `regional_epinow()` or
`estimate_truncation()` (checked all four formals). Handing a weekly series over
as one row per week is read as one row per **day**, and the fit is silently wrong
on the time axis — the same class of error as audit finding 4.

EpiNow2's answer is the `accumulate` column: expand onto the **daily** grid and
mark the days whose observation should be added to the next real one.
`fill_missing(data, missing_dates = "accumulate", initial_accumulate = …)` builds
it. So:

* `event_units == "days"` → pass through, no `accumulate` column;
* `event_units == "weeks"` → daily grid, `accumulate = TRUE` on the six filler
  days and `FALSE` on the day carrying the week's count;
* `months` / `years` / `numeric` → **abort**, naming the units, as
  `.epinowcast_timestep()`, `.nobbs_units()` and `.surveillance_aggregate_by()`
  already do. `numeric` has no calendar to expand onto at all.

`accumulate = "auto"` derives this from `get_event_units()`; `TRUE`/`FALSE` force
it. The old commented-out code emitted weekly rows verbatim.

**Build it with [EpiNow2::fill_missing()], do not lay it out by hand.** This was
checked, and the hand-rolled version was wrong: it put each period's count on the
period's *last* day, where `fill_missing()` leaves it on the date the caller gave
and back-fills the days *before* it. Same `accumulate` pattern, dates shifted by
six — every weekly fit misaligned, no error. It also wrote `0` on the filler days
where `fill_missing()` writes `NA`, which is the difference between "observed
zero" and "no observation".

**But pass `initial_accumulate` explicitly.** With `by`, EpiNow2 1.9.0's
inference **drops each group's first observation**: a two-region weekly series of
336/167 cases came back as 295/147, with the grid starting the day *after* the
first report. Single-series inference is fine; it is the `by` path. Passing the
step (7 for weekly) keeps every case, silences the "Detected fixed accumulation
frequency" warning, and is robust to a gappy series where inference could read
the period wrong.

`estimate_dist` is exempt: it works in dates and censoring windows, not on a grid,
so the unit maps to the window width exactly as `.epidist_window_days()` already
does.

### 3.2 Which snapshot the series is

`estimate_infections()` wants the series **as known at `now`**. That is
`get_latest_reported_cases(x)` — an exported getter that already handles all three
data types and the `now` edge. The old code re-implemented it inline with
`slice_max()` over report dates, which DEVELOPMENT_SKILL §4 forbids. Use the
getter; use `get_nth_reported_cases()` for the snapshot series.

Cumulative input de-accumulates first, which can yield negatives. `obs_opts()` has
no way to represent a negative count, so the converter must decide and say so.
Proposal: warn and clamp at 0, matching `enw_preprocess_data(set_negatives_to_zero
= TRUE)`, and document it.

### 3.3 `estimate_truncation()` needs complete, capped, ordered snapshots

Three requirements, only one obvious:

* **Complete** — `?estimate_truncation`: "All data sets must contain a complete
  vector of dates." Run `complete_zeroes()` per snapshot. A line list cannot
  express a zero at all, so this is not optional there.
* **Capped** — one snapshot per distinct report date is what the old code did. On
  a multi-year daily series that is ~1000 snapshots and the fit will not finish.
  `snapshots = NULL` should default to something small (EpiNow2's own
  `example_truncated` ships **five**), taken from the latest report dates, with the
  count printed under `verbose`.
* **Ordered** — `get_predictions.estimate_truncation()` reorders by `nrow` and
  trims to the latest common start date. Emit shortest-to-longest so the user's
  indices match the fit's.

### 3.4 Strata → one `region` column

`regional_epinow()` takes a single `region` column. A `tbl_now` may have several
stratifying columns, so paste them `" | "`-joined — the convention now used by
`triangle_list` names and `.epinowcast_stratum()`. Keep the *values* on the object,
not just the pasted label, so the inverse need not parse the separator back out.

For `estimate_infections` and `estimate_truncation`, strata must be **pooled** with
a warning, as `format = "matrix"` does. For `estimate_dist` they ride along as
ordinary columns (EpiNow2 ignores extra columns), which is what
`tbl_now_to_epidist()` already does with them.

### 3.5 `tidy()` must read the interval width, not assume it

Every EpiNow2 summary goes through `calc_summary_measures()`, which emits
`median`, `mean`, `sd` and one `lower_<pct>` / `upper_<pct>` pair per requested
credible interval. Default `CrIs = c(0.2, 0.5, 0.9)` gives `lower_90`/`upper_90`
as the widest — but **`CrIs` is a user argument**, so a fit made with
`CrIs = c(0.5, 0.95)` has `lower_95`/`upper_95` and no `lower_90` at all.
Confirmed on a real `estimate_dist()` fit:

```
variable  median  mean  sd  lower_90 lower_50 lower_20 upper_20 upper_50 upper_90
meanlog    2.246  ...
sdlog      0.701  ...
```

`tidy()` must pick the widest `lower_*`/`upper_*` pair **present** and derive
`level` from that column name. Hard-coding `0.90` would reproduce audit finding 3
in a new place.

**A trap worth recording.** In that table `mean` and `sd` are the posterior mean
and sd **of the parameter**. `tidy.epidist_fit()` reports the fitted
distribution's mean and sd as *rows* (`term = "mean"`, `term = "sd"`), because
`epidist::add_mean_sd()` derives them. They are different quantities with the same
names. `tidy.estimate_dist()` must not present one as the other; the distribution
itself comes from `get_parameters(fit)$delay`, a `<dist_spec>`.

### 3.6 `tidy()` methods needed

| class | shape | source |
|---|---|---|
| `estimate_infections`, `epinow` | nowcast schema | `get_predictions(format = "summary")` |
| `estimate_truncation` | nowcast schema | `get_predictions(format = "summary")` |
| `estimate_dist` | **delay** schema (`term`, `estimate`, …) | `summary(fit)` |
| `regional_epinow` return | nowcast schema, one block per region | a branch in `tidy.list()` |

`estimate_dist` is the second instance of DEVELOPMENT_SKILL §7's documented
exception — "a package that returns **only a delay distribution**" — alongside
`tidy.epidist_fit()`. It must return the delay-shaped table, not be forced into
the nowcast schema. `probs` is honourable for the count targets via
`get_predictions(format = "sample")`.

`regional_epinow()` returns a plain nested list, so it needs a third detector in
`tidy.list()` alongside NobBS and the per-stratum `baselinenowcast` list. Key it on
the nested `$regional` element, not on merely being a list.

---

## 4. Work items

Following DEVELOPMENT_SKILL §7 and the §10 checklist.

1. `.epinow2_grid()` — units → daily grid + `accumulate`; aborts on
   `months`/`years`/`numeric`. Mirrors `.epinowcast_timestep()`.
2. `.epinow2_region()` — strata → one `region` label, values kept alongside.
3. **Factor `.epidist_windows()` out of `tbl_now_to_epidist()`** — the builder for
   the four date columns plus `obs_date`/`n` — so `target = "estimate_dist"` uses
   it without `.need_pkg("epidist")`. Rename
   `.epidist_drop_unusable_counts()` → `.drop_unusable_counts()` and share it
   (§0.2).
4. `tbl_now_to_EpiNow2()` — the four targets; `.assert_tbl_now()` then
   `.tbl_now_collapse_censoring()` for the three date-keyed targets **but not for
   `estimate_dist`**, which is the one job that can use a censoring flag (the same
   carve-out `tbl_now_to_epidist()` has); `.need_pkg("EpiNow2")`,
   `.warn_lossy_conversion()`, line-list aggregation with a warning.
5. `tbl_now_epinow2_snapshots` — thin class + `print()` saying what it is and what
   it is not.
6. `as_tbl_now.tbl_now_epinow2_snapshots()` + `tbl_now_from_EpiNow2()` with a
   `report_dates` argument for a bare list.
7. `tidy()` methods per §3.6. Width read from the column names; `probs` from
   `format = "sample"`.
8. Docs: one Rd topic per direction, a **Non-daily data** section, an
   `estimate_secondary()` exclusion note, and an `estimate_delay()`-is-superseded
   note.
9. `NEWS.md`, `SKILL.md` capability table, a
   `vignettes/articles/nowcasting-models.Rmd` section showing **both** the plain and
   the stratified fit, a row in `data-raw/nowcast_comparison.R`, and a row in
   `data-raw/converter_matrix.R`.

---

## 5. How we would know it is correct

Split into what the test suite can assert and what must be checked by hand once.
EpiNow2 needs **cmdstan**, so no fit belongs in the suite:
`skip_if_not_installed("EpiNow2")` for shape tests, mocked fits for `tidy()`.

### Conservation — the load-bearing checks

1. **Cases are not created or destroyed.** For `estimate_infections`,
   `sum(out$confirm)` equals the case total of `get_latest_reported_cases(x)`. For
   `regional_epinow`, that holds **per region**, with counts an order of magnitude
   apart between regions so a mispaired label is arithmetically unmistakable — the
   pattern that caught audit finding 1.
2. **Every snapshot equals the series as of its own report date.** For snapshot
   `k`, `sum(confirm)` equals the total from `get_nth_reported_cases()` at report
   date `k`, and snapshot totals are non-decreasing.
3. **The daily expansion preserves the weekly totals.** On a weekly `tbl_now`,
   `sum(confirm)` is unchanged, and summing each accumulate-run reproduces that
   week's original count exactly.
4. **`estimate_dist` weights survive.** `sum(out$n)` equals the case total. On a
   real fit this is checkable against EpiNow2 itself:
   `attr(summary(fit), "n_obs") == sum(out$n)` — confirmed in §0.1.

### Grid and units

5. `event_units == "days"` → no `accumulate` column, one row per day.
6. `event_units == "weeks"` → dense daily `date` with no gaps, `accumulate` `TRUE`
   on exactly six days in seven, **and the grid identical to what
   `EpiNow2::fill_missing()` produces on the same frame** — the check that caught
   the six-day shift.
6b. **Every group keeps its first period.** Only reproduces on real, slightly
   irregular data; a regular synthetic series does not show it.
7. `months` / `years` / `numeric` → **abort naming the units**, mentioning
   `accumulate` so the user knows what to pass. Same shape as
   `test-converter-grids.R`'s "the date-based back-ends refuse a numeric grid by
   name".
8. `estimate_dist` is exempt from all of the above and must **not** abort on
   weekly data — its window is 7 days wide instead.

### Equivalence and round trip

9. **Line list == its own aggregate.** `tbl_now_to_EpiNow2(linelist)` identical to
   `tbl_now_to_EpiNow2(to_count(linelist, "count-incidence"))` for the three
   date-keyed targets. For `estimate_dist` they are *not* identical by
   construction — one row per case vs one row per cell with `n` — so assert the
   weaker true thing: same `sum(n)`, same set of `(pdate_lwr, sdate_lwr)` pairs.
10. **Snapshot round trip.** `tbl_now → snapshots → as_tbl_now()` reproduces the
    `count-incidence` triangle for the cells the snapshots cover. Lossy above the
    `snapshots` cap and below the earliest snapshot's start — assert *what
    survives*, as the baselinenowcast round-trip tests do.
11. **Against EpiNow2's own fixture.** `EpiNow2::example_truncated` is five
    snapshots the package ships and vouches for.
    `tbl_now_from_EpiNow2(example_truncated, report_dates = …)` then back out must
    reproduce it cell for cell. The strongest available check, because neither end
    is ours.
12. **`tbl_now_to_epidist()` and `tbl_now_to_EpiNow2(target = "estimate_dist")`
    agree** on the four date columns and `n` for the same object. They share a
    builder, so this is a regression guard on the factoring in work item 3.
13. **Negative delays are *not* a problem here**, unlike the triangle back-ends
    (audit finding 5). A report before its event still sits at its own event date
    in a date-indexed series, so no case is lost — assert the total survives. For
    `estimate_dist` it *is* a problem, and EpiNow2 will reject it, as epidist does.

### `tidy()`

14. **The width comes from the fit, not from a constant.** Mock a summary with
    `CrIs = c(0.5, 0.95)` — columns `lower_95`/`upper_95`, no `lower_90` — and
    assert `level == 0.95`. Then mock the default and assert `0.90`.
15. **`tidy.estimate_dist()` returns the delay schema**, not the nowcast one:
    `term`/`estimate`/`conf.low`/`conf.high`/`level`/`engine`, one row per
    parameter, `term` taken from `variable`. And it must **not** report the
    summary's `mean`/`sd` columns as if they were the distribution's mean and sd
    (§3.5).
16. **Region labels stay with their own estimates.** Mock a `regional_epinow()`
    return with two regions an order of magnitude apart; assert
    `(stratum, event_date)` is unique and each estimate lands under its own region.
17. **The standard contract.** `expect_tidy_contract(out, "EpiNow2")` from
    `test-tidy.R` for the count targets.

### Checked once, by hand, not in the suite

18. One real `estimate_infections()` fit on `denguedat` and one `regional_epinow()`
    fit on a two-stratum cut, precomputed into
    `vignettes/articles/nowcast-comparison.rds`. Confirm the estimates are on the
    **daily** axis and that the weekly `accumulate` path gives the same answer as
    feeding EpiNow2 a hand-built daily frame. This is the check the mocked tests
    cannot make, and §3.1 is why it matters most.
19. One `estimate_dist()` fit, cross-checked against `tbl_now_to_epidist()` →
    `epidist::epidist()` on the same object. Two packages, two Stan models, one
    `tbl_now`, and a delay distribution that should agree within uncertainty. A
    large disagreement means the censoring windows are wrong. This is a stronger
    check than anything internal, and it is available only because both converters
    now exist.
20. A `converter_matrix` re-run: EpiNow2 gets a row per shipped dataset, so
    `flusight` (weekly, count-cumulative) and `covid_us` (daily) both exercise the
    `accumulate` branch and the de-accumulation branch.
