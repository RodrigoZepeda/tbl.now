# tbl.now 0.18.0

## New: one call per model, and ensembles

Until now `tbl.now` prepared data for six nowcasting packages and normalised what
they returned, but running several of them still meant six different calls and
six different result shapes to reconcile by hand. This release adds the layer
that removes that bookkeeping.

* **`run_nowcast(x, method)`** fits any supported package and always returns a
  **`tbl_nowcast`**: an S7 object holding the predictions as one row per (event
  date, stratum, quantile level), plus the draws where the backend has them, plus
  the backend's own untouched fit. Backends ship for `"diseasenowcasting"`,
  `"baselinenowcast"`, `"epinowcast"`, `"NobBS"`, `"surveillance"` and
  `"EpiNow2"`, each feeding its package through the matching `tbl_now_to_*()`
  converter rather than building the input by hand.

  It is called `run_nowcast()` and not `nowcast()` because \pkg{diseasenowcasting}
  exports `nowcast()`; keeping the names distinct means both can be attached at
  once.

* **`nowcast_ensemble()`** combines several of them, either by averaging their
  quantiles level by level (`type = "quantile"`, vincentization -- narrower) or
  by pooling their draws into a mixture (`type = "linear_pool"` -- wider, and
  refused outright when a member has no draws, rather than silently dropping it).

* **`nowcast_backtest()`**, **`score_nowcast()`** and **`nowcast_weights()`**
  score models retrospectively and turn those scores into ensemble weights
  (`"inverse_score"`, `"optim"` or `"equal"`). `as_scoringutils()` hands the same
  object to \pkg{scoringutils} for its full score suite.

* **`nowcast_fit()` / `nowcast_tidy()`** are the extension point: two S3 methods,
  in any package, and `run_nowcast()` knows about your model. See
  `vignette("ensemble-nowcasting")`.

* **`autoplot()`** for a `tbl_nowcast` draws a fan chart, in the palette's green
  -- a nowcast estimates the epidemic process, not the reporting one.

## New: `tidy()` for a nowcast and for a backtest

`tidy()` already worked on every raw engine fit. It now also works on what
`run_nowcast()` and `nowcast_ensemble()` return, which is the way round it should
always have been.

* **`tidy()` on a `tbl_nowcast`** returns the package's standard frame --
  `event_date`, `stratum`, `estimate`, `conf.low`, `conf.high`, `level`,
  `engine`, plus `q*` columns for `probs`. `engine` is the method (or the
  ensemble's name); `level` is the width of the **widest symmetric pair of
  quantile levels the object actually carries**, and is `NA`, with `NA` bounds,
  when no symmetric pair exists. A guessed 0.95 there would defeat the one column
  that exists to stop a 90% band being compared with a 95% one.

  `probs` is honoured only when the nowcast carries draws, and errors otherwise:
  a quantile-only nowcast cannot produce a level it was not summarised at.

  Registered in `.onLoad()`, because `tbl_nowcast` is S7 and
  `tidy.tbl.now::tbl_nowcast` is not a writable S3 method name.

* **`tidy()` on a `nowcast_backtest`** gives one row per (method, `now` date,
  target), with the internal dot-prefixed columns traded for ordinary ones.

## New: reproducible backtests

`nowcast_backtest()` gains a **`seed`** argument. When given, the RNG is seeded
immediately before each fit, from the seed and the method and date that fit is
for. One `set.seed()` before the whole backtest only pins anything if every
method draws the same random numbers in the same order -- which stops being true
the moment a method is dropped or one date is refitted. This is the same lesson
`data-raw/nowcast_comparison.R` already records.

`nowcast_weights(type = "optim")` now falls back to equal weights, with a
warning, when the optimiser does not converge on a usable point. It used to
return `NA` weights, which do not fail until much later inside
`nowcast_ensemble()`, as an all-`NA` nowcast that reads like a modelling problem
rather than an optimisation one.

## Removed: the `nowcaster` backend

`nowcaster` was dropped in 0.16.0 along with its converters, for the reasons
recorded there. The `run_nowcast()` backend for it is not shipped: it called
`tbl_now_to_nowcaster()` and `get_nowcaster_strata()`, which no longer exist.
Neither `nowcaster` nor `INLA` is reintroduced to `DESCRIPTION`.

## Other

* `scoringutils` added to `Suggests` (CRAN, so no `Additional_repositories`
  entry is needed). The hand-rolled `.wis()` is now cross-checked against it in
  the test suite: two implementations agreeing is worth more than either alone.
* New article, `vignette("ensemble-nowcasting")`, with the fits precomputed by
  `data-raw/ensemble_comparison.R` so the build never fits anything. It reports
  WIS per model and per ensemble across three epidemics, and answers "does the
  ensemble beat its best member?" and "does performance weighting beat equal
  weighting?" from the cached numbers rather than by assertion.
* `vignette("nowcasting-models")` gains a `run_nowcast()` column in its package
  table, and a pointer to the new article.

# tbl.now 0.17.0

## New: \pkg{EpiNow2} support

`tbl_now_to_EpiNow2()` and `tbl_now_from_EpiNow2()`, against **EpiNow2 1.9.0**
(now the minimum in `Suggests`). EpiNow2 takes four different input shapes, one
per entry point, so `target` names the function the result is passed to and it can
be handed over unchanged:

* **`"estimate_infections"`** (default) -- `data.frame(date, confirm)`, the series
  as known at `get_now()`. Also what `epinow()` takes.
* **`"regional_epinow"`** -- the same plus a `region` column built from the
  object's strata (`" | "`-joined for several, matching the `triangle_list`
  convention). The other targets pool strata with a warning.
* **`"estimate_truncation"`** -- a `tbl_now_epinow2_snapshots` list, one
  `date`/`confirm` snapshot per report date. This is the one EpiNow2 model that
  uses the report dimension a `tbl_now` exists to carry.
* **`"estimate_dist"`** -- the interval-censored frame `EpiNow2::estimate_dist()`
  fits a delay distribution to. **New in EpiNow2 1.9.0**, and it documents the
  \pkg{epidist} schema exactly, so it shares `.delay_censoring_windows()` with
  `tbl_now_to_epidist()` rather than growing a second copy.

Three things worth knowing:

* **EpiNow2 models a daily process and has no `timestep`.** As of 1.9.0 there is
  no `timestep`, `interval` or `period` argument on any entry point (all four
  formals checked), so a weekly series passed as one row per week is read as one
  row per **day** -- no error, just an epidemic seven times too fast. The
  converter lays it on the daily grid with EpiNow2's own `accumulate` column
  instead -- built by [EpiNow2::fill_missing()] rather than by hand, because a
  hand-rolled version put each period's count on the period's *last* day where
  `fill_missing()` leaves it on the date given, shifting every weekly fit six days
  with no error. Units coarser than a week, and the `"numeric"` grid, are refused
  by name rather than approximated.

  `initial_accumulate` is passed explicitly rather than inferred: with `by`,
  EpiNow2 1.9.0's inference drops each group's first observation (a two-region
  weekly series of 336/167 cases came back as 295/147). Single-series inference is
  unaffected.
* **The snapshot form has a real inverse.** Snapshot *k* is the series as known at
  report date *k*, so differencing consecutive snapshots recovers
  `count-incidence` exactly. `tbl_now_epinow2_snapshots` carries the report dates
  so `as_tbl_now()` can do it; a bare list needs `report_dates`. Verified against
  `EpiNow2::example_truncated`, which round-trips to the case for the case.
  (The commented-out draft of this converter asserted no inverse was possible.
  For a single series that is true; for snapshots it is not.)
* **`estimate_secondary()` and `estimate_delay()` get no target.** The first models
  two data streams against each other and one `tbl_now` is one stream; the second
  is superseded by `estimate_dist()` by EpiNow2's own help and throws away the
  censoring a `tbl_now` carries.

**`obs_date` and the censoring windows are different quantities**, and the
converter now treats them as such. `[sdate_lwr, sdate_upr)` brackets *when the
report happened* -- at weekly resolution `[W, W + 7)`, a half-open interval whose
upper bound is the end of that week, not a claim that anything happened on day
`W + 7`. `obs_date` is *when observation stopped*, which `estimate_dist()` asserts
is `>= sdate_upr` on every row. A `tbl_now`'s `now` **labels a period**, so the
instant observation stopped is the end of it: `obs_date = now + w`. That makes the
assertion hold by construction, and nothing is observed after it. Clamping the
windows at `now` instead was tried and rejected -- it moves reports in the final
period into an earlier one, which the epidist round-trip test caught.

The `nowcasting-models` article now covers \pkg{EpiNow2} across all three strata,
with its results precomputed into `nowcast-comparison.rds` like every other
engine. Two caveats are stated in the article itself: the delay distributions are
\pkg{EpiNow2}'s shipped examples rather than distributions fitted to the
Colombian data, and sampling is lighter than the default (500 draws, 250 warmup,
2 chains) because it is much the slowest engine in the comparison.

`data-raw/nowcast_comparison.R` now takes engine names
(`Rscript data-raw/nowcast_comparison.R EpiNow2`) and merges them into the
existing file, leaving every other engine's rows and recorded timings alone; with
no arguments it rebuilds everything as before. This replaces a second script that
re-created the setup by parsing the first one.

Two correctness fixes came out of that. Every engine is now seeded per
`(engine, stratum)` immediately before its fit, rather than relying on a single
`set.seed()` at the top of the script -- which only pins results if every engine
consumes the same random numbers in the same order, and so does not survive
refitting a subset. Refitting `baselinenowcast` alone had been silently changing
its estimates, and one EpiNow2 fit produced a stratum whose upper credible bound
sat at `1e8` for all 181 days and would not reproduce. Both now refit to
`max abs diff == 0`. The script also refuses to cache any fit whose scale exceeds
100x the observed maximum for its stratum, since an unconverged Stan or INLA fit
returns numbers rather than an error.

`tidy()` gained methods for `estimate_infections`, `epinow`, `estimate_truncation`
and `estimate_dist`, plus a `regional_epinow` branch in `tidy.list()` giving one
block per region.

`tidy.estimate_dist()` reports the fitted distribution's **`mean` and `sd`**
alongside its parameters, so its output is directly comparable with
`tidy.epidist_fit()`. They are derived from the **distribution**, not from the
family's algebra: each draw's parameters go back into the fit's own `dist_spec`
and through [EpiNow2::discretise()], which knows the families, and the moments
follow by summation over the PMF. Nothing in this package names a distribution, so
a family \pkg{EpiNow2} adds later works as soon as `discretise()` supports it.
Against the closed forms the mean is exact and the sd runs about 1% high -- the
variance a discrete grid adds -- so expect a difference of that order against
\pkg{epidist}, which reports continuous-distribution moments.

It also honours `probs` and takes a `level` argument, matching
`tidy.epidist_fit()`. (An earlier draft rejected `probs` with a message claiming
the engine keeps no draws. It does: `summary.estimate_dist()` reads them.)

`tbl_now_to_EpiNow2(target = "estimate_dist")` warns when it pools strata --
`estimate_dist()` has no grouping argument, so it fits one distribution to
everything -- and warns when a large share of delays are exactly zero, since a
lognormal has zero density there and will inflate its variance rather than fail.
The message points at the families that do have positive density at zero
(`"exp"`, or `"gamma"`/`"weibull"` with shape below 1) rather than at a constant
shift, which would silently bias every parameter.

Two more points of care:

* `level` is read off the `lower_<pct>`/`upper_<pct>` column names, because
  EpiNow2's `CrIs` is a user argument -- a fit made with `CrIs = c(0.5, 0.95)` has
  no `lower_90` at all, and hard-coding `0.90` would report a width the fit never
  produced.
* `tidy.estimate_dist()` returns the **delay** schema (`term`, `estimate`, ...),
  not the nowcast one -- the second instance of the documented exception alongside
  `tidy.epidist_fit()`. Note that `summary()`'s `mean`/`sd` *columns* are the
  posterior mean and sd of each **parameter**, while the `mean`/`sd` *rows* this
  method reports are the **delay distribution's** moments. Same words, different
  quantities.

`.epidist_drop_unusable_counts()` is now `.drop_unusable_counts()` and shared:
`EpiNow2::estimate_dist()` asserts `n >= 1` with the identical message epidist
uses, so the same filter applies to both.


## Audit of the converters and `tidy()` against the target packages' own docs

Every claim the converters and `tidy()` methods make about `diseasenowcasting`,
`baselinenowcast`, `epinowcast`, `epidist`, `NobBS`, `surveillance`, `tsibble`
and `data.table` was re-checked against those packages' installed help pages and
source. Five defects came out of it, all of them cases where the code was
silently *plausible* rather than wrong-looking.

* **`tidy()` no longer pools strata under `"all"`.** `tidy.nowcast()` documents
  `stratum` as `"all"` *when the fit is unstratified*, so `(stratum, event_date)`
  is meant to be a unique key. Two methods broke that:

  * **`tidy.epinowcast()`** read `summary(fit, type = "nowcast")` and ignored
    both `.group` and the `by` columns sitting beside it. A real two-group fit
    (`by = "age_group"` on `germany_covid19_hosp`, age groups `00+` and `80+`)
    came back as 20 rows all labelled `"all"`, with every one of its 10 reference
    dates duplicated. It now emits one block per `by` group, and several
    grouping columns are pasted `" | "`-separated, matching
    `tbl_now_to_baselinenowcast(format = "triangle_list")`.
  * **`tidy.list()`** recognised a `NobBS::NobBS.strat()` fit -- it has the
    `estimates`/`onset_date` shape the detector looks for -- but ignored the
    `stratum` column that `NobBS.strat()` puts there. A two-stratum fit on
    `denguedat` returned 44 rows labelled `"all"`, 22 of them duplicate keys. It
    now reads `stratum` when present.

  The `probs` path for `epinowcast` was mispaired in the same way: it split the
  posterior samples on `reference_date` alone, so on a stratified fit each date's
  quantiles went to whichever stratum `split()` sorted first. The split is now
  keyed on `(stratum, reference_date)` and indexed by the summary's own rows.

* **`tidy()` no longer invents an interval for a `baselinenowcast` point fit.**
  `baselinenowcast(output_type = "point")` returns one value per reference date
  and stamps `output_type = "point"` on the result. `tidy()` ignored that column
  and took the 2.5%/97.5% quantiles of a single number, reporting
  `conf.low == conf.high == estimate` with `level = 0.95` -- a zero-width 95%
  band. It now returns `NA` bounds and `NA` `level`, and refuses `probs` rather
  than returning the point estimate under a quantile's name.

* **`tbl_now_to_nobbs()` prints the `units` string NobBS accepts.** Its verbose
  summary printed the object's own `"weeks"`, but `NobBS::NobBS()` documents
  `units` as `"1 day"` or `"1 week"`; pasting `"weeks"` into the call produces
  `-Inf`/`Inf` warnings from `seq()` and then an opaque `replacement has 1 row,
  data has 0`. It now prints `"1 week"`, and aborts up front for any grid NobBS
  cannot model.

* **The line-list back-ends no longer fabricate 1970 dates from a `numeric`
  grid.** `tbl_now_to_nobbs()` and `tbl_now_to_surveillance()` both coerced the
  event and report columns with `as.Date()`. On a `numeric`-unit `tbl_now` those
  columns are integer indices, so index 1 became 1970-01-02 and the conversion
  succeeded, silently, with a line list of invented dates. Both now abort naming
  the units, as `tbl_now_to_baselinenowcast()` and `tbl_now_to_epinowcast()`
  already did. `tbl_now_to_surveillance()` also gained `"years"` ->
  `"1 year"`; it previously fell through to `"1 week"`.

The remaining findings were addressed too:

* **A negative delay now warns instead of silently losing cases.** A reporting
  triangle is indexed by delay from 0, so a report that arrived *before* its
  event has no cell: 10 cases in gave a triangle summing to 9, with the affected
  cell reading `0` -- an *observed* zero -- rather than `NA`. Both triangle
  formats and `tbl_now_to_epinowcast()` now warn, naming how many rows and cases
  go and how to filter them yourself. `format = "long"` has no delay axis, keeps
  them, and stays quiet.

* **`tbl_now_to_epidist()` accepts `count-cumulative` data.** epidist asserts
  `n >= 1`, and de-accumulating a cumulative series produces a `0` wherever a
  report added nothing and a negative on any downward revision -- so the
  conversion died on epidist's own `Assertion on 'data$n' failed` for
  essentially any real cumulative input, and for plain incidence data that had
  been through `complete_zeroes()`. Rows carrying no case are now dropped before
  the epidist object is built: a zero contributes nothing to a delay
  distribution, so that is lossless and only reported under `verbose = TRUE`; a
  negative discards a revision, so it **warns**; and if nothing usable is left
  the converter aborts saying why. `flusight` -- the one `error` cell in the
  article's converter matrix -- now converts.

* **`tidy()` handles a per-stratum list of `baselinenowcast` fits.**
  `?tbl_now_triangle_list` recommends
  `lapply(triangles, baselinenowcast::baselinenowcast)`, and `tidy()` on the
  result used to error and suggest `engine = "NobBS"`. A list whose elements are
  all `baselinenowcast_df` is now recognised: each is tidied and labelled with
  its list name (or its position, when the list is unnamed), giving the same
  one-block-per-stratum table the natively stratified engines return. `probs`
  passes through.

* **`DEVELOPMENT_SKILL.md` section 2 corrected.** It claimed
  `tbl_now_to_surveillance()` sets `control$dRange`. It does not, and its own
  help page says so: `now` and the delay unit are deliberately left to the
  caller, because the converter cannot know which window you mean to fit.

## Behaviour changes

* **`tidy()` reports `level = NA` for a \pkg{NobBS} fit** instead of `0.95`.
  `NobBS()`'s `lower`/`upper` come from `specs$conf`, and its return value is
  `list(estimates, estimates.inflated, nowcast.post.samps, params.post)` -- no
  `specs`, so the width is genuinely unrecoverable from the fit. A guessed
  default is worse than `NA` in the one column that exists to stop widths being
  compared blindly. Pass `tidy(fit, level = 0.95)` to fill it in. The assertion
  in `test-tidy.R` that recorded the old behaviour was updated.

* **`tidy.epidist_fit()` warns on a delay model with covariates.**
  `epidist::predict_delay_parameters()` returns one row per draw *and*
  observation, and the reported quantiles pool over both. For `mu ~ 1` every
  observation shares the draw's value, so that is exactly the posterior
  interval; with covariates in the delay model the interval is a *mixture across
  covariate levels*, which the docs described simply as "Posterior median". The
  method now detects a parameter that varies within a single draw and says so,
  pointing at `newdata` for a specific covariate combination. The numbers are
  unchanged -- only the silence is.

## Tests

* New `test-tidy-strata.R`: stratified `tidy()` for `epinowcast` and
  `NobBS.strat()`, quantile-to-stratum alignment, the point-fit interval, the
  per-stratum list of `baselinenowcast_df` fits, and the `level` argument. The
  fits are mocked from the shape of real ones, so the file needs neither cmdstan
  nor JAGS.
* New `test-converter-grids.R`: the `numeric` grid across every converter,
  zero / negative / very long delays, gaps in the event grid, a trailing event
  period with no reports under each `complete` setting, the negative-delay
  warning, and epidist's `n >= 1` filtering (including `flusight`).
* New `test-converter-strata-shapes.R`: several stratifying columns, a factor
  level with no rows, and label-to-value pairing when the data order is not
  alphabetical.
* `test-converter-censoring.R` now covers `tbl_now_to_nobbs()`, which the
  converter loop skipped because its package name is not its suffix.


* **Removed \pkg{nowcaster} support**: `tbl_now_to_nowcaster()`,
  `get_nowcaster_strata()`, the `tidy()` branch for its fits, and its sections in
  the articles are all gone. The converter worked, but the package around it
  demanded enough special-casing that keeping it cost more than it returned:

  * **`Dmax` and `wdw` are counted in weeks, whatever grid you hand it.** On a
    daily series, values chosen as days are silently read as weeks: `Dmax = 30`,
    `wdw = 120` asked for a 30-week horizon over a 2.3-year window, which ran for
    **45 minutes** and had INLA reporting the fit diverging. The same fit with
    week-scaled values took **24 seconds**.
  * **It returns weekly estimates from daily data**, so its numbers are weekly
    *totals* while every other engine reports daily counts -- roughly 6x larger
    on the same axis, and not comparable without re-gridding. Its label is the
    week *start*.
  * **`age_col` must be numeric** even though the help calls it a stratum
    column: a character column errors inside `cut()`, and a character `bins_age`
    trips an `if (bins_age == "SI-PNI")` comparison against a vector. The
    converter existed largely to encode strata into codes and hand back the
    matching breaks.
  * **Results come back as those codes, not labels**, so a tidied stratified fit
    reported `stratum` values of `"1"` and `"2"` rather than the levels.
  * **It takes its maximum observable time from the last event date, not the
    last report**, so cutting the series anywhere except where
    `max(onset) == max(report)` made it NA-mask genuinely observed cells and
    nowcast *below* what had already been reported.
  * It needs **R-INLA**, and was itself installable only from GitHub.

  That last point has a side benefit: \pkg{nowcaster} was the only entry in
  `Remotes:`, so removing it drops that field entirely -- and with it the reason
  the package could not be submitted to CRAN as-is.
* **New `tbl_now_to_nobbs()`**, filling a real gap. \pkg{NobBS} counts *rows*,
  so handing it `count-incidence` data was silently wrong: a table of 1,174 rows
  carrying 50,160 cases was nowcast as 1,174 cases, with no error. The converter
  expands counts to one row per case first. The articles previously recommended
  `as.data.frame()`, which is correct only for a line list.

* **Fixed the pkgdown build on CI.** The shared "Learning more" fragment was
  pulled in with a relative child path (`../../man/fragments/...`). \pkg{rmarkdown}
  renders into an intermediates directory under `tempdir()` and copies relative
  resources alongside it, so a path containing `../..` escapes that directory --
  harmless where `tempdir()` is deep, fatal on CI where it sits two levels from
  the filesystem root (`cannot create file '/tmp/RtmpXXXX/../../man/...'`). The
  fragment moved to `inst/fragments/` and every caller now locates it with
  `system.file()`, which is path-independent and also works under
  `pkgload::load_all()`.
* **Dependency fixes for CI.** `almanac` is used by the package but was declared
  nowhere -- the `Remotes:` entry for it was inert, since `Remotes` only says
  *where* to fetch an already-declared dependency. It is now in `Suggests`, with
  its r-universe added to `Additional_repositories` (it was archived from CRAN).
  `nowcaster` was declared but unobtainable from any configured repository; it
  briefly gained a `Remotes:` entry, and was then dropped altogether (above).

* **`tidy()` on a \pkg{diseasenowcasting} fit now works directly.** From
  \pkg{diseasenowcasting} 2.1.0 that package re-exports the shared `generics`
  generic and ships its own method, so `tidy(fit)` returns the standard nowcast
  table. `tbl.now` now registers its own method for
  `diseasenowcasting::nowcast_prediction` **only when the package does not supply
  one**, so older versions keep working and newer ones are not overridden. The
  article calls plain `tidy(dnc_fit)` again.

* **Article fixes so the code on the page reproduces the output shown.** Three
  places displayed results the printed code could not produce: the
  \pkg{diseasenowcasting} section tidied the *fit* rather than `predict(fit)`,
  and the \pkg{baselinenowcast} section hid the
  trailing-row trim that keeps the final week from exploding. All three now
  match the precompute.
* **Documented two `tidy()` masking hazards.** `library(diseasenowcasting)`
  attaches its own `tidy()` generic, and `library(broom)` overwrites
  `tbl.now`'s `tidy.list()` method (which \pkg{NobBS} fits dispatch on). Neither errors; both silently return a different table.
  `tbl.now::tidy()` disambiguates.

* **New `tidy()` method for \pkg{epidist} fits.** \pkg{epidist} is the one
  supported package that does not nowcast -- it estimates the reporting-delay
  distribution -- so `tidy.epidist_fit()` returns a *delay-shaped* table (`term`,
  `estimate`, `conf.low`, `conf.high`, `level`, `engine`) with one row per
  distribution parameter, rather than forcing a delay fit into the per-event-date
  nowcast schema. `probs` works, because the fit keeps its draws. Note that
  `epidist()` returns `c("brmsfit", "epidist_fit")` in that order, so a loaded
  \pkg{broom.mixed} wins dispatch; call `tidy.epidist_fit()` explicitly if that
  matters.
* **`tidy()` now returns \pkg{surveillance}'s credible interval**, which it
  previously discarded. `surveillance::nowcast()` stores a prediction interval
  in the returned object's `pi` slot at the width `control$alpha` names (95% by
  default), but the method hard-coded `conf.low`, `conf.high` and `level` to
  `NA`, so surveillance was the one engine that appeared to report no
  uncertainty. Reaching for the JAGS-backed `bayes.trunc`/`bayes.trunc.ddcp`
  methods was never needed to get an interval.
* **Censored delays no longer break the converters.** A censoring indicator that
  is a property of the *case* rather than of the delay -- an administrative
  "this date is only an upper bound" mark, say -- splits one
  `(event_date, report_date)` cell into a censored and an uncensored row. A
  reporting triangle has one slot per cell, so `tbl_now_to_baselinenowcast()`
  and `tbl_now_to_epinowcast()` aborted on duplicate cells, and the converters
  that expand back to a line list picked the flag up as an unrequested
  stratifier. The censoring dimension is now collapsed before the conversion,
  and each route warns:
  * **count data**: counts are summed over the flag, so case totals are
    unchanged;
  * **line lists**: the column is dropped, leaving one row per case.

  `tbl_now_to_epidist()` is deliberately exempt: estimating a delay
  distribution is the one job that can use the flag.

* `tbl_now_to_baselinenowcast()` now handles a **line list on its own**. It
  already aggregated to incidence; it now also completes the zero periods out to
  the `now` (new `complete = TRUE` argument). A reporting triangle is a
  rectangular grid, and an event period with no reports has no rows, so the
  triangle used to stop short unless you remembered
  `to_count() |> complete_zeroes()` first. Linelist and count-incidence input now
  produce an **identical** triangle.
* `tbl_now_to_baselinenowcast()` also accepts **`count-cumulative`** data, which
  it used to refuse. De-accumulating produces negative increments wherever a
  total was revised downward, and \pkg{baselinenowcast} ships
  `preprocess_negative_values()` for exactly that; the converter applies it and
  warns. `negatives = "error"` restores the old refusal.
* **Bug fix: `tidy()` ignored the strata of a `diseasenowcasting` fit.** A
  stratified fit reports `strata_draws` (draws x event times x stratum), but the
  method read only the pooled `draws`, so every row came back with
  `stratum = "all"` even when the fit itself said "2 strata". It now returns one
  block per stratum, matching what the other engines do.
* Two new test files worth naming, because they exist to stop silent
  regressions:
  * `test-converter-equivalence.R` -- every converter accepts line-list input,
    and the triangle/preprocessing targets give the *same* result from a line
    list as from the equivalent count-incidence object.
  * `test-converter-datasets.R` -- every converter against every dataset the
    package ships. This is the testthat counterpart of the article's matrix: the
    article documents, this one fails.
* Website: fixed a regression that drew an empty scrollbar track ("a rectangle")
  under every code chunk. The no-wrap rules have to apply to `code` as well as
  its container, but `overflow-x` must apply ONLY to the container -- setting it
  on the inner `<code>` too made each one a second scroll context that reserved
  a gutter. Figure captions are now centred, smaller and grey.

* The nowcasting-models article now **shows the real output of every fit and
  every `tidy()` call**. The fits are far too slow to run on each build, so
  `data-raw/nowcast_comparison.R` captures what each one prints and what
  `tidy()` returns for it, and the article replays that. Each section pairs a
  copy-pasteable `tidy(fit)` (shown, not run) with a hidden `head(5)` whose
  output appears beneath it, so nothing in the visible code has to be trimmed for
  display. The ad-hoc result extraction each section used to do -- pulling
  `$estimates` out of NobBS, building a `data.frame()` from `epoch()` and
  `upperbound()` for surveillance -- is gone; every section now uses `tidy()`.
* New article section running **every converter, plus a nowcast, against every
  dataset the package ships** (`data-raw/converter_matrix.R`), recording which
  combinations work and explaining the ones that do not: `count-cumulative`
  cannot become a reporting triangle without inventing negative increments,
  `epidist` has no individual delays to censor in a cumulative series, and a
  `tsibble` needs a unique index/key. Each attempt is time-limited so the matrix
  is reproducible.
* The article states plainly that each modelling package is a **separate
  install** that `tbl.now` does not pull in, with the commands for the ones that
  are not on CRAN and the note that Stan, JAGS and R-INLA are software outside R.
* `SKILL.md` documents `tidy()`, the `surveillance` converter,
  `format = "triangle_list"`, the new `complete_zeroes()` behaviour, and the
  zero-period pitfalls.

* Every package section in that article now shows how to recover its predictions
  with `tidy()`.
* The comparison precompute falls back gracefully when \pkg{baselinenowcast}
  rejects a triangle whose most recent reference times are all zero. A thin
  stratum can hit that after the zero weeks are completed out to `now` -- here
  the female series has no case in the final week even though the pooled series
  does -- and the fallback completes only as far as the last week holding a case,
  costing that stratum one week rather than the whole nowcast.

* New **`tidy()`** methods, one shape of answer whatever engine produced the fit.
  The converters normalise what goes *into* a nowcasting package; `tidy()`
  normalises what comes back out. It returns `event_date`, `stratum`,
  `estimate`, `conf.low`, `conf.high`, `level` and `engine` for fits from
  \pkg{diseasenowcasting}, \pkg{baselinenowcast}, \pkg{epinowcast},
  \pkg{NobBS} and \pkg{surveillance}.
  * `probs` adds one column per requested quantile, named after the probability
    (`q5`, `q50`, `q2.5`, ...). Only the engines that keep draws
    (\pkg{diseasenowcasting}, \pkg{baselinenowcast}, \pkg{epinowcast}) can
    honour it; the others error rather than return an approximation dressed up
    as a quantile.
  * `level` records the width each engine's interval **actually** has --
    \pkg{epinowcast} reports a 90% band by default while the others report 95%,
    and without it the two get compared as if they were the same.
  * \pkg{NobBS} returns an *unclassed* list, so it is told
    apart by structure, with an `engine` argument to override.
  * `tidy()` deliberately does **not** re-grid: packages that bin onto their own
    week starts keep them, because snapping would hide a real difference.
  * The generic comes from \pkg{generics} (a new, dependency-free `Imports`), so
    it composes with \pkg{broom} rather than masking it.
* The nowcasting-models article now cuts at the **second week of July 2002**
  rather than the turn of the year: a December cut lands on the holiday reporting
  slump, which says more about December than about the models. Section headings
  are now just the package name, each package's figure carries a caption instead
  of a heading, each gains a *Simple nowcast* heading, and packages needing an
  external backend (Stan, JAGS, R-INLA) carry a coloured requirement callout. The
  overview table gained an *Additional requirements* column.
* Website: `.alert-warning` callouts now use the attenuated red the plots use for
  intervals, and code blocks are pinned to scroll sideways rather than wrap --
  pkgdown's `white-space: pre-wrap` on `code` inside `pre` was overriding the
  bare `pre` rule and folding long lines onto a second line.

* Print methods now write to **stdout** instead of emitting messages.
  `print.batch_test()`, `print.transport_discriminant()`,
  `print.tbl_now_triangle_list()` and the `temporal_effects` print method used
  the `cli_*()` family, whose output is a *message* -- so it vanished under
  `message = FALSE`, `sink()` or `capture.output()`, which is exactly where a
  print method is expected to work. They now use cli's `cat_*()` family. The
  matching tests were switched from `cli::cli_fmt()` to `capture.output()`.
* The `epinowcast` section filtered on a hard-coded `2008-12-20`, left over from
  the old window; against the new data that matched **zero rows** and aborted the
  build. It now trims relative to the series, using
  `tbl_now_to_epinowcast(preprocess = FALSE)` followed by
  `enw_filter_reference_dates()` and `enw_preprocess_data()`.


* `tbl_now_to_baselinenowcast()` gained `format = "triangle_list"`: one reporting
  triangle **per stratum**, instead of pooling them into a single matrix. Unlike
  splitting the long format by hand it takes the delay unit and the strata off
  the object, so neither has to be restated. With no strata attached the result
  is still a list — of length one, named `"all"` — so the return type never
  depends on whether strata happen to be present.
* The result is a thin `tbl_now_triangle_list` class: still an ordinary list, so
  `lapply()` and `[[` work as before, but with a `print()` method. The class
  earns its place as a guard: \pkg{baselinenowcast}'s
  `estimate_and_apply_delays()` also takes a list of triangles, but *retrospective
  snapshots of one series* rather than one per stratum, and would silently accept
  a per-stratum list and treat the strata as points in time.
* `as_tbl_now()` gained a method for `tbl_now_triangle_list`, rebuilding a
  `count-incidence` `tbl_now` with the strata recoded onto their column. The
  strata **values** are stored on the object rather than parsed back out of the
  element names, so a stratum containing the name separator still round-trips.
* **Bug fix: `as_tbl_now()` aborted on any weekly reporting triangle.**
  `tbl_now_from_baselinenowcast()` ignored the triangle's own `delays_unit`
  attribute and read the delay columns as *days*, so a weekly triangle produced
  daily report dates against weekly event dates and unit inference contradicted
  itself ("report_units must be coarser than or equal to event_units"). Both
  directions now default `delays_unit = NULL` and resolve it from the attribute;
  an explicit value still wins.

* **Bug fix in `complete_zeroes()`: it was silently deleting real cases.** The
  closing "don't look into the future" filter compared with `<` rather than
  `<=`, so every row reported on the final report date was dropped — in the
  function's own documented example, 5 of 55 cases vanished. A function whose
  job is to *add* zeroes was removing data at the boundary.
* `complete_zeroes()` now completes out to the object's `now`, not merely to the
  last event date present in the data, and gained an `until` argument to
  complete to a specific date instead. An event date with no reports at all does
  not appear in the data, so the old behaviour left a gap exactly at the `now`
  edge — where nowcasting matters. A supplied `until` never truncates below the
  data. The line-list error message now explains why a line list cannot hold a
  zero week and points at `to_count()`.
* `plot_reporting_hexamap()`'s `max_cells` is now a real bound. It previously
  took the delay at position `max_cells` and kept every cell sharing that delay,
  so a wide band at the cut overshot the documented cap.
* The nowcasting-models comparison now runs to the `now` for every engine.
  `baselinenowcast` gets there via `complete_zeroes()`; `surveillance` cannot (a
  zero-count row expands to zero line-list rows, so padding evaporates) and is
  instead given its grid directly through `control$dRange`. The article explains
  both, including that forcing `surveillance` to estimate a period with no
  observations is unstable on stratified data.

* The nowcasting-models article now builds its `tbl_now` from the **whole**
  `denguedat` series (52,987 cases over 1,091 weeks) instead of a pre-filtered
  two-year window. Every converter runs on the full object in a few seconds;
  where a package needs a shorter series to fit, the article now uses **that
  package's own argument** rather than subsetting the data first — `moving_window`
  in `NobBS` (which is what takes the full-series fit from impractical to about
  six seconds) and `when` in `surveillance`.
  `diseasenowcasting` (~12 s) and `baselinenowcast` (~10 s) take all 1,091 weeks
  as they are. `epinowcast` is the one engine with no such argument, since the
  reporting triangle is already built by the time you hold a preprocessed object;
  the article shows `tbl_now_to_epinowcast(preprocess = FALSE)` followed by
  `enw_filter_reference_dates()` and `enw_preprocess_data()`, and prints the full
  and trimmed objects side by side. Each section states which it is doing.
* Website: `.alert-info` callouts (pandoc `::: {.alert .alert-info}` fenced divs)
  are restyled from the Bootstrap default blue into the package's sage green,
  with a darker green left rule and heading colour.
* The nowcasting-models article's `baselinenowcast` fit referred to a
  `dengue_triangle2` object that no longer existed; it now uses
  `dengue_triangle`.

* The example article was rewritten from scratch around the new
  `hai_bucaramanga` dataset and is now a full **end-to-end tutorial**: cleaning a
  messy surveillance extract with `dplyr` + `tbl.now` (duplicate records, missing
  dates, and reports dated before the event they describe), reading the data with
  `autoplot()` and the standalone `plot_*()` diagnostics, testing the reporting
  delay for drift and change points, attaching only the temporal effects the data
  justifies, and finally nowcasting with `diseasenowcasting` and five other
  engines. Each modelling choice at the end is traced back to a diagnostic at the
  beginning. It moved from `vignettes/` to `vignettes/articles/` (the pkgdown
  URL is unchanged) because `diseasenowcasting` is not on CRAN and so cannot be
  fitted while building a shipped vignette.

* New converters for further back-ends:
  * `tbl_now_to_surveillance()` builds the individual-level line list
    [surveillance::nowcast()] works from, renaming the event and report dates to
    \pkg{surveillance}'s own `dHospital` / `dReport` defaults. `format = "sts"`
    instead returns the observed curve as a `surveillance` `sts` object.

  It accepts count data as well as line lists, expanding counts back to one row
  per case (de-accumulating first when the data is cumulative).
  \pkg{surveillance} is a new `Suggests`.
* The nowcasting-models article gained a section for it and a closing
  **comparison of every engine on one set of axes** — one plot for the
  unstratified object and one faceted by stratum, with a colour per package, the
  incomplete data each engine actually saw, and the counts those weeks
  eventually reached. The comparison deliberately uses an earlier 2002-2003
  window rather than the article's main `dengue_now`, because the latter runs to
  the end of `denguedat` and so has no ground truth to check against. The fits
  are precomputed by `data-raw/nowcast_comparison.R` and read from a saved file,
  so editing the prose no longer re-runs Stan, JAGS and INLA.

* `flusight` no longer ships duplicate rows (#25). The upstream FluSight
  `time-series.csv` contains 39,139 exact duplicates, which forced every example
  to open with a `distinct()` call; the dataset now goes from 491,706 to 452,567
  rows. The removal is lossless — every repeated
  (`as_of`, `target_end_date`, `location_name`) key carried an identical
  `observation`, with no conflicting values — so that triple is now a unique key.
  The help page documents the change, and the FluSight example vignette drops the
  de-duplication step.

* New dataset **`hai_bucaramanga`**: 1,423 healthcare-associated infections
  (IAAS) notified in Bucaramanga, Colombia, 2016-2023, from the Colombian open
  data portal. Column names and categorical values are translated from Spanish.
  It is a deliberately *unpolished* extract and its help page documents the
  defects in detail — a `1900-01-01` missing-date sentinel, 88 negative
  reporting delays, 100 exact duplicate records, and a strongly bimodal delay
  (3-day median, 92-day 90th percentile) — which makes it a realistic exercise
  for the delay diagnostics rather than a clean modelling example.
* `test_delay_drift()` and `test_delay_changepoint()` now document **every
  column of their output**, plus new *Interpreting the result* sections. The
  `test_delay_drift()` help gained a *Choosing a method* section explaining why
  `"hamed-rao"` is the default (deterministic, no AR(1) assumption, effectively
  instant) and when to cross-check with `"block-bootstrap"`, which is robust to
  weekly periodicity but stochastic and thousands of times slower.
* The Get Started vignette now opens the nowcasting problem with a **figure**
  showing observed-to-date cases, the reports still in transit, and the nowcast
  of the eventual total.
* The "Learning more" links live in a single `man/fragments/learning-more.Rmd`
  and are included in the README and at the end of every vignette and article,
  so they only have to be edited in one place.
* Website: the "Articles" navbar dropdown was rendering near-black with grey
  text because the styling targeted `.submenu`, which Bootstrap 5 does not use;
  it now targets `.dropdown-menu` and matches the pale red of the package
  plots. `pkgdown/extra.css` is also no longer listed under
  `includes: in_header:`, which was pasting raw CSS into `<head>` where it was
  ignored.
* README code blocks no longer wrap mid-tibble: printed output was being split
  into stacked column blocks by R itself, which no stylesheet could undo.
  
# tbl.now 0.15.0

* `autoplot()` panels are now consistently **colour-coded by process**: red for
  everything reporting-related (the delay distribution, the delay calendar/holiday
  effects, the delay periodogram) and green for the epidemic (event-date) process
  (the observed cases and their calendar/holiday effects). This matches the colours
  the standalone diagnostic plots (`plot_reporting_process()` /
  `plot_epidemic_process()`, `plot_scalogram()`, ...) already used, so a panel and
  its standalone twin read the same.
* Every `autoplot()` panel now says **which process it describes** in its subtitle
  — either "Reporting delay process" or "Epidemic (event-date) process" — replacing
  the per-panel explanatory subtitles. A single panel therefore reads on its own.
* The two periodogram panels are renamed **"Cycles (periodogram)"** (previously
  "Seasonality" / "Delay periodicity").
* Every `autoplot()` panel now has a standalone `plot_*()` twin that draws just
  that panel (identical data, colours and subtitle): `plot_day_of_week_effects()`,
  `plot_week_of_year_effects()`, `plot_month_of_year_effects()`,
  `plot_holiday_effects()`, `plot_holiday_lag_effects()` (each taking
  `type = "epidemic"` or `type = "report"`), plus `plot_cycles()`,
  `plot_delay_distribution()` and `plot_observed_cases()`. Use `autoplot()` for the
  grid and a `plot_*()` for one effect on its own.
* The day-of-week, week-of-year, month-of-year, holiday and weekend/holiday-lag
  panels gained a `measure` argument (in `autoplot()` and every `plot_*()` twin).
  `measure = "normalized"` (the default) is the existing view — each value divided
  by its overall mean, 1 = average. `measure = "percent"` instead shows the
  **share of cases** falling in each group with its IQR (e.g. "10% of cases at the
  weekend versus 90% on weekdays"); the reporting version shares out the reports by
  report date. Percentages need `Date` event/report columns.
* Vignettes: the Get Started guide documents the `plot_*()` twins and the
  `measure` argument, and marks the "Holiday effects", "Do delay distributions
  drift over time?" and "Detecting batch reporting" sections as AI-written, pointing
  readers to the human-written batch-reporting article. The FluSight example
  analysis is flagged as a work in progress.

# tbl.now 0.14.1

* Strata are now carried into the model converters that can use them.
  `tbl_now_to_epidist()` keeps the strata as data columns (usable as covariates in
  an epidist formula), and `tbl_now_to_baselinenowcast(format = "long")` keeps them
  so you can build one reporting triangle per stratum. A single reporting-triangle
  **matrix** has no strata dimension, so `format = "matrix"` now **pools** the
  strata with a warning instead of erroring on duplicate cells.
  `tbl_now_to_epinowcast()` already passed strata as its grouping (`by`).
* The nowcasting-models article was restructured: each package is now shown
  **bare** (from `dengue_now`) and then **enriched** — one `dengue_seasonal` object
  carrying a stratum and temporal effects flows through every converter — so the
  separate "Carrying delay effects into each model" section is gone. It adds a
  worked **per-stratum** `baselinenowcast` loop (one triangle per stratum). The
  \pkg{baselinenowcast} workflow also had a bug: it used the plural
  `estimate_and_apply_delays()` (which expects a *list* of retrospective triangles)
  on a single triangle; it now uses the one-call `baselinenowcast()` wrapper for
  samples and notes the singular `estimate_and_apply_delay()` for a point nowcast.
* New `plot_reporting_hexamap()`: draws the reporting triangle as an
  age-period-cohort **hexamap** (Jalal and Burke, 2020). Event date, report date
  and delay are the cohort, period and age (`report = event + delay`); each cell is
  a hexagon coloured by its report count, and a **batch** — a single report date —
  reads as a clean **vertical stripe**. The number of hexagons is bounded by a
  `max_cells` safety cap (the delay axis is auto-capped, with a message, rather
  than drawing an unbounded map). Replaces the reporting-V panel in the batch
  article.
* Bug fix for issue #33: `autoplot(x, strata = "race", by_strata = TRUE)` no longer errors with
  a strata passed as column name. 
* `autoplot()` gained four **holiday panels**, which describe the attached
  `temporal_effects()` spec rather than the event unit:
  - `"calendar_holiday"` / `"delay_holiday"` — normalized cases / mean reporting
    delay by **day type**. The categories follow the spec: a `holidays` calendar
    plus `weekend = TRUE` gives `Weekday`/`Weekend`/`Holiday`, a calendar alone
    gives `Non-holiday`/`Holiday`, and a weekend effect alone gives
    `Weekday`/`Weekend`. A holiday falling on a weekend counts as a holiday.
  - `"calendar_holiday_lag"` / `"delay_holiday_lag"` — the same, by **position
    relative to the nearest holiday** (`"2 before"`, `"1 before"`, `"Holiday"`,
    `"1 after"`, ..., plus `"Other"` as the reference), as asked for by
    `holiday_lags`. These show exactly the days the `..._holiday_lag_k` /
    `..._holiday_lead_k` columns flag, so you can check a lag is worth modelling
    before you model it. A date that is both after one holiday and before the next
    is attributed to the nearer one, ties going to the "after" side.

* Bug fix: `tbl_now_to_epinowcast()` now passes a `timestep` to \pkg{epinowcast},
  inferred from the object's report units (`"days"` -> `"day"`, `"weeks"` ->
  `"week"`) and overridable with the new `timestep` argument. It previously left
  \pkg{epinowcast} on its `"day"` default whatever the data, so **weekly** data was
  laid out on a daily grid. 

* Bug fix: `tbl_now_to_epinowcast()` now derives the temporal-effect covariates on
  \pkg{epinowcast}'s completed date grid instead of carrying them through
  `enw_complete_dates()`. That function fills the (reference, report) grid and
  extends the reference axis into the nowcast horizon, but sets every non-schema
  column to `NA` on the rows it adds — so the covariates previously survived only on
  the original rows. Becasue the effects are functions of a date alone, they are n
  ow re-derived from the completed grid and cover every row, 
  including the recent horizon dates a nowcast has to predict.

# tbl.now 0.14.0

* `holiday_lags` and `weekend_lags` in `temporal_effects()` now accept **negative
  depths**, placing the effect *before* the break instead of after it. A negative
  depth creates `..._holiday_lead_k` / `..._weekend_lead_k` indicator columns that
  flag dates exactly `k` **working days** before a holiday / weekend, counting
  backwards from it — so `_lead_1` is the working day closest to the break.
  `weekend_lags = -1` flags the Friday, `weekend_lags = -3` flags the Wednesday,
  Thursday and Friday, and `holiday_lags = -1` flags Christmas Eve. Working days
  skip weekends and holidays exactly as they do for positive depths, and
  `holiday_lags` still requires a `holidays` calendar for either sign. Use it to
  capture the reporting slowdown that precedes a break; attach one specification
  per direction to model both sides of it. Positive depths are unchanged.
* `?temporal_effects` gained a **"Using a different holiday calendar"** section.
  `holidays` has always accepted any `almanac::rcalendar()`, but the docs only
  showed `cal_us_federal()`; reporting holidays are local, so the section covers
  the building blocks (built-in `hol_*()` rules, custom `rholiday()` rules,
  weekend observance with `hol_observe()`, and editing a calendar with
  `cal_add()` / `cal_remove()`), and works through the New York City calendar as
  an example.

# tbl.now 0.13.1

* Fixed style in the batch reporting vignette
* Improved the axis title position in the v triangle to better visualize the dates

# tbl.now 0.13.0

* Bug fix: `batch_shape_test()` no longer errors ("missing value where TRUE/FALSE
  needed") on large count data. The standardised rank-sum expands counts to one
  value per item, so the group sizes could exceed the 32-bit integer range and
  their product overflowed to `NA`; the group sizes are now computed as doubles.
* `batch_test()` now returns a **lean, Benjamini-Hochberg-only** result:
  `report_date`, `stratum`, `reported`, `baseline`, `deficit`, `delta`,
  `p_transport`, `p_transport_bh` and the `batch` flag, each documented under
  `?batch_test`. The raw per-point `classification` column (and the
  `p_creation`/`p_deletion`/scale columns behind it) has been dropped: it was not
  multiplicity-corrected and over-identified, whereas `batch` controls the false
  discovery rate. (`transport_discriminant()` keeps its `classification`.)
* `batch_test()` (and `transport_discriminant()`) now infer the calendar
  `period` from the object's temporal effects: a **day-of-week** effect sets
  `period = 7`, a **week-of-year** effect `period = 52` (see
  [add_temporal_effects()]). A `period` you pass still wins, with a note if it
  disagrees; and if the data is daily with no temporal effect, the function
  suggests `period = 7`.
* The `baseline_method` argument of `batch_test()` and `transport_discriminant()`
  has been **removed** — the baseline is always the repeated-median local line.
  The running-median (local-constant) alternative had no advantage: it reduces to
  the same fit on a flat series and is biased the moment the series trends.

* New `covid_us` dataset: a compact aggregation of the U.S. CDC COVID-19 Case
  Surveillance Public Use Data, with both event and report dates in 2020-2021 (a
  self-consistent "as of the end of 2021" snapshot), built to demonstrate **batch
  reporting**. Its reporting delay is huge and heavily right-skewed — cases were
  released to CDC in large backlog dumps — so `batch_test()` and the batch plots
  recover a clear, real signal (and correctly call the biggest December-2021
  spikes *surges*, since they land on the Omicron wave). Prepared with duckdb from
  the 14 GB source (see `data-raw/covid_us.R`).
* New article, *Finding batch reporting in CDC COVID-19 case surveillance data*,
  written for public-health practitioners with no maths. It builds a made-up
  outbreak with a planted batch to show what each plot looks like (including a
  novel **V reporting triangle** -- the reporting triangle rotated 45° so a batch
  is a horizontal slice), rehearses on a **real dengue epidemic curve** with
  simulated log-normal reporting and self-planted batches, finds the batches in the
  real `covid_us` data, adds a **wavelet** view (window-inner report-vs-event
  scalograms, via \pkg{wavScalogram}), and ends with a one-page summary table. A
  new **transport-vs-creation tutorial** plants a hold, a batch and a surge in a
  made-up outbreak and colours each day the same way on the reporting timeline and
  in the creation/transport plane, so a reader can trace a bar to its dot and see
  why a batch goes *up*, a surge goes *right*, and a hold drifts *up-and-left*.
* Every plot function now takes **`plotly = TRUE`** to return an interactive
  \pkg{plotly} widget (hover, zoom) instead of a static \pkg{ggplot2} plot:
  `plot_reporting_process()`, `plot_epidemic_process()`, `plot_reporting_triangle()`,
  `plot_delay_profiles()`, `plot_delay_drift()`, `plot_transport_discriminant()`,
  `plot_reporting_v()`, `plot_scalogram()`, `diagnostic_plot()` and `autoplot()`.
  Needs the (suggested) \pkg{plotly} package.
* New `plot_reporting_v()`: the reporting **"V"** -- the same data as
  `plot_reporting_triangle()` (the same event-date x delay cells) rotated 45° so
  report date runs up the page and the data opens into a V (left arm = event date,
  right arm = delay). A batch, a diagonal in the square triangle, becomes a
  horizontal slice. The whole observable triangle is filled (pale-blue reported
  zeros + coloured reports).
* New wavelet **scalograms**, `plot_scalogram(type = "reporting")` and
  `plot_scalogram(type = "epidemic")`, plus the paired `plot_reporting_process()`
  and `plot_epidemic_process()` bar charts. The scalogram splits the count series
  into fast wiggles (short periods) and slow swings (long periods) and shows the energy
  at each: a **batch** lights up as a bright short-period ridge in the *reporting*
  scalogram that the *epidemic* (event) scalogram lacks. These use a **window-inner**
  scalogram (\pkg{wavScalogram}, `border_effects = "INNER"`): computed from observed
  data only, with **no border padding**, so nothing is fabricated at the recent
  ("now") edge that matters for nowcasting. Reporting views are drawn in red,
  epidemic views in green. `plot_scalogram()` defaults to the PAUL wavelet
  (`wname`), which localises a batch more sharply; takes a `format` argument for
  the x-axis date labels (default `"%d/%b/%y"`); and paints the region outside the
  cone of influence dark grey. The series is analysed on its own integer time grid,
  so weekly (or monthly) data is handled correctly, and the heat map tiles a
  uniform index relabelled with dates so it stays gapless even for long series.
* The conservation monitors — `plot_creation_transport()` (the two window scores as
  stacked panels) together with the cumulative-backlog, reporting-lag, dashboard and
  transport-minus-creation "batch score" plots — live in
  `devel/conservation_extras.R`, kept out of the package: clean on large batches but
  noisy in general. The transport diagnostics keep their exported
  `transport_discriminant()` / `plot_transport_discriminant()`.
* `simulate_batch()` gains a **`held_fraction`** argument: the fraction of each
  closed date's reports actually held back and released later (default `1`, a full
  closure). With `held_fraction = 0.5`, roughly half of each day's reports are held
  and half report on time -- a realistic partial slow-down rather than a total
  blackout. Supported for `"linelist"` and `"count-incidence"` data (a cumulative
  total cannot be split).
* The default `lookback` for `batch_test()` and `transport_discriminant()` is
  now **7** (a week of daily reporting) rather than 3.
* The `@details` of the batch functions (`batch_test()`,
  `transport_discriminant()`, `batch_shape_test()`, `simulate_batch()`) and the
  batch plots were trimmed: the formal theorem / null-distribution derivations
  were replaced with concise, plain-language explanations.
* New `diagnostic_plot()`: a gallery of complementary views of the reporting
  process for spotting reporting artefacts (above all *batch reporting*), laid out
  in **two columns**. The five panels are the **reporting process** (reports by
  report date), the **reporting triangle** (event date x delay), the per-date
  **delay profiles**, the **reporting-delay drift** (`plot_delay_drift()`), and the
  **transport discriminant** plane. Each is also its own exported function. Choose
  views with `panels` (a single one is returned as a plain plot), and every view
  is facetted by stratum. `by = c("report", "event")` switches the profiles panel;
  `...` (e.g. `period = 7`) is routed to whichever panels accept it.
    * Every panel carries a plain-language, grey **caption** explaining what it
      shows and what the colours mean, and legends are labelled in words.
    * The **reporting process** y-axis is capped at the 99th percentile only when a
      *pathological* dump (over 30x the median day, e.g. covid's 1.8M-report day)
      would otherwise flatten the whole series; an ordinary batch spike -- the very
      thing the plot exists to show -- is left to tower.
    * The **transport discriminant** y-axis is limited to the batch region (with
      default clipping, so points stop at the panel edge) so the deep-negative
      "hold" dates do not squash the confirmed batches; the shaded region is now
      labelled *"Potential batch region"* and each confirmed batch gets a bold,
      unclipped date label.
    * The **reporting triangle** draws a **third axis for report date**: evenly
      spaced dashed diagonals (`report = event + delay`) running up-right at 45°,
      labelled by report date, so event date (x), delay (y) and report date are all
      readable off one plot (`plot_reporting_triangle(report_ticks =)`, default 6;
      `mark_batches =` optionally highlights the biggest batch stripes). It also
      distinguishes an *observable reported zero* (muted blue) from a *not yet
      reportable* cell (blank), on the *full calendar* event axis.
    * The **delay profiles** draw in a single colour at fixed transparency.
    * The **transport discriminant** colours red only the
      `batch_test()`-confirmed batches (BH-corrected), not the raw per-point
      classification -- which at level `alpha` painted 10-20% of points
      batch/surge/hold by construction, ignoring multiplicity and the heavy
      autocorrelation of the window statistics. The shaded batch region and the
      `±z*` lines are drawn only as a reference for where a batch would sit.
* New `transport_discriminant()`: exposes the plane behind `batch_test()`'s
  conservation law -- for every report date the **deficit** (the transport axis:
  reports the preceding window is missing) and the window **discriminant** (the
  creation axis: the window total relative to its baseline), with robust
  standardised `transport_z` / `creation_z` and the same quadrant `classification`.
  A batch sits top-left (a deficit paid the spike, no net creation); a surge sits
  bottom-right. Returned as a `transport_discriminant` tibble and plotted by
  `diagnostic_plot(panels = "transport")`.
* The multi-panel `autoplot()` title changed from *"Diagnostic plots"* to
  **"Automatic plot of effects"** (that phrase now titles `diagnostic_plot()`).
* `batch_test(null_model = "auto")` is now **overdispersion-aware**. The exact
  Poisson/Binomial null assumes Poisson counts *and* a baseline that captures the
  mean; real surveillance counts are overdispersed, and the conditional transport
  test is then badly anti-conservative (on clean but overdispersed Poisson data it
  can fake dozens of batches). `auto` now reserves the exact null for non-negative
  counts with no detected overdispersion (dispersion `<= 1.5`) and otherwise falls
  back to the dispersion-corrected robust null; signed (count-cumulative)
  increments still always use the robust null. This makes the default far more
  realistic on overdispersed data (e.g. filtered `covid_colombia` drops from ~125
  flags to ~18; add `period = 7` for its weekly reporting cadence to reach ~4).
  Force the old behaviour with `null_model = "poisson"` if you need it.
* `autoplot()`'s **empirical delay distribution** panel now adapts to
  `count-cumulative` data: instead of a histogram of increments it shows the
  *cumulative growth by delay* — boxplots (on a log scale, with a dashed reference
  at `1`) of the ratio of each event date's cumulative count at a delay to its
  count at the previous delay. Ratios above `1` are upward revisions, below `1`
  downward ones, and they converge to `1` as reporting completes, so you can see
  the cumulative curve stabilise. The log scale makes a doubling and a halving
  symmetric about `1`. `linelist` / `count-incidence` data keep the histogram, and
  the panel respects `by_strata`.
* `tbl_now_to_baselinenowcast(delays_unit = )` now defaults to `NULL` and is
  **inferred** from the object's time units for the `"matrix"` format: when the
  event and report units are equal and either `"days"` or `"weeks"`, that unit is
  used; otherwise the function errors asking you to supply `delays_unit`
  explicitly. (The `"long"` format never uses it.)
* Added the `covid_colombia` dataset from `diseasenowcasting` to here. 
* Fixed several documentation issues that produced *"could not resolve link"*
  warnings when building the docs (links to internal helpers / to the un-declared
  `trend` package, a `[0, 1]` mis-parsed as a link, and a mis-ordered internal
  roxygen block).
* `to_count()` now supports `count-cumulative` -> `count-incidence` by
  **de-accumulating** the series (increment = cumulative total minus the previous
  one within each event date and grouping). Because cumulative totals can be
  revised downward, an increment can be negative. This fixes `autoplot()` (and the
  other delay diagnostics) on `count-cumulative` data such as FluSight, which
  previously errored with *"Transformation from `data_type` count-cumulative to
  count-incidence not implemented"* (#26).
* Updated `SKILL.md` (the AI-agent usage guide) to cover everything added since
  0.10.0: reporting-delay `autoplot()` panels and the `panels` / `by_strata`
  selectors, `plot_delay_drift()` / `test_delay_drift()` / `test_delay_changepoint()`,
  the model-free batch detectors (`batch_test()`, `batch_shape_test()`,
  `simulate_batch()`), `get_nth_reported_cases()`, the after-holiday/weekend
  temporal-effect lags, `as_tibble()` / `as.data.frame()` coercion, and the new
  `count-cumulative` -> `count-incidence` support.

# tbl.now 0.12.0

## Batch detection, rebuilt around a conservation law

The report-batch detectors were rebuilt on a single, exact principle: **a batch
moves reports along the report axis without creating them**, so a window of report
dates spanning both the lull and the release has an unchanged total, whereas a
genuine epidemic surge inflates it. The previous heuristic
`detect_report_batches()` / `plot_report_batches()` (multi-signal robust-z, and
the model-based conditional scan) are **removed** and replaced by three
model-free, `r lifecycle::badge("experimental")` functions. Each derives its
mathematics in a **"The mathematics"** section of its help page.

* New `batch_test()` returns, per (report date, stratum), the `deficit` (reports
  missing beforehand — sensitive to a batch) and `delta` (the window total minus
  its expected value — sensitive to a real surge), and classifies each date as
  `"batch"`, `"surge"`, `"batch_and_surge"`, `"hold_or_deletion"` or `"none"`. The
  transport (batch) test conditions on the window total, so its size does not
  depend on the unknown incidence nor on the quality of the baseline; the baseline
  itself is refit from report dates *outside* each candidate window, which makes
  `delta` invariant to a within-window batch pathwise. It handles all data types,
  including `"count-cumulative"` (signed increments), and takes a `period` argument
  that absorbs a fixed reporting schedule (weekends, holidays).
* New `batch_shape_test()` tests whether a flagged report date drew on unusually
  *old* event dates, by a permutation rank-sum on the reporting delays. It is
  exactly distribution-free whenever incidence is locally log-linear.
* New `simulate_batch()` plants a known batch (a deterministic close-and-release)
  in a `tbl_now`, for validation and teaching.
* New **Batch detection** article, with worked examples on dengue (a planted
  batch), FluSight (count-cumulative), and a weekend reporting schedule.

# tbl.now 0.10.1

* `autoplot()`'s reporting-delay calendar panels (`delay_weekday`, `delay_week`,
`delay_month`) are now **normalized**: each event date's mean delay is divided by
the overall mean delay, so `1` marks an average delay and a dashed reference line
is drawn there. Previously the ungrouped panels plotted the raw mean delay while
the `by_strata = TRUE` panels were already normalized. They now share one scale,
matching the case-count calendar panels and making the calendar *pattern*
comparable across strata (y-axis: `"Normalized delay"`).
* `plot_delay_drift()`'s `window` now defaults to **`7` periods regardless of the
time unit** — 7 days for daily data, 7 weeks for weekly data. Previously the
default was data-dependent (`max(5, n_periods / 20)`), which produced a very wide
window on long series. Pass `window =` to smooth a specific series.
* Internal: replaced the remaining base-R data-frame subsetting and column
assignment (`df[cond, ]`, `df$col <- ...`) outside the converters with the
equivalent `dplyr` verbs (`filter()`, `select()`, `slice()`, `mutate()`). No
user-facing behaviour change. The examples and vignettes now likewise use
`dplyr::filter()` rather than `[` (e.g. `dplyr::filter(batches, batch)`).

# tbl.now 0.10.0

* New `get_nth_reported_cases()`: the cumulative cases reported for each event
date **within a given delay**. `delay = 0` gives the initial snapshot, `delay = 1`
adds the delay-1 reports, and so on; `delay = Inf` (or the maximum delay) matches
`get_latest_reported_cases()`. Documented alongside `get_initial_reported_cases()`
and `get_latest_reported_cases()`.
* **Performance**: `get_latest_reported_cases()`, `get_initial_reported_cases()`
and `get_nth_reported_cases()` are substantially faster (~3-4x on the bundled
data) — the aggregation now runs on a declassed data frame and the `tbl_now` is
reconstructed once, with identical output.
* The experimental diagnostic functions (`plot_delay_drift()`,
`test_delay_drift()`, `test_delay_changepoint()`, `detect_report_batches()`,
`plot_report_batches()`) now carry a lifecycle **experimental** badge.
`test_delay_drift()` and `test_delay_changepoint()` additionally emit a `cli`
warning that they are experimental, their results are not guaranteed and their
interface may change. Flagged batches, change points and trend changes are
surfaced as **potential** (e.g. "potential batches", "potential change point").
* New `detect_report_batches()` and `plot_report_batches()` to detect **batch
reporting** — report dates on which a laboratory releases a backlog of many old
cases at once. Working on the report-date axis, it flags a report date using up
to four selectable robust-anomaly signals (`volume`, `delay`, `span`, `gap`),
AND-ed together. Requiring the `delay` (long/dispersed delays) signal alongside
`volume` is what **distinguishes a batch from an epidemic peak**: a peak also
spikes the report volume, but its cases keep the normal short delay distribution,
so its delay score stays low. `detect_report_batches()` returns a per-report-date
table with the features, robust scores and a `batch` flag; `plot_report_batches()`
shows the report-volume and mean-delay timelines with the flagged dates marked.

* New `test_delay_changepoint()` complements `test_delay_drift()`: where the
latter tests for a *gradual* monotonic trend, this tests for a **single abrupt
change point** in the per-period delay summaries using **Pettitt's**
nonparametric test (implemented directly, no extra dependency). It reports the
estimated change date, the before/after level of the statistic, the shift and a
`changepoint_detected` verdict, per stat (median / mean / IQR / 10-90 spread) and
per stratum, on mature data only.
* `plot_delay_drift()` gained a `changepoint` argument: set it to `TRUE` to mark
the estimated change point of the median delay on the fan chart with a vertical
line.

* New `plot_delay_drift()` and `test_delay_drift()` to answer *"do reporting
delay distributions drift over time?"*.
  * `plot_delay_drift()` draws a rolling **fan chart** of the count-weighted
  delay distribution indexed by event date: a solid rolling median, a dashed
  rolling mean, and 25-75% / 10-90% quantile bands. The recent, not-yet-fully
  reported region (after the `level` incompleteness cutoff) is shaded grey so the
  truncation-induced dip is not mistaken for drift. Supports `by_strata`.
  * `test_delay_drift()` runs an **autocorrelation-robust monotonic-trend test**
  (Hamed-Rao modified Mann-Kendall by default, with Yue-Pilon and block-bootstrap
  options via the new `modifiedmk` *Suggests*) on the per-period delay summaries,
  testing both a location statistic (median/mean) and a dispersion statistic
  (IQR / 10-90 spread), on mature data only. Returns a tidy tibble with the
  Kendall tau, Sen's slope, p-value and a `drift` verdict, per stat and stratum.

* `autoplot()` gained a `by_strata` argument (default `FALSE`). When `TRUE`,
every panel is split by stratum: the calendar and delay boxplots become dodged
boxes (one per stratum, side by side), the epidemic process and both
periodograms become one coloured line per stratum (no area fill), and the delay
distribution becomes dodged bars. Boxplots are normalized **per stratum** (1 =
that stratum's own average) so the calendar pattern is comparable across strata,
and strata are coloured with a `viridis` scale. A companion `strata` argument
chooses which columns to group on (defaults to the object's `strata`; pass a
subset such as `strata = "gender"` to override).

* `autoplot()` now draws **reporting-delay** diagnostic panels alongside the
case-count ones, so you can see *delay effects*: the mean reporting delay by day
of week / week of year / month (`delay_weekday`, `delay_week`, `delay_month`),
and a periodogram of the mean-delay series (`delay_seasonality`) that reveals
periodicity in the delay itself. The delay panels are computed on the complete
part of the series (before the incompleteness line) so recent truncation does not
bias them.
* `autoplot()` gained a `panels` argument to choose which panels to draw. It
accepts the concrete panel keys, or the aliases `"all"` (default), `"calendar"`
and `"delay_calendar"`. Selecting a single panel returns it as a plain `ggplot2`
object instead of a `patchwork`. Unknown panels error; panels that do not apply
to the data's time unit are skipped with a warning.
* New pkgdown article *"One dataset, many nowcasts"* now also demonstrates that
temporal (delay) effect columns are carried into `epinowcast`
(`metareference`/`metareport`), `baselinenowcast` (long) and `epidist`, with a
table clarifying which target formats can hold covariates and how each model can
use them.

* `temporal_effects()` gained an **after-holiday** and **after-weekend** effect
via the new `holiday_lags` and `weekend_lags` arguments. Each takes a
non-negative integer depth `N`; materialising the spec then adds indicator
columns `..._holiday_lag_1 … ..._holiday_lag_N` (and likewise `..._weekend_lag_k`)
that flag dates falling exactly `k` **working days** after a holiday / weekend.
Working days skip weekends and holidays, so the effect lands on the first day(s)
back at work — designed to capture the rise in cases just after a holiday or
weekend. `holiday_lags` requires a `holidays` calendar. The columns are picked up
automatically by every `tbl_now_to_*()` converter (as covariate columns) and by
`diseasenowcasting::nowcast()`.
* Documented and tested attaching temporal effects to the **report date** (in
addition to the default event date) via
`add_temporal_effects(x, spec, date_type = "report_date")`. Event- and
report-date effects can coexist on the same `tbl_now`; both sets of columns
(`.event_*` and `.report_*`) are carried through all converters.

# tbl.now 0.9.0

* Added `as_tibble()` and `as.data.frame()` methods for `tbl_now` with an opt-in
`compute_temporal_effects` argument (default `FALSE`). Passing
`compute_temporal_effects = TRUE` materialises the lazy `temporal_effects()`
spec (holidays, Fourier terms, calendar effects) into columns before returning
a plain `tibble` / `data.frame`; the input `tbl_now` is left unchanged. The
default stays lazy on purpose, because `dplyr` relies on these coercions being
cheap, non-materialising declassers internally (e.g. `group_by()`).
* The `tbl_now_to_*()` converters now carry the (lazy) temporal-effect columns
(holidays, Fourier seasonal terms, day-of-week / calendar effects) into the
target format as covariate columns. The spec is materialised on demand via
`compute_temporal_effects()` at conversion time (the input `tbl_now` is left
unchanged), and the columns are passed to `data.table`, `tsibble`,
`baselinenowcast` long format, `epidist`, and `epinowcast` (where they appear in
the observations and `metareference` tables for use in the reference module).
The `baselinenowcast` reporting-triangle matrix still cannot hold them.
* Removed the `|>` export and changed all the pipes to `|>`
* Refactored `converters.R` for readability (dplyr column operations instead of
base indexing, full variable names, lintr-clean).
* The `tbl_now_to_*()` converters now keep the `covariates` and `is_censored`
columns wherever the target format can hold them (`data.table`, `tsibble`,
`baselinenowcast` long format, `epidist` linelist); the fixed modelling objects
(`enw_preprocess_data`, the reporting-triangle matrix, the EpiNow2 series) still
cannot carry them.
* Added S3 methods on the other packages' coercion generics so they accept a
`tbl_now` directly: `as_epidist_linelist_data()`, `as_reporting_triangle()`,
`as_tsibble()` and `as.data.table()`, each wrapping the matching
`tbl_now_to_*()`.
* Fixed `tbl_now_to_data_table()` checking for `baselinenowcast` instead of
`data.table`, and `tbl_now_to_baselinenowcast(format = "long")` no longer
requiring `baselinenowcast` to be installed.

# tbl.now 0.8.0

* Modified the `update` as the `t_effect` argument was not doing anything. 
* Fixed bug that errored `complete_zeroes` when `is_censored` was given. 
* Removed explicit zeroes from the converters (`tbl_now_from_*`) as they
are not necessary in `tbl_now`. 
* Added `censor_delays_above()` to flag reports with an implausibly long delay
as censored (their delay becomes an upper bound).
* Improved documentation and README
* Documented all internal functions with roxygen (`@keywords internal` + `@noRd`)
and ensured every exported function has a `@return`.
* Homogenized `lifecycle` badges. 
* Brought the `censor_delays_above` function from `diseasenowcasting` to `tbl_now`. 
* `tbl_now_from_epinowcast()` now accepts not only the raw long input but also a
preprocessed `enw_preprocess_data` object or a fitted `epinowcast` object
(grouping auto-detected), matching the format `epinowcast` uses for summaries
and plots.
* `tbl_now_to_EpiNow2()` gained a `model` argument: `"estimate_infections"`
(default, the single `date`/`confirm` series) and `"estimate_truncation"` (a
list of report-date snapshots, the one EpiNow2 model that uses the report
dimension). Documentation clarified accordingly.
* Fixed two converter `requireNamespace()` guards: `tbl_now_to_data_table()`
checked for `baselinenowcast` instead of `data.table`, and
`tbl_now_to_baselinenowcast(format = "long")` no longer requires
`baselinenowcast` to be installed.

# tbl.now 0.7.5

* Bumped roxygen to version 8.0.0. This also resulted in updated documentation. 
* Changed `autoplot()`'s default level to 0.95 
* Added tests for converters and pillars.
* Throws warning when converting to `baselinenowcast` if data is 
`"count-cumulative"`. 

# tbl.now 0.7.3

* Added the `update_now()` function to make it more intuitive to update
the now. 

# tbl.now 0.7.0

* Added an `autoplot()` method for `tbl_now` objects that produces a multi-panel
diagnostic overview: the empirical delay distribution, the observed epidemic
process with an incompleteness line (controlled by `level`), normalized
calendar-effect boxplots (cases relative to the overall mean), and a periodogram
to help choose Fourier `seasons`. Daily data shows both a day-of-week and a
week-of-year boxplot panel; weekly data shows week-of-year. Built on `ggplot2`
and `patchwork`. The x-axis limits of each panel can be set individually
(`delay_distribution_xlim`, `event_date_xlim`, `calendar_effect_xlim`,
`seasonality_xlim`), and holidays from the temporal-effects spec are marked with
red dots on the epidemic process.
* Added converters to and from other packages, all of the form
`tbl_now_from_*()` / `tbl_now_to_*()`: `epinowcast`, `baselinenowcast`,
`EpiNow2` (to only), `epidist`, `data.table` and `tsibble`. The
`tbl_now_from_*()` functions wrap `as_tbl_now()` and forward `...` to
`tbl_now()`; the `tbl_now_to_*()` functions call into the target package. All
accept a `verbose` argument that reports the choices made (the inferred `now`,
data type, units, and column mapping).
* `as_tbl_now()` gained methods for the classes produced by `tbl_now_to_*()`
(`enw_preprocess_data`, `reporting_triangle`, `epidist_linelist_data`, `tbl_ts`
and `data.table`), so a converted object can be turned straight back into a
`tbl_now`.
* Documented `autoplot()` and the converters in the introduction vignette.

# tbl.now 0.6.4

* Fixed dependency on R >= 4.2.0
* Update function now defaults the censoring to FALSE if the update
is censored but the original is not. 

# tbl.now 0.6.3

* Added season length to seasons so we can get weekly seasonality. 

# tbl.now 0.6.2

* Removed warning when using columns for temporal effects that cascaded into `to_count`.
* Changed DESCRIPTION to fix ortographic error and trigger less messages of unknown words. 

# tbl.now 0.6.1

* Changed links in description of `tidy-select`

# tbl.now 0.6.0

* Changed temporal effects to be lazy (as required by #17) so that now its
easier to use `dplyr`
functions without compromising them. 
* Bumped the deprecated dplyr's `*_at` functions to use `all_of()`
* Fixed to no warnings during test. 
* Users can now pass the `.delay` column directly (#6) and it will recalculate 
the missing column (i.e. event or report)
* Added `complete_zeroes` to vignette (#13).
