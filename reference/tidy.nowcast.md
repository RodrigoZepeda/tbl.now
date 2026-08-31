# Tidy a fitted nowcast into one standard table

**\[experimental\]**

Every nowcasting package returns its answer in its own shape – a matrix
of posterior draws, an `stsNC` object, a Stan fit, an INLA summary, a
bare list. `tidy()` turns any of them into the **same** table, so
downstream code (plotting, scoring, comparison) does not care which
engine produced it.

## Usage

``` r
tidy(x, ...)

# S3 method for class 'baselinenowcast_df'
tidy(x, probs = NULL, ...)

# S3 method for class 'epinowcast'
tidy(x, probs = NULL, ...)

# S3 method for class 'stsNC'
tidy(x, probs = NULL, ...)

# S3 method for class 'estimate_infections'
tidy(x, probs = NULL, ...)

# S3 method for class 'epinow'
tidy(x, probs = NULL, ...)

# S3 method for class 'estimate_truncation'
tidy(x, probs = NULL, ...)

# S3 method for class 'list'
tidy(x, probs = NULL, engine = NULL, level = NULL, ...)
```

## Arguments

- x:

  A fitted nowcast. See *Supported objects*.

- ...:

  Passed to methods.

- probs:

  Optional numeric vector of probabilities in `[0, 1]`. Adds a `q*`
  column per probability. Only available for engines that expose draws.

- engine:

  Optional string naming the engine. Needed only for the shapes that
  arrive as an **unclassed list** – a NobBS fit, or a list of
  per-stratum baselinenowcast fits – which are otherwise recognised by
  their structure.

- level:

  Interval width to report for an engine that does not say what it
  produced. Only used by the NobBS branch (see `level` under *Value*);
  `NULL`, the default, reports `NA`.

## Value

A tibble, as described above.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with one
row per event date (per stratum, where the fit carries strata) and these
columns:

- `event_date`:

  `Date`. The event/reference date, **on the engine's own grid**.
  `tidy()` deliberately does not re-grid: some packages bin onto week
  starts of their own choosing, and silently snapping them would hide a
  real difference. Align afterwards if you need to.

- `stratum`:

  `character`. One label per stratum the fit reports, and `"all"` when
  the fit is unstratified. Several stratifying columns are pasted
  `" | "`-separated, matching the `triangle_list` naming of
  [`tbl_now_to_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_baselinenowcast.md).
  `(stratum, event_date)` is therefore a unique key.

- `estimate`:

  `numeric`. The point nowcast – the posterior median where the engine
  provides draws or a median, otherwise its point estimate.

- `conf.low`, `conf.high`:

  `numeric`. Interval bounds, following broom's naming. `NA` when the
  engine returns no interval.

- `level`:

  `numeric`. The width the interval actually has, e.g. `0.95`. Engines
  differ – epinowcast reports a 90% band by default while others report
  95% – and without this column those get compared as if they were the
  same thing. `NA` whenever the width cannot be established: because the
  engine returned no interval (a baselinenowcast fit made with
  `output_type = "point"`), or because it returned one without saying
  how wide it is. NobBS is the latter case – its `lower`/`upper` come
  from `specs$conf`, and `NobBS()` does not return `specs` – so pass
  `level` yourself if you need it filled in. A guessed default is worse
  than `NA` in the one column that exists to stop widths being compared
  blindly.

- `engine`:

  `character`. Which package produced the fit.

When `probs` is supplied, one extra column per requested quantile is
appended, named `q5`, `q50`, `q95` and so on (the probability times 100,
so `0.025` becomes `q2.5`).

## Which engines can honour `probs`

Only the engines that expose **draws** can compute an arbitrary
quantile: diseasenowcasting, baselinenowcast and epinowcast. The others
report a fixed set of summaries and nothing else, so asking them for a
quantile they did not compute is an error rather than a silent
approximation.

## Supported objects

- `nowcast_prediction` (S7) from `diseasenowcasting::predict()`

- `baselinenowcast_df` from
  [`baselinenowcast::baselinenowcast()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.html)

- `epinowcast` fits

- `stsNC` from
  [`surveillance::nowcast()`](https://rdrr.io/pkg/surveillance/man/nowcast.html)

- the list returned by
  [`NobBS::NobBS()`](https://rdrr.io/pkg/NobBS/man/NobBS.html) or by
  [`NobBS::NobBS.strat()`](https://rdrr.io/pkg/NobBS/man/NobBS.strat.html)
  (the stratified variant is recognised by its `stratum` column)

- a **list of `baselinenowcast_df` fits**, one per stratum – what
  [`lapply()`](https://rdrr.io/r/base/lapply.html)-ing over a
  [tbl_now_triangle_list](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_triangle_list.md)
  produces. Each element is tidied and labelled with its list name,
  giving the same one-block-per-stratum table the natively stratified
  engines return.

## Examples

``` r
data(denguedat)
# A few years of data and a small number of draws, to keep the example quick.
dengue <- tbl_now(denguedat[1:10000, ],
  event_date = "onset_week", report_date = "report_week", verbose = FALSE
)
triangle <- suppressWarnings(
  tbl_now_to_baselinenowcast(dengue, verbose = FALSE)
)
#> ℹ Using max_delay = 15 from data
fit <- baselinenowcast::baselinenowcast(
  triangle, output_type = "samples", draws = 25
)
#> ℹ 0.5 reference times were specified for delay estimation but 0.489 of reference times used for delay estimation.
#> ℹ `prop_delay` not identical to the proportion of reference times used for delay estimation due to rounding.
tidy(fit)
#> # A tibble: 191 × 7
#>    event_date stratum estimate conf.low conf.high level engine         
#>    <date>     <chr>      <dbl>    <dbl>     <dbl> <dbl> <chr>          
#>  1 1990-01-01 all           61       61        61  0.95 baselinenowcast
#>  2 1990-01-08 all           50       50        50  0.95 baselinenowcast
#>  3 1990-01-15 all           44       44        44  0.95 baselinenowcast
#>  4 1990-01-22 all           46       46        46  0.95 baselinenowcast
#>  5 1990-01-29 all           39       39        39  0.95 baselinenowcast
#>  6 1990-02-05 all           34       34        34  0.95 baselinenowcast
#>  7 1990-02-12 all           24       24        24  0.95 baselinenowcast
#>  8 1990-02-19 all           17       17        17  0.95 baselinenowcast
#>  9 1990-02-26 all           17       17        17  0.95 baselinenowcast
#> 10 1990-03-05 all           16       16        16  0.95 baselinenowcast
#> # ℹ 181 more rows
```
