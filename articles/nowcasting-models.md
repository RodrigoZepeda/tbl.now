# One dataset, many nowcasts: using tbl.now with different modelling packages

## Why this vignette?

Preparing the same data different ways for the different nowcasting
models can be tedious and error-prone. This is exactly what
[`tbl.now`](https://rodrigozepeda.github.io/tbl.now/) helps with. You
can describe your data once by specifying which column is the event
date, which is the report date, whether your data is linelist or has
counts (and what *those* counts mean!) and `tbl.now` will hands it to
each modelling package in the format that package expects.

> In this vignette we take a single dataset (`covid_colombia`), and from
> that one object, use several different nowcasting / delay-estimation
> tools:

| Package | What it does | Additional requirements | `tbl.now` converter |
|----|----|----|----|
| [diseasenowcasting](https://rodrigozepeda.github.io/diseasenowcasting/) | flexible Bayesian nowcast (delay + epidemic processes) | none (uses `RTMB`) | consumes a `tbl_now` **directly** |
| [baselinenowcast](https://baselinenowcast.epinowcast.org/) | fast, assumption-light baseline nowcast | none | [`tbl_now_to_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_baselinenowcast.md) |
| [epinowcast](https://package.epinowcast.org/) | flexible Bayesian nowcast (delay + reference modules) | **Stan** | [`tbl_now_to_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md) |
| [epidist](https://epidist.epinowcast.org/) | estimates only the reporting **delay distribution** | **Stan** | [`tbl_now_to_epidist()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epidist.md) |
| [NobBS](https://cran.r-project.org/package=NobBS) | Nowcasting by Bayesian Smoothing | **JAGS** | [`tbl_now_to_nobbs()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_nobbs.md) |
| [surveillance](https://cran.r-project.org/package=surveillance) | the classic Höhle & an der Heiden nowcast | none for the method used here (**JAGS** for `bayes.trunc.ddcp`) | [`tbl_now_to_surveillance()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_surveillance.md) |
| [EpiNow2](https://epiforecasts.io/EpiNow2/) | renewal-equation R_t, reporting truncation, and delay distributions | **Stan** (`cmdstanr`) | [`tbl_now_to_EpiNow2()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_EpiNow2.md) |

You do not need to be an expert in any of them to follow along, the
point is to show how little changes on *your* side when you switch
models.

**Each of these is a separate package, and `tbl.now` does not install
any of them.** It only knows how to *talk* to them. Install whichever
you actually want to use — and note that some also need software outside
R (the *Additional requirements* column above): **Stan** via `cmdstanr`
(needed by , and ) and **JAGS** as a standalone program (needed by ).

``` r

# CRAN packges
install.packages(c("baselinenowcast", "EpiNow2", "NobBS", "surveillance"))

# Not on CRAN:
install.packages("cmdstanr",   repos = c('https://stan-dev.r-universe.dev', getOption("repos")))
install.packages("epidist",    repos = "https://epinowcast.r-universe.dev")
install.packages("epinowcast", repos = "https://epinowcast.r-universe.dev")

# Packages from Github:
install.packages("pak") 
pak::pkg_install("RodrigoZepeda/diseasenowcasting")
```

Every code block below is guarded, so the article still builds when a
package is missing — but nothing will run on *your* machine until you
install it.

## The data

`tbl.now` ships with `covid_colombia`, **daily** COVID-19 case counts
from Colombia’s national surveillance system (INS), 2020–2023. Each row
is a `(notification_date, diagnosis_date, sex)` combination with a case
count `n`: the date the case was notified, and the date the laboratory
diagnosis was registered. The gap between the two is the reporting
delay.

``` r

library(dplyr)
library(tbl.now)

data(covid_colombia)
```

    #>   notification_date diagnosis_date    sex n
    #> 1        2020-03-02     2020-03-06 Female 1
    #> 2        2020-03-03     2020-03-14 Female 1
    #> 3        2020-03-06     2020-03-09   Male 1
    #> 4        2020-03-07     2020-03-09 Female 1
    #> 5        2020-03-08     2020-03-11 Female 2
    #> 6        2020-03-09     2020-03-11 Female 1

We build a single `tbl_now`. We say which column is the event date
(`notification_date`), which is the report date (`diagnosis_date`) and
which holds the counts (`n`); `tbl.now` infers the rest — that the grid
is daily, and that the “now” is the last report date.

We cut **both** dates at the start of March 2023. The example assumes we
are back on that date, nowcasting with only the information available
then (`now = "2023-03-02"`).

`covid_colombia` is **daily** and already **aggregated**: one row per
`(notification_date, diagnosis_date, sex)` with a case count `n`. That
makes it `count-incidence` data rather than a line list. It also means a
*pooled* object has to sum over `sex` first — leaving `sex` in the data
without declaring it as a stratum would put two rows in every
`(event, report)` cell, and a reporting triangle has one slot per cell.

``` r

cutoff <- as.Date("2023-03-03")

covid <- covid_colombia |>
  filter(notification_date < cutoff, diagnosis_date < cutoff)

covid_now <- covid |>
  group_by(notification_date, diagnosis_date) |>
  summarise(n = sum(n), .groups = "drop") |>
  tbl_now(
    event_date  = notification_date,
    report_date = diagnosis_date,
    case_count  = n,
    data_type   = "count-incidence"
  )

covid_now
#> # A tibble:  19,816 × 6
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>    notification_date diagnosis_date       n .event_num .report_num .delay
#>    <date>            <date>           <int>      <dbl>       <dbl>  <dbl>
#>    [event_date]      [report_date]  [cases]      [...]       [...]  [...]
#>  1 2020-03-02        2020-03-06           1          0           4      4
#>  2 2020-03-03        2020-03-14           1          1          12     11
#>  3 2020-03-06        2020-03-09           1          4           7      3
#>  4 2020-03-07        2020-03-09           1          5           7      2
#>  5 2020-03-08        2020-03-11           2          6           9      3
#>  6 2020-03-09        2020-03-11           3          7           9      2
#>  7 2020-03-10        2020-03-11           1          8           9      1
#>  8 2020-03-10        2020-03-12           2          8          10      2
#>  9 2020-03-10        2020-03-13           1          8          11      3
#> 10 2020-03-11        2020-03-12           1          9          10      1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2023-03-02 | Event date: "notification_date" | Report date:
#> # "diagnosis_date"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 19,806 more rows
```

That is 6,120,923 cases over 1094 days, from March 2020 to 2023-03-02.
Everything in this article — every fit, every figure — is built from
this one object, so following along step by step reproduces exactly what
you see.

**A note on series length**, with the measured cost of each choice on
this 1,094-day series:

- `diseasenowcasting` takes the **whole** series untrimmed (33s).
  Nothing is dropped.
- `baselinenowcast` also keeps every day, but the **delays** are capped
  at 30 days (55s). That is a modelling assumption, not a workaround:
  99% of reports arrive within 15 days, and a single 330-day straggler
  otherwise makes the triangle 1094 × 331 and the fit take 314s.
- `NobBS` and `surveillance` need the series shortened, and each has its
  own argument for it: `moving_window` and `when`. They also need a
  **line list**, so the counts are expanded to one row per case — after
  trimming, because the whole series is 6.1M cases.
- `epinowcast` has no such argument, because the reporting triangle is
  already built by the time you hold a preprocessed object. It is
  trimmed one step earlier, in its own section.
- `epidist` is the awkward one here and gets its own note in that
  section.

### A more complex object: strata and temporal effects

Reporting delays are rarely constant and many series are reported for
several **strata** (here, `sex`) that you may want to nowcast
separately. Here we build a second object, `covid_seasonal`, that
declares `sex` as a stratum and adds day-of-week plus annual-Fourier
**temporal (delay) effects**. Daily data makes day-of-week the effect
worth modelling: only 9.6% of these cases are notified on a Sunday
against 16.1% on a Tuesday.

``` r

covid_seasonal <- covid |>
  tbl_now(
    event_date  = notification_date,
    report_date = diagnosis_date,
    case_count  = n,
    strata      = sex,       # nowcast Male/Female separately
    data_type   = "count-incidence",
    verbose     = FALSE
  ) |>
  add_temporal_effects(
    temporal_effects(
      day_of_week = TRUE,    # a separate level per weekday
      seasons     = 365      # an annual Fourier cycle (period = 365 days)
    )
  )
```

For each package below we show the **bare** conversion of `covid_now`
first, then how the same call carries the strata and the temporal
effects when handed `covid_seasonal`.

**TIP**: Run
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
with `panels = "delay_seasonality"` or `"delay_calendar"` first to *see*
whether such effects are present before modelling them.

> **Checking each nowcast** Every section below ends with the same
> figure: three panels — the **total** series and the two **sex** strata
> — each showing what had been reported by `now` (grey bars), what those
> dates eventually reached (the dark **truth** line), and that package’s
> nowcast, with its uncertainty interval where the package provides one.

## diseasenowcasting

[`diseasenowcasting`](https://rodrigozepeda.github.io/diseasenowcasting/)
is designed hand-in-hand with `tbl.now`, so it takes a `tbl_now`
**directly**. You just hand it the object:

### Simple nowcast

``` r

# `diseasenowcasting::` rather than `library()`: it is not on CRAN, so attaching
# it in a vignette leaves `R CMD check` with an undeclared dependency. Qualifying
# the call also keeps this article's `tidy()` calls on the `generics` generic --
# versions before 2.1.0 declared their own, which masks every other package's
# methods with no error.
dnc_fit <- diseasenowcasting::nowcast(covid_now)

dnc_fit
```

    #> -- diseasenowcasting --------------------------------------- as of 2023-03-02 --
    #> Model: NegBin / HSGP / LogNormal
    #> two_stage (1096 event-times; 25 fits, rung 'multi')
    #> Use `predict()` / `autoplot()` for the nowcast, `coef()` / `summary()` for
    #> estimates.
    #> Call `print(nc@model)` for the full model spec (including priors).

### With strata and effects.

`diseasenowcasting` reaches into the `tbl_now` itself, so the enriched
object needs no extra arguments: it picks up the `sex` stratum and the
day-of-week / seasonal effect columns automatically.

``` r

diseasenowcasting::nowcast(covid_seasonal)   # strata and effects used automatically
```

    #> -- diseasenowcasting --------------------------------------- as of 2023-03-02 --
    #> Model: NegBin / HSGP / LogNormal
    #> two_stage (1096 event-times, 2 strata; 1 fit, rung 'onestage')
    #> Use `predict()` / `autoplot()` for the nowcast, `coef()` / `summary()` for
    #> estimates.
    #> Call `print(nc@model)` for the full model spec (including priors).

Predictions can be obtained via `tidy`:

``` r

tidy(diseasenowcasting::nowcast(covid_seasonal))
```

    #> # A tibble: 5 × 7
    #>   event_date stratum estimate conf.low conf.high level engine           
    #>   <date>     <chr>      <dbl>    <dbl>     <dbl> <dbl> <chr>            
    #> 1 2020-03-02 Female         1        1         1  0.95 diseasenowcasting
    #> 2 2020-03-03 Female         1        1         1  0.95 diseasenowcasting
    #> 3 2020-03-04 Female         0        0         0  0.95 diseasenowcasting
    #> 4 2020-03-05 Female         0        0         0  0.95 diseasenowcasting
    #> 5 2020-03-06 Female         0        0         0  0.95 diseasenowcasting

In both stratified and unstratified cases:

``` r

tidy(dnc_fit)
```

    #> # A tibble: 5 × 7
    #>   event_date stratum estimate conf.low conf.high level engine           
    #>   <date>     <chr>      <dbl>    <dbl>     <dbl> <dbl> <chr>            
    #> 1 2020-03-02 all            1        1         1  0.95 diseasenowcasting
    #> 2 2020-03-03 all            1        1         1  0.95 diseasenowcasting
    #> 3 2020-03-04 all            0        0         0  0.95 diseasenowcasting
    #> 4 2020-03-05 all            0        0         0  0.95 diseasenowcasting
    #> 5 2020-03-06 all            1        1         1  0.95 diseasenowcasting

**NOTE**: this needs **diseasenowcasting \>= 2.1.0**. Earlier versions
declared their own
[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
generic rather than re-exporting the shared one, so a bare
`tidy(dnc_fit)` returned that package’s *model-parameter* table (`term`,
`std.error`, …) instead of a nowcast — no error, just a different table.
On 2.1.0 and later,
[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
returns the nowcast and the parameter table moved to
`model_parameters()`. On an older version, use
`tbl.now::tidy(predict(dnc_fit))`.

A related hazard remains with : loading it overwrites `tbl.now`’s
[`tidy.list()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
method, which is what fits dispatch on. Qualify the call as
[`tbl.now::tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
there if is attached.

![Three panels (Total, Female, Male) comparing the diseasenowcasting
nowcast against the counts reported by now and the counts those dates
eventually
reached.](nowcasting-models_files/figure-html/dnc-panels-1.png)

Nowcast both stratified and total using the diseasenowcasting package

## baselinenowcast

[`baselinenowcast`](https://baselinenowcast.epinowcast.org/) is a
simple, fast baseline. It works from a **reporting triangle**, a matrix
with one row per event (reference) date and one column per reporting
delay. The lower-right corner of the matrix corresponds to the
not-yet-observed part the nowcast will fill in.

[`tbl_now_to_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_baselinenowcast.md)
builds that triangle directly from the count-incidence data:

### Simple nowcast

``` r

library(baselinenowcast)

# Cap the delays first: a single 330-day straggler would otherwise give the
# triangle 331 columns. 99% of reports arrive within 15 days.
covid_triangle <- covid_now |>
  filter(.delay <= 30) |>
  tbl_now_to_baselinenowcast(verbose = FALSE)

# rows = notification dates, columns = delay in days
covid_triangle[1:5, 1:6]
#>            0 1 2 3 4 5
#> 2020-03-02 0 0 0 0 1 0
#> 2020-03-03 0 0 0 0 0 0
#> 2020-03-06 0 0 0 1 0 0
#> 2020-03-07 0 0 1 0 0 0
#> 2020-03-08 0 0 0 2 0 0
```

**Why cap the delays?** It is a modelling assumption, not a workaround.
The triangle keeps every one of the 1094 event dates; only the delay
axis is bounded. Left uncapped the fit takes **314s** against **55s**,
for a tail that carries well under 1% of cases.
[`tbl_now_to_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md)
has a `max_delay` argument for the same purpose; for `baselinenowcast`
the cap is an ordinary [`filter()`](https://rdrr.io/r/stats/filter.html)
on the delay column.

From here you can follow `baselinenowcast`’s own workflow. For example
calling
[`baselinenowcast()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.html)
to estimate the delay from the triangle, apply it, and draw nowcast
samples:

``` r

# One-call workflow: estimate the delay, apply it, and draw nowcast samples.
nowcast_samples <- baselinenowcast(
  covid_triangle,
  output_type = "samples",
  draws       = 1000
)
```

### With strata and effects.

A reporting-triangle *matrix* is a single table with no room for strata
or covariates, so `format = "matrix"` pools the strata (with a warning)
and drops the effect columns. To nowcast each stratum we need to do it
separately via a `format = "triangle_list"`. This creates one
reporting-triangle per stratum.

``` r

# One reporting triangle per stratum, straight from the object.
triangles_by_stratum <- tbl_now_to_baselinenowcast(
  covid_seasonal,
  format  = "triangle_list",
  verbose = FALSE
)
```

``` r

triangles_by_stratum
#> ── 2 reporting triangles from a <tbl_now> ──────────────────────────────────────
#> • One per stratum ("sex"): "Female" and "Male"
#> • Delays unit: "days"
#> • Now: "2023-03-02"
#> • Dimensions (event x delay): "1093 x 331" and "1090 x 325"
#> ℹ This is one triangle per STRATUM. `baselinenowcast::estimate_and_apply_delays()` expects retrospective snapshots of a single series instead -- do not pass this object to it.
```

This can be used to nowcast each stratum via `lapply`:

``` r

# One nowcast per triangle, each trimmed the same way.
nowcasts_by_stratum <- triangles_by_stratum |>
  lapply(\(tri) baselinenowcast(tri, output_type = "samples", draws = 1000))
```

In both stratified and unstratified cases the predictions can be
recovered with `tidy`:

``` r

tidy(nowcast_samples)
```

    #> # A tibble: 5 × 7
    #>   event_date stratum estimate conf.low conf.high level engine         
    #>   <date>     <chr>      <dbl>    <dbl>     <dbl> <dbl> <chr>          
    #> 1 2020-03-02 all            1        1         1  0.95 baselinenowcast
    #> 2 2020-03-03 all            1        1         1  0.95 baselinenowcast
    #> 3 2020-03-06 all            1        1         1  0.95 baselinenowcast
    #> 4 2020-03-07 all            1        1         1  0.95 baselinenowcast
    #> 5 2020-03-08 all            2        2         2  0.95 baselinenowcast

![Three panels (Total, Female, Male) comparing the baselinenowcast
nowcast against the counts reported by now and the counts those dates
eventually
reached.](nowcasting-models_files/figure-html/bln-panels-1.png)

Nowcast both stratified and total using the baselinenowcast package

## epinowcast

[`epinowcast`](https://package.epinowcast.org/) fits a flexible Bayesian
model with separate modules for the reporting delay and the reference
(epidemic) process. It expects a preprocessed object built by
[`enw_preprocess_data()`](https://package.epinowcast.org/reference/enw_preprocess_data.html).

[`tbl_now_to_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md)
handles the preprocessing, returning an object you can pass straight to
[`epinowcast::epinowcast()`](https://package.epinowcast.org/reference/epinowcast.html):

**Series length: trimmed, before preprocessing.** `epinowcast` is the
one engine here with no argument for shortening the series, because by
the time you hold a preprocessed object the reporting triangle is
already built. Its filters work one step earlier, on the *observations*
— and
[`tbl_now_to_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md)
will hand you those instead of the finished object if you ask with
`preprocess = FALSE`:

**Warning: requires Stan.** This package fits its model with **Stan**,
so it needs a working `cmdstanr` (or `rstan`) toolchain installed before
any of the code below will run.

### Simple nowcast

``` r

library(epinowcast)

# Trim FIRST, then convert: two years of history is plenty and keeps the Stan
# fit tractable.
covid_enw_recent <- covid_now |>
  filter(notification_date >= cutoff - 7 * 104) |>
  tbl_now_to_epinowcast(max_delay = 10, verbose = FALSE, quiet = TRUE)

covid_enw_recent
#> ── Preprocessed nowcast data ─────────────────────────────────────────────────── 
#> Groups: 1 | Timestep: day | Max delay: 10 
#> Observations: 728 timepoints x 728 snapshots 
#> Max date: 2023-03-02 
#> 
#> Datasets (access with `enw_get_data(x, "<name>")`): 
#>   obs                :   7,235 x 7 
#>   new_confirm        :   7,235 x 9 
#>   latest             :     728 x 8 
#>   missing_reference  :       0 x 4 
#>   reporting_triangle :     728 x 12 
#>   metareference      :     728 x 7 
#>   metareport         :     737 x 10 
#>   metadelay          :      10 x 5
```

Two years preprocesses in well under a second, and it is
`covid_enw_recent` — not the full series — that goes to the Stan fit.

This can then be passed to
[`epinowcast()`](https://package.epinowcast.org/reference/epinowcast.html):

``` r

# A minimal epinowcast fit from the preprocessed object
fit <- epinowcast(
  covid_enw_recent,   # the trimmed object, not the full series
  fit = enw_fit_opts(
    pp = TRUE, chains = 2, iter_sampling = 500, iter_warmup = 500
  )
)
```

`max_delay` above is a **modelling choice, not a detail**: left unset,
the converter infers it from the longest delay present (330 days here),
and the nowcast then carries one reference date per delay. Setting it to
10 matches the maximum delay every other engine on this page is given,
so the comparison at the end is like for like.

### With strata and effects.

Handing
[`tbl_now_to_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md)
the enriched object does two things automatically: the `sex` stratum
becomes epinowcast’s grouping (`by`), so the model fits a delay per sex,
and the temporal-effect columns land in the `metareference` /
`metareport` tables so that they can be used in a module formula.

``` r

# Same trim, from the enriched object.
enw_seasonal <- covid_seasonal |>
  filter(notification_date >= cutoff - 7 * 104) |>
  tbl_now_to_epinowcast(max_delay = 10, verbose = FALSE, quiet = TRUE)

# Drop the seasonal terms into a reference-module formula and fit as usual: the
# covariates now enter the reference model.
epinowcast(
  enw_seasonal,
  reference = enw_reference(
    parametric   = ~ 1 + .event_season_52_sin + .event_season_52_cos,
    distribution = "lognormal",
    data         = enw_seasonal
  ),
  fit = enw_fit_opts(
    pp = TRUE, chains = 2, iter_sampling = 500, iter_warmup = 500
  )
)
```

In both stratified and unstratified cases the predictions can be
recovered with `tidy`:

``` r

tidy(fit)
```

    #> # A tibble: 5 × 7
    #>   event_date stratum estimate conf.low conf.high level engine    
    #>   <date>     <chr>      <dbl>    <dbl>     <dbl> <dbl> <chr>     
    #> 1 2023-02-01 all          123      123       123   0.9 epinowcast
    #> 2 2023-02-02 all          104      104       104   0.9 epinowcast
    #> 3 2023-02-03 all          118      118       118   0.9 epinowcast
    #> 4 2023-02-04 all          103      103       103   0.9 epinowcast
    #> 5 2023-02-05 all           69       69        70   0.9 epinowcast

![Three panels (Total, Female, Male) comparing the epinowcast nowcast
against the counts reported by now and the counts those dates eventually
reached.](nowcasting-models_files/figure-html/enw-panels-1.png)

Nowcast both stratified and total using the epinowcast package

## epidist

Sometimes the quantity you actually want is the **delay distribution**
itself — how long, on average, between onset and report, and how
variable is it? [`epidist`](https://epidist.epinowcast.org/) estimates
exactly that, treating each case as an interval-censored onset/report
pair.

[`tbl_now_to_epidist()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epidist.md)
converts the `tbl_now` into the censored form `epidist` expects. Our
data are daily counts, so the converter produces
`epidist_aggregate_data` — one row per distinct
`(delay, observation time)` combination with a weight — rather than one
row per case:

**Warning: requires Stan.** This package fits its model with **Stan**,
so it needs a working `cmdstanr` (or `rstan`) toolchain installed before
any of the code below will run.

### Simple delay fit

``` r

library(epidist)

covid_epidist <- tbl_now_to_epidist(covid_now, verbose = FALSE)
```

`epidist` offers several model types, and on **count** data the choice
matters more than it looks. The **marginal** model is the one built for
aggregated counts — but it does not currently compile (see the warning
below). The **latent** model works, at a price: it carries one latent
variable per *case*, so handing it the whole series expands 6.1M cases
into 6.1M rows and it never finishes. It is therefore fitted on the last
30 days (2,705 cases, about seven minutes). A delay distribution needs
delays, not a long series.

``` r

# Fit the delay distribution (see the epidist documentation for model choices)
delay_model <- covid_now |>
  filter(notification_date >= cutoff - 30) |>
  tbl_now_to_epidist(verbose = FALSE) |>
  as_epidist_latent_model() |>
  epidist()
```

**The `marginal` model does not currently fit, and it is the one you
would want here.** With **epidist 0.4.0** and **primarycensored 1.5.1**,
[`as_epidist_marginal_model()`](https://epidist.epinowcast.org/reference/as_epidist_marginal_model.html)
fails at Stan compilation: the generated code calls
`primarycensored_lpmf()` with 8 arguments against a 9-argument signature
(primarycensored gained an `L` lower-truncation argument that the
generated call does not supply). That matters more for count data than
for a line list, because the marginal model is what consumes aggregated
counts efficiently — without it, the latent model has to expand them
back to one row per case. Reported upstream; check the [epidist issue
tracker](https://github.com/epinowcast/epidist/issues/) before reaching
for
[`as_epidist_marginal_model()`](https://epidist.epinowcast.org/reference/as_epidist_marginal_model.html).

The fitted delay distribution can then feed back into a nowcast for
example as a data-informed prior in `epinowcast`.

### With strata and effects.

`epidist` has no separate grouping argument, so the strata (`sex`) and
the temporal-effect columns become extra columns that can be used within
the model’s formula. For example:

``` r

covid_epidist_eff <- tbl_now_to_epidist(covid_seasonal, verbose = FALSE)
```

``` r

# A sex-varying mean delay.
delay_by_sex <- covid_seasonal |>
  filter(notification_date >= cutoff - 30) |>
  tbl_now_to_epidist(verbose = FALSE) |>
  as_epidist_latent_model() |>
  epidist(formula = mu ~ 1 + sex)
```

[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
works here too, but it returns a **different table**, because `epidist`
estimates a different thing. There are no per-date case estimates to
report, so instead of one row per event date you get one row per
parameter of the fitted delay distribution:

``` r

tidy(delay_model)
```

| column                  | meaning                                      |
|-------------------------|----------------------------------------------|
| `term`                  | a parameter of the fitted delay distribution |
| `estimate`              | posterior median                             |
| `conf.low`, `conf.high` | interval bounds, using ’s names              |
| `level`                 | the width of that interval                   |
| `engine`                | always `"epidist"`                           |

The `term` values are the distribution’s own parameters (`mu`, `sigma`,
or whichever the `family` has) **plus** the derived `mean` and `sd` —
the delay summaries those parameters imply, which are usually the
numbers you actually want to quote. The fit keeps its draws, so `probs`
works as it does elsewhere: `tidy(delay_model, probs = c(0.05, 0.95))`
adds `q5` and `q95` columns.

**Watch the dispatch.**
[`epidist()`](https://epidist.epinowcast.org/reference/epidist.html)
returns an object whose class is `c("brmsfit", "epidist_fit")`, in that
order. If is loaded, its `tidy.brmsfit()` method matches **first** and
you get raw parameters instead of the delay table above. Call
`tidy.epidist_fit(fit)` explicitly when you cannot be sure which method
wins.

The natural use of the result here is as a **data-informed delay prior**
for one of the other engines.

### A caveat on this dataset: same-day reporting

Every other section ends with the nowcast plotted against the truth. The
equivalent check here would be the fitted delay distribution against the
delays actually observed — and on `covid_colombia` that check **fails**,
for a reason worth understanding before you reach for a delay model on
your own data.

**57.4%** of the cases in the fitting window carry a delay of exactly
**zero** days: notification and laboratory diagnosis are recorded on the
same date. Another 20% arrive the next day. A lognormal — `epidist`’s
default family, and the usual choice for a delay — has no way to
represent a point mass at zero, because `log(0)` is undefined. Asked to
fit one anyway, it does not fail loudly; it inflates the variance until
the density piles up near zero, and reports:

| term    | estimate        |
|---------|-----------------|
| `mu`    | 7.36            |
| `sigma` | **17.95**       |
| `mean`  | 1.5 × 10⁷³ days |

A mean delay of 10⁷³ days is not a convergence warning you can tune
away. It is the model telling you that the family cannot describe the
data.

**What to do instead.** If same-day reporting dominates your series, a
continuous positive-support distribution is the wrong tool. The options
are to model the delay as **discrete** (a distribution that puts real
mass on 0), to fit a **zero-inflated** or hurdle form that separates
“reported same day” from “how long otherwise”, or to keep the continuous
fit for the *non-zero* delays only and report the zero share separately.
`epidist` also offers
[`Gamma()`](https://epiforecasts.io/EpiNow2/reference/Distributions.html)
and
[`weibull()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
but neither solves this: both have zero density at zero too.

This is a property of the data, not of `tbl.now` or `epidist` — the
converter does its job, and the diagnosis above comes straight from
[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md).

## NobBS

[`NobBS`](https://cran.r-project.org/package=NobBS) works from a **line
list** with an onset-date column and a report-date column, and it counts
**rows** — each row is one case. Our data are counts, so they have to be
expanded first.
[`tbl_now_to_nobbs()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_nobbs.md)
does that and names the columns what
[`NobBS()`](https://rdrr.io/pkg/NobBS/man/NobBS.html) expects:

**Warning: requires JAGS.** This package fits its model with **JAGS**,
which is a separate program: install JAGS itself, not just the R
package, before running the code below.

### Simple nowcast

``` r

# Trim BEFORE converting: the expansion is one row per case, and `moving_window`
# below limits only what NobBS fits, not what it is handed.
covid_linelist <- covid_now |>
  filter(notification_date >= cutoff - 180) |>
  tbl_now_to_nobbs(verbose = FALSE)
#> Warning: `tbl_now_to_nobbs()` needs a line list; expanding "count-incidence" counts in
#> "n" to one row per case.

nrow(covid_linelist)   # one row per case
#> [1] 50160
```

**Do not hand [`NobBS()`](https://rdrr.io/pkg/NobBS/man/NobBS.html)
count data directly.** It counts rows, so a table of 1,174 rows carrying
50,160 cases is nowcast as **1,174 cases** — no error, just an answer
that is 40 times too small. The converter exists to make that
impossible.

``` r

library(NobBS)

nobbs_fit <- NobBS(
  data          = covid_linelist,
  now           = get_now(covid_now),
  units         = "1 day",
  onset_date    = "onset_date",
  report_date   = "report_date",
  max_D         = 30,   # delays beyond 30 days are negligible here
  moving_window = 180   # fit to the last six months
)
```

The predictions come out with
[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md),
in the same shape as every other engine here:

``` r

tidy(nobbs_fit)
```

    #> # A tibble: 5 × 7
    #>   event_date stratum estimate conf.low conf.high level engine
    #>   <date>     <chr>      <dbl>    <dbl>     <dbl> <dbl> <chr> 
    #> 1 2022-09-04 all           81       81        81  0.95 NobBS 
    #> 2 2022-09-05 all          158      158       158  0.95 NobBS 
    #> 3 2022-09-06 all          147      147       147  0.95 NobBS 
    #> 4 2022-09-07 all          130      130       130  0.95 NobBS 
    #> 5 2022-09-08 all          148      148       148  0.95 NobBS

Notice that even the arguments come from the `tbl_now`:
[`get_now()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md),
[`get_event_date()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
and
[`get_report_date()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
tell `NobBS` what `tbl.now` already figured out.

### With strata and effects.

Call [`NobBS.strat()`](https://rdrr.io/pkg/NobBS/man/NobBS.strat.html)
and pass the column name to the strata option:

``` r

dengue_nobbs     <- as.data.frame(covid_seasonal)
stratified_nobbs <- NobBS.strat(dengue_nobbs,
                                strata        = "sex",
                                now           = get_now(covid_seasonal),
                                units         = "1 day",
                                onset_date    = get_event_date(covid_seasonal),
                                report_date   = get_report_date(covid_seasonal),
                                max_D         = 10,
                                moving_window = 104
                                )
```

In both stratified and unstratified cases the predictions can be
recovered with `tidy`:

![Three panels (Total, Female, Male) comparing the NobBS nowcast against
the counts reported by now and the counts those dates eventually
reached.](nowcasting-models_files/figure-html/nobbs-panels-1.png)

Nowcast both stratified and total using the NobBS package

## surveillance

[`surveillance`](https://cran.r-project.org/package=surveillance) is the
long-standing R package for outbreak detection and nowcasting, and
implements the Höhle & an der Heiden (2014) approach.
[`surveillance::nowcast()`](https://rdrr.io/pkg/surveillance/man/nowcast.html)
works from an individual-level **line list** with one column for the
event date and one for the report date, named by its `dEventCol` /
`dReportCol` arguments.

[`tbl_now_to_surveillance()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_surveillance.md)
builds that data frame and renames the two dates to `surveillance`’s own
defaults:

### Simple nowcast

``` r

library(surveillance)

# Trim before converting: the converter expands counts to one row per case.
covid_sur <- covid_now |>
  filter(notification_date >= cutoff - 180) |>
  tbl_now_to_surveillance(verbose = FALSE)
#> Warning: `tbl_now_to_surveillance()` needs a line list; expanding "count-incidence"
#> counts in "n" to one row per case.

head(covid_sur)
#>    dHospital    dReport
#> 1 2022-09-04 2022-09-04
#> 2 2022-09-04 2022-09-04
#> 3 2022-09-04 2022-09-04
#> 4 2022-09-04 2022-09-04
#> 5 2022-09-04 2022-09-04
#> 6 2022-09-04 2022-09-04
```

The nowcast itself needs a `now`, the dates you want estimated (`when`),
and a maximum delay `D`. All of it can come from the `tbl_now`:

``` r

sur_now  <- get_now(covid_now)
sur_when <- seq(sur_now - 30, sur_now, by = "1 day")   # days to estimate

# `dRange` gives surveillance the grid explicitly, so it reaches the `now` even
# if the most recent days carry no reports yet: a line list cannot express a
# zero day (no rows), so zero-completion is no help here.
sur_range <- seq(min(covid_sur$dHospital), sur_now, by = "1 day")

sur_fit <- nowcast(
  now          = sur_now,
  when         = sur_when,
  data         = covid_sur,
  dEventCol    = "dHospital",
  dReportCol   = "dReport",
  aggregate.by = "1 day",
  D            = 30,
  method       = "bayes.notrunc.bnb",
  control      = list(dRange = sur_range, N.tInf.max = 100000, nSamples = 1000)
)
```

The predictions come out with
[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md),
in the same shape as every other engine here:

``` r

tidy(sur_fit)
```

    #> # A tibble: 5 × 7
    #>   event_date stratum estimate conf.low conf.high level engine      
    #>   <date>     <chr>      <dbl>    <dbl>     <dbl> <dbl> <chr>       
    #> 1 2023-01-31 all          146      146       146  0.95 surveillance
    #> 2 2023-02-01 all          123      123       123  0.95 surveillance
    #> 3 2023-02-02 all          104      104       104  0.95 surveillance
    #> 4 2023-02-03 all          118      118       118  0.95 surveillance
    #> 5 2023-02-04 all          103      103       103  0.95 surveillance

**TIP**: the interval is already there.
[`surveillance::nowcast()`](https://rdrr.io/pkg/surveillance/man/nowcast.html)
stores a prediction interval in the returned object’s `pi` slot, at
whatever width `control$alpha` names (`0.05`, so 95%, by default), and
[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
reads it into `conf.low` / `conf.high`. The JAGS-backed `bayes.trunc`
and `bayes.trunc.ddcp` methods are worth reaching for when you want the
delay distribution modelled differently, **not** merely to get
uncertainty out of the fit.

### With strata and effects.

[`surveillance::nowcast()`](https://rdrr.io/pkg/surveillance/man/nowcast.html)
has **no strata argument** as it only models one series. The strata ride
along as ordinary columns, so fit one nowcast per stratum in a loop:

``` r

covid_sur_eff <- tbl_now_to_surveillance(covid_seasonal, verbose = FALSE)

# `sex` is kept as a column, so it can be split on.
head(covid_sur_eff)
#>    dHospital    dReport    sex .event_day_of_week .event_season_365_cos
#> 1 2020-03-02 2020-03-06 Female                  2             1.0000000
#> 2 2020-03-03 2020-03-14 Female                  3             0.9998518
#> 3 2020-03-06 2020-03-09   Male                  6             0.9976303
#> 4 2020-03-07 2020-03-09 Female                  7             0.9962982
#> 5 2020-03-08 2020-03-11 Female                  1             0.9946708
#> 6 2020-03-08 2020-03-11 Female                  1             0.9946708
#>   .event_season_365_sin
#> 1            0.00000000
#> 2            0.01721336
#> 3            0.06880243
#> 4            0.08596480
#> 5            0.10310170
#> 6            0.10310170
```

``` r

sur_by_stratum <- covid_sur_eff |>
  split(covid_sur_eff$sex) |>
  lapply(\(df) nowcast(
    now          = sur_now,
    when         = sur_when,
    data         = df,
    dEventCol    = "dHospital",
    dReportCol   = "dReport",
    aggregate.by = "1 week",
    D            = 10,
    method       = "bayes.notrunc.bnb",
    control      = list(
      dRange     = seq(min(df$dHospital), sur_now, by = "1 week"),
      N.tInf.max = 1000,
      nSamples   = 1000
    )
  ))
```

In both stratified and unstratified cases the predictions can be
recovered with `tidy`:

![Three panels (Total, Female, Male) comparing the surveillance nowcast
against the counts reported by now and the counts those dates eventually
reached.](nowcasting-models_files/figure-html/sur-panels-1.png)

Nowcast both stratified and total using the surveillance package

## EpiNow2

[`EpiNow2`](https://epiforecasts.io/EpiNow2/) is the odd one out here:
it is not a single nowcast model but **four entry points**, each taking
a different shape of data.
[`tbl_now_to_EpiNow2()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_EpiNow2.md)
therefore takes a `target` argument, named for the function the result
is passed to, so whatever it gives you can be handed over unchanged.

**Warning: requires Stan.** These models fit through `cmdstanr`, so a
working CmdStan installation is needed before running the code below.

| `target` | you get | for |
|----|----|----|
| `"estimate_infections"` (default) | `date` / `confirm` | [`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.html), [`epinow()`](https://epiforecasts.io/EpiNow2/reference/epinow.html) |
| `"regional_epinow"` | the same plus `region` | [`regional_epinow()`](https://epiforecasts.io/EpiNow2/reference/regional_epinow.html) |
| `"estimate_truncation"` | a list of snapshots | [`estimate_truncation()`](https://epiforecasts.io/EpiNow2/reference/estimate_truncation.html) |
| `"estimate_dist"` | interval-censored date columns | [`estimate_dist()`](https://epiforecasts.io/EpiNow2/reference/estimate_dist.html) |

**EpiNow2 models a *daily* process and has no `timestep`.** Handing it a
weekly series as one row per week is read as one row per **day** — no
error, just an epidemic seven times too fast. The converter lays
non-daily data on EpiNow2’s own daily grid using its `accumulate`
column, so you do not have to. Units coarser than a week are refused
rather than approximated.

### Simple nowcast

[`estimate_infections()`](https://epiforecasts.io/EpiNow2/reference/estimate_infections.html)
wants the series as known at the `now` — one row per day, with the
filler days marked:

``` r

covid_en2 <- covid_now |>
  filter(notification_date >= cutoff - 180) |>
  tbl_now_to_EpiNow2(verbose = FALSE, quiet = TRUE)

head(covid_en2)
#>         date confirm
#> 1 2022-09-04      81
#> 2 2022-09-05     158
#> 3 2022-09-06     147
#> 4 2022-09-07     130
#> 5 2022-09-08     148
#> 6 2022-09-09     159
```

``` r

library(EpiNow2)

epinow2_fit <- estimate_infections(
  covid_en2,
  generation_time = gt_opts(example_generation_time),
  delays          = delay_opts(example_incubation_period + example_reporting_delay),
  rt              = rt_opts(prior = LogNormal(mean = 2, sd = 0.1)),
  stan            = stan_opts(samples = 500, warmup = 250, chains = 2)
)
```

Two things about that call, so the numbers below are read for what they
are. **The delay distributions are EpiNow2’s own shipped examples** —
COVID-in-the-UK priors — not distributions fitted to these Colombian
data. That is the normal footing for a comparison of machinery rather
than of estimates, but it does mean the EpiNow2 curve answers a slightly
different question from the engines that learn their delay from the
reporting triangle in front of them. Fitting the delay from the data
instead is one
[`estimate_dist()`](https://epiforecasts.io/EpiNow2/reference/estimate_dist.html)
call away — see below. **Sampling is also lighter than the default**
(500 draws, 250 warmup, 2 chains), because this is much the slowest
engine here.

The predictions come out with
[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md),
in the same shape as every other engine here:

``` r

tidy(epinow2_fit)
```

    #> # A tibble: 5 × 7
    #>   event_date stratum estimate conf.low conf.high level engine 
    #>   <date>     <chr>      <dbl>    <dbl>     <dbl> <dbl> <chr>  
    #> 1 2022-09-03 all           60     41.9      83     0.9 EpiNow2
    #> 2 2022-09-04 all           49     31        71.0   0.9 EpiNow2
    #> 3 2022-09-05 all          102     70       143.    0.9 EpiNow2
    #> 4 2022-09-06 all          113     77       149     0.9 EpiNow2
    #> 5 2022-09-07 all          117     77.9     162     0.9 EpiNow2

[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
reads the interval width off the fit rather than assuming it: EpiNow2’s
`CrIs` is a user argument, so a model fitted with `CrIs = c(0.5, 0.95)`
reports `level = 0.95` and one fitted with the defaults reports
`level = 0.9`.

### The report dimension: `estimate_truncation()`

This is the one EpiNow2 model that uses the **report** dimension a
`tbl_now` exists to carry. It takes a list of snapshots — the series as
it looked at each of several report dates — which is exactly what the
object already knows:

``` r

covid_snapshots <- covid_now |>
  filter(notification_date >= cutoff - 180) |>
  tbl_now_to_EpiNow2(
    target = "estimate_truncation", snapshots = 5,
    verbose = FALSE, quiet = TRUE
  )

covid_snapshots
#> ── 5 reporting snapshots from a <tbl_now> ──────────────────────────────────────
#> • One per report date: "2023-02-26", "2023-02-27", "2023-02-28", "2023-03-01", and "2023-03-02"
#> • Rows each: 176, 177, 178, 179, and 180
#> • Now: "2023-03-02"
#> ℹ Pass this to `EpiNow2::estimate_truncation()`. `EpiNow2::estimate_secondary()` wants a single data frame of linked series instead -- not this.
```

``` r

truncation_fit <- estimate_truncation(covid_snapshots)
```

Because the snapshots carry their report dates, this is the one EpiNow2
shape that can be turned **back** into a `tbl_now` — differencing
consecutive snapshots recovers the incidence:

``` r

as_tbl_now(covid_snapshots)
#> # A tibble:  198 × 6
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>    notification_date diagnosis_date   count .event_num .report_num .delay
#>    <date>            <date>           <dbl>      <dbl>       <dbl>  <dbl>
#>    [event_date]      [report_date]  [cases]      [...]       [...]  [...]
#>  1 2022-09-04        2023-02-26          81          0         175    175
#>  2 2022-09-05        2023-02-26         158          1         175    174
#>  3 2022-09-06        2023-02-26         147          2         175    173
#>  4 2022-09-07        2023-02-26         130          3         175    172
#>  5 2022-09-08        2023-02-26         148          4         175    171
#>  6 2022-09-09        2023-02-26         159          5         175    170
#>  7 2022-09-10        2023-02-26         100          6         175    169
#>  8 2022-09-11        2023-02-26          51          7         175    168
#>  9 2022-09-12        2023-02-26         124          8         175    167
#> 10 2022-09-13        2023-02-26         108          9         175    166
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2023-03-02 | Event date: "notification_date" | Report date:
#> # "diagnosis_date"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 188 more rows
```

### The delay distribution: `estimate_dist()`

[`estimate_dist()`](https://epiforecasts.io/EpiNow2/reference/estimate_dist.html)
(new in EpiNow2 1.9.0) estimates a reporting-delay distribution,
accounting for double interval censoring and right truncation. It takes
the same schema as , so the two are directly comparable on the same
object:

``` r

covid_dist <- covid_now |>
  filter(notification_date >= cutoff - 180) |>
  tbl_now_to_EpiNow2(target = "estimate_dist", verbose = FALSE, quiet = TRUE)

head(covid_dist)
#>    pdate_lwr  pdate_upr  sdate_lwr  sdate_upr   obs_date  n
#> 1 2022-09-04 2022-09-05 2022-09-04 2022-09-05 2023-03-03 39
#> 2 2022-09-04 2022-09-05 2022-09-05 2022-09-06 2023-03-03 21
#> 3 2022-09-04 2022-09-05 2022-09-06 2022-09-07 2023-03-03  3
#> 4 2022-09-04 2022-09-05 2022-09-07 2022-09-08 2023-03-03  6
#> 5 2022-09-04 2022-09-05 2022-09-08 2022-09-09 2023-03-03  2
#> 6 2022-09-04 2022-09-05 2022-09-15 2022-09-16 2023-03-03 10
```

``` r

dist_fit <- estimate_dist(covid_dist, dist = "lognormal")
```

Like
[`tidy.epidist_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.epidist_fit.md),
this returns a **delay-shaped** table — one row per parameter, plus the
distribution’s `mean` and `sd`:

``` r

tidy(dist_fit)
```

    #> # A tibble: 4 × 6
    #>   term    estimate conf.low conf.high level engine 
    #>   <chr>      <dbl>    <dbl>     <dbl> <dbl> <chr>  
    #> 1 meanlog    -1.37    -1.40     -1.35  0.95 EpiNow2
    #> 2 sdlog       2.34     2.32      2.37  0.95 EpiNow2
    #> 3 mean        1.30     1.29      1.32  0.95 EpiNow2
    #> 4 sd          2.83     2.81      2.86  0.95 EpiNow2

### With strata and effects.

[`regional_epinow()`](https://epiforecasts.io/EpiNow2/reference/regional_epinow.html)
takes a single `region` column, so the object’s strata are folded into
one label:

``` r

covid_regional <- covid_seasonal |>
  filter(notification_date >= cutoff - 180) |>
  tbl_now_to_EpiNow2(target = "regional_epinow", verbose = FALSE, quiet = TRUE)

head(covid_regional)
#>         date confirm region
#> 1 2022-09-04      48 Female
#> 2 2022-09-04      33   Male
#> 3 2022-09-05      85 Female
#> 4 2022-09-05      73   Male
#> 5 2022-09-06      78 Female
#> 6 2022-09-06      69   Male
```

``` r

regional_fit <- regional_epinow(
  covid_regional,
  generation_time = gt_opts(example_generation_time),
  delays          = delay_opts(example_incubation_period + example_reporting_delay)
)
```

[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
on the result gives one block per region, so `stratum` stays a unique
key alongside `event_date`.

**Two shapes EpiNow2 has that `tbl.now` does not convert for.**
[`estimate_secondary()`](https://epiforecasts.io/EpiNow2/reference/estimate_secondary.html)
models *two* data streams against each other (cases and deaths, say) and
one `tbl_now` is one stream, so there is no honest mapping.
[`estimate_delay()`](https://epiforecasts.io/EpiNow2/reference/estimate_delay.html)
takes a bare vector of delays; EpiNow2’s own help now points at
[`estimate_dist()`](https://epiforecasts.io/EpiNow2/reference/estimate_dist.html)
instead, and it discards the censoring a `tbl_now` carries. If you want
it anyway it is `x$.delay`.

## Does this work on *your* data?

Everything above uses one daily, count-incidence dataset. `tbl.now`
ships six others with different shapes — daily and weekly, line-list and
count-incidence and count-cumulative — so the table below records what
actually happens when every converter, plus one representative nowcast,
is run against each of them. It is generated by
`data-raw/converter_matrix.R`, and it reports failures as failures.

| dataset | data_type | to_count(incidence) | baselinenowcast | epinowcast | epidist | surveillance | NobBS | data.table | tsibble | nowcast (dnc) |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| denguedat | linelist | ok | ok | ok | ok | ok | ok | ok | ok | ok |
| hai_bucaramanga | linelist | ok | ok | ok | ok | ok | ok | ok | ok | ok |
| covid_colombia | count-incidence | ok | ok | ok | ok | ok | ok | ok | ok | ok |
| covidat | count-incidence | ok | ok | ok | ok | ok | ok | ok | ok | ok |
| covid_us | count-incidence | ok | ok | ok | ok | ok | ok | ok | ok | ok |
| mpoxdat | count-incidence | ok | ok | ok | ok | ok | ok | ok | ok | ok |
| flusight | count-cumulative | ok | ok | ok | ok | ok | ok | ok | ok | fails |

Every converter against every shipped dataset {.table
style="width:100%;"}

| dataset | data_type | step | status | message |
|:---|:---|:---|:---|:---|
| flusight | count-cumulative | nowcast (dnc) | error | Joint fit failed to converge for all init attempts. |

What went wrong, and why {.table}

Two of these datasets needed fixing before they would convert at all,
and both problems are ones you will meet in your own data:

- **`mpoxdat` has a `race` column that must be declared.** Left as an
  undeclared extra column, the `(event_date, report_date)` pair is not
  unique — there is one row per race — and every target that needs a
  unique cell rejects it: a reporting triangle reported *832 duplicate
  pairs*, and `tsibble` refused for the same reason. `strata = race`
  fixes both. If a column genuinely is not a stratum, aggregate it away
  with
  [`to_count()`](https://rodrigozepeda.github.io/tbl.now/reference/to_count.md)
  instead.
- **`covidat` has rows registered *before* symptom onset.** A negative
  delay is not a delay, and `epidist` rejects it outright rather than
  guess. Filter them before converting.

A third needed fixing too, and it is the one most likely to catch you
out:

- **FluSight’s reporting weekday is not constant.** `target_end_date` is
  always a Saturday, but `as_of` lands on Saturdays *and* Wednesdays —
  so the delay between them is `0.571` weeks, not a whole number, and no
  reporting triangle can be built at all. That is an **alignment**
  problem masquerading as a cumulative-data problem.
  `align_weeks = TRUE` in
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
  snaps the reports onto a consistent weekday and the delays become
  integers.

**`count-cumulative` data does convert**, once its delays are whole
periods. De-accumulating a cumulative series produces *negative*
increments wherever a total was revised downward, and `baselinenowcast`
ships
[`preprocess_negative_values()`](https://baselinenowcast.epinowcast.org/reference/preprocess_negative_values.html)
for exactly that — it redistributes each negative back into earlier
delays.
[`tbl_now_to_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_baselinenowcast.md)
calls it for you and says so; pass `negatives = "error"` if you would
rather be stopped than have the correction applied silently.

What is left is a modelling limit rather than a conversion one:

- **`epidist` needs a delay it can censor, and counts of at least one.**
  It models an interval-censored onset-to-report pair; a cumulative
  series has no individual delays to censor, and its zero cells fail
  epidist’s `n >= 1` check.
- **Not every model converges on every dataset.** `diseasenowcasting`
  reports *“joint fit failed to converge for all init attempts”* on the
  cumulative FluSight series. The converter did its job; the model could
  not fit the shape it was given.

The honest summary: **every converter works on `linelist`,
`count-incidence` and `count-cumulative` data** — provided the delays
are whole periods and any undeclared strata are declared. Those two data
problems, not the converters, are what you will spend your time on. —\>

## The `tidy()` function

The converters normalize what goes *into* each package.
[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
normalizes what comes out. Every engine above returns something
different: a matrix of draws, an `stsNC` object, a Stan fit, an INLA
summary, a bare list. The
[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
turns any of them into the same table:

``` r

# A short window so this runs quickly; any fit from any section works the same.
recent_triangle <- covid_now |>
  filter(notification_date >= cutoff - 365 * 2) |>
  to_count(to = "count-incidence") |>
  complete_zeroes() |>
  tbl_now_to_baselinenowcast(verbose = FALSE)

bln_fit <- baselinenowcast(recent_triangle, output_type = "samples", draws = 500)

tidy(bln_fit) |> tail(4)
#> # A tibble: 4 × 7
#>   event_date stratum estimate conf.low conf.high level engine         
#>   <date>     <chr>      <dbl>    <dbl>     <dbl> <dbl> <chr>          
#> 1 2023-02-27 all          107     95        144.  0.95 baselinenowcast
#> 2 2023-02-28 all           91     78        127   0.95 baselinenowcast
#> 3 2023-03-01 all          118     97        167.  0.95 baselinenowcast
#> 4 2023-03-02 all           93     67.0      135.  0.95 baselinenowcast
```

The columns are the same whatever produced the fit:

| column | meaning |
|----|----|
| `event_date` | event/reference date, **on the engine’s own grid** |
| `stratum` | `"all"` when the fit is unstratified |
| `estimate` | point nowcast (posterior median where available) |
| `conf.low`, `conf.high` | interval bounds, using ’s names |
| `level` | the width that interval **actually** has |
| `engine` | which package produced it |
| `qXX` | (optional) quantile columns of the prediction |

Pass `probs` for columns with other quantiles:

``` r

tidy(bln_fit, probs = c(0.05, 0.5, 0.95)) |> tail(3)
#> # A tibble: 3 × 10
#>   event_date stratum estimate conf.low conf.high level engine     q5   q50   q95
#>   <date>     <chr>      <dbl>    <dbl>     <dbl> <dbl> <chr>   <dbl> <dbl> <dbl>
#> 1 2023-02-28 all           91     78        127   0.95 baseli…    79    91  122 
#> 2 2023-03-01 all          118     97        167.  0.95 baseli…    99   118  157.
#> 3 2023-03-02 all           93     67.0      135.  0.95 baseli…    71    93  129.
```

**Only engines that keep draws can answer an arbitrary `probs`.** That
is `diseasenowcasting`, `baselinenowcast` and `epinowcast`. `NobBS` and
`surveillance` report a fixed set of summaries, so asking them for a
quantile they never computed is an **error** rather than a silent
approximation.

### Asking the other engines for the quantiles you want

That error is about *when* the quantiles are computed, not about whether
you can have them. Several of these packages will happily compute any
quantile — you just have to **ask at fit time**, because once the fit
returns, the draws are gone and there is nothing left to re-summarise.

`NobBS` is the clearest example. Its `specs` argument takes a
`quantiles` element, and whatever you put there is computed alongside
the default summaries:

``` r

nobbs_quantiles <- NobBS(
  data          = covid_linelist,
  now           = get_now(covid_now),
  units         = "1 day",
  onset_date    = "onset_date",
  report_date   = "report_date",
  max_D         = 10,
  moving_window = 104,
  specs         = list(quantiles = c(0.1, 0.5, 0.9))   # <- ask here
)

tail(nobbs_quantiles$estimates, 5)
```

    #>     estimate lower upper q_0.1 q_0.5 q_0.9 onset_date n.reported
    #> 176       52    47    59    49    52    56 2023-02-26         44
    #> 177      109   102   118   104   109   115 2023-02-27         94
    #> 178       96    88   106    90    96   103 2023-02-28         77
    #> 179      121   110   134   114   121   129 2023-03-01         93
    #> 180       97    82   114    87    97   108 2023-03-02         55

The requested quantiles arrive as `q_0.1`, `q_0.5` and `q_0.9` columns
on `$estimates`, next to the usual `estimate` / `lower` / `upper`.
[`tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/tidy.nowcast.md)
still returns the standard six columns for such a fit — it does not
invent a schema per engine — so join the extra columns on by date when
you need them:

``` r

tidy(nobbs_quantiles) |>
  left_join(
    nobbs_quantiles$estimates |>
      select(event_date = onset_date, q_0.1, q_0.5, q_0.9),
    by = "event_date"
  ) |>
  tail(5)
```

    #> # A tibble: 5 × 10
    #>   event_date stratum estimate conf.low conf.high level engine q_0.1 q_0.5 q_0.9
    #>   <date>     <chr>      <dbl>    <dbl>     <dbl> <dbl> <chr>  <dbl> <dbl> <dbl>
    #> 1 2023-02-26 all           52       47        59  0.95 NobBS     49    52    56
    #> 2 2023-02-27 all          109      102       118  0.95 NobBS    104   109   115
    #> 3 2023-02-28 all           96       88       106  0.95 NobBS     90    96   103
    #> 4 2023-03-01 all          121      110       134  0.95 NobBS    114   121   129
    #> 5 2023-03-02 all           97       82       114  0.95 NobBS     87    97   108

The same idea applies elsewhere:
[`surveillance::nowcast()`](https://rdrr.io/pkg/surveillance/man/nowcast.html)
takes the interval width through `control$alpha`. Decide what you need
**before** you fit.

## Summary

![Three panels (Total, Female, Male). Grey bars show the counts reported
by now, a dark line the counts those dates eventually reached, and one
coloured line per package its
nowcast.](nowcasting-models_files/figure-html/comparison-all-1.png)

We described the dengue data once as a `tbl_now`, and then a single
converter call (or, for `diseasenowcasting`, no call at all) handed it
to each package in the shape it needed:

``` r

covid_now <- tbl_now(dengue, event_date = notification_date, report_date = diagnosis_date,
                      data_type = "linelist")

covid_now                              # diseasenowcasting
tbl_now_to_baselinenowcast(covid_now)  # baselinenowcast
tbl_now_to_epinowcast(covid_now)       # epinowcast
tbl_now_to_epidist(covid_now)          # epidist
tbl_now_to_surveillance(covid_now)     # surveillance
as.data.frame(covid_now)               # NobBS or others
```

Attaching **strata** and **temporal effects** once (`covid_seasonal`)
uses the same converters: each package receives them in whatever way it
can use a grouping in `epinowcast`, covariate columns in `epidist` and
the `baselinenowcast` long format, one triangle/series per stratum where
the model takes a single series, and automatically in
`diseasenowcasting`.

## Learning more

- Introduction vignette:
  <https://rodrigozepeda.github.io/tbl.now/articles/tbl.now.html> for
  the full anatomy of a `tbl_now`, data types, and temporal effects.
- End-to-end tutorial on real, messy surveillance data — cleaning,
  diagnostics and nowcasting:
  <https://rodrigozepeda.github.io/tbl.now/articles/Example.html>
- Tutorial on detecting batches and other reporting-delay artifacts:
  <https://rodrigozepeda.github.io/tbl.now/articles/batch-reporting.html>
- Comparing nowcasting engines on one dataset:
  <https://rodrigozepeda.github.io/tbl.now/articles/nowcasting-models.html>
- Package reference:
  <https://rodrigozepeda.github.io/tbl.now/reference/>
