# One dataset, many nowcasts: using tbl.now with five modelling packages

## Why this vignette?

Preparing the same outbreak data different ways can be tedious and
error-prone. This is exactly what
[`tbl.now`](https://rodrigozepeda.github.io/tbl.now/) does for you. You
describe your data once by specifying which column is the event date,
which is the report date, what the counts mean and `tbl.now` hands it to
each modelling package in the format that package expects. In this
vignette we take a single dengue line-list and, from that one object,
drive five different nowcasting / delay-estimation tools:

| Package | What it does | `tbl.now` bridge |
|----|----|----|
| [diseasenowcasting](https://rodrigozepeda.github.io/diseasenowcasting/) | Bayesian structural time-series nowcast | consumes a `tbl_now` **directly** |
| [baselinenowcast](https://baselinenowcast.epinowcast.org/) | fast, assumption-light baseline nowcast | [`tbl_now_to_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_baselinenowcast.md) |
| [epinowcast](https://package.epinowcast.org/) | flexible Bayesian nowcast (delay + reference modules) | [`tbl_now_to_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md) |
| [epidist](https://epidist.epinowcast.org/) | estimates the reporting **delay distribution** | [`tbl_now_to_epidist()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epidist.md) |
| [NobBS](https://cran.r-project.org/package=NobBS) | Nowcasting by Bayesian Smoothing | a plain line-list ([`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)) |

You do not need to be an expert in any of them to follow along — the
point is how little changes on *your* side when you switch models.

## The data: one `tbl_now` to rule them all

`tbl.now` ships with `denguedat`, a weekly dengue line-list (one row per
case) with the week of symptom **onset** and the week the case was
**reported**.

``` r

library(dplyr)
library(tbl.now)

data(denguedat)
```

    #>   onset_week report_week gender
    #> 1 1990-01-01  1990-01-01   Male
    #> 2 1990-01-01  1990-01-01 Female
    #> 3 1990-01-01  1990-01-01 Female
    #> 4 1990-01-01  1990-01-08 Female
    #> 5 1990-01-01  1990-01-08   Male
    #> 6 1990-01-01  1990-01-15 Female

We build a single `tbl_now`. We only have to say which column is the
event date (`onset_week`) and which is the report date (`report_week`);
`tbl.now` infers the rest (the data is line-list, measured in weeks, and
the “now” is the last report, `2010-12-20`).

``` r

# A compact recent window keeps the vignette fast to build.
dengue <- denguedat %>% 
  filter(onset_week >= as.Date("2009-01-01"))

dengue_now <- tbl_now(
  dengue,
  event_date  = onset_week,
  report_date = report_week,
  data_type   = "linelist"
  )

dengue_now
#> # A tibble:  9,268 × 6
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week   gender .event_num .report_num .delay
#>    <date>       <date>        <chr>       <dbl>       <dbl>  <dbl>
#>    [event_date] [report_date] [...]       [...]       [...]  [...]
#>  1 2009-01-05   2009-01-12    Male            0           1      1
#>  2 2009-01-05   2009-01-12    Male            0           1      1
#>  3 2009-01-05   2009-01-19    Female          0           2      2
#>  4 2009-01-05   2009-01-12    Male            0           1      1
#>  5 2009-01-05   2009-01-12    Female          0           1      1
#>  6 2009-01-05   2009-01-12    Female          0           1      1
#>  7 2009-01-05   2009-01-12    Female          0           1      1
#>  8 2009-01-05   2009-01-19    Female          0           2      2
#>  9 2009-01-05   2009-01-12    Male            0           1      1
#> 10 2009-01-05   2009-01-12    Female          0           1      1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "onset_week" | Report date: "report_week"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 9,258 more rows
```

### A more complex object: strata and temporal effects

Reporting delays are rarely constant and many series are reported for
several **strata** (here, the patient’s sex) that you may want to
nowcast separately.
[`tbl.now`](https://rodrigozepeda.github.io/tbl.now/) lets you attach
both **once** and carries them into whichever format can hold them. Here
we build a second object, `dengue_seasonal`, that adds a `gender`
stratum and a week-of-year plus annual-Fourier **temporal (delay)
effect** on top of `dengue_now`:

``` r

dengue_seasonal <- dengue_now |>
  add_strata(gender) |>      # nowcast Male/Female separately
  add_temporal_effects(
    temporal_effects(
      week_of_year = TRUE,   # a separate level per epidemiological week
      seasons      = 52      # an annual Fourier cycle (period = 52 weeks)
    )
  )
```

For each package below we show the **bare** conversion of `dengue_now`
first, then how the same call carries the strata and the effects when
handed `dengue_seasonal`. (Tip: run
[`autoplot()`](https://rodrigozepeda.github.io/tbl.now/reference/autoplot.tbl_now.html)
with `panels = "delay_seasonality"` or `"delay_calendar"` first to *see*
whether such effects are present before modelling them.)

## 1. diseasenowcasting — no conversion at all

[`diseasenowcasting`](https://rodrigozepeda.github.io/diseasenowcasting/)
was designed hand-in-hand with `tbl.now`, so it takes a `tbl_now`
**directly**. You just hand it the object:

``` r

library(diseasenowcasting)

dnc_fit <- nowcast(dengue_now)

dnc_fit
```

### With strata and effects.

`diseasenowcasting` reaches into the `tbl_now` itself, so the enriched
object needs no extra arguments: it picks up the `gender` stratum and
the week-of-year / seasonal effect columns automatically.

``` r

nowcast(dengue_seasonal)   # the strata and temporal effects are used automatically
```

## 2. baselinenowcast — a reporting triangle

[`baselinenowcast`](https://baselinenowcast.epinowcast.org/) is a
simple, fast baseline. It works from a **reporting triangle**: a matrix
with one row per reference (onset) week and one column per reporting
delay, where the lower-right corner is the not-yet-observed part we want
to fill in.

[`tbl_now_to_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_baselinenowcast.md)
builds that triangle for you (aggregating the line-list to weekly
incidence on the way):

``` r

library(baselinenowcast)

# `delays_unit` defaults to NULL and is inferred from the object's weekly units;
# pass it explicitly (e.g. delays_unit = "weeks") if the units are ambiguous.
dengue_triangle <- tbl_now_to_baselinenowcast(dengue_now, verbose = FALSE)

# rows = onset weeks, columns = delay in weeks
dengue_triangle[1:5, 1:6]
#>            0  1  2 3 4 5
#> 2009-01-05 0 17 11 2 0 0
#> 2009-01-12 0 18 17 0 0 0
#> 2009-01-19 1 20  9 7 3 0
#> 2009-01-26 0 13 12 4 0 0
#> 2009-02-02 1 22 17 8 0 0
```

Because
[`as_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.html)
is registered as a method for `tbl_now`, alternatively you can also
write the idiomatic `baselinenowcast` call and pass the `tbl_now`
straight in:

``` r

dengue_triangle2 <- as_reporting_triangle(dengue_now)
```

From here you follow `baselinenowcast`’s own workflow. The one-call
[`baselinenowcast()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.html)
wrapper estimates the delay from the triangle, applies it, and draws
nowcast samples in a single step:

``` r

# One-call workflow: estimate the delay, apply it, and draw nowcast samples.
nowcast_samples <- baselinenowcast(
  dengue_triangle2,
  output_type = "samples",
  draws       = 1000
)

head(nowcast_samples)
```

If you want the intermediate point nowcast (rather than samples), the
single-triangle helper is
[`estimate_and_apply_delay()`](https://baselinenowcast.epinowcast.org/reference/estimate_and_apply_delay.html):

``` r

point_nowcast <- estimate_and_apply_delay(dengue_triangle2)
```

### With strata and effects.

A reporting-triangle *matrix* is a single table with no room for strata
or covariates, so `format = "matrix"` pools the strata (with a warning)
and drops the effect columns. To keep them and to nowcast each stratum
separately take the **long** format, which carries `gender` and the
effect columns, and build one triangle per stratum:

``` r

dengue_long <- tbl_now_to_baselinenowcast(dengue_seasonal, format = "long", verbose = FALSE)

# List with one reporting triangle per stratum
triangles_by_stratum <- dengue_long |>
  split(dengue_long$gender) |> #
  lapply(\(df) as_reporting_triangle(df, delays_unit = "weeks"))
```

``` r

#One nowcast per triangle
nowcasts_by_stratum <- triangles_by_stratum |>
  lapply(\(tri) baselinenowcast(tri, output_type = "samples", draws = 1000))
```

## 3. epinowcast — a preprocessed Bayesian model object

[`epinowcast`](https://package.epinowcast.org/) fits a flexible Bayesian
model with separate modules for the reporting delay and the reference
(epidemic) process. It expects a preprocessed object built by
[`enw_preprocess_data()`](https://package.epinowcast.org/reference/enw_preprocess_data.html).

[`tbl_now_to_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md)
handles the preprocessing, returning an object you can pass straight to
[`epinowcast::epinowcast()`](https://package.epinowcast.org/reference/epinowcast.html):

``` r

library(epinowcast)

dengue_enw <- tbl_now_to_epinowcast(dengue_now, verbose = FALSE, quiet = TRUE)

dengue_enw
```

``` r

# A minimal epinowcast fit from the preprocessed object
fit <- epinowcast(
  dengue_enw,
  fit = enw_fit_opts(pp = TRUE, chains = 2, iter_sampling = 500)
)
```

### With strata and effects.

Handing
[`tbl_now_to_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md)
the enriched object does two things automatically: the `gender` stratum
becomes epinowcast’s grouping (`by`), so the model fits a delay per sex,
and the temporal-effect columns land in the `metareference` /
`metareport` tables on every completed reference date, including the
recent incomplete ones ready to be used in a module formula.

``` r

enw_seasonal <- tbl_now_to_epinowcast(dengue_seasonal, verbose = FALSE, quiet = TRUE)

# One group per stratum:
length(unique(enw_seasonal$metareference[[1]]$.group))
#> [1] 2

# The effect columns are in the per-reference-date metadata, ready for a formula:
names(enw_seasonal$metareference[[1]])
#> [1] ... ".event_week_of_year"  ".event_season_52_cos" ".event_season_52_sin" ...

# Drop the seasonal terms into a reference-module formula and fit as usual: the
# covariates now enter the reference model.
epinowcast(
  enw_seasonal,
  reference = enw_reference(
    parametric   = ~ 1 + .event_season_52_sin + .event_season_52_cos,
    distribution = "lognormal",
    data         = enw_seasonal
  ),
  fit = enw_fit_opts(pp = TRUE, chains = 2, iter_sampling = 500)
)
```

## 4. epidist — the reporting delay distribution

Sometimes the quantity you actually want is the **delay distribution**
itself — how long, on average, between onset and report, and how
variable is it? [`epidist`](https://epidist.epinowcast.org/) estimates
exactly that, treating each case as an interval-censored onset/report
pair.

[`tbl_now_to_epidist()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epidist.md)
converts the `tbl_now` into the censored line-list `epidist` expects.
Because our data are weekly, each event is censored over its whole week
automatically:

``` r

library(epidist)

dengue_epidist <- tbl_now_to_epidist(dengue_now, verbose = FALSE)
```

``` r

# Fit the delay distribution (see the epidist documentation for model choices)
delay_model <- dengue_epidist |>
  as_epidist_marginal_model() |>
  epidist()
```

The fitted delay distribution can then feed back into a nowcast for
example as a data-informed prior in `epinowcast`.

### With strata and effects.

`epidist` has no separate grouping argument, so the `gender` stratum and
the temporal-effect columns simply ride along as extra columns on the
censored line-list — where an `epidist` model **formula** can use them.
The mean of the delay distribution (`mu`) can be made to vary by sex, or
by season through the Fourier terms:

``` r

dengue_epidist_eff <- tbl_now_to_epidist(dengue_seasonal, verbose = FALSE)

# the strata and effect columns are on the line-list, ready for a formula
names(dengue_epidist_eff)
#>  [1] "ptime_lwr"            "ptime_upr"            "stime_lwr"           
#>  [4] "stime_upr"            "obs_time"             "pdate_lwr"           
#>  [7] "pdate_upr"            "sdate_lwr"            "sdate_upr"           
#> [10] "gender"               ".event_week_of_year"  ".event_season_52_cos"
#> [13] ".event_season_52_sin" "obs_date"
```

``` r

# A sex-varying mean delay: `gender` enters the model for `mu`.
delay_by_sex <- dengue_epidist_eff |>
  as_epidist_marginal_model() |>
  epidist(formula = mu ~ 1 + gender)

# A season-varying mean delay uses the Fourier pair instead:
delay_seasonal <- dengue_epidist_eff |>
  as_epidist_marginal_model() |>
  epidist(formula = mu ~ 1 + .event_season_52_sin + .event_season_52_cos)
```

## 5. NobBS — Nowcasting by Bayesian Smoothing

[`NobBS`](https://cran.r-project.org/package=NobBS) works from a
**line-list** with an onset-date column and a report-date column —
precisely what a `linelist` `tbl_now` already is. Drop the `tbl.now`
classes with
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) and hand
the columns to `NobBS()`:

``` r

dengue_linelist <- as.data.frame(dengue_now)
```

``` r

library(NobBS)

nobbs_fit <- NobBS(
  data        = dengue_linelist,
  now         = get_now(dengue_now),
  units       = "1 week",
  onset_date  = get_event_date(dengue_now),
  report_date = get_report_date(dengue_now)
)

nobbs_fit$estimates |> head()
```

Notice that even the arguments come from the `tbl_now`:
[`get_now()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md),
[`get_event_date()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
and
[`get_report_date()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
tell `NobBS` what `tbl.now` already figured out.

### With strata and effects.

Call `NobBS.strat()` and pass the column name to the strata option:

``` r

dengue_nobbs     <- as.data.frame(dengue_seasonal)
stratified_nobbs <- NobBS.strat(dengue_nobbs, 
                                strata        = "gender", 
                                now         = get_now(dengue_seasonal), 
                                units       = "1 week",
                                onset_date  = get_event_date(dengue_seasonal),
                                report_date = get_report_date(dengue_seasonal),
                                #These two values are set small for the example
                                max_D         = 5, 
                                moving_window = 7
                                )
```

## Summary

We described the dengue data once as a `tbl_now`, and then a single
converter call (or, for `diseasenowcasting`, no call at all) handed it
to each package in the shape it needed:

``` r

dengue_now <- tbl_now(dengue, event_date = onset_week, report_date = report_week,
                      data_type = "linelist")

dengue_now                              # diseasenowcasting
tbl_now_to_baselinenowcast(dengue_now)  # baselinenowcast
tbl_now_to_epinowcast(dengue_now)       # epinowcast
tbl_now_to_epidist(dengue_now)          # epidist
as.data.frame(dengue_now)               # NobBS or others
```

Attaching **strata** and **temporal effects** once (`dengue_seasonal`)
then rides along the same converters: each package receives them in
whatever way it can use — a grouping in `epinowcast`, covariate columns
in `epidist` and the `baselinenowcast` long format, one triangle/series
per stratum where the model takes a single series, and automatically in
`diseasenowcasting`.

## See also

- The main [`tbl.now`
  vignette](https://rodrigozepeda.github.io/tbl.now/articles/tbl.now.html)
  for the full anatomy of a `tbl_now`, data types, and temporal effects.
- [`autoplot()`](https://rodrigozepeda.github.io/tbl.now/reference/autoplot.tbl_now.html)
  for a quick diagnostic overview (delay distribution, epidemic curve,
  calendar effects, seasonality) of any `tbl_now`.
- Tutorial on detecting batches and other reporting-delay artifacts:
  <https://rodrigozepeda.github.io/tbl.now/articles/batch-reporting.html>
