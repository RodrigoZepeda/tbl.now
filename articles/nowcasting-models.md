# One dataset, many nowcasts: using tbl.now with five modelling packages

## Why this vignette?

If you work in public health surveillance you have almost certainly hit
the same wall: **reported case counts for the most recent days or weeks
are incomplete.** A case that happened last week may not be reported
until next week, so the freshest numbers always look artificially low.
*Nowcasting* is the family of methods that corrects for this reporting
delay and estimates what the recent counts will look like once reporting
is complete.

The good news is that the `R` ecosystem has many excellent nowcasting
packages. The bad news is that **each one wants its data in a different
shape**: a reporting triangle here, a cumulative long table there, a
line list somewhere else. Preparing the same outbreak data five
different ways is tedious and error-prone.

This is exactly the gap
[`tbl.now`](https://rodrigozepeda.github.io/tbl.now/) fills. You
describe your data **once** — which column is the event date, which is
the report date, what the counts mean — and `tbl.now` hands it to each
modelling package in the format that package expects. In this vignette
we take a single dengue line list and, from that one object, drive five
different nowcasting / delay-estimation tools:

| Package | What it does | `tbl.now` bridge |
|----|----|----|
| [diseasenowcasting](https://rodrigozepeda.github.io/diseasenowcasting/) | Bayesian structural time-series nowcast | consumes a `tbl_now` **directly** |
| [baselinenowcast](https://baselinenowcast.epinowcast.org/) | fast, assumption-light baseline nowcast | [`tbl_now_to_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_baselinenowcast.md) |
| [epinowcast](https://package.epinowcast.org/) | flexible Bayesian nowcast (delay + reference modules) | [`tbl_now_to_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md) |
| [epidist](https://epidist.epinowcast.org/) | estimates the reporting **delay distribution** | [`tbl_now_to_epidist()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epidist.md) |
| [NobBS](https://cran.r-project.org/package=NobBS) | Nowcasting by Bayesian Smoothing | a plain line list ([`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)) |

You do not need to be an expert in any of them to follow along — the
point is how little changes on *your* side when you switch models.

## The data: one `tbl_now` to rule them all

`tbl.now` ships with `denguedat`, a weekly dengue line list (one row per
case) with the week of symptom **onset** and the week the case was
**reported**.

``` r

library(tbl.now)

data(denguedat)
head(denguedat)
#>   onset_week report_week gender
#> 1 1990-01-01  1990-01-01   Male
#> 2 1990-01-01  1990-01-01 Female
#> 3 1990-01-01  1990-01-01 Female
#> 4 1990-01-01  1990-01-08 Female
#> 5 1990-01-01  1990-01-08   Male
#> 6 1990-01-01  1990-01-15 Female
```

We build a single `tbl_now`. We only have to say which column is the
event date (symptom onset) and which is the report date; `tbl.now`
infers the rest (the data are a line list, measured in weeks, and the
“now” is the last report).

``` r

# A compact recent window keeps the vignette fast to build.
dengue <- denguedat[denguedat$onset_week >= as.Date("2009-01-01"), ]

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

That is the *only* data-wrangling step in this whole vignette.
Everything below starts from `dengue_now`.

## 1. diseasenowcasting — no conversion at all

[`diseasenowcasting`](https://rodrigozepeda.github.io/diseasenowcasting/)
was designed hand-in-hand with `tbl.now`, so it takes a `tbl_now`
**directly**. You just hand it the object:

``` r

library(diseasenowcasting)

dnc_fit <- nowcast(dengue_now)

dnc_fit
```

`diseasenowcasting` will even reach into the `tbl_now` for any temporal
effects you attached (see the [temporal
effects](https://rodrigozepeda.github.io/tbl.now/articles/tbl.now.html#temporal-effects)
section of the main vignette). For example, to let the model account for
a seasonal cycle and a post-holiday reporting bump:

``` r

library(almanac)

dengue_seasonal <- dengue_now |>
  add_temporal_effects(
    temporal_effects(
      seasons      = 52,                          # annual seasonality (weekly data)
      holidays     = rcalendar(hol_christmas()),  # Christmas holiday effect
      holiday_lags = 2                            # + the two weeks "after" it
    )
  )

nowcast(dengue_seasonal)
```

## 2. baselinenowcast — a reporting triangle

[`baselinenowcast`](https://baselinenowcast.epinowcast.org/) is a
deliberately simple, fast baseline. It works from a **reporting
triangle**: a matrix with one row per reference (onset) week and one
column per reporting delay, where the lower-right corner is the
not-yet-observed part we want to fill in.

[`tbl_now_to_baselinenowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_baselinenowcast.md)
builds that triangle for you (aggregating the line list to weekly
incidence on the way):

``` r

library(baselinenowcast)

dengue_triangle <- tbl_now_to_baselinenowcast(
  dengue_now,
  format      = "matrix",
  delays_unit = "weeks",
  verbose     = FALSE
)

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
is registered as a method for `tbl_now`, you can also write the
idiomatic `baselinenowcast` call and pass the `tbl_now` straight in:

``` r

dengue_triangle2 <- as_reporting_triangle(dengue_now, delays_unit = "weeks")
identical(dim(dengue_triangle), dim(dengue_triangle2))
#> [1] TRUE
```

From here you follow `baselinenowcast`’s own workflow — estimate the
delay from the triangle and sample nowcasts:

``` r

# Estimate the delay from the triangle, apply it, and draw nowcast samples
# (see the baselinenowcast documentation for the full workflow).
point_nowcast <- dengue_triangle |>
  estimate_and_apply_delays()

nowcast_samples <- point_nowcast |>
  sample_nowcasts()
```

## 3. epinowcast — a preprocessed Bayesian model object

[`epinowcast`](https://package.epinowcast.org/) fits a flexible Bayesian
model with separate modules for the reporting delay and the reference
(epidemic) process. It expects **cumulative** counts and a preprocessed
object built by
[`enw_preprocess_data()`](https://package.epinowcast.org/reference/enw_preprocess_data.html).

[`tbl_now_to_epinowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epinowcast.md)
handles the cumulation and the preprocessing, returning an object you
can pass straight to
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

If you attached temporal effects to the `tbl_now`, they are carried into
the `metareference` / `metareport` tables as covariate columns, ready
for use in epinowcast’s reference and report modules.

## 4. epidist — the reporting delay distribution

Sometimes the quantity you actually want is the **delay distribution**
itself — how long, on average, between onset and report, and how
variable is it? [`epidist`](https://epidist.epinowcast.org/) estimates
exactly that, treating each case as an interval-censored onset/report
pair.

[`tbl_now_to_epidist()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_epidist.md)
converts the `tbl_now` into the censored line list `epidist` expects.
Because our data are weekly, each event is censored over its whole week
automatically:

``` r

library(epidist)

dengue_epidist <- tbl_now_to_epidist(dengue_now, verbose = FALSE)

head(dengue_epidist)
#> # A tibble: 6 × 10
#>   ptime_lwr ptime_upr stime_lwr stime_upr obs_time pdate_lwr  pdate_upr 
#>       <dbl>     <dbl>     <dbl>     <dbl>    <dbl> <date>     <date>    
#> 1         0         7         7        14      721 2009-01-05 2009-01-12
#> 2         0         7         7        14      721 2009-01-05 2009-01-12
#> 3         0         7        14        21      721 2009-01-05 2009-01-12
#> 4         0         7         7        14      721 2009-01-05 2009-01-12
#> 5         0         7         7        14      721 2009-01-05 2009-01-12
#> 6         0         7         7        14      721 2009-01-05 2009-01-12
#> # ℹ 3 more variables: sdate_lwr <date>, sdate_upr <date>, obs_date <date>
```

``` r

# Fit the delay distribution (see the epidist documentation for model choices)
delay_model <- dengue_epidist |>
  as_epidist_marginal_model() |>
  epidist()
```

The fitted delay distribution can then feed back into a nowcast — for
example as a prior in `epinowcast` or `EpiNow2`.

## 5. NobBS — Nowcasting by Bayesian Smoothing

[`NobBS`](https://cran.r-project.org/package=NobBS) works from a **line
list** with an onset-date column and a report-date column — precisely
what a `linelist` `tbl_now` already is. Drop the `tbl.now` classes with
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) and hand
the columns to `NobBS()`:

``` r

dengue_linelist <- as.data.frame(dengue_now)

names(dengue_linelist)
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

## Carrying delay effects into each model

Reporting delays are rarely constant — they wax and wane with the day of
the week, the season, and around holidays. `tbl.now` lets you attach
these **temporal (delay) effects** once, with
[`add_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.html),
and then carries the resulting covariate columns into whichever model
format can hold them. (Use
[`autoplot()`](https://rodrigozepeda.github.io/tbl.now/reference/autoplot.tbl_now.html)
with `panels = "delay_calendar"` or `panels = "delay_seasonality"` first
to *see* whether such effects exist in your data before modelling them.)

``` r

library(almanac)

dengue_eff <- dengue_now |>
  add_temporal_effects(
    temporal_effects(
      week_of_year = TRUE,                        # week-of-year delay effect
      holidays     = rcalendar(hol_christmas()),  # holiday indicator
      holiday_lags = 1                            # + the first week after a holiday
    )
  )

# The spec is lazy; materialise it to see the columns it produces
compute_temporal_effects(dengue_eff) |>
  get_temporal_effect_cols()
#> [1] ".event_week_of_year"  ".event_holiday"       ".event_holiday_lag_1"
```

These columns then ride along automatically when you convert. For
example, into the `baselinenowcast` long format and into `epinowcast`’s
metadata tables:

``` r

bln_long <- tbl_now_to_baselinenowcast(dengue_eff, format = "long", verbose = FALSE)
grep("holiday|week_of_year", names(bln_long), value = TRUE)
#> [1] ".event_week_of_year"  ".event_holiday"       ".event_holiday_lag_1"
```

``` r

enw <- tbl_now_to_epinowcast(dengue_eff, verbose = FALSE, quiet = TRUE)
# The effect columns appear in the reference and report metadata tables,
# ready to be used in epinowcast's reference / report module formulas.
grep("holiday|week_of_year", names(enw$metareference[[1]]), value = TRUE)
```

**Where do the columns end up, and does the model use them?** Carrying a
column and *using* it are two different things — `tbl.now` does the
former; whether a model consumes it is up to that model:

| Target | Delay-effect columns | Generic `covariates` | How the model can use them |
|----|----|----|----|
| `diseasenowcasting` | consumed automatically | — | `nowcast()` reads the effect columns straight from the `tbl_now` and adds them to the model |
| `epinowcast` | in `metareference` **and** `metareport` | dropped (fixed schema) | reference / report module formulas (e.g. `~ 1 + .event_holiday`) |
| `baselinenowcast` (long) | extra columns | extra columns | carried for you; the core triangle estimator ignores them |
| `baselinenowcast` (matrix) | dropped | dropped | the matrix holds only counts |
| `epidist` | extra linelist columns | extra linelist columns | available to formula-based delay models |
| `data.table` / `tsibble` | kept | kept | yours to use however you like |
| `NobBS` | not used | not used | `NobBS` models only the onset/report dates |

Two practical notes for practitioners:

- **Delay effects are date-derived**, so they aggregate cleanly from a
  line list to counts and survive every conversion above. A **generic
  per-case covariate** (say a per-patient lab value) does *not*
  aggregate to a reference-date × report-date cell, so it is only
  meaningful for the line-list / long formats — and `epinowcast`’s
  cumulative schema will reject it.
- The `baselinenowcast` **matrix** and any purely count-based format
  cannot carry covariates at all; use the **long** format when you want
  the effects to travel.

## The pay-off

Look back at what changed between the five models: **almost nothing.**
We described the dengue data once as a `tbl_now`, and then a single
converter call (or, for `diseasenowcasting`, no call at all) handed it
to each package in the shape it needed:

``` r

dengue_now <- tbl_now(dengue, event_date = onset_week, report_date = report_week,
                      data_type = "linelist")

nowcast(dengue_now)                                        # diseasenowcasting
tbl_now_to_baselinenowcast(dengue_now, format = "matrix")  # baselinenowcast
tbl_now_to_epinowcast(dengue_now)                          # epinowcast
tbl_now_to_epidist(dengue_now)                             # epidist
as.data.frame(dengue_now)                                  # NobBS
```

For a public-health team this means you can **benchmark several
nowcasting methods on your own surveillance data without re-plumbing it
each time** — making it practical to compare models, cross-check
results, and pick the tool that works best for your disease and
reporting system.

## See also

- The main [`tbl.now`
  vignette](https://rodrigozepeda.github.io/tbl.now/articles/tbl.now.html)
  for the full anatomy of a `tbl_now`, data types, and temporal effects.
- [`autoplot()`](https://rodrigozepeda.github.io/tbl.now/reference/autoplot.tbl_now.html)
  for a quick diagnostic overview (delay distribution, epidemic curve,
  calendar effects, seasonality) of any `tbl_now`.
