# A complete nowcasting workflow: hospital-acquired infections in Bucaramanga

This article is an end-to-end walk-through of the `tbl.now` workflow on
a real, **deliberately unpolished** dataset. We will:

1.  **Clean** a messy surveillance extract with `dplyr` and `tbl.now`,
    checking the things that actually break a nowcast — duplicate
    records, missing dates, and reports that arrive *before* the event
    they describe.
2.  **Look** at the data with
    [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
    and the standalone `plot_*()` diagnostics.
3.  Ask whether the reporting delay is **stable** — does it drift, does
    it jump?
4.  Attach **temporal effects** that the data actually justifies.
5.  **Nowcast**, first with `diseasenowcasting` and then with five other
    engines, letting everything we found in steps 1–3 inform the model.

The thread running through all of it: *look before you model.* Every
modelling choice at the end is traceable to something we measured at the
beginning.

``` r

library(dplyr, quietly = TRUE)
library(lubridate)
library(ggplot2)
library(tbl.now)
```

## The data

`hai_bucaramanga` is a line list of **healthcare-associated infections**
(IAAS, *Infecciones Asociadas a la Atención en Salud*) notified in the
municipality of Bucaramanga, Colombia, published on the Colombian
open-data portal. Each row is one infection: a specimen taken from a
hospitalised patient, the laboratory result, and the microorganism
isolated.

``` r

data(hai_bucaramanga)

glimpse(hai_bucaramanga)
#> Rows: 1,423
#> Columns: 13
#> $ id              <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 162…
#> $ specimen_date   <date> 2018-10-01, 2018-11-01, 2018-01-27, 2018-01-27, 2018-04-20, 2018-01-22, 2018-01-02, 2018-06-28, 2018-11-03, 2018-12-02, 2016-01-22, 2…
#> $ received_date   <date> 2018-10-01, 2018-11-01, 2018-01-27, 2018-01-27, 2018-04-20, 2018-01-22, 2018-01-02, 2018-06-28, 2018-03-13, 2018-12-02, NA, NA, 2018-…
#> $ report_date     <date> 2018-01-13, NA, 2018-01-31, 2018-01-30, 2018-04-26, 2018-01-25, 2018-03-02, 2018-06-30, 2018-03-13, NA, NA, NA, 2018-12-03, 2018-08-0…
#> $ specimen        <fct> Whole blood, Whole blood, Whole blood, Whole blood, Urine, Whole blood, Whole blood, Whole blood, Whole blood, Whole blood, Whole bloo…
#> $ test            <fct> Blood culture, Blood culture, Blood culture, Blood culture, Urine culture, Blood culture, Blood culture, Blood culture, Blood culture,…
#> $ microorganism   <chr> "Burkholderia cepacia", "Klebsiella pneumoniae", "Stenotrophomonas maltophilia", "Klebsiella pneumoniae", "Klebsiella pneumoniae", "Kl…
#> $ sex             <fct> Female, Male, Female, Male, Male, Male, Female, Male, Male, Male, Male, Male, Female, Female, Female, Male, Male, Male, Male, Male, Fe…
#> $ age_group       <ord> 20-29, 50-59, 50-59, 50-59, 70+, 30-39, 20-29, 50-59, 50-59, 70+, <1, 5-9, 60-69, 40-49, 30-39, 60-69, 10-14, 70+, <1, 1-4, 50-59, 20-…
#> $ case_type       <fct> Laboratory-confirmed, Laboratory-confirmed, Laboratory-confirmed, Laboratory-confirmed, Laboratory-confirmed, Laboratory-confirmed, La…
#> $ final_condition <fct> Alive, Alive, Alive, Alive, Dead, Alive, Alive, Dead, Alive, Alive, NA, NA, Alive, Alive, Alive, Alive, NA, Alive, Alive, NA, Alive, A…
#> $ icu_type        <fct> Adult, Adult, Adult, Adult, Adult, Adult, Adult, Adult, Adult, Adult, Paediatric, Paediatric, Adult, Adult, Adult, Adult, Paediatric, …
#> $ institution     <int> 5, 4, 5, 5, 4, 5, 5, 5, 2, 4, 1, 1, 5, 2, 5, 2, 5, 6, 1, 1, 5, 2, 2, 1, 6, 6, 1, 1, 5, 5, 1, 1, 7, 6, 5, 1, 1, 1, 1, 1, 1, 8, 8, 7, 5,…
```

For nowcasting, two columns matter:

- `specimen_date` — the **event date**, when the sample was taken.
- `report_date` — the **report date**, when the laboratory issued the
  result.

The delay between them is the specimen-to-result turnaround, and it is
exactly what a nowcast has to model. (`received_date` would split that
into a transport and a processing leg, but it is 84% missing and stops
in 2019, so we leave it alone.)

## 1. Cleaning

[`?hai_bucaramanga`](https://rodrigozepeda.github.io/tbl.now/reference/hai_bucaramanga.md)
documents the defects; here we deal with them one at a time rather than
trusting the help page. Nothing below is specific to this dataset —
these are the checks worth running on *any* surveillance extract before
it reaches a model.

### Duplicate records

The source’s `id` column is documented as a unique autonumber, so it
should have no repeats:

``` r

hai_bucaramanga |>
  count(id) |>
  filter(n > 1) |>
  nrow()
#> [1] 100
```

It does. Are those rows genuinely identical, or do they differ somewhere
and merely share an id? That distinction decides whether it is safe to
drop them:

``` r

# Rows that are byte-identical to another row, across every column.
sum(duplicated(hai_bucaramanga))
#> [1] 100
```

The count of repeated ids and the count of exact duplicate rows agree,
so every repeat is a whole-record copy — an export artifact, not a real
second infection.
[`distinct()`](https://dplyr.tidyverse.org/reference/distinct.html)
removes them without losing information:

``` r

hai <- distinct(hai_bucaramanga)

nrow(hai_bucaramanga) - nrow(hai)
#> [1] 100
```

### Missing dates

The source encodes missing dates as `1900-01-01`; that sentinel has
already been converted to `NA` when the dataset was built (see
[`?hai_bucaramanga`](https://rodrigozepeda.github.io/tbl.now/reference/hai_bucaramanga.md)).
A row without both dates cannot contribute to a reporting triangle:

``` r

hai |>
  summarise(
    missing_event  = sum(is.na(specimen_date)),
    missing_report = sum(is.na(report_date)),
    missing_either = sum(is.na(specimen_date) | is.na(report_date))
  )
#> # A tibble: 1 × 3
#>   missing_event missing_report missing_either
#>           <int>          <int>          <int>
#> 1           293            558            569
```

That is a large fraction of the file, and it is worth pausing on rather
than filtering away reflexively. Missingness that depends on *when* a
case occurred would bias the nowcast; missingness spread evenly over
time mostly costs precision. Here it is heavily concentrated in the
early years:

``` r

hai |>
  filter(!is.na(specimen_date)) |>
  mutate(year = year(specimen_date)) |>
  summarise(
    cases        = n(),
    missing_rate = round(mean(is.na(report_date)), 2),
    .by          = year
  ) |>
  arrange(year)
#> # A tibble: 8 × 3
#>    year cases missing_rate
#>   <dbl> <int>        <dbl>
#> 1  2016   115         1   
#> 2  2017   116         0.77
#> 3  2018   116         0.03
#> 4  2019    87         0.09
#> 5  2020   185         0.3 
#> 6  2021   278         0   
#> 7  2022   125         0.04
#> 8  2023     8         0
```

``` r

hai <- hai |> filter(!is.na(specimen_date), !is.na(report_date))

nrow(hai)
#> [1] 754
```

### Reports that arrive before the event

This is the check that most often catches real data-entry problems, and
it is easy to forget: a result cannot be issued before the sample was
taken, so the delay must never be negative.

``` r

hai |>
  mutate(delay_days = as.numeric(report_date - specimen_date)) |>
  summarise(
    negative = sum(delay_days < 0),
    worst    = min(delay_days),
    median   = median(delay_days),
    max      = max(delay_days)
  )
#> # A tibble: 1 × 4
#>   negative worst median   max
#>      <int> <dbl>  <dbl> <dbl>
#> 1       81  -331      3   242
```

Eighty-one records have a report date *before* their specimen date, one
by almost a year. There is no way to tell from the file whether the
event date, the report date, or both are wrong, so the only defensible
move is to drop them and say so:

``` r

hai <- hai |> filter(report_date >= specimen_date)

nrow(hai)
#> [1] 673
```

### The cleaning cascade

It is worth writing the whole cascade down. Readers of your analysis —
and you, in six months — need to know how much data reached the model
and why the rest did not:

| Step                                | Records | Lost at this step |
|:------------------------------------|--------:|------------------:|
| Raw records                         |    1423 |                NA |
| After removing exact duplicates     |    1323 |               100 |
| After requiring both dates          |     754 |               569 |
| After requiring report \>= specimen |     673 |                81 |

Cleaning cascade {.table}

Just under half the file survives. That is uncomfortable, and it is the
honest starting point for everything that follows.

## 2. Choosing a time resolution

A `tbl_now` will happily take the daily dates, but daily is not usable
here:

``` r

hai |>
  summarise(
    cases            = n(),
    distinct_days    = n_distinct(specimen_date),
    cases_per_day    = round(n() / n_distinct(specimen_date), 2),
    distinct_weeks   = n_distinct(floor_date(specimen_date, "week", week_start = 1)),
    cases_per_week   = round(n() / n_distinct(floor_date(specimen_date, "week", week_start = 1)), 2)
  )
#> # A tibble: 1 × 5
#>   cases distinct_days cases_per_day distinct_weeks cases_per_week
#>   <int>         <int>         <dbl>          <int>          <dbl>
#> 1   673           488          1.38            225           2.99
```

Roughly **1.4 cases per day** cannot support a delay distribution with
any resolution at all. Aggregating to weeks gives about three cases a
week, which is still thin but workable.

Monthly would be denser still, and statistically it is the better fit
for this data — but it is worth knowing that **your time unit constrains
your choice of engine**. Of the five nowcasting packages used at the end
of this article, only `diseasenowcasting` and `baselinenowcast` accept
monthly data: `NobBS` supports only daily and weekly units, and
`epinowcast` accepts only `"day"` and `"week"` timesteps. Weeks are the
finest unit every engine agrees on, so weeks it is.

``` r

hai_weekly <- hai |>
  mutate(
    event_week  = floor_date(specimen_date, "week", week_start = 1),
    report_week = floor_date(report_date,   "week", week_start = 1)
  )
```

## 3. Building the `tbl_now`

Now the data can be handed to
[`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md).
We declare the two dates, say the data is a line list (one row per
case), and stratify by the type of intensive care unit:

``` r

hai_now <- hai_weekly |>
  tbl_now(
    event_date  = event_week,
    report_date = report_week,
    strata      = icu_type,
    data_type   = "linelist"
  )

hai_now
#> # A tibble:  673 × 18
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>       id specimen_date received_date report_date specimen    test        microorganism sex   age_group case_type final_condition icu_type institution event_week
#>    <int> <date>        <date>        <date>      <fct>       <fct>       <chr>         <fct> <ord>     <fct>     <fct>           <fct>          <int> <date>    
#>    [...] [...]         [...]         [...]       [...]       [...]       [...]         [...] [...]     [...]     [...]           [strata]       [...] [event_da…
#>  1     3 2018-01-27    2018-01-27    2018-01-31  Whole blood Blood cult… Stenotrophom… Fema… 50-59     Laborato… Alive           Adult              5 2018-01-22
#>  2     4 2018-01-27    2018-01-27    2018-01-30  Whole blood Blood cult… Klebsiella p… Male  50-59     Laborato… Alive           Adult              5 2018-01-22
#>  3     5 2018-04-20    2018-04-20    2018-04-26  Urine       Urine cult… Klebsiella p… Male  70+       Laborato… Dead            Adult              4 2018-04-16
#>  4     6 2018-01-22    2018-01-22    2018-01-25  Whole blood Blood cult… Klebsiella p… Male  30-39     Laborato… Alive           Adult              5 2018-01-22
#>  5     7 2018-01-02    2018-01-02    2018-03-02  Whole blood Blood cult… Klebsiella p… Fema… 20-29     Laborato… Alive           Adult              5 2018-01-01
#>  6     8 2018-06-28    2018-06-28    2018-06-30  Whole blood Blood cult… Acinetobacte… Male  50-59     Laborato… Dead            Adult              5 2018-06-25
#>  7    13 2018-10-03    2018-10-03    2018-12-03  Urine       Urine cult… Enterobacter… Fema… 60-69     Laborato… Alive           Adult              5 2018-10-01
#>  8    14 2018-05-03    2018-05-03    2018-08-03  Urine       Urine cult… Candida albi… Fema… 40-49     Laborato… Alive           Adult              2 2018-04-30
#>  9    15 2018-07-03    2018-07-03    2018-08-03  Whole blood Blood cult… Acinetobacte… Fema… 30-39     Laborato… Alive           Adult              5 2018-07-02
#> 10    16 2018-03-17    2018-03-17    2018-03-20  Urine       Urine cult… Staphylococc… Male  60-69     Laborato… Alive           Adult              2 2018-03-12
#> # ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> # Now: 2023-01-30 | Event date: "event_week" | Report date: "report_week"
#> # Strata: "icu_type"
#> # ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> # ℹ 663 more rows
#> # ℹ 4 more variables: report_week <date>, .event_num <dbl>, .report_num <dbl>, .delay <dbl>
```

[`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
inferred that both dates are **weekly**, computed `.delay` and the
numeric indices, and set the **now** to the most recent report week.
From here on, everything — the plots, the diagnostics, and every model
converter — reads those settings off the object instead of being told
again.

``` r

c(
  now          = format(get_now(hai_now)),
  event_units  = get_event_units(hai_now),
  report_units = get_report_units(hai_now),
  strata       = paste(get_strata(hai_now), collapse = ", ")
)
#>          now  event_units report_units       strata 
#> "2023-01-30"      "weeks"      "weeks"   "icu_type"
```

## 4. Looking at the data

[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
draws a diagnostic grid: the case-count panels in green describe the
**epidemic** process, the red ones the **reporting** process.

``` r

autoplot(hai_now)
```

![A six-panel diagnostic grid: empirical delay distribution, observed
epidemic curve, month-of-year effects for cases and for reporting, and
periodograms for both
processes.](example_files/figure-html/autoplot-1.png)

Four things stand out, and each of them changes a modelling decision
later.

**The epidemic curve has a large 2021 wave.** Case counts sit in the low
single digits for most of the series and then rise sharply through 2021
— the period when COVID-19 filled Colombian intensive care units. The
dashed line marks where the data become incomplete: everything to its
right is still arriving.

**The delay distribution is not a single hump.** This is the most
consequential feature of the dataset, so it is worth its own plot:

``` r

plot_delay_distribution(hai_now)
```

![Histogram of the reporting delay in weeks, showing a large spike at
zero and a long right tail.](example_files/figure-html/delay-dist-1.png)

More than half of all results are reported in the **same week** the
specimen was taken, but the tail runs out past thirty weeks. Tabulating
the cumulative share makes the shape explicit:

``` r

hai_weekly |>
  mutate(delay_weeks = as.numeric(report_week - event_week) / 7) |>
  count(delay_weeks) |>
  mutate(cumulative = round(cumsum(n) / sum(n), 3)) |>
  filter(delay_weeks %in% c(0, 1, 4, 8, 9, 13, 17, 22, 26)) |>
  select(delay_weeks, n, cumulative)
#> # A tibble: 9 × 3
#>   delay_weeks     n cumulative
#>         <dbl> <int>      <dbl>
#> 1           0   362      0.538
#> 2           1   135      0.738
#> 3           4    11      0.755
#> 4           8    16      0.786
#> 5           9    35      0.838
#> 6          13    58      0.932
#> 7          17    13      0.958
#> 8          22     7      0.993
#> 9          26     3      0.997
```

This is **bimodal**. Roughly 74% of results arrive within a week, then
reporting almost stops — only four more percentage points accumulate
between weeks 1 and 8 — and then a *second* wave of reports lands
between weeks 9 and 18, taking the total to 98%. That is the signature
of two different reporting streams: routine same-week laboratory
results, plus a slower retrospective process that sweeps up the rest
months later.

**The reporting process has an annual cycle.** In the bottom-right
periodogram the reporting series peaks at a period of about 12 months,
which the epidemic series does not. An annual rhythm in *reporting* —
not in infections — is consistent with that second, retrospective stream
being an annual audit or reconciliation exercise.

## 5. Is the reporting delay stable?

A nowcast fitted to the whole series assumes the delay distribution is
the same throughout. If it is not, the model averages over reporting
regimes that are not exchangeable, and the nowcast is biased. `tbl.now`
gives you two tests for this, which answer different questions.

First, look at it:

``` r

plot_delay_drift(hai_now, changepoint = TRUE)
```

![Fan chart of the reporting delay over time, showing the median as a
solid line and the interquartile and 10-90 percent bands. The bands
widen substantially from early 2020
onward.](example_files/figure-html/drift-plot-1.png)

The median delay bounces around zero throughout, but the spread of the
distribution clearly widens from 2020 onwards. The two tests confirm
exactly that split.

### Is there a gradual trend?

[`test_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/test_delay_drift.md)
runs an autocorrelation-robust Mann-Kendall test for a *monotonic*
trend:

``` r

test_delay_drift(hai_now)
#> # A tibble: 2 × 9
#>   strata stat       n      tau sens_slope statistic p_value method    drift
#>   <chr>  <chr>  <int>    <dbl>      <dbl>     <dbl>   <dbl> <chr>     <lgl>
#> 1 all    median   213 -0.00323          0   -0.0851   0.932 hamed-rao FALSE
#> 2 all    spread   213  0.0995           0    1.34     0.181 hamed-rao FALSE
```

No. Neither the median nor the spread shows a monotonic trend. Read
`tau` before `p_value`: both effect sizes are tiny, so this is a genuine
null rather than an underpowered one.

### Is there an abrupt shift?

A trend test cannot see a step change — a jump up and a jump down can
even cancel out to no trend at all.
[`test_delay_changepoint()`](https://rodrigozepeda.github.io/tbl.now/reference/test_delay_changepoint.md)
uses Pettitt’s test to look for exactly that:

``` r

test_delay_changepoint(hai_now)
#> # A tibble: 2 × 10
#>   strata stat       n changepoint statistic p_value before after shift changepoint_detected
#>   <chr>  <chr>  <int> <date>          <dbl>   <dbl>  <dbl> <dbl> <dbl> <lgl>               
#> 1 all    median   213 2019-11-25        736 1         2.69  3.23 0.532 FALSE               
#> 2 all    spread   213 2020-03-09       3248 0.00295   1.99  4.78 2.79  TRUE
```

Here is the finding that matters. The **median** delay has no change
point, but the **spread** does, and a decisive one: the 10–90 percentile
range of the delay jumps from about 2 weeks to about 4.8 weeks, and the
estimated change point is **early March 2020** — the week COVID-19
reached Colombia and hospital laboratories were abruptly redirected.

So the typical case was reported no more slowly after March 2020, but
the *variability* of reporting more than doubled. That is a real change
in the reporting process, and it has a direct modelling consequence,
which we act on in section 7.

### Does this differ by unit?

``` r

test_delay_drift(hai_now, by_strata = TRUE)
#> # A tibble: 6 × 9
#>   strata     stat       n     tau sens_slope statistic p_value method    drift
#>   <chr>      <chr>  <int>   <dbl>      <dbl>     <dbl>   <dbl> <chr>     <lgl>
#> 1 Adult      median   187 -0.0225          0    -0.534  0.593  hamed-rao FALSE
#> 2 Adult      spread   187  0.0972          0     1.22   0.224  hamed-rao FALSE
#> 3 Neonatal   median    42  0.141           0     2.15   0.0318 hamed-rao TRUE 
#> 4 Neonatal   spread    42  0.0488          0     0.910  0.363  hamed-rao FALSE
#> 5 Paediatric median    67 -0.0710          0    -1.57   0.116  hamed-rao FALSE
#> 6 Paediatric spread    67  0.0687          0     1.39   0.164  hamed-rao FALSE
```

The neonatal unit shows a positive trend in the median delay (`tau` =
0.14, p = 0.032). Resist the urge to make something of it: this is one
significant result out of six tests, and a Bonferroni threshold would be
0.05 / 6 = 0.008. It does not survive. With only 42 weeks contributing,
the honest reading is *suggestive, not established* — worth watching,
not worth modelling.

Note also that `sens_slope` is `0` everywhere. Delays are whole numbers
of weeks and more than half of them are zero, so the median pairwise
slope really is zero; that is a property of discrete, heavily-tied data,
not a bug.

## 6. Temporal effects

[`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md)
attaches calendar covariates a model can use. The temptation is to add
everything available; the discipline is to add only what the data
supports.

**Day-of-week effects are not justified here.** They are the usual first
choice for surveillance data — laboratories are quieter at weekends —
but this dataset does not behave that way:

``` r

hai |>
  count(weekday = wday(specimen_date, label = TRUE, week_start = 1)) |>
  mutate(percent = round(100 * n / sum(n), 1))
#> # A tibble: 7 × 3
#>   weekday     n percent
#>   <ord>   <int>   <dbl>
#> 1 Mon        88    13.1
#> 2 Tue        94    14  
#> 3 Wed       101    15  
#> 4 Thu        90    13.4
#> 5 Fri        91    13.5
#> 6 Sat       113    16.8
#> 7 Sun        96    14.3
```

Specimens are spread almost evenly across all seven days, weekends
included. And in any case we aggregated to weeks, so a day-of-week term
has nothing to attach to. Adding one would be modelling noise.

**An annual cycle is justified**, on the reporting side, by the
periodogram in section 4 and by the bimodal delay. Since the data are
weekly, that is a Fourier season of period 52:

``` r

hai_effects <- temporal_effects(
  week_of_year = TRUE,   # position within the year
  seasons      = 52      # an annual Fourier cycle on weekly data
)

hai_effects
#> ── Temporal Effects ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> The following effects are in place:
#> • "week_of_year"
#> • "season" periods: 52
```

Attach them to the report date, because the annual rhythm we saw is a
property of *reporting*, not of infection:

``` r

hai_seasonal <- hai_now |>
  add_temporal_effects(hai_effects, date_type = "report_date")

hai_seasonal
#> # A tibble:  673 × 18
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>       id specimen_date received_date report_date specimen    test        microorganism sex   age_group case_type final_condition icu_type institution event_week
#>    <int> <date>        <date>        <date>      <fct>       <fct>       <chr>         <fct> <ord>     <fct>     <fct>           <fct>          <int> <date>    
#>    [...] [...]         [...]         [...]       [...]       [...]       [...]         [...] [...]     [...]     [...]           [strata]       [...] [event_da…
#>  1     3 2018-01-27    2018-01-27    2018-01-31  Whole blood Blood cult… Stenotrophom… Fema… 50-59     Laborato… Alive           Adult              5 2018-01-22
#>  2     4 2018-01-27    2018-01-27    2018-01-30  Whole blood Blood cult… Klebsiella p… Male  50-59     Laborato… Alive           Adult              5 2018-01-22
#>  3     5 2018-04-20    2018-04-20    2018-04-26  Urine       Urine cult… Klebsiella p… Male  70+       Laborato… Dead            Adult              4 2018-04-16
#>  4     6 2018-01-22    2018-01-22    2018-01-25  Whole blood Blood cult… Klebsiella p… Male  30-39     Laborato… Alive           Adult              5 2018-01-22
#>  5     7 2018-01-02    2018-01-02    2018-03-02  Whole blood Blood cult… Klebsiella p… Fema… 20-29     Laborato… Alive           Adult              5 2018-01-01
#>  6     8 2018-06-28    2018-06-28    2018-06-30  Whole blood Blood cult… Acinetobacte… Male  50-59     Laborato… Dead            Adult              5 2018-06-25
#>  7    13 2018-10-03    2018-10-03    2018-12-03  Urine       Urine cult… Enterobacter… Fema… 60-69     Laborato… Alive           Adult              5 2018-10-01
#>  8    14 2018-05-03    2018-05-03    2018-08-03  Urine       Urine cult… Candida albi… Fema… 40-49     Laborato… Alive           Adult              2 2018-04-30
#>  9    15 2018-07-03    2018-07-03    2018-08-03  Whole blood Blood cult… Acinetobacte… Fema… 30-39     Laborato… Alive           Adult              5 2018-07-02
#> 10    16 2018-03-17    2018-03-17    2018-03-20  Urine       Urine cult… Staphylococc… Male  60-69     Laborato… Alive           Adult              2 2018-03-12
#> # ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> # Now: 2023-01-30 | Event date: "event_week" | Report date: "report_week"
#> # Strata: "icu_type"
#> # T. effects (lazy): [report_date] week_of_year, season(52)
#> # ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> # ℹ 663 more rows
#> # ℹ 4 more variables: report_week <date>, .event_num <dbl>, .report_num <dbl>, .delay <dbl>
```

Effects are attached **lazily** — the object records the specification
but does not materialise the columns until a model asks for them (or you
call
[`compute_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/compute_temporal_effects.md)):

``` r

hai_seasonal |>
  compute_temporal_effects() |>
  select(event_week, report_week, starts_with(".report_")) |>
  head(4)
#> Warning: Dropped protected column(?s): ".event_num" and ".delay". Returning a `tibble`
#> # A tibble: 4 × 6
#>   event_week report_week .report_num .report_week_of_year .report_season_52_cos .report_season_52_sin
#>   <date>     <date>            <dbl>                <int>                 <dbl>                 <dbl>
#> 1 2018-01-22 2018-01-29           55                    1                 0.935                 0.355
#> 2 2018-01-22 2018-01-29           55                    1                 0.935                 0.355
#> 3 2018-04-16 2018-04-23           67                   13                -0.239                 0.971
#> 4 2018-01-22 2018-01-22           54                   52                 0.971                 0.239
```

## 7. Nowcasting with `diseasenowcasting`

Now we let the diagnostics decide the model. Three findings carry over:

- **The delay distribution changed in March 2020.** Fitting across that
  boundary would average two reporting regimes. We restrict to the
  post-change-point period.
- **The delay reaches 98% by about 20 weeks.** That sets the maximum
  delay: long enough to capture the second reporting stream, short
  enough not to carry a mostly-empty tail.
- **No monotonic drift within that period**, so a single delay
  distribution for the restricted window is defensible.

``` r

changepoint_date <- as.Date("2020-03-09")

hai_model <- hai_weekly |>
  filter(event_week >= changepoint_date) |>
  tbl_now(
    event_date  = event_week,
    report_date = report_week,
    strata      = icu_type,
    data_type   = "linelist",
    verbose     = FALSE
  ) |>
  censor_delays_above(max_delay = 20)
#> ℹ Marked 10 reports with delay > 20 weeks as censored.
#> • This delay is now an upper bound (is_censored).

hai_model
#> # A tibble:  462 × 19
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>       id specimen_date received_date report_date specimen    test        microorganism sex   age_group case_type final_condition icu_type institution event_week
#>    <int> <date>        <date>        <date>      <fct>       <fct>       <chr>         <fct> <ord>     <fct>     <fct>           <fct>          <int> <date>    
#>    [...] [...]         [...]         [...]       [...]       [...]       [...]         [...] [...]     [...]     [...]           [strata]       [...] [event_da…
#>  1   588 2020-06-07    NA            2020-06-07  Whole blood Blood cult… Staphylococc… Fema… 70+       Laborato… Alive           Adult              5 2020-06-01
#>  2   589 2020-07-07    NA            2020-11-07  Whole blood Blood cult… Klebsiella p… Fema… 60-69     Laborato… Alive           Adult              5 2020-07-06
#>  3   591 2020-07-14    NA            2020-07-18  Whole blood Blood cult… Klebsiella p… Fema… 60-69     Laborato… Alive           Adult              5 2020-07-13
#>  4   592 2020-07-14    NA            2020-07-18  Whole blood Blood cult… Candida albi… Fema… 60-69     Laborato… Dead            Adult              5 2020-07-13
#>  5   593 2020-07-13    NA            2020-07-17  Whole blood Blood cult… Candida albi… Male  40-49     Laborato… Alive           Adult              5 2020-07-13
#>  6   594 2020-07-19    NA            2020-07-19  Whole blood Blood cult… Klebsiella p… Male  60-69     Laborato… Dead            Adult              5 2020-07-13
#>  7   595 2020-07-19    NA            2020-07-19  Whole blood Blood cult… Klebsiella p… Fema… 50-59     Laborato… Alive           Adult              5 2020-07-13
#>  8   596 2020-07-19    NA            2020-07-23  Whole blood Blood cult… Klebsiella p… Male  30-39     Laborato… Alive           Adult              5 2020-07-13
#>  9   597 2020-07-25    NA            2020-07-25  Whole blood Blood cult… Stenotrophom… Male  50-59     Laborato… Alive           Adult              5 2020-07-20
#> 10   598 2020-07-28    NA            2020-07-29  Urine       Urine cult… Candida trop… Fema… 50-59     Laborato… Alive           Adult              1 2020-07-27
#> # ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> # Now: 2023-01-30 | Event date: "event_week" | Report date: "report_week"
#> # left-censored indicator: ".is_censored"
#> # Strata: "icu_type"
#> # ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> # ℹ 452 more rows
#> # ℹ 5 more variables: report_week <date>, .event_num <dbl>, .report_num <dbl>, .delay <dbl>, .is_censored <lgl>
```

[`censor_delays_above()`](https://rodrigozepeda.github.io/tbl.now/reference/censor_delays_above.md)
is doing real work: it caps the delay dimension at 20 weeks so the model
is not asked to estimate probabilities for delays that occur a handful
of times in the whole series.

`diseasenowcasting` consumes a `tbl_now` **directly** — no converter, no
restating of which column is which:

``` r

# `diseasenowcasting::` rather than `library()` -- it is not on CRAN, and
# attaching it would leave `R CMD check` with an undeclared dependency.
hai_fit <- diseasenowcasting::nowcast(hai_model, type = "one_stage", n_draws = 2000)

hai_fit
```

The posterior draws of the eventual counts come from
[`predict()`](https://rdrr.io/r/stats/predict.html):

``` r

hai_prediction <- predict(hai_fit)

draws       <- S7::prop(hai_prediction, "draws")        # draws x event weeks
event_weeks <- S7::prop(hai_prediction, "event_dates")
observed    <- S7::prop(hai_prediction, "observed_series")

nowcast_summary <- tibble::tibble(
  event_week = event_weeks,
  observed   = observed,
  median     = apply(draws, 2, median),
  lower      = apply(draws, 2, quantile, 0.025),
  upper      = apply(draws, 2, quantile, 0.975)
)

tail(nowcast_summary, 8)
```

``` r

recent <- nowcast_summary |> filter(event_week >= max(event_week) - 7 * 30)

ggplot(recent, aes(x = event_week)) +
  geom_col(aes(y = observed, fill = "Reported so far"), width = 5.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#e78b7f", alpha = 0.35) +
  geom_line(aes(y = median, colour = "Nowcast"), linewidth = 0.9) +
  scale_fill_manual(name = NULL, values = c("Reported so far" = "#dfe5e0")) +
  scale_colour_manual(name = NULL, values = c("Nowcast" = "#B85348")) +
  labs(
    x = "Event week (specimen taken)", y = "Infections",
    title = "Nowcast of hospital-acquired infections",
    subtitle = paste0("Now = ", format(get_now(hai_model)),
                      "; the gap at the right edge is what has not been reported yet")
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", colour = "#334335"))
```

The bars are what had been reported by the `now`; the ribbon is the
model’s estimate of where those weeks will end up.

Look closely at what the nowcast actually does here, because it is not
the textbook picture. The **median barely moves**: it exceeds the
observed count in only one of the 152 modelled weeks. What changes
dramatically is the **uncertainty** — the mean width of the 95% interval
is about 0.1 cases over the settled part of the series and about 2 cases
over the final ten weeks, a twenty-fold widening.

That is the correct behaviour for data this sparse, and it is worth
understanding rather than treating as a disappointment. We know from the
delay distribution that roughly a quarter of results arrive more than a
week late, so a week sitting five weeks from `now` really is missing
about a quarter of its eventual cases. But a quarter of *two cases* is
half a case, and the median of a posterior over counts is an integer.
The correction is real; it is simply smaller than the resolution of the
answer. With one to three infections a week, a nowcast’s honest output
is a statement about what it does not know, not a large upward revision.

This is also why the point of the earlier diagnostics was never to
produce a better point estimate. It was to make sure the interval is
trustworthy — that it comes from one reporting regime rather than two
averaged together.

### Carrying the strata and effects through

The enriched object needs no extra arguments: `diseasenowcasting` reads
the `icu_type` stratum and the seasonal columns off the object itself.

``` r

hai_model_seasonal <- hai_model |>
  add_temporal_effects(hai_effects, date_type = "report_date")

diseasenowcasting::nowcast(hai_model_seasonal, type = "one_stage", n_draws = 2000)
```

## 8. The same data, other engines

The value of describing the data once is that every other package is now
one call away. Each converter reads the event date, report date, strata
and units off `hai_model`.

### baselinenowcast — a reporting triangle

``` r

library(baselinenowcast)

hai_triangle <- tbl_now_to_baselinenowcast(hai_model, verbose = FALSE)

hai_triangle[1:4, 1:6]
#>            0 1 2 3 4 5
#> 2020-03-09 1 0 0 0 0 0
#> 2020-03-16 0 0 0 0 0 0
#> 2020-03-23 0 0 0 0 0 0
#> 2020-03-30 0 0 0 0 0 0
```

``` r

bln_samples <- baselinenowcast(hai_triangle, output_type = "samples", draws = 1000)

bln_samples |>
  as_tibble() |>
  summarise(median = median(pred_count), .by = reference_date) |>
  tail(4)
#> # A tibble: 4 × 2
#>   reference_date median
#>   <date>          <dbl>
#> 1 2023-01-09          0
#> 2 2023-01-16          2
#> 3 2023-01-23          3
#> 4 2023-01-30          0
```

### surveillance — the classic line-list nowcast

``` r

library(surveillance)

hai_sur <- tbl_now_to_surveillance(hai_model, verbose = FALSE)
sur_now <- get_now(hai_model)

sur_fit <- nowcast(
  now          = sur_now,
  when         = seq(sur_now - 7 * 8, sur_now, by = "1 week"),
  data         = hai_sur,
  dEventCol    = "dHospital",
  dReportCol   = "dReport",
  aggregate.by = "1 week",
  D            = 20,
  method       = "bayes.notrunc.bnb"
)
#> Building reporting triangle...
#> No. cases:  462
#> No. cases within moving window:  462 
#> bayes.notrunc.bnb...

tibble::tibble(
  event_week = as.Date(epoch(sur_fit)),
  nowcast    = as.numeric(upperbound(sur_fit))
) |>
  tail(4)
#> # A tibble: 4 × 2
#>   event_week nowcast
#>   <date>       <dbl>
#> 1 2023-01-02       0
#> 2 2023-01-09       0
#> 3 2023-01-16       3
#> 4 2023-01-23       4
```

### NobBS — Nowcasting by Bayesian Smoothing

`NobBS` takes a plain line list, which a `linelist` `tbl_now` already is
— and even its arguments come off the object.

``` r

library(NobBS)

nobbs_fit <- NobBS(
  data        = as.data.frame(hai_model),
  now         = get_now(hai_model),
  units       = "1 week",
  onset_date  = get_event_date(hai_model),
  report_date = get_report_date(hai_model),
  max_D       = 20
)

nobbs_fit$estimates |> tail(4)
#>     estimate lower upper q_0.025 q_0.25 q_0.5 q_0.75 q_0.975 onset_date n.reported
#> 149        0     0     2       0      0     0      1       2 2023-01-09         NA
#> 150        2     2     4       2      2     2      3       4 2023-01-16          2
#> 151        3     3     5       3      3     3      4       5 2023-01-23          3
#> 152        1     0     3       0      0     1      1       3 2023-01-30         NA
```

### epinowcast — a Bayesian model with delay and reference modules

`epinowcast` compiles a Stan model, so the fit below is not run when
this page is built; the conversion is.

``` r

library(epinowcast)

hai_enw <- tbl_now_to_epinowcast(hai_model, verbose = FALSE, quiet = TRUE)

hai_enw
#> ── Preprocessed nowcast data ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> Groups: 3 (icu_type) | Timestep: week | Max delay: 36 
#> Observations: 152 timepoints x 456 snapshots 
#> Max date: 2023-01-30 
#> 
#> Datasets (access with `enw_get_data(x, "<name>")`): 
#>   obs                :  14,526 x 8 
#>   new_confirm        :  14,526 x 10 
#>   latest             :     456 x 9 
#>   missing_reference  :       0 x 5 
#>   reporting_triangle :     456 x 38 
#>   metareference      :     456 x 8 
#>   metareport         :     561 x 11 
#>   metadelay          :      36 x 5
```

``` r

epinowcast(
  hai_enw,
  fit = enw_fit_opts(pp = TRUE, chains = 2, iter_sampling = 500, iter_warmup = 500)
)
```

For a side-by-side comparison of all of these engines on one dataset,
with a held-back ground truth, see the [nowcasting-models
article](https://rodrigozepeda.github.io/tbl.now/articles/nowcasting-models.html).

## Summary

The workflow, start to finish:

``` r

# 1. Clean, checking what actually breaks a nowcast.
hai <- hai_bucaramanga |>
  distinct() |>                                       # duplicate records
  filter(!is.na(specimen_date), !is.na(report_date)) |>  # missing dates
  filter(report_date >= specimen_date)                # report before event

# 2. Choose a resolution the data (and your engine) can support.
hai_weekly <- hai |>
  mutate(event_week  = floor_date(specimen_date, "week", week_start = 1),
         report_week = floor_date(report_date,   "week", week_start = 1))

# 3. Describe it once.
hai_now <- tbl_now(hai_weekly, event_date = event_week, report_date = report_week,
                   strata = icu_type, data_type = "linelist")

# 4. Look at it, and test the delay for drift and change points.
autoplot(hai_now)
test_delay_drift(hai_now)
test_delay_changepoint(hai_now)

# 5. Let those findings set the model window and the maximum delay.
hai_model <- hai_now |> censor_delays_above(max_delay = 20)

# 6. Hand it to whichever engine you like.
diseasenowcasting::nowcast(hai_model)         # diseasenowcasting
tbl_now_to_baselinenowcast(hai_model)         # baselinenowcast
tbl_now_to_epinowcast(hai_model)              # epinowcast
tbl_now_to_surveillance(hai_model)            # surveillance
as.data.frame(hai_model)                      # NobBS
```

Two closing points worth carrying to your own data.

**The diagnostics were not decoration.** The change-point test found a
real break in March 2020 and that set the model window; the delay
quantiles set `max_delay`; the day-of-week check *stopped* us adding a
term the data did not support. Had we skipped straight to
[`nowcast()`](https://rdrr.io/pkg/surveillance/man/nowcast.html), all
three would have been guesses.

**Half of this dataset never reached the model.** Duplicate records,
missing dates and impossible delays removed 750 of 1,423 rows. A nowcast
built on the remainder is only as good as the assumption that what was
dropped resembles what was kept — which for the missing dates,
concentrated in the early years, is worth stating out loud rather than
burying.

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
