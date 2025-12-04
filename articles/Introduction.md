# tbl.now

``` r
library(dplyr)
library(lubridate)
library(tbl.now)
```

## Introduction

In `R`, one of the main frameworks for data analysis is the
[tidyverse](https://tidyverse.org/) ([Wickham et al.
2019](#ref-tidyverse)). Within it, data is arranged with **rows**
corresponding to single observations and **columns** representing
different variables. This is referred to as [tidy
data](https://tidyr.tidyverse.org/articles/tidy-data.html) ([Wickham
2014](#ref-wickham2014tidy)).

For time series, tidy extensions to the classical `data.frame` (or
[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)) have
previously been proposed with the
[tsibble](https://tsibble.tidyverts.org/), the
[tibbletime](https://business-science.github.io/tibbletime/), and the
[timetk](https://business-science.github.io/timetk/) packages ([E. Wang,
Cook, and Hyndman 2020](#ref-wang2020new); [Y. Wang
2019](#ref-wang2019tidy); [Dancho and Vaughan 2023](#ref-timetk)).
However, in the context of epidemiological nowcasting were two different
time-indexes are on the same table, the previous methods are lacking.
This is where `tbl.now` comes in.

The tibble now (`tbl.now`) is an extension fo the
[`tibble`](https://tibble.tidyverse.org/reference/tibble.html) that tags
specific variables as indexes for nowcasting models in the context of
[`diseasenowcasting`](https://rodrigozepeda.github.io/diseasenowcasting/).
The main purpose of the `tbl.now` is to unify the data inputted to
[`diseasenowcasting`](https://rodrigozepeda.github.io/diseasenowcasting/)’s
models while allowing the user to keep a `tidy` structure on the data
and perform the usual \[`dplyr`\] data cleaning operations. This allows
the user to move easierly into one of the classical modeling workflow
frameworks such as ([Gelman et al. 2020](#ref-gelman2020bayesian);
[Wickham, Çetinkaya-Rundel, and Grolemund 2023](#ref-wickham2023r)):

    Data Cleaning -> Modeling -> New Data Cleaning -> New Modeling -> ...

## The `tbl.now` framework

In epidemiological nowcasting scenarios we have at least two dates:

- `event_date`: When something happened (*e.g.* symptom onset or a test
  was taken).

- `report_date`: When it was reported (*e.g.* the patient visited a
  physician or the test results were registered).

The nowcasting problem is:

> To estimate the total number of events **now** that have occurred at
> any **past** `event_date` given that not all of them have been
> reported yet (i.e. some \``report_date`s are potentially in the
> future).

In the context of nowcasting, the `tbl_now` can be thought of as a
specific
[`tibble()`](https://tibble.tidyverse.org/reference/tibble.html) that
guarantees an `event_date` and a `report_date.` by tagging them as
**attributes** and allows for additional operations such as delay
calculation.

### Example

The `tbl_now` works from a `data.frame` by specifying the `event_date`
and `report_date` columns:

``` r
df <- data.frame(
  symptom_onset = c(ymd("2023/12/25"), ymd("2023/12/26"), ymd("2023/12/25"), ymd("2023/12/26")),
  medical_visit = c(ymd("2023/12/26"), ymd("2023/12/26"), ymd("2023/12/27"), ymd("2023/12/27")),
  n = c(10, 2, 5, 11)
)

df
#>   symptom_onset medical_visit  n
#> 1    2023-12-25    2023-12-26 10
#> 2    2023-12-26    2023-12-26  2
#> 3    2023-12-25    2023-12-27  5
#> 4    2023-12-26    2023-12-27 11
```

In the previous `data.frame`, column **symptom_onset** represents the
`event` while **medical_visit** corresponds to the `report`. Finally `n`
corresponds to the number of cases which can be specified in the
`case_count` variable.

The previous options can be specified in the `tbl_now`:

``` r
df %>% 
  tbl_now(event_date = symptom_onset, report_date = medical_visit, case_count = n)
#> ℹ Identified data as <count-incidence> with counts in column "n".
#> # A tibble:  4 × 6
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>   symptom_onset medical_visit       n .event_num .report_num .delay
#>   <date>        <date>          <dbl>      <dbl>       <dbl>  <dbl>
#>   [event_date]  [report_date] [cases]      [...]       [...]  [...]
#> 1 2023-12-25    2023-12-26         10          0           1      1
#> 2 2023-12-26    2023-12-26          2          1           1      0
#> 3 2023-12-25    2023-12-27          5          0           2      2
#> 4 2023-12-26    2023-12-27         11          1           2      1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2023-12-27 | Event date: "symptom_onset" | Report date: "medical_visit"
#> # ────────────────────────────────────────────────────────────────────────────────
```

Notice that the \[tbl_now()\] function does several things at once:

- It auto-detects the **data-type** as `count-incidence`. Other data
  types such as `linelist` are available and are discussed in section
  @ref(data-types).

- It auto-detects the **units (frequency)** of the event and report
  dates as ‘daily’ for both.

- It **tags** `symptom_onset` as an `event_date`, `medical_visit` as a
  `report_date` and `n` as `cases`.

- It generates the `.event_num` and `.report_num` columns which are
  numerical versions of the `event_date` and `report_date` columns.

- It generates the `.delay` column with the delay given by the
  difference between report and event:
  `.delay = report_date - event_date`.

- It automatically detects the **now** for the nowcasting.

The following sections explain each of these perks of the `tbl.now`
framework as well as additional functions that can be applied to a
`tbl.now`.

### Attributes of a `tbl_now`

#### Data types

#### Temporal effects

#### Modifying the attributes of a `tbl_now`

#### Transforming a `tbl_now` to count data

#### Modifying a `tbl_now` with `dplyr`

#### Updating a `tbl_now`

## Other functions

## References

Dancho, Matt, and Davis Vaughan. 2023. *Timetk: A Tool Kit for Working
with Time Series*. <https://doi.org/10.32614/CRAN.package.timetk>.

Gelman, Andrew, Aki Vehtari, Daniel Simpson, Charles C Margossian, Bob
Carpenter, Yuling Yao, Lauren Kennedy, Jonah Gabry, Paul-Christian
Bürkner, and Martin Modrák. 2020. “Bayesian Workflow.” *arXiv Preprint
arXiv:2011.01808*.

Wang, Earo, Dianne Cook, and Rob J Hyndman. 2020. “A New Tidy Data
Structure to Support Exploration and Modeling of Temporal Data.”
*Journal of Computational and Graphical Statistics* 29 (3): 466–78.

Wang, Yiru. 2019. “Tidy Tools for Supporting Fluent Workflow in Temporal
Data Analysis.” PhD thesis, Monash University.

Wickham, Hadley. 2014. “Tidy Data.” *Journal of Statistical Software*
59: 1–23.

Wickham, Hadley, Mara Averick, Jennifer Bryan, Winston Chang, Lucy
D’Agostino McGowan, Romain François, Garrett Grolemund, et al. 2019.
“Welcome to the tidyverse.” *Journal of Open Source Software* 4 (43):
1686. <https://doi.org/10.21105/joss.01686>.

Wickham, Hadley, Mine Çetinkaya-Rundel, and Garrett Grolemund. 2023. *R
for Data Science: Import, Tidy, Transform, Visualize, and Model Data*.
O’Reilly Media, Inc.
