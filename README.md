
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Tibble now (tbl.now)

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/RodrigoZepeda/tbl.now/graph/badge.svg)](https://app.codecov.io/gh/RodrigoZepeda/tbl.now)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/tbl.now)](https://CRAN.R-project.org/package=tbl.now)
[![R-CMD-check](https://github.com/RodrigoZepeda/tbl.now/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RodrigoZepeda/tbl.now/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of tbl.now is to provide a unified `tibble`-like interface for
nowcasting algorithms within the
[`diseasenowcasting`](https://rodrigozepeda.github.io/diseasenowcasting/)
framework.

## Installation

You can install the development version of tbl.now from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("RodrigoZepeda/tbl.now")
```

## Class

The `tbl.now` package contains the `tbl.now` class which extends the
`tbl` class from ([`tibble()`](https://tibble.tidyverse.org/)). Nowcasts
require an `event_date` and a `report_date` thus the`tbl.now` class can
be thought of as a specific `tibble` that **guarantees** an `event_date`
and a `report_date`. It also **tags** variables as: covariates, strata
or as a batched indicator. Finally it converts event and report dates to
numeric.

Here is an example with a fake linelist data.frame where cases have
happened at `symptom_onset` but have been reported at
``` report_time``. The tibble includes ```sex`and`age_group`as covariate,`state`as strata and an unused variable called`comorbidity\`.

``` r
library(tbl.now)
library(dplyr, quietly = TRUE)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

#This is what the dataframe looks like
df <- tibble(
  patient_id  = 1:5,
  symptom_onset = c(as.Date("2020/12/01"), as.Date("2020/12/01"), as.Date("2020/12/01"), as.Date("2020/12/02"), as.Date("2020/12/02")),
  report_time   = c(as.Date("2020/12/01"), as.Date("2020/12/01"), as.Date("2020/12/02"), as.Date("2020/12/02"), as.Date("2020/12/03")),
  sex           = c("M", "F", "F", "M", "F"),
  age_group     = c("20-40","40-60","20-40","40-60","40-60"),
  state         = c("NY","NJ", "NY","NY","NY"),
  comorbidity   = c(T,F,F,F,T)
)

df
#> # A tibble: 5 × 7
#>   patient_id symptom_onset report_time sex   age_group state comorbidity
#>        <int> <date>        <date>      <chr> <chr>     <chr> <lgl>      
#> 1          1 2020-12-01    2020-12-01  M     20-40     NY    TRUE       
#> 2          2 2020-12-01    2020-12-01  F     40-60     NJ    FALSE      
#> 3          3 2020-12-01    2020-12-02  F     20-40     NY    FALSE      
#> 4          4 2020-12-02    2020-12-02  M     40-60     NY    FALSE      
#> 5          5 2020-12-02    2020-12-03  F     40-60     NY    TRUE
```

and transform it to `tbl_now` involves specifying the date of the event,
the date of the report, the strata and the covariates.

``` r
vdat <- tbl_now(df,
                event_date  = "symptom_onset",
                report_date = "report_time", 
                covariate   = c("sex", "age_group"),  
                strata      = "state")     
#> ℹ Identified data as linelist-data where each observation is a test.

vdat
#> # A tibble:  5 × 9
#> # Data type: "linelist"
#> # Frequency: Event: `days` | Report: `days`
#>   patient_id symptom_onset report_time   sex         age_group state comorbidity
#>        <int> <date>        <date>        <chr>       <chr>     <chr> <lgl>      
#>        [...] [event_date]  [report_date] [covariate] [covaria… [str… [...]      
#> 1          1 2020-12-01    2020-12-01    M           20-40     NY    TRUE       
#> 2          2 2020-12-01    2020-12-01    F           40-60     NJ    FALSE      
#> 3          3 2020-12-01    2020-12-02    F           20-40     NY    FALSE      
#> 4          4 2020-12-02    2020-12-02    M           40-60     NY    FALSE      
#> 5          5 2020-12-02    2020-12-03    F           40-60     NY    TRUE       
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2020-12-03 | Event date: "symptom_onset" | Report date: "report_time"
#> # Strata: "state"
#> # Covariates: "sex" and "age_group"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 2 more variables: .event_num <dbl>, .report_num <dbl>
```

The algorithm automatically infers it corresponds to `linelist` data (vs
count) and infers that the now is 2020-12-03 (the last date). The usual
`dplyr` operations can be performed on the dataset with it adjusting on
the fly. For example, removing the `age_group` removes it from the
covariate list at the bottom:

``` r
vdat %>% select(-age_group)
#> # A tibble:  5 × 8
#> # Data type: "linelist"
#> # Frequency: Event: `days` | Report: `days`
#>   patient_id symptom_onset report_time   sex        state comorbidity .event_num
#>        <int> <date>        <date>        <chr>      <chr> <lgl>            <dbl>
#>        [...] [event_date]  [report_date] [covariat… [str… [...]            [...]
#> 1          1 2020-12-01    2020-12-01    M          NY    TRUE                 0
#> 2          2 2020-12-01    2020-12-01    F          NJ    FALSE                0
#> 3          3 2020-12-01    2020-12-02    F          NY    FALSE                0
#> 4          4 2020-12-02    2020-12-02    M          NY    FALSE                1
#> 5          5 2020-12-02    2020-12-03    F          NY    TRUE                 1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2020-12-03 | Event date: "symptom_onset" | Report date: "report_time"
#> # Strata: "state"
#> # Covariates: "sex"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 1 more variable: .report_num <dbl>
```

While changing the structure in a way that removes the event and report
dates returns a `tibble`:

``` r
vdat %>% 
  group_by(sex) %>% 
  tally()
#> Warning: Dropping `tbl_now` attributes and converting to `tibble`
#> # A tibble: 2 × 2
#>   sex       n
#>   <chr> <int>
#> 1 F         3
#> 2 M         2
```
