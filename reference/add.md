# Add attributes to a `tbl_now` object

Functions to add the attributes to a `tbl_now` object.

## Usage

``` r
add_is_censored(x, is_censored)

add_strata(x, ...)

add_covariates(x, ...)
```

## Arguments

- x:

  A `tbl_now` object

- is_censored:

  (optional)
  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  or `NULL` (default). The name of a column containing either `TRUE` or
  `FALSE` indicating whether the `report_date` is correctly specified or
  corresponds to a `batch` and thus is censored. In other words, if the
  `report_date` is accurately measured set `is_censored = FALSE` but if
  the `report_date` corresponds to an error and is only an upper bound
  of the real report date set `is_censored = TRUE`.

- ...:

  [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  with the columns for the attribute. In the case of `covariates` and
  `strata` argument `...` can refer to multiple columns.

## Value

A `tbl_now` object with updated attributes

## Details

Variable selection can be used with the auxiliary dplyr verbs such as
[`dplyr::starts_with()`](https://dplyr.tidyverse.org/reference/reexports.html),
[`dplyr::all_of()`](https://dplyr.tidyverse.org/reference/reexports.html),
and
[`dplyr::where()`](https://dplyr.tidyverse.org/reference/reexports.html).
See
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
for additional info.

## See also

[`add_temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/add_temporal_effects.md)
[change](https://rodrigozepeda.github.io/tbl.now/reference/change.md)
[remove](https://rodrigozepeda.github.io/tbl.now/reference/remove.md)

## Examples

``` r
data(denguedat)
ndata <- tbl_now(denguedat,
                 event_date = onset_week,
                 report_date = report_week,
                 verbose = FALSE)

# Add strata
ndata <- ndata %>% add_strata(dplyr::starts_with("gender"))
ndata
#> # A tibble:  52,987 × 6
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week   gender   .event_num .report_num .delay
#>    <date>       <date>        <chr>         <dbl>       <dbl>  <dbl>
#>    [event_date] [report_date] [strata]      [...]       [...]  [...]
#>  1 1990-01-01   1990-01-01    Male              0           0      0
#>  2 1990-01-01   1990-01-01    Female            0           0      0
#>  3 1990-01-01   1990-01-01    Female            0           0      0
#>  4 1990-01-01   1990-01-08    Female            0           1      1
#>  5 1990-01-01   1990-01-08    Male              0           1      1
#>  6 1990-01-01   1990-01-15    Female            0           2      2
#>  7 1990-01-01   1990-01-15    Female            0           2      2
#>  8 1990-01-01   1990-01-15    Female            0           2      2
#>  9 1990-01-01   1990-01-22    Female            0           3      3
#> 10 1990-01-01   1990-01-08    Female            0           1      1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "onset_week" | Report date: "report_week"
#> # Strata: "gender"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows

# Add covariates
ndata$temperature <- rnorm(nrow(ndata), 25, 4)
ndata$humidity    <- rbeta(nrow(ndata), 0.6, 0.4)
ndata <- ndata %>% add_covariates(temperature, humidity)
ndata
#> # A tibble:  52,987 × 8
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week   gender   .event_num .report_num .delay temperature
#>    <date>       <date>        <chr>         <dbl>       <dbl>  <dbl>       <dbl>
#>    [event_date] [report_date] [strata]      [...]       [...]  [...] [covariate]
#>  1 1990-01-01   1990-01-01    Male              0           0      0        19.4
#>  2 1990-01-01   1990-01-01    Female            0           0      0        26.0
#>  3 1990-01-01   1990-01-01    Female            0           0      0        15.3
#>  4 1990-01-01   1990-01-08    Female            0           1      1        25.0
#>  5 1990-01-01   1990-01-08    Male              0           1      1        27.5
#>  6 1990-01-01   1990-01-15    Female            0           2      2        29.6
#>  7 1990-01-01   1990-01-15    Female            0           2      2        17.7
#>  8 1990-01-01   1990-01-15    Female            0           2      2        24.0
#>  9 1990-01-01   1990-01-22    Female            0           3      3        24.0
#> 10 1990-01-01   1990-01-08    Female            0           1      1        23.9
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "onset_week" | Report date: "report_week"
#> # Strata: "gender"
#> # Covariates: "temperature" and "humidity"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows
#> # ℹ 1 more variable: humidity <dbl>
```
