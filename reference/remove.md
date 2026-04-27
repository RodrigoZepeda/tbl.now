# Remove attributes from a `tbl_now` object

Functions to remove the attributes to a `tbl_now` object.

## Usage

``` r
remove_is_censored(x)

remove_strata(x, ...)

remove_all_strata(x)

remove_covariates(x, ...)

remove_all_covariates(x)

replace_temporal_effects(x, t_effects)

remove_temporal_effects(x)
```

## Arguments

- x:

  A `tbl_now` object

- ...:

  [`` <`tidy-select`> ``](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  with the columns for the attribute. In the case of `covariates` and
  `strata` argument `...` can refer to multiple columns.

- t_effects:

  (optional) Either `NULL` (default), a
  [`temporal_effects()`](https://rodrigozepeda.github.io/tbl.now/reference/temporal_effects.md)
  object or a character vector with the names of the columns containing
  the temporal effects.

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
[add](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
[change](https://rodrigozepeda.github.io/tbl.now/reference/change.md)

## Examples

``` r
data(denguedat)
ndata <- tbl_now(denguedat,
                 event_date = onset_week,
                 report_date = report_week,
                 strata = gender,
                 verbose = FALSE)
#> Warning: Please use a `temporal_effects` object. Setting from colnames is not
#> recommended and could lead to unexpected behaviour.

# Add strata
ndata <- remove_strata(ndata, gender)
ndata
#> # A tibble:  52,987 × 6
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week   gender .event_num .report_num .delay
#>    <date>       <date>        <chr>       <dbl>       <dbl>  <dbl>
#>    [event_date] [report_date] [...]       [...]       [...]  [...]
#>  1 1990-01-01   1990-01-01    Male            0           0      0
#>  2 1990-01-01   1990-01-01    Female          0           0      0
#>  3 1990-01-01   1990-01-01    Female          0           0      0
#>  4 1990-01-01   1990-01-08    Female          0           1      1
#>  5 1990-01-01   1990-01-08    Male            0           1      1
#>  6 1990-01-01   1990-01-15    Female          0           2      2
#>  7 1990-01-01   1990-01-15    Female          0           2      2
#>  8 1990-01-01   1990-01-15    Female          0           2      2
#>  9 1990-01-01   1990-01-22    Female          0           3      3
#> 10 1990-01-01   1990-01-08    Female          0           1      1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "onset_week" | Report date: "report_week"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows

# Change covariates
ndata$temperature <- rnorm(nrow(ndata), 25, 4)
ndata$humidity    <- rbeta(nrow(ndata), 0.6, 0.4)
ndata <- ndata %>% add_covariates(temperature, humidity)
ndata
#> # A tibble:  52,987 × 8
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week   gender .event_num .report_num .delay temperature
#>    <date>       <date>        <chr>       <dbl>       <dbl>  <dbl>       <dbl>
#>    [event_date] [report_date] [...]       [...]       [...]  [...] [covariate]
#>  1 1990-01-01   1990-01-01    Male            0           0      0        26.3
#>  2 1990-01-01   1990-01-01    Female          0           0      0        25.7
#>  3 1990-01-01   1990-01-01    Female          0           0      0        23.1
#>  4 1990-01-01   1990-01-08    Female          0           1      1        25.0
#>  5 1990-01-01   1990-01-08    Male            0           1      1        25.8
#>  6 1990-01-01   1990-01-15    Female          0           2      2        28.2
#>  7 1990-01-01   1990-01-15    Female          0           2      2        24.1
#>  8 1990-01-01   1990-01-15    Female          0           2      2        26.6
#>  9 1990-01-01   1990-01-22    Female          0           3      3        27.2
#> 10 1990-01-01   1990-01-08    Female          0           1      1        21.1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "onset_week" | Report date: "report_week"
#> # Covariates: "temperature" and "humidity"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows
#> # ℹ 1 more variable: humidity <dbl>

ndata %>% remove_covariates(temperature, humidity)
#> # A tibble:  52,987 × 8
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    onset_week   report_week   gender .event_num .report_num .delay temperature
#>    <date>       <date>        <chr>       <dbl>       <dbl>  <dbl>       <dbl>
#>    [event_date] [report_date] [...]       [...]       [...]  [...]       [...]
#>  1 1990-01-01   1990-01-01    Male            0           0      0        26.3
#>  2 1990-01-01   1990-01-01    Female          0           0      0        25.7
#>  3 1990-01-01   1990-01-01    Female          0           0      0        23.1
#>  4 1990-01-01   1990-01-08    Female          0           1      1        25.0
#>  5 1990-01-01   1990-01-08    Male            0           1      1        25.8
#>  6 1990-01-01   1990-01-15    Female          0           2      2        28.2
#>  7 1990-01-01   1990-01-15    Female          0           2      2        24.1
#>  8 1990-01-01   1990-01-15    Female          0           2      2        26.6
#>  9 1990-01-01   1990-01-22    Female          0           3      3        27.2
#> 10 1990-01-01   1990-01-08    Female          0           1      1        21.1
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2010-12-20 | Event date: "onset_week" | Report date: "report_week"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 52,977 more rows
#> # ℹ 1 more variable: humidity <dbl>
```
