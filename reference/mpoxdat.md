# mpoxdat: Mpox reporting data from the 2022 New York City outbreak

Surveillance line list data provided by the New York City (NYC) Health
Department at https://github.com/nychealth/mpox_nowcast_eval, to
accompany a nowcasting performance evaluation (doi: 10.2196/56495).
Patients with a confirmed or probable mpox diagnosis or illness onset
from July 8 through September 30, 2022 were included. The original
dataset was aggregated and pre-processed as described in the note below.

## Usage

``` r
data(mpoxdat)
```

## Format

A data frame with 1,417 rows and 4 variables:

- dx_date:

  `Date`. Specimen collection date of the first positive result – the
  *event* date.

- dx_report_date:

  `Date`. When the Health Department received that result – the *report*
  date.

- race:

  `character`. Synthetic; see the note.

- n:

  `integer`. Number of cases with that combination.

## Details

This is **count** data: each row holds the number of cases sharing a
diagnosis date, a report date and a race. The columns are as follows:

- `dx_date`: is the specimen collection date of the first positive mpox
  laboratory result,

- `dx_report_date`: is the date the report of first positive mpox
  laboratory result was received by the NYC Health Department,

- `n`: the case count of individuals within those dates.

- `race`: the race corresponding to those cases. Race was randomly
  assigned with probabilities "Non-Hispanic White" = 0.309, "Hispanic" =
  0.283, "Black" = 0.202, "Asian" = 0.156, and "Other" = 0.05 which
  follow what has been reported for the US Census.

## Note

While `dx_date`, `dx_report_date` and `n` correspond to actual observed
data the `race` was constructed exclusively for the examples of this
package. It is a synthetic (simulated) variable and does not correspond
to any reality.

## References

ROHRER, Rebecca, et al. Nowcasting to Monitor Real-Time Mpox Trends
During the 2022 Outbreak in New York City: Evaluation Using Reportable
Disease Data Stratified by Race or Ethnicity. Online Journal of Public
Health Informatics, 2025, vol. 17, no 1, p. e56495.

## See also

[`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
to declare the date columns;
[summary()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_summary.md)
and
[`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
to inspect the result; the package's other datasets –
[denguedat](https://rodrigozepeda.github.io/tbl.now/reference/denguedat.md),
mpoxdat,
[flusight](https://rodrigozepeda.github.io/tbl.now/reference/flusight.md),
[covid_colombia](https://rodrigozepeda.github.io/tbl.now/reference/covid_colombia.md),
[covid_us](https://rodrigozepeda.github.io/tbl.now/reference/covid_us.md)
and
[hai_bucaramanga](https://rodrigozepeda.github.io/tbl.now/reference/hai_bucaramanga.md).

## Examples

``` r
data(mpoxdat)
head(mpoxdat)
#> # A tibble: 6 × 4
#> # Rowwise: 
#>   dx_date    dx_report_date race                   n
#>   <date>     <date>         <chr>              <int>
#> 1 2022-07-08 2022-07-12     Asian                  4
#> 2 2022-07-08 2022-07-12     Black                  6
#> 3 2022-07-08 2022-07-12     Hispanic               6
#> 4 2022-07-08 2022-07-12     Non-Hispanic White     6
#> 5 2022-07-08 2022-07-13     Asian                  2
#> 6 2022-07-08 2022-07-13     Black                  3

# Count data, and daily rather than weekly -- unlike denguedat.
mpox <- tbl_now(mpoxdat,
  event_date = dx_date, report_date = dx_report_date,
  case_count = n, strata = race, verbose = FALSE
)
mpox
#> # A tibble:  1,417 × 7
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>    dx_date      dx_report_date race              n .event_num .report_num .delay
#>    <date>       <date>         <chr>         <int>      <dbl>       <dbl>  <dbl>
#>    [event_date] [report_date]  [strata]      [cas…      [...]       [...]  [...]
#>  1 2022-07-08   2022-07-12     Asian             4          0           4      4
#>  2 2022-07-08   2022-07-12     Black             6          0           4      4
#>  3 2022-07-08   2022-07-12     Hispanic          6          0           4      4
#>  4 2022-07-08   2022-07-12     Non-Hispanic…     6          0           4      4
#>  5 2022-07-08   2022-07-13     Asian             2          0           5      5
#>  6 2022-07-08   2022-07-13     Black             3          0           5      5
#>  7 2022-07-08   2022-07-13     Hispanic          8          0           5      5
#>  8 2022-07-08   2022-07-13     Non-Hispanic…     5          0           5      5
#>  9 2022-07-08   2022-07-14     Black             1          0           6      6
#> 10 2022-07-08   2022-07-14     Hispanic          3          0           6      6
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2023-05-19 | Event date: "dx_date" | Report date: "dx_report_date"
#> # Strata: "race"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 1,407 more rows

# A short, sharp outbreak: about three months of data.
range(mpoxdat$dx_date)
#> [1] "2022-07-08" "2022-10-12"
sum(mpoxdat$n)
#> [1] 3323
```
