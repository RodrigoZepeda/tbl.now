# denguedat: Dengue fever individual-level reporting data from Puerto Rico

Surveillance data from CDC Division of Vector-Borne Diseases. 1990-2010
case reporting data included.

## Usage

``` r
data(denguedat)
```

## Format

A data frame with 52,987 rows (one per case) and 3 variables:

- onset_week:

  `Date`. The week symptoms began – the *event* date.

- report_week:

  `Date`. The week the case reached the surveillance system – the
  *report* date. Always on or after `onset_week`.

- gender:

  `character`. `"Male"` or `"Female"`. Synthetic; see the note.

## Details

Each row represents a case with the columns indicating the following:

- `onset_week`: the week of symptom onset.

- `report_week`: the week of case report.

- `gender`: the gender of the infected individual (randomly assigned
  with 0.5:0.5 probability of "Male"/"Female").

## Note

Data originally from the `NobBS` package. While `onset_week` and
`report_week` correspond to actual observed data the `gender` was
constructed exclusively for the examples of `NobBS`. It is a synthetic
(simulated) variable and does not correspond to any reality.

## References

MCGOUGH, Sarah F., et al. Nowcasting by Bayesian Smoothing: A flexible,
generalizable model for real-time epidemic tracking. PLoS computational
biology, 2020, vol. 16, no 4, p. e1007735.

## See also

[`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
to declare the date columns;
[summary()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_summary.md)
and
[`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
to inspect the result; the package's other datasets – denguedat,
[mpoxdat](https://rodrigozepeda.github.io/tbl.now/reference/mpoxdat.md),
[flusight](https://rodrigozepeda.github.io/tbl.now/reference/flusight.md),
[covid_colombia](https://rodrigozepeda.github.io/tbl.now/reference/covid_colombia.md),
[covid_us](https://rodrigozepeda.github.io/tbl.now/reference/covid_us.md)
and
[hai_bucaramanga](https://rodrigozepeda.github.io/tbl.now/reference/hai_bucaramanga.md).

## Examples

``` r
data(denguedat)
head(denguedat)
#>   onset_week report_week gender
#> 1 1990-01-01  1990-01-01   Male
#> 2 1990-01-01  1990-01-01 Female
#> 3 1990-01-01  1990-01-01 Female
#> 4 1990-01-01  1990-01-08 Female
#> 5 1990-01-01  1990-01-08   Male
#> 6 1990-01-01  1990-01-15 Female

# The two dates every nowcast needs. Weekly data, twenty years of it.
range(denguedat$onset_week)
#> [1] "1990-01-01" "2010-11-29"

# Declaring them turns the data frame into a tbl_now.
dengue <- tbl_now(denguedat,
  event_date = onset_week, report_date = report_week,
  strata = gender, verbose = FALSE
)
dengue
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

# Most cases arrive within a week or two of onset; a few take much longer.
summary(as.numeric(dengue$.delay))
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   0.000   1.000   1.000   1.738   2.000  26.000 
```
