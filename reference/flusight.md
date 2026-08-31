# flusight: NHSN Weekly Hospital Respiratory Data from FluSight

FluSight's weekly hospital admission prediction targets based on the
'total number of new hospital admissions of patients with confirmed
influenza captured during the reporting week' reported through CDC's
NHSN (the dataset formerly known as HHS-Protect), Weekly Hospital
Respiratory Data. Data was downloaded on November 12th 2025.

## Usage

``` r
data(flusight)
```

## Format

A data frame with 452,567 rows and 4 variables:

- as_of:

  `Date`. The date this row's value was published – the *report* date.
  The same week appears many times, once per publication.

- target_end_date:

  `Date`. The week being reported on – the *event* date.

- location_name:

  `character`. US state or territory.

- observation:

  `numeric`. Hospital admissions reported for that week as of that
  publication date.

## Details

Data represents how many cases were considered influenza during the week
of *target_end_date* given the information known until week *as_of*.
Note that *as_of* is always one week ahead of *target_end_date*.

This is count data with 452,567 rows and 4 columns:

- `as_of`: The report date – the date the snapshot was taken, i.e. what
  was known as of that week.

- `target_end_date`: The event date – the week the admissions occurred.

- `location_name`: State, district or territory (53 levels).

- `observation`: Case counts for those dates. `NA` for 1,152 rows.

Together, `as_of`, `target_end_date` and `location_name` form a unique
key.

## Duplicate rows removed

The upstream FluSight `time-series.csv` ships exact duplicate rows –
39,139 of them in this snapshot. They were dropped with
[`dplyr::distinct()`](https://dplyr.tidyverse.org/reference/distinct.html)
before the dataset was saved (issue \#25), taking it from 491,706 to
452,567 rows.

The removal is lossless: every repeated (`as_of`, `target_end_date`,
`location_name`) key carried an *identical* `observation`, with no
conflicting values anywhere in the file, so no information was discarded
and the key became unique. If you download the upstream file yourself
you will still see the duplicates and should
[`distinct()`](https://dplyr.tidyverse.org/reference/distinct.html) them
before use.

## References

Target data from Flusight. Online:
<https://github.com/cdcepi/FluSight-forecast-hub/blob/main/target-data/time-series.csv>

## See also

[`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
to declare the date columns;
[summary()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_summary.md)
and
[`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
to inspect the result; the package's other datasets –
[denguedat](https://rodrigozepeda.github.io/tbl.now/reference/denguedat.md),
[mpoxdat](https://rodrigozepeda.github.io/tbl.now/reference/mpoxdat.md),
flusight,
[covid_colombia](https://rodrigozepeda.github.io/tbl.now/reference/covid_colombia.md),
[covid_us](https://rodrigozepeda.github.io/tbl.now/reference/covid_us.md)
and
[hai_bucaramanga](https://rodrigozepeda.github.io/tbl.now/reference/hai_bucaramanga.md).

## Examples

``` r
data(flusight)
head(flusight)
#> # A tibble: 6 × 4
#>   as_of      target_end_date location_name observation
#>   <date>     <date>          <chr>               <dbl>
#> 1 2023-09-23 2022-02-12      Alabama                10
#> 2 2023-09-23 2022-02-12      Alaska                  0
#> 3 2023-09-23 2022-02-12      Arizona                64
#> 4 2023-09-23 2022-02-12      Arkansas               29
#> 5 2023-09-23 2022-02-12      California             36
#> 6 2023-09-23 2022-02-12      Colorado               29

## This is count data: one row per (week, publication date, state).
nrow(flusight)
#> [1] 452567
length(unique(flusight$location_name))
#> [1] 53

# One state is enough to see the reporting process.
texas <- flusight[flusight$location_name == "Texas", ]
flu <- tbl_now(texas,
  event_date = target_end_date, report_date = as_of,
  case_count = observation, verbose = FALSE
)
flu
#> # A tibble:  8,539 × 7
#> # Data type: "count-incidence"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>    as_of        target_end_date location_name observation .event_num .report_num
#>    <date>       <date>          <chr>               <dbl>      <dbl>       <dbl>
#>    [report_dat… [event_date]    [...]             [cases]      [...]       [...]
#>  1 2023-09-23   2022-02-12      Texas                 164          1          85
#>  2 2023-09-23   2022-02-19      Texas                 266          2          85
#>  3 2023-09-23   2022-02-26      Texas                 350          3          85
#>  4 2023-09-23   2022-03-05      Texas                 334          4          85
#>  5 2023-09-23   2022-03-12      Texas                 438          5          85
#>  6 2023-09-23   2022-03-19      Texas                 639          6          85
#>  7 2023-09-23   2022-03-26      Texas                 637          7          85
#>  8 2023-09-23   2022-04-02      Texas                 538          8          85
#>  9 2023-09-23   2022-04-09      Texas                 364          9          85
#> 10 2023-09-23   2022-04-16      Texas                 355         10          85
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2025-11-12 | Event date: "target_end_date" | Report date: "as_of"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 8,529 more rows
#> # ℹ 1 more variable: .delay <dbl>

# `as_of` is not always the same weekday, so some delays are not whole weeks.
## `align_weeks()` fixes that.
mean(flu$.delay != round(flu$.delay))
#> [1] 0.08841785
```
