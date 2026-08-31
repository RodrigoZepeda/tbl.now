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

A data frame with 452,567 rows and 4 variables.

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

## Examples

``` r
data(flusight)
```
