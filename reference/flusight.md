# flusight:

NHSN Weekly Hospital Respiratory Data from FluSight FluSight's weekly
hospital admission prediction targets based on the 'total number of new
hospital admissions of patients with confirmed influenza captured during
the reporting week' reported through CDC's NHSN (the dataset formerly
known as HHS-Protect), Weekly Hospital Respiratory Data. Data was
downloaded on November 12th 2025.

## Usage

``` r
data(flusight)
```

## Format

A data frame.

## Details

Data represents how many cases were considered influenza during the week
of *target_end_date* given the information known until week *as_of*.
Note that *as_of* is always one week ahead of *target_end_date*.

This is count data. The columns are as follows:

- `as_of`: The

- `target_end_date`: The date where the case happened

- `observation`: Case counts for those dates

- `location_name`: State, district or territory

## References

Target data from Flusight. Online:
<https://github.com/cdcepi/FluSight-forecast-hub/blob/main/target-data/time-series.csv>

## Examples

``` r
data(flusight)
```
