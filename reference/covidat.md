# covidat: COVID-19 reporting data from Mexico City (2020-2022)

Surveillance count data provided by the Mexico City's Health Ministry at
https://datos.cdmx.gob.mx/dataset/base-covid-sinave. Data comes from the
National System of Epidemiologic Surveillance (SINAVE) and corresponds
to the update of January 9th 2024, 12:35 (UTC-05:00). The dataset
includes all confirmed COVID-19 cases registered in the city. The
original dataset was aggregated and pre-processed as described in the
note below.

## Usage

``` r
data(covidat)
```

## Format

A data frame.

## Details

This is count data with each row representing case counts. The columns
are as follows:

- `date_of_registry`: date when the report was registered on the
  database.

- `date_of_symptom_onset`: date when the patient reported symptoms
  started.

- `sex`: biological sex (either male or female)

- `n`: the case count of individuals within those dates for each sex

## Note

Dates are provided as present in the dataset and contain errors.
Theoretically no individual should have a `date_of_symptom_onset` after
their `date_of_registry`; however the database does contain cases like
that which are close to what practitioners find in the wild.

## Examples

``` r
data(covidat)
```
