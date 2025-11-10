# vectordat: Vector borne reportable diseases from Mexico (Feb-Dec 2024)

Surveillance count data provided by Mexico's General Directorate of
Epidemiology (Direccion General de Epidemiologia) at
https://www.gob.mx/cms/uploads/attachment/file/965318/Datos_abiertos_historicos_etv_2024.pdf.
Data comes from the National System of Epidemiologic Surveillance
(SINAVE) and was downloaded on May 30th 2025. The dataset includes all
suspected vector-borne disease cases for the states of Veracruz,
Guerrero and Tabasco. The original dataset was aggregated and
pre-processed.

## Usage

``` r
data(vectordat)
```

## Format

A data frame.

## Details

This is count data with each row representing case counts. The columns
are as follows:

- `update`: date when the report was first registered on the database.

- `symptoms`: date when the patient reported symptoms started.

- `sex`: biological sex (either male or female)

- `n`: the case count of individuals within those dates for each sex

- `state`: state where the report was originated

## Note

Dates are provided as present in the dataset and contain errors.
Theoretically no individual should have an `update` after their
`symptoms`; however the database does contain cases like that which are
close to what practitioners find in the wild.

## Examples

``` r
data(vectordat)
```
