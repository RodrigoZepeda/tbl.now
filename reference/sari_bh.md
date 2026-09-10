# sari_bh: Severe Acute Respiratory Illness data from Belo Horizonte (Brazil), 2020-2022

An anonymized dataset containing cases of Severe Acute Respiratory
Illness (SARI) cases from the Brazilian municipality, Belo Horizonte,
with symptom onset varying from 2019-12-29 to 2022-03-27.

## Usage

``` r
sari_bh
```

## Format

A data frame with 65404 rows and 5 variables:

- symptom_onset_date:

  Date of onset symptoms, formatted in ISO8601 as YYYY-MM-DD.

- record_date:

  Date the case was recorded in the surveillance system, formatted in
  ISO8601 as YYYY-MM-DD. May be `NA` when no record date was available.

- final_classification:

  Final classification of the case: one of `"Influenza"`,
  `"Other respiratory virus"`, `"Other ethiological agent"`,
  `"Not specified"`, or `"COVID-19"`.

- case_evolution:

  Case evolution outcome: one of `"Cured"`, `"Dead"`,
  `"Dead (other causes)"`, `"Unknown"`, or `"Not in dictionary"`.

- age_yrs:

  Age of the patient in years.

## Source

Redistributed from the nowcaster package
(<https://github.com/covid19br/nowcaster>), which derives it from the
Brazilian Ministry of Health's SIVEP-Gripe respiratory surveillance
system, published on the OpenDataSUS portal.

## References

From the `nowcaster` package. Bastos, S L, Economou, Theodoros, Gomes,
FC M, Villela, AM D, Coelho, C F, Cruz, G O, Stoner, Oliver, Bailey,
Trevor, Codeço, T C (2019). “A Modelling approach for correcting
reporting delays in disease surveillance data.” *Statistics in
medicine*, *38*(22), 4363-4377.
[doi:10.1002/sim.8303](https://doi.org/10.1002/sim.8303)
