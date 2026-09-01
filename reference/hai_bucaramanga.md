# Healthcare-Associated Infections – Bucaramanga, Colombia 2016-2023

A line list of healthcare-associated infections (IAAS, *Infecciones
Asociadas a la Atencion en Salud*) notified in the municipality of
Bucaramanga, Santander, Colombia, between January 2016 and January 2023.
Each row is one notified infection: a specimen taken from a hospitalised
patient, the laboratory result, and the isolated microorganism.

## Usage

``` r
hai_bucaramanga
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with
1,423 rows and 13 variables:

- id:

  `integer`. The source's `Orden` autonumber. Documented as unique but
  repeated for 100 records (see above), so it is *not* a reliable key.

- specimen_date:

  `Date`. The event date – when the sample was taken from the patient.
  `NA` for 318 records.

- received_date:

  `Date`. When the laboratory received the sample. `NA` for 1,194
  records and absent after 2019-12-01.

- report_date:

  `Date`. The report date – when the laboratory result was issued. `NA`
  for 583 records.

- specimen:

  `factor`, 6 levels. Sample type: `"Whole blood"`, `"Urine"`,
  `"Secretions"`, `"Other sterile fluids"`, `"Sputum"`,
  `"Bronchoalveolar lavage"`. `NA` for clinically-confirmed cases.

- test:

  `factor`, 7 levels. Laboratory test performed, e.g. `"Blood culture"`,
  `"Urine culture"`. `NA` for clinically-confirmed cases.

- microorganism:

  `character`, 85 distinct values. The isolated organism, in
  conventional binomial form, e.g. `"Klebsiella pneumoniae"`. `NA` for
  clinically-confirmed cases. Where the source recorded only a genus the
  value is `"<Genus> spp."`.

- sex:

  `factor`. `"Female"` or `"Male"`.

- age_group:

  `ordered factor`, 11 levels from `"<1"` to `"70+"`.

- case_type:

  `factor`. `"Laboratory-confirmed"` (1,090) or `"Clinically-confirmed"`
  (333).

- final_condition:

  `factor`. Patient status on discharge: `"Alive"` or `"Dead"`. `NA` for
  8 records.

- icu_type:

  `factor`. Intensive-care unit where the case occurred: `"Adult"`,
  `"Paediatric"` or `"Neonatal"`.

- institution:

  `integer`, 1-10. Anonymised code of the reporting health institution.

## Source

Secretaria de Salud y Ambiente de Bucaramanga, via the Colombian
national open-data portal
<https://www.datos.gov.co/Salud-y-Protecci-n-Social/48-Infecciones-asociadas-a-la-atenci-n-en-salud-IA/w4zx-wbff/about_data>.
Retrieved 2026-08-17. Column names and categorical values translated
from Spanish; see `data-raw/hai_bucaramanga.R` for the mapping.

## Details

In the nowcasting context the **event date** is `specimen_date` (when
the sample was taken from the patient) and the **report date** is
`report_date` (when the laboratory result was issued). The delay between
the two is the specimen-to-result turnaround that a nowcasting model
would estimate and correct for. A third date, `received_date`, records
when the laboratory received the sample and splits the delay into a
transport and a processing leg – but see the warnings below before
relying on it.

The column names and all categorical values have been translated from
the original Spanish. The dataset has been trimmed to the columns
relevant to a delay or nowcasting analysis; see
`data-raw/hai_bucaramanga.R` in the package sources for the full
translation tables and the list of dropped columns.

## Data quality – read this first

This is a real, unpolished open-data extract, and it is included partly
*because* it is messy: it is a realistic exercise for the delay
diagnostics in this package
([`diagnose_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_drift.md),
[`diagnose_changepoint()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_changepoint.md),
[`plot_delay_profiles()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_profiles.md)).
Nothing below has been silently repaired.

- Missing dates:

  The source uses `1900-01-01` as an undocumented missing-date sentinel.
  It has been converted to `NA` here – left in, it produces delays of
  about -45,000 days. After conversion `specimen_date` is present for
  1,105 records (77.7%), `report_date` for 840 (59.0%) and
  `received_date` for only 229 (16.1%).

- `received_date` is largely unusable:

  Beyond being 84% missing, it stops entirely at 2019-12-01, so the
  three-date chain exists only for the first third of the study period.
  Prefer the `specimen_date` -\> `report_date` pair.

- Negative delays:

  88 records (10.7% of the otherwise-valid pairs) have a `report_date`
  *before* their `specimen_date`, by up to 331 days. These are kept
  exactly as recorded. Filter them out before fitting anything.

- Exact duplicates:

  100 records are byte-identical duplicates of another record, sharing
  the `id` that the source documents as a unique autonumber. All 1,423
  rows are shipped for fidelity; use
  [`dplyr::distinct()`](https://dplyr.tidyverse.org/reference/distinct.html)
  to reduce to the 1,323 unique records.

- Clinically-confirmed cases:

  For the 333 cases confirmed on clinical grounds rather than by
  laboratory, the source wrote the literal string
  `"CONFIRMADO POR CLINICA"` into the `muestra`, `nombre prueba` and
  `microorganismo` fields. Those are `NA` here; the information is
  preserved in `case_type`.

- Sparsity:

  Only 738 records (51.9%) support a non-negative `specimen_date` -\>
  `report_date` delay, spread over 488 distinct event dates – roughly
  1.5 cases per event date. That is thin for fitting a nowcasting model,
  though ample for delay diagnostics. Aggregating to weeks or months is
  usually necessary.

The delay distribution is strongly bimodal: the median is 3 days but the
90th percentile is 92 days, which makes this a useful test case for
delay diagnostics that assume a unimodal delay.

## See also

[`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
to declare the date columns, and
[add_validation_date()](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
for the third one this dataset has;
[`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
and
[`diagnose_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_drift.md),
which this dataset is deliberately messy enough to exercise; the
package's other datasets –
[denguedat](https://rodrigozepeda.github.io/tbl.now/reference/denguedat.md),
[mpoxdat](https://rodrigozepeda.github.io/tbl.now/reference/mpoxdat.md),
[flusight](https://rodrigozepeda.github.io/tbl.now/reference/flusight.md),
[covid_colombia](https://rodrigozepeda.github.io/tbl.now/reference/covid_colombia.md)
and
[covid_us](https://rodrigozepeda.github.io/tbl.now/reference/covid_us.md).

## Examples

``` r
data(hai_bucaramanga)

# The source ships 100 exact duplicates and a number of unusable rows.
# Reduce to unique records with a valid, non-negative reporting delay.
iaas_clean <- dplyr::distinct(hai_bucaramanga) |>
  dplyr::filter(
    !is.na(specimen_date), !is.na(report_date),
    report_date >= specimen_date
  )
nrow(iaas_clean)
#> [1] 673

# Roughly 1.5 cases per event date, so aggregate to weeks before building a
# tbl_now for anything model-shaped.
iaas_now <- tbl_now(
  iaas_clean,
  event_date  = "specimen_date",
  report_date = "report_date",
  verbose     = FALSE
)
iaas_now
#> # A tibble:  673 × 16
#> # Data type: "linelist"
#> # Frequency: Event: `days` | Report: `days`
#>       id specimen_date received_date report_date   specimen  test  microorganism
#>    <int> <date>        <date>        <date>        <fct>     <fct> <chr>        
#>    [...] [event_date]  [...]         [report_date] [...]     [...] [...]        
#>  1     3 2018-01-27    2018-01-27    2018-01-31    Whole bl… Bloo… Stenotrophom…
#>  2     4 2018-01-27    2018-01-27    2018-01-30    Whole bl… Bloo… Klebsiella p…
#>  3     5 2018-04-20    2018-04-20    2018-04-26    Urine     Urin… Klebsiella p…
#>  4     6 2018-01-22    2018-01-22    2018-01-25    Whole bl… Bloo… Klebsiella p…
#>  5     7 2018-01-02    2018-01-02    2018-03-02    Whole bl… Bloo… Klebsiella p…
#>  6     8 2018-06-28    2018-06-28    2018-06-30    Whole bl… Bloo… Acinetobacte…
#>  7    13 2018-10-03    2018-10-03    2018-12-03    Urine     Urin… Enterobacter…
#>  8    14 2018-05-03    2018-05-03    2018-08-03    Urine     Urin… Candida albi…
#>  9    15 2018-07-03    2018-07-03    2018-08-03    Whole bl… Bloo… Acinetobacte…
#> 10    16 2018-03-17    2018-03-17    2018-03-20    Urine     Urin… Staphylococc…
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2023-02-01 | Event date: "specimen_date" | Report date: "report_date"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 663 more rows
#> # ℹ 9 more variables: sex <fct>, age_group <ord>, case_type <fct>,
#> #   final_condition <fct>, icu_type <fct>, institution <int>, .event_num <dbl>,
#> #   .report_num <dbl>, .delay <dbl>

# The delay is strongly bimodal: a 3-day median with a long secondary mode.
quantile(iaas_now$.delay, c(0.5, 0.75, 0.9, 0.99), na.rm = TRUE)
#> 50% 75% 90% 99% 
#>   3  31  92 153 
```
