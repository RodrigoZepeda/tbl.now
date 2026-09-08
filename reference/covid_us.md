# covid_us: CDC COVID-19 Case Surveillance Public Use Data (2020)

A compact aggregation of the U.S. CDC's individual-level COVID-19 case
surveillance database. It is the package's worked example for two
different things: **batch reporting**, and the **revision process** –
the optional third date a surveillance record can carry.

## Usage

``` r
data(covid_us)
```

## Format

A data frame with 192,953 rows and six variables:

- onset_dt:

  `Date`. The event date – symptom onset.

- pos_spec_dt:

  `Date`. The report date – collection of the first positive specimen.

- cdc_report_dt:

  `Date`. The revision date – when the case was registered at CDC.

- current_status:

  `character`. CDC's classification, either
  `"Laboratory-confirmed case"` or `"Probable Case"`. Map it with
  `revision_levels` (see above).

- sex:

  `character`. `"Female"`, `"Male"`, `"Other"`, `"Unknown"` or
  `"Missing"`.

- n:

  `integer`. Number of cases sharing that combination.

## Source

Centers for Disease Control and Prevention (CDC), COVID-19 Response.
*COVID-19 Case Surveillance Public Use Data* (version date: June 21,
2024).
<https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data/vbim-akqf/about_data>.
COVID-19 case surveillance data are collected by jurisdictions and
reported voluntarily to CDC.

## Details

Each row is a unique (onset date, specimen date, CDC report date,
status, sex) combination with the number of cases `n`.

## The three dates

The source file carries four date columns. `cdc_case_earliest_dt` is
derived by CDC as the earliest of the others, and equals `onset_dt` for
99.997% of the rows kept here, so it is dropped as redundant. The three
that remain are the only chain that runs forward in time, and they map
onto the three roles a
[`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
knows about:

- `onset_dt`:

  the **event** – symptoms begin.

- `pos_spec_dt`:

  the **report** – the first positive specimen is collected, which is
  when the surveillance system first sees the case.

- `cdc_report_dt`:

  the **revision** – the case is registered at CDC with a status.

## `current_status` and `revision_levels`

`current_status` is kept in CDC's own words rather than recoded, because
translating it is exactly what `tbl_now(revision_levels = )` is for:

    revision_levels = c(
      "Laboratory-confirmed case" = "confirmed",
      "Probable Case"             = "pending"
    )

A *probable* case is one that met the clinical and epidemiological
criteria without meeting the laboratory-confirmed definition. Every row
here has a positive specimen, so "probable" means the specimen was
collected and the case was never laboratory-settled – `"pending"` in
this package's vocabulary. Note what is **not** there: CDC does not
withdraw cases, so `"retracted"` does not occur in this dataset. It is a
two-outcome revision process, and code that needs a retraction has to
look elsewhere.

The relationship between the outcome and the revision delay is real
rather than fabricated: probable cases are registered a median of 2 days
after the specimen, laboratory-confirmed ones 4 days.

## What was kept

Cases where all three dates are present, correctly ordered
(`onset_dt <= pos_spec_dt <= cdc_report_dt`) and falling entirely within
2020 – a self-consistent "as of the end of 2020" snapshot. Rows out of
order are data-entry errors; rows missing a date cannot be placed on the
chain at all. See `data-raw/covid_us.R` for the exact duckdb aggregation
of the 14 GB source file.

The reporting delay is enormous and heavily right-skewed: cases reached
CDC not smoothly but in large backlog dumps – a textbook batch-reporting
pattern that
[`diagnose_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches.md)
and
[`transport_discriminant()`](https://rodrigozepeda.github.io/tbl.now/reference/transport_discriminant.md)
recover.

## See also

[`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
to declare the date columns;
[add_revision_date()](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
to attach the third one to an object that has none;
[revised_cases](https://rodrigozepeda.github.io/tbl.now/reference/revised_cases.md)
to count the outcomes;
[summary()](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_summary.md)
and
[`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
to inspect the result; the package's other datasets –
[denguedat](https://rodrigozepeda.github.io/tbl.now/reference/denguedat.md),
[mpoxdat](https://rodrigozepeda.github.io/tbl.now/reference/mpoxdat.md),
[flusight](https://rodrigozepeda.github.io/tbl.now/reference/flusight.md),
[covid_colombia](https://rodrigozepeda.github.io/tbl.now/reference/covid_colombia.md)
and
[hai_bucaramanga](https://rodrigozepeda.github.io/tbl.now/reference/hai_bucaramanga.md).

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

data(covid_us)

# The three dates with CDC's labels translated to this package's vocabulary.
covid_us <- covid_us |>
  filter(onset_dt <= as.Date("2020-02-01"))

tn3 <- tbl_now(
  covid_us,
  event_date       = onset_dt,
  report_date      = pos_spec_dt,
  revision_date    = cdc_report_dt,
  revision_type    = current_status,
  revision_levels = c(
    "Laboratory-confirmed case" = "confirmed",
    "Probable Case"             = "pending"
  ),
  case_count = n,
  strata     = sex,
  data_type  = "count-incidence",
  verbose    = FALSE
)
```
