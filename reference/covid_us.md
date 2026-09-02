# covid_us: CDC COVID-19 Case Surveillance Public Use Data (2020)

A compact aggregation of the U.S. CDC's individual-level COVID-19 case
surveillance database. It is the package's worked example for two
different things: **batch reporting**, and the **validation process** –
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

  `Date`. The validation date – when the case was registered at CDC.

- current_status:

  `character`. CDC's classification, either
  `"Laboratory-confirmed case"` or `"Probable Case"`. Map it with
  `validation_levels` (see above).

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

  the **validation** – the case is registered at CDC with a status.

## `current_status` and `validation_levels`

`current_status` is kept in CDC's own words rather than recoded, because
translating it is exactly what `tbl_now(validation_levels = )` is for:

    validation_levels = c(
      "Laboratory-confirmed case" = "confirmed",
      "Probable Case"             = "pending"
    )

A *probable* case is one that met the clinical and epidemiological
criteria without meeting the laboratory-confirmed definition. Every row
here has a positive specimen, so "probable" means the specimen was
collected and the case was never laboratory-settled – `"pending"` in
this package's vocabulary. Note what is **not** there: CDC does not
withdraw cases, so `"retracted"` does not occur in this dataset. It is a
two-outcome validation process, and code that needs a retraction has to
look elsewhere.

The relationship between the outcome and the validation delay is real
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
[add_validation_date()](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
to attach the third one to an object that has none;
[validated_cases](https://rodrigozepeda.github.io/tbl.now/reference/validated_cases.md)
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
data(covid_us)

# The two-date object: onset -> positive specimen.
tn <- tbl_now(
  covid_us,
  event_date  = onset_dt,
  report_date = pos_spec_dt,
  case_count  = n,
  strata      = sex,
  data_type   = "count-incidence",
  verbose     = FALSE
)
#> Warning: *Non-unique*: 170518 rows share an (onset_dt, pos_spec_dt) combination.
#> ℹ 2 columns "cdc_report_dt" and "current_status" are not declared, so they
#>   split each cell into several rows. Declare them with `strata = ` to model
#>   them separately, or `to_count()` to pool them away. The `tbl_now_to_()`
#>   converters pool undeclared columns for you, so this is a warning rather than
#>   an error.
tn
#> # A tibble:  192,953 × 9
#> # Data type: "count-incidence"
#> # Frequency: Event: `days` | Report: `days`
#>    onset_dt     pos_spec_dt  cdc_report_dt current_status sex       n .event_num
#>    <date>       <date>       <date>        <chr>          <chr> <int>      <dbl>
#>    [event_date] [report_dat… [...]         [...]          [str… [cas…      [...]
#>  1 2020-01-01   2020-01-01   2020-01-01    Probable Case  Fema…     1          0
#>  2 2020-01-01   2020-03-25   2020-09-05    Laboratory-co… Fema…     1          0
#>  3 2020-01-01   2020-03-27   2020-05-13    Laboratory-co… Fema…     1          0
#>  4 2020-01-01   2020-04-16   2020-04-25    Laboratory-co… Male      1          0
#>  5 2020-01-01   2020-04-16   2020-07-28    Laboratory-co… Fema…     1          0
#>  6 2020-01-01   2020-04-24   2020-09-05    Laboratory-co… Fema…     1          0
#>  7 2020-01-01   2020-06-02   2020-06-04    Probable Case  Fema…     1          0
#>  8 2020-01-01   2020-07-08   2020-08-17    Laboratory-co… Male      1          0
#>  9 2020-01-01   2020-07-15   2020-07-18    Probable Case  Male      1          0
#> 10 2020-01-01   2020-07-21   2020-07-25    Laboratory-co… Fema…     1          0
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2020-12-31 | Event date: "onset_dt" | Report date: "pos_spec_dt"
#> # Strata: "sex"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 192,943 more rows
#> # ℹ 2 more variables: .report_num <dbl>, .delay <dbl>

# The third date, with CDC's labels translated to this package's vocabulary.
tn3 <- tbl_now(
  covid_us,
  event_date       = onset_dt,
  report_date      = pos_spec_dt,
  validation_date  = cdc_report_dt,
  validation_type  = current_status,
  validation_levels = c(
    "Laboratory-confirmed case" = "confirmed",
    "Probable Case"             = "pending"
  ),
  case_count = n,
  strata     = sex,
  data_type  = "count-incidence",
  verbose    = FALSE
)
has_validation(tn3)
#> [1] TRUE
get_validation_levels(tn3)
#> Laboratory-confirmed case             Probable Case 
#>               "confirmed"                 "pending" 

# "How many cases were there" now has more than one answer.
head(get_latest_reported_cases(tn3))
#> # A tibble:  6 × 7
#> # Data type: "count-cumulative"
#> # Frequency: Event: `days` | Report: `days`
#>   onset_dt     pos_spec_dt   .event_num .report_num sex            n .delay
#>   <date>       <date>             <dbl>       <dbl> <chr>      <dbl>  <dbl>
#>   [event_date] [report_date]      [...]       [...] [strata] [cases]  [...]
#> 1 2020-01-01   2020-09-04             0         247 Female         8    247
#> 2 2020-01-01   2020-08-05             0         217 Male           4    217
#> 3 2020-01-03   2020-04-24             2         114 Female         1    112
#> 4 2020-01-03   2020-03-31             2          90 Male           1     88
#> 5 2020-01-04   2020-08-07             3         219 Female         2    216
#> 6 2020-01-04   2020-07-06             3         187 Male           2    184
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2020-12-31 | Event date: "onset_dt" | Report date: "pos_spec_dt"
#> # Strata: "sex"
#> # ────────────────────────────────────────────────────────────────────────────────
head(get_latest_validated_cases(tn3, type = "confirmed"))
#> # A tibble:  6 × 11
#> # Data type: "count-cumulative"
#> # Frequency: Event: `days` | Report: `days`
#>   onset_dt     pos_spec_dt   .event_num .report_num cdc_report_dt     sex     
#>   <date>       <date>             <dbl>       <dbl> <date>            <chr>   
#>   [event_date] [report_date]      [...]       [...] [validation_date] [strata]
#> 1 2020-01-01   2020-09-04             0         247 2020-09-07        Female  
#> 2 2020-01-01   2020-07-08             0         189 2020-08-17        Male    
#> 3 2020-01-03   2020-04-24             2         114 2020-05-03        Female  
#> 4 2020-01-03   2020-03-31             2          90 2020-04-05        Male    
#> 5 2020-01-04   2020-07-06             3         187 2020-09-12        Male    
#> 6 2020-01-04   2020-09-14             3         257 2020-09-24        Unknown 
#> # ────────────────────────────────────────────────────────────────────────────────
#> # Now: 2020-12-31 | Event date: "onset_dt" | Report date: "pos_spec_dt"
#> # Validation date: "cdc_report_dt" ("days") | resolved: 6/6
#> # Strata: "sex"
#> # ────────────────────────────────────────────────────────────────────────────────
#> # ℹ 5 more variables: current_status <chr>, n <dbl>, .delay <dbl>,
#> #   .validation_num <dbl>, .validation_delay <dbl>
```
