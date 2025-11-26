# Automatically infer the \`data_type\`

Infers whether the data is line-data or count-data whether it has (or
has not) a column named \`n\`

## Usage

``` r
infer_data_type(
  data,
  data_type,
  event_date,
  report_date,
  strata = NULL,
  is_batched = NULL,
  case_col = NULL,
  verbose = FALSE
)
```

## Arguments

- data:

  A `data.frame` to be converted.

- data_type:

  (optional) Character. Either "auto", "linelist" or "count-incidence"
  or "count-cumulative". See section below for an explanation on data
  types.

- event_date:

  Character. The name of the column containing the event date.

- report_date:

  Character. The name of the column containing the report date.

- strata:

  (optional) Character vector or `NULL` (default). Name of different
  variables (column names) in strata. Strata correspond to variables
  that are of interest by themselves. For example if it is of interest
  to generate nowcasts by gender then `gender` is a `strata`.

- is_batched:

  (optional) Character or `NULL` (default). The name of a column
  containing either `TRUE` or `FALSE` indicating whether the
  `report_date` is correctly specified or corresponds to a `batch` and
  thus is censored. In other words, if the `report_date` is accurately
  measured set `report_date = TRUE` but if the `report_date` corresponds
  to an error and is only an upper bound of the real, idealized, report
  date set `is_batched = TRUE`.

- case_col:

  (optional) Name of the column with the case counts if `data_type` is
  "count". If `case_col` is specified even if `data_type` is "linelist"
  that name will be used if the `to_count` function is applied.

- verbose:

  (optional) Logical. Whether to throw a message. Default = `TRUE`.

## Value

Whether the data is \`count\` or \`linelist\`
