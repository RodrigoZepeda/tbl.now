# Automatically infer the \`data_type\`

Infers whether the data is line-data or count-data whether it has (or
has not) a column named \`n\`

## Usage

``` r
infer_data_type(data, data_type, case_col, verbose = FALSE)
```

## Arguments

- data:

  A \`data.frame\` to be converted.

- data_type:

  (optional) Character. Either "auto", "linelist" or "count".

- case_col:

  (optional) Name of the column with the case counts if \`data_type\` is
  "count". If \`case_col\` is specified even if \`data_type\` is
  "linelist" that name will be used if the \`to_count\` function is
  applied.

- verbose:

  (optional) Logical. Whether to throw a message. Default = \`TRUE\`.

## Value

Whether the data is \`count\` or \`linelist\`
