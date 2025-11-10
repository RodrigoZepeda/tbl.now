# Automatically infer the \`data_type\`

Infers whether the data is line-data or count-data whether it has (or
has not) a column named \`n\`

## Usage

``` r
infer_data_type(data, data_type, verbose = FALSE)
```

## Arguments

- data:

  A \`data.frame\` to be converted.

- data_type:

  (optional) Character. Either "auto", "linelist" or "count".

- verbose:

  (optional) Logical. Whether to throw a message. Default = \`TRUE\`.

## Value

Whether the data is \`count\` or \`linelist\`
