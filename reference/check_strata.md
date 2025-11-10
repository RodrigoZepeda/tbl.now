# Check the strata columns

The \`strata\` columns should be present in the data and should be
character, integers, or factors.

## Usage

``` r
check_strata(data, strata)
```

## Arguments

- data:

  A \`data.frame\` to be converted.

- strata:

  (optional) Character vector or \`NULL\` (default). Name of different
  variables (column names) in strata. Strata correspond to variables
  that are of interest by themselves. For example if it is of interest
  to generate nowcasts by gender then \`gender\` is a \`strata\`.

## Value

(invisible) \`TRUE\` if the columns in \`strata\` are NULL or are
character, integers, or factors in the tibble \`data\`.
