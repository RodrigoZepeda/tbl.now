# Check the strata columns

The \`strata\` columns should be present in the data and should be
character, integers, or factors.

## Usage

``` r
check_strata(data, strata)
```

## Arguments

- data:

  A `data.frame` or `tibble` to be converted.

- strata:

  (optional)
  [`` <`tidy-select`> ``](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  or `NULL` (default). Name of different variables (column names) in
  strata. Strata correspond to variables that are of interest by
  themselves. For example if it is of interest to generate nowcasts by
  gender then `gender` is a `strata`.

## Value

(invisible) \`TRUE\` if the columns in \`strata\` are NULL or are
character, integers, or factors in the tibble \`data\`.
