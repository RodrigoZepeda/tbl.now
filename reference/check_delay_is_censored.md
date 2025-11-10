# Check the delay_is_censored column

The \`delay_is_censored\` column should only have values between 0
and 1. The idea is to indicate whether a different column in \`data\`
has censored values.

## Usage

``` r
check_delay_is_censored(data, delay_is_censored)
```

## Arguments

- data:

  A \`data.frame\` to be converted.

## Value

(invisible) \`TRUE\` if the column \`delay_is_censored\` is NULL or has
valid values and is in the tibble \`data\`.
