# Check update when new_data is a data.frame

Checks that the attributes of object are columns in new_data

## Usage

``` r
update_check_data_frame_internal(object, new_data)
```

## Arguments

- object:

  A \`tbl_now\` object

- new_data:

  Another \`tbl_now\` with the same \`strata\`, \`covariates\`,
  \`is_batched\`, and \`temporal_effects\` or a \`data.frame\` with
  additional (newer) data not present in \`x\`

## Value

TRUE (invisibly). Called for its side effects
