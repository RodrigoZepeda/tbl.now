# Check update when new_data is a tbl_now

Checks that the attributes match between object and new_data

## Usage

``` r
update_check_tbl_now_internal(object, new_data)
```

## Arguments

- object:

  A `tbl_now` object

- new_data:

  Another `tbl_now` with the same `strata`, `covariates`, `is_censored`,
  and `temporal_effects` or a `data.frame` with additional (newer) data
  not present in `x`

## Value

TRUE (invisibly). Called for its side effects
