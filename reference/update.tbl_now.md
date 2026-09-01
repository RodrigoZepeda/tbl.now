# Append newly arrived data to a `tbl_now`

**\[experimental\]**

Surveillance data does not arrive once; it arrives every week.
[`update()`](https://rdrr.io/r/stats/update.html) takes a `tbl_now` and
a batch of newer rows – as another `tbl_now` or as a plain `data.frame`
– and returns a single object containing both, still knowing everything
the original knew about itself.

It also moves `now` forward, because the new rows may carry a later
report than the object had seen. That is the difference between this and
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html),
which would give you back a plain data frame with no idea what a nowcast
is.

## Usage

``` r
# S3 method for class 'tbl_now'
update(
  object,
  ...,
  new_data,
  strata = "left",
  covariates = strata,
  t_effects = strata,
  now = NULL,
  remove_duplicates = NULL
)
```

## Arguments

- object:

  A `tbl_now` object

- ...:

  Additional arguments to pass to `tbl_now`

- new_data:

  Another `tbl_now` with the same `strata`, `covariates`,
  `is_censored_report`, and `temporal_effects` or a `data.frame` with
  additional (newer) data not present in `x`

- strata:

  (optional) Whether to keep the strata from `object` ("left"), from
  `new_data` ("right") or from `both` ("both")

- covariates:

  (optional) Whether to keep the covariates from `object` ("left"), from
  `new_data` ("right") or from `both` ("both")

- t_effects:

  (optional) Which temporal-effects spec to keep: from `object`
  (`"left"`, the default), from `new_data` (`"right"`) or the union of
  both (`"both"`).

- now:

  (optional) Date or `NULL` (default). The date that is considered the
  `now` of the nowcast. If no `now` is given then the function
  automatically uses the last `event_date`.

- remove_duplicates:

  Whether to remove duplicated rows from data (only applies for `count`
  data)

## Value

A `tbl_now` object with all the properties of `object`

## Note

By default it keeps the strata, covariates and temporal effects of
`object`. Use the `strata`, `covariates` and `t_effects` arguments to
change it.

## See also

[update_now()](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
to move `now` without adding rows;
[`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
for the attributes that are carried over;
[`add()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md) and
[change()](https://rodrigozepeda.github.io/tbl.now/reference/add.md) to
edit those attributes instead of the data.

## Examples

``` r
data(denguedat)

# Pretend the first 500 rows are what you had last week ...
initial_tbl <- tbl_now(denguedat[1:500, ],
  event_date = "onset_week",
  report_date = "report_week", strata = "gender",
  verbose = FALSE
)
nrow(initial_tbl)
#> [1] 500
get_now(initial_tbl)
#> [1] "1990-09-03"

# ... and these arrived since.
new_rows <- denguedat[501:1000, ]

# The result has both, keeps `gender` as a stratum, and has moved `now`
# forward to the latest report it has now seen.
updated <- update(initial_tbl, new_data = new_rows)
nrow(updated)
#> [1] 1000
get_strata(updated)
#> [1] "gender"
get_now(updated)
#> [1] "1990-11-12"
```
