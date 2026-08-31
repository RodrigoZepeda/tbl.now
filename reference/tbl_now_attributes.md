# List what a `tbl_now` was told about itself

**\[stable\]**

A
[`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
remembers which of its columns is the event date, which is the report
date, which are strata, and so on. That information is stored as
attributes on the object. `tbl_now_attributes()` shows you those, and
only those – the bookkeeping attributes every `tibble` carries are left
out.

Use it when you want to check what an object thinks it is, especially
after a long `dplyr` pipeline.

## Usage

``` r
tbl_now_attributes(x)
```

## Arguments

- x:

  A
  [`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
  object.

## Value

A named list of the attributes specific to the `tbl_now` class (those
not shared with a plain `tibble`). Attributes are only present when they
were set, so an object with no strata has no `strata` element.

## See also

[`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
and its *Attributes* section for what each attribute means; the
[getters](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_data_getters.md)
for reading one attribute at a time;
[`change()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
and [`add()`](https://rodrigozepeda.github.io/tbl.now/reference/add.md)
for setting them;
[`validate_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md)
to check they are coherent.

## Examples

``` r
data(denguedat)
df_now <- tbl_now(denguedat,
  event_date = onset_week,
  report_date = report_week, strata = gender, verbose = FALSE
)

## `attributes()` returns everything, including tibble internals like `names`
# and `row.names`.
attributes(df_now) |> names()
#>  [1] "names"                         "row.names"                    
#>  [3] "class"                         "event_date"                   
#>  [5] "report_date"                   "strata"                       
#>  [7] "now"                           "event_units"                  
#>  [9] "report_units"                  "data_type"                    
#> [11] "temporal_effects"              "computed_temporal_effect_cols"

## `tbl_now_attributes()` returns only what makes it a tbl_now.
tbl_now_attributes(df_now) |> names()
#> [1] "event_date"                    "report_date"                  
#> [3] "strata"                        "now"                          
#> [5] "event_units"                   "report_units"                 
#> [7] "data_type"                     "temporal_effects"             
#> [9] "computed_temporal_effect_cols"

# And their values: the roles it recorded, plus the `now` of the nowcast.
tbl_now_attributes(df_now)[c("event_date", "report_date", "strata", "now")]
#> $event_date
#> [1] "onset_week"
#> 
#> $report_date
#> [1] "report_week"
#> 
#> $strata
#> [1] "gender"
#> 
#> $now
#> [1] "2010-12-20"
#> 
```
