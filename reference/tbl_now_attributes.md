# Show the names of the attributes of a `tbl_now`

**\[stable\]**

Shows exclusively the attributes of a `tbl_now` that are not part of the
attributes of a `tibble` (or a grouped tibble).

## Usage

``` r
tbl_now_attributes(x)
```

## Arguments

- x:

  A `tbl_now` object

## Value

A named list with the attributes that are specific to the `tbl_now`
class (i.e. those not shared with a plain `tibble`).

## Examples

``` r
data(denguedat)
df_now <- tbl_now(denguedat, event_date = onset_week,
  report_date = report_week, strata = gender, verbose = FALSE)

#Attributes gets all attributes
attributes(df_now) %>% names()
#>  [1] "names"                         "row.names"                    
#>  [3] "class"                         "event_date"                   
#>  [5] "report_date"                   "strata"                       
#>  [7] "now"                           "event_units"                  
#>  [9] "report_units"                  "data_type"                    
#> [11] "temporal_effects"              "computed_temporal_effect_cols"

#tbl_now_attributes gets only those associated to the `tbl_now` class
tbl_now_attributes(df_now) %>% names()
#> [1] "event_date"                    "report_date"                  
#> [3] "now"                           "event_units"                  
#> [5] "report_units"                  "data_type"                    
#> [7] "temporal_effects"              "computed_temporal_effect_cols"
```
