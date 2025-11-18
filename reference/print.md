# Print temporal effects

Print function for printing the a \[temporal_effects()\].

## Arguments

- x:

  A temporal_effects object created with \[temporal_effects()\]

- ...:

  Additional arguments to pass to print.

## Examples

``` r
print(temporal_effects(day_of_week = TRUE, week_of_year = TRUE))
#> <tbl.now::temporal_effects>
#>  @ day_of_week  : logi TRUE
#>  @ weekend      : logi FALSE
#>  @ day_of_month : logi FALSE
#>  @ month_of_year: logi FALSE
#>  @ week_of_year : logi TRUE
#>  @ seasons      : int(0) 
#>  @ holidays     : NULL
print(temporal_effects(day_of_week = FALSE, week_of_year = FALSE))
#> <tbl.now::temporal_effects>
#>  @ day_of_week  : logi FALSE
#>  @ weekend      : logi FALSE
#>  @ day_of_month : logi FALSE
#>  @ month_of_year: logi FALSE
#>  @ week_of_year : logi FALSE
#>  @ seasons      : int(0) 
#>  @ holidays     : NULL
print(temporal_effects(day_of_week = FALSE, week_of_year = FALSE, seasons = 52))
#> <tbl.now::temporal_effects>
#>  @ day_of_week  : logi FALSE
#>  @ weekend      : logi FALSE
#>  @ day_of_month : logi FALSE
#>  @ month_of_year: logi FALSE
#>  @ week_of_year : logi FALSE
#>  @ seasons      : num 52
#>  @ holidays     : NULL
```
