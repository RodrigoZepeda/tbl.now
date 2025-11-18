# Check whether a date is a weekday vs weekend

Function that checks whether a date object is a weekday or weekend.

## Usage

``` r
is_weekday(date)
```

## Arguments

- date:

  A date object

## References

From https://stackoverflow.com/a/60346779/5067372

## Examples

``` r
is_weekday(as.Date("2020-04-22"))
#> [1] TRUE
is_weekday(as.Date("2020-04-19"))
#> [1] FALSE
```
