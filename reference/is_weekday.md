# Check whether a date is a weekday vs weekend (international definition)

Function that checks whether a date object is a weekday or weekend.

## Usage

``` r
is_weekday(date, weekend_days = c("Sat", "Sun"))
```

## Arguments

- date:

  A Date (or POSIXt) object.

- weekend_days:

  A character or numeric vector defining weekend days. -Numeric: must be
  integers in 1-7 corresponding to \[lubridate::wday()\] when
  \`week_start = 1\`. -Character: any of c("Mon","Tuesday","wed",...)
  case-insensitive. Defaults to Saturday and Sunday (weekend_days =
  c("Sat", "Sun")).

## Value

A logical vector: TRUE if weekday, FALSE if weekend.

## Examples

``` r
is_weekday(as.Date("2020-04-22"))                   # TRUE (Wed)
#> [1] TRUE
is_weekday(as.Date("2020-04-19"))                   # FALSE (Sun)
#> [1] FALSE

# Middle East weekend (Fri - Sat)
is_weekday(as.Date("2020-04-17"), weekend_days = c("Fri","Sat"))
#> [1] TRUE

# Weekend only on Friday
is_weekday(as.Date("2020-04-17"), weekend_days = "Friday")
#> [1] TRUE
is_weekday(as.Date("2020-04-18"), weekend_days = "Friday")
#> [1] FALSE

# Weekend on Sun - Mon (numeric: 7 = Sun, 1 = Mon)
is_weekday(as.Date("2020-04-20"), weekend_days = c(7, 1))
#> [1] FALSE
```
