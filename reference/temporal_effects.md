# Temporal Effects Class

The \`temporal_effects\` class specifies which temporal covariates or
effects should be included in a nowcasting model (e.g., day of week,
month, holidays, etc.).

## Usage

``` r
temporal_effects(
  day_of_week = FALSE,
  weekend = FALSE,
  day_of_month = FALSE,
  month_of_year = FALSE,
  week_of_year = FALSE,
  seasons = integer(0),
  holidays = NULL
)
```

## Arguments

- day_of_week:

  Logical. Whether to include an effect for each of the seven days of
  the week.

- weekend:

  Logical. Whether to include an effect for the weekend vs the weekday.

- day_of_month:

  Logical. Whether to include an effect for the day of the month (1 to
  31).

- month_of_year:

  Logical. Whether to include an effect for the month of the year.

- week_of_year:

  Logical. Whether to include an effect for the epidemiological week.

- seasons:

  Vector. Either \`integer(0)\` or a vector where each entry is the
  length of the seasons included in the model.

- holidays:

  Either \`NULL\` or an \[\`almanac::rcalendar()\`\] specifying how to
  calculate holidays.

## Value

An object of class \`temporal_effects\`.

## Details

US Federal holidays can be passed by providing the
\[\`almanac::cal_us_federal()\`\] calendar.

Example: “\`r library(almanac) temporal_effects(holidays =
cal_us_federal()) “\`

## Examples

``` r
temporal_effects(day_of_week = TRUE, week_of_year = TRUE)
#> <tbl.now::temporal_effects>
#>  @ day_of_week  : logi TRUE
#>  @ weekend      : logi FALSE
#>  @ day_of_month : logi FALSE
#>  @ month_of_year: logi FALSE
#>  @ week_of_year : logi TRUE
#>  @ seasons      : int(0) 
#>  @ holidays     : NULL

if (rlang::is_installed("almanac")) {
  cal <- almanac::rcalendar(almanac::hol_christmas())
  temporal_effects(holidays = cal, day_of_month = TRUE, seasons = c(7, 365))
}
#> <tbl.now::temporal_effects>
#>  @ day_of_week  : logi FALSE
#>  @ weekend      : logi FALSE
#>  @ day_of_month : logi TRUE
#>  @ month_of_year: logi FALSE
#>  @ week_of_year : logi FALSE
#>  @ seasons      : num [1:2] 7 365
#>  @ holidays     :List of 3
#>  .. $ names    : chr "Christmas"
#>  .. $ rholidays:List of 1
#>  ..  ..$ :List of 3
#>  ..  .. ..$ name       : chr "Christmas"
#>  ..  .. ..$ robserved  :List of 2
#>  ..  .. .. ..$ rules:List of 13
#>  ..  .. .. .. ..$ since        : Date[1:1], format: "1900-01-01"
#>  ..  .. .. .. ..$ until        : Date[1:1], format: "2100-01-01"
#>  ..  .. .. .. ..$ frequency    : chr "yearly"
#>  ..  .. .. .. ..$ count        : NULL
#>  ..  .. .. .. ..$ interval     : NULL
#>  ..  .. .. .. ..$ week_start   : NULL
#>  ..  .. .. .. ..$ month_of_year: int 12
#>  ..  .. .. .. ..$ week_of_year : NULL
#>  ..  .. .. .. ..$ day_of_year  : NULL
#>  ..  .. .. .. ..$ day_of_month : int 25
#>  ..  .. .. .. ..$ day_of_week  : NULL
#>  ..  .. .. .. ..$ position     : NULL
#>  ..  .. .. .. ..$ easter       : NULL
#>  ..  .. .. ..$ cache:Classes 'cache_rrule', 'R6' <cache_rrule>
#>   Public:
#>     get_events: function () 
#>     initialize: function (rules) 
#>   Private:
#>     built: FALSE
#>     cache_build: function () 
#>     events: NULL
#>     rules: list 
#>  ..  .. .. ..- attr(*, "class")= chr [1:2] "almanac_rrule" "almanac_rschedule"
#>  ..  .. ..$ runobserved:List of 2
#>  ..  .. .. ..$ rules:List of 13
#>  ..  .. .. .. ..$ since        : Date[1:1], format: "1900-01-01"
#>  ..  .. .. .. ..$ until        : Date[1:1], format: "2100-01-01"
#>  ..  .. .. .. ..$ frequency    : chr "yearly"
#>  ..  .. .. .. ..$ count        : NULL
#>  ..  .. .. .. ..$ interval     : NULL
#>  ..  .. .. .. ..$ week_start   : NULL
#>  ..  .. .. .. ..$ month_of_year: int 12
#>  ..  .. .. .. ..$ week_of_year : NULL
#>  ..  .. .. .. ..$ day_of_year  : NULL
#>  ..  .. .. .. ..$ day_of_month : int 25
#>  ..  .. .. .. ..$ day_of_week  : NULL
#>  ..  .. .. .. ..$ position     : NULL
#>  ..  .. .. .. ..$ easter       : NULL
#>  ..  .. .. ..$ cache:Classes 'cache_rrule', 'R6' <cache_rrule>
#>   Public:
#>     get_events: function () 
#>     initialize: function (rules) 
#>   Private:
#>     built: FALSE
#>     cache_build: function () 
#>     events: NULL
#>     rules: list 
#>  ..  .. .. ..- attr(*, "class")= chr [1:2] "almanac_rrule" "almanac_rschedule"
#>  ..  .. ..- attr(*, "class")= chr [1:2] "almanac_rholiday" "almanac_rschedule"
#>  .. $ cache    :Classes 'cache_rcalendar', 'R6' <cache_rcalendar>
#>   Public:
#>     get_events: function (observed) 
#>     get_events_frame: function (observed) 
#>     initialize: function (names, rholidays) 
#>   Private:
#>     cache_build: function (observed) 
#>     names: Christmas
#>     observed: list
#>     rholidays: list
#>     unobserved: list 
#>  .. - attr(*, "class")= chr [1:2] "almanac_rcalendar" "almanac_rschedule"
```
