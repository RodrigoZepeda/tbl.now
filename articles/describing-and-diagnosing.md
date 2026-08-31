# Describing and diagnosing a tbl_now

``` r

library(dplyr, quietly = TRUE)
library(tbl.now)
```

Two questions come up with every new dataset, and they are different
questions:

- **What is in it?** — how many cases, over what period, arriving how
  late, how sparse, how concentrated in one stratum.
  [`summary()`](https://rdrr.io/r/base/summary.html) answers this.
- **What is wrong with it?** — missing dates, impossible orderings,
  repeated rows, units that do not line up, data that stops before its
  own `now`.
  [`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
  answers this.

Both return a **tibble**, not printed text. That is the design decision
everything else follows from: an answer you can
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html),
`join()`, plot, or assert on in a test is worth far more than one you
can only read.

``` r

data(denguedat)

dengue_now <- tbl_now(denguedat,
  event_date  = "onset_week",
  report_date = "report_week",
  strata      = "gender",
  verbose     = FALSE
)
```

## Part 1 — `summary()`

### One table, several blocks

[`summary()`](https://rdrr.io/r/base/summary.html) stacks blocks of
related statistics into a single table. The `component` column says
which block a row belongs to:

``` r

summary(dengue_now) |>
  count(component)
#> # A tibble: 7 × 2
#>   component           n
#>   <chr>           <int>
#> 1 autocorrelation     6
#> 2 cases               6
#> 3 completeness       24
#> 4 composition         2
#> 5 coverage           29
#> 6 delay               3
#> 7 zero_run            6
```

Every row is one quantity, described by up to eighteen columns. Not
every column applies to every row — a delay distribution has a `mean`
but no `prop`, a proportion has a `prop` but no `q90` — so the table is
deliberately sparse:

``` r

summary(dengue_now) |>
  filter(component == "cases") |>
  select(component, quantity, stratum, n, total, mean, sd, min, q50, q90, max, prop_zero)
#> # A tibble: 6 × 12
#>   component quantity        stratum     n total  mean    sd   min   q50   q90   max prop_zero
#>   <chr>     <chr>           <chr>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>     <dbl>
#> 1 cases     per_event_date  all      1095 52987  48.4  53.3     0    30   104   358   0.00365
#> 2 cases     per_event_date  Female   1095 26592  24.3  26.7     0    15    52   189   0.0119 
#> 3 cases     per_event_date  Male     1095 26395  24.1  27.0     0    15    53   176   0.0119 
#> 4 cases     per_report_date all      1095 52987  48.4  54.3     0    29   111   420   0.00274
#> 5 cases     per_report_date Female   1095 26592  24.3  27.3     0    15    57   217   0.0155 
#> 6 cases     per_report_date Male     1095 26395  24.1  27.5     0    15    54   203   0.0201
```

`stratum` is always “which subset of the data does this row describe”:
`"all"` for the pooled rows, or a stratum label. The *category* of a
compositional row goes in `quantity` instead, which keeps the two ideas
from colliding:

``` r

summary(dengue_now) |>
  filter(component == "composition") |>
  select(quantity, stratum, n, total, prop)
#> # A tibble: 2 × 5
#>   quantity        stratum     n total  prop
#>   <chr>           <chr>   <int> <dbl> <dbl>
#> 1 strata = Female all      4133 26592 0.502
#> 2 strata = Male   all      4132 26395 0.498
```

### The blocks worth knowing

**`delay`** — the case-weighted reporting delay. Weighted means “equal
to what you would get by expanding the counts to one row per case”, so
it needs no separate explanation:

``` r

delay_summary(dengue_now) |>
  select(quantity, stratum, n, total, mean, sd, q50, q90, max)
#> # A tibble: 3 × 9
#>   quantity        stratum     n total  mean    sd   q50   q90   max
#>   <chr>           <chr>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 event_to_report all      8265 52987  1.74  1.21     1     3    26
#> 2 event_to_report Female   4133 26592  1.74  1.20     1     3    15
#> 3 event_to_report Male     4132 26395  1.74  1.22     1     3    26
```

**`completeness`** — the share of each event date’s eventual total that
had arrived by delay `d`. This is usually the most decision-relevant
block in the table, because it says how far back a nowcast has anything
left to estimate:

``` r

reporting_completeness(dengue_now, delays = 0:4) |>
  filter(stratum == "all") |>
  select(quantity, n, mean, q50, prop)
#> # A tibble: 5 × 5
#>   quantity       n   mean    q50   prop
#>   <chr>      <int>  <dbl>  <dbl>  <dbl>
#> 1 delay <= 0  1090 0.0381 0.0220 0.0396
#> 2 delay <= 1  1090 0.510  0.510  0.502 
#> 3 delay <= 2  1090 0.844  0.867  0.850 
#> 4 delay <= 3  1090 0.931  0.953  0.941 
#> 5 delay <= 4  1090 0.963  0.984  0.972
```

Four percent of a week’s cases are in by the end of that week, half by
the end of the next, and 94% by three weeks. Beyond about three weeks
there is very little missing to reconstruct.

**`zero_run`** — how sparse the series is, measured as runs of
consecutive zero dates rather than as a simple proportion. A series that
is 30% zeros scattered at random is a very different modelling problem
from one that is 30% zeros in three long gaps:

``` r

zero_run_summary(dengue_now, axis = "event") |>
  select(quantity, stratum, n, total, mean, q50, max)
#> # A tibble: 3 × 7
#>   quantity   stratum     n total  mean   q50   max
#>   <chr>      <chr>   <int> <dbl> <dbl> <dbl> <dbl>
#> 1 event_date all         2     4  2        1     3
#> 2 event_date Female     10    13  1.3      1     3
#> 3 event_date Male        8    13  1.62     1     3
```

**`coverage`** — the reach of the object, including how stale it is:

``` r

triangle_occupancy(dengue_now) |>
  filter(stratum == "all") |>
  select(quantity, n, value)
#> # A tibble: 6 × 3
#>   quantity                    n  value
#>   <chr>                   <int>  <dbl>
#> 1 max_delay                  NA 26    
#> 2 triangle_cells_observed  5154 NA    
#> 3 triangle_cells_possible 29214 NA    
#> 4 triangle_occupancy         NA  0.176
#> 5 now_gap_event              NA  3    
#> 6 now_gap_report             NA  0
```

### Every block is also a function

[`summary()`](https://rdrr.io/r/base/summary.html) is exactly the
[`bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html) of
its components, and each one is exported, so you can ask a single
question without computing the rest:

``` r

prop_strata(dengue_now) |>
  select(quantity, total, prop)
#> # A tibble: 2 × 3
#>   quantity        total  prop
#>   <chr>           <dbl> <dbl>
#> 1 strata = Female 26592 0.502
#> 2 strata = Male   26395 0.498
```

The full set is
[`cases_per_date()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md),
[`delay_summary()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md),
[`zero_run_summary()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md),
[`prop_censored()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md),
[`prop_confirmation_type()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md),
[`prop_strata()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md),
[`prop_covariate_levels()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md),
[`case_autocorrelation()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md),
[`date_ranges()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md),
[`triangle_occupancy()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md),
[`reporting_completeness()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md)
and
[`cumulative_growth()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md).

**Quantiles are inverse-ECDF (type 1).** `q50` is the smallest value
whose cumulative weight reaches 0.5 — for an even number of
observations, the upper of the two middle values rather than their
average. This is the same estimator
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
and
[`plot_delay_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/plot_delay_drift.md)
use, so the table and the figures always agree, and for count data it
always returns a value that was actually observed. A half-case delay is
not.

## Part 2 — `diagnose()`

### Findings, worst first

``` r

diagnose(dengue_now) |>
  count(status)
#> # A tibble: 4 × 2
#>   status      n
#>   <ord>   <int>
#> 1 note        9
#> 2 ok         14
#> 3 not_run     3
#> 4 skipped     6
```

`status` is an **ordered factor** — `error` \> `warning` \> `note` \>
`ok` \> `not_run` \> `skipped` — so the table sorts itself and filtering
to what needs acting on is a comparison rather than a set membership
test:

``` r

diagnose(dengue_now) |>
  filter(status <= "note") |>
  select(check, scope, stratum, status, n_affected, n_total, message)
#> # A tibble: 9 × 7
#>   check      scope          stratum status n_affected n_total message                                                                                           
#>   <chr>      <chr>          <chr>   <ord>       <dbl>   <dbl> <chr>                                                                                             
#> 1 now        now_gap_event  Female  note            3      NA "The last event date is 3 weeks before now (\"2010-12-20\")."                                     
#> 2 now        now_gap_event  Male    note            3      NA "The last event date is 3 weeks before now (\"2010-12-20\")."                                     
#> 3 now        now_gap_event  all     note            3      NA "The last event date is 3 weeks before now (\"2010-12-20\")."                                     
#> 4 now        now_gap_report Male    note            1      NA "The last report date is 1 week before now (\"2010-12-20\")."                                     
#> 5 strata     size           Male    note        26395   52987 "The smallest stratum is \"Male\" with 26395 cases, 49.8% of the total."                          
#> 6 strata     sparsity       Female  note           13    1095 "The sparsest stratum is \"Female\": 1.2% of the event dates on the grid carry no cases at all."  
#> 7 truncation event_date     Female  note            1    1082 "1 event date is younger than the 95th percentile of the delay, so its counts are still filling i…
#> 8 truncation event_date     Male    note            1    1082 "1 event date is younger than the 95th percentile of the delay, so its counts are still filling i…
#> 9 truncation event_date     all     note            1    1091 "1 event date is younger than the 95th percentile of the delay, so its counts are still filling i…
```

Each finding also carries a `hint` saying what to do about it, and a
`rows` list-column of the offending row indices, so you can go straight
to them:

``` r

finding <- diagnose(dengue_now) |> filter(check == "declarations", status <= "note")

finding$hint
#> character(0)
```

### The six statuses, and why `skipped` is not `ok`

The distinction that matters most is between a check that **ran and
found nothing** and one that **could not run**:

``` r

diagnose(dengue_now) |>
  filter(status == "skipped") |>
  select(check, scope, message)
#> # A tibble: 6 × 3
#>   check      scope                  message                                                                               
#>   <chr>      <chr>                  <chr>                                                                                 
#> 1 duplicates key                    A line list is one row per case, so identical rows are two cases rather than a repeat.
#> 2 negatives  count                  A line list has no count column to go negative.                                       
#> 3 ordering   event_to_confirmation  The object carries no confirmation process.                                           
#> 4 ordering   report_to_confirmation The object carries no confirmation process.                                           
#> 5 signposts  confirmation           The object carries no confirmation process.                                           
#> 6 strata     pending                The object carries no confirmation process.
```

`dengue_now` is a line list with no confirmation process, so four checks
have nothing to work on. None of them is a pass. Reporting them as `ok`
would be a quiet lie, and this is the single most common way a health
check misleads: silence that reads as approval.

Note the first row in particular.
**[`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
will not look for duplicate records in a line list**, because a line
list is one row per case — two identical rows are two infections, not a
repeat. Deduplicating a line list needs a key the object does not have
(a record id, say), so that check stays yours.

### Structural, never statistical

[`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
runs no statistical tests at all. It is fast, and its answer never
depends on a random seed or an optional package. The questions that *do*
need a test come back as `not_run` signposts naming the call that
answers each one:

``` r

diagnose_signposts(dengue_now) |>
  select(scope, status, message)
#> # A tibble: 4 × 3
#>   scope                status  message                                            
#>   <chr>                <ord>   <chr>                                              
#> 1 confirmation_batches not_run "Run: diagnose_batches(x, axis = \"confirmation\")"
#> 2 report               not_run "Run: diagnose_drift(x, axis = \"report\")"        
#> 3 report_batches       not_run "Run: diagnose_batches(x, axis = \"report\")"      
#> 4 confirmation         skipped "The object carries no confirmation process."
```

Those calls —
[`diagnose_drift()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_drift.md),
[`diagnose_changepoint()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_changepoint.md),
[`diagnose_batches()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batches.md),
[`diagnose_batch_shape()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose_batch_shape.md)
— are the statistical diagnostics, documented in the [batch-reporting
article](https://rodrigozepeda.github.io/tbl.now/articles/batch-reporting.html).
They return their own shapes rather than the findings schema, because a
hypothesis test has a p-value and an effect size that a findings row has
nowhere to put.

### Cumulative data: the revisions

Cumulative counts get revised downwards, and a downward revision becomes
a **negative increment** the moment the series is de-accumulated.
Anything consuming incidence has to cope with that, so
[`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
counts them. FluSight is the standard example:

``` r

data(flusight)

flu_now <- flusight |>
  filter(location_name %in% c("Alabama", "Alaska", "Arizona"),
         target_end_date >= as.Date("2023-01-01")) |>
  tbl_now(
    event_date  = target_end_date,
    report_date = as_of,
    case_count  = observation,
    strata      = location_name,
    data_type   = "count-cumulative",
    verbose     = FALSE
  )

diagnose_negatives(flu_now) |>
  select(scope, stratum, status, n_affected, n_total, message)
#> # A tibble: 4 × 6
#>   scope     stratum status n_affected n_total message                                                  
#>   <chr>     <chr>   <ord>       <dbl>   <dbl> <chr>                                                    
#> 1 increment Alabama note           73    5499 73 de-accumulated increments are negative (total -175).  
#> 2 increment Alaska  note           12    5499 12 de-accumulated increments are negative (total -26).   
#> 3 increment Arizona note           71    5499 71 de-accumulated increments are negative (total -7522). 
#> 4 increment all     note          156   16497 156 de-accumulated increments are negative (total -7723).
```

For cumulative data [`summary()`](https://rdrr.io/r/base/summary.html)
also swaps its `delay` block for a `growth` one — the ratio of one
delay’s running total to the previous one’s — because a cumulative total
is not additive across delays and a case-weighted delay distribution
would be meaningless:

``` r

cumulative_growth(flu_now, k = 3) |>
  filter(stratum == "all") |>
  select(quantity, n, total, mean, q50, max)
#> # A tibble: 3 × 6
#>   quantity     n total  mean   q50   max
#>   <chr>    <int> <dbl> <dbl> <dbl> <dbl>
#> 1 delay 1     64  2024 1.08      1  3.44
#> 2 delay 2     75  -103 1.01      1  1.78
#> 3 delay 3     81 -1042 0.993     1  1.72
```

The mean ratio falls below 1 by the third delay, and the totals go
negative: these series shrink on revision more often than they grow.
[`delay_summary()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md)
refuses to run on this object at all, and says why.

## The same findings, two presentations

[`validate_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md)
runs the **same engine** as
[`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md).
It does not return the findings; it re-emits the `error` rows as an
abort and the `warning` rows as warnings. That is why a malformed object
complains the moment you build it, and why the two can never drift
apart:

``` r

bad <- denguedat |>
  mutate(report_week = report_week - 400) |>
  tbl_now(event_date = "onset_week", report_date = "report_week", verbose = FALSE)
#> Warning: 52987 rows have a `report_date` before `event_date`
#> ℹ A negative reporting delay is not a delay; the two date columns may be swapped, or the rows may be data-entry errors.
```

The warning above and the `ordering` row of `diagnose(bad)` are the same
finding, formatted for two different audiences: one for a person who is
about to make a mistake, one for a program that needs to decide what to
do.

`note` rows are deliberately never promoted to warnings.
[`validate_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md)
runs on every `dplyr` verb, so a new warning there would turn a quiet
construction into a noisy one for data that has always been accepted.

## Where to go next

- [`?tbl_now_summary`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_summary.md)
  and
  [`?nowcast_summary_components`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_summary_components.md)
  — every column, every block.
- [`?diagnose`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
  and
  [`?nowcast_diagnose_components`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_diagnose_components.md)
  — every check, every status.
- The [worked
  example](https://rodrigozepeda.github.io/tbl.now/articles/example.html)
  applies both to a real, deliberately unpolished dataset, and cleans it
  on the strength of what
  [`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
  reports.
