# Base R operations on a `tbl_now`

**\[stable\]**

A
[`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
is a `tibble` with extra attributes recording which column is the event
date, which is the report date, and so on. These methods make sure those
attributes survive ordinary base-R manipulation, so that `x[1:10, ]`,
`names(x) <- ...` and `x$new <- ...` give you back a `tbl_now` rather
than a bare data frame.

You never call them directly – they are what makes the operators work.

## Usage

``` r
# S3 method for class 'tbl_now'
x[...]

# S3 method for class 'grouped_tbl_now'
x[...]

# S3 method for class 'tbl_now'
names(x) <- value

# S3 method for class 'grouped_tbl_now'
names(x) <- value

# S3 method for class 'tbl_now'
x$name <- value

# S3 method for class 'grouped_tbl_now'
x$name <- value
```

## Arguments

- x:

  A `tbl_now` object.

- ...:

  Passed to the underlying `[` method: rows and columns to keep.

- value:

  For `names<-` and `$<-`, the replacement value.

- name:

  For `$<-`, the column being assigned to.

## Value

A `tbl_now` object, or a plain data frame when the operation invalidated
the class.

## Details

When an operation leaves the object unable to describe a nowcast –
because it dropped or renamed the event-date column, say – the class
cannot honestly be kept. In that case the result is **demoted** to a
plain data frame (with a warning), rather than pretending to still be a
`tbl_now`. Attributes that are still meaningful are preserved on the way
down.

The same applies to `dplyr` verbs, through
[dplyr_reconstruct()](https://dplyr.tidyverse.org/reference/dplyr_extending.html).

## See also

[`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
for the attributes being preserved;
[`tbl_now_attributes()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_attributes.md)
to check what survived;
[as_tibble()](https://rodrigozepeda.github.io/tbl.now/reference/as_tibble.tbl_now.md)
to drop the class on purpose;
[`validate_tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/validate_tbl_now.md)
to confirm the result is still well formed.

## Examples

``` r
data(denguedat)
dengue <- tbl_now(denguedat,
  event_date = onset_week, report_date = report_week,
  strata = gender, verbose = FALSE
)

# Subsetting rows keeps the class and everything it knows.
small <- dengue[1:10, ]
class(small)[1]
#> [1] "tbl_now"
get_event_date(small)
#> [1] "onset_week"

# So does adding a column with `$<-`.
dengue$season <- ifelse(
  lubridate::month(dengue$onset_week) %in% 6:11, "wet", "dry"
)
class(dengue)[1]
#> [1] "tbl_now"

# And renaming an unimportant column with `names<-`.
renamed <- dengue
names(renamed)[names(renamed) == "season"] <- "period"
get_event_date(renamed)
#> [1] "onset_week"

# But dropping the event date leaves nothing a nowcast could use, so the
# object is demoted to a plain tibble instead of lying about itself.
demoted <- suppressWarnings(dengue[, c("report_week", "gender")])
class(demoted)[1]
#> [1] "tbl_df"
```
