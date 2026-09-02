# Check that an object is a valid `tbl_now`

**\[experimental\]**

Two different questions about an object, and one function for each.

- `is_tbl_now()` asks **"is this the class?"**. It answers quietly with
  `TRUE` or `FALSE`, and it is cheap: a class check, the attributes a
  `tbl_now` cannot do without, and the columns those attributes name.
  Use it in an `if`.

- `validate_tbl_now()` asks **"is the data in it sane?"**. It answers
  loudly: it stops with an error explaining what is wrong, and warns
  about the merely suspicious. Use it when you want the pipeline to halt
  rather than carry on with a broken object.

Neither checks whether the data are *good* – only whether the object is
put together correctly. For the quality of the data itself, use
[`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md).

## Usage

``` r
validate_tbl_now(x, warn_non_uniqueness = FALSE, warn_now = TRUE)

is_tbl_now(x)
```

## Arguments

- x:

  An object to check.

- warn_non_uniqueness:

  (optional) Logical. Whether to throw a warning if data has multiple
  observations for same event and report date (conditional on covariates
  and strata)

- warn_now:

  Boolean. Whether to warn if `now` falls before the last report date,
  or unreasonably far into the future.

## Value

`is_tbl_now()` returns a single `TRUE` or `FALSE`.

`validate_tbl_now()` returns `TRUE` invisibly; it is called for the
error or warning it raises when the object is malformed.

## Details

`validate_tbl_now()` and
[`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
share one implementation. This function is the *condition* presentation
of it: it aborts on the `error` findings and warns about the `warning`
ones.
[`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
is the *data* presentation, and additionally reports the `note`-level
observations that would make every `dplyr` verb noisy if they were
emitted here.

`is_tbl_now()` deliberately runs **none** of that. It used to, and the
cost was paid twice over: the findings engine ran on every
`.assert_tbl_now()`, and the warnings it raised escaped – so an object
the user had already chosen to keep re-reported its problems from
wherever the predicate happened to be called. An object can therefore be
a `tbl_now` (`is_tbl_now()` is `TRUE`) and still have data
`validate_tbl_now()` warns about. That is the point: the class is a
container, and a container is not a claim that what is in it is clean.

## See also

[`diagnose()`](https://rodrigozepeda.github.io/tbl.now/reference/diagnose.md)
for the same findings returned as a tibble, plus the softer notes;
[`tbl_now()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now.md)
to build a valid object;
[`tbl_now_attributes()`](https://rodrigozepeda.github.io/tbl.now/reference/tbl_now_attributes.md)
to see what it recorded. The [*Diagnosing a tbl_now*
article](https://rodrigozepeda.github.io/tbl.now/articles/diagnosing-a-tbl-now.html)
explains what each finding means.

## Examples

``` r
data(denguedat)
ndata <- tbl_now(denguedat,
  event_date = "onset_week",
  report_date = "report_week", verbose = FALSE
)

# A well-formed object passes both checks.
is_tbl_now(ndata)
#> [1] TRUE
validate_tbl_now(ndata)

# `is_tbl_now()` is a question about the CLASS, so it stays quiet about the
# data. This object's report dates include an `NA`, which validate_tbl_now()
# warns about -- and which does not stop it being a `tbl_now`.
messy <- ndata
messy$report_week[1] <- NA
is_tbl_now(messy)
#> [1] TRUE

# A plain data.frame is not a tbl_now ...
is_tbl_now(data.frame(x = 1:3))
#> [1] FALSE

## ... and asking for validation says so, with a reason. (Wrapped in try()
# because it is meant to fail here.)
try(validate_tbl_now(data.frame(x = 1:3)))
#> Error in .tbl_now_emit_findings(findings) : 
#>   Invalid `tbl_now` object:
#> Missing required attribute: "data_type"
#> Missing required attribute: "event_date"
#> Missing required attribute: "event_units"
#> Missing required attribute: "now"
#> Missing required attribute: "report_date"
#> Missing required attribute: "report_units"
```
