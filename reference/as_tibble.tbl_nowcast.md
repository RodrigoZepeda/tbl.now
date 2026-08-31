# Coerce a `tbl_nowcast` into a `tibble`

**\[experimental\]**

Turns a fitted nowcast into an ordinary `tibble` you can plot, join or
write out: one row per event date and quantile level by default, or one
row per posterior draw with `type = "draws"`.

Not every engine keeps draws. When the backend returned only summarised
quantiles, `type = "draws"` has nothing to give you.

## Arguments

- x:

  A
  [tbl_nowcast](https://rodrigozepeda.github.io/tbl.now/reference/tbl_nowcast.md).

- ...:

  Unused.

- type:

  Either `"quantiles"` (default) for the tidy quantile predictions, or
  `"draws"` for the posterior draws.

## Value

A `tibble`. For `type = "quantiles"`, one row per event date, stratum
and quantile level, with the quantile level in `.quantile_level` and the
predicted count in `.value`. For `type = "draws"`, one row per draw.

## Details

Registered in `.onLoad()` rather than assigned with
`S7::method(as_tibble, tbl_nowcast) <- `, which is what the neighbouring
[`print()`](https://rdrr.io/r/base/print.html) and
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) methods
use. Assigning an S7 method onto an *imported* generic copies that
generic into this namespace, and
[`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
carries `rownames = pkgconfig::get_config(...)` as a default argument –
so the copy makes `R CMD check` report pkgconfig as an undeclared `::`
import of a package this one never uses. Plain S3 registration
dispatches on `class(x)`, which for an S7 object is
`"tbl.now::tbl_nowcast"`, and copies nothing.

## See also

[tidy()](https://rodrigozepeda.github.io/tbl.now/reference/tidy.tbl_nowcast.md)
for the same content with the engine's metadata attached;
[autoplot()](https://rodrigozepeda.github.io/tbl.now/reference/autoplot.tbl_nowcast.md)
to plot it;
[`score_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/score_nowcast.md)
to score it against observed counts.

## Examples

``` r
predictions <- data.frame(
  onset_week = as.Date("2020-01-05"),
  .quantile_level = c(0.5, 0.9), .value = c(10, 14)
)
nc <- tbl_nowcast(predictions = predictions, method = "toy", event_date = "onset_week")
tibble::as_tibble(nc)
#> # A tibble: 2 × 3
#>   onset_week .quantile_level .value
#>   <date>               <dbl>  <dbl>
#> 1 2020-01-05             0.5     10
#> 2 2020-01-05             0.9     14
```
