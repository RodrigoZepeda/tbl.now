# List the available nowcasting methods

**\[experimental\]**

Scans the S3 methods registered for
[`nowcast_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_fit.md)
in every loaded namespace, so any backend you (or another package)
defined shows up here as soon as it is loaded.

`"example"` in the list is
[`example_engine()`](https://rodrigozepeda.github.io/tbl.now/reference/example_engine.md),
the toy used to keep this package's examples runnable. It is not a
nowcasting method – ignore it when choosing one.

## Usage

``` r
list_nowcast_methods(installed_only = TRUE)
```

## Arguments

- installed_only:

  Logical. When `TRUE` (default) only the methods whose backing package
  is installed are returned. Set to `FALSE` to see every registered
  method.

## Value

A character vector of method names, suitable for passing to
[`engine()`](https://rodrigozepeda.github.io/tbl.now/reference/engine.md).

## See also

[`engine()`](https://rodrigozepeda.github.io/tbl.now/reference/engine.md)
to turn one of these names into a configured model;
[nowcast_engines](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md)
for the engines that ship with the package;
[`nowcast_fit()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_fit.md)
and
[`nowcast_tidy()`](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_tidy.md),
the two functions a new method must define. The [*Adding your own
nowcasting model*
article](https://rodrigozepeda.github.io/tbl.now/articles/custom-nowcast-models.html)
walks through writing one.

## Examples

``` r
## What can this installation actually fit right now? ("example" is the toy
# engine, not a method you would nowcast with.)
list_nowcast_methods()
#> [1] "EpiNow2"         "NobBS"           "baselinenowcast" "epinowcast"     
#> [5] "example"         "surveillance"   

# Including methods whose modelling package is not installed
list_nowcast_methods(installed_only = FALSE)
#> [1] "EpiNow2"           "NobBS"             "baselinenowcast"  
#> [4] "diseasenowcasting" "epinowcast"        "example"          
#> [7] "surveillance"     
```
