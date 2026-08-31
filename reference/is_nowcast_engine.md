# Is this an engine?

**\[experimental\]**

Tests whether an object is a nowcasting engine – the specification built
by
[`engine()`](https://rodrigozepeda.github.io/tbl.now/reference/engine.md)
that says which modelling package to use and how to configure it. A bare
package name is *not* an engine, which is what this is usually used to
check.

## Usage

``` r
is_nowcast_engine(x)
```

## Arguments

- x:

  An object.

## Value

A single `TRUE` or `FALSE`.

## See also

[`engine()`](https://rodrigozepeda.github.io/tbl.now/reference/engine.md)
to build one;
[nowcast_engines](https://rodrigozepeda.github.io/tbl.now/reference/nowcast_engines.md)
for the engines that ship with the package;
[`run_nowcast()`](https://rodrigozepeda.github.io/tbl.now/reference/run_nowcast.md),
which takes one.

## Examples

``` r
## Built by engine(): yes.
is_nowcast_engine(engine("baselinenowcast"))
#> [1] TRUE

# The name of a package on its own: no.
is_nowcast_engine("baselinenowcast")
#> [1] FALSE
```
