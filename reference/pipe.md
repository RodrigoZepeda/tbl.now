# Pipe operator

See `magrittr::%>%` for details.

## Usage

``` r
lhs %>% rhs
```

## Arguments

- lhs:

  A value or the dplyr placeholder.

- rhs:

  A function call using the dplyr semantics.

## Value

The result of calling \`rhs(lhs)\`.
