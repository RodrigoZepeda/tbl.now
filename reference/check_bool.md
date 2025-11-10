# Check a boolean value

Function that checks whether a boolean variable is \`TRUE\` or \`FALSE\`

## Usage

``` r
check_bool(x, name)
```

## Arguments

- x:

  A variable

- name:

  Name of the argument for printing error messages

## Value

invisible (TRUE). Called for its side effects.

## References

From https://stackoverflow.com/a/60346779/5067372

## Examples

``` r
if (FALSE) { # \dontrun{
check_bool(TRUE, "value")

#Using a non boolean gives an error message
not_a_boolean <- "hello"
check_bool(not_a_boolean, "not_a_boolean")
} # }
```
