# Function to get an attribute from an object or default value

Checks if attribute exists in object and returns \`default\` if not

## Usage

``` r
attr_default(x, name, default = NULL)
```

## Arguments

- x:

  An object with attribute \`name\`

- name:

  The name of the attribute in \`x\`

- default:

  (optional) The default value of attribute \`name\` in \`x\`

## Examples

``` r
if (FALSE) { # \dontrun{
#Create an object
x <- 42
attr(x, "meaning") <- "meaning of life"

#See the attributes
attributes(x)

#Return the meaning attribute
attr_default(x, "meaning")

#Return null for attribute that doesn't exist
attr_default(x, "DOES_NOT_EXIST")

#Or return default when it doesn't exist
attr_default(x, "DOES_NOT_EXIST", default = 15)

} # }
```
