# Checks whether the \`tbl_now\` object is valid

This is a wrapper for \[\`validate_tbl_now\`\] in a \[\`tryCatch()\`\]
in order to not error if the input object is invalid and returns
\`TRUE\` or \`FALSE\` on if the object is valid. If the object is valid
it can be "reconstructed" and not downgraded to a \`data.frame\`.

## Usage

``` r
tbl_now_can_reconstruct(data)
```

## Arguments

- data:

  A \`data.frame\` to be converted.

## Value

A boolean logical (\`TRUE\` or \`FALSE\`)
