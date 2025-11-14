# Translating shape strings

`translate_shape_string()` is a helper function for translating point
shapes given as a character vector into integers that are interpreted by
the grid system.

## Usage

``` r
translate_shape_string(shape_string)
```

## Arguments

- shape_string:

  A character vector giving point shapes. Non-character input will be
  returned.

## Value

An integer vector with translated shapes.

## Examples

``` r
translate_shape_string(c("circle", "square", "triangle"))
#> [1] 19 15 17

# Strings with 1 or less characters are interpreted as symbols
translate_shape_string(c("a", "b", "?"))
#> [1] "a" "b" "?"
```
