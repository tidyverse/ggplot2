# Standardise aesthetic names

This function standardises aesthetic names by converting `color` to
`colour` (also in substrings, e.g. `point_color` to `point_colour`) and
translating old style R names to ggplot names (eg. `pch` to `shape`,
`cex` to `size`).

## Usage

``` r
standardise_aes_names(x)
```

## Arguments

- x:

  Character vector of aesthetics names, such as
  `c("colour", "size", "shape")`.

## Value

Character vector of standardised names.
