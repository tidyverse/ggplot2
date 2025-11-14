# The theme class

The theme class holds information on how non-data elements of the plot
should be rendered. The preferred way to construct an object of this
class is through the
[`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md)
function.

## Usage

``` r
class_theme(elements = list(), ..., complete = FALSE, validate = TRUE)
```

## Arguments

- elements:

  A named list containing theme elements.

- ...:

  Reserved for future expansion.

- complete:

  A boolean value stating whether a theme is complete.

- validate:

  A boolean value stating whether a theme should still be validated.
