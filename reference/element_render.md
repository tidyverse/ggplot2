# Render a specified theme element into a grob

Given a theme object and element name, returns a grob for the element.
Uses
[`element_grob()`](https://ggplot2.tidyverse.org/reference/element_grob.md)
to generate the grob.

## Usage

``` r
element_render(theme, element, ..., name = NULL)
```

## Arguments

- theme:

  The theme object

- element:

  The element name given as character vector

- ...:

  Other arguments provided to
  [`element_grob()`](https://ggplot2.tidyverse.org/reference/element_grob.md)

- name:

  Character vector added to the name of the grob
