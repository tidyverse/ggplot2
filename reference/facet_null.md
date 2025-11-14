# Facet specification: a single panel.

Facet specification: a single panel.

## Usage

``` r
facet_null(shrink = TRUE)
```

## Arguments

- shrink:

  If `TRUE`, will shrink scales to fit output of statistics, not raw
  data. If `FALSE`, will be range of raw data before statistical
  summary.

## Layer layout

The [`layer(layout)`](https://ggplot2.tidyverse.org/reference/layer.md)
argument in context of `facet_null()` is completely ignored.

## Examples

``` r
# facet_null is the default faceting specification if you
# don't override it with facet_grid or facet_wrap
ggplot(mtcars, aes(mpg, wt)) + geom_point()
```
