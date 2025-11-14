# Render panel strips

All positions are rendered and it is up to the facet to decide which to
use

## Usage

``` r
render_strips(x = NULL, y = NULL, labeller = identity, theme)
```

## Arguments

- x, y:

  A data.frame with a column for each variable and a row for each
  combination to draw

- labeller:

  A labeller function

- theme:

  a `theme` object

## Value

A list with an "x" and a "y" element, each containing a "top" and
"bottom" or "left" and "right" element respectively. These contains a
list of rendered strips as gtables.
