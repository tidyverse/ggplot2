# Render panel axes

These helpers facilitates generating theme compliant axes when building
up the plot.

## Usage

``` r
render_axes(x = NULL, y = NULL, coord, theme, transpose = FALSE)
```

## Arguments

- x, y:

  A list of ranges as available to the draw_panel method in `Facet`
  subclasses.

- coord:

  A `Coord` object

- theme:

  A `theme` object

- transpose:

  Should the output be transposed?

## Value

A list with the element "x" and "y" each containing axis specifications
for the ranges passed in. Each axis specification is a list with a "top"
and "bottom" element for x-axes and "left" and "right" element for
y-axis, holding the respective axis grobs. Depending on the content of x
and y some of the grobs might be zeroGrobs. If `transpose=TRUE` the
content of the x and y elements will be transposed so e.g. all left-axes
are collected in a left element as a list of grobs.
