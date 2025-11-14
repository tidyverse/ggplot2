# Angle axis guide

This is a specialised guide used in
[`coord_radial()`](https://ggplot2.tidyverse.org/dev/reference/coord_radial.md)
to represent the theta position scale.

## Usage

``` r
guide_axis_theta(
  title = waiver(),
  theme = NULL,
  angle = waiver(),
  minor.ticks = FALSE,
  cap = "none",
  order = 0,
  position = waiver()
)
```

## Arguments

- title:

  A character string or expression indicating a title of guide. If
  `NULL`, the title is not shown. By default
  ([`waiver()`](https://ggplot2.tidyverse.org/dev/reference/waiver.md)),
  the name of the scale object or the name specified in
  [`labs()`](https://ggplot2.tidyverse.org/dev/reference/labs.md) is
  used for the title.

- theme:

  A [`theme`](https://ggplot2.tidyverse.org/dev/reference/theme.md)
  object to style the guide individually or differently from the plot's
  theme settings. The `theme` argument in the guide partially overrides,
  and is combined with, the plot's theme.

- angle:

  Compared to setting the angle in
  [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md) /
  [`element_text()`](https://ggplot2.tidyverse.org/dev/reference/element.md),
  this also uses some heuristics to automatically pick the `hjust` and
  `vjust` that you probably want. Can be one of the following:

  - `NULL` to take the angles and `hjust`/`vjust` directly from the
    theme.

  - [`waiver()`](https://ggplot2.tidyverse.org/dev/reference/waiver.md)
    to allow reasonable defaults in special cases.

  - A number representing the text angle in degrees.

- minor.ticks:

  Whether to draw the minor ticks (`TRUE`) or not draw minor ticks
  (`FALSE`, default).

- cap:

  A `character` to cut the axis line back to the last breaks. Can be
  `"none"` (default) to draw the axis line along the whole panel, or
  `"upper"` and `"lower"` to draw the axis to the upper or lower break,
  or `"both"` to only draw the line in between the most extreme breaks.
  `TRUE` and `FALSE` are shorthand for `"both"` and `"none"`
  respectively.

- order:

  A positive `integer` of length 1 that specifies the order of this
  guide among multiple guides. This controls in which order guides are
  merged if there are multiple guides for the same position. If 0
  (default), the order is determined by a secret algorithm.

- position:

  Where this guide should be drawn: one of top, bottom, left, or right.

## Note

The axis labels in this guide are insensitive to `hjust` and `vjust`
settings. The distance from the tick marks to the labels is determined
by the largest `margin` size set in the theme.

## Examples

``` r
# A plot using coord_radial
p <- ggplot(mtcars, aes(disp, mpg)) +
  geom_point() +
  coord_radial()

# The `angle` argument can be used to set relative angles
p + guides(theta = guide_axis_theta(angle = 0))
```
