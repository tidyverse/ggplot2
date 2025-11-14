# Axis guide

Axis guides are the visual representation of position scales like those
created with
[scale\_(x\|y)\_continuous()](https://ggplot2.tidyverse.org/dev/reference/scale_continuous.md)
and
[scale\_(x\|y)\_discrete()](https://ggplot2.tidyverse.org/dev/reference/scale_discrete.md).

## Usage

``` r
guide_axis(
  title = waiver(),
  theme = NULL,
  check.overlap = FALSE,
  angle = waiver(),
  n.dodge = 1,
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

- check.overlap:

  silently remove overlapping labels, (recursively) prioritizing the
  first, last, and middle labels.

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

- n.dodge:

  The number of rows (for vertical axes) or columns (for horizontal
  axes) that should be used to render the labels. This is useful for
  displaying labels that would otherwise overlap.

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

## Examples

``` r
# plot with overlapping text
p <- ggplot(mpg, aes(cty * 100, hwy * 100)) +
  geom_point() +
  facet_wrap(vars(class))

# axis guides can be customized in the scale_* functions or
# using guides()
p + scale_x_continuous(guide = guide_axis(n.dodge = 2))

p + guides(x = guide_axis(angle = 90))


# can also be used to add a duplicate guide
p + guides(x = guide_axis(n.dodge = 2), y.sec = guide_axis())
```
