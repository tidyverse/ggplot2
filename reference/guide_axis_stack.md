# Stacked axis guides

This guide can stack other position guides that represent position
scales, like those created with
[scale\_(x\|y)\_continuous()](https://ggplot2.tidyverse.org/reference/scale_continuous.md)
and
[scale\_(x\|y)\_discrete()](https://ggplot2.tidyverse.org/reference/scale_discrete.md).

## Usage

``` r
guide_axis_stack(
  first = "axis",
  ...,
  title = waiver(),
  theme = NULL,
  spacing = NULL,
  order = 0,
  position = waiver()
)
```

## Arguments

- first:

  A position guide given as one of the following:

  - A string, for example `"axis"`.

  - A call to a guide function, for example
    [`guide_axis()`](https://ggplot2.tidyverse.org/reference/guide_axis.md).

- ...:

  Additional guides to stack given in the same manner as `first`.

- title:

  A character string or expression indicating a title of guide. If
  `NULL`, the title is not shown. By default
  ([`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.md)), the
  name of the scale object or the name specified in
  [`labs()`](https://ggplot2.tidyverse.org/reference/labs.md) is used
  for the title.

- theme:

  A [`theme`](https://ggplot2.tidyverse.org/reference/theme.md) object
  to style the guide individually or differently from the plot's theme
  settings. The `theme` argument in the guide partially overrides, and
  is combined with, the plot's theme.

- spacing:

  A [`unit()`](https://rdrr.io/r/grid/unit.html) objects that determines
  how far separate guides are spaced apart.

- order:

  A positive `integer` of length 1 that specifies the order of this
  guide among multiple guides. This controls in which order guides are
  merged if there are multiple guides for the same position. If 0
  (default), the order is determined by a secret algorithm.

- position:

  Where this guide should be drawn: one of top, bottom, left, or right.

## Details

The `first` guide will be placed closest to the panel and any subsequent
guides provided through `...` will follow in the given order.

## Examples

``` r
# A standard plot
p <- ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  theme(axis.line = element_line())

# A normal axis first, then a capped axis
p + guides(x = guide_axis_stack("axis", guide_axis(cap = "both")))
```
