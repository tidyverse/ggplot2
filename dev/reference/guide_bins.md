# A binned version of guide_legend

This guide is a version of the
[`guide_legend()`](https://ggplot2.tidyverse.org/dev/reference/guide_legend.md)
guide for binned scales. It differs in that it places ticks correctly
between the keys, and sports a small axis to better show the binning.
Like
[`guide_legend()`](https://ggplot2.tidyverse.org/dev/reference/guide_legend.md)
it can be used for all non-position aesthetics though colour and fill
defaults to
[`guide_coloursteps()`](https://ggplot2.tidyverse.org/dev/reference/guide_coloursteps.md),
and it will merge aesthetics together into the same guide if they are
mapped in the same way.

## Usage

``` r
guide_bins(
  title = waiver(),
  theme = NULL,
  angle = NULL,
  position = NULL,
  direction = NULL,
  override.aes = list(),
  reverse = FALSE,
  order = 0,
  show.limits = NULL,
  ...
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
  and is combined with, the plot's theme. Arguments that apply to a
  single legend are respected, most of which have the `legend`-prefix.
  Arguments that apply to combined legends (the legend box) are ignored,
  including `legend.position`, `legend.justification.*`,
  `legend.location` and `legend.box.*`.

- angle:

  Overrules the theme settings to automatically apply appropriate
  `hjust` and `vjust` for angled legend text. Can be a single number
  representing the text angle in degrees, or `NULL` to not overrule the
  settings (default).

- position:

  A character string indicating where the legend should be placed
  relative to the plot panels. One of "top", "right", "bottom", "left",
  or "inside".

- direction:

  A character string indicating the direction of the guide. One of
  "horizontal" or "vertical".

- override.aes:

  A list specifying aesthetic parameters of legend key. See details and
  examples.

- reverse:

  logical. If `TRUE` the order of legends is reversed.

- order:

  positive integer less than 99 that specifies the order of this guide
  among multiple guides. This controls the order in which multiple
  guides are displayed, not the contents of the guide itself. If 0
  (default), the order is determined by a secret algorithm.

- show.limits:

  Logical. Should the limits of the scale be shown with labels and
  ticks. Default is `NULL` meaning it will take the value from the
  scale. This argument is ignored if `labels` is given as a vector of
  values. If one or both of the limits is also given in `breaks` it will
  be shown irrespective of the value of `show.limits`.

- ...:

  ignored.

## Value

A guide object

## Use with discrete scale

This guide is intended to show binned data and work together with
ggplot2's binning scales. However, it is sometimes desirable to perform
the binning in a separate step, either as part of a stat (e.g.
[`stat_contour_filled()`](https://ggplot2.tidyverse.org/dev/reference/geom_contour.md))
or prior to the visualisation. If you want to use this guide for
discrete data the levels must follow the naming scheme implemented by
[`base::cut()`](https://rdrr.io/r/base/cut.html). This means that a bin
must be encoded as `"(<lower>, <upper>]"` with `<lower>` giving the
lower bound of the bin and `<upper>` giving the upper bound
(`"[<lower>, <upper>)"` is also accepted). If you use
[`base::cut()`](https://rdrr.io/r/base/cut.html) to perform the binning
everything should work as expected, if not, some recoding may be needed.

## See also

Other guides:
[`guide_colourbar()`](https://ggplot2.tidyverse.org/dev/reference/guide_colourbar.md),
[`guide_coloursteps()`](https://ggplot2.tidyverse.org/dev/reference/guide_coloursteps.md),
[`guide_legend()`](https://ggplot2.tidyverse.org/dev/reference/guide_legend.md),
[`guides()`](https://ggplot2.tidyverse.org/dev/reference/guides.md)

## Examples

``` r
p <- ggplot(mtcars) +
  geom_point(aes(disp, mpg, size = hp)) +
  scale_size_binned()

# Standard look
p


# Remove the axis or style it
p + guides(size = guide_bins(
  theme = theme(legend.axis.line = element_blank())
))


p + guides(size = guide_bins(show.limits = TRUE))


my_arrow <- arrow(length = unit(1.5, "mm"), ends = "both")
p + guides(size = guide_bins(
  theme = theme(legend.axis.line = element_line(arrow = my_arrow))
))


# Guides are merged together if possible
ggplot(mtcars) +
  geom_point(aes(disp, mpg, size = hp, colour = hp)) +
  scale_size_binned() +
  scale_colour_binned(guide = "bins")

```
