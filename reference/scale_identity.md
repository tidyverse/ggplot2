# Use values without scaling

Use this set of scales when your data has already been scaled, i.e. it
already represents aesthetic values that ggplot2 can handle directly.
These scales will not produce a legend unless you also supply the
`breaks`, `labels`, and type of `guide` you want.

## Usage

``` r
scale_colour_identity(
  name = waiver(),
  ...,
  guide = "none",
  aesthetics = "colour"
)

scale_fill_identity(name = waiver(), ..., guide = "none", aesthetics = "fill")

scale_shape_identity(
  name = waiver(),
  ...,
  guide = "none",
  aesthetics = "shape"
)

scale_linetype_identity(
  name = waiver(),
  ...,
  guide = "none",
  aesthetics = "linetype"
)

scale_linewidth_identity(
  name = waiver(),
  ...,
  guide = "none",
  aesthetics = "linewidth"
)

scale_alpha_identity(
  name = waiver(),
  ...,
  guide = "none",
  aesthetics = "alpha"
)

scale_size_identity(name = waiver(), ..., guide = "none", aesthetics = "size")

scale_discrete_identity(aesthetics, name = waiver(), ..., guide = "none")

scale_continuous_identity(aesthetics, name = waiver(), ..., guide = "none")
```

## Arguments

- name:

  The name of the scale. Used as the axis or legend title. If
  [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.md), the
  default, the name of the scale is taken from the first mapping used
  for that aesthetic. If `NULL`, the legend title will be omitted.

- ...:

  Other arguments passed on to
  [`discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.md)
  or
  [`continuous_scale()`](https://ggplot2.tidyverse.org/reference/continuous_scale.md)

- guide:

  Guide to use for this scale. Defaults to `"none"`.

- aesthetics:

  Character string or vector of character strings listing the name(s) of
  the aesthetic(s) that this scale works with. This can be useful, for
  example, to apply colour settings to the `colour` and `fill`
  aesthetics at the same time, via `aesthetics = c("colour", "fill")`.

## Details

The functions `scale_colour_identity()`, `scale_fill_identity()`,
`scale_size_identity()`, etc. work on the aesthetics specified in the
scale name: `colour`, `fill`, `size`, etc. However, the functions
`scale_colour_identity()` and `scale_fill_identity()` also have an
optional `aesthetics` argument that can be used to define both `colour`
and `fill` aesthetic mappings via a single function call. The functions
`scale_discrete_identity()` and `scale_continuous_identity()` are
generic scales that can work with any aesthetic or set of aesthetics
provided via the `aesthetics` argument.

## See also

The [identity scales
section](https://ggplot2-book.org/scales-other#sec-scale-identity) of
the online ggplot2 book.

Other shape scales:
[`scale_shape()`](https://ggplot2.tidyverse.org/reference/scale_shape.md),
[`scale_shape_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.md).

Other linetype scales:
[`scale_linetype()`](https://ggplot2.tidyverse.org/reference/scale_linetype.md),
[`scale_linetype_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.md).

Other alpha scales:
[`scale_alpha()`](https://ggplot2.tidyverse.org/reference/scale_alpha.md),
[`scale_alpha_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.md).

Other size scales:
[`scale_size()`](https://ggplot2.tidyverse.org/reference/scale_size.md),
[`scale_size_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.md).

Other colour scales:
[`scale_alpha()`](https://ggplot2.tidyverse.org/reference/scale_alpha.md),
[`scale_colour_brewer()`](https://ggplot2.tidyverse.org/reference/scale_brewer.md),
[`scale_colour_continuous()`](https://ggplot2.tidyverse.org/reference/scale_colour_continuous.md),
[`scale_colour_discrete()`](https://ggplot2.tidyverse.org/reference/scale_colour_discrete.md),
[`scale_colour_gradient()`](https://ggplot2.tidyverse.org/reference/scale_gradient.md),
[`scale_colour_grey()`](https://ggplot2.tidyverse.org/reference/scale_grey.md),
[`scale_colour_hue()`](https://ggplot2.tidyverse.org/reference/scale_hue.md),
[`scale_colour_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.md),
[`scale_colour_steps()`](https://ggplot2.tidyverse.org/reference/scale_steps.md),
[`scale_colour_viridis_d()`](https://ggplot2.tidyverse.org/reference/scale_viridis.md)

## Examples

``` r
ggplot(luv_colours, aes(u, v)) +
  geom_point(aes(colour = col), size = 3) +
  scale_color_identity() +
  coord_fixed()


df <- data.frame(
  x = 1:4,
  y = 1:4,
  colour = c("red", "green", "blue", "yellow")
)
ggplot(df, aes(x, y)) + geom_tile(aes(fill = colour))

ggplot(df, aes(x, y)) +
  geom_tile(aes(fill = colour)) +
  scale_fill_identity()


# To get a legend guide, specify guide = "legend"
ggplot(df, aes(x, y)) +
  geom_tile(aes(fill = colour)) +
  scale_fill_identity(guide = "legend")

# But you'll typically also need to supply breaks and labels:
ggplot(df, aes(x, y)) +
  geom_tile(aes(fill = colour)) +
  scale_fill_identity("trt", labels = letters[1:4], breaks = df$colour,
  guide = "legend")


# cyl scaled to appropriate size
ggplot(mtcars, aes(mpg, wt)) +
  geom_point(aes(size = cyl))


# cyl used as point size
ggplot(mtcars, aes(mpg, wt)) +
  geom_point(aes(size = cyl)) +
  scale_size_identity()
```
