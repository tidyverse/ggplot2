# Rectangles

`geom_rect()` and `geom_tile()` do the same thing, but are parameterised
differently: `geom_tile()` uses the center of the tile and its size
(`x`, `y`, `width`, `height`), while `geom_rect()` can use those or the
locations of the corners (`xmin`, `xmax`, `ymin` and `ymax`).
`geom_raster()` is a high performance special case for when all the
tiles are the same size, and no pattern fills are applied.

## Usage

``` r
geom_raster(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  interpolate = FALSE,
  hjust = 0.5,
  vjust = 0.5,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_rect(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  lineend = "butt",
  linejoin = "mitre",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_tile(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  lineend = "butt",
  linejoin = "mitre",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/dev/reference/aes.md). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  - `NULL` (default): the data is inherited from the plot data as
    specified in the call to
    [`ggplot()`](https://ggplot2.tidyverse.org/dev/reference/ggplot.md).

  - A `data.frame`, or other object, will override the plot data. All
    objects will be fortified to produce a data frame. See
    [`fortify()`](https://ggplot2.tidyverse.org/dev/reference/fortify.md)
    for which variables will be created.

  - A `function` will be called with a single argument, the plot data.
    The return value must be a `data.frame`, and will be used as the
    layer data. A `function` can be created from a `formula` (e.g.
    `~ head(.x, 10)`).

- stat:

  The statistical transformation to use on the data for this layer. When
  using a `geom_*()` function to construct a layer, the `stat` argument
  can be used to override the default coupling between geoms and stats.
  The `stat` argument accepts the following:

  - A `Stat` ggproto subclass, for example `StatCount`.

  - A string naming the stat. To give the stat as a string, strip the
    function name of the `stat_` prefix. For example, to use
    [`stat_count()`](https://ggplot2.tidyverse.org/dev/reference/geom_bar.md),
    give the stat as `"count"`.

  - For more information and other ways to specify the stat, see the
    [layer
    stat](https://ggplot2.tidyverse.org/dev/reference/layer_stats.md)
    documentation.

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    [`position_jitter()`](https://ggplot2.tidyverse.org/dev/reference/position_jitter.md).
    This method allows for passing extra arguments to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use
    [`position_jitter()`](https://ggplot2.tidyverse.org/dev/reference/position_jitter.md),
    give the position as `"jitter"`.

  - For more information and other ways to specify the position, see the
    [layer
    position](https://ggplot2.tidyverse.org/dev/reference/layer_positions.md)
    documentation.

- ...:

  Other arguments passed on to
  [`layer()`](https://ggplot2.tidyverse.org/dev/reference/layer.md)'s
  `params` argument. These arguments broadly fall into one of 4
  categories below. Notably, further arguments to the `position`
  argument, or aesthetics that are required can *not* be passed through
  `...`. Unknown arguments that are not part of the 4 categories below
  are ignored.

  - Static aesthetics that are not mapped to a scale, but are at a fixed
    value and apply to the layer as a whole. For example,
    `colour = "red"` or `linewidth = 3`. The geom's documentation has an
    **Aesthetics** section that lists the available options. The
    'required' aesthetics cannot be passed on to the `params`. Please
    note that while passing unmapped aesthetics as vectors is
    technically possible, the order and required length is not
    guaranteed to be parallel to the input data.

  - When constructing a layer using a `stat_*()` function, the `...`
    argument can be used to pass on parameters to the `geom` part of the
    layer. An example of this is
    `stat_density(geom = "area", outline.type = "both")`. The geom's
    documentation lists which parameters it can accept.

  - Inversely, when constructing a layer using a `geom_*()` function,
    the `...` argument can be used to pass on parameters to the `stat`
    part of the layer. An example of this is
    `geom_area(stat = "density", adjust = 0.5)`. The stat's
    documentation lists which parameters it can accept.

  - The `key_glyph` argument of
    [`layer()`](https://ggplot2.tidyverse.org/dev/reference/layer.md)
    may also be passed on through `...`. This can be one of the
    functions described as [key
    glyphs](https://ggplot2.tidyverse.org/dev/reference/draw_key.md), to
    change the display of the layer in the legend.

- interpolate:

  If `TRUE` interpolate linearly, if `FALSE` (the default) don't
  interpolate.

- hjust, vjust:

  horizontal and vertical justification of the grob. Each justification
  value should be a number between 0 and 1. Defaults to 0.5 for both,
  centering each pixel over its data location.

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- show.legend:

  Logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display. To include legend
  keys for all levels, even when no data exists, use `TRUE`. If `NA`,
  all levels are shown in legend, but unobserved levels are omitted.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`annotation_borders()`](https://ggplot2.tidyverse.org/dev/reference/annotation_borders.md).

- lineend:

  Line end style, one of `"round"`, `"butt"` or `"square"`.

- linejoin:

  Line join style, one of `"round"`, `"mitre"` or `"bevel"`.

## Details

Please note that the `width` and `height` aesthetics are not true
position aesthetics and therefore are not subject to scale
transformation. It is only after transformation that these aesthetics
are applied.

## Aesthetics

`geom_rect()` understands the following aesthetics. Required aesthetics
are displayed in bold and defaults are displayed for optional
aesthetics:

|     |                                                                                                                                                                                                                                            |                                                                         |
|-----|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------|
| •   | **[`x`](https://ggplot2.tidyverse.org/dev/reference/aes_position.md) *or* `width` *or* [`xmin`](https://ggplot2.tidyverse.org/dev/reference/aes_position.md) *or* [`xmax`](https://ggplot2.tidyverse.org/dev/reference/aes_position.md)**  |                                                                         |
| •   | **[`y`](https://ggplot2.tidyverse.org/dev/reference/aes_position.md) *or* `height` *or* [`ymin`](https://ggplot2.tidyverse.org/dev/reference/aes_position.md) *or* [`ymax`](https://ggplot2.tidyverse.org/dev/reference/aes_position.md)** |                                                                         |
| •   | [`alpha`](https://ggplot2.tidyverse.org/dev/reference/aes_colour_fill_alpha.md)                                                                                                                                                            | → `NA`                                                                  |
| •   | [`colour`](https://ggplot2.tidyverse.org/dev/reference/aes_colour_fill_alpha.md)                                                                                                                                                           | → via [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md) |
| •   | [`fill`](https://ggplot2.tidyverse.org/dev/reference/aes_colour_fill_alpha.md)                                                                                                                                                             | → via [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md) |
| •   | [`group`](https://ggplot2.tidyverse.org/dev/reference/aes_group_order.md)                                                                                                                                                                  | → inferred                                                              |
| •   | [`linetype`](https://ggplot2.tidyverse.org/dev/reference/aes_linetype_size_shape.md)                                                                                                                                                       | → via [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md) |
| •   | [`linewidth`](https://ggplot2.tidyverse.org/dev/reference/aes_linetype_size_shape.md)                                                                                                                                                      | → via [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md) |

`geom_tile()` understands only the `x`/`width` and `y`/`height`
combinations. Note that `geom_raster()` ignores `colour`.

Learn more about setting these aesthetics in
[`vignette("ggplot2-specs")`](https://ggplot2.tidyverse.org/dev/articles/ggplot2-specs.md).

## Examples

``` r
# The most common use for rectangles is to draw a surface. You always want
# to use geom_raster here because it's so much faster, and produces
# smaller output when saving to PDF
ggplot(faithfuld, aes(waiting, eruptions)) +
 geom_raster(aes(fill = density))


# Interpolation smooths the surface & is most helpful when rendering images.
ggplot(faithfuld, aes(waiting, eruptions)) +
 geom_raster(aes(fill = density), interpolate = TRUE)


# If you want to draw arbitrary rectangles, use geom_tile() or geom_rect()
df <- data.frame(
  x = rep(c(2, 5, 7, 9, 12), 2),
  y = rep(c(1, 2), each = 5),
  z = factor(rep(1:5, each = 2)),
  w = rep(diff(c(0, 4, 6, 8, 10, 14)), 2)
)
ggplot(df, aes(x, y)) +
  geom_tile(aes(fill = z), colour = "grey50")

ggplot(df, aes(x, y, width = w)) +
  geom_tile(aes(fill = z), colour = "grey50")

ggplot(df, aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y, ymax = y + 1)) +
  geom_rect(aes(fill = z), colour = "grey50")


# \donttest{
# Justification controls where the cells are anchored
df <- expand.grid(x = 0:5, y = 0:5)
set.seed(1)
df$z <- runif(nrow(df))
# default is compatible with geom_tile()
ggplot(df, aes(x, y, fill = z)) +
  geom_raster()

# zero padding
ggplot(df, aes(x, y, fill = z)) +
  geom_raster(hjust = 0, vjust = 0)


# Inspired by the image-density plots of Ken Knoblauch
cars <- ggplot(mtcars, aes(mpg, factor(cyl)))
cars + geom_point()

cars + stat_bin_2d(aes(fill = after_stat(count)), binwidth = c(3,1))

cars + stat_bin_2d(aes(fill = after_stat(density)), binwidth = c(3,1))


cars +
  stat_density(
    aes(fill = after_stat(density)),
    geom = "raster",
    position = "identity"
   )

cars +
  stat_density(
    aes(fill = after_stat(count)),
    geom = "raster",
    position = "identity"
  )

# }
```
