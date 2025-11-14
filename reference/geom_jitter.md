# Jittered points

The jitter geom is a convenient shortcut for
`geom_point(position = "jitter")`. It adds a small amount of random
variation to the location of each point, and is a useful way of handling
overplotting caused by discreteness in smaller datasets.

## Usage

``` r
geom_jitter(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "jitter",
  ...,
  width = NULL,
  height = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.md). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.md).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.md) for
  which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data. A `function` can be created from a `formula` (e.g.
  `~ head(.x, 10)`).

- stat:

  The statistical transformation to use on the data for this layer. When
  using a `geom_*()` function to construct a layer, the `stat` argument
  can be used to override the default coupling between geoms and stats.
  The `stat` argument accepts the following:

  - A `Stat` ggproto subclass, for example `StatCount`.

  - A string naming the stat. To give the stat as a string, strip the
    function name of the `stat_` prefix. For example, to use
    [`stat_count()`](https://ggplot2.tidyverse.org/reference/geom_bar.md),
    give the stat as `"count"`.

  - For more information and other ways to specify the stat, see the
    [layer stat](https://ggplot2.tidyverse.org/reference/layer_stats.md)
    documentation.

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.md).
    This method allows for passing extra arguments to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.md),
    give the position as `"jitter"`.

  - For more information and other ways to specify the position, see the
    [layer
    position](https://ggplot2.tidyverse.org/reference/layer_positions.md)
    documentation.

- ...:

  Other arguments passed on to
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.md)'s
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
    [`layer()`](https://ggplot2.tidyverse.org/reference/layer.md) may
    also be passed on through `...`. This can be one of the functions
    described as [key
    glyphs](https://ggplot2.tidyverse.org/reference/draw_key.md), to
    change the display of the layer in the legend.

- width, height:

  Amount of vertical and horizontal jitter. The jitter is added in both
  positive and negative directions, so the total spread is twice the
  value specified here.

  If omitted, defaults to 40% of the resolution of the data: this means
  the jitter values will occupy 80% of the implied bins. Categorical
  data is aligned on the integers, so a width or height of 0.5 will
  spread the data so it's not possible to see the distinction between
  the categories.

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
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
  [`annotation_borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.md).

## See also

[`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.md)
for regular, unjittered points,
[`geom_boxplot()`](https://ggplot2.tidyverse.org/reference/geom_boxplot.md)
for another way of looking at the conditional distribution of a variable

## Aesthetics

[`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.md)
understands the following aesthetics. Required aesthetics are displayed
in bold and defaults are displayed for optional aesthetics:

|     |                                                                               |                                                                     |
|-----|-------------------------------------------------------------------------------|---------------------------------------------------------------------|
| •   | **[`x`](https://ggplot2.tidyverse.org/reference/aes_position.md)**            |                                                                     |
| •   | **[`y`](https://ggplot2.tidyverse.org/reference/aes_position.md)**            |                                                                     |
| •   | [`alpha`](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.md)   | → `NA`                                                              |
| •   | [`colour`](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.md)  | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.md) |
| •   | [`fill`](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.md)    | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.md) |
| •   | [`group`](https://ggplot2.tidyverse.org/reference/aes_group_order.md)         | → inferred                                                          |
| •   | [`shape`](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.md) | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.md) |
| •   | [`size`](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.md)  | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.md) |
| •   | `stroke`                                                                      | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.md) |

Learn more about setting these aesthetics in
[`vignette("ggplot2-specs")`](https://ggplot2.tidyverse.org/articles/ggplot2-specs.md).

## Examples

``` r
p <- ggplot(mpg, aes(cyl, hwy))
p + geom_point()

p + geom_jitter()


# Add aesthetic mappings
p + geom_jitter(aes(colour = class))


# Use smaller width/height to emphasise categories
ggplot(mpg, aes(cyl, hwy)) +
  geom_jitter()

ggplot(mpg, aes(cyl, hwy)) +
  geom_jitter(width = 0.25)


# Use larger width/height to completely smooth away discreteness
ggplot(mpg, aes(cty, hwy)) +
  geom_jitter()

ggplot(mpg, aes(cty, hwy)) +
  geom_jitter(width = 0.5, height = 0.5)
```
