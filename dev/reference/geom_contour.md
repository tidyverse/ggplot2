# 2D contours of a 3D surface

ggplot2 can not draw true 3D surfaces, but you can use `geom_contour()`,
`geom_contour_filled()`, and
[`geom_tile()`](https://ggplot2.tidyverse.org/dev/reference/geom_tile.md)
to visualise 3D surfaces in 2D.

These functions require regular data, where the `x` and `y` coordinates
form an equally spaced grid, and each combination of `x` and `y` appears
once. Missing values of `z` are allowed, but contouring will only work
for grid points where all four corners are non-missing. If you have
irregular data, you'll need to first interpolate on to a grid before
visualising, using `interp::interp()`, `akima::bilinear()`, or similar.

## Usage

``` r
geom_contour(
  mapping = NULL,
  data = NULL,
  stat = "contour",
  position = "identity",
  ...,
  bins = NULL,
  binwidth = NULL,
  breaks = NULL,
  arrow = NULL,
  arrow.fill = NULL,
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_contour_filled(
  mapping = NULL,
  data = NULL,
  stat = "contour_filled",
  position = "identity",
  ...,
  bins = NULL,
  binwidth = NULL,
  breaks = NULL,
  rule = "evenodd",
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_contour(
  mapping = NULL,
  data = NULL,
  geom = "contour",
  position = "identity",
  ...,
  bins = NULL,
  binwidth = NULL,
  breaks = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_contour_filled(
  mapping = NULL,
  data = NULL,
  geom = "contour_filled",
  position = "identity",
  ...,
  bins = NULL,
  binwidth = NULL,
  breaks = NULL,
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

- bins:

  Number of contour bins. Overridden by `breaks`.

- binwidth:

  The width of the contour bins. Overridden by `bins`.

- breaks:

  One of:

  - Numeric vector to set the contour breaks

  - A function that takes the range of the data and binwidth as input
    and returns breaks as output. A function can be created from a
    formula (e.g. ~ fullseq(.x, .y)).

  Overrides `binwidth` and `bins`. By default, this is a vector of
  length ten with [`pretty()`](https://rdrr.io/r/base/pretty.html)
  breaks.

- arrow:

  Arrow specification. Can be created by
  [`grid::arrow()`](https://rdrr.io/r/grid/arrow.html) or `NULL` to not
  draw an arrow.

- arrow.fill:

  Fill colour to use for closed arrowheads. `NULL` means use `colour`
  aesthetic.

- lineend:

  Line end style, one of `"round"`, `"butt"` or `"square"`.

- linejoin:

  Line join style, one of `"round"`, `"mitre"` or `"bevel"`.

- linemitre:

  Line mitre limit, a number greater than 1.

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

- rule:

  Either `"evenodd"` or `"winding"`. If polygons with holes are being
  drawn (using the `subgroup` aesthetic) this argument defines how the
  hole coordinates are interpreted. See the examples in
  [`grid::pathGrob()`](https://rdrr.io/r/grid/grid.path.html) for an
  explanation.

- geom:

  The geometric object to use to display the data for this layer. When
  using a `stat_*()` function to construct a layer, the `geom` argument
  can be used to override the default coupling between stats and geoms.
  The `geom` argument accepts the following:

  - A `Geom` ggproto subclass, for example `GeomPoint`.

  - A string naming the geom. To give the geom as a string, strip the
    function name of the `geom_` prefix. For example, to use
    [`geom_point()`](https://ggplot2.tidyverse.org/dev/reference/geom_point.md),
    give the geom as `"point"`.

  - For more information and other ways to specify the geom, see the
    [layer
    geom](https://ggplot2.tidyverse.org/dev/reference/layer_geoms.md)
    documentation.

## Computed variables

These are calculated by the 'stat' part of layers and can be accessed
with [delayed
evaluation](https://ggplot2.tidyverse.org/dev/reference/aes_eval.md).
The computed variables differ somewhat for contour lines (computed by
`stat_contour()`) and contour bands (filled contours, computed by
`stat_contour_filled()`). The variables `nlevel` and `piece` are
available for both, whereas `level_low`, `level_high`, and `level_mid`
are only available for bands. The variable `level` is a numeric or a
factor depending on whether lines or bands are calculated.

- `after_stat(level)`  
  Height of contour. For contour lines, this is a numeric vector that
  represents bin boundaries. For contour bands, this is an ordered
  factor that represents bin ranges.

- `after_stat(level_low)`, `after_stat(level_high)`,
  `after_stat(level_mid)`  
  (contour bands only) Lower and upper bin boundaries for each band, as
  well as the mid point between boundaries.

- `after_stat(nlevel)`  
  Height of contour, scaled to a maximum of 1.

- `after_stat(piece)`  
  Contour piece (an integer).

## Dropped variables

- `z`:

  After contouring, the z values of individual data points are no longer
  available.

## See also

[`geom_density_2d()`](https://ggplot2.tidyverse.org/dev/reference/geom_density_2d.md):
2d density contours

## Aesthetics

`geom_contour()` understands the following aesthetics. Required
aesthetics are displayed in bold and defaults are displayed for optional
aesthetics:

|     |                                                                                       |                                                                         |
|-----|---------------------------------------------------------------------------------------|-------------------------------------------------------------------------|
| •   | **[`x`](https://ggplot2.tidyverse.org/dev/reference/aes_position.md)**                |                                                                         |
| •   | **[`y`](https://ggplot2.tidyverse.org/dev/reference/aes_position.md)**                |                                                                         |
| •   | [`alpha`](https://ggplot2.tidyverse.org/dev/reference/aes_colour_fill_alpha.md)       | → `NA`                                                                  |
| •   | [`colour`](https://ggplot2.tidyverse.org/dev/reference/aes_colour_fill_alpha.md)      | → via [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md) |
| •   | [`group`](https://ggplot2.tidyverse.org/dev/reference/aes_group_order.md)             | → inferred                                                              |
| •   | [`linetype`](https://ggplot2.tidyverse.org/dev/reference/aes_linetype_size_shape.md)  | → via [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md) |
| •   | [`linewidth`](https://ggplot2.tidyverse.org/dev/reference/aes_linetype_size_shape.md) | → via [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md) |
| •   | `weight`                                                                              | → `1`                                                                   |

`geom_contour_filled()` understands the following aesthetics. Required
aesthetics are displayed in bold and defaults are displayed for optional
aesthetics:

|     |                                                                                       |                                                                         |
|-----|---------------------------------------------------------------------------------------|-------------------------------------------------------------------------|
| •   | **[`x`](https://ggplot2.tidyverse.org/dev/reference/aes_position.md)**                |                                                                         |
| •   | **[`y`](https://ggplot2.tidyverse.org/dev/reference/aes_position.md)**                |                                                                         |
| •   | [`alpha`](https://ggplot2.tidyverse.org/dev/reference/aes_colour_fill_alpha.md)       | → `NA`                                                                  |
| •   | [`colour`](https://ggplot2.tidyverse.org/dev/reference/aes_colour_fill_alpha.md)      | → via [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md) |
| •   | [`fill`](https://ggplot2.tidyverse.org/dev/reference/aes_colour_fill_alpha.md)        | → via [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md) |
| •   | [`group`](https://ggplot2.tidyverse.org/dev/reference/aes_group_order.md)             | → inferred                                                              |
| •   | [`linetype`](https://ggplot2.tidyverse.org/dev/reference/aes_linetype_size_shape.md)  | → via [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md) |
| •   | [`linewidth`](https://ggplot2.tidyverse.org/dev/reference/aes_linetype_size_shape.md) | → via [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md) |
| •   | `subgroup`                                                                            | → `NULL`                                                                |

`stat_contour()` understands the following aesthetics. Required
aesthetics are displayed in bold and defaults are displayed for optional
aesthetics:

|     |                                                                           |                       |
|-----|---------------------------------------------------------------------------|-----------------------|
| •   | **[`x`](https://ggplot2.tidyverse.org/dev/reference/aes_position.md)**    |                       |
| •   | **[`y`](https://ggplot2.tidyverse.org/dev/reference/aes_position.md)**    |                       |
| •   | **`z`**                                                                   |                       |
| •   | [`group`](https://ggplot2.tidyverse.org/dev/reference/aes_group_order.md) | → inferred            |
| •   | `order`                                                                   | → `after_stat(level)` |

`stat_contour_filled()` understands the following aesthetics. Required
aesthetics are displayed in bold and defaults are displayed for optional
aesthetics:

|     |                                                                                |                       |
|-----|--------------------------------------------------------------------------------|-----------------------|
| •   | **[`x`](https://ggplot2.tidyverse.org/dev/reference/aes_position.md)**         |                       |
| •   | **[`y`](https://ggplot2.tidyverse.org/dev/reference/aes_position.md)**         |                       |
| •   | **`z`**                                                                        |                       |
| •   | [`fill`](https://ggplot2.tidyverse.org/dev/reference/aes_colour_fill_alpha.md) | → `after_stat(level)` |
| •   | [`group`](https://ggplot2.tidyverse.org/dev/reference/aes_group_order.md)      | → inferred            |
| •   | `order`                                                                        | → `after_stat(level)` |

Learn more about setting these aesthetics in
[`vignette("ggplot2-specs")`](https://ggplot2.tidyverse.org/dev/articles/ggplot2-specs.md).

## Examples

``` r
# Basic plot
v <- ggplot(faithfuld, aes(waiting, eruptions, z = density))
v + geom_contour()


# Or compute from raw data
ggplot(faithful, aes(waiting, eruptions)) +
  geom_density_2d()


# \donttest{
# use geom_contour_filled() for filled contours
v + geom_contour_filled()


# Setting bins creates evenly spaced contours in the range of the data
v + geom_contour(bins = 3)

v + geom_contour(bins = 5)


# Setting binwidth does the same thing, parameterised by the distance
# between contours
v + geom_contour(binwidth = 0.01)

v + geom_contour(binwidth = 0.001)


# Other parameters
v + geom_contour(aes(colour = after_stat(level)))

v + geom_contour(colour = "red")

v + geom_raster(aes(fill = density)) +
  geom_contour(colour = "white")

# }
```
