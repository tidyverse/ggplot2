# Line segments and curves

`geom_segment()` draws a straight line between points (x, y) and (xend,
yend). `geom_curve()` draws a curved line. See the underlying drawing
function [`grid::curveGrob()`](https://rdrr.io/r/grid/grid.curve.html)
for the parameters that control the curve.

## Usage

``` r
geom_segment(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  arrow = NULL,
  arrow.fill = NULL,
  lineend = "butt",
  linejoin = "round",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_curve(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  curvature = 0.5,
  angle = 90,
  ncp = 5,
  shape = 0.5,
  arrow = NULL,
  arrow.fill = NULL,
  lineend = "butt",
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

- curvature:

  A numeric value giving the amount of curvature. Negative values
  produce left-hand curves, positive values produce right-hand curves,
  and zero produces a straight line.

- angle:

  A numeric value between 0 and 180, giving an amount to skew the
  control points of the curve. Values less than 90 skew the curve
  towards the start point and values greater than 90 skew the curve
  towards the end point.

- ncp:

  The number of control points used to draw the curve. More control
  points creates a smoother curve.

- shape:

  A numeric vector of values between -1 and 1, which control the shape
  of the curve relative to its control points. See `grid.xspline` for
  more details.

## Details

Both geoms draw a single segment/curve per case. See
[`geom_path()`](https://ggplot2.tidyverse.org/dev/reference/geom_path.md)
if you need to connect points across multiple cases.

## See also

[`geom_path()`](https://ggplot2.tidyverse.org/dev/reference/geom_path.md)
and
[`geom_line()`](https://ggplot2.tidyverse.org/dev/reference/geom_path.md)
for multi- segment lines and paths.

[`geom_spoke()`](https://ggplot2.tidyverse.org/dev/reference/geom_spoke.md)
for a segment parameterised by a location (x, y), and an angle and
radius.

## Aesthetics

`geom_segment()` understands the following aesthetics. Required
aesthetics are displayed in bold and defaults are displayed for optional
aesthetics:

|     |                                                                                                                                                      |                                                                         |
|-----|------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------|
| •   | **[`x`](https://ggplot2.tidyverse.org/dev/reference/aes_position.md)**                                                                               |                                                                         |
| •   | **[`y`](https://ggplot2.tidyverse.org/dev/reference/aes_position.md)**                                                                               |                                                                         |
| •   | **[`xend`](https://ggplot2.tidyverse.org/dev/reference/aes_position.md) *or* [`yend`](https://ggplot2.tidyverse.org/dev/reference/aes_position.md)** |                                                                         |
| •   | [`alpha`](https://ggplot2.tidyverse.org/dev/reference/aes_colour_fill_alpha.md)                                                                      | → `NA`                                                                  |
| •   | [`colour`](https://ggplot2.tidyverse.org/dev/reference/aes_colour_fill_alpha.md)                                                                     | → via [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md) |
| •   | [`group`](https://ggplot2.tidyverse.org/dev/reference/aes_group_order.md)                                                                            | → inferred                                                              |
| •   | [`linetype`](https://ggplot2.tidyverse.org/dev/reference/aes_linetype_size_shape.md)                                                                 | → via [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md) |
| •   | [`linewidth`](https://ggplot2.tidyverse.org/dev/reference/aes_linetype_size_shape.md)                                                                | → via [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md) |

Learn more about setting these aesthetics in
[`vignette("ggplot2-specs")`](https://ggplot2.tidyverse.org/dev/articles/ggplot2-specs.md).

## Examples

``` r
b <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point()

df <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)
b +
 geom_curve(aes(x = x1, y = y1, xend = x2, yend = y2, colour = "curve"), data = df) +
 geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, colour = "segment"), data = df)


b + geom_curve(aes(x = x1, y = y1, xend = x2, yend = y2), data = df, curvature = -0.2)

b + geom_curve(aes(x = x1, y = y1, xend = x2, yend = y2), data = df, curvature = 1)

b + geom_curve(
  aes(x = x1, y = y1, xend = x2, yend = y2),
  data = df,
  arrow = arrow(length = unit(0.03, "npc"))
)


# The `shape` and `ncp` arguments of geom_curve control the sharpness of the spline
b +
  geom_curve(
    aes(x = x1, y = y1, xend = x2, yend = y2, colour = "ncp = 5"),
    data = df,
    curvature = 1,
    shape = 0,
    ncp = 5
  ) +
  geom_curve(
    aes(x = x1, y = y1, xend = x2, yend = y2, colour = "ncp = 1"),
    data = df,
    curvature = 1,
    shape = 0,
    ncp = 1
  )


if (requireNamespace('maps', quietly = TRUE)) {
ggplot(seals, aes(long, lat)) +
  geom_segment(aes(xend = long + delta_long, yend = lat + delta_lat),
    arrow = arrow(length = unit(0.1,"cm"))) +
  annotation_borders("state")
}


# Use lineend and linejoin to change the style of the segments
df2 <- expand.grid(
  lineend = c('round', 'butt', 'square'),
  linejoin = c('round', 'mitre', 'bevel'),
  stringsAsFactors = FALSE
)
df2 <- data.frame(df2, y = 1:9)
ggplot(df2, aes(x = 1, y = y, xend = 2, yend = y, label = paste(lineend, linejoin))) +
  geom_segment(
     lineend = df2$lineend, linejoin = df2$linejoin,
     size = 3, arrow = arrow(length = unit(0.3, "inches"))
  ) +
  geom_text(hjust = 'outside', nudge_x = -0.2) +
  xlim(0.5, 2)


# You can also use geom_segment to recreate plot(type = "h") :
set.seed(1)
counts <- as.data.frame(table(x = rpois(100,5)))
counts$x <- as.numeric(as.character(counts$x))
with(counts, plot(x, Freq, type = "h", lwd = 10))


ggplot(counts, aes(x, Freq)) +
  geom_segment(aes(xend = x, yend = 0), linewidth = 10, lineend = "butt")
```
