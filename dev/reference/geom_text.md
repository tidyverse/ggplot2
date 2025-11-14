# Text

Text geoms are useful for labeling plots. They can be used by themselves
as scatterplots or in combination with other geoms, for example, for
labeling points or for annotating the height of bars. `geom_text()` adds
only text to the plot. `geom_label()` draws a rectangle behind the text,
making it easier to read.

## Usage

``` r
geom_label(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "nudge",
  ...,
  parse = FALSE,
  label.padding = unit(0.25, "lines"),
  label.r = unit(0.15, "lines"),
  label.size = deprecated(),
  border.colour = NULL,
  border.color = NULL,
  text.colour = NULL,
  text.color = NULL,
  size.unit = "mm",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_text(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "nudge",
  ...,
  parse = FALSE,
  check_overlap = FALSE,
  size.unit = "mm",
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

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot()`](https://ggplot2.tidyverse.org/dev/reference/ggplot.md).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`fortify()`](https://ggplot2.tidyverse.org/dev/reference/fortify.md)
  for which variables will be created.

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

- parse:

  If `TRUE`, the labels will be parsed into expressions and displayed as
  described in [`?plotmath`](https://rdrr.io/r/grDevices/plotmath.html).

- label.padding:

  Amount of padding around label. Defaults to 0.25 lines.

- label.r:

  Radius of rounded corners. Defaults to 0.15 lines.

- label.size:

  **\[deprecated\]** Replaced by the `linewidth` aesthetic. Size of
  label border, in mm.

- border.colour, border.color:

  Colour of label border. When `NULL` (default), the `colour` aesthetic
  determines the colour of the label border. `border.color` is an alias
  for `border.colour`.

- text.colour, text.color:

  Colour of the text. When `NULL` (default), the `colour` aesthetic
  determines the colour of the text. `text.color` is an alias for
  `text.colour`.

- size.unit:

  How the `size` aesthetic is interpreted: as millimetres (`"mm"`,
  default), points (`"pt"`), centimetres (`"cm"`), inches (`"in"`), or
  picas (`"pc"`).

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
  [`annotation_borders()`](https://ggplot2.tidyverse.org/dev/reference/annotation_borders.md).

- check_overlap:

  If `TRUE`, text that overlaps previous text in the same layer will not
  be plotted. `check_overlap` happens at draw time and in the order of
  the data. Therefore data should be arranged by the label column before
  calling `geom_text()`. Note that this argument is not supported by
  `geom_label()`.

## Details

Note that when you resize a plot, text labels stay the same size, even
though the size of the plot area changes. This happens because the
"width" and "height" of a text element are 0. Obviously, text labels do
have height and width, but they are physical units, not data units. For
the same reason, stacking and dodging text will not work by default, and
axis limits are not automatically expanded to include all text.

`geom_text()` and `geom_label()` add labels for each row in the data,
even if coordinates x, y are set to single values in the call to
`geom_label()` or `geom_text()`. To add labels at specified points use
[`annotate()`](https://ggplot2.tidyverse.org/dev/reference/annotate.md)
with `annotate(geom = "text", ...)` or `annotate(geom = "label", ...)`.

To automatically position non-overlapping text labels see the
[ggrepel](https://cran.r-project.org/package=ggrepel) package.

## `geom_label()`

Currently `geom_label()` does not support the `check_overlap` argument.
Also, it is considerably slower than `geom_text()`. The `fill` aesthetic
controls the background colour of the label.

## Alignment

You can modify text alignment with the `vjust` and `hjust` aesthetics.
These can either be a number between 0 (left/bottom) and 1 (right/top)
or a character (`"left"`, `"middle"`, `"right"`, `"bottom"`, `"center"`,
`"top"`). There are two special alignments: `"inward"` and `"outward"`.
Inward always aligns text towards the center, and outward aligns it away
from the center.

## See also

The [text labels
section](https://ggplot2-book.org/annotations#sec-text-labels) of the
online ggplot2 book.

## Aesthetics

`geom_text()` understands the following aesthetics. Required aesthetics
are displayed in bold and defaults are displayed for optional
aesthetics:

|     |                                                                                  |                                                                         |
|-----|----------------------------------------------------------------------------------|-------------------------------------------------------------------------|
| •   | **[`x`](https://ggplot2.tidyverse.org/dev/reference/aes_position.md)**           |                                                                         |
| •   | **[`y`](https://ggplot2.tidyverse.org/dev/reference/aes_position.md)**           |                                                                         |
| •   | **`label`**                                                                      |                                                                         |
| •   | [`alpha`](https://ggplot2.tidyverse.org/dev/reference/aes_colour_fill_alpha.md)  | → `NA`                                                                  |
| •   | `angle`                                                                          | → `0`                                                                   |
| •   | [`colour`](https://ggplot2.tidyverse.org/dev/reference/aes_colour_fill_alpha.md) | → via [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md) |
| •   | `family`                                                                         | → via [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md) |
| •   | `fontface`                                                                       | → `1`                                                                   |
| •   | [`group`](https://ggplot2.tidyverse.org/dev/reference/aes_group_order.md)        | → inferred                                                              |
| •   | `hjust`                                                                          | → `0.5`                                                                 |
| •   | `lineheight`                                                                     | → `1.2`                                                                 |
| •   | [`size`](https://ggplot2.tidyverse.org/dev/reference/aes_linetype_size_shape.md) | → via [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md) |
| •   | `vjust`                                                                          | → `0.5`                                                                 |

Learn more about setting these aesthetics in
[`vignette("ggplot2-specs")`](https://ggplot2.tidyverse.org/dev/articles/ggplot2-specs.md).

## Examples

``` r
p <- ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars)))

p + geom_text()

# Avoid overlaps
p + geom_text(check_overlap = TRUE)

# Labels with background
p + geom_label()

# Change size of the label
p + geom_text(size = 10)


# Set aesthetics to fixed value
p +
  geom_point() +
  geom_text(hjust = 0, nudge_x = 0.05)

p +
  geom_point() +
  geom_text(vjust = 0, nudge_y = 0.5)

p +
  geom_point() +
  geom_text(angle = 45)

if (FALSE) { # \dontrun{
# Doesn't work on all systems
p +
  geom_text(family = "Times New Roman")
} # }

# Add aesthetic mappings
p + geom_text(aes(colour = factor(cyl)))

p + geom_text(aes(colour = factor(cyl))) +
  scale_colour_hue(l = 40)

p + geom_label(aes(fill = factor(cyl)), colour = "white", fontface = "bold")


# Scale size of text, and change legend key glyph from a to point
p + geom_text(aes(size = wt), key_glyph = "point")

# Scale height of text, rather than sqrt(height)
p +
  geom_text(aes(size = wt), key_glyph = "point") +
  scale_radius(range = c(3,6))


# You can display expressions by setting parse = TRUE.  The
# details of the display are described in ?plotmath, but note that
# geom_text uses strings, not expressions.
p +
  geom_text(
    aes(label = paste(wt, "^(", cyl, ")", sep = "")),
    parse = TRUE
  )


# Add a text annotation
p +
  geom_text() +
  annotate(
    "text", label = "plot mpg vs. wt",
    x = 2, y = 15, size = 8, colour = "red"
  )


# \donttest{
# Aligning labels and bars --------------------------------------------------
df <- data.frame(
  x = factor(c(1, 1, 2, 2)),
  y = c(1, 3, 2, 1),
  grp = c("a", "b", "a", "b")
)

# ggplot2 doesn't know you want to give the labels the same virtual width
# as the bars:
ggplot(data = df, aes(x, y, group = grp)) +
  geom_col(aes(fill = grp), position = "dodge") +
  geom_text(aes(label = y), position = "dodge")
#> Warning: Width not defined
#> ℹ Set with `position_dodge(width = ...)`

# So tell it:
ggplot(data = df, aes(x, y, group = grp)) +
  geom_col(aes(fill = grp), position = "dodge") +
  geom_text(aes(label = y), position = position_dodge(0.9))

# You can't nudge and dodge text, so instead adjust the y position
ggplot(data = df, aes(x, y, group = grp)) +
  geom_col(aes(fill = grp), position = "dodge") +
  geom_text(
    aes(label = y, y = y + 0.05),
    position = position_dodge(0.9),
    vjust = 0
  )


# To place text in the middle of each bar in a stacked barplot, you
# need to set the vjust parameter of position_stack()
ggplot(data = df, aes(x, y, group = grp)) +
 geom_col(aes(fill = grp)) +
 geom_text(aes(label = y), position = position_stack(vjust = 0.5))


# Justification -------------------------------------------------------------
df <- data.frame(
  x = c(1, 1, 2, 2, 1.5),
  y = c(1, 2, 1, 2, 1.5),
  text = c("bottom-left", "top-left", "bottom-right", "top-right", "center")
)
ggplot(df, aes(x, y)) +
  geom_text(aes(label = text))

ggplot(df, aes(x, y)) +
  geom_text(aes(label = text), vjust = "inward", hjust = "inward")

# }
```
