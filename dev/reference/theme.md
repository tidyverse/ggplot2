# Modify components of a theme

Themes are a powerful way to customize the non-data components of your
plots: i.e. titles, labels, fonts, background, gridlines, and legends.
Themes can be used to give plots a consistent customized look. Modify a
single plot's theme using `theme()`; see
[`theme_update()`](https://ggplot2.tidyverse.org/dev/reference/get_theme.md)
if you want modify the active theme, to affect all subsequent plots. Use
the themes available in [complete
themes](https://ggplot2.tidyverse.org/dev/reference/ggtheme.md) if you
would like to use a complete theme such as
[`theme_bw()`](https://ggplot2.tidyverse.org/dev/reference/ggtheme.md),
[`theme_minimal()`](https://ggplot2.tidyverse.org/dev/reference/ggtheme.md),
and more. Theme elements are documented together according to
inheritance, read more about theme inheritance below.

## Usage

``` r
theme(
  ...,
  line,
  rect,
  text,
  title,
  point,
  polygon,
  geom,
  spacing,
  margins,
  aspect.ratio,
  axis.title,
  axis.title.x,
  axis.title.x.top,
  axis.title.x.bottom,
  axis.title.y,
  axis.title.y.left,
  axis.title.y.right,
  axis.text,
  axis.text.x,
  axis.text.x.top,
  axis.text.x.bottom,
  axis.text.y,
  axis.text.y.left,
  axis.text.y.right,
  axis.text.theta,
  axis.text.r,
  axis.ticks,
  axis.ticks.x,
  axis.ticks.x.top,
  axis.ticks.x.bottom,
  axis.ticks.y,
  axis.ticks.y.left,
  axis.ticks.y.right,
  axis.ticks.theta,
  axis.ticks.r,
  axis.minor.ticks.x.top,
  axis.minor.ticks.x.bottom,
  axis.minor.ticks.y.left,
  axis.minor.ticks.y.right,
  axis.minor.ticks.theta,
  axis.minor.ticks.r,
  axis.ticks.length,
  axis.ticks.length.x,
  axis.ticks.length.x.top,
  axis.ticks.length.x.bottom,
  axis.ticks.length.y,
  axis.ticks.length.y.left,
  axis.ticks.length.y.right,
  axis.ticks.length.theta,
  axis.ticks.length.r,
  axis.minor.ticks.length,
  axis.minor.ticks.length.x,
  axis.minor.ticks.length.x.top,
  axis.minor.ticks.length.x.bottom,
  axis.minor.ticks.length.y,
  axis.minor.ticks.length.y.left,
  axis.minor.ticks.length.y.right,
  axis.minor.ticks.length.theta,
  axis.minor.ticks.length.r,
  axis.line,
  axis.line.x,
  axis.line.x.top,
  axis.line.x.bottom,
  axis.line.y,
  axis.line.y.left,
  axis.line.y.right,
  axis.line.theta,
  axis.line.r,
  legend.background,
  legend.margin,
  legend.spacing,
  legend.spacing.x,
  legend.spacing.y,
  legend.key,
  legend.key.size,
  legend.key.height,
  legend.key.width,
  legend.key.spacing,
  legend.key.spacing.x,
  legend.key.spacing.y,
  legend.key.justification,
  legend.frame,
  legend.ticks,
  legend.ticks.length,
  legend.axis.line,
  legend.text,
  legend.text.position,
  legend.title,
  legend.title.position,
  legend.position,
  legend.position.inside,
  legend.direction,
  legend.byrow,
  legend.justification,
  legend.justification.top,
  legend.justification.bottom,
  legend.justification.left,
  legend.justification.right,
  legend.justification.inside,
  legend.location,
  legend.box,
  legend.box.just,
  legend.box.margin,
  legend.box.background,
  legend.box.spacing,
  panel.background,
  panel.border,
  panel.spacing,
  panel.spacing.x,
  panel.spacing.y,
  panel.grid,
  panel.grid.major,
  panel.grid.minor,
  panel.grid.major.x,
  panel.grid.major.y,
  panel.grid.minor.x,
  panel.grid.minor.y,
  panel.ontop,
  panel.widths,
  panel.heights,
  plot.background,
  plot.title,
  plot.title.position,
  plot.subtitle,
  plot.caption,
  plot.caption.position,
  plot.tag,
  plot.tag.position,
  plot.tag.location,
  plot.margin,
  strip.background,
  strip.background.x,
  strip.background.y,
  strip.clip,
  strip.placement,
  strip.text,
  strip.text.x,
  strip.text.x.bottom,
  strip.text.x.top,
  strip.text.y,
  strip.text.y.left,
  strip.text.y.right,
  strip.switch.pad.grid,
  strip.switch.pad.wrap,
  complete = FALSE,
  validate = TRUE
)
```

## Arguments

- ...:

  additional element specifications not part of base ggplot2. In
  general, these should also be defined in the `element tree` argument.
  [Splicing](https://rlang.r-lib.org/reference/splice.html) a list is
  also supported.

- line:

  all line elements
  ([`element_line()`](https://ggplot2.tidyverse.org/dev/reference/element.md))

- rect:

  all rectangular elements
  ([`element_rect()`](https://ggplot2.tidyverse.org/dev/reference/element.md))

- text:

  all text elements
  ([`element_text()`](https://ggplot2.tidyverse.org/dev/reference/element.md))

- title:

  all title elements: plot, axes, legends
  ([`element_text()`](https://ggplot2.tidyverse.org/dev/reference/element.md);
  inherits from `text`)

- point:

  all point elements
  ([`element_point()`](https://ggplot2.tidyverse.org/dev/reference/element.md))

- polygon:

  all polygon elements
  ([`element_polygon()`](https://ggplot2.tidyverse.org/dev/reference/element.md))

- geom:

  defaults for geoms
  ([`element_geom()`](https://ggplot2.tidyverse.org/dev/reference/element.md))

- spacing:

  all spacings ([`unit()`](https://rdrr.io/r/grid/unit.html))

- margins:

  all margins
  ([`margin()`](https://ggplot2.tidyverse.org/dev/reference/element.md))

- aspect.ratio:

  aspect ratio of the panel

- axis.title, axis.title.x, axis.title.y, axis.title.x.top,
  axis.title.x.bottom, axis.title.y.left, axis.title.y.right:

  labels of axes
  ([`element_text()`](https://ggplot2.tidyverse.org/dev/reference/element.md)).
  Specify all axes' labels (`axis.title`), labels by plane (using
  `axis.title.x` or `axis.title.y`), or individually for each axis
  (using `axis.title.x.bottom`, `axis.title.x.top`, `axis.title.y.left`,
  `axis.title.y.right`). `axis.title.*.*` inherits from `axis.title.*`
  which inherits from `axis.title`, which in turn inherits from `text`

- axis.text, axis.text.x, axis.text.y, axis.text.x.top,
  axis.text.x.bottom, axis.text.y.left, axis.text.y.right,
  axis.text.theta, axis.text.r:

  tick labels along axes
  ([`element_text()`](https://ggplot2.tidyverse.org/dev/reference/element.md)).
  Specify all axis tick labels (`axis.text`), tick labels by plane
  (using `axis.text.x` or `axis.text.y`), or individually for each axis
  (using `axis.text.x.bottom`, `axis.text.x.top`, `axis.text.y.left`,
  `axis.text.y.right`). `axis.text.*.*` inherits from `axis.text.*`
  which inherits from `axis.text`, which in turn inherits from `text`

- axis.ticks, axis.ticks.x, axis.ticks.x.top, axis.ticks.x.bottom,
  axis.ticks.y, axis.ticks.y.left, axis.ticks.y.right, axis.ticks.theta,
  axis.ticks.r:

  tick marks along axes
  ([`element_line()`](https://ggplot2.tidyverse.org/dev/reference/element.md)).
  Specify all tick marks (`axis.ticks`), ticks by plane (using
  `axis.ticks.x` or `axis.ticks.y`), or individually for each axis
  (using `axis.ticks.x.bottom`, `axis.ticks.x.top`, `axis.ticks.y.left`,
  `axis.ticks.y.right`). `axis.ticks.*.*` inherits from `axis.ticks.*`
  which inherits from `axis.ticks`, which in turn inherits from `line`

- axis.minor.ticks.x.top, axis.minor.ticks.x.bottom,
  axis.minor.ticks.y.left, axis.minor.ticks.y.right,
  axis.minor.ticks.theta, axis.minor.ticks.r, :

  minor tick marks along axes
  ([`element_line()`](https://ggplot2.tidyverse.org/dev/reference/element.md)).
  `axis.minor.ticks.*.*` inherit from the corresponding major ticks
  `axis.ticks.*.*`.

- axis.ticks.length, axis.ticks.length.x, axis.ticks.length.x.top,
  axis.ticks.length.x.bottom, axis.ticks.length.y,
  axis.ticks.length.y.left, axis.ticks.length.y.right,
  axis.ticks.length.theta, axis.ticks.length.r:

  length of tick marks (`unit`). `axis.ticks.length` inherits from
  `spacing`.

- axis.minor.ticks.length, axis.minor.ticks.length.x,
  axis.minor.ticks.length.x.top, axis.minor.ticks.length.x.bottom,
  axis.minor.ticks.length.y, axis.minor.ticks.length.y.left,
  axis.minor.ticks.length.y.right, axis.minor.ticks.length.theta,
  axis.minor.ticks.length.r:

  length of minor tick marks (`unit`), or relative to
  `axis.ticks.length` when provided with
  [`rel()`](https://ggplot2.tidyverse.org/dev/reference/element.md).

- axis.line, axis.line.x, axis.line.x.top, axis.line.x.bottom,
  axis.line.y, axis.line.y.left, axis.line.y.right, axis.line.theta,
  axis.line.r:

  lines along axes
  ([`element_line()`](https://ggplot2.tidyverse.org/dev/reference/element.md)).
  Specify lines along all axes (`axis.line`), lines for each plane
  (using `axis.line.x` or `axis.line.y`), or individually for each axis
  (using `axis.line.x.bottom`, `axis.line.x.top`, `axis.line.y.left`,
  `axis.line.y.right`). `axis.line.*.*` inherits from `axis.line.*`
  which inherits from `axis.line`, which in turn inherits from `line`

- legend.background:

  background of legend
  ([`element_rect()`](https://ggplot2.tidyverse.org/dev/reference/element.md);
  inherits from `rect`)

- legend.margin:

  the margin around each legend
  ([`margin()`](https://ggplot2.tidyverse.org/dev/reference/element.md));
  inherits from `margins`.

- legend.spacing, legend.spacing.x, legend.spacing.y:

  the spacing between legends (`unit`). `legend.spacing.x` &
  `legend.spacing.y` inherit from `legend.spacing` or can be specified
  separately. `legend.spacing` inherits from `spacing`.

- legend.key:

  background underneath legend keys
  ([`element_rect()`](https://ggplot2.tidyverse.org/dev/reference/element.md);
  inherits from `rect`)

- legend.key.size, legend.key.height, legend.key.width:

  size of legend keys (`unit`); key background height & width inherit
  from `legend.key.size` or can be specified separately. In turn
  `legend.key.size` inherits from `spacing`.

- legend.key.spacing, legend.key.spacing.x, legend.key.spacing.y:

  spacing between legend keys given as a `unit`. Spacing in the
  horizontal (x) and vertical (y) direction inherit from
  `legend.key.spacing` or can be specified separately.
  `legend.key.spacing` inherits from `spacing`.

- legend.key.justification:

  Justification for positioning legend keys when more space is available
  than needed for display. The default, `NULL`, stretches keys into the
  available space. Can be a location like `"center"` or `"top"`, or a
  two-element numeric vector.

- legend.frame:

  frame drawn around the bar
  ([`element_rect()`](https://ggplot2.tidyverse.org/dev/reference/element.md)).

- legend.ticks:

  tick marks shown along bars or axes
  ([`element_line()`](https://ggplot2.tidyverse.org/dev/reference/element.md))

- legend.ticks.length:

  length of tick marks in legend
  ([`unit()`](https://rdrr.io/r/grid/unit.html)); inherits from
  `legend.key.size`.

- legend.axis.line:

  lines along axes in legends
  ([`element_line()`](https://ggplot2.tidyverse.org/dev/reference/element.md))

- legend.text:

  legend item labels
  ([`element_text()`](https://ggplot2.tidyverse.org/dev/reference/element.md);
  inherits from `text`)

- legend.text.position:

  placement of legend text relative to legend keys or bars ("top",
  "right", "bottom" or "left"). The legend text placement might be
  incompatible with the legend's direction for some guides.

- legend.title:

  title of legend
  ([`element_text()`](https://ggplot2.tidyverse.org/dev/reference/element.md);
  inherits from `title`)

- legend.title.position:

  placement of legend title relative to the main legend ("top", "right",
  "bottom" or "left").

- legend.position:

  the default position of legends ("none", "left", "right", "bottom",
  "top", "inside")

- legend.position.inside:

  A numeric vector of length two setting the placement of legends that
  have the `"inside"` position.

- legend.direction:

  layout of items in legends ("horizontal" or "vertical")

- legend.byrow:

  whether the legend-matrix is filled by columns (`FALSE`, the default)
  or by rows (`TRUE`).

- legend.justification:

  anchor point for positioning legend inside plot ("center" or
  two-element numeric vector) or the justification according to the plot
  area when positioned outside the plot

- legend.justification.top, legend.justification.bottom,
  legend.justification.left, legend.justification.right,
  legend.justification.inside:

  Same as `legend.justification` but specified per `legend.position`
  option.

- legend.location:

  Relative placement of legends outside the plot as a string. Can be
  `"panel"` (default) to align legends to the panels or `"plot"` to
  align legends to the plot as a whole.

- legend.box:

  arrangement of multiple legends ("horizontal" or "vertical")

- legend.box.just:

  justification of each legend within the overall bounding box, when
  there are multiple legends ("top", "bottom", "left", "right", "center"
  or "centre")

- legend.box.margin:

  margins around the full legend area, as specified using
  [`margin()`](https://ggplot2.tidyverse.org/dev/reference/element.md);
  inherits from `margins`.

- legend.box.background:

  background of legend area
  ([`element_rect()`](https://ggplot2.tidyverse.org/dev/reference/element.md);
  inherits from `rect`)

- legend.box.spacing:

  The spacing between the plotting area and the legend box (`unit`);
  inherits from `spacing`.

- panel.background:

  background of plotting area, drawn underneath plot
  ([`element_rect()`](https://ggplot2.tidyverse.org/dev/reference/element.md);
  inherits from `rect`)

- panel.border:

  border around plotting area, drawn on top of plot so that it covers
  tick marks and grid lines. This should be used with `fill = NA`
  ([`element_rect()`](https://ggplot2.tidyverse.org/dev/reference/element.md);
  inherits from `rect`)

- panel.spacing, panel.spacing.x, panel.spacing.y:

  spacing between facet panels (`unit`). `panel.spacing.x` &
  `panel.spacing.y` inherit from `panel.spacing` or can be specified
  separately. `panel.spacing` inherits from `spacing`.

- panel.grid, panel.grid.major, panel.grid.minor, panel.grid.major.x,
  panel.grid.major.y, panel.grid.minor.x, panel.grid.minor.y:

  grid lines
  ([`element_line()`](https://ggplot2.tidyverse.org/dev/reference/element.md)).
  Specify major grid lines, or minor grid lines separately (using
  `panel.grid.major` or `panel.grid.minor`) or individually for each
  axis (using `panel.grid.major.x`, `panel.grid.minor.x`,
  `panel.grid.major.y`, `panel.grid.minor.y`). Y axis grid lines are
  horizontal and x axis grid lines are vertical. `panel.grid.*.*`
  inherits from `panel.grid.*` which inherits from `panel.grid`, which
  in turn inherits from `line`

- panel.ontop:

  option to place the panel (background, gridlines) over the data layers
  (`logical`). Usually used with a transparent or blank
  `panel.background`.

- panel.widths, panel.heights:

  Sizes for panels (`units`). Can be a single unit to set the total size
  for the panel area, or a unit vector to set the size of individual
  panels. Using this setting overrides the aspect ratio set by the
  theme, coord or facets. An exception is made when the plot has a
  single panel and exactly one of the width *or* height is set, in which
  case an attempt is made to preserve the aspect ratio.

- plot.background:

  background of the entire plot
  ([`element_rect()`](https://ggplot2.tidyverse.org/dev/reference/element.md);
  inherits from `rect`)

- plot.title:

  plot title (text appearance)
  ([`element_text()`](https://ggplot2.tidyverse.org/dev/reference/element.md);
  inherits from `title`) left-aligned by default

- plot.title.position, plot.caption.position:

  Alignment of the plot title/subtitle and caption. The setting for
  `plot.title.position` applies to both the title and the subtitle. A
  value of "panel" (the default) means that titles and/or caption are
  aligned to the plot panels. A value of "plot" means that titles and/or
  caption are aligned to the entire plot (minus any space for margins
  and plot tag).

- plot.subtitle:

  plot subtitle (text appearance)
  ([`element_text()`](https://ggplot2.tidyverse.org/dev/reference/element.md);
  inherits from `title`) left-aligned by default

- plot.caption:

  caption below the plot (text appearance)
  ([`element_text()`](https://ggplot2.tidyverse.org/dev/reference/element.md);
  inherits from `title`) right-aligned by default

- plot.tag:

  upper-left label to identify a plot (text appearance)
  ([`element_text()`](https://ggplot2.tidyverse.org/dev/reference/element.md);
  inherits from `title`) left-aligned by default

- plot.tag.position:

  The position of the tag as a string ("topleft", "top", "topright",
  "left", "right", "bottomleft", "bottom", "bottomright") or a
  coordinate. If a coordinate, can be a numeric vector of length 2 to
  set the x,y-coordinate relative to the whole plot. The coordinate
  option is unavailable for `plot.tag.location = "margin"`.

- plot.tag.location:

  The placement of the tag as a string, one of `"panel"`, `"plot"` or
  `"margin"`. Respectively, these will place the tag inside the panel
  space, anywhere in the plot as a whole, or in the margin around the
  panel space.

- plot.margin:

  margin around entire plot (`unit` with the sizes of the top, right,
  bottom, and left margins); inherits from `margin`.

- strip.background, strip.background.x, strip.background.y:

  background of facet labels
  ([`element_rect()`](https://ggplot2.tidyverse.org/dev/reference/element.md);
  inherits from `rect`). Horizontal facet background
  (`strip.background.x`) & vertical facet background
  (`strip.background.y`) inherit from `strip.background` or can be
  specified separately

- strip.clip:

  should strip background edges and strip labels be clipped to the
  extend of the strip background? Options are `"on"` to clip, `"off"` to
  disable clipping or `"inherit"` (default) to take the clipping setting
  from the parent viewport.

- strip.placement:

  placement of strip with respect to axes, either "inside" or "outside".
  Only important when axes and strips are on the same side of the plot.

- strip.text, strip.text.x, strip.text.y, strip.text.x.top,
  strip.text.x.bottom, strip.text.y.left, strip.text.y.right:

  facet labels
  ([`element_text()`](https://ggplot2.tidyverse.org/dev/reference/element.md);
  inherits from `text`). Horizontal facet labels (`strip.text.x`) &
  vertical facet labels (`strip.text.y`) inherit from `strip.text` or
  can be specified separately. Facet strips have dedicated
  position-dependent theme elements (`strip.text.x.top`,
  `strip.text.x.bottom`, `strip.text.y.left`, `strip.text.y.right`) that
  inherit from `strip.text.x` and `strip.text.y`, respectively. As a
  consequence, some theme stylings need to be applied to the
  position-dependent elements rather than to the parent elements

- strip.switch.pad.grid, strip.switch.pad.wrap:

  space between strips and axes when strips are switched (`unit`);
  inherits from `spacing`.

- complete:

  set this to `TRUE` if this is a complete theme, such as the one
  returned by
  [`theme_grey()`](https://ggplot2.tidyverse.org/dev/reference/ggtheme.md).
  Complete themes behave differently when added to a ggplot object.
  Also, when setting `complete = TRUE` all elements will be set to
  inherit from blank elements.

- validate:

  `TRUE` to run `check_element()`, `FALSE` to bypass checks.

## Theme inheritance

Theme elements inherit properties from other theme elements
hierarchically. For example, `axis.title.x.bottom` inherits from
`axis.title.x` which inherits from `axis.title`, which in turn inherits
from `text`. All text elements inherit directly or indirectly from
`text`; all lines inherit from `line`, and all rectangular objects
inherit from `rect`. This means that you can modify the appearance of
multiple elements by setting a single high-level component.

Learn more about setting these aesthetics in
[`vignette("ggplot2-specs")`](https://ggplot2.tidyverse.org/dev/articles/ggplot2-specs.md).

## See also

[`add_gg()`](https://ggplot2.tidyverse.org/dev/reference/gg-add.md) and
[`%+replace%`](https://ggplot2.tidyverse.org/dev/reference/get_theme.md),
[`element_blank()`](https://ggplot2.tidyverse.org/dev/reference/element.md),
[`element_line()`](https://ggplot2.tidyverse.org/dev/reference/element.md),
[`element_rect()`](https://ggplot2.tidyverse.org/dev/reference/element.md),
and
[`element_text()`](https://ggplot2.tidyverse.org/dev/reference/element.md)
for details of the specific theme elements.

The [modifying theme
components](https://ggplot2-book.org/themes#modifying-theme-components)
and [theme elements
sections](https://ggplot2-book.org/themes#sec-theme-elements) of the
online ggplot2 book.

## Examples

``` r
p1 <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  labs(title = "Fuel economy declines as weight increases")
p1


# Plot ---------------------------------------------------------------------
p1 + theme(plot.title = element_text(size = rel(2)))

p1 + theme(plot.background = element_rect(fill = "green"))


# Panels --------------------------------------------------------------------

p1 + theme(panel.background = element_rect(fill = "white", colour = "grey50"))

p1 + theme(panel.border = element_rect(linetype = "dashed"))

p1 + theme(panel.grid.major = element_line(colour = "black"))

p1 + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank()
)


# Put gridlines on top of data
p1 + theme(
  panel.background = element_rect(fill = NA),
  panel.grid.major = element_line(colour = "grey50"),
  panel.ontop = TRUE
)


# Axes ----------------------------------------------------------------------
# Change styles of axes texts and lines
p1 + theme(axis.line = element_line(linewidth = 3, colour = "grey80"))

p1 + theme(axis.text = element_text(colour = "blue"))

p1 + theme(axis.ticks = element_line(linewidth = 2))


# Change the appearance of the y-axis title
p1 + theme(axis.title.y = element_text(size = rel(1.5), angle = 90))


# Make ticks point outwards on y-axis and inwards on x-axis
p1 + theme(
  axis.ticks.length.y = unit(.25, "cm"),
  axis.ticks.length.x = unit(-.25, "cm"),
  axis.text.x = element_text(margin = margin(t = .3, unit = "cm"))
)


# \donttest{
# Legend --------------------------------------------------------------------
p2 <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point(aes(colour = factor(cyl), shape = factor(vs))) +
  labs(
    x = "Weight (1000 lbs)",
    y = "Fuel economy (mpg)",
    colour = "Cylinders",
    shape = "Transmission"
   )
p2


# Position
p2 + theme(legend.position = "none")

p2 + theme(legend.justification = "top")

p2 + theme(legend.position = "bottom")


# Or place legends inside the plot using relative coordinates between 0 and 1
# legend.justification sets the corner that the position refers to
p2 + theme(
  legend.position = "inside",
  legend.position.inside = c(.95, .95),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin_auto(6)
)


# The legend.box properties work similarly for the space around
# all the legends
p2 + theme(
  legend.box.background = element_rect(),
  legend.box.margin = margin_auto(6)
)


# You can also control the display of the keys
# and the justification related to the plot area can be set
p2 + theme(legend.key = element_rect(fill = "white", colour = "black"))

p2 + theme(legend.text = element_text(size = 8, colour = "red"))

p2 + theme(legend.title = element_text(face = "bold"))


# Strips --------------------------------------------------------------------

p3 <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  facet_wrap(~ cyl)
p3


p3 + theme(strip.background = element_rect(colour = "black", fill = "white"))

p3 + theme(strip.text.x = element_text(colour = "white", face = "bold"))

# More direct strip.text.x here for top
# as in the facet_wrap the default strip.position is "top"
p3 + theme(strip.text.x.top = element_text(colour = "white", face = "bold"))

p3 + theme(panel.spacing = unit(1, "lines"))


# Colours -------------------------------------------------------------------

p4 <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  annotate(label = "Text Annotation", x = 5, y = 30, geom = "text")

# You can use the 'ink' setting to set colour defaults for all layers
p4 + theme(geom = element_geom(ink = "dodgerblue"))

# Alternate colours are derived from the 'accent' and 'paper' settings
p4 + geom_smooth(method = "lm", formula = y ~ x) +
  theme(geom = element_geom(accent = "tomato", paper = "orchid"))

# You can also set default palettes in the theme
p4 + aes(colour = drat) +
  theme(palette.colour.continuous = c("white", "pink", "hotpink"))

# }
```
