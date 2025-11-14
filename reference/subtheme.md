# Shortcuts for theme settings

This collection of functions serves as a shortcut for
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.md) with
shorter argument names. Besides the shorter arguments, it also helps in
keeping theme declarations more organised.

## Usage

``` r
theme_sub_axis(..., title, text, ticks, ticks.length, line, minor.ticks.length)

theme_sub_axis_x(
  ...,
  title,
  text,
  ticks,
  ticks.length,
  line,
  minor.ticks.length
)

theme_sub_axis_y(
  ...,
  title,
  text,
  ticks,
  ticks.length,
  line,
  minor.ticks.length
)

theme_sub_axis_bottom(
  ...,
  title,
  text,
  ticks,
  ticks.length,
  line,
  minor.ticks,
  minor.ticks.length
)

theme_sub_axis_top(
  ...,
  title,
  text,
  ticks,
  ticks.length,
  line,
  minor.ticks,
  minor.ticks.length
)

theme_sub_axis_left(
  ...,
  title,
  text,
  ticks,
  ticks.length,
  line,
  minor.ticks,
  minor.ticks.length
)

theme_sub_axis_right(
  ...,
  title,
  text,
  ticks,
  ticks.length,
  line,
  minor.ticks,
  minor.ticks.length
)

theme_sub_legend(
  ...,
  text,
  text.position,
  title,
  title.position,
  background,
  frame,
  ticks,
  ticks.length,
  axis.line,
  spacing,
  spacing.x,
  spacing.y,
  margin,
  key,
  key.size,
  key.height,
  key.width,
  key.spacing,
  key.spacing.x,
  key.spacing.y,
  key.justification,
  byrow,
  position,
  direction,
  location,
  position.inside,
  justification,
  justification.top,
  justification.bottom,
  justification.left,
  justification.right,
  justification.inside,
  box,
  box.just,
  box.margin,
  box.background,
  box.spacing
)

theme_sub_panel(
  ...,
  background,
  border,
  widths,
  heights,
  spacing,
  spacing.x,
  spacing.y,
  grid,
  grid.major,
  grid.minor,
  grid.major.x,
  grid.major.y,
  grid.minor.x,
  grid.minor.y,
  ontop
)

theme_sub_plot(
  ...,
  background,
  title,
  title.position,
  subtitle,
  caption,
  caption.position,
  tag,
  tag.position,
  tag.location,
  margin
)

theme_sub_strip(
  ...,
  background,
  background.x,
  background.y,
  clip,
  placement,
  text,
  text.x,
  text.x.bottom,
  text.x.top,
  text.y,
  text.y.left,
  text.y.right,
  switch.pad.grid,
  switch.pad.wrap
)
```

## Arguments

- ...:

  Not in use, expected to be empty.

- axis.line, background, background.x, background.y, border, box,
  box.background, box.just, box.margin, box.spacing, byrow, caption,
  caption.position, clip, direction, frame, grid, grid.major,
  grid.major.x, grid.major.y, grid.minor, grid.minor.x, grid.minor.y,
  heights, justification, justification.bottom, justification.inside,
  justification.left, justification.right, justification.top, key,
  key.height, key.justification, key.size, key.spacing, key.spacing.x,
  key.spacing.y, key.width, line, location, margin, minor.ticks,
  minor.ticks.length, ontop, placement, position, position.inside,
  spacing, spacing.x, spacing.y, subtitle, switch.pad.grid,
  switch.pad.wrap, tag, tag.location, tag.position, text, text.position,
  text.x, text.x.bottom, text.x.top, text.y, text.y.left, text.y.right,
  ticks, ticks.length, title, title.position, widths:

  Arguments that are renamed and passed on to
  [`theme()`](https://ggplot2.tidyverse.org/reference/theme.md).

## Value

A `theme`-class object that can be added to a plot.

## Functions

- `theme_sub_axis()`: Theme specification for all axes.

- `theme_sub_axis_x()`: Theme specification for both x axes.

- `theme_sub_axis_y()`: Theme specification for both y axes.

- `theme_sub_axis_bottom()`: Theme specification for the bottom x axis.

- `theme_sub_axis_top()`: Theme specification for the top x axis.

- `theme_sub_axis_left()`: Theme specification for the left y axis.

- `theme_sub_axis_right()`: Theme specification for the right y axis.

- `theme_sub_legend()`: Theme specification for the legend.

- `theme_sub_panel()`: Theme specification for the panels.

- `theme_sub_plot()`: Theme specification for the whole plot.

- `theme_sub_strip()`: Theme specification for facet strips.

## Examples

``` r
# A standard plot
p <- ggplot(mtcars, aes(disp, mpg, colour = drat)) +
  geom_point()

red_text <- element_text(colour = "red")
red_line <- element_line(colour = "red")

# The theme settings below:
p + theme(
  axis.title.x.bottom = red_text,
  axis.text.x.bottom  = red_text,
  axis.line.x.bottom  = red_line,
  axis.ticks.x.bottom = red_line
)


# Are equivalent to these less verbose theme settings
p + theme_sub_axis_bottom(
  title = red_text,
  text  = red_text,
  line  = red_line,
  ticks = red_line
)
```
