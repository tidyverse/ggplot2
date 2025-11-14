# Key glyphs for legends

Each geom has an associated function that draws the key when the geom
needs to be displayed in a legend. These functions are called
`draw_key_*()`, where `*` stands for the name of the respective key
glyph. The key glyphs can be customized for individual geoms by
providing a geom with the `key_glyph` argument (see
[`layer()`](https://ggplot2.tidyverse.org/dev/reference/layer.md) or
examples below.)

## Usage

``` r
draw_key_point(data, params, size)

draw_key_abline(data, params, size)

draw_key_rect(data, params, size)

draw_key_polygon(data, params, size)

draw_key_blank(data, params, size)

draw_key_boxplot(data, params, size)

draw_key_crossbar(data, params, size)

draw_key_path(data, params, size)

draw_key_vpath(data, params, size)

draw_key_dotplot(data, params, size)

draw_key_linerange(data, params, size)

draw_key_pointrange(data, params, size)

draw_key_smooth(data, params, size)

draw_key_text(data, params, size)

draw_key_label(data, params, size)

draw_key_vline(data, params, size)

draw_key_timeseries(data, params, size)
```

## Arguments

- data:

  A single row data frame containing the scaled aesthetics to display in
  this key

- params:

  A list of additional parameters supplied to the geom.

- size:

  Width and height of key in mm.

## Value

A grid grob.

## Examples

``` r
p <- ggplot(economics, aes(date, psavert, color = "savings rate"))
# key glyphs can be specified by their name
p + geom_line(key_glyph = "timeseries")


# key glyphs can be specified via their drawing function
p + geom_line(key_glyph = draw_key_rect)
```
