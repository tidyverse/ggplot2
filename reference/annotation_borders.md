# Create a layer of map borders

This is a quick and dirty way to get map data (from the maps package)
onto your plot. This is a good place to start if you need some crude
reference lines, but you'll typically want something more sophisticated
for communication graphics.

## Usage

``` r
annotation_borders(
  database = "world",
  regions = ".",
  fill = NA,
  colour = "grey50",
  xlim = NULL,
  ylim = NULL,
  ...
)

borders(...) # Deprecated
```

## Arguments

- database:

  map data, see [`maps::map()`](https://rdrr.io/pkg/maps/man/map.html)
  for details

- regions:

  map region

- fill:

  fill colour

- colour:

  border colour

- xlim, ylim:

  latitudinal and longitudinal ranges for extracting map polygons, see
  [`maps::map()`](https://rdrr.io/pkg/maps/man/map.html) for details.

- ...:

  Arguments passed on to
  [`geom_polygon`](https://ggplot2.tidyverse.org/reference/geom_polygon.md)

  `lineend`

  :   Line end style (round, butt, square).

  `linejoin`

  :   Line join style (round, mitre, bevel).

  `linemitre`

  :   Line mitre limit (number greater than 1).

  `rule`

  :   Either `"evenodd"` or `"winding"`. If polygons with holes are
      being drawn (using the `subgroup` aesthetic) this argument defines
      how the hole coordinates are interpreted. See the examples in
      [`grid::pathGrob()`](https://rdrr.io/r/grid/grid.path.html) for an
      explanation.

  `mapping`

  :   Set of aesthetic mappings created by
      [`aes()`](https://ggplot2.tidyverse.org/reference/aes.md). If
      specified and `inherit.aes = TRUE` (the default), it is combined
      with the default mapping at the top level of the plot. You must
      supply `mapping` if there is no plot mapping.

  `data`

  :   The data to be displayed in this layer. There are three options:

      If `NULL`, the default, the data is inherited from the plot data
      as specified in the call to
      [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.md).

      A `data.frame`, or other object, will override the plot data. All
      objects will be fortified to produce a data frame. See
      [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.md)
      for which variables will be created.

      A `function` will be called with a single argument, the plot data.
      The return value must be a `data.frame`, and will be used as the
      layer data. A `function` can be created from a `formula` (e.g.
      `~ head(.x, 10)`).

  `stat`

  :   The statistical transformation to use on the data for this layer.
      When using a `geom_*()` function to construct a layer, the `stat`
      argument can be used to override the default coupling between
      geoms and stats. The `stat` argument accepts the following:

      - A `Stat` ggproto subclass, for example `StatCount`.

      - A string naming the stat. To give the stat as a string, strip
        the function name of the `stat_` prefix. For example, to use
        [`stat_count()`](https://ggplot2.tidyverse.org/reference/geom_bar.md),
        give the stat as `"count"`.

      - For more information and other ways to specify the stat, see the
        [layer
        stat](https://ggplot2.tidyverse.org/reference/layer_stats.md)
        documentation.

  `position`

  :   A position adjustment to use on the data for this layer. This can
      be used in various ways, including to prevent overplotting and
      improving the display. The `position` argument accepts the
      following:

      - The result of calling a position function, such as
        [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.md).
        This method allows for passing extra arguments to the position.

      - A string naming the position adjustment. To give the position as
        a string, strip the function name of the `position_` prefix. For
        example, to use
        [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.md),
        give the position as `"jitter"`.

      - For more information and other ways to specify the position, see
        the [layer
        position](https://ggplot2.tidyverse.org/reference/layer_positions.md)
        documentation.

  `show.legend`

  :   logical. Should this layer be included in the legends? `NA`, the
      default, includes if any aesthetics are mapped. `FALSE` never
      includes, and `TRUE` always includes. It can also be a named
      logical vector to finely select the aesthetics to display. To
      include legend keys for all levels, even when no data exists, use
      `TRUE`. If `NA`, all levels are shown in legend, but unobserved
      levels are omitted.

  `inherit.aes`

  :   If `FALSE`, overrides the default aesthetics, rather than
      combining with them. This is most useful for helper functions that
      define both data and aesthetics and shouldn't inherit behaviour
      from the default plot specification, e.g. `annotation_borders()`.

  `na.rm`

  :   If `FALSE`, the default, missing values are removed with a
      warning. If `TRUE`, missing values are silently removed.

## Examples

``` r
if (require("maps")) {
data(us.cities)
capitals <- subset(us.cities, capital == 2)
ggplot(capitals, aes(long, lat)) +
  annotation_borders("state") +
  geom_point(aes(size = pop)) +
  scale_size_area() +
  coord_quickmap()
}
#> Loading required package: maps


if (require("maps")) {
# Same map, with some world context
ggplot(capitals, aes(long, lat)) +
  annotation_borders("world", xlim = c(-130, -60), ylim = c(20, 50)) +
  geom_point(aes(size = pop)) +
  scale_size_area() +
  coord_quickmap()
}
```
