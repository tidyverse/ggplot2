# Contours of a 2D density estimate

Perform a 2D kernel density estimation using
[`MASS::kde2d()`](https://rdrr.io/pkg/MASS/man/kde2d.html) and display
the results with contours. This can be useful for dealing with
overplotting. This is a 2D version of
[`geom_density()`](https://ggplot2.tidyverse.org/dev/reference/geom_density.md).
`geom_density_2d()` draws contour lines, and `geom_density_2d_filled()`
draws filled contour bands.

## Usage

``` r
geom_density_2d(
  mapping = NULL,
  data = NULL,
  stat = "density_2d",
  position = "identity",
  ...,
  contour_var = "density",
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_density_2d_filled(
  mapping = NULL,
  data = NULL,
  stat = "density_2d_filled",
  position = "identity",
  ...,
  contour_var = "density",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_density_2d(
  mapping = NULL,
  data = NULL,
  geom = "density_2d",
  position = "identity",
  ...,
  contour = TRUE,
  contour_var = "density",
  h = NULL,
  adjust = c(1, 1),
  n = 100,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_density_2d_filled(
  mapping = NULL,
  data = NULL,
  geom = "density_2d_filled",
  position = "identity",
  ...,
  contour = TRUE,
  contour_var = "density",
  h = NULL,
  adjust = c(1, 1),
  n = 100,
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

  Arguments passed on to
  [`geom_contour`](https://ggplot2.tidyverse.org/dev/reference/geom_contour.md)

  `binwidth`

  :   The width of the contour bins. Overridden by `bins`.

  `bins`

  :   Number of contour bins. Overridden by `breaks`.

  `breaks`

  :   One of:

      - Numeric vector to set the contour breaks

      - A function that takes the range of the data and binwidth as
        input and returns breaks as output. A function can be created
        from a formula (e.g. ~ fullseq(.x, .y)).

      Overrides `binwidth` and `bins`. By default, this is a vector of
      length ten with [`pretty()`](https://rdrr.io/r/base/pretty.html)
      breaks.

- contour_var:

  Character string identifying the variable to contour by. Can be one of
  `"density"`, `"ndensity"`, or `"count"`. See the section on computed
  variables for details.

- lineend:

  Line end style (round, butt, square).

- linejoin:

  Line join style (round, mitre, bevel).

- linemitre:

  Line mitre limit (number greater than 1).

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

- geom, stat:

  Use to override the default connection between `geom_density_2d()` and
  `stat_density_2d()`. For more information at overriding these
  connections, see how the
  [stat](https://ggplot2.tidyverse.org/dev/reference/layer_stats.md) and
  [geom](https://ggplot2.tidyverse.org/dev/reference/layer_geoms.md)
  arguments work.

- contour:

  If `TRUE`, contour the results of the 2d density estimation.

- h:

  Bandwidth (vector of length two). If `NULL`, estimated using
  [`MASS::bandwidth.nrd()`](https://rdrr.io/pkg/MASS/man/bandwidth.nrd.html).

- adjust:

  A multiplicative bandwidth adjustment to be used if 'h' is 'NULL'.
  This makes it possible to adjust the bandwidth while still using the a
  bandwidth estimator. For example, `adjust = 1/2` means use half of the
  default bandwidth.

- n:

  Number of grid points in each direction.

## Computed variables

These are calculated by the 'stat' part of layers and can be accessed
with [delayed
evaluation](https://ggplot2.tidyverse.org/dev/reference/aes_eval.md).
`stat_density_2d()` and `stat_density_2d_filled()` compute different
variables depending on whether contouring is turned on or off. With
contouring off (`contour = FALSE`), both stats behave the same, and the
following variables are provided:

- `after_stat(density)`  
  The density estimate.

- `after_stat(ndensity)`  
  Density estimate, scaled to a maximum of 1.

- `after_stat(count)`  
  Density estimate \* number of observations in group.

- `after_stat(n)`  
  Number of observations in each group.

With contouring on (`contour = TRUE`), either
[`stat_contour()`](https://ggplot2.tidyverse.org/dev/reference/geom_contour.md)
or
[`stat_contour_filled()`](https://ggplot2.tidyverse.org/dev/reference/geom_contour.md)
(for contour lines or contour bands, respectively) is run after the
density estimate has been obtained, and the computed variables are
determined by these stats. Contours are calculated for one of the three
types of density estimates obtained before contouring, `density`,
`ndensity`, and `count`. Which of those should be used is determined by
the `contour_var` parameter.

## Dropped variables

- `z`:

  After density estimation, the z values of individual data points are
  no longer available.

If contouring is enabled, then similarly `density`, `ndensity`, and
`count` are no longer available after the contouring pass.

## See also

[`geom_contour()`](https://ggplot2.tidyverse.org/dev/reference/geom_contour.md),
[`geom_contour_filled()`](https://ggplot2.tidyverse.org/dev/reference/geom_contour.md)
for information about how contours are drawn;
[`geom_bin_2d()`](https://ggplot2.tidyverse.org/dev/reference/geom_bin_2d.md)
for another way of dealing with overplotting.

## Aesthetics

`geom_density2d()` understands the following aesthetics. Required
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

`geom_density2d_filled()` understands the following aesthetics. Required
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

Learn more about setting these aesthetics in
[`vignette("ggplot2-specs")`](https://ggplot2.tidyverse.org/dev/articles/ggplot2-specs.md).

## Examples

``` r
m <- ggplot(faithful, aes(x = eruptions, y = waiting)) +
 geom_point() +
 xlim(0.5, 6) +
 ylim(40, 110)

# contour lines
m + geom_density_2d()


# \donttest{
# contour bands
m + geom_density_2d_filled(alpha = 0.5)


# contour bands and contour lines
m + geom_density_2d_filled(alpha = 0.5) +
  geom_density_2d(linewidth = 0.25, colour = "black")


set.seed(4393)
dsmall <- diamonds[sample(nrow(diamonds), 1000), ]
d <- ggplot(dsmall, aes(x, y))
# If you map an aesthetic to a categorical variable, you will get a
# set of contours for each value of that variable
d + geom_density_2d(aes(colour = cut))


# If you draw filled contours across multiple facets, the same bins are
# used across all facets
d + geom_density_2d_filled() + facet_wrap(vars(cut))

# If you want to make sure the peak intensity is the same in each facet,
# use `contour_var = "ndensity"`.
d + geom_density_2d_filled(contour_var = "ndensity") + facet_wrap(vars(cut))

# If you want to scale intensity by the number of observations in each group,
# use `contour_var = "count"`.
d + geom_density_2d_filled(contour_var = "count") + facet_wrap(vars(cut))


# If we turn contouring off, we can use other geoms, such as tiles:
d + stat_density_2d(
  geom = "raster",
  aes(fill = after_stat(density)),
  contour = FALSE
) + scale_fill_viridis_c()

# Or points:
d + stat_density_2d(geom = "point", aes(size = after_stat(density)), n = 20, contour = FALSE)

# }
```
