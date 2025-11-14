# A quantile-quantile plot

`geom_qq()` and `stat_qq()` produce quantile-quantile plots.
`geom_qq_line()` and `stat_qq_line()` compute the slope and intercept of
the line connecting the points at specified quartiles of the theoretical
and sample distributions.

## Usage

``` r
geom_qq_line(
  mapping = NULL,
  data = NULL,
  geom = "abline",
  position = "identity",
  ...,
  distribution = stats::qnorm,
  dparams = list(),
  line.p = c(0.25, 0.75),
  fullrange = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_qq_line(
  mapping = NULL,
  data = NULL,
  geom = "abline",
  position = "identity",
  ...,
  distribution = stats::qnorm,
  dparams = list(),
  line.p = c(0.25, 0.75),
  fullrange = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_qq(
  mapping = NULL,
  data = NULL,
  geom = "point",
  position = "identity",
  ...,
  distribution = stats::qnorm,
  dparams = list(),
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_qq(
  mapping = NULL,
  data = NULL,
  geom = "point",
  position = "identity",
  ...,
  distribution = stats::qnorm,
  dparams = list(),
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

- geom:

  The geometric object to use to display the data for this layer. When
  using a `stat_*()` function to construct a layer, the `geom` argument
  can be used to override the default coupling between stats and geoms.
  The `geom` argument accepts the following:

  - A `Geom` ggproto subclass, for example `GeomPoint`.

  - A string naming the geom. To give the geom as a string, strip the
    function name of the `geom_` prefix. For example, to use
    [`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.md),
    give the geom as `"point"`.

  - For more information and other ways to specify the geom, see the
    [layer geom](https://ggplot2.tidyverse.org/reference/layer_geoms.md)
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

- distribution:

  Distribution function to use, if x not specified

- dparams:

  Additional parameters passed on to `distribution` function.

- line.p:

  Vector of quantiles to use when fitting the Q-Q line, defaults
  defaults to `c(.25, .75)`.

- fullrange:

  Should the q-q line span the full range of the plot, or just the data

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

## Computed variables

These are calculated by the 'stat' part of layers and can be accessed
with [delayed
evaluation](https://ggplot2.tidyverse.org/reference/aes_eval.md).  
Variables computed by `stat_qq()`:

- `after_stat(sample)`  
  Sample quantiles.

- `after_stat(theoretical)`  
  Theoretical quantiles.

Variables computed by `stat_qq_line()`:

- `after_stat(x)`  
  x-coordinates of the endpoints of the line segment connecting the
  points at the chosen quantiles of the theoretical and the sample
  distributions.

- `after_stat(y)`  
  y-coordinates of the endpoints.

- `after_stat(slope)`  
  Amount of change in `y` across 1 unit of `x`.

- `after_stat(intercept)`  
  Value of `y` at `x == 0`.

## Aesthetics

`stat_qq()` understands the following aesthetics. Required aesthetics
are displayed in bold and defaults are displayed for optional
aesthetics:

|     |                                                                       |                             |
|-----|-----------------------------------------------------------------------|-----------------------------|
| •   | **`sample`**                                                          |                             |
| •   | [`group`](https://ggplot2.tidyverse.org/reference/aes_group_order.md) | → inferred                  |
| •   | [`x`](https://ggplot2.tidyverse.org/reference/aes_position.md)        | → `after_stat(theoretical)` |
| •   | [`y`](https://ggplot2.tidyverse.org/reference/aes_position.md)        | → `after_stat(sample)`      |

`stat_qq_line()` understands the following aesthetics. Required
aesthetics are displayed in bold and defaults are displayed for optional
aesthetics:

|     |                                                                       |                   |
|-----|-----------------------------------------------------------------------|-------------------|
| •   | **`sample`**                                                          |                   |
| •   | [`group`](https://ggplot2.tidyverse.org/reference/aes_group_order.md) | → inferred        |
| •   | [`x`](https://ggplot2.tidyverse.org/reference/aes_position.md)        | → `after_stat(x)` |
| •   | [`y`](https://ggplot2.tidyverse.org/reference/aes_position.md)        | → `after_stat(y)` |

Learn more about setting these aesthetics in
[`vignette("ggplot2-specs")`](https://ggplot2.tidyverse.org/articles/ggplot2-specs.md).

## Examples

``` r
# \donttest{
df <- data.frame(y = rt(200, df = 5))
p <- ggplot(df, aes(sample = y))
p + stat_qq() + stat_qq_line()


# Use fitdistr from MASS to estimate distribution params:
# if (requireNamespace("MASS", quietly = TRUE)) {
#   params <- as.list(MASS::fitdistr(df$y, "t")$estimate)
# }
# Here, we use pre-computed params
params <- list(m = -0.02505057194115, s = 1.122568610124, df = 6.63842653897)
ggplot(df, aes(sample = y)) +
  stat_qq(distribution = qt, dparams = params["df"]) +
  stat_qq_line(distribution = qt, dparams = params["df"])


# Using to explore the distribution of a variable
ggplot(mtcars, aes(sample = mpg)) +
  stat_qq() +
  stat_qq_line()

ggplot(mtcars, aes(sample = mpg, colour = factor(cyl))) +
  stat_qq() +
  stat_qq_line()

# }
```
