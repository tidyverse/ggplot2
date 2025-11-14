# Smoothed conditional means

Aids the eye in seeing patterns in the presence of overplotting.
`geom_smooth()` and `stat_smooth()` are effectively aliases: they both
use the same arguments. Use `stat_smooth()` if you want to display the
results with a non-standard geom.

## Usage

``` r
geom_smooth(
  mapping = NULL,
  data = NULL,
  stat = "smooth",
  position = "identity",
  ...,
  method = NULL,
  formula = NULL,
  se = TRUE,
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_smooth(
  mapping = NULL,
  data = NULL,
  geom = "smooth",
  position = "identity",
  ...,
  orientation = NA,
  method = NULL,
  formula = NULL,
  se = TRUE,
  n = 80,
  span = 0.75,
  fullrange = FALSE,
  xseq = NULL,
  level = 0.95,
  method.args = list(),
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

- method:

  Smoothing method (function) to use, accepts either `NULL` or a
  character vector, e.g. `"lm"`, `"glm"`, `"gam"`, `"loess"` or a
  function, e.g. [`MASS::rlm`](https://rdrr.io/pkg/MASS/man/rlm.html) or
  [`mgcv::gam`](https://rdrr.io/pkg/mgcv/man/gam.html),
  [`stats::lm`](https://rdrr.io/r/stats/lm.html), or
  [`stats::loess`](https://rdrr.io/r/stats/loess.html). `"auto"` is also
  accepted for backwards compatibility. It is equivalent to `NULL`.

  For `method = NULL` the smoothing method is chosen based on the size
  of the largest group (across all panels).
  [`stats::loess()`](https://rdrr.io/r/stats/loess.html) is used for
  less than 1,000 observations; otherwise
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) is used with
  `formula = y ~ s(x, bs = "cs")` with `method = "REML"`. Somewhat
  anecdotally, `loess` gives a better appearance, but is \\O(N^{2})\\ in
  memory, so does not work for larger datasets.

  If you have fewer than 1,000 observations but want to use the same
  `gam()` model that `method = NULL` would use, then set
  `method = "gam", formula = y ~ s(x, bs = "cs")`.

- formula:

  Formula to use in smoothing function, eg. `y ~ x`, `y ~ poly(x, 2)`,
  `y ~ log(x)`. `NULL` by default, in which case `method = NULL` implies
  `formula = y ~ x` when there are fewer than 1,000 observations and
  `formula = y ~ s(x, bs = "cs")` otherwise.

- se:

  Display confidence band around smooth? (`TRUE` by default, see `level`
  to control.)

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- orientation:

  The orientation of the layer. The default (`NA`) automatically
  determines the orientation from the aesthetic mapping. In the rare
  event that this fails it can be given explicitly by setting
  `orientation` to either `"x"` or `"y"`. See the *Orientation* section
  for more detail.

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

  Use to override the default connection between `geom_smooth()` and
  `stat_smooth()`. For more information about overriding these
  connections, see how the
  [stat](https://ggplot2.tidyverse.org/dev/reference/layer_stats.md) and
  [geom](https://ggplot2.tidyverse.org/dev/reference/layer_geoms.md)
  arguments work.

- n:

  Number of points at which to evaluate smoother.

- span:

  Controls the amount of smoothing for the default loess smoother.
  Smaller numbers produce wigglier lines, larger numbers produce
  smoother lines. Only used with loess, i.e. when `method = "loess"`, or
  when `method = NULL` (the default) and there are fewer than 1,000
  observations.

- fullrange:

  If `TRUE`, the smoothing line gets expanded to the range of the plot,
  potentially beyond the data. This does not extend the line into any
  additional padding created by `expansion`.

- xseq:

  A numeric vector of values at which the smoother is evaluated. When
  `NULL` (default), `xseq` is internally evaluated as a sequence of `n`
  equally spaced points for continuous data.

- level:

  Level of confidence band to use (0.95 by default).

- method.args:

  List of additional arguments passed on to the modelling function
  defined by `method`.

## Details

Calculation is performed by the (currently undocumented) `predictdf()`
generic and its methods. For most methods the standard error bounds are
computed using the [`predict()`](https://rdrr.io/r/stats/predict.html)
method – the exceptions are
[`loess()`](https://rdrr.io/r/stats/loess.html), which uses a t-based
approximation, and [`glm()`](https://rdrr.io/r/stats/glm.html), where
the normal confidence band is constructed on the link scale and then
back-transformed to the response scale.

## Orientation

This geom treats each axis differently and, thus, can thus have two
orientations. Often the orientation is easy to deduce from a combination
of the given mappings and the types of positional scales in use. Thus,
ggplot2 will by default try to guess which orientation the layer should
have. Under rare circumstances, the orientation is ambiguous and
guessing may fail. In that case the orientation can be specified
directly using the `orientation` parameter, which can be either `"x"` or
`"y"`. The value gives the axis that the geom should run along, `"x"`
being the default orientation you would expect for the geom.

## Computed variables

These are calculated by the 'stat' part of layers and can be accessed
with [delayed
evaluation](https://ggplot2.tidyverse.org/dev/reference/aes_eval.md).
`stat_smooth()` provides the following variables, some of which depend
on the orientation:

- `after_stat(y)` *or* `after_stat(x)`  
  Predicted value.

- `after_stat(ymin)` *or* `after_stat(xmin)`  
  Lower pointwise confidence band around the mean.

- `after_stat(ymax)` *or* `after_stat(xmax)`  
  Upper pointwise confidence band around the mean.

- `after_stat(se)`  
  Standard error.

## See also

See individual modelling functions for more details:
[`lm()`](https://rdrr.io/r/stats/lm.html) for linear smooths,
[`glm()`](https://rdrr.io/r/stats/glm.html) for generalised linear
smooths, and [`loess()`](https://rdrr.io/r/stats/loess.html) for local
smooths.

## Aesthetics

`geom_smooth()` understands the following aesthetics. Required
aesthetics are displayed in bold and defaults are displayed for optional
aesthetics:

|     |                                                                                       |                                                                         |
|-----|---------------------------------------------------------------------------------------|-------------------------------------------------------------------------|
| •   | **[`x`](https://ggplot2.tidyverse.org/dev/reference/aes_position.md)**                |                                                                         |
| •   | **[`y`](https://ggplot2.tidyverse.org/dev/reference/aes_position.md)**                |                                                                         |
| •   | [`alpha`](https://ggplot2.tidyverse.org/dev/reference/aes_colour_fill_alpha.md)       | → `0.4`                                                                 |
| •   | [`colour`](https://ggplot2.tidyverse.org/dev/reference/aes_colour_fill_alpha.md)      | → via [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md) |
| •   | [`fill`](https://ggplot2.tidyverse.org/dev/reference/aes_colour_fill_alpha.md)        | → via [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md) |
| •   | [`group`](https://ggplot2.tidyverse.org/dev/reference/aes_group_order.md)             | → inferred                                                              |
| •   | [`linetype`](https://ggplot2.tidyverse.org/dev/reference/aes_linetype_size_shape.md)  | → via [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md) |
| •   | [`linewidth`](https://ggplot2.tidyverse.org/dev/reference/aes_linetype_size_shape.md) | → via [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md) |
| •   | `weight`                                                                              | → `1`                                                                   |
| •   | [`ymax`](https://ggplot2.tidyverse.org/dev/reference/aes_position.md)                 |                                                                         |
| •   | [`ymin`](https://ggplot2.tidyverse.org/dev/reference/aes_position.md)                 |                                                                         |

Learn more about setting these aesthetics in
[`vignette("ggplot2-specs")`](https://ggplot2.tidyverse.org/dev/articles/ggplot2-specs.md).

## Examples

``` r
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth()
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'


# If you need the fitting to be done along the y-axis set the orientation
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(orientation = "y")
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'


# Use span to control the "wiggliness" of the default loess smoother.
# The span is the fraction of points used to fit each local regression:
# small numbers make a wigglier curve, larger numbers make a smoother curve.
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(span = 0.3)
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'


# Instead of a loess smooth, you can use any other modelling function:
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)
#> `geom_smooth()` using formula = 'y ~ x'


ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = FALSE)


# Smooths are automatically fit to each group (defined by categorical
# aesthetics or the group aesthetic) and for each facet.

ggplot(mpg, aes(displ, hwy, colour = class)) +
  geom_point() +
  geom_smooth(se = FALSE, method = lm)
#> `geom_smooth()` using formula = 'y ~ x'

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(span = 0.8) +
  facet_wrap(~drv)
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'


# \donttest{
binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}
# To fit a logistic regression, you need to coerce the values to
# a numeric vector lying between 0 and 1.
ggplot(rpart::kyphosis, aes(Age, Kyphosis)) +
  geom_jitter(height = 0.05) +
  binomial_smooth()
#> `geom_smooth()` using formula = 'y ~ x'
#> Warning: Failed to fit group 2.
#> Caused by error:
#> ! y values must be 0 <= y <= 1


ggplot(rpart::kyphosis, aes(Age, as.numeric(Kyphosis) - 1)) +
  geom_jitter(height = 0.05) +
  binomial_smooth()
#> `geom_smooth()` using formula = 'y ~ x'


ggplot(rpart::kyphosis, aes(Age, as.numeric(Kyphosis) - 1)) +
  geom_jitter(height = 0.05) +
  binomial_smooth(formula = y ~ splines::ns(x, 2))


# But in this case, it's probably better to fit the model yourself
# so you can exercise more control and see whether or not it's a good model.
# }
```
