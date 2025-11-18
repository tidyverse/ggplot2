# Dot plot

In a dot plot, the width of a dot corresponds to the bin width (or
maximum width, depending on the binning algorithm), and dots are
stacked, with each dot representing one observation.

## Usage

``` r
geom_dotplot(
  mapping = NULL,
  data = NULL,
  position = "identity",
  ...,
  binwidth = NULL,
  binaxis = "x",
  method = "dotdensity",
  binpositions = "bygroup",
  stackdir = "up",
  stackratio = 1,
  dotsize = 1,
  stackgroups = FALSE,
  origin = NULL,
  right = TRUE,
  width = 0.9,
  drop = FALSE,
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

- binwidth:

  When `method` is "dotdensity", this specifies maximum bin width. When
  `method` is "histodot", this specifies bin width. Defaults to 1/30 of
  the range of the data

- binaxis:

  The axis to bin along, "x" (default) or "y"

- method:

  "dotdensity" (default) for dot-density binning, or "histodot" for
  fixed bin widths (like stat_bin)

- binpositions:

  When `method` is "dotdensity", "bygroup" (default) determines
  positions of the bins for each group separately. "all" determines
  positions of the bins with all the data taken together; this is used
  for aligning dot stacks across multiple groups.

- stackdir:

  which direction to stack the dots. "up" (default), "down", "center",
  "centerwhole" (centered, but with dots aligned)

- stackratio:

  how close to stack the dots. Default is 1, where dots just touch. Use
  smaller values for closer, overlapping dots.

- dotsize:

  The diameter of the dots relative to `binwidth`, default 1.

- stackgroups:

  should dots be stacked across groups? This has the effect that
  `position = "stack"` should have, but can't (because this geom has
  some odd properties).

- origin:

  When `method` is "histodot", origin of first bin

- right:

  When `method` is "histodot", should intervals be closed on the right
  (a, b\], or not \[a, b)

- width:

  When `binaxis` is "y", the spacing of the dot stacks for dodging.

- drop:

  If TRUE, remove all bins with zero counts

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

## Details

There are two basic approaches: *dot-density* and *histodot*. With
dot-density binning, the bin positions are determined by the data and
`binwidth`, which is the maximum width of each bin. See Wilkinson (1999)
for details on the dot-density binning algorithm. With histodot binning,
the bins have fixed positions and fixed widths, much like a histogram.

When binning along the x axis and stacking along the y axis, the numbers
on y axis are not meaningful, due to technical limitations of ggplot2.
You can hide the y axis, as in one of the examples, or manually scale it
to match the number of dots.

## Computed variables

These are calculated by the 'stat' part of layers and can be accessed
with [delayed
evaluation](https://ggplot2.tidyverse.org/dev/reference/aes_eval.md).

- `after_stat(x)`  
  center of each bin, if `binaxis` is `"x"`.

- `after_stat(y)`  
  center of each bin, if `binaxis` is `"x"`.

- `after_stat(binwidth)`  
  maximum width of each bin if method is `"dotdensity"`; width of each
  bin if method is `"histodot"`.

- `after_stat(count)`  
  number of points in bin.

- `after_stat(ncount)`  
  count, scaled to a maximum of 1.

- `after_stat(density)`  
  density of points in bin, scaled to integrate to 1, if method is
  `"histodot"`.

- `after_stat(ndensity)`  
  density, scaled to maximum of 1, if method is `"histodot"`.

## References

Wilkinson, L. (1999) Dot plots. The American Statistician, 53(3),
276-281.

## Aesthetics

`geom_dotplot()` understands the following aesthetics. Required
aesthetics are displayed in bold and defaults are displayed for optional
aesthetics:

|     |                                                                                      |                                                                         |
|-----|--------------------------------------------------------------------------------------|-------------------------------------------------------------------------|
| •   | **[`x`](https://ggplot2.tidyverse.org/dev/reference/aes_position.md)**               |                                                                         |
| •   | **[`y`](https://ggplot2.tidyverse.org/dev/reference/aes_position.md)**               |                                                                         |
| •   | [`alpha`](https://ggplot2.tidyverse.org/dev/reference/aes_colour_fill_alpha.md)      | → `NA`                                                                  |
| •   | [`colour`](https://ggplot2.tidyverse.org/dev/reference/aes_colour_fill_alpha.md)     | → via [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md) |
| •   | [`fill`](https://ggplot2.tidyverse.org/dev/reference/aes_colour_fill_alpha.md)       | → via [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md) |
| •   | [`group`](https://ggplot2.tidyverse.org/dev/reference/aes_group_order.md)            | → inferred                                                              |
| •   | [`linetype`](https://ggplot2.tidyverse.org/dev/reference/aes_linetype_size_shape.md) | → via [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md) |
| •   | `stroke`                                                                             | → via [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md) |
| •   | `weight`                                                                             | → `1`                                                                   |
| •   | `width`                                                                              | → `0.9`                                                                 |

Learn more about setting these aesthetics in
[`vignette("ggplot2-specs")`](https://ggplot2.tidyverse.org/dev/articles/ggplot2-specs.md).

## Examples

``` r
ggplot(mtcars, aes(x = mpg)) +
  geom_dotplot()
#> Bin width defaults to 1/30 of the range of the data. Pick better value
#> with `binwidth`.


ggplot(mtcars, aes(x = mpg)) +
  geom_dotplot(binwidth = 1.5)


# Use fixed-width bins
ggplot(mtcars, aes(x = mpg)) +
  geom_dotplot(method="histodot", binwidth = 1.5)


# Some other stacking methods
ggplot(mtcars, aes(x = mpg)) +
  geom_dotplot(binwidth = 1.5, stackdir = "center")


ggplot(mtcars, aes(x = mpg)) +
  geom_dotplot(binwidth = 1.5, stackdir = "centerwhole")


# y axis isn't really meaningful, so hide it
ggplot(mtcars, aes(x = mpg)) + geom_dotplot(binwidth = 1.5) +
  scale_y_continuous(NULL, breaks = NULL)


# Overlap dots vertically
ggplot(mtcars, aes(x = mpg)) +
  geom_dotplot(binwidth = 1.5, stackratio = .7)


# Expand dot diameter
ggplot(mtcars, aes(x = mpg)) +
  geom_dotplot(binwidth = 1.5, dotsize = 1.25)


# Change dot fill colour, stroke width
ggplot(mtcars, aes(x = mpg)) +
  geom_dotplot(binwidth = 1.5, fill = "white", stroke = 2)


# \donttest{
# Examples with stacking along y axis instead of x
ggplot(mtcars, aes(x = 1, y = mpg)) +
  geom_dotplot(binaxis = "y", stackdir = "center")
#> Bin width defaults to 1/30 of the range of the data. Pick better value
#> with `binwidth`.


ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  geom_dotplot(binaxis = "y", stackdir = "center")
#> Bin width defaults to 1/30 of the range of the data. Pick better value
#> with `binwidth`.


ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  geom_dotplot(binaxis = "y", stackdir = "centerwhole")
#> Bin width defaults to 1/30 of the range of the data. Pick better value
#> with `binwidth`.


ggplot(mtcars, aes(x = factor(vs), fill = factor(cyl), y = mpg)) +
  geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge")
#> Bin width defaults to 1/30 of the range of the data. Pick better value
#> with `binwidth`.


# binpositions="all" ensures that the bins are aligned between groups
ggplot(mtcars, aes(x = factor(am), y = mpg)) +
  geom_dotplot(binaxis = "y", stackdir = "center", binpositions="all")
#> Bin width defaults to 1/30 of the range of the data. Pick better value
#> with `binwidth`.


# Stacking multiple groups, with different fill
ggplot(mtcars, aes(x = mpg, fill = factor(cyl))) +
  geom_dotplot(stackgroups = TRUE, binwidth = 1, binpositions = "all")


ggplot(mtcars, aes(x = mpg, fill = factor(cyl))) +
  geom_dotplot(stackgroups = TRUE, binwidth = 1, method = "histodot")


ggplot(mtcars, aes(x = 1, y = mpg, fill = factor(cyl))) +
  geom_dotplot(binaxis = "y", stackgroups = TRUE, binwidth = 1, method = "histodot")

# }
```
