# Colour related aesthetics: colour, fill, and alpha

These aesthetics parameters change the colour (`colour` and `fill`) and
the opacity (`alpha`) of geom elements on a plot. Almost every geom has
either colour or fill (or both), as well as can have their alpha
modified. Modifying colour on a plot is a useful way to enhance the
presentation of data, often especially when a plot graphs more than two
variables.

## Colour and fill

The `colour` aesthetic is used to draw lines and strokes, such as in
[`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.md)
and
[`geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.md),
but also the line contours of
[`geom_rect()`](https://ggplot2.tidyverse.org/reference/geom_tile.md)
and
[`geom_polygon()`](https://ggplot2.tidyverse.org/reference/geom_polygon.md).
The `fill` aesthetic is used to colour the inside areas of geoms, such
as [`geom_rect()`](https://ggplot2.tidyverse.org/reference/geom_tile.md)
and
[`geom_polygon()`](https://ggplot2.tidyverse.org/reference/geom_polygon.md),
but also the insides of shapes 21-25 of
[`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.md).

Colours and fills can be specified in the following ways:

- A name, e.g., `"red"`. R has 657 built-in named colours, which can be
  listed with
  [`grDevices::colors()`](https://rdrr.io/r/grDevices/colors.html).

- An rgb specification, with a string of the form `"#RRGGBB"` where each
  of the pairs `RR`, `GG`, `BB` consists of two hexadecimal digits
  giving a value in the range `00` to `FF`. You can optionally make the
  colour transparent by using the form `"#RRGGBBAA"`.

- An `NA`, for a completely transparent colour.

## Alpha

Alpha refers to the opacity of a geom. Values of `alpha` range from 0 to
1, with lower values corresponding to more transparent colors.

Alpha can additionally be modified through the `colour` or `fill`
aesthetic if either aesthetic provides color values using an rgb
specification (`"#RRGGBBAA"`), where `AA` refers to transparency values.

## See also

- Other options for modifying colour:
  [`scale_colour_brewer()`](https://ggplot2.tidyverse.org/reference/scale_brewer.md),
  [`scale_colour_gradient()`](https://ggplot2.tidyverse.org/reference/scale_gradient.md),
  [`scale_colour_grey()`](https://ggplot2.tidyverse.org/reference/scale_grey.md),
  [`scale_colour_hue()`](https://ggplot2.tidyverse.org/reference/scale_hue.md),
  [`scale_colour_identity()`](https://ggplot2.tidyverse.org/reference/scale_identity.md),
  [`scale_colour_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.md),
  [`scale_colour_viridis_d()`](https://ggplot2.tidyverse.org/reference/scale_viridis.md)

- Other options for modifying fill:
  [`scale_fill_brewer()`](https://ggplot2.tidyverse.org/reference/scale_brewer.md),
  [`scale_fill_gradient()`](https://ggplot2.tidyverse.org/reference/scale_gradient.md),
  [`scale_fill_grey()`](https://ggplot2.tidyverse.org/reference/scale_grey.md),
  [`scale_fill_hue()`](https://ggplot2.tidyverse.org/reference/scale_hue.md),
  [`scale_fill_identity()`](https://ggplot2.tidyverse.org/reference/scale_identity.md),
  [`scale_fill_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.md),
  [`scale_fill_viridis_d()`](https://ggplot2.tidyverse.org/reference/scale_viridis.md)

- Other options for modifying alpha:
  [`scale_alpha()`](https://ggplot2.tidyverse.org/reference/scale_alpha.md),
  [`scale_alpha_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.md),
  [`scale_alpha_identity()`](https://ggplot2.tidyverse.org/reference/scale_identity.md)

- Run
  [`vignette("ggplot2-specs")`](https://ggplot2.tidyverse.org/articles/ggplot2-specs.md)
  to see an overview of other aesthetics that can be modified.

Other aesthetics documentation:
[`aes()`](https://ggplot2.tidyverse.org/reference/aes.md),
[`aes_group_order`](https://ggplot2.tidyverse.org/reference/aes_group_order.md),
[`aes_linetype_size_shape`](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.md),
[`aes_position`](https://ggplot2.tidyverse.org/reference/aes_position.md)

## Examples

``` r
# \donttest{

# Bar chart example
p <- ggplot(mtcars, aes(factor(cyl)))
# Default plotting
p + geom_bar()

# To change the interior colouring use fill aesthetic
p + geom_bar(fill = "red")

# Compare with the colour aesthetic which changes just the bar outline
p + geom_bar(colour = "red")

# Combining both, you can see the changes more clearly
p + geom_bar(fill = "white", colour = "red")

# Both colour and fill can take an rgb specification.
p + geom_bar(fill = "#00abff")

# Use NA for a completely transparent colour.
p + geom_bar(fill = NA, colour = "#00abff")


# Colouring scales differ depending on whether a discrete or
# continuous variable is being mapped. For example, when mapping
# fill to a factor variable, a discrete colour scale is used.
ggplot(mtcars, aes(factor(cyl), fill = factor(vs))) + geom_bar()


# When mapping fill to continuous variable a continuous colour
# scale is used.
ggplot(faithfuld, aes(waiting, eruptions)) +
  geom_raster(aes(fill = density))


# Some geoms only use the colour aesthetic but not the fill
# aesthetic (e.g. geom_point() or geom_line()).
p <- ggplot(economics, aes(x = date, y = unemploy))
p + geom_line()

p + geom_line(colour = "green")

p + geom_point()

p + geom_point(colour = "red")


# For large datasets with overplotting the alpha
# aesthetic will make the points more transparent.
set.seed(1)
df <- data.frame(x = rnorm(5000), y = rnorm(5000))
p  <- ggplot(df, aes(x,y))
p + geom_point()

p + geom_point(alpha = 0.5)

p + geom_point(alpha = 1/10)


# Alpha can also be used to add shading.
p <- ggplot(economics, aes(x = date, y = unemploy)) + geom_line()
p

yrng <- range(economics$unemploy)
p <- p +
  geom_rect(
    aes(NULL, NULL, xmin = start, xmax = end, fill = party),
    ymin = yrng[1], ymax = yrng[2], data = presidential
  )
p

p + scale_fill_manual(values = alpha(c("blue", "red"), .3))

# }
```
