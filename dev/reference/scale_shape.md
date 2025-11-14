# Scales for shapes, aka glyphs

`scale_shape()` maps discrete variables to six easily discernible
shapes. If you have more than six levels, you will get a warning
message, and the seventh and subsequent levels will not appear on the
plot. Use
[`scale_shape_manual()`](https://ggplot2.tidyverse.org/dev/reference/scale_manual.md)
to supply your own values. You can not map a continuous variable to
shape unless `scale_shape_binned()` is used. Still, as shape has no
inherent order, this use is not advised.

## Usage

``` r
scale_shape(name = waiver(), ..., solid = NULL, aesthetics = "shape")

scale_shape_binned(name = waiver(), ..., solid = TRUE, aesthetics = "shape")
```

## Arguments

- name:

  The name of the scale. Used as the axis or legend title. If
  [`waiver()`](https://ggplot2.tidyverse.org/dev/reference/waiver.md),
  the default, the name of the scale is taken from the first mapping
  used for that aesthetic. If `NULL`, the legend title will be omitted.

- ...:

  Arguments passed on to
  [`discrete_scale`](https://ggplot2.tidyverse.org/dev/reference/discrete_scale.md)

  `breaks`

  :   One of:

      - `NULL` for no breaks

      - [`waiver()`](https://ggplot2.tidyverse.org/dev/reference/waiver.md)
        for the default breaks (the scale limits)

      - A character vector of breaks

      - A function that takes the limits as input and returns breaks as
        output. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation.

  `limits`

  :   One of:

      - `NULL` to use the default scale values

      - A character vector that defines possible values of the scale and
        their order

      - A function that accepts the existing (automatic) values and
        returns new ones. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation.

  `drop`

  :   Should unused factor levels be omitted from the scale? The
      default, `TRUE`, uses the levels that appear in the data; `FALSE`
      includes the levels in the factor. Please note that to display
      every level in a legend, the layer should use
      `show.legend = TRUE`.

  `na.translate`

  :   Unlike continuous scales, discrete scales can easily show missing
      values, and do so by default. If you want to remove missing values
      from a discrete scale, specify `na.translate = FALSE`.

  `na.value`

  :   If `na.translate = TRUE`, what aesthetic value should the missing
      values be displayed as? Does not apply to position scales where
      `NA` is always placed at the far right.

  `minor_breaks`

  :   One of:

      - `NULL` for no minor breaks

      - [`waiver()`](https://ggplot2.tidyverse.org/dev/reference/waiver.md)
        for the default breaks (none for discrete, one minor break
        between each major break for continuous)

      - A numeric vector of positions

      - A function that given the limits returns a vector of minor
        breaks. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation. When the function has two arguments, it will
        be given the limits and major break positions.

  `labels`

  :   One of the options below. Please note that when `labels` is a
      vector, it is highly recommended to also set the `breaks` argument
      as a vector to protect against unintended mismatches.

      - `NULL` for no labels

      - [`waiver()`](https://ggplot2.tidyverse.org/dev/reference/waiver.md)
        for the default labels computed by the transformation object

      - A character vector giving labels (must be same length as
        `breaks`)

      - An expression vector (must be the same length as breaks). See
        ?plotmath for details.

      - A function that takes the breaks as input and returns labels as
        output. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation.

  `guide`

  :   A function used to create a guide or its name. See
      [`guides()`](https://ggplot2.tidyverse.org/dev/reference/guides.md)
      for more information.

  `call`

  :   The `call` used to construct the scale for reporting messages.

  `super`

  :   The super class to use for the constructed scale

- solid:

  Should the shapes be solid, `TRUE`, or hollow, `FALSE`?

- aesthetics:

  The names of the aesthetics that this scale works with.

## Details

Shapes can be referred to by number or name. Shapes in \[0, 20\] do not
support a fill aesthetic, whereas shapes in \[21, 25\] do.

![All shapes by number and name](figures/shape_table.svg)

## See also

The documentation for [differentiation related
aesthetics](https://ggplot2.tidyverse.org/dev/reference/aes_linetype_size_shape.md).

Other shape scales:
[`scale_shape_manual()`](https://ggplot2.tidyverse.org/dev/reference/scale_manual.md),
[`scale_shape_identity()`](https://ggplot2.tidyverse.org/dev/reference/scale_identity.md).

The [shape
section](https://ggplot2-book.org/scales-other#sec-scale-shape) of the
online ggplot2 book.

## Examples

``` r
set.seed(596)
dsmall <- diamonds[sample(nrow(diamonds), 100), ]

(d <- ggplot(dsmall, aes(carat, price)) + geom_point(aes(shape = cut)))
#> Warning: Using shapes for an ordinal variable is not advised

d + scale_shape(solid = TRUE) # the default

d + scale_shape(solid = FALSE)

d + scale_shape(name = "Cut of diamond")


# To change order of levels, change order of
# underlying factor
levels(dsmall$cut) <- c("Fair", "Good", "Very Good", "Premium", "Ideal")

# Need to recreate plot to pick up new data
ggplot(dsmall, aes(price, carat)) + geom_point(aes(shape = cut))
#> Warning: Using shapes for an ordinal variable is not advised


# Show a list of available shapes
df_shapes <- data.frame(shape = 0:24)
ggplot(df_shapes, aes(0, 0, shape = shape)) +
  geom_point(aes(shape = shape), size = 5, fill = 'red') +
  scale_shape_identity() +
  facet_wrap(~shape) +
  theme_void()
```
