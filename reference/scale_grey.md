# Sequential grey colour scales

Based on
[`gray.colors()`](https://rdrr.io/r/grDevices/gray.colors.html). This is
black and white equivalent of
[`scale_colour_gradient()`](https://ggplot2.tidyverse.org/reference/scale_gradient.md).

## Usage

``` r
scale_colour_grey(
  name = waiver(),
  ...,
  start = 0.2,
  end = 0.8,
  na.value = "red",
  aesthetics = "colour"
)

scale_fill_grey(
  name = waiver(),
  ...,
  start = 0.2,
  end = 0.8,
  na.value = "red",
  aesthetics = "fill"
)
```

## Arguments

- name:

  The name of the scale. Used as the axis or legend title. If
  [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.md), the
  default, the name of the scale is taken from the first mapping used
  for that aesthetic. If `NULL`, the legend title will be omitted.

- ...:

  Arguments passed on to
  [`discrete_scale`](https://ggplot2.tidyverse.org/reference/discrete_scale.md)

  `breaks`

  :   One of:

      - `NULL` for no breaks

      - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.md)
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

  `minor_breaks`

  :   One of:

      - `NULL` for no minor breaks

      - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.md)
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

      - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.md)
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
      [`guides()`](https://ggplot2.tidyverse.org/reference/guides.md)
      for more information.

  `call`

  :   The `call` used to construct the scale for reporting messages.

  `super`

  :   The super class to use for the constructed scale

- start:

  grey value at low end of palette

- end:

  grey value at high end of palette

- na.value:

  Colour to use for missing values

- aesthetics:

  Character string or vector of character strings listing the name(s) of
  the aesthetic(s) that this scale works with. This can be useful, for
  example, to apply colour settings to the `colour` and `fill`
  aesthetics at the same time, via `aesthetics = c("colour", "fill")`.

## See also

The documentation on [colour
aesthetics](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.md).

The [hue and grey scales
section](https://ggplot2-book.org/scales-colour#hue-and-grey-scales) of
the online ggplot2 book.

Other colour scales:
[`scale_alpha()`](https://ggplot2.tidyverse.org/reference/scale_alpha.md),
[`scale_colour_brewer()`](https://ggplot2.tidyverse.org/reference/scale_brewer.md),
[`scale_colour_continuous()`](https://ggplot2.tidyverse.org/reference/scale_colour_continuous.md),
[`scale_colour_discrete()`](https://ggplot2.tidyverse.org/reference/scale_colour_discrete.md),
[`scale_colour_gradient()`](https://ggplot2.tidyverse.org/reference/scale_gradient.md),
[`scale_colour_hue()`](https://ggplot2.tidyverse.org/reference/scale_hue.md),
[`scale_colour_identity()`](https://ggplot2.tidyverse.org/reference/scale_identity.md),
[`scale_colour_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.md),
[`scale_colour_steps()`](https://ggplot2.tidyverse.org/reference/scale_steps.md),
[`scale_colour_viridis_d()`](https://ggplot2.tidyverse.org/reference/scale_viridis.md)

## Examples

``` r
p <- ggplot(mtcars, aes(mpg, wt)) + geom_point(aes(colour = factor(cyl)))
p + scale_colour_grey()

p + scale_colour_grey(end = 0)


# You may want to turn off the pale grey background with this scale
p + scale_colour_grey() + theme_bw()


# Colour of missing values is controlled with na.value:
miss <- factor(sample(c(NA, 1:5), nrow(mtcars), replace = TRUE))
ggplot(mtcars, aes(mpg, wt)) +
  geom_point(aes(colour = miss)) +
  scale_colour_grey()

ggplot(mtcars, aes(mpg, wt)) +
  geom_point(aes(colour = miss)) +
  scale_colour_grey(na.value = "green")
```
