# Evenly spaced colours for discrete data

Maps each level to an evenly spaced hue on the colour wheel. It does not
generate colour-blind safe palettes.

## Usage

``` r
scale_colour_hue(
  name = waiver(),
  ...,
  h = c(0, 360) + 15,
  c = 100,
  l = 65,
  h.start = 0,
  direction = 1,
  na.value = "grey50",
  aesthetics = "colour"
)

scale_fill_hue(
  name = waiver(),
  ...,
  h = c(0, 360) + 15,
  c = 100,
  l = 65,
  h.start = 0,
  direction = 1,
  na.value = "grey50",
  aesthetics = "fill"
)
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

- h:

  range of hues to use, in \[0, 360\]

- c:

  chroma (intensity of colour), maximum value varies depending on
  combination of hue and luminance.

- l:

  luminance (lightness), in \[0, 100\]

- h.start:

  hue to start at

- direction:

  direction to travel around the colour wheel, 1 = clockwise, -1 =
  counter-clockwise

- na.value:

  Colour to use for missing values

- aesthetics:

  Character string or vector of character strings listing the name(s) of
  the aesthetic(s) that this scale works with. This can be useful, for
  example, to apply colour settings to the `colour` and `fill`
  aesthetics at the same time, via `aesthetics = c("colour", "fill")`.

## See also

The documentation on [colour
aesthetics](https://ggplot2.tidyverse.org/dev/reference/aes_colour_fill_alpha.md).

The [hue and grey scales
section](https://ggplot2-book.org/scales-colour#hue-and-grey-scales) of
the online ggplot2 book.

Other colour scales:
[`scale_alpha()`](https://ggplot2.tidyverse.org/dev/reference/scale_alpha.md),
[`scale_colour_brewer()`](https://ggplot2.tidyverse.org/dev/reference/scale_brewer.md),
[`scale_colour_continuous()`](https://ggplot2.tidyverse.org/dev/reference/scale_colour_continuous.md),
[`scale_colour_discrete()`](https://ggplot2.tidyverse.org/dev/reference/scale_colour_discrete.md),
[`scale_colour_gradient()`](https://ggplot2.tidyverse.org/dev/reference/scale_gradient.md),
[`scale_colour_grey()`](https://ggplot2.tidyverse.org/dev/reference/scale_grey.md),
[`scale_colour_identity()`](https://ggplot2.tidyverse.org/dev/reference/scale_identity.md),
[`scale_colour_manual()`](https://ggplot2.tidyverse.org/dev/reference/scale_manual.md),
[`scale_colour_steps()`](https://ggplot2.tidyverse.org/dev/reference/scale_steps.md),
[`scale_colour_viridis_d()`](https://ggplot2.tidyverse.org/dev/reference/scale_viridis.md)

## Examples

``` r
# \donttest{
set.seed(596)
dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
(d <- ggplot(dsamp, aes(carat, price)) + geom_point(aes(colour = clarity)))


# Change scale label
d + scale_colour_hue()

d + scale_colour_hue("clarity")

d + scale_colour_hue(expression(clarity[beta]))


# Adjust luminosity and chroma
d + scale_colour_hue(l = 40, c = 30)

d + scale_colour_hue(l = 70, c = 30)

d + scale_colour_hue(l = 70, c = 150)

d + scale_colour_hue(l = 80, c = 150)


# Change range of hues used
d + scale_colour_hue(h = c(0, 90))

d + scale_colour_hue(h = c(90, 180))

d + scale_colour_hue(h = c(180, 270))

d + scale_colour_hue(h = c(270, 360))


# Vary opacity
# (only works with pdf, quartz and cairo devices)
d <- ggplot(dsamp, aes(carat, price, colour = clarity))
d + geom_point(alpha = 0.9)

d + geom_point(alpha = 0.5)

d + geom_point(alpha = 0.2)


# Colour of missing values is controlled with na.value:
miss <- factor(sample(c(NA, 1:5), nrow(mtcars), replace = TRUE))
ggplot(mtcars, aes(mpg, wt)) +
  geom_point(aes(colour = miss))

ggplot(mtcars, aes(mpg, wt)) +
  geom_point(aes(colour = miss)) +
  scale_colour_hue(na.value = "black")

# }
```
