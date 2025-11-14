# Discrete colour scales

The default discrete colour scale.

## Usage

``` r
scale_colour_discrete(
  ...,
  palette = NULL,
  aesthetics = "colour",
  na.value = "grey50",
  type = getOption("ggplot2.discrete.colour")
)

scale_fill_discrete(
  ...,
  palette = NULL,
  aesthetics = "fill",
  na.value = "grey50",
  type = getOption("ggplot2.discrete.fill")
)
```

## Arguments

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

  `name`

  :   The name of the scale. Used as the axis or legend title. If
      [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.md),
      the default, the name of the scale is taken from the first mapping
      used for that aesthetic. If `NULL`, the legend title will be
      omitted.

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

- palette:

  One of the following:

  - `NULL` for the default palette stored in the theme.

  - a character vector of colours.

  - a single string naming a palette.

  - a palette function that when called with a single integer argument
    (the number of levels in the scale) returns the values that they
    should take.

- aesthetics:

  The names of the aesthetics that this scale works with.

- na.value:

  If `na.translate = TRUE`, what aesthetic value should the missing
  values be displayed as? Does not apply to position scales where `NA`
  is always placed at the far right.

- type:

  **\[superseded\]** The preferred mechanism for setting the default
  palette is by using the theme. For example:
  `theme(palette.colour.discrete = "Okabe-Ito")`.

## See also

[`discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.md)

The [discrete colour scales
section](https://ggplot2-book.org/scales-colour#sec-colour-discrete) of
the online ggplot2 book.

Other colour scales:
[`scale_alpha()`](https://ggplot2.tidyverse.org/reference/scale_alpha.md),
[`scale_colour_brewer()`](https://ggplot2.tidyverse.org/reference/scale_brewer.md),
[`scale_colour_continuous()`](https://ggplot2.tidyverse.org/reference/scale_colour_continuous.md),
[`scale_colour_gradient()`](https://ggplot2.tidyverse.org/reference/scale_gradient.md),
[`scale_colour_grey()`](https://ggplot2.tidyverse.org/reference/scale_grey.md),
[`scale_colour_hue()`](https://ggplot2.tidyverse.org/reference/scale_hue.md),
[`scale_colour_identity()`](https://ggplot2.tidyverse.org/reference/scale_identity.md),
[`scale_colour_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.md),
[`scale_colour_steps()`](https://ggplot2.tidyverse.org/reference/scale_steps.md),
[`scale_colour_viridis_d()`](https://ggplot2.tidyverse.org/reference/scale_viridis.md)

## Examples

``` r
# A standard plot
p <- ggplot(mpg, aes(displ, hwy, colour = class)) +
  geom_point()

# You can use the scale to give a palette directly
p + scale_colour_discrete(palette = scales::pal_brewer(palette = "Dark2"))


# The default colours are encoded into the theme
p + theme(palette.colour.discrete = scales::pal_grey())


# You can globally set default colour palette via the theme
old <- update_theme(palette.colour.discrete = scales::pal_viridis())

# Plot now shows new global default
p


# Restoring the previous theme
theme_set(old)
```
