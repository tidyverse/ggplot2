# Discrete scale constructor

Discrete scale constructor

## Usage

``` r
discrete_scale(
  aesthetics,
  scale_name = deprecated(),
  palette,
  name = waiver(),
  breaks = waiver(),
  minor_breaks = waiver(),
  labels = waiver(),
  limits = NULL,
  expand = waiver(),
  na.translate = TRUE,
  na.value = NA,
  drop = TRUE,
  guide = "legend",
  position = "left",
  fallback.palette = NULL,
  call = caller_call(),
  super = ScaleDiscrete
)
```

## Arguments

- aesthetics:

  The names of the aesthetics that this scale works with.

- scale_name:

  **\[deprecated\]** The name of the scale that should be used for error
  messages associated with this scale.

- palette:

  A palette function that when called with a single integer argument
  (the number of levels in the scale) returns the values that they
  should take (e.g.,
  [`scales::pal_hue()`](https://scales.r-lib.org/reference/pal_hue.html)).

- name:

  The name of the scale. Used as the axis or legend title. If
  [`waiver()`](https://ggplot2.tidyverse.org/dev/reference/waiver.md),
  the default, the name of the scale is taken from the first mapping
  used for that aesthetic. If `NULL`, the legend title will be omitted.

- breaks:

  One of:

  - `NULL` for no breaks

  - [`waiver()`](https://ggplot2.tidyverse.org/dev/reference/waiver.md)
    for the default breaks (the scale limits)

  - A character vector of breaks

  - A function that takes the limits as input and returns breaks as
    output. Also accepts rlang
    [lambda](https://rlang.r-lib.org/reference/as_function.html)
    function notation.

- minor_breaks:

  One of:

  - `NULL` for no minor breaks

  - [`waiver()`](https://ggplot2.tidyverse.org/dev/reference/waiver.md)
    for the default breaks (none for discrete, one minor break between
    each major break for continuous)

  - A numeric vector of positions

  - A function that given the limits returns a vector of minor breaks.
    Also accepts rlang
    [lambda](https://rlang.r-lib.org/reference/as_function.html)
    function notation. When the function has two arguments, it will be
    given the limits and major break positions.

- labels:

  One of the options below. Please note that when `labels` is a vector,
  it is highly recommended to also set the `breaks` argument as a vector
  to protect against unintended mismatches.

  - `NULL` for no labels

  - [`waiver()`](https://ggplot2.tidyverse.org/dev/reference/waiver.md)
    for the default labels computed by the transformation object

  - A character vector giving labels (must be same length as `breaks`)

  - An expression vector (must be the same length as breaks). See
    ?plotmath for details.

  - A function that takes the breaks as input and returns labels as
    output. Also accepts rlang
    [lambda](https://rlang.r-lib.org/reference/as_function.html)
    function notation.

- limits:

  One of:

  - `NULL` to use the default scale values

  - A character vector that defines possible values of the scale and
    their order

  - A function that accepts the existing (automatic) values and returns
    new ones. Also accepts rlang
    [lambda](https://rlang.r-lib.org/reference/as_function.html)
    function notation.

- expand:

  For position scales, a vector of range expansion constants used to add
  some padding around the data to ensure that they are placed some
  distance away from the axes. Use the convenience function
  [`expansion()`](https://ggplot2.tidyverse.org/dev/reference/expansion.md)
  to generate the values for the `expand` argument. The defaults are to
  expand the scale by 5% on each side for continuous variables, and by
  0.6 units on each side for discrete variables.

- na.translate:

  Unlike continuous scales, discrete scales can easily show missing
  values, and do so by default. If you want to remove missing values
  from a discrete scale, specify `na.translate = FALSE`.

- na.value:

  If `na.translate = TRUE`, what aesthetic value should the missing
  values be displayed as? Does not apply to position scales where `NA`
  is always placed at the far right.

- drop:

  Should unused factor levels be omitted from the scale? The default,
  `TRUE`, uses the levels that appear in the data; `FALSE` includes the
  levels in the factor. Please note that to display every level in a
  legend, the layer should use `show.legend = TRUE`.

- guide:

  A function used to create a guide or its name. See
  [`guides()`](https://ggplot2.tidyverse.org/dev/reference/guides.md)
  for more information.

- position:

  For position scales, The position of the axis. `left` or `right` for y
  axes, `top` or `bottom` for x axes.

- fallback.palette:

  Function to use when `palette = NULL` and the palette is not
  represented in the theme.

- call:

  The `call` used to construct the scale for reporting messages.

- super:

  The super class to use for the constructed scale

## See also

The [new scales
section](https://ggplot2-book.org/extensions#sec-new-scales) of the
online ggplot2 book.
