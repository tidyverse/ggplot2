# Gradient colour scales

`scale_*_gradient` creates a two colour gradient (low-high),
`scale_*_gradient2` creates a diverging colour gradient (low-mid-high),
`scale_*_gradientn` creates a n-colour gradient. For binned variants of
these scales, see the [color
steps](https://ggplot2.tidyverse.org/dev/reference/scale_steps.md)
scales.

## Usage

``` r
scale_colour_gradient(
  name = waiver(),
  ...,
  low = "#132B43",
  high = "#56B1F7",
  space = "Lab",
  na.value = "grey50",
  guide = "colourbar",
  aesthetics = "colour"
)

scale_fill_gradient(
  name = waiver(),
  ...,
  low = "#132B43",
  high = "#56B1F7",
  space = "Lab",
  na.value = "grey50",
  guide = "colourbar",
  aesthetics = "fill"
)

scale_colour_gradient2(
  name = waiver(),
  ...,
  low = muted("red"),
  mid = "white",
  high = muted("blue"),
  midpoint = 0,
  space = "Lab",
  na.value = "grey50",
  transform = "identity",
  guide = "colourbar",
  aesthetics = "colour"
)

scale_fill_gradient2(
  name = waiver(),
  ...,
  low = muted("red"),
  mid = "white",
  high = muted("blue"),
  midpoint = 0,
  space = "Lab",
  na.value = "grey50",
  transform = "identity",
  guide = "colourbar",
  aesthetics = "fill"
)

scale_colour_gradientn(
  name = waiver(),
  ...,
  colours,
  values = NULL,
  space = "Lab",
  na.value = "grey50",
  guide = "colourbar",
  aesthetics = "colour",
  colors
)

scale_fill_gradientn(
  name = waiver(),
  ...,
  colours,
  values = NULL,
  space = "Lab",
  na.value = "grey50",
  guide = "colourbar",
  aesthetics = "fill",
  colors
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
  [`continuous_scale`](https://ggplot2.tidyverse.org/dev/reference/continuous_scale.md)

  `scale_name`

  :   **\[deprecated\]** The name of the scale that should be used for
      error messages associated with this scale.

  `breaks`

  :   One of:

      - `NULL` for no breaks

      - [`waiver()`](https://ggplot2.tidyverse.org/dev/reference/waiver.md)
        for the default breaks computed by the [transformation
        object](https://scales.r-lib.org/reference/new_transform.html)

      - A numeric vector of positions

      - A function that takes the limits as input and returns breaks as
        output (e.g., a function returned by
        [`scales::extended_breaks()`](https://scales.r-lib.org/reference/breaks_extended.html)).
        Note that for position scales, limits are provided after scale
        expansion. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation.

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

  `n.breaks`

  :   An integer guiding the number of major breaks. The algorithm may
      choose a slightly different number to ensure nice break labels.
      Will only have an effect if `breaks = waiver()`. Use `NULL` to use
      the default number of breaks given by the transformation.

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

  `limits`

  :   One of:

      - `NULL` to use the default scale range

      - A numeric vector of length two providing limits of the scale.
        Use `NA` to refer to the existing minimum or maximum

      - A function that accepts the existing (automatic) limits and
        returns new limits. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation. Note that setting limits on positional scales
        will **remove** data outside of the limits. If the purpose is to
        zoom, use the limit argument in the coordinate system (see
        [`coord_cartesian()`](https://ggplot2.tidyverse.org/dev/reference/coord_cartesian.md)).

  `rescaler`

  :   A function used to scale the input values to the range \[0, 1\].
      This is always
      [`scales::rescale()`](https://scales.r-lib.org/reference/rescale.html),
      except for diverging and n colour gradients (i.e.,
      `scale_colour_gradient2()`, `scale_colour_gradientn()`). The
      `rescaler` is ignored by position scales, which always use
      [`scales::rescale()`](https://scales.r-lib.org/reference/rescale.html).
      Also accepts rlang
      [lambda](https://rlang.r-lib.org/reference/as_function.html)
      function notation.

  `oob`

  :   One of:

      - Function that handles limits outside of the scale limits (out of
        bounds). Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation.

      - The default
        ([`scales::censor()`](https://scales.r-lib.org/reference/oob.html))
        replaces out of bounds values with `NA`.

      - [`scales::squish()`](https://scales.r-lib.org/reference/oob.html)
        for squishing out of bounds values into range.

      - [`scales::squish_infinite()`](https://scales.r-lib.org/reference/oob.html)
        for squishing infinite values into range.

  `trans`

  :   **\[deprecated\]** Deprecated in favour of `transform`.

  `call`

  :   The `call` used to construct the scale for reporting messages.

  `super`

  :   The super class to use for the constructed scale

- low, high:

  Colours for low and high ends of the gradient.

- space:

  colour space in which to calculate gradient. Must be "Lab" - other
  values are deprecated.

- na.value:

  Colour to use for missing values

- guide:

  Type of legend. Use `"colourbar"` for continuous colour bar, or
  `"legend"` for discrete colour legend.

- aesthetics:

  Character string or vector of character strings listing the name(s) of
  the aesthetic(s) that this scale works with. This can be useful, for
  example, to apply colour settings to the `colour` and `fill`
  aesthetics at the same time, via `aesthetics = c("colour", "fill")`.

- mid:

  colour for mid point

- midpoint:

  The midpoint (in data value) of the diverging scale. Defaults to 0.

- transform:

  For continuous scales, the name of a transformation object or the
  object itself. Built-in transformations include "asn", "atanh",
  "boxcox", "date", "exp", "hms", "identity", "log", "log10", "log1p",
  "log2", "logit", "modulus", "probability", "probit", "pseudo_log",
  "reciprocal", "reverse", "sqrt" and "time".

  A transformation object bundles together a transform, its inverse, and
  methods for generating breaks and labels. Transformation objects are
  defined in the scales package, and are called `transform_<name>`. If
  transformations require arguments, you can call them from the scales
  package, e.g.
  [`scales::transform_boxcox(p = 2)`](https://scales.r-lib.org/reference/transform_boxcox.html).
  You can create your own transformation with
  [`scales::new_transform()`](https://scales.r-lib.org/reference/new_transform.html).

- colours, colors:

  Vector of colours to use for n-colour gradient.

- values:

  if colours should not be evenly positioned along the gradient this
  vector gives the position (between 0 and 1) for each colour in the
  `colours` vector. See
  [`rescale()`](https://scales.r-lib.org/reference/rescale.html) for a
  convenience function to map an arbitrary range to between 0 and 1.

## Details

Default colours are generated with munsell and
`mnsl(c("2.5PB 2/4", "2.5PB 7/10"))`. Generally, for continuous colour
scales you want to keep hue constant, but vary chroma and luminance. The
munsell package makes this easy to do using the Munsell colour system.

## See also

[`scales::pal_seq_gradient()`](https://scales.r-lib.org/reference/pal_seq_gradient.html)
for details on underlying palette,
[`scale_colour_steps()`](https://ggplot2.tidyverse.org/dev/reference/scale_steps.md)
for binned variants of these scales.

The documentation on [colour
aesthetics](https://ggplot2.tidyverse.org/dev/reference/aes_colour_fill_alpha.md).

Other colour scales:
[`scale_alpha()`](https://ggplot2.tidyverse.org/dev/reference/scale_alpha.md),
[`scale_colour_brewer()`](https://ggplot2.tidyverse.org/dev/reference/scale_brewer.md),
[`scale_colour_continuous()`](https://ggplot2.tidyverse.org/dev/reference/scale_colour_continuous.md),
[`scale_colour_discrete()`](https://ggplot2.tidyverse.org/dev/reference/scale_colour_discrete.md),
[`scale_colour_grey()`](https://ggplot2.tidyverse.org/dev/reference/scale_grey.md),
[`scale_colour_hue()`](https://ggplot2.tidyverse.org/dev/reference/scale_hue.md),
[`scale_colour_identity()`](https://ggplot2.tidyverse.org/dev/reference/scale_identity.md),
[`scale_colour_manual()`](https://ggplot2.tidyverse.org/dev/reference/scale_manual.md),
[`scale_colour_steps()`](https://ggplot2.tidyverse.org/dev/reference/scale_steps.md),
[`scale_colour_viridis_d()`](https://ggplot2.tidyverse.org/dev/reference/scale_viridis.md)

## Examples

``` r
set.seed(1)
df <- data.frame(
  x = runif(100),
  y = runif(100),
  z1 = rnorm(100),
  z2 = abs(rnorm(100))
)

df_na <- data.frame(
  value = seq(1, 20),
  x = runif(20),
  y = runif(20),
  z1 = c(rep(NA, 10), rnorm(10))
)

# Default colour scale colours from light blue to dark blue
ggplot(df, aes(x, y)) +
  geom_point(aes(colour = z2))


# For diverging colour scales use gradient2
ggplot(df, aes(x, y)) +
  geom_point(aes(colour = z1)) +
  scale_colour_gradient2()


# Use your own colour scale with gradientn
ggplot(df, aes(x, y)) +
  geom_point(aes(colour = z1)) +
  scale_colour_gradientn(colours = terrain.colors(10))


# The gradientn scale can be centered by using a rescaler
ggplot(df, aes(x, y)) +
  geom_point(aes(colour = z1)) +
  scale_colour_gradientn(
    colours = c("blue", "dodgerblue", "white", "orange", "red"),
    rescaler = ~ scales::rescale_mid(.x, mid = 0)
  )


# Equivalent fill scales do the same job for the fill aesthetic
ggplot(faithfuld, aes(waiting, eruptions)) +
  geom_raster(aes(fill = density)) +
  scale_fill_gradientn(colours = terrain.colors(10))


# Adjust colour choices with low and high
ggplot(df, aes(x, y)) +
  geom_point(aes(colour = z2)) +
  scale_colour_gradient(low = "white", high = "black")

# Avoid red-green colour contrasts because ~10% of men have difficulty
# seeing them

# Use `na.value = NA` to hide missing values but keep the original axis range
ggplot(df_na, aes(x = value, y)) +
  geom_bar(aes(fill = z1), stat = "identity") +
  scale_fill_gradient(low = "yellow", high = "red", na.value = NA)


 ggplot(df_na, aes(x, y)) +
   geom_point(aes(colour = z1)) +
   scale_colour_gradient(low = "yellow", high = "red", na.value = NA)
#> Warning: Removed 10 rows containing missing values or values outside the scale
#> range (`geom_point()`).

```
