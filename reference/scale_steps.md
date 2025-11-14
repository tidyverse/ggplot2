# Binned gradient colour scales

`scale_*_steps` creates a two colour binned gradient (low-high),
`scale_*_steps2` creates a diverging binned colour gradient
(low-mid-high), and `scale_*_stepsn` creates a n-colour binned gradient.
These scales are binned variants of the [gradient
scale](https://ggplot2.tidyverse.org/reference/scale_gradient.md) family
and works in the same way.

## Usage

``` r
scale_colour_steps(
  name = waiver(),
  ...,
  low = "#132B43",
  high = "#56B1F7",
  space = "Lab",
  na.value = "grey50",
  guide = "coloursteps",
  aesthetics = "colour"
)

scale_colour_steps2(
  name = waiver(),
  ...,
  low = muted("red"),
  mid = "white",
  high = muted("blue"),
  midpoint = 0,
  space = "Lab",
  na.value = "grey50",
  transform = "identity",
  guide = "coloursteps",
  aesthetics = "colour"
)

scale_colour_stepsn(
  name = waiver(),
  ...,
  colours,
  values = NULL,
  space = "Lab",
  na.value = "grey50",
  guide = "coloursteps",
  aesthetics = "colour",
  colors
)

scale_fill_steps(
  name = waiver(),
  ...,
  low = "#132B43",
  high = "#56B1F7",
  space = "Lab",
  na.value = "grey50",
  guide = "coloursteps",
  aesthetics = "fill"
)

scale_fill_steps2(
  name = waiver(),
  ...,
  low = muted("red"),
  mid = "white",
  high = muted("blue"),
  midpoint = 0,
  space = "Lab",
  na.value = "grey50",
  transform = "identity",
  guide = "coloursteps",
  aesthetics = "fill"
)

scale_fill_stepsn(
  name = waiver(),
  ...,
  colours,
  values = NULL,
  space = "Lab",
  na.value = "grey50",
  guide = "coloursteps",
  aesthetics = "fill",
  colors
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
  [`binned_scale`](https://ggplot2.tidyverse.org/reference/binned_scale.md)

  `n.breaks`

  :   The number of break points to create if breaks are not given
      directly.

  `nice.breaks`

  :   Logical. Should breaks be attempted placed at nice values instead
      of exactly evenly spaced between the limits. If `TRUE` (default)
      the scale will ask the transformation object to create breaks, and
      this may result in a different number of breaks than requested.
      Ignored if breaks are given explicitly.

  `oob`

  :   One of:

      - Function that handles limits outside of the scale limits (out of
        bounds). Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation.

      - The default
        ([`scales::squish()`](https://scales.r-lib.org/reference/oob.html))
        squishes out of bounds values into range.

      - [scales::censor](https://scales.r-lib.org/reference/oob.html)
        for replacing out of bounds values with `NA`.

      - [`scales::squish_infinite()`](https://scales.r-lib.org/reference/oob.html)
        for squishing infinite values into range.

  `right`

  :   Should the intervals be closed on the right (`TRUE`, default) or
      should the intervals be closed on the left (`FALSE`)? 'Closed on
      the right' means that values at break positions are part of the
      lower bin (open on the left), whereas they are part of the upper
      bin when intervals are closed on the left (open on the right).

  `show.limits`

  :   should the limits of the scale appear as ticks

  `breaks`

  :   One of:

      - `NULL` for no breaks

      - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.md)
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
        [`coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.md)).

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
[`scale_colour_gradient()`](https://ggplot2.tidyverse.org/reference/scale_gradient.md)
for continuous scales without binning.

The documentation on [colour
aesthetics](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.md).

The [binned colour scales
section](https://ggplot2-book.org/scales-colour#sec-binned-colour) of
the online ggplot2 book.

Other colour scales:
[`scale_alpha()`](https://ggplot2.tidyverse.org/reference/scale_alpha.md),
[`scale_colour_brewer()`](https://ggplot2.tidyverse.org/reference/scale_brewer.md),
[`scale_colour_continuous()`](https://ggplot2.tidyverse.org/reference/scale_colour_continuous.md),
[`scale_colour_discrete()`](https://ggplot2.tidyverse.org/reference/scale_colour_discrete.md),
[`scale_colour_gradient()`](https://ggplot2.tidyverse.org/reference/scale_gradient.md),
[`scale_colour_grey()`](https://ggplot2.tidyverse.org/reference/scale_grey.md),
[`scale_colour_hue()`](https://ggplot2.tidyverse.org/reference/scale_hue.md),
[`scale_colour_identity()`](https://ggplot2.tidyverse.org/reference/scale_identity.md),
[`scale_colour_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.md),
[`scale_colour_viridis_d()`](https://ggplot2.tidyverse.org/reference/scale_viridis.md)

## Examples

``` r
set.seed(1)
df <- data.frame(
  x = runif(100),
  y = runif(100),
  z1 = rnorm(100)
)

# Use scale_colour_steps for a standard binned gradient
ggplot(df, aes(x, y)) +
  geom_point(aes(colour = z1)) +
  scale_colour_steps()


# Get a divergent binned scale with the *2 variant
ggplot(df, aes(x, y)) +
  geom_point(aes(colour = z1)) +
  scale_colour_steps2()


# Define your own colour ramp to extract binned colours from
ggplot(df, aes(x, y)) +
  geom_point(aes(colour = z1)) +
  scale_colour_stepsn(colours = terrain.colors(10))
```
