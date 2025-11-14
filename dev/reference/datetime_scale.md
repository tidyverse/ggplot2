# Date/time scale constructor

Date/time scale constructor

## Usage

``` r
datetime_scale(
  aesthetics,
  transform,
  trans = deprecated(),
  palette,
  breaks = pretty_breaks(),
  minor_breaks = waiver(),
  labels = waiver(),
  date_breaks = waiver(),
  date_labels = waiver(),
  date_minor_breaks = waiver(),
  timezone = NULL,
  guide = "legend",
  call = caller_call(),
  ...
)
```

## Arguments

- aesthetics:

  The names of the aesthetics that this scale works with.

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

- trans:

  For date/time scales, the name of a date/time transformation or the
  object itself. Built-in transformations include "hms", "date" and
  "time".

- palette:

  A palette function that when called with a numeric vector with values
  between 0 and 1 returns the corresponding output values (e.g.,
  [`scales::pal_area()`](https://scales.r-lib.org/reference/pal_area.html)).

- breaks:

  One of:

  - `NULL` for no breaks

  - [`waiver()`](https://ggplot2.tidyverse.org/dev/reference/waiver.md)
    for the breaks specified by `date_breaks`

  - A `Date`/`POSIXct` vector giving positions of breaks

  - A function that takes the limits as input and returns breaks as
    output

- minor_breaks:

  One of:

  - `NULL` for no breaks

  - [`waiver()`](https://ggplot2.tidyverse.org/dev/reference/waiver.md)
    for the breaks specified by `date_minor_breaks`

  - A `Date`/`POSIXct` vector giving positions of minor breaks

  - A function that takes the limits as input and returns minor breaks
    as output

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

- date_breaks:

  A string giving the distance between breaks like "2 weeks", or "10
  years". If both `breaks` and `date_breaks` are specified,
  `date_breaks` wins. Valid specifications are 'sec', 'min', 'hour',
  'day', 'week', 'month' or 'year', optionally followed by 's'.

- date_labels:

  A string giving the formatting specification for the labels. Codes are
  defined in [`strftime()`](https://rdrr.io/r/base/strptime.html). If
  both `labels` and `date_labels` are specified, `date_labels` wins.

- date_minor_breaks:

  A string giving the distance between minor breaks like "2 weeks", or
  "10 years". If both `minor_breaks` and `date_minor_breaks` are
  specified, `date_minor_breaks` wins. Valid specifications are 'sec',
  'min', 'hour', 'day', 'week', 'month' or 'year', optionally followed
  by 's'.

- timezone:

  The timezone to use for display on the axes. The default (`NULL`) uses
  the timezone encoded in the data.

- guide:

  A function used to create a guide or its name. See
  [`guides()`](https://ggplot2.tidyverse.org/dev/reference/guides.md)
  for more information.

- call:

  The `call` used to construct the scale for reporting messages.

- ...:

  Arguments passed on to
  [`continuous_scale`](https://ggplot2.tidyverse.org/dev/reference/continuous_scale.md)

  `scale_name`

  :   **\[deprecated\]** The name of the scale that should be used for
      error messages associated with this scale.

  `name`

  :   The name of the scale. Used as the axis or legend title. If
      [`waiver()`](https://ggplot2.tidyverse.org/dev/reference/waiver.md),
      the default, the name of the scale is taken from the first mapping
      used for that aesthetic. If `NULL`, the legend title will be
      omitted.

  `n.breaks`

  :   An integer guiding the number of major breaks. The algorithm may
      choose a slightly different number to ensure nice break labels.
      Will only have an effect if `breaks = waiver()`. Use `NULL` to use
      the default number of breaks given by the transformation.

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
      [`scale_colour_gradient2()`](https://ggplot2.tidyverse.org/dev/reference/scale_gradient.md),
      [`scale_colour_gradientn()`](https://ggplot2.tidyverse.org/dev/reference/scale_gradient.md)).
      The `rescaler` is ignored by position scales, which always use
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

  `na.value`

  :   Missing values will be replaced with this value.

  `expand`

  :   For position scales, a vector of range expansion constants used to
      add some padding around the data to ensure that they are placed
      some distance away from the axes. Use the convenience function
      [`expansion()`](https://ggplot2.tidyverse.org/dev/reference/expansion.md)
      to generate the values for the `expand` argument. The defaults are
      to expand the scale by 5% on each side for continuous variables,
      and by 0.6 units on each side for discrete variables.

  `fallback.palette`

  :   Function to use when `palette = NULL` and the palette is not
      represented in the theme.

  `position`

  :   For position scales, The position of the axis. `left` or `right`
      for y axes, `top` or `bottom` for x axes.

  `super`

  :   The super class to use for the constructed scale
