# Scales

All `scale_*()` functions (like
[`scale_fill_continuous()`](https://ggplot2.tidyverse.org/reference/scale_colour_continuous.md))
return a `Scale*` object. The main purpose of these objects is to
translate data values to aesthetic values and populating breaks and
labels.

## Details

All `scale_*` functions like
[`scale_x_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.md)
return a `Scale*` object like `ScaleContinuous`. Each of the `Scale*`
objects is a
[`ggproto()`](https://ggplot2.tidyverse.org/reference/ggproto.md) object
descended from the top-level `Scale`.

Scales generally need to track three types of spaces:

1.  Data space. These are values as they are evaluated from the plot or
    layer mapping, prior to any transformation.

2.  Transformed space. This is the space after original data has been
    transformed. Effectively, scales internally operate in transformed
    space in that ranges and breaks get passed around in this space.
    Discrete scales don't do transformations, so for these scales,
    transformed space is the same as untransformed space.

3.  Aesthetic space. Graphic values that are mapped from the transformed
    space. This is dependent on the `palette` field for most scales and
    on the `rescaler` field for continuous position scales.

The user is expected to give any vector-based `minor_breaks`, `breaks`
or `limits` in data space. When `breaks`, `limits` or `labels` is a
function, its input is expected to be in data space.

Before you attempt to create a new `Scale*` class of your own, it is
recommended to think through whether your aims cannot be implemented
with one of the existing classes. Making a wrapper for the
[`continuous_scale()`](https://ggplot2.tidyverse.org/reference/continuous_scale.md),
[`discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.md)
and
[`binned_scale()`](https://ggplot2.tidyverse.org/reference/binned_scale.md)
should cover many cases, and should be considered prior to commiting to
build a `Scale*` extension.

For example, if you aim to develop a scale for a new data type, it
should generally be possible to create a new
[transformation](https://scales.r-lib.org/reference/new_transform.html)
instead. One reason to implement your own `Scale*` class is to
accommodate a data type does not lend itself for continuous or discrete
range training.

In such case, you can override the following:

- The `range` field.

- The `transform`, `train` and `map` methods.

- The `get_limits()`, `get_breaks()` and `get_labels()` methods.

## Fields

- `call`:

  A [call](https://rdrr.io/r/base/call.html) object with the user-facing
  constructor function, for use in error messaging. This field is
  populated by scale constructors.

- `range`:

  A [`Range`](https://scales.r-lib.org/reference/Range.html) class
  object, like
  [`scales::ContinuousRange`](https://scales.r-lib.org/reference/Range.html)
  or
  [`scales::DiscreteRange`](https://scales.r-lib.org/reference/Range.html).
  These are 'trained' to keep track of the data range (continuous) or
  data levels (discrete). Continuous ranges are tracked in transformed
  space.

- `aesthetics,palette,name,breaks,labels,limits,name,guide,position,na.value,expand`:

  Fields populated by the scale constructor that can take on the same
  values as described in e.g.
  [`?continuous_scale`](https://ggplot2.tidyverse.org/reference/continuous_scale.md).
  Note that `limits` is expected in transformed space.

- `transform_df,transform`:

  **Description**

  A function method for orchestrating the transformation of aesthetics
  in a data frame. Data transformation occurs before stats are computed.
  The `transform_df()` method ensures the
  [`transform()`](https://rdrr.io/r/base/transform.html) method is
  applied to the correct columns.

  **Usage**

      Scale$transform_df(df)
      Scale$transform(x)

  **Arguments**

  `df`

  :   A data frame with the layer's data.

  `x`

  :   A vector of the relevant aesthetic.

  **Value**

  For [`transform()`](https://rdrr.io/r/base/transform.html) a vector of
  transformed values. For `transform_df()`, a named list with
  transformed values for each transformed aesthetic.

- `train_df,train`:

  **Description**

  A function method for orchestrating scale training for keeping track
  of the data range or levels. The `train_df()` method ensures the
  `train()` method is applied to the correct columns.

  **Usage**

      Scale$train_df(df)
      Scale$train(x)

  **Arguments**

  `df`

  :   A data frame with the layer's data.

  `x`

  :   A vector of the relevant aesthetic.

  **Value**

  Nothing, these are called for their side effect of updating the
  `range` field.

- `map_df,map`:

  **Description**

  A function method for orchestrating the mapping of data values to
  aesthetics. The `map_df()` method ensures the `map()` method is
  applied to the correct columns. When the scale uses a
  [`palette()`](https://rdrr.io/r/grDevices/palette.html) function, it
  is applied in the `map()` method.

  **Usage**

      Scale$map_df(df, i)
      Scale$map(x, limits)

  **Arguments**

  `df`

  :   A data frame with the layer's data.

  `i`

  :   An integer vector giving an index to map a subset of data. The
      default, `NULL`, will map all rows.

  `x`

  :   A vector of the relevant aesthetic.

  `limits`

  :   A vector of the relevant aesthetic, usually via the `get_limits()`
      method.

  **Value**

  For `map()` a vector of mapped values in aesthetics space. For
  `map_df()`, a named list with mapped values for each aesthetic.

- `recale`:

  **Description**

  A function method for applying the recale function in the `rescaler`
  field. It is used during the continuous `map()` and
  `Coord$transform()` methods to ensure values are in the 0-1 range.

  **Usage**

      Scale$rescale(x, limits, range)

  **Arguments**

  `x`

  :   A vector of values to rescale. Can contain out-of-bounds or
      missing values depending on the `map()` method.

  `limits`

  :   A length two vector giving the limits of the relevant aesthetic,
      usually via the `get_limits()` method.

  `range`

  :   A length two vector giving the range that should coincide with the
      0-1 points. For most purpuses, this should be the same as the
      `limits` argument.

  **Value**

  A vector of values between 0 and 1 for in-bounds values of `x`.

- `get_limits`:

  **Description**

  A function method for resolving user input and getting the scale
  limits.

  **Usage**

      Scale$get_limits()

  **Value**

  The scale limits, without any expansion applied, in transformed space.

- `dimension`:

  **Description**

  A function method for getting a continuous representation of the
  limits of position scales. For continuous scales, the dimension is the
  same concept as the limits. For discrete scales the dimension is the
  continuous range occupied by the mapped breaks, which by default take
  integer positions.

  **Usage**

      Scale$dimension(expand, limits)

  **Arguments**

  `expand`

  :   A length 4 vector giving scale
      [expansion](https://ggplot2.tidyverse.org/reference/expansion.md).
      This is optional and defaults to no expansion.

  `limits`

  :   A vector of the relevant aesthetic, usually via the `get_limits()`
      method.

  **Value**

  A numeric vector of length 2

- `get_breaks,get_breaks_minor`:

  **Description**

  A function method for resolving user input and getting the scale
  breaks or minor breaks. Note that these may return out-of-bounds
  values for the purpose of coordinating with the `get_labels()` method.

  **Usage**

      Scale$get_breaks(limits)
      Scale$get_breaks_minor(n, b, limits)

  **Arguments**

  `limits`

  :   A vector of the relevant aesthetic, usually via the `get_limits()`
      method.

  `n`

  :   An integer setting the desired number of minor breaks per major
      break. Note that the resulting minor breaks may coincide with
      major breaks.

  `b`

  :   A vector of mapped major breaks from the `get_breaks()` method.

  **Value**

  A vector of breaks in transformed space.

- `get_labels`:

  **Description**

  A function method for resolving user input and getting the scale
  labels for a set of breaks.

  **Usage**

      Scale$get_labels(breaks)

  **Arguments**

  `breaks`

  :   A vector of unmapped major breaks from the `get_breaks()` method,
      in transformed space.

  **Value**

  A vector of labels of the same length as `breaks`.

- `get_transformation`:

  **Description**

  A helper method to access the scale's transformation object.

  **Usage**

      Scale$get_transformation()

  **Value**

  A [transform](https://scales.r-lib.org/reference/new_transform.html)
  object.

- `break_info`:

  **Description**

  A function method for getting all break related information for
  position scales. It is in use by coords that do not use the modern
  Guide system and secondary axes.

  **Usage**

      Scale$break_info(range)

  **Arguments**

  `range`

  :   A vector of the relevant aesthetic.

  **Value**

  A named list with the following structure:

  - `range` a length 2 vector giving continuous range

  - `labels` a character or expression vector of the same length as
    major breaks.

  - `major` a numeric vector with mapped numeric values for major
    breaks.

  - `major_source` a numeric vector with (transformed) data values for
    major breaks.

  - `minor` a numeric vector with mapped numeric values for minor
    breaks.

  - `minor_source` a numeric vector with (transformed) data values for
    minor breaks.

- `break_position`:

  **Description**

  A function method for getting mapped break positions. It is in use as
  a default value in `get_breaks_minor()`, but is otherwise vestigial.

  **Usage**

      Scale$break_info(range)

  **Arguments**

  `range`

  :   A vector of the relevant aesthetic.

  **Value**

  A vector with mapped break positions

- `make_title,make_sec_title`:

  **Description**

  A function method for picking the title to use. This is usually called
  in the `Guide$extract_params()` or `Layout$resolve_label()` methods.
  The hierarchy of titles goes from guide (highest priority), to scale,
  to labs (lowest priority). When the guide or scale title are
  functions, they're applied to the next in line. The `make_sec_title()`
  method by default re-uses the primary `make_title()` method and only
  applies to position aesthetics.

  **Usage**

      Scale$make_title(guide_title, scale_title, label_title)
      Scale$make_sec_title(...)

  **Arguments**

  `guide_title`

  :   The `title` parameter coming from a guide.

  `scale_title`

  :   The `name` field of the Scale.

  `label_title`

  :   The relevant entry in the `plot$labels` field.

  `...`

  :   By default, arguments forwarded to the `make_title()` method

  **Value**

  A scalar character or expression title

- `axis_order`:

  **Description**

  A function method for setting the order of axes titles used to
  coordinate with `Facet$draw_labels()`.

  **Usage**

      Scale$axis_order()

  **Value**

  Either `c("primary", "secondary")` or `c("secondary", "primary")`.

- `clone`:

  **Description**

  A function method for making an untrained copy of the scale. Due to
  reference semantics of ggproto objects, in contrast to copy-on-modify
  semantics, scales need to be cloned at the start of plot building. The
  cloned scale can be trained independently of the original.

  **Usage**

      Scale$clone()

  **Value**

  A Scale object.

- `reset`:

  **Description**

  A function method for to reset the `range` field, effectively
  'untraining' the scale. This is used in the `Layout$reset_scales()`
  method, so that scales can be re-trained on data with final position
  aesthetics. For discrete scales, only the continuous range (`range_c`)
  is reset.

  **Usage**

      Scale$clone()

  **Value**

  None, called for the side-effect of resetting the range.

- `is_empty`:

  **Description**

  A function method for determining whether a scale is empty, i.e. when
  no information with which to calculate limits.

  **Usage**

      Scale$is_empty()

  **Value**

  A scalar boolean value.

- `is_empty`:

  **Description**

  A function method for determining whether a scale is discrete.

  **Usage**

      Scale$is_discrete()

  **Value**

  A scalar boolean value.

## Conventions

The object name that a new class is assigned to is typically the same as
the class name. Scale class names are in UpperCamelCase and start with
the `Scale*` prefix, like `ScaleNew`.

In scales, there is a difference between user-facing and
developer-facing constructors. Developer facing constructors have the
shape `{foundation}_scale()`, like
[`discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.md)
corresponding to `ScaleDiscrete`. User-facing constructors have the
`scale_{aesthetic}_{type}` as name. If you implement a new `Scale*`
class, you like want both these types of constructor.

## Examples

``` r
# TODO: find easy to digest example
NULL
#> NULL
```
