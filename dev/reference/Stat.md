# Stats

All `stat_*()` functions (like
[`stat_bin()`](https://ggplot2.tidyverse.org/dev/reference/geom_histogram.md))
return a layer that contains a `Stat*` object (like `StatBin`). The
`Stat*` object is responsible for rendering the data in the plot.

## Details

Each of the `Stat*` objects is a
[`ggproto()`](https://ggplot2.tidyverse.org/dev/reference/ggproto.md)
object, descended from the top-level `Stat`, and each implements various
methods and fields. The object and its parameters are chaperoned by the
[Layer](https://ggplot2.tidyverse.org/dev/reference/Layer-class.md)
class.

To create a new type of Stat object, you typically will want to override
one or more of the following:

- The `required_aes` and `default_aes` fields.

- One of the `compute_layer()`, `compute_panel()` or `compute_group()`
  functions. Typically it best to implement `compute_group()` and use
  the higher-up methods when there are substantial performance
  improvements to be gained.

- The `finish_layer()` method

## Fields

- `required_aes`:

  A character vector naming aesthetics that are necessary to compute the
  stat.

- `non_missing_aes`:

  A character vector naming aesthetics that will cause removal if they
  have missing values.

- `optional_aes`:

  A character vector naming aesthetics that will be accepted by
  [`layer()`](https://ggplot2.tidyverse.org/dev/reference/layer.md), but
  are not required or dscribed in the `default_aes` field.

- `default_aes`:

  A [mapping](https://ggplot2.tidyverse.org/dev/reference/aes.md) of
  default values for aesthetics. Aesthetics can be set to `NULL` to be
  included as optional aesthetic.

- `dropped_aes`:

  A character vector naming aesthetics that can be dropped from the data
  without warning. Typically used for aesthetics that are 'consumed'
  during computation like `"weight"`.

- `extra_params`:

  A character vector of parameter names in addition to those imputed
  from the `compute_panel()` or `compute_groups()` methods. This field
  can be set to include parameters for `setup_data()` methods. By
  default, this only contains `"na.rm"`.

- `retransform`:

  A scalar boolean: should the values produced by the statistic also be
  transformed in the second pass when recently added statistics are
  trained to the scales

- `setup_params`:

  **Description**

  A function method for modifying or checking the parameters based on
  the data. The default method returns the parameters unaltered.

  **Usage**

      Stat$setup_params(data, params)

  **Arguments**

  `data`

  :   A data frame with the layer's data.

  `params`

  :   A list of current parameters

  **Value**

  A list of parameters

- `setup_data`:

  **Description**

  A function method for modifying or checking the data. The default
  method returns data unaltered.

  **Usage**

      Stat$setup_data(data, params)

  **Arguments**

  `data`

  :   A data frame with the layer's data.

  `params`

  :   A list of parameters coming from the `setup_params()` method

  **Value**

  A data frame with layer data

- `compute_layer`:

  **Description**

  A function method for orchestrating the computation of the statistic.
  The default method splits the data and passes on computation tasks to
  the panel-level `compute_panel()` method. In addition, the default
  method handles missing values by removing rows that have missing
  values for the aesthetics listed in the `required_aes` and
  `non_missing_aes` fields. It is not recommended to use this method as
  an extension point.

  **Usage**

      Stat$compute_layer(data, params, layout)

  **Arguments**

  `data`

  :   A data frame with the layer's data.

  `params`

  :   A list of parameters

  `layout`

  :   A pre-trained `<Layout>` ggproto object.

  **Value**

  A data frame with computed data

- `compute_panel,compute_group`:

  **Description**

  A function method orchestrating the computation of statistics for a
  single panel or group. The default `compute_panel()` method splits the
  data into groups, and passes on computation tasks to the
  `compute_group()` method. In addition, `compute_panel()` is tasked
  with preserving aesthetics that are constant within a group and
  preserving these if the computation loses them. The default
  `compute_group()` is not implemented.

  **Usage**

      Stat$compute_panel(data, scales, ...)
      Stat$compute_group(data, scales, ...)

  **Arguments**

  `data`

  :   A data frame with the layer's data.

  `scales`

  :   A list of pre-trained `x` and `y` scales. Note that the position
      scales are not finalised at this point and reflect the initial
      data range before computing stats.

  `...`

  :   Reserved for extensions. By default, this passes parameters to the
      `compute_group()` method.

  **Value**

  A data frame with layer data

- `finish_layer`:

  **Description**

  A function method acting as a hook to modify data after scales have
  been applied, but before geoms have to render. The default is to pass
  the data unaltered. This can be used as an extension point when actual
  aesthetic values rather than values mapped to the aesthetic are
  needed.

  **Usage**

      Stat$finish_layer(data, params)

  **Arguments**

  `data`

  :   A data frame with layer data

  `params`

  :   A list of parameters

  **Value**

  A data frame with layer data

- `parameters`:

  **Description**

  A function method for listing out all acceptable parameters for this
  stat.

  **Usage**

      Stat$parameters(extra)

  **Arguments**

  `extra`

  :   A boolean: whether to include the `extra_params` field.

  **Value**

  A character vector of parameter names.

- `aesthetics`:

  **Description**

  A function method for listing out all acceptable aesthetics for this
  stat.

  **Usage**

      Stat$aesthetics()

  **Value**

  A character vector of aesthetic names.

## Conventions

The object name that a new class is assigned to is typically the same as
the class name. Stat class names are in UpperCamelCase and start with
the `Stat*` prefix, like `StatNew`.

A constructor function is usually paired wih a Stat class. The
constructor wraps a call to
[`layer()`](https://ggplot2.tidyverse.org/dev/reference/layer.md), where
e.g. `layer(stat = StatNew)`. The constructor function name is formatted
by taking the Stat class name and formatting it with snake_case, so that
`StatNew` becomes `stat_new()`.

## See also

The [new stats
section](https://ggplot2-book.org/extensions#sec-new-stats) of the
online ggplot2 book..

Run
[`vignette("extending-ggplot2")`](https://ggplot2.tidyverse.org/dev/articles/extending-ggplot2.md),
in particular the "Creating a new stat" section.

Other Layer components:
[`Geom`](https://ggplot2.tidyverse.org/dev/reference/Geom.md),
[`Layer-class`](https://ggplot2.tidyverse.org/dev/reference/Layer-class.md),
[`Position`](https://ggplot2.tidyverse.org/dev/reference/Position.md)

## Examples

``` r
# Extending the class
StatKmeans <- ggproto(
  "StatKmeans", Stat,
  # Fields
  required_aes = c("x", "y"),
  # You can relate computed variables to aesthetics using `after_stat()`
  # in defaults
  default_aes = aes(colour = after_stat(cluster)),
  # Methods
  compute_panel = function(data, scales, k = 2L) {
    km <- kmeans(cbind(scale(data$x), scale(data$y)), centers = k)
    data$cluster <- factor(km$cluster)
    data
  }
)

# Building a constructor
stat_kmeans <- function(mapping = NULL, data = NULL, geom = "point",
                        position = "identity", ..., k = 2L, na.rm = FALSE,
                        show.legend = NA, inherit.aes = TRUE) {
  layer(
    mapping = mapping, data = data,
    geom = geom, stat = StatKmeans, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, k = k, ...)
  )
}

# Use new stat in plot
ggplot(mpg, aes(displ, hwy)) +
  stat_kmeans(k = 3)
```
