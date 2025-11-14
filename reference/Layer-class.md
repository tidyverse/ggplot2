# Layers

The Layer class is a chaperone class not available for extension. The
class fulfils the following tasks. The class houses the Geom, Stat and
Position trinity and tracks their stateful parameters. Furthermore, its
methods are responsible for managing the layer data and exposing it to
other components of the plot at the right time.

## Details

The Layer class is an internal class that is not exported because the
class is not intended for extension. The
[`layer()`](https://ggplot2.tidyverse.org/reference/layer.md) function
instantiates the LayerInstance class, which inherits from Layer, but has
relevant fields populated.

The class is mostly used in
[`ggplot_build()`](https://ggplot2.tidyverse.org/reference/ggplot_build.md),
with the notable exception of the `draw_geom()` method, which is used in
[`ggplot_gtable()`](https://ggplot2.tidyverse.org/reference/ggplot_gtable.md)
instead.

## Fields

- `constructor`:

  A [call](https://rdrr.io/r/base/call.html) object with the user-facing
  constructor function, for use in error messaging. This field is
  populated by
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.md).

- `geom,stat,position`:

  These fields house the Geom, Stat and Position trifecta in ggproto
  form and is populated by
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.md).

- `stat_params,computed_stat_params`:

  These fields hold parameters assigned to the Stat. The `stat_params`
  field is directly derived from user input and is populated by
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.md). The
  `computed_stat_params` carries state and is constructed by the
  `Stat$setup_params()` method.

- `geom_params,computed_geom_params`:

  These fields hold parameters assigned to the Geom. The `geom_params`
  field is directly derived from user input and is populated by
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.md). The
  `computed_geom_params` carries state and is constructed by the
  `Geom$setup_params()` method.

- `mapping,computed_mapping`:

  These fields hold
  [mapping](https://ggplot2.tidyverse.org/reference/aes.md)s. The
  `mapping` field holds the `layer(mapping)` argument. The
  `computed_mapping` field carries state and is constructed in the
  `setup_layer()` method.

- `data`:

  The fortified `layer(data)` argument.

- `aes_params`:

  Holds the fixed, unmapped aesthetics passed to `layer(params)` as
  determined by `Geom$aesthetics()`.

- `inherit.aes`:

  A scalar boolean used in the `setup_layer()` method to indicate
  whether the `computed_mapping` should include the global mapping
  (`TRUE`) or only the layer mapping (`FALSE`). This is populated by the
  `layer(inherit.aes)` parameter.

- `layer_data`:

  **Description**

  A function method for initially resolving layer data. If layer data is
  missing or is a function, it will derive layer data from the global
  plot data.

  **Usage**

      Layer$layer_data(plot_data)

  **Arguments**

  `plot_data`

  :   The `data` field of the ggplot object.

  **Value**

  A data frame with layer data or `NULL`

- `setup_layer`:

  **Description**

  A function method is a hook to allow a final access to layer data in
  input form. In addition, it allows a layer access to global plot
  information. The latter is used to enforce the `inherit.aes` parameter
  by supplementing the layer mapping with the global mapping when
  requested.

  **Usage**

      Layer$setup_data(data, plot)

  **Arguments**

  `data`

  :   A data frame with the layer's data.

  `plot`

  :   A ggplot object

  **Value**

  A data frame with layer data. As a side effect, the `computed_mapping`
  field is populated.

- `compute_aesthetics`:

  **Description**

  A function method that evaluates aesthetics and warns about any
  problems. It also infers a `group` aesthetic if not provided. This
  method is also the step where layer data becomes standardised to base
  data frames without row names or additional attributes.

  **Usage**

      Layer$compute_aesthetics(data, plot)

  **Arguments**

  `data`

  :   A data frame with the layer's data.

  `plot`

  :   A ggplot object

  **Value**

  A data frame with layer data

- `compute_aesthetics`:

  **Description**

  A function method that orchestrates computing statistics. It executes
  methods from the Stat class to form new computed variables.

  **Usage**

      Layer$compute_statistic(data, layout)

  **Arguments**

  `data`

  :   A data frame with the layer's data.

  `layout`

  :   A `<Layout>` ggproto object.

  **Value**

  A data frame with layer data. As a side effect the
  `computed_stat_params` field is populated.

- `map_statistic`:

  **Description**

  A function method that finishes the result of computed statistics. It
  has several tasks:

  - It evaluates the
    [`after_stat()`](https://ggplot2.tidyverse.org/reference/aes_eval.md)
    stage of the mapping from both the `computed_mapping` but also the
    `Stat$default_aes` fields.

  - It ensures relevant scales are instantiated for computed aesthetics.

  - It takes care that scale transformation is applied to computed
    aesthetics.

  **Usage**

      Layer$map_statistic(data, plot)

  **Arguments**

  `data`

  :   A data frame with the layer's data.

  `plot`

  :   A ggplot object.

  **Value**

  A data frame with layer data

- `compute_geom_1`:

  **Description**

  A function method that prepares data for drawing. It checks that all
  required aesthetics are present and sets up parameters and data using
  the Geom class.

  **Usage**

      Layer$compute_geom_1(data)

  **Arguments**

  `data`

  :   A data frame with the layer's data.

  **Value**

  A data frame with layer data. As a side effect the
  `computed_geom_params` field is populated.

- `compute_position`:

  **Description**

  A function method that orchestrates the position adjustment. It
  executes methods from the Position class.

  **Usage**

      Layer$compute_position(data, layout)

  **Arguments**

  `data`

  :   A data frame with the layer's data.

  `layout`

  :   A `<Layout>` ggproto object.

  **Value**

  A data frame with layer data.

- `compute_geom_2`:

  **Description**

  A function method that add defaults and fixed parameters. It wraps the
  `Geom$use_defaults()` method.

  **Usage**

      Layer$compute_geom_2(data, params, theme, ...)

  **Arguments**

  `data`

  :   A data frame with the layer's data.

  `params`

  :   A list with fixed aesthetic parameters, typically the `aes_params`
      field.

  `theme`

  :   A [theme](https://ggplot2.tidyverse.org/reference/theme.md) object

  `...`

  :   Passed on to `Geom$use_defaults()`, not in use.

  **Value**

  A data frame with layer data.

- `finish_statistics`:

  **Description**

  A function method that wraps `Stat$finish_layer()`.

  **Usage**

      Layer$finish_statistics(data)

  **Arguments**

  `data`

  :   A data frame with the layer's data.

  **Value**

  A data frame with layer data.

- `draw_geom`:

  **Description**

  A function method that produces graphics for every panel. It uses Geom
  class methods to handle missing data and produce grobs. In contrast to
  other methods, this is called during the
  [`ggplot_gtable()`](https://ggplot2.tidyverse.org/reference/ggplot_gtable.md)
  stage, not the
  [`ggplot_build()`](https://ggplot2.tidyverse.org/reference/ggplot_build.md)
  stage.

  **Usage**

      Layer$draw_geom(data, layout)

  **Arguments**

  `data`

  :   A data frame with the layer's data.

  `layout`

  :   A `<Layout>` ggproto object.

  **Value**

  A list of grobs, one per panel.

- `print`:

  **Description**

  A function method that prints information about the layer.

  **Usage**

      Layer$print()

  **Value**

  Nothing (`NULL`), invisibly

## Layer data diagram

As the Layer class is a chaparone for the data, it makes sense to give a
small overview of how layer data flows through a plot. In the diagram
below we following the `layer(data)` argument over the course of plot
building through Layer class methods. When an outside class acts on the
data without the Layer class, this is indicated with the left arrow
`<-`. Subcomponents of a method that touch data are indicated with the
right arrow `->`.

    # Inside `ggplot_build()`
     |
    layer(data)
     |
     |
     | # Inherit plot data
     |
    Layer$layer_data()
     |
     |
     | # Finalise mapping
     |
    Layer$setup_layer()
     |
     |
     | # Append PANEL variable for facets
     |
     |<- Layout$setup()
     |    |
     |    +-> Facet$setup_data()
     |    |
     |    +-> Coord$setup_data()
     |
     |
     | # Evaluate mappings to new data and infer group
     |
    Layer$compute_aesthetics()
     |
     |
     | # Scale-transform all aesthetics
     |
     |<- ScalesList$transform_df()
     |    |
     |    +-> Scale$transform_df()
     |
     |
     | # Map x/y aesthetics with initial scale
     |
     |<- Layout$map_position()
     |    |
     |    +-> Scale$map()
     |
     |
     | # Compute stat part of layer
     |
    Layer$compute_statistic()
     | |
     | +-> Stat$setup_data()
     | |
     | +-> Stat$compute_layer()
     |
     |
     | # Add `after_stat()` stage
     | # Scale transform computed variables
     |
    Layer$map_statistic()
     |
     |
     | # Setup geom part of layer
     |
    Layer$compute_geom_1()
     | |
     | +-> Geom$setup_data()
     |
     |
     | # Apply position adjustments
     |
    Layer$compute_position()
     | |
     | +-> Position$use_defaults()
     | |
     | +-> Position$setup_data()
     | |
     | +-> Position$compute_layer()
     |
     |
     | # Map x/y aesthetics with final scales
     |
     |<- Layout$map_position()
     |    |
     |    +-> Scale$map()
     |
     | # Map non-position aesthetics
     |
     |<- ScalesList$map_df()
     |    |
     |    +-> Scale$map()
     |
     |
     | # Fill in defaults and fixed aesthetics
     |
    Layer$compute_geom_2()
     | |
     | +-> Geom$use_defaults()
     |
     |
     | # Apply final Stat hook
     |
    Layer$finish_statistics()
     | |
     | +-> Stat$finish_layer()
     |
     |
     | # Apply final Facet hook
     |
     |<- Layout$finish_data()
     |    |
     |    +-> Facet$finish_data()
     |
     V
    # `ggplot_build()` is finished
    # Hand off to `ggplot_gtable()`
     |
     |
     | # Draw the geom part
     |
    Layer$draw_geom()
     |
     +-> Geom$handle_na()
     |
     +-> Geom$draw_layer()

## See also

Other Layer components:
[`Geom`](https://ggplot2.tidyverse.org/reference/Geom.md),
[`Position`](https://ggplot2.tidyverse.org/reference/Position.md),
[`Stat`](https://ggplot2.tidyverse.org/reference/Stat.md)

Other chaperone classes:
[`Layout`](https://ggplot2.tidyverse.org/reference/Layout.md)

## Examples

``` r
# None: Layer is not intended to be extended
```
