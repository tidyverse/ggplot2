# Layout

The Layout class is a chaperone class discouraged for extension. The
class fulfils the following tasks. The class houses the Coord and Facet
classes and tracks their stateful parameters. In addition, it manages
the position scales for each panel. It is responsible for keeping track
of panel specifications and matching pieces of the data to scales and
parameters in panel-wise manners.

## Details

The Layout class is only exported for extensions that re-implement a
[`ggplot_build()`](https://ggplot2.tidyverse.org/reference/ggplot_build.md)
method for their specific class of plots. It is discouraged to subclass
the Layout class and for all purposes be considered an internal
structure. It has no user-facing constructor to put an small barrier in
the way.

The class is used throughout
[`ggplot_build()`](https://ggplot2.tidyverse.org/reference/ggplot_build.md),
with the notable exception of the `render()` method, which is used in
[`ggplot_gtable()`](https://ggplot2.tidyverse.org/reference/ggplot_gtable.md)
instead.

## Fields

- `coord,coord_params`:

  A [`<Coord>`](https://ggplot2.tidyverse.org/reference/Coord.md)
  ggproto object and a list of the coordinate system's parameters.
  Parameters get populated by the `Coord$setup_params()` method.

- `facet,facet_params`:

  A [`<Facet>`](https://ggplot2.tidyverse.org/reference/Facet.md)
  ggproto object and a list of the faceting specification's parameters.
  Parameters get populated by the `Facet$setup_params()` method.

- `layout`:

  A data frame with a row for each panel. The data frame contains
  integer columns `PANEL`, `SCALE_X`, `SCALE_Y`, `ROW` and `COL`
  representing a panel ID, scale indices and placement locations. In
  addition, the layout may contain faceting variables or other
  additional information. This field gets populated by the
  `Facet$compute_layout()` method.

- `panel_scales_x,panel_scales_y`:

  A list of `x` and `y` position scales parallel to the layout field's
  `SCALE_X` and `SCALE_Y` levels respectively. This fields gets
  populated by the `Facet$init_scales()` method.

- `panel_params`:

  A named list of parameters per panel populated by the
  `Coord$setup_panel_params()` method. Contains `<ViewScale>` entries
  for the `x` and `y` variables in addition to ranges and other
  information the coordinate system might need to transform or render
  guides and grids.

- `setup`:

  **Description**

  A function method for setting up the relevant information for the
  layout of the plot. It populates the `facet_params`, `coord_params`
  and `layout` fields and appends a `PANEL` variable to the layer data.

  **Usage**

      Layout$setup(data, plot_data, plot_env)

  **Arguments**

  `data`

  :   A list of data frames with layer data.

  `plot_data`

  :   The data frame in the `data` field of the ggplot object.

  `plot_env`

  :   The environment in the `plot_env` field of the ggplot object.

  **Value**

  A list of data frames from the `data` argument with a `PANEL` variable
  corresponding to rows in the `layout` field. Also called for the side
  effects of populating fields.

- `train_position`:

  **Description**

  A function method for training position scales and optionally
  initiating them. Implementation is via the `Facet$train_scales()` and
  `Facet$init_scales()` methods.

  **Usage**

      Layout$train_position(data, x_scale, y_scale)

  **Arguments**

  `data`

  :   A list of data frames with layer data.

  `x_scale`,`y_scale`

  :   A single prototype position scale for the `x` and `y` aesthetics
      respectively.

  **Value**

  Nothing, this method is called for the side effect of training scales
  and optionally populating the `panel_scales_x` and `panel_scales_y`
  fields.

- `map_position`:

  **Description**

  A function method for mapping position aesthetics. For discrete scales
  this converts discrete levels to a numeric representation, usually
  integers. For continuous scales, this applies out-of-bounds handling.

  **Usage**

      Layout$map_position(data)

  **Arguments**

  `data`

  :   A list of data frames with layer data.

  **Value**

  A list of data frames per the `data` argument with mapped position
  aesthetics.

- `reset_scales`:

  **Description**

  A function method for resetting scale ranges. After computing stats
  and position adjustments, scales need to be reset and re-trained to
  have an accurate measure of the data limits. This goes through the
  `panel_scales_x` and `panel_scales_y` fields and invokes the
  `Scale$reset()` method.

  **Usage**

      Layout$reset_scales()

  **Value**

  Nothing, it is called for the side-effect of resetting scale ranges.

- `setup_panel_params`:

  **Description**

  A function method for executing `Coord$setup_panel_params()` once per
  panel with the appropriate scales. For efficiency reasons, the setup
  is invoked once per unique combination of `x` and `y` scale.

  **Usage**

      Layout$setup_panel_params()

  **Value**

  Nothing, it is called for the side effect of populating the
  `panel_params` field.

- `setup_panel_guides`:

  **Description**

  A function method for setting up and training the position guides
  (axes) once per panel with the appropriate scales. For efficiency
  reasons, the guides are setup once per unique combination of `x` and
  `y` scale. It calls the `Coord$setup_panel_guides()` and
  `Coord$train_panel_guides()` methods.

  **Usage**

      Layout$setup_panel_guides(guides, layers)

  **Arguments**

  `guides`

  :   A `<Guides>` ggproto object from the `guides` field of the ggplot
      object.

  `layers`

  :   A list of layers from the `layers` field of the ggplot object.

  **Value**

  Nothing, it is called for the side effect of augmenting each entry of
  the `panel_params` field with position guides.

- `setup_panel_guides`:

  **Description**

  A function method for setting up the `Facet$finish_data()` hook.

  **Usage**

      Layout$finish_data(data)

  **Arguments**

  `data`

  :   A list of data frames with layer data.

  **Value**

  A list of data frames with layer data.

- `render`:

  **Description**

  A function method for drawing and assembling the core plot. Mostly it
  delegates tasks to the specific Facet methods for drawing components.

  **Usage**

      Layout$render(panels, data, theme, labels)

  **Arguments**

  `panels`

  :   A list parallel to layers. Each element is another list with grobs
      for each panel, generated by `Layer$draw_geom()`.

  `data`

  :   A list of data frames with layer data.

  `theme`

  :   A [complete
      theme](https://ggplot2.tidyverse.org/reference/complete_theme.md).

  `labels`

  :   A list of labels from the `labels` field of the ggplot object.

  **Value**

  A gtable containing a plot with panels, axes, axis titles and strips.

- `resolve_label`:

  **Description**

  A function method for prying the axis titles from guides, scales or
  plot labels.

  **Usage**

      Layout$resolve_label(scale, labels)

  **Arguments**

  `scale`

  :   A single scale from the `panel_scales_x` or `panel_scales_y`
      fields.

  `labels`

  :   A list of labels from the `labels` field of the ggplot object.

  **Value**

  A named list containing a two titles named `"primary"` and
  `"secondary"`.

- `render_labels`:

  **Description**

  A function method for drawing axis title grobs. The position guides
  themselves do not typically render the axis title grobs as they are
  orchestrated by the layout to draw one title even for multiple axes.

  **Usage**

      Layout$render_labels(labels, theme)

  **Arguments**

  `labels`

  :   A named list containing an `x` list and a `y` list. The `x` and
      `y` lists have `primary` and `secondary` labels. It originates
      from the `Coord$labels()` method.

  `theme`

  :   A [complete
      theme](https://ggplot2.tidyverse.org/reference/complete_theme.md).

  **Value**

  A list with the same structure and names as the `labels` argument, but
  with grobs instead of text.

- `get_scales`:

  **Description**

  A function method for retrieving panel specific scales. It is called
  in the `Stat$compute_layer()` and `Position$compute_layer()` methods.
  The `Geom` uses the `panel_params` field instead of the raw scales.

  **Usage**

      Layout$get_scales(i)

  **Arguments**

  `i`

  :   A scalar integer panel index giving the panel for which to
      retrieve scales

  **Value**

  A named list of scales giving the `x` and `y` scale for the panel.

## See also

Other Layout components:
[`Coord`](https://ggplot2.tidyverse.org/reference/Coord.md),
[`Facet`](https://ggplot2.tidyverse.org/reference/Facet.md)

Other chaperone classes:
[`Layer-class`](https://ggplot2.tidyverse.org/reference/Layer-class.md)

## Examples

``` r
# Some dummy layout components
facet <- facet_null()
coord <- coord_cartesian()

# Use in custom `ggplot_build()` methods
layout <- ggproto(NULL, Layout, facet = facet, coord = coord)
```
