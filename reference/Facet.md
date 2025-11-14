# Facets

All `facet_*()` functions returns a `Facet` object or an object of a
`Facet` subclass. This object describes how to assign data to different
panels, how to apply positional scales and how to lay out the panels,
once rendered.

## Details

Extending facets can range from the simple modifications of current
facets, to very laborious rewrites with a lot of
[`gtable()`](https://gtable.r-lib.org/reference/gtable.html)
manipulation.For some examples of both, please see the extension
vignette. The object and its parameters are chaperoned by the
[Layout](https://ggplot2.tidyverse.org/reference/Layout.md) class.

`Facet` subclasses, like other extendible ggproto classes, have a range
of methods that can be modified. Some of these are required for all new
subclasses, while other only need to be modified if need arises.

The required methods are:

- `compute_layout`

- [`map_data()`](https://ggplot2.tidyverse.org/reference/map_data.md)

- `draw_panels()` or its subsidiaries:

  - `init_gtable()`

  - `attach_axes()`

  - `attach_strips()`

In addition to the methods above, it can be useful to override the
default behaviour of one or more of the following methods:

- `setup_params()`

- `init_scales()`

- `train_scale()`

- `finish_data()`

- `draw_back()`, `draw_front()` or `draw_labels()`

All extension methods receive the content of the params field as the
params argument, so the constructor function will generally put all
relevant information into this field.

## Fields

- `shink`:

  A scalar boolean which when `TRUE`, will shrink scales to fit output
  statistics rather than raw data. If `FALSE`, will only include raw
  data before statistical summary. By exception this is not part of the
  `params` field.

- `params`:

  A named list of parameters populated by the constructor function.

- `setup_params`:

  **Description**

  A function method for modifying or checking the parameters based on
  the data. The default method includes a `.possible_columns` variable
  giving column names.

  **Usage**

      Facet$setup_params(data, params)

  **Arguments**

  `data`

  :   A list of data frames. The first item is the global data, which is
      followed by layer data in subsequent items.

  `params`

  :   A list of current parameters.

  **Value**

  A list of parameters

- `setup_data`:

  **Description**

  A function method for modifying or checking the data prior to adding
  defaults. The default method returns data unaltered.

  **Usage**

      Facet$setup_data(data, params)

  **Arguments**

  `data`

  :   A list of data frames. The first item is the global data, which is
      followed by layer data in subsequent items.

  `params`

  :   A list of parameters coming from the `setup_params()` method.

  **Value**

  A list of data frames of the same length as the `data` argument

- `compute_layout`:

  **Description**

  A function method for creating the correspondence between faceting
  variable levels, panels and position scales. It places panels like
  cells in a matrix.

  **Usage**

      Facet$compute_layout(data, params)

  **Arguments**

  `data`

  :   A list of data frames. The first item is the global data, which is
      followed by layer data in subsequent items.

  `params`

  :   A list of parameters coming from the `setup_params()` method.

  **Value**

  A data frame with 1 row per panel, containing at least integer columns
  `ROW`, `COL`, `PANEL`, `SCALE_X` and `SCALE_Y`. Can contain additional
  information in terms of columns, typically faceting variables.

- `map_data`:

  **Description**

  A function method for to create the `PANEL` variable in layer data.
  The `PANEL` variable is a special variable that tracks the
  relationship between rows in the layer data and the panels described
  in the `layout` input.

  In addition, \#' this function may copy or discard rows as needed, for
  example when adding margins in FacetGrid.

  **Usage**

      Facet$map_data(data, layout, params)

  **Arguments**

  `data`

  :   A list of data frames containing layer data.

  `layout`

  :   A data frame computed by the `compute_layout()` method. Typically
      contains the faceting variables, `ROW`, `COL`, `PANEL`, `SCALE_X`
      and `SCALE_Y` variables.

  `params`

  :   A list of parameters coming from the `setup_params()` method.

  **Value**

  A list of data frames containing layer data including a `PANEL`
  variable.

- `init_scales`:

  **Description**

  A function method for initialising position scales. Given a prototype
  scale for `x` and `y`, creates layout specific scales to accommodate
  the relationships between panels and scales. By default, the prototype
  scales are cloned for each `SCALE_X` and `SCALE_Y` level. The function
  is called separately; once for `x` and once for `y`.

  **Usage**

      Facet$init_scales(layout, x_scale, y_scale, params)

  **Arguments**

  `layout`

  :   A data frame computed by the `compute_layout()` method. Typically
      contains the faceting variables, `ROW`, `COL`, `PANEL`, `SCALE_X`
      and `SCALE_Y` variables.

  `x_scale`,`y_scale`

  :   A position scale for the `x` and `y` aesthetics respectively.

  `params`

  :   A list of parameters coming from the `setup_params()` method.

  **Value**

  A named list with `x` and `y` elements containing a list of panel
  scales for each `SCALE_X` and/or `SCALE_Y` level respectively.

- `train_scales`:

  **Description**

  A function method for training position scales. The default trains
  each scale on the data related to its panels.

  **Usage**

      Facet$train_scales(x_scales, y_scales, layout, data, params)

  **Arguments**

  `x_scales`,`y_scales`

  :   A list of panel scales for each `SCALE_X` and `SCALE_Y` level
      respectively.

  `layout`

  :   A data frame computed by the `compute_layout()` method. Typically
      contains the faceting variables, `ROW`, `COL`, `PANEL`, `SCALE_X`
      and `SCALE_Y` variables.

  `data`

  :   A list of data frames containing layer data.

  `params`

  :   A list of parameters coming from the `setup_params()` method.

  **Value**

  Nothing, this method is called for its side-effect of training the
  scales.

- `setup_panel_params`:

  **Description**

  A function method as a hook to give facets input over panel
  parameters. By default, returns panel parameters unaltered.

  **Usage**

      Facet$setup_panel_params(panel_params, coord, ...)

  **Arguments**

  `panel_params`

  :   A named list of view scales, ranges and other optional parameters
      from `Coord$setup_panel_params()`.

  `coord`

  :   A `<Coord>` ggproto object.

  `...`

  :   Currently not in use. For future expansion.

  **Value**

  A list of panel parameters.

- `finish_data`:

  **Description**

  A function method as a hook for making last-minute modifications to
  layer data before it is rendered by Geoms. The default is to not
  modify the data.

  **Usage**

      Facet$finish_data(data, layout, x_scales, y_scales, params)

  **Arguments**

  `data`

  :   A data frame containing layer data of a single layer.

  `layout`

  :   A data frame computed by the `compute_layout()` method. Typically
      contains the faceting variables, `ROW`, `COL`, `PANEL`, `SCALE_X`
      and `SCALE_Y` variables.

  `x_scales`,`y_scales`

  :   A list of panel scales for each `SCALE_X` and `SCALE_Y` level
      respectively.

  `params`

  :   A list of parameters coming from the `setup_params()` method.

  **Value**

  A data frame containing layer data.

- `draw_panel_content`:

  **Description**

  A function method to assemble the panel contents. It delegates the
  `draw_back()` and `draw_front()` methods, as well as
  `Coord$draw_panel()`.

  **Usage**

      Facet$draw_panel_content(
        panels,
        layout,
        x_scales,
        y_scales,
        ranges,
        coord,
        theme,
        params,
        ...
      )

  **Arguments**

  `panels`

  :   A list parallel to layers. Each element is another list with grobs
      for each panel, generated by `Layer$draw_geom()`.

  `layout`

  :   A data frame computed by the `compute_layout()` method. Typically
      contains the faceting variables, `ROW`, `COL`, `PANEL`, `SCALE_X`
      and `SCALE_Y` variables.

  `x_scales`,`y_scales`

  :   A list of panel scales for each `SCALE_X` and `SCALE_Y` level
      respectively.

  `ranges`

  :   A list of panel parameters from the `setup_panel_params()`
      augmented with position guides.

  `coord`

  :   A `<Coord>` ggproto object.

  `data`

  :   A list of data frames containing layer data.

  `theme`

  :   A [complete
      theme](https://ggplot2.tidyverse.org/reference/complete_theme.md)
      object.

  `params`

  :   A list of parameters coming from the `setup_params()` method.

  `...`

  :   Currently not in use.

  **Value**

  A list of grobs, one for each level of the `PANEL` layout variable.
  Grob can be
  [`zeroGrob()`](https://ggplot2.tidyverse.org/reference/zeroGrob.md) to
  draw nothing.

- `draw_back,draw_front`:

  **Description**

  A function method draw facet background (back) and foreground (front)
  for panels. The front and back will sandwich the grobs created by
  layers. The default methods draw nothing.

  **Usage**

      Facet$draw_back(data, layout, x_scales, y_scales, theme, params)
      Facet$draw_front(data, layout, x_scales, y_scales, theme, params)

  **Arguments**

  `data`

  :   A list of data frames containing layer data.

  `layout`

  :   A data frame computed by the `compute_layout()` method. Typically
      contains the faceting variables, `ROW`, `COL`, `PANEL`, `SCALE_X`
      and `SCALE_Y` variables.

  `x_scales`,`y_scales`

  :   A list of panel scales for each `SCALE_X` and `SCALE_Y` level
      respectively.

  `theme`

  :   A [complete
      theme](https://ggplot2.tidyverse.org/reference/complete_theme.md)
      object.

  `params`

  :   A list of parameters coming from the `setup_params()` method.

  **Value**

  A list of grobs, one for each level of the `PANEL` layout variable.
  Grob can be
  [`zeroGrob()`](https://ggplot2.tidyverse.org/reference/zeroGrob.md) to
  draw nothing.

- `draw_panels`:

  **Description**

  A function method that orchestrates the majority of facet drawing. It
  is responsible for assembling a gtable with panel content decorated
  with axes and strips. The resulting gtable is the basis for the plot
  in its entirety. It delegates these tasks to the `init_gtable()`,
  `attach_axes()` and `attach_strips()` methods.

  **Usage**

      Facet$draw_panels(
        panels,
        layout,
        x_scales,
        y_scales,
        ranges,
        coord,
        data,
        theme,
        params
      )

  **Arguments**

  `panels`

  :   A list of grobs, one per panel.

  `layout`

  :   A data frame computed by the `compute_layout()` method. Typically
      contains the faceting variables, `ROW`, `COL`, `PANEL`, `SCALE_X`
      and `SCALE_Y` variables.

  `x_scales`,`y_scales`

  :   A list of panel scales for each `SCALE_X` and `SCALE_Y` level
      respectively.

  `ranges`

  :   A list of panel parameters from the `setup_panel_params()`
      augmented with position guides.

  `coord`

  :   A `<Coord>` ggproto object.

  `data`

  :   A list of data frames containing layer data.

  `theme`

  :   A [complete
      theme](https://ggplot2.tidyverse.org/reference/complete_theme.md)
      object.

  `params`

  :   A list of parameters coming from the `setup_params()` method.

  **Value**

  A [`gtable`](https://gtable.r-lib.org/reference/gtable.html) object.

- `init_gtable`:

  **Description**

  A function method that initiates a gtable object containing panels set
  at the appropriate `ROW` and `COL` cells from the layout. The panels
  are separated by the `panel.spacing.{x/y}` spacing.

  **Usage**

      Facet$init_gtable(panels, layout, theme, ranges, params, aspect_ratio)

  **Arguments**

  `panels`

  :   A list of grobs, one per panel.

  `layout`

  :   A data frame computed by the `compute_layout()` method. Typically
      contains the faceting variables, `ROW`, `COL`, `PANEL`, `SCALE_X`
      and `SCALE_Y` variables.

  `theme`

  :   A [complete
      theme](https://ggplot2.tidyverse.org/reference/complete_theme.md)
      object.

  `ranges`

  :   A list of panel parameters from the `setup_panel_params()`
      augmented with position guides.

  `aspect_ratio`

  :   A scalar numeric for the panel aspect ratio or `NULL` for no
      aspect ratio.

  **Value**

  A [`gtable`](https://gtable.r-lib.org/reference/gtable.html) object
  containing panel grobs prefixed with `"panel"`.

- `attach_axes`:

  **Description**

  A function method that renders position guides (axes) and attaches
  these to the gtable with panels. The default method returns the gtable
  unaltered.

  **Usage**

      Facet$attach_axes(table, layout, ranges, coord, theme, params)

  **Arguments**

  `table`

  :   A [`gtable`](https://gtable.r-lib.org/reference/gtable.html)
      object populated with panels from the `init_gtable()` method.

  `layout`

  :   A data frame computed by the `compute_layout()` method. Typically
      contains the faceting variables, `ROW`, `COL`, `PANEL`, `SCALE_X`
      and `SCALE_Y` variables.

  `ranges`

  :   A list of panel parameters from the `setup_panel_params()`
      augmented with position guides.

  `coord`

  :   A `<Coord>` ggproto object.

  `theme`

  :   A [complete
      theme](https://ggplot2.tidyverse.org/reference/complete_theme.md)
      object.

  `params`

  :   A list of parameters coming from the `setup_params()` method.

  **Value**

  A [`gtable`](https://gtable.r-lib.org/reference/gtable.html) object.

- `attach_strips`:

  **Description**

  A function method that renders strips and attaches these to the gtable
  with panels and axes. The `format_strip_labels()` method is used to
  format the strip text. The default method returns the gtable
  unaltered.

  **Usage**

      Facet$attach_strips(table, layout, ranges, coord, theme, params)

  **Arguments**

  `table`

  :   A [`gtable`](https://gtable.r-lib.org/reference/gtable.html)
      object from the `attach_axes()` method.

  `layout`

  :   A data frame computed by the `compute_layout()` method. Typically
      contains the faceting variables, `ROW`, `COL`, `PANEL`, `SCALE_X`
      and `SCALE_Y` variables.

  `params`

  :   A list of parameters coming from the `setup_params()` method.

  `theme`

  :   A [complete
      theme](https://ggplot2.tidyverse.org/reference/complete_theme.md)
      object.

  **Value**

  A [`gtable`](https://gtable.r-lib.org/reference/gtable.html) object.

- `format_strip_labels`:

  **Description**

  A function method that formats the text for strips. It is used in the
  `attach_strips` methods, but also the
  [`get_strip_labels()`](https://ggplot2.tidyverse.org/reference/get_strip_labels.md)
  function. The default method returns `NULL`.

  **Usage**

      Facet$format_strip_labels(layout, params)

  **Arguments**

  `layout`

  :   A data frame computed by the `compute_layout()` method. Typically
      contains the faceting variables, `ROW`, `COL`, `PANEL`, `SCALE_X`
      and `SCALE_Y` variables.

  `params`

  :   A list of parameters coming from the `setup_params()` method.

  **Value**

  A list containing a data frame with strip labels.

- `set_panel_size`:

  **Description**

  A function method that enforces the `panel.widths` and `panel.heights`
  theme settings.

  **Usage**

      Facet$set_panel_size(table, theme)

  **Arguments**

  `table`

  :   A [`gtable`](https://gtable.r-lib.org/reference/gtable.html)
      object populated by the `draw_panels()` method.

  `theme`

  :   A [complete
      theme](https://ggplot2.tidyverse.org/reference/complete_theme.md)
      object.

  **Value**

  The `table` object, optionally with different `widths` and `heights`
  properties.

- `attach_axes`:

  **Description**

  A function method that renders axis titles and adds them to the
  gtable. The default is to add one title at each side depending on the
  position and presence of axes.

  **Usage**

      Facet$draw_labels(
        panels,
        layout,
        x_scales,
        y_scales,
        ranges,
        coord,
        data,
        theme,
        labels,
        params
      )

  **Arguments**

  `panels`

  :   A [`gtable`](https://gtable.r-lib.org/reference/gtable.html)
      object initiated by the `draw_panels()` method.

  `layout`

  :   A data frame computed by the `compute_layout()` method. Typically
      contains the faceting variables, `ROW`, `COL`, `PANEL`, `SCALE_X`
      and `SCALE_Y` variables.

  `x_scales`,`y_scales`

  :   A list of panel scales for each `SCALE_X` and `SCALE_Y` level
      respectively.

  `ranges`

  :   A list of panel parameters from the `setup_panel_params()`
      augmented with position guides.

  `coord`

  :   A `<Coord>` ggproto object.

  `data`

  :   A list of data frames containing layer data.

  `theme`

  :   A [complete
      theme](https://ggplot2.tidyverse.org/reference/complete_theme.md)
      object.

  `labels`

  :   A named list containing an `x` list and `y` list. The `x` and `y`
      lists have `primary` and `secondary` labels.

  `params`

  :   A list of parameters coming from the `setup_params()` method.

  **Value**

  A [`gtable`](https://gtable.r-lib.org/reference/gtable.html) object.

- `vars`:

  **Description**

  A function method that returns the names of faceting variables. The
  default method returns an character vector with 0 length.

  **Usage**

      Facet$vars()

  **Value**

  A character vector

## Conventions

The object name that a new class is assigned to is typically the same as
the class name. Facet class names are in UpperCamelCase and start with
the `Facet*` prefix, like `FacetNew`.

A constructor function is usually paired with a Facet class. The
constructor copies the facet class and populates the `params` field. The
constructor function name should take the Facet class name and be
formatted with snake_case, so that `FacetNew` becomes `facet_new()`.

## See also

The the [new facets
section](https://ggplot2-book.org/extensions#new-facets) of the online
ggplot2 book.

Run
[`vignette("extending-ggplot2")`](https://ggplot2.tidyverse.org/articles/extending-ggplot2.md),
in particular the "Creating a new faceting" section.

Other Layout components:
[`Coord`](https://ggplot2.tidyverse.org/reference/Coord.md),
[`Layout`](https://ggplot2.tidyverse.org/reference/Layout.md)

## Examples

``` r
# Please see extension vignette
NULL
#> NULL
```
