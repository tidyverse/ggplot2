# Guides

The `guide_*` functions (like
[`guide_legend()`](https://ggplot2.tidyverse.org/reference/guide_legend.md))
return `Guide*` objects (like `GuideLegend`). The `Guide*` object is
responsible for rendering the guide for at least one aesthetic.

## Details

Each of the `Guide*` objects is a
[`ggproto()`](https://ggplot2.tidyverse.org/reference/ggproto.md)
object, descended from the top-level `Guide`, and each implements
various methods and fields.

Building a guide has three stages:

1.  The guide extracts the relevant information from scales.

2.  The guide interacts with other parts of the plot, like coords or
    layers to supplement information.

3.  The guide is rendered.

When creating a new Guide class, you may want to consider overriding one
or more of the following:

- The `params`, `elements`, `hashables` and `available_aes` fields.

- The `extract_key()`, `extract_decor()` and `extract_params()` methods.

- The [`transform()`](https://rdrr.io/r/base/transform.html) or
  `get_layer_key()` methods.

- The `setup_params()` and `override_elements()` methods.

- Any of the `build_*` methods.

- As a last resort the `measure_grobs()`, `arrange_layout()`, and
  `assemble_drawing()` methods.

## Fields

- `params`:

  A list of initial parameters that the guide needs to function. The
  base `Guide$params` contains mandatory parameters, but extensions can
  add new parameters. It has the following roles:

  - It provides the default values for parameters.

  - `names(params)` determines what are valid arguments for
    [`new_guide()`](https://ggplot2.tidyverse.org/reference/new_guide.md).

  - During build stages, a mutable copy of `params` holds information
    about the guide.

- `available_aes`:

  A character vector of aesthetic names for which the guide is
  appropriate. Can use keyword `"any"` to indicate all non-position
  aesthetics.

- `elements`:

  A named list of strings stating which theme elements this guide uses.
  By default, strings will be translated in `Guide$setup_elements()`
  using
  [`calc_element()`](https://ggplot2.tidyverse.org/reference/calc_element.md).
  Strings are expected to occur in `names(get_element_tree())`, like
  `"legend.text"` for example. Position guides typically append the
  `{aes}.{position}` suffix in the `setup_elements()` method when the
  position is known.

- `hashables`:

  A list of calls or names constructed by
  [`rlang::exprs()`](https://rlang.r-lib.org/reference/defusing-advanced.html)
  indicating objects in the `params` field. These will be evaluated in
  the context of the `params` field and the resulting list will be
  hashed. The hash uniquely identify guides that can merge. Guides that
  have different hashes will not merge. For extensions, you should
  include objects that clearly mark two guides from one another that
  cannot be merged.

- `train`:

  **Description**

  A function method for orchestrating the training of a guide, which
  extracts necessary information from a Scale object. As orchestrator,
  this method is not intended for extension.

  **Usage**

      Guide$train(params, scale, aesthetic, ...)

  **Arguments**

  `params`

  :   A list of parameters initiated by the `params` field.

  `scale`

  :   A `<Scale>` ggproto object. In the case of position guides, can be
      a `<ViewScale>` ggproto object.

  `aesthetic`

  :   A scalar string specifying the aesthetic. If missing (default), it
      will use the first aesthetic specified in the scale.

  `...`

  :   Additional parameters passed on to the `extract_params()` method.

  **Value**

  A modified list of parameters

- `extract_key`:

  **Description**

  A function method for extracting break information from the scale
  called the 'key'. It retrieves breaks, maps these breaks and derives
  labels. These form the basis for tick marks and labels in some guides.
  It is appropriate to override in extensions.

  **Usage**

      Guide$extract_key(scale, aesthetic, ...)

  **Arguments**

  `scale`

  :   A `<Scale>` ggproto object. In the case of position guides, can be
      a `<ViewScale>` ggproto object.

  `aesthetic`

  :   A scalar string specifying the aesthetic.

  `...`

  :   Optional arguments from the `params` field.

  **Value**

  A 'key' data frame containing annotated scale breaks, including at
  least a column for the aesthetic, `.label` and `.value`. If there are
  no breaks, returns `NULL`.

- `extract_decor`:

  **Description**

  A function method for extracting 'decor' from the scale. The 'decor'
  acts as a wildcard for anything the guide may need to render that is
  not based on the key. For this reason, it has guide specific meaning
  that indicates different things for different guides. In
  [`guide_colourbar()`](https://ggplot2.tidyverse.org/reference/guide_colourbar.md)
  it is the colour gradient, but in
  [`guide_axis()`](https://ggplot2.tidyverse.org/reference/guide_axis.md)
  it is the axis line information. It is appropriate to override in
  extensions.

  **Usage**

      Guide$extract_decor(scale, aesthetic, ...)

  **Arguments**

  `scale`

  :   A `<Scale>` ggproto object. In the case of position guides, can be
      a `<ViewScale>` ggproto object.

  `aesthetic`

  :   A scalar string specifying the aesthetic.

  `...`

  :   Optional arguments from the `params` field.

  **Value**

  Undefined. `NULL` by default.

- `extract_params`:

  **Description**

  A function method for extracting any other information from the scale
  that the guide may need. A typical example is to derive the title from
  the scale, or apply any edits to the `key` or `decor` variables.

  **Usage**

      Geom$extract_params(scale, params, ...)

  **Arguments**

  `scale`

  :   A `<Scale>` ggproto object. In the case of position guides, can be
      a `<ViewScale>` ggproto object.

  `params`

  :   A list of parameters initiated by the `params` field, which at
      this point includes the `key` and `decor` from previous
      extractors.

  `...`

  :   Additional arguments passed from the `train()` method. For
      non-position guides, often includes `title` as derived from the
      `plot$labels` field.

  **Value**

  A modified list of parameters

- `transform`:

  **Description**

  A function method to apply coord transformation and munching to the
  'key' and 'decor' parameters. This method only applies to position
  guides like
  [`guide_axis()`](https://ggplot2.tidyverse.org/reference/guide_axis.md)
  and is not called for non-position guides. It is recommended to
  override this method if you have a position guide that does not
  inherit from `GuideAxis` or has custom key' or 'decor' structures that
  `GuideAxis$transform()` does not handle well.

  **Usage**

      Guide$transform(params, coord, ...)

  **Arguments**

  `params`

  :   A list of parameters initiated by the `params` field.

  `coord`

  :   A `<Coord>` ggproto object.

  `...`

  :   Optional arguments, typically `panel_params` for most position
      guides.

  **Value**

  A list of parameters. The default throws an error.

- `merge`:

  **Description**

  A function method for combining information from two guides. When two
  guides have the same computed `hash` parameter derived from the
  `hashables` field, this function will be called to merge them. If more
  than two guides need to be merged, they are merged successively in a
  [`Reduce()`](https://rdrr.io/r/base/funprog.html)-like fashion.

  Merging guides is the mechanism by which
  [`guide_legend()`](https://ggplot2.tidyverse.org/reference/guide_legend.md)
  can take one guide trained on the `shape` scale, another trained on
  the `colour` scale and display them together in the same guide, for
  example.

  Overriding this method is recommended if the extension descends
  directly from `Guide` and not its children. Otherwise, it should be
  overridden if presented with no superior options.

  **Usage**

      Guide$merge(params, new_guide, new_params)

  **Arguments**

  `params`

  :   A list of parameters derived from the `params` field of this
      guide.

  `new_guide`

  :   A `<Guide>` ggproto object representing the other guide class

  `new_params`

  :   A list of parameters derived from the `params` field of the other
      guide

  **Value**

  A named list containing `guide` and `params`, where `guide` is a
  `<Guide>` ggproto object and `params` is a list with parameters. By
  default, returns the new guide and its parameters.

- `process_layers,get_layer_key`:

  **Description**

  These function methods extract information from layers that the guide
  may need. The `process_layers()` method is tasked with selecting
  layers that are represented by the guide and are to be included. The
  selected layers should be passed on to the `get_layer_key()` method.

  Typical use of these methods is for
  [`guide_legend()`](https://ggplot2.tidyverse.org/reference/guide_legend.md)
  to extract the `Geom$draw_key` function to render glyphs in addition
  to any default or fixed aesthetics. While these methods are called in
  position guides, the `layers` and `data` arguments are empty as these
  are unavailable at that point.

  You can override `get_layer_key()`, but `process_layers()` should
  probably only be overridden if the extension does not inherit from
  `GuideLegend`.

  **Usage**

      Guide$process_layers(params, layers, data, theme)
      Guide$get_layer_key(params, layers, data, theme)

  **Arguments**

  `params`

  :   A list of parameters initiated by the `params` field.

  `layers`

  :   A list of layers from `plot$layers`.

  `data`

  :   A list of layer data frames.

  `theme`

  :   A [completed
      theme](https://ggplot2.tidyverse.org/reference/complete_theme.md)
      object.

  **Value**

  A list of parameters

- `draw`:

  **Description**

  A function method is the main orchestrator for drawing the guide. It
  sets up the final pieces in context of the position, direction and
  theme and. Subsequenty, it renders the individual components like
  titles, ticks, labels and decor. Finally, it arranges these components
  into a guide.

  This method should only be overridden if the extension has non
  standard components that do not fit into 'decor' or when this method
  can be greatly simplified for humble guides. All subsidiaries are fine
  to override.

  **Usage**

      Geom$setup_params(theme, position, direction, params)

  **Arguments**

  `theme`

  :   A [complete
      theme](https://ggplot2.tidyverse.org/reference/complete_theme.md)
      object.

  `position`

  :   A scalar string indicating the position where the guide should be
      drawn. Typically `"top"`, `"right"`, `"bottom"` or `"left"`,
      unless it is a position guide for an exotic coord. Can be `NULL`,
      in which case `params$position` should be used.

  `direction`

  :   A scalar string indicating the legend direction. Can be `NULL`, in
      which case `params$direction` should be used.

  `params`

  :   A list of parameters initiated by the `params` field.

  **Value**

  A grob with the guide.

- `draw_early_exit`:

  **Description**

  A function method that determines what should be drawn when the guide
  'key' is empty. The default method returns
  [`zeroGrob()`](https://ggplot2.tidyverse.org/reference/zeroGrob.md).
  You can override this method if an empty key should draw anything.
  Used in
  [`guide_axis()`](https://ggplot2.tidyverse.org/reference/guide_axis.md)
  to render the `axis.line` part even if no ticks or labels should be
  drawn.

  **Usage**

      Guide$draw_early_exit(params, elements)

  **Arguments**

  `params`

  :   A list of parameters initiated by the `params` field.

  `elements`

  :   A list of elements or resolved theme settings from the
      `override_elements()` method.

  **Value**

  A grob.

- `setup_params`:

  **Description**

  A function method for finalising parameters. Typically used to make
  checks on the `params` object or to make any position or direction
  based adjustments.

  **Usage**

      Guide$setup_params(params)

  **Arguments**

  `params`

  :   A list of parameters initiated by the `params` field.

  **Value**

  A list of parameters

- `setup_elements,override_elements`:

  **Description**

  A function method for resolving required theme elements. The
  `setup_elements()` method joins local guide theme with global theme
  and calculates the necessary theme elements. The `override_elements()`
  method is a hook to edit elements after they've been calculated.

  You can override the `setup_elements()` method if you need more
  complicated theme handling before calculating elements or want to
  intervene in inheritance. For example,
  [`guide_legend()`](https://ggplot2.tidyverse.org/reference/guide_legend.md)
  has special handling of text margins and
  [`guide_axis()`](https://ggplot2.tidyverse.org/reference/guide_axis.md)
  suffixes `{aes}.{position}` to get the theme elements for the correct
  position.

  For other purposes, you can override the `override_elements()` method.

  **Usage**

      Guide$setup_elements(params, elements, theme)
      Guide$override_elements(params, elements, theme)

  **Arguments**

  `params`

  :   A list of parameters initiated by the `params` field.

  `elements`

  :   A named list of strings initiated by the `elements` field.

  `theme`

  :   A [complete
      theme](https://ggplot2.tidyverse.org/reference/complete_theme.md)

  **Value**

  A list of elements or resolved theme settings.

- `build_title`:

  **Description**

  A function method for rendering the title. Note that titles for
  position guides are rendered by the Facet class and not this method.

  You can override this method if you need to render more than one title
  (or none) or adjust margin settings.

  **Usage**

      Guide$build_title(label, elements, params)

  **Arguments**

  `label`

  :   A single string or expression with the title text.

  `elements`

  :   A list of elements or resolved theme settings from the
      `override_elements()` method. The default method expects
      `elements$title` to inherit from the `<element_text>` class.

  `params`

  :   A list of parameters initiated by the `params` field.

  **Value**

  A grob representing the title.

- `build_ticks`:

  **Description**

  A function method for rendering tick marks.

  You can override this function if you don't need ticks or have
  completely different logic on how these should be drawn.

  **Usage**

      Guide$build_ticks(key, elements, params, position, length)

  **Arguments**

  `key`

  :   A data frame with the key information derived from the
      `extract_key()` method.

  `elements`

  :   A list of elements or resolved theme settings from the
      `override_elements()` method. The default method expects
      `elements$ticks` to inherit from the `<element_line>` class and
      `elements$ticks_length` to be a scalar `<unit>` object.

  `params`

  :   A list of parameters initiated by the `params` field.

  `position`

  :   A scalar string indicating the position. Due to historic error
      this works in the opposite way to intuition: if you want ticks for
      an axis at the bottom of a plot, you should use `"top"` here.

  `length`

  :   A scalar `<unit>` object giving the tick length.

  **Value**

  A grob representing tick marks.

- `build_labels`:

  **Description**

  A function method for rendering labels. The default method returns an
  empty grob. It is recommended to override this method when your
  extension directly descends from Guide.

  **Usage**

      Guide$build_labels(key, elements, params)

  **Arguments**

  `key`

  :   A data frame with the key information derived from the
      `extract_key()` method.

  `elements`

  :   A list of elements or resolved theme settings from the
      `override_elements()` method. Most non-default methods expects
      `elements$text` to inherit from the `<element_text>`.

  `params`

  :   A list of parameters initiated by the `params` field.

  **Value**

  A grob representing labels.

- `build_decor`:

  **Description**

  A function method for rendering decor. As the 'wildcard' component,
  this can draw whatever component the guide needs that isn't already
  captured by the key. The default method returns an empty grob. It is
  recommended to override this method.

  For some examples:
  [`guide_legend()`](https://ggplot2.tidyverse.org/reference/guide_legend.md)
  renders the keys with the glyphs,
  [`guide_colourbar()`](https://ggplot2.tidyverse.org/reference/guide_colourbar.md)
  renders the colour gradient rectangle and
  [`guide_axis()`](https://ggplot2.tidyverse.org/reference/guide_axis.md)
  renders the axis line.

  **Usage**

      Guide$build_decor(decor, grobs, elements, params)

  **Arguments**

  `decor`

  :   A data frame (or other structure) with information derived from
      the `extract_decor()` method.

  `grobs`

  :   A list with grobs generated by the other `build_*` methods.

  `elements`

  :   A list of elements or resolved theme settings from the
      `override_elements()` method. Most non-default methods expects
      `elements$text` to inherit from the `<element_text>`.

  `params`

  :   A list of parameters initiated by the `params` field.

  **Value**

  A grob.

- `measure_grobs`:

  **Description**

  A function method for measuring grobs. In preparation for arranging
  grobs, they often need to be measured to determine their widths and
  heights. It is convention that every measurement is converted to
  centimetres. You can override this method if your extension directly
  descends from Guide, or the parent class measurement is defective.

  **Usage**

      Guide$measure_grobs(grobs, params, elements)

  **Arguments**

  `grobs`

  :   A list with grobs generated by the `build_*` methods.

  `params`

  :   A list of parameters initiated by the `params` field.

  `elements`

  :   A list of elements or resolved theme settings from the
      `override_elements()` method.

  **Value**

  A named list or `<unit>` vector giving sizes of components,
  coordinated with `arrange_layout()` and `assemble_drawing()` methods.
  The default method returns `NULL`.

- `arrange_layout`:

  **Description**

  A function method for determining the location or order of grobs in a
  gtable. Typically determines rows and columns where decor and labels
  are placed. Titles are added seperately.You can override this method
  if your extension directly descends from Guide.

  **Usage**

      Guide$arrange_layout(key, sizes, params, elements)

  **Arguments**

  `key`

  :   A data frame with the key information derived from the
      `extract_key()` method.

  `sizes`

  :   A list of `<unit>` vector from the `measure_grobs()` method.

  `params`

  :   A list of parameters initiated by the `params` field.

  `elements`

  :   A list of elements or resolved theme settings from the
      `override_elements()` method.

  **Value**

  Any structure holding placement information coordinated with the
  `assemble_drawing()` method.

- `assemble_drawing`:

  **Description**

  A function method that takes measurements, placement information and
  grobs and assembles these together in a gtable structure. You can
  override this method if your extension directly descends from Guide,
  or the parent class assembly does not work for your guide.

  **Usage**

      Guide$assemble_drawing(grobs, layout, sizes, params, elements)

  **Arguments**

  `grobs`

  :   A list with grobs generated by the `build_*` methods.

  `layout`

  :   A data structure from the `arrange_layout()` method.

  `sizes`

  :   A list of `<unit>` vector from the `measure_grobs()` method.

  `params`

  :   A list of parameters initiated by the `params` field.

  `elements`

  :   A list of elements or resolved theme settings from the
      `override_elements()` method.

  **Value**

  A finished gtable containing the guide.

- `arrange_layout`:

  **Description**

  A function method for placing the title. It is a subsidiary method
  used in the `assemble_drawing()` method for non-position guides.
  Titles are typically added before `legend.margin` is applied. It is
  not recommended to override this method.

  **Usage**

      Guide$add_title(gtable, title, position, just)

  **Arguments**

  `gtable`

  :   An unfinished gtable under construction in the
      `assemble_drawing()` method.

  `title`

  :   The grob resulting from the `build_title()` method.

  `position`

  :   A scaler string, either `"top"`, `"right"`, `"bottom"` or `"left"`
      corresponding to the `legend.title.position`.

  `just`

  :   A named list having `hjust` and `vjust` components with scalar
      numeric values between 0 and 1.

  **Value**

  The `gtable` argument with added title.

## Conventions

The object name that a new class is assigned to is typically the same as
the class name. Guide class names are in UpperCamelCase and start with
the `Guide*` prefix, like `GuideNew`.

A constructor function is usually paired with a Guide class. The
constructor wraps a call to
[`new_guide()`](https://ggplot2.tidyverse.org/reference/new_guide.md),
where e.g. `new_guide(super = GuideNew)`. The constructor name is
formatted by taking the Guide class name and formatting it with
snake_case, so that `GuideNew` becomes `guide_new()`.

## See also

Run
[`vignette("extending-ggplot2")`](https://ggplot2.tidyverse.org/articles/extending-ggplot2.md),
in particular the "Creating new guides" section.

## Examples

``` r
# Extending the class
GuideDescribe <- ggproto(
  "GuideDescribe", Guide,
  # Fields
  elements  = list(text = "legend.text", margin = "legend.margin"),
  hashables = rlang::exprs(key$.label),

  # Methods
  build_title = function(...) zeroGrob(), # Turn off title

  build_labels = function(key, elements, params) {
    labels <- key$.label
    n <- length(labels)
    labels <- paste0(paste0(labels[-n], collapse = ", "), ", and ", labels[n])
    labels <- paste0("A guide showing ", labels, " categories")
    element_grob(elements$text, label = labels, margin_x = TRUE, margin_y = TRUE)
  },

  measure_grobs = function(grobs, params, elements) {
    # Measuring in centimetres is the convention
    width  <- grid::convertWidth(grid::grobWidth(grobs$labels), "cm", valueOnly = TRUE)
    height <- grid::convertHeight(grid::grobHeight(grobs$labels), "cm", valueOnly = TRUE)
    list(width = unit(width, "cm"), height = unit(height, "cm"))
  },

  assemble_drawing = function(self, grobs, layout, sizes, params, elements) {
    gt <- gtable::as.gtable(grobs$labels, width = sizes$width, height = sizes$height)
    gt <- gtable::gtable_add_padding(gt, elements$margin)
    gt
  }
)

# Building a constructor
guide_describe <- function(position = NULL) {
  new_guide(position = position, super = GuideDescribe)
}

# Use new guide plot
ggplot(mpg, aes(displ, hwy, colour = class)) +
  geom_point() +
  guides(colour = guide_describe("bottom"))
```
