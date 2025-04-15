#' @include theme-elements.R
NULL

#' Guide constructor
#'
#' A constructor function for guides, which performs some standard compatibility
#' checks between the guide and provided arguments.
#'
#' @param ... Named arguments that match the parameters of `super$params` or
#'   the theme elements in `super$elements`.
#' @param available_aes A vector of character strings listing the aesthetics
#'   for which the guide can be drawn.
#' @param super The super class to use for the constructed guide. Should be a
#'   Guide class object.
#'
#' @return A `Guide` ggproto object.
#' @keywords internal
#' @export
new_guide <- function(..., available_aes = "any", super) {

  pf <- parent.frame()
  super <- validate_subclass(super, "Guide", env = pf)

  args <- list2(...)

  # Set parameters
  param_names <- names(super$params)
  params <- intersect(names(args), param_names)
  params <- defaults(args[params], super$params)

  # Warn about extra arguments
  extra_args <- setdiff(names(args), param_names)
  if (length(extra_args) > 0) {
    cli::cli_warn(paste0(
      "Ignoring unknown {cli::qty(extra_args)} argument{?s} to ",
      "{.fn {snake_class(super)}}: {.arg {extra_args}}."
    ))
  }

  # Stop when some required parameters are missing.
  # This should only happen with mis-constructed guides
  required_params <- names(Guide$params)
  missing_params  <- setdiff(required_params, names(params))
  if (length(missing_params) > 0) {
    cli::cli_abort(paste0(
      "The following parameter{?s} {?is/are} required for setting up a guide, ",
      "but {?is/are} missing: {.field {missing_params}}"
    ))
  }

  # Validate theme settings
  if (!is.null(params$theme)) {
    check_object(params$theme, is_theme, what = "a {.cls theme} object")
    check_theme(params$theme, call = caller_env())
    params$direction <- params$direction %||% params$theme$legend.direction
  }

  # Ensure 'order' is length 1 integer
  params$order <- vec_cast(params$order, 0L, x_arg = "order", call = pf)
  vec_assert(params$order, 0L, size = 1L, arg = "order", call = pf)

  ggproto(
    NULL, super,
    params = params,
    available_aes = available_aes
  )
}

#' @export
#' @rdname is_tests
is_guide <- function(x) inherits(x, "Guide")

#' Guides
#'
#' @description
#' The `guide_*` functions (like `guide_legend()`) return `Guide*` objects
#' (like `GuideLegend`). The `Guide*` object is responsible for rendering the
#' guide for at least one aesthetic.
#'
#' @details
#' Each of the `Guide*` objects is a [ggproto()] object, descended from the
#' top-level `Guide`, and each implements various methods and fields.
#'
#' Building a guide has three stages:
#' 1. The guide extracts the relevant information from scales.
#' 2. The guide interacts with other parts of the plot, like coords or layers to
#'    supplement information.
#' 3. The guide is rendered.
#'
#' When creating a new Guide class, you may want to consider overriding one or
#' more of the following:
#'
#' * The `params`, `elements`, `hashables` and `available_aes` fields.
#' * The `extract_key()`, `extract_decor()` and `extract_params()` methods.
#' * The `transform()` or `get_layer_key()` methods.
#' * The `setup_params()` and `override_elements()` methods.
#' * Any of the `build_*` methods.
#' * As a last resort the `measure_grobs()`, `arrange_layout()`, and
#'   `assemble_drawing()` methods.
#'
#' @section Conventions:
#'
#' The object name that a new class is assigned to is typically the same as the
#' class name. Guide class names are in UpperCamelCase and start with the
#' `Guide*` prefix, like `GuideNew`.
#'
#' A constructor function is usually paired with a Guide class. The constructor
#' wraps a call to `new_guide()`, where e.g. `new_guide(super = GuideNew)`. The
#' constructor name is formatted by taking the Guide class name and formatting
#' it with snake_case, so that `GuideNew` becomes `guide_new()`.
#'
#' @export
#' @format NULL
#' @usage NULL
#' @seealso Run `vignette("extending-ggplot2")`, in particular the "Creating
#' new guides" section.
#' @examples
#' # Extending the class
#' GuideDescribe <- ggproto(
#'   "GuideDescribe", Guide,
#'   # Fields
#'   elements  = list(text = "legend.text", margin = "legend.margin"),
#'   hashables = rlang::exprs(key$.label),
#'
#'   # Methods
#'   build_title = function(...) zeroGrob(), # Turn off title
#'
#'   build_labels = function(key, elements, params) {
#'     labels <- key$.label
#'     n <- length(labels)
#'     labels <- paste0(paste0(labels[-n], collapse = ", "), ", and ", labels[n])
#'     labels <- paste0("A guide showing ", labels, " categories")
#'     element_grob(elements$text, label = labels, margin_x = TRUE, margin_y = TRUE)
#'   },
#'
#'   measure_grobs = function(grobs, params, elements) {
#'     # Measuring in centimetres is the convention
#'     width  <- grid::convertWidth(grid::grobWidth(grobs$labels), "cm", valueOnly = TRUE)
#'     height <- grid::convertHeight(grid::grobHeight(grobs$labels), "cm", valueOnly = TRUE)
#'     list(width = unit(width, "cm"), height = unit(height, "cm"))
#'   },
#'
#'   assemble_drawing = function(self, grobs, layout, sizes, params, elements) {
#'     gt <- gtable::as.gtable(grobs$labels, width = sizes$width, height = sizes$height)
#'     gt <- gtable::gtable_add_padding(gt, elements$margin)
#'     gt
#'   }
#' )
#'
#' # Building a constructor
#' guide_describe <- function(position = NULL) {
#'   new_guide(position = position, super = GuideDescribe)
#' }
#'
#' # Use new guide plot
#' ggplot(mpg, aes(displ, hwy, colour = class)) +
#'   geom_point() +
#'   guides(colour = guide_describe("bottom"))
Guide <- ggproto(
  "Guide",

  # Fields ------------------------------------------------------------------

  #' @field params A list of initial parameters that the guide needs to
  #' function. The base `Guide$params` contains mandatory parameters,
  #' but extensions can  add new parameters. It has the following roles:
  #' * It provides the default values for parameters.
  #' * `names(params)` determines what are valid arguments for `new_guide()`.
  #' * During build stages, a mutable copy of `params` holds information
  #'   about the guide.
  params = list(
    title     = waiver(),
    theme     = NULL,
    name      = character(),
    position  = waiver(),
    direction = NULL,
    order     = 0,
    hash      = character()
  ),

  #' @field available_aes A character vector of aesthetic names for which the
  #' guide is appropriate. Can use keyword `"any"` to indicate all non-position
  #' aesthetics.
  available_aes = character(),

  #' @field elements A named list of strings stating which theme elements this
  #' guide uses. By default, strings will be translated in
  #' `Guide$setup_elements()` using `calc_element()`. Strings are expected to
  #' occur in `names(get_element_tree())`, like `"legend.text"` for example.
  #' Position guides typically append the  `{aes}.{position}` suffix in the
  #' `setup_elements()` method when the position is known.
  elements = list(),

  #' @field hashables A list of calls or names constructed by `rlang::exprs()`
  #' indicating objects in the `params` field. These will be evaluated in the
  #' context of the `params` field and the resulting list will be hashed. The
  #' hash uniquely identify guides that can merge. Guides that have different
  #' hashes will not merge. For extensions, you should include objects that
  #' clearly mark two guides from one another that cannot be merged.
  hashables = exprs(title, name),

  # Methods -----------------------------------------------------------------

  ## Training ---------------------------------------------------------------

  #' @field train
  #' **Description**
  #'
  #' A function method for orchestrating the training of a guide, which extracts
  #' necessary information from a Scale object. As orchestrator, this method is
  #' not intended for extension.
  #'
  #' **Usage**
  #' ```r
  #' Guide$train(params, scale, aesthetic, ...)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`params`}{A list of parameters initiated by the `params` field.}
  #'   \item{`scale`}{A `<Scale>` ggproto object. In the case of position
  #'   guides, can be a `<ViewScale>` ggproto object.}
  #'   \item{`aesthetic`}{A scalar string specifying the aesthetic.
  #'   If missing (default), it will use the first aesthetic specified in the
  #'   scale.}
  #'   \item{`...`}{Additional parameters passed on to the `extract_params()`
  #'   method.}
  #' }
  #'
  #' **Value**
  #'
  #' A modified list of parameters
  train = function(self, params = self$params, scale, aesthetic = NULL, ...) {
    params$aesthetic <- aesthetic %||% scale$aesthetics[1]
    params$key   <- inject(self$extract_key(scale, !!!params))
    if (is.null(params$key)) {
      return(NULL)
    }
    params$decor <- inject(self$extract_decor(scale, !!!params))
    params <- self$extract_params(scale, params, ...)
    # Make hash
    # TODO: Maybe we only need the hash on demand during merging?
    params$hash <- hash(lapply(unname(self$hashables), eval_tidy, data = params))
    params
  },

  #' @field extract_key
  #' **Description**
  #'
  #' A function method for extracting break information from the scale called
  #' the 'key'. It retrieves breaks, maps these breaks and derives labels. These
  #' form the basis for tick marks and labels in some guides. It is appropriate
  #' to override in extensions.
  #'
  #' **Usage**
  #' ```r
  #' Guide$extract_key(scale, aesthetic, ...)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`scale`}{A `<Scale>` ggproto object. In the case of position
  #'   guides, can be a `<ViewScale>` ggproto object.}
  #'   \item{`aesthetic`}{A scalar string specifying the aesthetic.}
  #'   \item{`...`}{Optional arguments from the `params` field.}
  #' }
  #'
  #' **Value**
  #'
  #' A 'key' data frame containing annotated scale breaks, including at least a
  #' column for the aesthetic, `.label` and `.value`. If there are no breaks,
  #' returns `NULL`.
  extract_key = function(scale, aesthetic, ...) {
    breaks <- scale$get_breaks()
    if (length(breaks) == 0) {
      return(NULL)
    }

    mapped <- scale$map(breaks)
    labels <- scale$get_labels(breaks)

    key <- data_frame(!!aesthetic := mapped)
    key$.value <- breaks
    key$.label <- labels

    if (is.numeric(breaks)) {
      range <- scale$continuous_range %||% scale$get_limits()
      key <- vec_slice(key, is.finite(oob_censor_any(breaks, range)))
    } else {
      key
    }
  },

  #' @field extract_decor
  #' **Description**
  #'
  #' A function method for extracting 'decor' from the scale. The 'decor' acts
  #' as a wildcard for anything the guide may need to render that is not based
  #' on the key. For this reason, it has guide specific meaning that indicates
  #' different things for different guides. In `guide_colourbar()` it is the
  #' colour gradient, but in `guide_axis()` it is the axis line information.
  #' It is appropriate to override in extensions.
  #'
  #' **Usage**
  #' ```r
  #' Guide$extract_decor(scale, aesthetic, ...)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`scale`}{A `<Scale>` ggproto object. In the case of position
  #'   guides, can be a `<ViewScale>` ggproto object.}
  #'   \item{`aesthetic`}{A scalar string specifying the aesthetic.}
  #'   \item{`...`}{Optional arguments from the `params` field.}
  #' }
  #'
  #' **Value**
  #'
  #' Undefined. `NULL` by default.
  extract_decor = function(scale, aesthetic, ...) {
    return(invisible()) # By default, nothing else needs to be extracted
  },

  #' @field extract_params
  #' **Description**
  #'
  #' A function method for extracting any other information from the scale that
  #' the guide may need. A typical example is to derive the title from the scale,
  #' or apply any edits to the `key` or `decor` variables.
  #'
  #' **Usage**
  #' ```r
  #' Geom$extract_params(scale, params, ...)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`scale`}{A `<Scale>` ggproto object. In the case of position
  #'   guides, can be a `<ViewScale>` ggproto object.}
  #'   \item{`params`}{A list of parameters initiated by the `params` field,
  #'   which at this point includes the `key` and `decor` from previous
  #'   extractors.}
  #'   \item{`...`}{Additional arguments passed from the `train()` method. For
  #'   non-position guides, often includes `title` as derived from the
  #'   `plot$labels` field.}
  #' }
  #'
  #' **Value**
  #'
  #' A modified list of parameters
  extract_params = function(scale, params, ...) {
    params
  },

  ## Intermediate steps ------------------------------------------------------

  #' @field transform
  #' **Description**
  #'
  #' A function method to apply coord transformation and munching to the
  #' 'key' and 'decor' parameters. This method only applies to position guides
  #' like `guide_axis()` and is not called for non-position guides. It is
  #' recommended to override this method if you have a position guide that
  #' does not inherit from `GuideAxis` or has custom key' or 'decor' structures
  #' that `GuideAxis$transform()` does not handle well.
  #'
  #' **Usage**
  #' ```r
  #' Guide$transform(params, coord, ...)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`params`}{A list of parameters initiated by the `params` field.}
  #'   \item{`coord`}{A `<Coord>` ggproto object.}
  #'   \item{`...`}{Optional arguments, typically `panel_params` for most
  #'   position guides.}
  #' }
  #'
  #' **Value**
  #'
  #' A list of parameters. The default throws an error.
  transform = function(self, params, coord, ...) {
    cli::cli_abort(c(
      "{.fn {snake_class(self)}} does not implement a {.fn transform} method.",
      "i" = "Did you mean to use {.fn guide_axis}?"
    ))
  },

  #' @field merge
  #' **Description**
  #'
  #' A function method for combining information from two guides. When two
  #' guides have the same computed `hash` parameter derived from the `hashables`
  #' field, this function will be called to merge them. If more than two guides
  #' need to be merged, they are merged successively in a `Reduce()`-like
  #' fashion.
  #'
  #' Merging guides is the mechanism by which `guide_legend()` can take one
  #' guide trained on the `shape` scale, another trained on the `colour` scale
  #' and display them together in the same guide, for example.
  #'
  #' Overriding this method is recommended if the extension descends directly
  #' from `Guide` and not its children. Otherwise, it should be overridden if
  #' presented with no superior options.
  #'
  #' **Usage**
  #' ```r
  #' Guide$merge(params, new_guide, new_params)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`params`}{A list of parameters derived from the `params` field of
  #'   this guide.}
  #'   \item{`new_guide`}{A `<Guide>` ggproto object representing the other guide class}
  #'   \item{`new_params`}{A list of parameters derived from the `params` field
  #'   of the other guide}
  #' }
  #'
  #' **Value**
  #'
  #' A named list containing `guide` and `params`, where `guide` is a `<Guide>`
  #' ggproto object and `params` is a list with parameters. By default, returns
  #' the new guide and its parameters.
  merge = function(self, params, new_guide, new_params) {
    return(list(guide = new_guide, params = new_params))
  },

  #' @field process_layers,get_layer_key
  #' **Description**
  #'
  #' These function methods extract information from layers that the guide may
  #' need. The `process_layers()` method is tasked with selecting layers that
  #' are represented by the guide and are to be included. The selected layers
  #' should be passed on to the `get_layer_key()` method.
  #'
  #' Typical use of these methods is for `guide_legend()` to extract the
  #' `Geom$draw_key` function to render glyphs in addition to any default or
  #' fixed aesthetics. While these methods are called in position guides,
  #' the `layers` and `data` arguments are empty as these are unavailable at
  #' that point.
  #'
  #' You can override `get_layer_key()`, but `process_layers()` should
  #' probably only be overridden if the extension does not inherit from
  #' `GuideLegend`.
  #'
  #' **Usage**
  #' ```r
  #' Guide$process_layers(params, layers, data, theme)
  #' Guide$get_layer_key(params, layers, data, theme)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`params`}{A list of parameters initiated by the `params` field.}
  #'   \item{`layers`}{A list of layers from `plot$layers`.}
  #'   \item{`data`}{A list of layer data frames.}
  #'   \item{`theme`}{A [completed theme][complete_theme()] object.}
  #' }
  #'
  #' **Value**
  #'
  #' A list of parameters
  process_layers = function(self, params, layers, data = NULL, theme = NULL) {
    # TODO: should we just not call this method for position guides?
    self$get_layer_key(params, layers, data, theme)
  },

  get_layer_key = function(params, layers, data = NULL, theme = NULL) {
    return(params)
  },

  ## Drawing -----------------------------------------------------------------

  #' @field draw
  #' **Description**
  #'
  #' A function method is the main orchestrator for drawing the guide. It sets
  #' up the final pieces in context of the position, direction and theme
  #' and. Subsequenty, it renders the individual components like titles, ticks,
  #' labels and decor. Finally, it arranges these components into a guide.
  #'
  #' This method should only be overridden if the extension has non standard
  #' components that do not fit into 'decor' or when this method can be greatly
  #' simplified for humble guides. All subsidiaries are fine to override.
  #'
  #' **Usage**
  #' ```r
  #' Geom$setup_params(theme, position, direction, params)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`theme`}{A [complete theme][complete_theme()] object.}
  #'   \item{`position`}{A scalar string indicating the position where
  #'   the guide should be drawn. Typically `"top"`, `"right"`, `"bottom"`
  #'   or `"left"`, unless it is a position guide for an exotic coord. Can be
  #'   `NULL`, in which case `params$position` should be used.}
  #'   \item{`direction`}{A scalar string indicating the legend direction.
  #'   Can be `NULL`, in which case `params$direction` should be used.}
  #'   \item{`params`}{A list of parameters initiated by the `params` field.}
  #' }
  #' **Value**
  #'
  #' A grob with the guide.
  draw = function(self, theme, position = NULL, direction = NULL,
                  params = self$params) {

    # Setup parameters
    params <- replace_null(params, position = position, direction = direction)
    params <- self$setup_params(params)
    key    <- params$key

    # Setup style options
    elems  <- self$setup_elements(params, self$elements, theme)
    elems  <- self$override_elements(params, elems, theme)

    # Allow early exit when key is empty
    if (prod(dim(key)) == 0) {
      out <- self$draw_early_exit(params, elems)
      return(out)
    }

    # Build grobs
    grobs <- list(
      title  = self$build_title(params$title, elems, params),
      ticks  = self$build_ticks(key, elems, params)
    )
    if (params$draw_label %||% TRUE) {
      grobs$labels <- self$build_labels(key, elems, params)
    } else {
      grobs$labels <- list(zeroGrob())
    }
    grobs$decor <- self$build_decor(params$decor, grobs, elems, params)

    # Arrange and assemble grobs
    sizes  <- self$measure_grobs(grobs, params, elems)
    layout <- self$arrange_layout(key, sizes, params, elems)
    self$assemble_drawing(grobs, layout, sizes, params, elems)
  },

  #' @field draw_early_exit
  #' **Description**
  #'
  #' A function method that determines what should be drawn when the guide 'key'
  #' is empty. The default method returns [`zeroGrob()`]. You can override
  #' this method if an empty key should draw anything. Used in `guide_axis()`
  #' to render the `axis.line` part even if no ticks or labels should be drawn.
  #'
  #' **Usage**
  #' ```r
  #' Guide$draw_early_exit(params, elements)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`params`}{A list of parameters initiated by the `params` field.}
  #'   \item{`elements`}{A list of elements or resolved theme settings from
  #'   the `override_elements()` method.}
  #' }
  #'
  #' **Value**
  #'
  #' A grob.
  draw_early_exit = function(self, params, elements) {
    zeroGrob()
  },

  ### Setup -------------------------------------------------------------------

  #' @field setup_params
  #' **Description**
  #'
  #' A function method for finalising parameters. Typically used to make checks
  #' on the `params` object or to make any position or direction based
  #' adjustments.
  #'
  #' **Usage**
  #' ```r
  #' Guide$setup_params(params)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`params`}{A list of parameters initiated by the `params` field.}
  #' }
  #'
  #' **Value**
  #'
  #' A list of parameters
  setup_params = function(params) {
    params
  },

  #' @field setup_elements,override_elements
  #' **Description**
  #'
  #' A function method for resolving required theme elements. The
  #' `setup_elements()` method joins local guide theme with global theme and
  #' calculates the necessary theme elements. The `override_elements()` method
  #' is a hook to edit elements after they've been calculated.
  #'
  #' You can override the `setup_elements()` method if you need more complicated
  #' theme handling before calculating elements or want to intervene in inheritance.
  #' For example, `guide_legend()` has special handling of text margins and
  #' `guide_axis()` suffixes `{aes}.{position}` to get the theme elements for
  #' the correct position.
  #'
  #' For other purposes, you can override the `override_elements()` method.
  #'
  #' **Usage**
  #' ```r
  #' Guide$setup_elements(params, elements, theme)
  #' Guide$override_elements(params, elements, theme)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`params`}{A list of parameters initiated by the `params` field.}
  #'   \item{`elements`}{A named list of strings initiated by the `elements` field.}
  #'   \item{`theme`}{A [complete theme][complete_theme()]}
  #' }
  #'
  #' **Value**
  #'
  #' A list of elements or resolved theme settings.
  setup_elements = function(params, elements, theme) {
    theme <- add_theme(theme, params$theme)
    is_char  <- vapply(elements, is.character, logical(1))
    elements[is_char] <- lapply(elements[is_char], calc_element, theme = theme)
    elements
  },

  override_elements = function(params, elements, theme) {
    elements
  },

  ### Building --------------------------------------------------------------

  #' @field build_title
  #' **Description**
  #'
  #' A function method for rendering the title. Note that titles for position
  #' guides are rendered by the Facet class and not this method.
  #'
  #' You can override this method if you need to render more than one title
  #' (or none) or adjust margin settings.
  #'
  #' **Usage**
  #' ```r
  #' Guide$build_title(label, elements, params)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`label`}{A single string or expression with the title text.}
  #'   \item{`elements`}{A list of elements or resolved theme settings from
  #'   the `override_elements()` method. The default method expects
  #'   `elements$title` to inherit from the `<element_text>` class.}
  #'   \item{`params`}{A list of parameters initiated by the `params` field.}
  #' }
  #'
  #' **Value**
  #'
  #' A grob representing the title.
  build_title = function(label, elements, params) {
    ggname(
      "guide.title",
      element_grob(
        elements$title,
        label    = label,
        margin_x = TRUE,
        margin_y = TRUE
      )
    )
  },

  #' @field build_ticks
  #' **Description**
  #'
  #' A function method for rendering tick marks.
  #'
  #' You can override this function if you don't need ticks or have completely
  #' different logic on how these should be drawn.
  #'
  #' **Usage**
  #' ```r
  #' Guide$build_ticks(key, elements, params, position, length)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`key`}{A data frame with the key information derived from the
  #'   `extract_key()` method.}
  #'   \item{`elements`}{A list of elements or resolved theme settings from
  #'   the `override_elements()` method. The default method expects
  #'   `elements$ticks` to inherit from the `<element_line>` class and
  #'   `elements$ticks_length` to be a scalar `<unit>` object.}
  #'   \item{`params`}{A list of parameters initiated by the `params` field.}
  #'   \item{`position`}{A scalar string indicating the position. Due to
  #'   historic error this works in the opposite way to intuition: if you want
  #'   ticks for an axis at the bottom of a plot, you should use `"top"` here.}
  #'   \item{`length`}{A scalar `<unit>` object giving the tick length.}
  #' }
  #'
  #' **Value**
  #'
  #' A grob representing tick marks.
  build_ticks = function(key, elements, params, position = params$position,
                         length = elements$ticks_length) {
    # TODO: position logic is crooked, should this be reversed?
    if (!is_theme_element(elements)) {
      elements <- elements$ticks
    }
    if (!is_theme_element(elements, "line")) {
      return(zeroGrob())
    }

    if (!is.list(key)) {
      breaks <- key
    } else {
      breaks <- key[[params$aes]]
    }
    n_breaks <- length(breaks)

    # Early exit if there are no breaks
    if (n_breaks < 1) {
      return(zeroGrob())
    }

    tick_len <- rep(length %||% unit(0.2, "npc"), length.out = n_breaks)

    # Resolve mark
    mark <- unit(rep(breaks, each = 2), "npc")

    # Resolve ticks
    pos <- unname(c(top = 1, bottom = 0, left = 0, right = 1)[position])
    dir <- -2 * pos + 1
    pos <- unit(rep(pos, 2 * n_breaks), "npc")
    dir <- rep(vec_interleave(dir, 0), n_breaks) * rep(tick_len, each = 2)
    tick <- pos + dir

    # Build grob
    flip_element_grob(
      elements,
      x = tick, y = mark,
      id.lengths = rep(2, n_breaks),
      flip = position %in% c("top", "bottom")
    )
  },

  #' @field build_labels
  #' **Description**
  #'
  #' A function method for rendering labels. The default method returns an
  #' empty grob. It is recommended to override this method when your extension
  #' directly descends from Guide.
  #'
  #' **Usage**
  #' ```r
  #' Guide$build_labels(key, elements, params)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`key`}{A data frame with the key information derived from the
  #'   `extract_key()` method.}
  #'   \item{`elements`}{A list of elements or resolved theme settings from
  #'   the `override_elements()` method. Most non-default methods expects
  #'   `elements$text` to inherit from the `<element_text>`.}
  #'   \item{`params`}{A list of parameters initiated by the `params` field.}
  #' }
  #'
  #' **Value**
  #'
  #' A grob representing labels.
  build_labels = function(key, elements, params) {
    # TODO: See if we can generalise label drawing for many guides
    zeroGrob()
  },

  #' @field build_decor
  #' **Description**
  #'
  #' A function method for rendering decor. As the 'wildcard' component, this
  #' can draw whatever component the guide needs that isn't already captured by
  #' the key. The default method returns an empty grob. It is recommended to
  #' override this method.
  #'
  #' For some examples: `guide_legend()` renders the keys with the glyphs,
  #' `guide_colourbar()` renders the colour gradient rectangle and
  #' `guide_axis()` renders the axis line.
  #'
  #' **Usage**
  #' ```r
  #' Guide$build_decor(decor, grobs, elements, params)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`decor`}{A data frame (or other structure) with information derived
  #'   from the `extract_decor()` method.}
  #'   \item{`grobs`}{A list with grobs generated by the other `build_*`
  #'   methods.}
  #'   \item{`elements`}{A list of elements or resolved theme settings from
  #'   the `override_elements()` method. Most non-default methods expects
  #'   `elements$text` to inherit from the `<element_text>`.}
  #'   \item{`params`}{A list of parameters initiated by the `params` field.}
  #' }
  #'
  #' **Value**
  #'
  #' A grob.
  build_decor = function(decor, grobs, elements, params) {
    zeroGrob()
  },

  ### Assembly --------------------------------------------------------------

  #' @field measure_grobs
  #' **Description**
  #'
  #' A function method for measuring grobs. In preparation for arranging grobs,
  #' they often need to be measured to determine their widths and heights.
  #' It is convention that every measurement is converted to centimetres.
  #' You can override this method if your extension directly descends from
  #' Guide, or the parent class measurement is defective.
  #'
  #' **Usage**
  #' ```r
  #' Guide$measure_grobs(grobs, params, elements)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`grobs`}{A list with grobs generated by the `build_*` methods.}
  #'   \item{`params`}{A list of parameters initiated by the `params` field.}
  #'   \item{`elements`}{A list of elements or resolved theme settings from
  #'   the `override_elements()` method.}
  #' }
  #'
  #' **Value**
  #'
  #' A named list or `<unit>` vector giving sizes of components, coordinated
  #' with `arrange_layout()` and `assemble_drawing()` methods. The default
  #' method returns `NULL`.
  measure_grobs = function(grobs, params, elements) {
    return(invisible())
  },

  #' @field arrange_layout
  #' **Description**
  #'
  #' A function method for determining the location or order of grobs in a
  #' gtable. Typically determines rows and columns where decor and labels are
  #' placed. Titles are added seperately.You can override this method if your
  #' extension directly descends from Guide.
  #'
  #' **Usage**
  #' ```r
  #' Guide$arrange_layout(key, sizes, params, elements)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`key`}{A data frame with the key information derived from the
  #'   `extract_key()` method.}
  #'   \item{`sizes`}{A list of `<unit>` vector from the `measure_grobs()` method.}
  #'   \item{`params`}{A list of parameters initiated by the `params` field.}
  #'   \item{`elements`}{A list of elements or resolved theme settings from
  #'   the `override_elements()` method.}
  #' }
  #'
  #' **Value**
  #'
  #' Any structure holding placement information coordinated with the
  #' `assemble_drawing()` method.
  arrange_layout = function(key, sizes, params, elements) {
    return(invisible())
  },

  #' @field assemble_drawing
  #' **Description**
  #'
  #' A function method that takes measurements, placement information and grobs
  #' and assembles these together in a gtable structure. You can override this
  #' method if your extension directly descends from Guide, or the parent class
  #' assembly does not work for your guide.
  #'
  #' **Usage**
  #' ```r
  #' Guide$assemble_drawing(grobs, layout, sizes, params, elements)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`grobs`}{A list with grobs generated by the `build_*` methods.}
  #'   \item{`layout`}{A data structure from the `arrange_layout()` method.}
  #'   \item{`sizes`}{A list of `<unit>` vector from the `measure_grobs()` method.}
  #'   \item{`params`}{A list of parameters initiated by the `params` field.}
  #'   \item{`elements`}{A list of elements or resolved theme settings from
  #'   the `override_elements()` method.}
  #' }
  #'
  #' **Value**
  #'
  #' A finished gtable containing the guide.
  assemble_drawing = function(grobs, layout, sizes, params, elements) {
    zeroGrob()
  },

  #' @field arrange_layout
  #' **Description**
  #'
  #' A function method for placing the title. It is a subsidiary method used
  #' in the `assemble_drawing()` method for non-position guides. Titles are
  #' typically added before `legend.margin` is applied. It is not
  #' recommended to override this method.
  #'
  #' **Usage**
  #' ```r
  #' Guide$add_title(gtable, title, position, just)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`gtable`}{An unfinished gtable under construction in the
  #'   `assemble_drawing()` method.}
  #'   \item{`title`}{The grob resulting from the `build_title()` method.}
  #'   \item{`position`}{A scaler string, either `"top"`, `"right"`, `"bottom"`
  #'   or `"left"` corresponding to the `legend.title.position`.}
  #'   \item{`just`}{A named list having `hjust` and `vjust` components with
  #'   scalar numeric values between 0 and 1.}
  #' }
  #'
  #' **Value**
  #'
  #' The `gtable` argument with added title.
  add_title = function(gtable, title, position, just) {
    if (is.zero(title)) {
      return(gtable)
    }

    title_width_cm  <- width_cm(title)
    title_height_cm <- height_cm(title)

    # Add extra row/col for title
    gtable <- switch(
      position,
      top    = gtable_add_rows(gtable, unit(title_height_cm, "cm"), pos =  0),
      right  = gtable_add_cols(gtable, unit(title_width_cm,  "cm"), pos = -1),
      bottom = gtable_add_rows(gtable, unit(title_height_cm, "cm"), pos = -1),
      left   = gtable_add_cols(gtable, unit(title_width_cm,  "cm"), pos =  0)
    )

    # Add title
    args <- switch(
      position,
      top    = list(t =  1, l =  1, r = -1, b =  1),
      right  = list(t =  1, l = -1, r = -1, b = -1),
      bottom = list(t = -1, l =  1, r = -1, b = -1),
      left   = list(t =  1, l =  1, r =  1, b = -1),
    )
    gtable <- inject(gtable_add_grob(
      x = gtable, grobs = title, !!!args, z = -Inf, name = "title", clip = "off"
    ))

    if (position %in% c("top", "bottom")) {

      if (any(unitType(gtable$widths) == "null")) {
        # Don't need to add extra title size for stretchy legends
        return(gtable)
      }
      table_width <- sum(width_cm(gtable$widths))
      extra_width <- max(0, title_width_cm - table_width)
      if (extra_width == 0) {
        return(gtable)
      }
      extra_width <- unit((c(1, -1) * just$hjust + c(0, 1)) * extra_width, "cm")
      gtable <- gtable_add_cols(gtable, extra_width[1], pos =  0)
      gtable <- gtable_add_cols(gtable, extra_width[2], pos = -1)

    } else {

      if (any(unitType(gtable$heights) == "null")) {
        # Don't need to add extra title size for stretchy legends
        return(gtable)
      }
      table_height <- sum(height_cm(gtable$heights))
      extra_height <- max(0, title_height_cm - table_height)
      if (extra_height == 0) {
        return(gtable)
      }
      extra_height <- unit((c(-1, 1) * just$vjust + c(1, 0)) * extra_height, "cm")
      gtable <- gtable_add_rows(gtable, extra_height[1], pos =  0)
      gtable <- gtable_add_rows(gtable, extra_height[2], pos = -1)
    }

    gtable
  }
)

# Helpers -----------------------------------------------------------------

# Helper function that may facilitate flipping theme elements by
# swapping x/y related arguments to `element_grob()`
flip_element_grob <- function(..., flip = FALSE) {
  if (!flip) {
    ans <- element_grob(...)
    return(ans)
  }
  args <- list(...)
  translate <- names(args) %in% names(flip_names)
  names(args)[translate] <- flip_names[names(args)[translate]]
  do.call(element_grob, args)
}

# The flippable arguments for `flip_element_grob()`.
flip_names <- c(
  "x"        = "y",
  "y"        = "x",
  "width"    = "height",
  "height"   = "width",
  "hjust"    = "vjust",
  "vjust"    = "hjust",
  "margin_x" = "margin_y",
  "margin_y" = "margin_x"
)

# Shortcut for position argument matching
.trbl <- c("top", "right", "bottom", "left")

opposite_position <- function(position) {
  switch(
    position,
    top    = "bottom",
    bottom = "top",
    left   = "right",
    right  = "left",
    position
  )
}

# Ensure that labels aren't a list of expressions, but proper expressions
validate_labels <- function(labels) {
  if (!obj_is_list(labels)) {
    return(labels)
  }
  labels[lengths(labels) == 0L] <- ""
  if (any(vapply(labels, is.language, logical(1)))) {
    inject(expression(!!!labels))
  } else {
    unlist(labels)
  }
}
