# S3 classes --------------------------------------------------------------

# Meta classes:
# TODO: These should be replaced once R 4.3.0 is the minimum version as `+`
# dispatch should work as intended.
class_gg    <- S7::new_class("gg", abstract = TRUE)
class_S3_gg <- S7::new_S3_class("gg")

# Proper S3 classes we need awareness for
class_ggproto <- S7::new_S3_class("ggproto")
class_gtable  <- S7::new_S3_class("gtable")

# The important ggproto classes that we treat as S3 classes in S7 even though
# they are their own thing.
class_scale  <- S7::new_S3_class("Scale")
class_guides <- S7::new_S3_class("Guides")
class_coord  <- S7::new_S3_class("Coord")
class_facet  <- S7::new_S3_class("Facet")
class_layer  <- S7::new_S3_class("Layer")
class_layout <- S7::new_S3_class("Layout")
class_scales_list <- S7::new_S3_class("ScalesList")

# User facing classes -----------------------------------------------------

#' The theme class
#'
#' The theme class holds information on how non-data elements of the plot
#' should be rendered. The preferred way to construct an object of this class
#' is through the [`theme()`] function.
#'
#' @param elements A named list containing theme elements.
#' @param complete A boolean value stating whether a theme is complete.
#' @param validate A boolean value stating whether a theme should still be
#'   validated.
#'
#' @keywords internal
#' @export
class_theme <- S7::new_class(
  "theme", class_S3_gg,
  properties = list(
    complete = S7::class_logical,
    validate = S7::class_logical
  ),
  constructor = function(elements, complete, validate) {
    S7::new_object(
      elements,
      complete = complete,
      validate = validate
    )
  }
)

#' The labels class
#'
#' The labels class holds a list with label information to display as titles
#' of plot components. The preferred way to construct an object of the labels
#' class is to use the [`labs()`] function.
#'
#' @param labels A named list.
#'
#' @keywords internal
#' @export
class_labels <- S7::new_class(
  "labels", parent = class_S3_gg,
  constructor = function(labels) S7::new_object(labels),
  validator = function(self) {
    if (!is.list(self)) {
      return("labels must be a list.")
    }
    if (!is_named2(self)) {
      return("every label must be named.")
    }
    dups <- unique(names(self)[duplicated(names(self))])
    if (length(dups) > 0) {
      dups <- oxford_comma(dups, final = "and")
      return(paste0("labels cannot contain duplicate names (", dups, ")."))
    }
    return(NULL)
  }
)

#' The mapping class
#'
#' The mapping class holds a list of quoted expressions
#' ([quosures][rlang::topic-quosure]) or constants. An object is typically
#' constructed using the [`aes()`] function.
#'
#' @param x A list of quosures and constants.
#' @param env An environment for symbols that are not quosures or constants.
#'
#' @keywords internal
#' @export
class_mapping <- S7::new_class(
  "mapping", parent = class_S3_gg,
  constructor = function(x, env = globalenv()) {
    check_object(x, is.list, "a {.cls list}")
    x <- lapply(x, new_aesthetic, env = env)
    S7::new_object(x)
  }
)

#' The ggplot class
#'
#' The ggplot class collects the needed information to render a plot.
#' This class can be constructed using the [`ggplot()`] function.
#'
#' @param data A property containing any data coerced by [`fortify()`].
#' @param layers A list of layer instances created by [`layer()`].
#' @param scales A ScalesList ggproto object.
#' @param guides A Guides ggproto object created by [`guides()`].
#' @param mapping A mapping class object created by [`aes()`].
#' @param theme A theme class object created by [`theme()`].
#' @param coordinates A Coord ggproto object created by `coord_*()` family of
#'   functions.
#' @param facet A Facet ggproto object created by `facet_*()` family of
#'   functions.
#' @param layout A Layout ggproto object.
#' @param labels A labels object created by [`labs()`].
#' @param plot_env An environment.
#'
#' @keywords internal
#' @export
class_ggplot <- S7::new_class(
  name = "ggplot", parent = class_gg,
  properties = list(
    data    = S7::class_any,
    layers  = S7::class_list,
    scales  = class_scales_list,
    guides  = class_guides,
    mapping = class_mapping,
    theme   = class_theme,
    coordinates = class_coord,
    facet   = class_facet,
    layout  = class_layout,
    labels  = class_labels,
    meta    = S7::class_list,
    plot_env = S7::class_environment
  ),
  constructor = function(data = waiver(), layers = list(), scales = NULL,
                         guides = NULL, mapping = aes(), theme = NULL,
                         coordinates = coord_cartesian(default = TRUE),
                         facet = facet_null(), layout = NULL,
                         labels = labs(), meta = list(),
                         plot_env = parent.frame()) {
    S7::new_object(
      S7::S7_object(),
      data        = data,
      layers      = layers,
      scales      = scales %||% scales_list(),
      guides      = guides %||% guides_list(),
      mapping     = mapping,
      theme       = theme %||% theme(),
      coordinates = coordinates,
      facet       = facet,
      layout      = layout %||% ggproto(NULL, Layout),
      labels      = labels,
      meta        = meta,
      plot_env    = plot_env
    )
  }
)

#' The ggplot built class
#'
#' The ggplot built class is an intermediate class and represents a processed
#' ggplot object ready for rendering. It is constructed by calling
#' [`ggplot_build()`] on a [ggplot][class_ggplot] object and is not meant to be
#' instantiated directly. The class can be rendered to a gtable object by
#' calling the [`ggplot_gtable()`] function on a ggplot built class object.
#'
#' @param data A list of plain data frames; one for each layer.
#' @param layout A Layout ggproto object.
#' @param plot A completed ggplot class object.
#'
#' @keywords internal
#' @export
class_ggplot_built <- S7::new_class(
  "ggplot_built", parent = class_gg,
  properties = list(
    data   = S7::class_list,
    layout = class_layout,
    plot   = class_ggplot
  ),
  constructor = function(data = NULL, layout = NULL, plot = NULL) {
    if (is.null(data) || is.null(layout) || is.null(plot)) {
      cli::cli_abort(
        "The {.cls ggplot_built} class should be constructed by {.fn ggplot_build}."
      )
    }
    S7::new_object(
      S7::S7_object(),
      data = data, layout = layout, plot = plot
    )
  }
)
