#' Coords
#'
#' @description
#' All `coord_*()` functions (like `coord_trans()`) return a `Coord*` object
#' (like `CoordTrans`). These objects contain methods that support the
#' coordinate systems in ggplot2.
#'
#' @details
#' Each of the `Coord*` objects is a [ggproto()] object, descended from the
#' top-level `Coord`, and each implements various methods and fields.
#' The object and its parameters are chaperoned by the [Layout] class.
#'
#' To create a new type of Coord object, it is recommended
#' to extend not the base `Coord` class, but one of its children like
#' `CoordCartesian`.
#'
#' When overriding the `transform()` method, it may be necessary to adapt the
#' implementation of `render_bg()` and possibly axis placement too.
#'
#' An important data structure that coordinate systems create is the
#' `panel_params` structure. When overriding that structure, many methods may
#' need to be adapted as well.
#'
#' @section Conventions:
#'
#' The object name that a new class is assigned to is typically the same as the
#' class name. Coord class names are in UpperCamelCase and start with the
#' `Coord*` prefix, like `CoordNew`.
#'
#' A constructor function is usually paired with a Coord class. The constructor
#' copies the coord class and populates parameters. The constructor function name
#' should take the Coord class name and be formatted with snake_case,
#' so that `CoordNew` becomes `coord_new()`.
#'
#' @export
#' @format NULL
#' @family Layout components
#' @usage NULL
#' @seealso The `r link_book("new coords section", "extensions#sec-new-coords")`
#' @examples
#' # Extending the class
#' CoordJitter <- ggproto(
#'   "CoordJitter", CoordCartesian,
#'   # Fields
#'   amount = 0,
#'   # Methods
#'   is_linear = function() FALSE,
#'   transform = function(self, data, panel_params) {
#'     data   <- ggproto_parent(CoordCartesian, self)$transform(data, panel_params)
#'     data$x <- jitter(data$x, amount = self$amount)
#'     data$y <- jitter(data$y, amount = self$amount)
#'     data
#'   }
#' )
#'
#' # Building a constructor
#' coord_jitter <- function(amount = 0.005, xlim = NULL, ylim = NULL, expand = TRUE,
#'                          clip = "on", reverse = "none") {
#'   ggproto(
#'     NULL, CoordJitter,
#'     amount = amount,
#'     limits = list(x = xlim, y = ylim),
#'     reverse = reverse, expand = expand, clip = clip
#'   )
#' }
#'
#' # Use new coord in plot
#' set.seed(42)
#' ggplot(mpg, aes(drv, displ)) +
#'   geom_boxplot() +
#'   coord_jitter()
Coord <- ggproto("Coord",

  # Fields ------------------------------------------------------------------

  # Is this the default coordinate system?
  #' @field default Scaler boolean indicating whether this is the default
  #' coordinate system. Non-default coordinate systems raise a message when
  #' a new system replaces it.
  default = FALSE,

  #' @field clip A scalar string grid setting controlling whether layers should
  #' be clipped to the extent of the plot panel extent. Can be `"on"` to
  #' perform clipping, `"off"` to not clip, or `"inherit"` to take on the
  #' setting of the parent viewport.
  clip = "on",

  #' @field reverse A scalar string giving which directions to reverse. For
  #' Cartesian systems, can be `"none`, `"x"`, `"y"` or `"xy"` for both.
  #' Non-Cartesian may define their own settings.
  reverse = "none",

  # Methods -----------------------------------------------------------------

  ## setup ------------------------------------------------------------------

  #' @field setup_params
  #' **Description**
  #'
  #' A function method for modifying or checking the parameters based on the
  #' data. The default method parses the `expand` parameter.
  #'
  #' **Usage**
  #' ```r
  #' Coord$setup_params(data)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`data`}{A list of data frames. The first item is the global data,
  #'   which is followed by layer data in subsequent items.}
  #' }
  #'
  #' **Value**
  #'
  #' A list of parameters
  setup_params = function(self, data) {
    list(expand = parse_coord_expand(self$expand %||% TRUE))
  },

  #' @field setup_data
  #' **Description**
  #'
  #' A function method for modifying or checking the data prior to adding
  #' defaults. The default method returns data unaltered.
  #'
  #' **Usage**
  #' ```r
  #' Coord$setup_data(data, params)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`data`}{A list of data frames. The first item is the global data,
  #'   which is followed by layer data in subsequent items.}
  #'   \item{`params`}{A list of parameters coming from the `setup_params()`
  #'   method.}
  #' }
  #'
  #' **Value**
  #'
  #' A list of data frames of the same length as the `data` argument
  setup_data = function(data, params = list()) {
    data
  },

  #' @field setup_layout
  #' **Description**
  #'
  #' A function method that acts as a hook for the coordinate system to have
  #' input on the layout computed by facets.
  #'
  #' **Usage**
  #' ```r
  #' Coord$setup_layout(layout, params)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`layout`}{A data frame computed by `Facet$compute_layout()`.
  #'   Typically contains the faceting variables, `ROW`, `COL`, `PANEL`,
  #'   `SCALE_X` and `SCALE_Y` variables.}
  #'   \item{`params`}{A list of parameters coming from the `setup_params()`
  #'   method.}
  #' }
  #'
  #' **Value**
  #'
  #' A data frame from the modified `layout` argument. The default creates a
  #' new `COORD` column to identify unique combinations of x and y scales for
  #' efficiency purposes. It should never remove columns.
  setup_layout = function(layout, params) {
    # We're appending a COORD variable to the layout that determines the
    # uniqueness of panel parameters. The layout uses this to prevent redundant
    # setups of these parameters.
    scales <- layout[c("SCALE_X", "SCALE_Y")]
    layout$COORD <- vec_match(scales, unique0(scales))
    layout
  },

  ## setup_panel_params ------------------------------------------------------

  #' @field modify_scales
  #' **Description**
  #'
  #' A function method for modifying scales in place. This is optional and
  #' currently used by CoordFlip and CoordPolar to ensure axis positions are
  #' conforming to the coordinate system.
  #'
  #' **Usage**
  #' ```r
  #' Coord$modify_scales(scales_x, scales_y)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`scales_x`,`scales_y`}{A list of trained scales for the `x` and `y`
  #'   aesthetics respectively.}
  #' }
  #'
  #' **Value**
  #'
  #' Nothing, this is called for the side effect of modifying scales.
  modify_scales = function(scales_x, scales_y) {
    invisible()
  },

  #' @field setup_panel_params
  #' **Description**
  #'
  #' This function method is used to setup panel parameters per panel.
  #' For efficiency reasons, this method is called once per combination of
  #' `x` and `y` scales. It is used to instantiate ViewScale class objects and
  #' ranges for position aesthetics and optionally append additional
  #' parameters needed for the `transform()` method and rendering axes.
  #'
  #' **Usage**
  #' ```r
  #' Coord$setup_panel_params(scale_x, scale_y, params)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`scale_x`,`scale_y`}{A list of trained scales for the `x` and `y`
  #'   aesthetics respectively.}
  #'   \item{`params`}{A list of parameters coming from the `setup_params()`
  #'   method.}
  #' }
  #'
  #' **Value**
  #'
  #' A named list of view scales, ranges and other optional parameters.
  setup_panel_params = function(scale_x, scale_y, params = list()) {
    list()
  },

  ## setup_panel_guides ------------------------------------------------------

  #' @field setup_panel_guides
  #' **Description**
  #'
  #' This function method is used to initiate position guides for each panel.
  #' For efficiency reasons, this method is called once per combination of `x`
  #' and `y` scales. For the primary and secondary positions, it should resolve
  #' guides coming from the `plot$guides` field and `Scale$guide` fields and
  #' set appropriate `Guide$params$position` parameters.
  #'
  #' **Usage**
  #' ```r
  #' Coord$setup_panel_guides(panel_params, guides, params)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`panel_params`}{A list of ViewScale class objects and additional
  #'   parameters from the `setup_panel_params()` method.}
  #'   \item{`guides`}{A `<Guides>` ggproto class.}
  #'   \item{`params`}{A list of parameters coming from the `setup_params()`
  #'   method.}
  #' }
  #'
  #' **Value**
  #'
  #' The `panel_params` object but with a Guides class object appended with
  #' the name 'guides'.
  setup_panel_guides = function(self, panel_params, guides, params = list()) {
    aesthetics <- c("x", "y", "x.sec", "y.sec")
    names(aesthetics) <- aesthetics
    is_sec <- grepl("sec$", aesthetics)
    scales <- panel_params[aesthetics]

    # Do guide setup
    guides <- guides$setup(
      scales, aesthetics,
      default = params$guide_default %||% guide_axis(),
      missing = params$guide_missing %||% guide_none()
    )
    guide_params <- guides$get_params(aesthetics)

    # Resolve positions
    scale_position <- lapply(scales, `[[`, "position")
    guide_position <- lapply(guide_params, `[[`, "position")
    guide_position[!is_sec] <- Map(
      function(guide, scale) guide %|W|% scale,
      guide = guide_position[!is_sec],
      scale = scale_position[!is_sec]
    )
    opposite <- c(
      "top"  = "bottom", "bottom" = "top",
      "left" = "right",   "right" = "left"
    )
    guide_position[is_sec] <- Map(
      function(sec, prim) sec %|W|% unname(opposite[prim]),
      sec  = guide_position[is_sec],
      prim = guide_position[!is_sec]
    )
    guide_params <- Map(
      function(params, pos) {
        params[["position"]] <- pos
        params
      },
      params = guide_params,
      pos    = guide_position
    )

    # Update positions
    guides$update_params(guide_params)

    panel_params$guides <- guides
    panel_params
  },

  #' @field setup_panel_guides
  #' **Description**
  #'
  #' This function method is used to train and transform position guides for each
  #' panel. For efficiency reasons, this method is called once per combination
  #' of `x` and `y` scales.
  #'
  #' **Usage**
  #' ```r
  #' Coord$train_panel_guides(panel_params, layers, params)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`panel_params`}{A list of ViewScale class objects, a Guides class
  #'   object and additional parameters from the `setup_panel_params()` method.}
  #'   \item{`layers`}{A list of layers from `plot$layers`.}
  #'   \item{`params`}{A list of parameters coming from the `setup_params()`
  #'   method.}
  #' }
  #'
  #' **Value**
  #'
  #' The `panel_params` object, but with trained and transformed `guides`
  #' parameter.
  train_panel_guides = function(self, panel_params, layers, params = list()) {

    aesthetics <- c("x", "y", "x.sec", "y.sec")

    # If the panel_params doesn't contain the scale, there's no guide for the aesthetic
    aesthetics <- intersect(aesthetics, names(panel_params$guides$aesthetics))
    names(aesthetics) <- aesthetics

    guides <- panel_params$guides$get_guide(aesthetics)
    empty  <- vapply(guides, inherits, logical(1), "GuideNone")
    guide_params <- panel_params$guides$get_params(aesthetics)
    aesthetics <- aesthetics[!empty]

    guide_params[!empty] <- Map(
      function(guide, guide_param, scale) {
        guide_param <- guide$train(guide_param, scale)
        guide_param <- guide$transform(guide_param, self, panel_params)
        guide_param <- guide$get_layer_key(guide_param, layers)
        guide_param
      },
      guide = guides[!empty],
      guide_param = guide_params[!empty],
      scale = panel_params[aesthetics]
    )

    panel_params$guides$update_params(guide_params)

    panel_params
  },

  ## draw_geom ---------------------------------------------------------------
  # The Layer$draw_geom() context

  #' @field transform
  #' **Description**
  #'
  #' This function method is used to apply transformations and rescale position
  #' aesthetics. This method is used in several places:
  #' * The Geom drawing code, used through `coord_munch()` in many Geoms.
  #' * The Guide transform method
  #' * Panel grid transformation in `render_bg()`
  #'
  #' **Usage**
  #' ```r
  #' Coord$transform(data, panel_params)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`data`}{A data frame with columns for numeric position aesthetics.}
  #'   \item{`panel_params`}{A list of ViewScale class objects and additional
  #'   parameters from the `setup_panel_params()` method.}
  #' }
  #'
  #' **Value**
  #'
  #' The `data` argument with rescaled and transformed position aesthetics.
  transform = function(data, panel_params) {
    NULL
  },

  #' @field distance
  #' **Description**
  #'
  #' This function method is used to calculate distances between subsequent
  #' data points. `coord_munch()` uses this method determine how many points
  #' should be used to interpolate.
  #'
  #' **Usage**
  #' ```r
  #' Coord$distance(x, y, panel_params)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`x`,`y`}{x and y coordinates of a set of points in data space.}
  #'   \item{`panel_params`}{A list of ViewScale class objects and additional
  #'   parameters from the `setup_panel_params()` method.}
  #' }
  #'
  #' **Value**
  #'
  #' The `data` argument with rescaled and transformed position aesthetics.
  distance = function(x, y, panel_params) {
    NULL
  },

  #' @field backtransform_range
  #' **Description**
  #'
  #' This function method is used to convert ranges from transformed coordinates
  #' back into data coordinates. The data coordinates may possibly be scale-
  #' transformed. It is used in `coord_munch()` to ensure limits are in data
  #' coordinates.
  #'
  #' The back-transformation may be needed for coords such as `coord_trans()`,
  #' where the range in the transformed coordinates differs from the range in
  #' the untransformed coordinates.
  #'
  #' **Usage**
  #' ```r
  #' Coord$backtransform_range(panel_params)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`panel_params`}{A list of ViewScale class objects and additional
  #'   parameters from the `setup_panel_params()` method.}
  #' }
  #'
  #' **Value**
  #'
  #' A list containing numeric ranges for `x` and `y` in data coordinates.
  backtransform_range = function(self, panel_params) {
    cli::cli_abort("{.fn {snake_class(self)}} has not implemented a {.fn backtransform_range} method.")
  },

  # return range stored in panel_params
  #' @field range
  #' **Description**
  #'
  #' This function method is a small helper method to extract ranges from the
  #' `panel_params` object. It exists because `panel_params` can be opaque at
  #' times.
  #'
  #' **Usage**
  #' ```r
  #' Coord$range(panel_params)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`panel_params`}{A list of ViewScale class objects and additional
  #'   parameters from the `setup_panel_params()` method.}
  #' }
  #'
  #' **Value**
  #'
  #' A list containing numeric ranges for `x` and `y`.
  range = function(self, panel_params) {
    cli::cli_abort("{.fn {snake_class(self)}} has not implemented a {.fn range} method.")
  },

  ## render -----------------------------------------------------------------
  # The `Layout$render()` context

  #' @field draw_panel
  #' **Description**
  #'
  #' This function method is used to orchestrate decorating panel drawings with
  #' foreground and background drawings. It is called once per panel, invokes
  #' the `render_fg()` and `render_bg()` methods and enforces the `clip` field.
  #'
  #' **Usage**
  #' ```r
  #' Coord$draw_panel(panel, params, theme)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`panel`}{A grob containing drawn layers and facet foreground and
  #'   background.}
  #'   \item{`params`}{A list of ViewScale class objects and additional
  #'   parameters from the `setup_panel_params()` method.}
  #'   \item{`theme`}{A [complete theme][complete_theme()]}
  #' }
  #'
  #' **Value**
  #'
  #' A grob with panel content.
  draw_panel = function(self, panel, params, theme) {
    fg <- self$render_fg(params, theme)
    bg <- self$render_bg(params, theme)
    if (isTRUE(theme$panel.ontop)) {
      panel <- list2(!!!panel, bg, fg)
    } else {
      panel <- list2(bg, !!!panel, fg)
    }
    gTree(
      children = inject(gList(!!!panel)),
      vp = viewport(clip = self$clip)
    )
  },

  #' @field render_fg
  #' **Description**
  #'
  #' This function method is used to draw the panel foreground. For all
  #' intents and purposes is just the `panel.border` theme element, but you can
  #' repurpose this method.
  #'
  #' **Usage**
  #' ```r
  #' Coord$render_fg(panel_params, theme)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`panel_params`}{A list of ViewScale class objects and additional
  #'   parameters from the `setup_panel_params()` method.}
  #'   \item{`theme`}{A [complete theme][complete_theme()]}
  #' }
  #'
  #' **Value**
  #'
  #' A grob with panel foreground.
  render_fg = function(panel_params, theme) {
    element_render(theme, "panel.border", fill = NA)
  },

  #' @field render_bg
  #' **Description**
  #'
  #' This function method is used to draw the panel background. Typically
  #' this is a combination of the `panel.background` and `panel.grid` theme
  #' elements.
  #'
  #' **Usage**
  #' ```r
  #' Coord$render_bg(panel_params, theme)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`panel_params`}{A list of ViewScale class objects and additional
  #'   parameters from the `setup_panel_params()` method.}
  #'   \item{`theme`}{A [complete theme][complete_theme()]}
  #' }
  #'
  #' **Value**
  #'
  #' A grob with panel background.
  render_bg = function(self, panel_params, theme) {
    cli::cli_abort("{.fn {snake_class(self)}} has not implemented a {.fn render_bg} method.")
  },

  #' @field labels
  #' **Description**
  #'
  #' This function method is used to format axis titles. It is used in some
  #' coordinate systems to (conditionally) swap x and y labels.
  #'
  #' **Usage**
  #' ```r
  #' Coord$labels(labels, panel_params)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`labels`}{A named list containing an `x` list and a `y` list. The
  #'   `x` and `y` lists have `primary` and `secondary` labels.}
  #'   \item{`panel_params`}{A list of ViewScale class objects and additional
  #'   parameters from the `setup_panel_params()` method.}
  #' }
  #'
  #' **Value**
  #'
  #' A list with the same structure and names as the `labels` argument.
  labels = function(self, labels, panel_params) {
    labels
  },

  ## draw panels -------------------------------------------------------------
  # In Facet$draw_panels() context

  #' @field aspect
  #' **Description**
  #'
  #' This function method that gives the aspect ratio for panels. It allows for
  #' `CoordFixed` to compute an aspect ratio based on data ranges.
  #'
  #' **Usage**
  #' ```r
  #' Coord$render_bg(panel_params, theme)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`ranges`}{A list of ViewScale class objects and additional
  #'   parameters from the `setup_panel_params()` method. If there are
  #'   multiple panels, the parameters for the first panel is used.}
  #' }
  #'
  #' **Value**
  #'
  #' A scalar numeric
  aspect = function(ranges) {
    NULL
  },

  #' @field render_axis_h,render_axis_v
  #' **Description**
  #'
  #' These function methods are used to render axes to place at the outside edge
  #' of panels. Interior axes should not be rendered here. The `render_axis_h()`
  #' methods produces the horizontal axes for the top and bottom position.
  #' The `render_axis_v()` method renders the vertical axes for the left and
  #' right position.
  #'
  #' **Usage**
  #' ```r
  #' Coord$render_axis_h(panel_params, theme
  #' Coord$render_axis_v(panel_params, theme)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`panel_params`}{A list of ViewScale class objects, a Guides class
  #'   object and additional parameters from the `setup_panel_params()` method.}
  #'   \item{`theme`}{A [complete theme][complete_theme()]}
  #' }
  #'
  #' **Value**
  #'
  #' For `render_axis_h()` a named list where `"top"` and `"bottom"` are grobs
  #' with an axis. For `render_axis_v()` a named list where `"left"` and
  #' `"right"` are grobs with an axis. These grobs should be [`zeroGrob()`]
  #' when no axes should be rendered.
  render_axis_h = function(self, panel_params, theme) {
    cli::cli_abort("{.fn {snake_class(self)}} has not implemented a {.fn render_axis_h} method.")
  },

  render_axis_v = function(self, panel_params, theme) {
    cli::cli_abort("{.fn {snake_class(self)}} has not implemented a {.fn render_axis_v} method.")
  },

  ## Utilities --------------------------------------------------------------

  #' @field is_linear
  #' **Description**
  #'
  #' This function method is used to signal whether a coordinate system is
  #' linear. In `coord_munch()` and several Geom drawing methods, it is used to
  #' determine whether points should be interpolated.
  #'
  #' **Usage**
  #' ```r
  #' Coord$is_linear()
  #' ```
  #'
  #' **Value**
  #'
  #' A scalar boolean.
  is_linear = function() {
    FALSE
  },

  #' @field is_free
  #' **Description**
  #'
  #' This function method is used to signal whether a coordinate system supports
  #' free scaling of axes in faceted plots. This should generally return `FALSE`
  #' for coordinate systems that enforce a fixed aspect ratio.
  #'
  #' **Usage**
  #' ```r
  #' Coord$is_free()
  #' ```
  #'
  #' **Value**
  #'
  #' A scalar boolean.
  is_free = function() {
    FALSE
  }
)

# Helpers -----------------------------------------------------------------

#' @export
#' @rdname is_tests
is_coord <- function(x) inherits(x, "Coord")

#' @export
#' @rdname is_tests
#' @usage is.Coord(x) # Deprecated
is.Coord <- function(x) {
  deprecate_soft0("3.5.2", "is.Coord()", "is_coord()")
  is_coord(x)
}

# Renders an axis with the correct orientation or zeroGrob if no axis should be
# generated
render_axis <- function(panel_params, axis, scale, position, theme) {
  if (axis == "primary") {
    draw_axis(panel_params[[paste0(scale, ".major")]], panel_params[[paste0(scale, ".labels")]], position, theme)
  } else if (axis == "secondary" && !is.null(panel_params[[paste0(scale, ".sec.major")]])) {
    draw_axis(panel_params[[paste0(scale, ".sec.major")]], panel_params[[paste0(scale, ".sec.labels")]], position, theme)
  } else {
    zeroGrob()
  }
}

# Elaborates an 'expand' argument for every side (top, right, bottom or left)
parse_coord_expand <- function(expand) {
  if (is.numeric(expand) && all(expand %in% c(0, 1))) {
    expand <- as.logical(expand)
  }
  check_logical(expand)
  if (anyNA(expand)) {
    cli::cli_abort("{.arg expand} cannot contain missing values.")
  }

  if (!is_named(expand)) {
    return(rep_len(expand, 4))
  }

  # Match by top/right/bottom/left
  out <- rep(TRUE, 4)
  i <- match(names(expand), .trbl)
  if (sum(!is.na(i)) > 0) {
    out[i] <- unname(expand)[!is.na(i)]
  }
  out
}

# Utility function to check coord limits
check_coord_limits <- function(
    limits, arg = caller_arg(limits), call = caller_env()
) {
  if (is.null(limits)) {
    return(invisible(NULL))
  }
  check_object(limits, is_vector, "a vector", arg = arg, call = call)
  check_length(limits, 2L, arg = arg, call = call)
}

is_transform_immune <- function(data, coord_name) {
  x <- inherits(data$x, "AsIs")
  y <- inherits(data$y, "AsIs")
  if (!(x || y)) {
    # Neither variable is AsIs, so we need to transform
    return(FALSE)
  }
  if (x && y) {
    # Both variables are AsIs, so no need to transform
    return(TRUE)
  }
  # We're now in the `xor(x, y)` case
  var <- if (x) "x" else "y"
  alt <- if (x) "y" else "x"
  cli::cli_warn(
    "{.fn {coord_name}} cannot respect the {.cls AsIs} class of {.var {var}} \\
    when {.var {alt}} is not also {.cls AsIs}."
  )
  return(FALSE)
}
