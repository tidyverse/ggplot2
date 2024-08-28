#' @section Coordinate systems:
#'
#' All `coord_*()` functions (like `coord_trans()`) return a `Coord*`
#' object (like `CoordTrans`).
#'
#' Each of the `Coord*` objects is a [ggproto()] object,
#' descended from the top-level `Coord`.  To create a new type of Coord
#' object, you typically will want to implement one or more of the following:
#'
#'   - `aspect`: Returns the desired aspect ratio for the plot.
#'   - `labels`: Returns a list containing labels for x and y.
#'   - `render_fg`: Renders foreground elements.
#'   - `render_bg`: Renders background elements.
#'   - `render_axis_h`: Renders the horizontal axes.
#'   - `render_axis_v`: Renders the vertical axes.
#'   - `backtransform_range(panel_params)`: Extracts the panel range provided
#'     in `panel_params` (created by `setup_panel_params()`, see below) and
#'     back-transforms to data coordinates. This back-transformation can be needed
#'     for coords such as `coord_trans()` where the range in the transformed
#'     coordinates differs from the range in the untransformed coordinates. Returns
#'     a list of two ranges, `x` and `y`, and these correspond to the variables
#'     mapped to the `x` and `y` aesthetics, even for coords such as `coord_flip()`
#'     where the `x` aesthetic is shown along the y direction and vice versa.
#'   - `range(panel_params)`: Extracts the panel range provided
#'     in `panel_params` (created by `setup_panel_params()`, see below) and
#'     returns it. Unlike `backtransform_range()`, this function does not perform
#'     any back-transformation and instead returns final transformed coordinates. Returns
#'     a list of two ranges, `x` and `y`, and these correspond to the variables
#'     mapped to the `x` and `y` aesthetics, even for coords such as `coord_flip()`
#'     where the `x` aesthetic is shown along the y direction and vice versa.
#'   - `transform`: Transforms x and y coordinates.
#'   - `distance`: Calculates distance.
#'   - `is_linear`: Returns `TRUE` if the coordinate system is
#'     linear; `FALSE` otherwise.
#'   - `is_free`: Returns `TRUE` if the coordinate system supports free
#'     positional scales; `FALSE` otherwise.
#'   - `setup_panel_params(scale_x, scale_y, params)`: Determines the appropriate
#'     x and y ranges for each panel, and also calculates anything else needed to
#'     render the panel and axes, such as tick positions and labels for major
#'     and minor ticks. Returns all this information in a named list.
#'   - `setup_data(data, params)`: Allows the coordinate system to
#'     manipulate the plot data. Should return list of data frames.
#'   - `setup_layout(layout, params)`: Allows the coordinate
#'     system to manipulate the `layout` data frame which assigns
#'     data to panels and scales.
#'
#' See also the `r link_book("new coords section", "extensions#sec-new-coords")`
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
Coord <- ggproto("Coord",

  # Is this the default coordinate system?
  default = FALSE,

  # should drawing be clipped to the extent of the plot panel?
  # "on" = yes, "off" = no
  clip = "on",

  aspect = function(ranges) NULL,

  labels = function(self, labels, panel_params) {
    labels
  },

  render_fg = function(panel_params, theme) {
    element_render(theme, "panel.border", fill = NA)
  },

  render_bg = function(self, panel_params, theme) {
    cli::cli_abort("{.fn {snake_class(self)}} has not implemented a {.fn render_bg} method.")
  },

  render_axis_h = function(self, panel_params, theme) {
    cli::cli_abort("{.fn {snake_class(self)}} has not implemented a {.fn render_axis_h} method.")
  },

  render_axis_v = function(self, panel_params, theme) {
    cli::cli_abort("{.fn {snake_class(self)}} has not implemented a {.fn render_axis_v} method.")
  },

  # transform range given in transformed coordinates
  # back into range in given in (possibly scale-transformed)
  # data coordinates
  backtransform_range = function(self, panel_params) {
    cli::cli_abort("{.fn {snake_class(self)}} has not implemented a {.fn backtransform_range} method.")
  },

  # return range stored in panel_params
  range = function(self, panel_params) {
    cli::cli_abort("{.fn {snake_class(self)}} has not implemented a {.fn range} method.")
  },

  setup_panel_params = function(scale_x, scale_y, params = list()) {
    list()
  },

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

  transform = function(data, range) NULL,

  distance = function(x, y, panel_params) NULL,

  is_linear = function() FALSE,

  # Does the coordinate system support free scaling of axes in a faceted plot?
  # Will generally have to return FALSE for coordinate systems that enforce a fixed aspect ratio.
  is_free = function() FALSE,

  setup_params = function(self, data) {
    list(
      guide_default = guide_axis(),
      guide_missing = guide_none(),
      expand = parse_coord_expand(self$expand %||% TRUE)
    )
  },

  setup_data = function(data, params = list()) {
    data
  },

  setup_layout = function(layout, params) {
    # We're appending a COORD variable to the layout that determines the
    # uniqueness of panel parameters. The layout uses this to prevent redundant
    # setups of these parameters.
    scales <- layout[c("SCALE_X", "SCALE_Y")]
    layout$COORD <- vec_match(scales, unique0(scales))
    layout
  },

  # Optionally, modify list of x and y scales in place. Currently
  # used as a fudge for CoordFlip and CoordPolar
  modify_scales = function(scales_x, scales_y) {
    invisible()
  },

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
  }
)

#' Is this object a coordinate system?
#'
#' @export is.Coord
#' @keywords internal
is.Coord <- function(x) inherits(x, "Coord")

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
  if (!obj_is_vector(limits) || length(limits) != 2) {
    what <- "{.obj_type_friendly {limits}}"
    if (is.vector(limits)) {
      what <- paste0(what, " of length {length(limits)}")
    }
    cli::cli_abort(
      paste0("{.arg {arg}} must be a vector of length 2, not ", what, "."),
      call = call
    )
  }
}
