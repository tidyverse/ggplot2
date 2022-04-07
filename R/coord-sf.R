#' @export
#' @rdname ggsf
#' @usage NULL
#' @format NULL
CoordSf <- ggproto("CoordSf", CoordCartesian,

  # CoordSf needs to keep track of some parameters
  # internally as the plot is built. These are stored
  # here.
  params = list(),

  # the method used to convert limits across nonlinear
  # coordinate systems.
  lims_method = "cross",

  get_default_crs = function(self) {
    self$default_crs %||% self$params$default_crs
  },

  setup_params = function(self, data) {
    crs <- self$determine_crs(data)

    params <- list(
      crs = crs,
      default_crs = self$default_crs
    )
    self$params <- params

    params
  },

  # Helper function for setup_params(),
  # finds the first CRS if not already supplied
  determine_crs = function(self, data) {
    if (!is.null(self$crs)) {
      return(self$crs)
    }

    for (layer_data in data) {
      if (is_sf(layer_data)) {
        geometry <- sf::st_geometry(layer_data)
      } else
        next

      crs <- sf::st_crs(geometry)
      if (is.na(crs))
        next

      return(crs)
    }

    NULL
  },

  # Transform all layers to common CRS (if provided)
  setup_data = function(data, params) {
    if (is.null(params$crs))
      return(data)

    lapply(data, function(layer_data) {
      if (! is_sf(layer_data)) {
        return(layer_data)
      }

      idx <- vapply(layer_data, inherits, what = "sfc", FUN.VALUE = logical(1L))
      layer_data[idx] <- lapply(layer_data[idx], sf::st_transform, crs = params$crs)
      layer_data
    })
  },

  # Allow sf layer to record the bounding boxes of elements
  record_bbox = function(self, xmin, xmax, ymin, ymax) {
    bbox <- self$params$bbox
    bbox$xmin <- min(bbox$xmin, xmin)
    bbox$xmax <- max(bbox$xmax, xmax)
    bbox$ymin <- min(bbox$ymin, ymin)
    bbox$ymax <- max(bbox$ymax, ymax)
    self$params$bbox <- bbox
  },

  transform = function(self, data, panel_params) {
    # we need to transform all non-sf data into the correct coordinate system
    source_crs <- panel_params$default_crs
    target_crs <- panel_params$crs

    # normalize geometry data, it should already be in the correct crs here
    data[[ geom_column(data) ]] <- sf_rescale01(
      data[[ geom_column(data) ]],
      panel_params$x_range,
      panel_params$y_range
    )

    # transform and normalize regular position data
    data <- transform_position(
      sf_transform_xy(data, target_crs, source_crs),
      function(x) sf_rescale01_x(x, panel_params$x_range),
      function(x) sf_rescale01_x(x, panel_params$y_range)
    )

    transform_position(data, squish_infinite, squish_infinite)
  },


  # internal function used by setup_panel_params,
  # overrides the graticule labels based on scale settings if necessary
  fixup_graticule_labels = function(self, graticule, scale_x, scale_y, params = list()) {
    needs_parsing <- rep(FALSE, nrow(graticule))
    needs_autoparsing <- rep(FALSE, nrow(graticule))

    x_breaks <- graticule$degree[graticule$type == "E"]
    if (is.null(scale_x$labels)) {
      x_labels <- rep(NA, length(x_breaks))
    } else if (is.waive(scale_x$labels)) {
      x_labels <- graticule$degree_label[graticule$type == "E"]
      needs_autoparsing[graticule$type == "E"] <- TRUE
    } else {
      if (is.function(scale_x$labels)) {
        x_labels <- scale_x$labels(x_breaks)
      } else {
        x_labels <- scale_x$labels
      }

      # all labels need to be temporarily stored as character vectors,
      # but expressions need to be parsed afterwards
      needs_parsing[graticule$type == "E"] <- !(is.character(x_labels) || is.factor(x_labels))
      x_labels <- as.character(x_labels)
    }

    if (length(x_labels) != length(x_breaks)) {
      cli::cli_abort("Breaks and labels along x direction are different lengths")
    }
    graticule$degree_label[graticule$type == "E"] <- x_labels


    y_breaks <- graticule$degree[graticule$type == "N"]
    if (is.null(scale_y$labels)) {
      y_labels <- rep(NA, length(y_breaks))
    } else if (is.waive(scale_y$labels)) {
      y_labels <- graticule$degree_label[graticule$type == "N"]
      needs_autoparsing[graticule$type == "N"] <- TRUE
    } else {
      if (is.function(scale_y$labels)) {
        y_labels <- scale_y$labels(y_breaks)
      } else {
        y_labels <- scale_y$labels
      }

      # all labels need to be temporarily stored as character vectors,
      # but expressions need to be parsed afterwards
      needs_parsing[graticule$type == "N"] <- !(is.character(y_labels) || is.factor(y_labels))
      y_labels <- as.character(y_labels)
    }

    if (length(y_labels) != length(y_breaks)) {
      cli::cli_abort("Breaks and labels along y direction are different lengths")
    }
    graticule$degree_label[graticule$type == "N"] <- y_labels

    # Parse labels if requested/needed
    has_degree <- grepl("\\bdegree\\b", graticule$degree_label)
    needs_parsing <- needs_parsing | (needs_autoparsing & has_degree)
    if (any(needs_parsing)) {
      labels <- as.list(graticule$degree_label)
      labels[needs_parsing] <- parse_safe(graticule$degree_label[needs_parsing])
      graticule$degree_label <- labels
    }

    graticule
  },

  setup_panel_params = function(self, scale_x, scale_y, params = list()) {
    # expansion factors for scale limits
    expansion_x <- default_expansion(scale_x, expand = self$expand)
    expansion_y <- default_expansion(scale_y, expand = self$expand)

    # get scale limits and coord limits and merge together
    # coord limits take precedence over scale limits
    scale_xlim <- scale_x$get_limits()
    scale_ylim <- scale_y$get_limits()
    coord_xlim <- self$limits$x %||% c(NA_real_, NA_real_)
    coord_ylim <- self$limits$y %||% c(NA_real_, NA_real_)

    scale_xlim <- ifelse(is.na(coord_xlim), scale_xlim, coord_xlim)
    scale_ylim <- ifelse(is.na(coord_ylim), scale_ylim, coord_ylim)

    # now, transform limits to common crs
    # note: return value is not a true bounding box, but a
    # list of x and y values whose max/mins are the bounding
    # box
    scales_bbox <- calc_limits_bbox(
      self$lims_method,
      scale_xlim, scale_ylim,
      params$crs, params$default_crs
    )

    # merge coord bbox into scale limits if scale limits not explicitly set
    if (is.null(self$limits$x) && is.null(self$limits$y) &&
        is.null(scale_x$limits) && is.null(scale_y$limits)) {
      coord_bbox <- self$params$bbox
      scales_xrange <- range(scales_bbox$x, coord_bbox$xmin, coord_bbox$xmax, na.rm = TRUE)
      scales_yrange <- range(scales_bbox$y, coord_bbox$ymin, coord_bbox$ymax, na.rm = TRUE)
    } else if (any(!is.finite(scales_bbox$x) | !is.finite(scales_bbox$y))) {
      if (self$lims_method != "geometry_bbox") {
        cli::cli_warn(c(
                "Projection of {.field x} or {.field y} limits failed in {.fn coord_sf}.",
          "i" = "Consider setting {.code lims_method = \"geometry_bbox\"} or {.code default_crs = NULL}."
        ))
      }
      coord_bbox <- self$params$bbox
      scales_xrange <- c(coord_bbox$xmin, coord_bbox$xmax) %||% c(0, 0)
      scales_yrange <- c(coord_bbox$ymin, coord_bbox$ymax) %||% c(0, 0)
    } else {
      scales_xrange <- range(scales_bbox$x, na.rm = TRUE)
      scales_yrange <- range(scales_bbox$y, na.rm = TRUE)
    }

    # apply coordinate expansion
    x_range <- expand_limits_continuous(scales_xrange, expansion_x)
    y_range <- expand_limits_continuous(scales_yrange, expansion_y)
    bbox <- c(
      x_range[1], y_range[1],
      x_range[2], y_range[2]
    )

    # Generate graticule and rescale to plot coords
    graticule <- sf::st_graticule(
      bbox,
      crs = params$crs,
      lat = scale_y$breaks %|W|% NULL,
      lon = scale_x$breaks %|W|% NULL,
      datum = self$datum,
      ndiscr = self$ndiscr
    )

    # override graticule labels provided by sf::st_graticule() if necessary
    graticule <- self$fixup_graticule_labels(graticule, scale_x, scale_y, params)

    sf::st_geometry(graticule) <- sf_rescale01(sf::st_geometry(graticule), x_range, y_range)
    graticule$x_start <- sf_rescale01_x(graticule$x_start, x_range)
    graticule$x_end <- sf_rescale01_x(graticule$x_end, x_range)
    graticule$y_start <- sf_rescale01_x(graticule$y_start, y_range)
    graticule$y_end <- sf_rescale01_x(graticule$y_end, y_range)

    list(
      x_range = x_range,
      y_range = y_range,
      graticule = graticule,
      crs = params$crs,
      default_crs = params$default_crs,
      label_axes = self$label_axes,
      label_graticule = self$label_graticule
    )
  },

  backtransform_range = function(self, panel_params) {
    target_crs <- panel_params$default_crs
    source_crs <- panel_params$crs

    x <- panel_params$x_range
    y <- panel_params$y_range
    data <- list(x = c(x, x), y = c(y, rev(y)))
    data <- sf_transform_xy(data, target_crs, source_crs)
    list(x = range(data$x), y = range(data$y))
  },

  range = function(panel_params) {
    list(x = panel_params$x_range, y = panel_params$y_range)
  },

  # CoordSf enforces a fixed aspect ratio -> axes cannot be changed freely under faceting
  is_free = function() FALSE,

  # for regular geoms (such as geom_path, geom_polygon, etc.), CoordSf is non-linear
  # if the default_crs option is being used, i.e., not set to NULL
  is_linear = function(self) is.null(self$get_default_crs()),

  distance = function(self, x, y, panel_params) {
    d <- self$backtransform_range(panel_params)
    max_dist <- dist_euclidean(d$x, d$y)
    dist_euclidean(x, y) / max_dist
  },

  aspect = function(self, panel_params) {
    if (isTRUE(sf::st_is_longlat(panel_params$crs))) {
      # Contributed by @edzer
      mid_y <- mean(panel_params$y_range)
      ratio <- cos(mid_y * pi / 180)
    } else {
      # Assume already projected
      ratio <- 1
    }

    diff(panel_params$y_range) / diff(panel_params$x_range) / ratio
  },

  labels = function(labels, panel_params) labels,

  render_bg = function(self, panel_params, theme) {
    el <- calc_element("panel.grid.major", theme)

    # we don't draw the graticules if the major panel grid is
    # turned off
    if (inherits(el, "element_blank")) {
      grobs <- list(element_render(theme, "panel.background"))
    } else {
      line_gp <- gpar(
        col = el$colour,
        lwd = len0_null(el$size*.pt),
        lty = el$linetype
      )
      grobs <- c(
        list(element_render(theme, "panel.background")),
        lapply(sf::st_geometry(panel_params$graticule), sf::st_as_grob, gp = line_gp)
      )
    }
    ggname("grill", do.call("grobTree", grobs))
  },

  render_axis_h = function(self, panel_params, theme) {
    graticule <- panel_params$graticule

    # top axis
    id1 <- id2 <- integer(0)
    # labels based on panel side
    id1 <- c(id1, which(graticule$type == panel_params$label_axes$top & graticule$y_start > 0.999))
    id2 <- c(id2, which(graticule$type == panel_params$label_axes$top & graticule$y_end > 0.999))

    # labels based on graticule direction
    if ("S" %in% panel_params$label_graticule) {
      id1 <- c(id1, which(graticule$type == "E" & graticule$y_start > 0.999))
    }
    if ("N" %in% panel_params$label_graticule) {
      id2 <- c(id2, which(graticule$type == "E" & graticule$y_end > 0.999))
    }
    if ("W" %in% panel_params$label_graticule) {
      id1 <- c(id1, which(graticule$type == "N" & graticule$y_start > 0.999))
    }
    if ("E" %in% panel_params$label_graticule) {
      id2 <- c(id2, which(graticule$type == "N" & graticule$y_end > 0.999))
    }

    ticks1 <- graticule[unique(id1), ]
    ticks2 <- graticule[unique(id2), ]
    tick_positions <- c(ticks1$x_start, ticks2$x_end)
    tick_labels <- c(ticks1$degree_label, ticks2$degree_label)

    if (length(tick_positions) > 0) {
      top <- draw_axis(
        tick_positions,
        tick_labels,
        axis_position = "top",
        theme = theme
      )
    } else {
      top <- zeroGrob()
    }

    # bottom axis
    id1 <- id2 <- integer(0)
    # labels based on panel side
    id1 <- c(id1, which(graticule$type == panel_params$label_axes$bottom & graticule$y_start < 0.001))
    id2 <- c(id2, which(graticule$type == panel_params$label_axes$bottom & graticule$y_end < 0.001))

    # labels based on graticule direction
    if ("S" %in% panel_params$label_graticule) {
      id1 <- c(id1, which(graticule$type == "E" & graticule$y_start < 0.001))
    }
    if ("N" %in% panel_params$label_graticule) {
      id2 <- c(id2, which(graticule$type == "E" & graticule$y_end < 0.001))
    }
    if ("W" %in% panel_params$label_graticule) {
      id1 <- c(id1, which(graticule$type == "N" & graticule$y_start < 0.001))
    }
    if ("E" %in% panel_params$label_graticule) {
      id2 <- c(id2, which(graticule$type == "N" & graticule$y_end < 0.001))
    }

    ticks1 <- graticule[unique(id1), ]
    ticks2 <- graticule[unique(id2), ]
    tick_positions <- c(ticks1$x_start, ticks2$x_end)
    tick_labels <- c(ticks1$degree_label, ticks2$degree_label)

    if (length(tick_positions) > 0) {
      bottom <- draw_axis(
        tick_positions,
        tick_labels,
        axis_position = "bottom",
        theme = theme
      )
    } else {
      bottom <- zeroGrob()
    }

    list(top = top, bottom = bottom)
  },

  render_axis_v = function(self, panel_params, theme) {
    graticule <- panel_params$graticule

    # right axis
    id1 <- id2 <- integer(0)
    # labels based on panel side
    id1 <- c(id1, which(graticule$type == panel_params$label_axes$right & graticule$x_end > 0.999))
    id2 <- c(id2, which(graticule$type == panel_params$label_axes$right & graticule$x_start > 0.999))

    # labels based on graticule direction
    if ("N" %in% panel_params$label_graticule) {
      id1 <- c(id1, which(graticule$type == "E" & graticule$x_end > 0.999))
    }
    if ("S" %in% panel_params$label_graticule) {
      id2 <- c(id2, which(graticule$type == "E" & graticule$x_start > 0.999))
    }
    if ("E" %in% panel_params$label_graticule) {
      id1 <- c(id1, which(graticule$type == "N" & graticule$x_end > 0.999))
    }
    if ("W" %in% panel_params$label_graticule) {
      id2 <- c(id2, which(graticule$type == "N" & graticule$x_start > 0.999))
    }

    ticks1 <- graticule[unique(id1), ]
    ticks2 <- graticule[unique(id2), ]
    tick_positions <- c(ticks1$y_end, ticks2$y_start)
    tick_labels <- c(ticks1$degree_label, ticks2$degree_label)

    if (length(tick_positions) > 0) {
      right <- draw_axis(
        tick_positions,
        tick_labels,
        axis_position = "right",
        theme = theme
      )
    } else {
      right <- zeroGrob()
    }

    # left axis
    id1 <- id2 <- integer(0)
    # labels based on panel side
    id1 <- c(id1, which(graticule$type == panel_params$label_axes$left & graticule$x_end < 0.001))
    id2 <- c(id2, which(graticule$type == panel_params$label_axes$left & graticule$x_start < 0.001))

    # labels based on graticule direction
    if ("N" %in% panel_params$label_graticule) {
      id1 <- c(id1, which(graticule$type == "E" & graticule$x_end < 0.001))
    }
    if ("S" %in% panel_params$label_graticule) {
      id2 <- c(id2, which(graticule$type == "E" & graticule$x_start < 0.001))
    }
    if ("E" %in% panel_params$label_graticule) {
      id1 <- c(id1, which(graticule$type == "N" & graticule$x_end < 0.001))
    }
    if ("W" %in% panel_params$label_graticule) {
      id2 <- c(id2, which(graticule$type == "N" & graticule$x_start < 0.001))
    }

    ticks1 <- graticule[unique(id1), ]
    ticks2 <- graticule[unique(id2), ]
    tick_positions <- c(ticks1$y_end, ticks2$y_start)
    tick_labels <- c(ticks1$degree_label, ticks2$degree_label)

    if (length(tick_positions) > 0) {
      left <- draw_axis(
        tick_positions,
        tick_labels,
        axis_position = "left",
        theme = theme
      )
    } else {
      left <- zeroGrob()
    }

    list(left = left, right = right)
  }
)

#' Transform spatial position data
#'
#' Helper function that can transform spatial position data (pairs of x, y
#' values) among coordinate systems. This is implemented as a thin wrapper
#' around [sf::sf_project()].
#'
#' @param data Data frame or list containing numerical columns `x` and `y`.
#' @param target_crs,source_crs Target and source coordinate reference systems.
#'   If `NULL` or `NA`, the data is not transformed.
#' @param authority_compliant logical; `TRUE` means handle axis order authority
#'   compliant (e.g. EPSG:4326 implying `x = lat`, `y = lon`), `FALSE` means use
#'   visualisation order (i.e. always `x = lon`, `y = lat`). Default is `FALSE`.
#' @return A copy of the input data with `x` and `y` replaced by transformed values.
#' @examples
#' if (requireNamespace("sf", quietly = TRUE)) {
#' # location of cities in NC by long (x) and lat (y)
#' data <- data.frame(
#'   city = c("Charlotte", "Raleigh", "Greensboro"),
#'   x =  c(-80.843, -78.639, -79.792),
#'   y = c(35.227, 35.772, 36.073)
#' )
#'
#' # transform to projected coordinates
#' data_proj <- sf_transform_xy(data, 3347, 4326)
#' data_proj
#'
#' # transform back
#' sf_transform_xy(data_proj, 4326, 3347)
#' }
#' @keywords internal
#' @export
sf_transform_xy <- function(data, target_crs, source_crs, authority_compliant = FALSE) {
  if (identical(target_crs, source_crs) ||
      is.null(target_crs) || is.null(source_crs) || is.null(data) ||
      is.na(target_crs) || is.na(source_crs) ||
      !all(c("x", "y") %in% names(data))) {
    return(data)
  }

  sf_data <- cbind(data$x, data$y)
  out <- sf::sf_project(
    sf::st_crs(source_crs), sf::st_crs(target_crs),
    sf_data,
    keep = TRUE, warn = FALSE,
    authority_compliant = authority_compliant
  )
  out <- ifelse(is.finite(out), out, NA) # replace any infinites with NA

  data$x <- out[, 1]
  data$y <- out[, 2]

  data
}

## helper functions to normalize geometry and position data

# normalize geometry data (variable x is geometry column)
sf_rescale01 <- function(x, x_range, y_range) {
  if (is.null(x)) {
    return(x)
  }

  sf::st_normalize(x, c(x_range[1], y_range[1], x_range[2], y_range[2]))
}

# normalize position data (variable x is x or y position)
sf_rescale01_x <- function(x, range) {
  (x - range[1]) / diff(range)
}

# different limits methods
calc_limits_bbox <- function(method, xlim, ylim, crs, default_crs) {
  if (any(!is.finite(c(xlim, ylim))) && method != "geometry_bbox") {
    cli::cli_abort(c(
            "Scale limits cannot be mapped onto spatial coordinates in {.fn coord_sf}",
      "i" = "Consider setting {.code lims_method = \"geometry_bbox\"} or {.code default_crs = NULL}."
    ))
  }

  bbox <- switch(
    method,
    # For method "box", we take the limits and turn them into a
    # box. We subdivide the box edges into multiple segments to
    # better cover the respective area under non-linear transformation
    box = list(
      x = c(
        rep(xlim[1], 20), seq(xlim[1], xlim[2], length.out = 20),
        rep(xlim[2], 20), seq(xlim[2], xlim[1], length.out = 20)
      ),
      y = c(
        seq(ylim[1], ylim[2], length.out = 20), rep(ylim[2], 20),
        seq(ylim[2], ylim[1], length.out = 20), rep(ylim[1], 20)
      )
    ),
    # For method "geometry_bbox" we ignore all limits info provided here
    geometry_bbox = list(
      x = c(NA_real_, NA_real_),
      y = c(NA_real_, NA_real_)
    ),
    # For method "orthogonal" we simply return what we are given
    orthogonal = list(
      x = xlim,
      y = ylim
    ),
    # For method "cross" we take the mid-point along each side of
    # the scale range for better behavior when box is nonlinear or
    # rotated in projected space
    #
    # Method "cross" is also the default
    cross =,
    list(
      x = c(rep(mean(xlim), 20), seq(xlim[1], xlim[2], length.out = 20)),
      y = c(seq(ylim[1], ylim[2], length.out = 20), rep(mean(ylim), 20))
    )
  )
  sf_transform_xy(bbox, crs, default_crs)
}

#' @param crs The coordinate reference system (CRS) into which all data should
#'   be projected before plotting. If not specified, will use the CRS defined
#'   in the first sf layer of the plot.
#' @param default_crs The default CRS to be used for non-sf layers (which
#'   don't carry any CRS information) and scale limits. The default value of
#'   `NULL` means that the setting for `crs` is used. This implies that all
#'   non-sf layers and scale limits are assumed to be specified in projected
#'   coordinates. A useful alternative setting is `default_crs = sf::st_crs(4326)`,
#'   which means x and y positions are interpreted as longitude and latitude,
#'   respectively, in the World Geodetic System 1984 (WGS84).
#' @param xlim,ylim Limits for the x and y axes. These limits are specified
#'   in the units of the default CRS. By default, this means projected coordinates
#'   (`default_crs = NULL`). How limit specifications translate into the exact
#'   region shown on the plot can be confusing when non-linear or rotated coordinate
#'   systems are used as the default crs. First, different methods can be preferable
#'   under different conditions. See parameter `lims_method` for details. Second,
#'   specifying limits along only one direction can affect the automatically generated
#'   limits along the other direction. Therefore, it is best to always specify limits
#'   for both x and y. Third, specifying limits via position scales or `xlim()`/`ylim()`
#'   is strongly discouraged, as it can result in data points being dropped from the plot even
#'   though they would be visible in the final plot region.
#' @param lims_method Method specifying how scale limits are converted into
#'   limits on the plot region. Has no effect when `default_crs = NULL`.
#'   For a very non-linear CRS (e.g., a perspective centered
#'   around the North pole), the available methods yield widely differing results, and
#'   you may want to try various options. Methods currently implemented include `"cross"`
#'   (the default), `"box"`, `"orthogonal"`, and `"geometry_bbox"`. For method `"cross"`,
#'   limits along one direction (e.g., longitude) are applied at the midpoint of the
#'   other direction (e.g., latitude). This method avoids excessively large limits for
#'   rotated coordinate systems but means that sometimes limits need to be expanded a
#'   little further if extreme data points are to be included in the final plot region.
#'   By contrast, for method `"box"`, a box is generated out of the limits along both directions,
#'   and then limits in projected coordinates are chosen such that the entire box is
#'   visible. This method can yield plot regions that are too large. Finally, method
#'   `"orthogonal"` applies limits separately along each axis, and method
#'   `"geometry_bbox"` ignores all limit information except the bounding boxes of any
#'   objects in the `geometry` aesthetic.
#' @param datum CRS that provides datum to use when generating graticules.
#' @param label_axes Character vector or named list of character values
#'   specifying which graticule lines (meridians or parallels) should be labeled on
#'   which side of the plot. Meridians are indicated by `"E"` (for East) and
#'   parallels by `"N"` (for North). Default is `"--EN"`, which specifies
#'   (clockwise from the top) no labels on the top, none on the right, meridians
#'   on the bottom, and parallels on the left. Alternatively, this setting could have been
#'   specified with `list(bottom = "E", left = "N")`.
#'
#'   This parameter can be used alone or in combination with `label_graticule`.
#' @param label_graticule Character vector indicating which graticule lines should be labeled
#'   where. Meridians run north-south, and the letters `"N"` and `"S"` indicate that
#'   they should be labeled on their north or south end points, respectively.
#'   Parallels run east-west, and the letters `"E"` and `"W"` indicate that they
#'   should be labeled on their east or west end points, respectively. Thus,
#'   `label_graticule = "SW"` would label meridians at their south end and parallels at
#'   their west end, whereas `label_graticule = "EW"` would label parallels at both
#'   ends and meridians not at all. Because meridians and parallels can in general
#'   intersect with any side of the plot panel, for any choice of `label_graticule` labels
#'   are not guaranteed to reside on only one particular side of the plot panel. Also,
#'   `label_graticule` can cause labeling artifacts, in particular if a graticule line
#'   coincides with the edge of the plot panel. In such circumstances, `label_axes` will
#'   generally yield better results and should be used instead.
#'
#'   This parameter can be used alone or in combination with `label_axes`.
#' @param ndiscr Number of segments to use for discretising graticule lines;
#'   try increasing this number when graticules look incorrect.
#' @inheritParams coord_cartesian
#' @export
#' @rdname ggsf
coord_sf <- function(xlim = NULL, ylim = NULL, expand = TRUE,
                     crs = NULL, default_crs = NULL,
                     datum = sf::st_crs(4326),
                     label_graticule = waiver(),
                     label_axes = waiver(), lims_method = "cross",
                     ndiscr = 100, default = FALSE, clip = "on") {

  if (is.waive(label_graticule) && is.waive(label_axes)) {
    # if both `label_graticule` and `label_axes` are set to waive then we
    # use the default of labels on the left and at the bottom
    label_graticule <- ""
    label_axes <- "--EN"
  } else {
    # if at least one is set we ignore the other
    label_graticule <- label_graticule %|W|% ""
    label_axes <- label_axes %|W|% ""
  }

  if (is.character(label_axes)) {
    label_axes <- parse_axes_labeling(label_axes)
  } else if (!is.list(label_axes)) {
    cli::cli_abort("Panel labeling format not recognized.")
    label_axes <- list(left = "N", bottom = "E")
  }

  if (is.character(label_graticule)) {
    label_graticule <- unlist(strsplit(label_graticule, ""))
  } else {
    cli::cli_abort("Graticule labeling format not recognized.")
    label_graticule <- ""
  }

  # switch limit method to "orthogonal" if not specified and default_crs indicates projected coords
  if (is.null(default_crs) && is_missing(lims_method)) {
    lims_method <- "orthogonal"
  } else {
    lims_method <- arg_match0(lims_method, c("cross", "box", "orthogonal", "geometry_bbox"))
  }

  ggproto(NULL, CoordSf,
    limits = list(x = xlim, y = ylim),
    lims_method = lims_method,
    datum = datum,
    crs = crs,
    default_crs = default_crs,
    label_axes = label_axes,
    label_graticule = label_graticule,
    ndiscr = ndiscr,
    expand = expand,
    default = default,
    clip = clip
  )
}

parse_axes_labeling <- function(x) {
  labs = unlist(strsplit(x, ""))
  list(top = labs[1], right = labs[2], bottom = labs[3], left = labs[4])
}
