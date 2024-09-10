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
    params <- ggproto_parent(Coord, self)$setup_params(data)

    params$crs <- self$determine_crs(data)
    params$default_crs <- self$default_crs
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
      function(x) rescale(x, from = panel_params$x_range),
      function(x) rescale(x, from = panel_params$y_range)
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
      cli::cli_abort("{.arg breaks} and {.arg labels} along {.code x} direction have different lengths.")
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
      cli::cli_abort("{.arg breaks} and {.arg labels} along {.code y} direction have different lengths.")
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
    expansion_x <- default_expansion(scale_x, expand = params$expand[c(4, 2)])
    expansion_y <- default_expansion(scale_y, expand = params$expand[c(3, 1)])

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
          "i" = "Consider setting {.code lims_method = {.val geometry_bbox}} or {.code default_crs = NULL}."
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

    breaks <- sf_breaks(scale_x, scale_y, bbox, params$crs)

    # Generate graticule and rescale to plot coords
    graticule <- sf::st_graticule(
      bbox,
      crs = params$crs,
      lat = breaks$y %|W|% NULL,
      lon = breaks$x %|W|% NULL,
      datum = self$datum,
      ndiscr = self$ndiscr
    )

    if (is.null(breaks$x)) {
      graticule <- vec_slice(graticule, graticule$type != "E")
    }
    if (is.null(breaks$y)) {
      graticule <- vec_slice(graticule, graticule$type != "N")
    }

    # override graticule labels provided by sf::st_graticule() if necessary
    graticule <- self$fixup_graticule_labels(graticule, scale_x, scale_y, params)

    # Convert graticule to viewscales for axis guides
    viewscales <- Map(
      view_scales_from_graticule,
      scale = list(x = scale_x, y = scale_y, x.sec = scale_x, y.sec = scale_y),
      aesthetic = c("x", "y", "x.sec", "y.sec"),
      label = self$label_axes[c("bottom", "left", "top", "right")],
      MoreArgs = list(
        graticule = graticule,
        bbox = bbox,
        label_graticule = self$label_graticule
      )
    )

    # Rescale graticule for panel grid
    sf::st_geometry(graticule) <- sf_rescale01(sf::st_geometry(graticule), x_range, y_range)
    graticule$x_start <- rescale(graticule$x_start, from = x_range)
    graticule$x_end   <- rescale(graticule$x_end,   from = x_range)
    graticule$y_start <- rescale(graticule$y_start, from = y_range)
    graticule$y_end   <- rescale(graticule$y_end,   from = y_range)

    list2(
      x_range = x_range,
      y_range = y_range,
      graticule = graticule,
      crs = params$crs,
      default_crs = params$default_crs,
      !!!viewscales
    )
  },

  train_panel_guides = function(self, panel_params, layers, params = list()) {
    # The guide positions are already in the target CRS, so we mask the default
    # CRS to prevent a double transformation.
    panel_params$guides <- ggproto_parent(Coord, self)$train_panel_guides(
      vec_assign(panel_params, "default_crs", panel_params["crs"]),
      layers, params
    )$guides
    panel_params
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
      line_gp <- gg_par(
        col = el$colour,
        lwd = el$linewidth,
        lty = el$linetype
      )
      grobs <- c(
        list(element_render(theme, "panel.background")),
        lapply(sf::st_geometry(panel_params$graticule), sf::st_as_grob, gp = line_gp)
      )
    }
    ggname("grill", inject(grobTree(!!!grobs)))
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

# different limits methods
calc_limits_bbox <- function(method, xlim, ylim, crs, default_crs) {
  if (!all(is.finite(c(xlim, ylim))) && method != "geometry_bbox") {
    cli::cli_abort(c(
            "Scale limits cannot be mapped onto spatial coordinates in {.fn coord_sf}.",
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
  }

  if (is.character(label_graticule)) {
    label_graticule <- unlist(strsplit(label_graticule, ""))
  } else {
    cli::cli_abort("Graticule labeling format not recognized.")
  }

  # switch limit method to "orthogonal" if not specified and default_crs indicates projected coords
  if (is.null(default_crs) && is_missing(lims_method)) {
    lims_method <- "orthogonal"
  } else {
    lims_method <- arg_match0(lims_method, c("cross", "box", "orthogonal", "geometry_bbox"))
  }

  check_coord_limits(xlim)
  check_coord_limits(ylim)

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
  labs <- unlist(strsplit(x, ""))
  list(top = labs[1], right = labs[2], bottom = labs[3], left = labs[4])
}

# This function does two things differently from standard breaks:
#   1. It does not resolve `waiver()`, unless `n.breaks` is given. In the case
#      that breaks are `waiver()`, we use the default graticule breaks.
#   2. It discards non-finite breaks because they are invalid input to the
#      graticule. This may cause atomic `labels` to be out-of-sync.
sf_breaks <- function(scale_x, scale_y, bbox, crs) {

  has_x <- !is.null(scale_x$breaks) || !is.null(scale_x$n.breaks)
  has_y <- !is.null(scale_y$breaks) || !is.null(scale_y$n.breaks)

  x_breaks <- if (has_x) waiver() else NULL
  y_breaks <- if (has_y) waiver() else NULL


  if (has_x || has_y) {
    if (!is.null(crs)) {
      # Atomic breaks input are assumed to be in long/lat coordinates.
      # To preserve that assumption for function breaks, the bounding box
      # needs to be translated to long/lat coordinates.
      if (!is_named(bbox)) {
        names(bbox) <- c("xmin", "ymin", "xmax", "ymax")
      }
      # Convert bounding box to long/lat coordinates
      bbox <- sf::st_as_sfc(sf::st_bbox(bbox, crs = crs))
      bbox <- sf::st_bbox(sf::st_transform(bbox, 4326))
      bbox <- as.numeric(bbox)

      # If any bbox is NA the transformation has probably failed.
      # (.e.g from IGH to long/lat). In this case, just provide full long/lat.
      bbox[is.na(bbox)] <- c(-180, -90, 180, 90)[is.na(bbox)]
    }

    if (!(is.waive(scale_x$breaks) && is.null(scale_x$n.breaks))) {
      x_breaks <- scale_x$get_breaks(limits = bbox[c(1, 3)])
      finite <- is.finite(x_breaks)
      x_breaks <- if (any(finite)) x_breaks[finite] else NULL
    }

    if (!(is.waive(scale_y$breaks) && is.null(scale_y$n.breaks))) {
      y_breaks <- scale_y$get_breaks(limits = bbox[c(2, 4)])
      finite <- is.finite(y_breaks)
      y_breaks <- if (any(finite)) y_breaks[finite] else NULL
    }
  }

  list(x = x_breaks, y = y_breaks)
}

#' ViewScale from graticule
#'
#' This function converts a graticule and other CoordSf's settings to a
#' ViewScale with the appropriate `breaks` and `labels` to be rendered by a
#' guide.
#'
#' @param graticule A graticule as produced by `sf::st_graticule()`.
#' @param scale An x or y position scale for a panel.
#' @param aesthetic One of `"x"`, `"y"`, `"x.sec"` or `"y.sec'` specifying the
#'   plot position of the guide.
#' @param label One of `"E"` for meridians or `"N"` for parallels. If neither,
#'   no tick information will be produced.
#' @param label_graticule See `?coord_sf`.
#' @param bbox A `numeric(4)` bounding box with 'xmin', 'ymin', 'xmax' and
#'   'ymax' positions.
#'
#' @return A `ViewScale` object.
#' @noRd
#' @keywords internal
view_scales_from_graticule <- function(graticule, scale, aesthetic,
                                       label, label_graticule, bbox) {

  # Setup position specific parameters
  # Note that top/bottom doesn't necessarily mean to label the meridians and
  # left/right doesn't necessarily mean to label the parallels.
  position <- switch(
    arg_match0(aesthetic, c("x", "x.sec", "y", "y.sec")),
    "x"     = "bottom",
    "x.sec" = "top",
    "y"     = "left",
    "y.sec" = "right"
  )
  axis <- gsub("\\.sec$", "", aesthetic)
  if (axis == "x") {
    orth   <- "y"
    thres  <- bbox[c(2, 4)] # To determine graticule is close to axis
    limits <- bbox[c(1, 3)] # To use as scale limits
  } else {
    orth   <- "x"
    thres  <- bbox[c(1, 3)]
    limits <- bbox[c(2, 4)]
  }

  # Determine what columns in the graticule contain the starts and ends of the
  # axis direction and the orthogonal direction.
  axis_start <- paste0(axis, "_start")
  axis_end   <- paste0(axis, "_end")
  orth_start <- paste0(orth, "_start")
  orth_end   <- paste0(orth, "_end")

  # Find the start and endpoints in the graticule that are in close proximity
  # to the axis position to generate 'accepted' starts and ends. Close proximity
  # here is defined as within 0.1% of the scale range of the *orthogonal* scale.
  if (position %in% c("top", "right")) {
    thres <- thres[1] + 0.999 * diff(thres)
    accept_start <- graticule[[orth_start]] > thres
    accept_end   <- graticule[[orth_end]]   > thres
  } else {
    thres <- thres[1] + 0.001 * diff(thres)
    accept_start <- graticule[[orth_start]] < thres
    accept_end   <- graticule[[orth_end]]   < thres
  }

  # Parsing the information of the `label_axes` argument:
  # should we label the meridians ("E") or parallels ("N")?
  type <- graticule$type
  idx_start <- idx_end <- integer(0)
  idx_start <- c(idx_start, which(type == label & accept_start))
  idx_end   <- c(idx_end,   which(type == label & accept_end))

  # Parsing the information of the `label_graticule` argument. Because
  # geometry can be rotated, not all meridians are guaranteed to intersect the
  # top/bottom axes and likewise not all parallels are guaranteed to intersect
  # the left/right axes.
  if ("S" %in% label_graticule) {
    idx_start <- c(idx_start, which(type == "E" & accept_start))
  }
  if ("N" %in% label_graticule) {
    idx_end   <- c(idx_end,   which(type == "E" & accept_end))
  }
  if ("W" %in% label_graticule) {
    idx_start <- c(idx_start, which(type == "N" & accept_start))
  }
  if ("E" %in% label_graticule) {
    idx_end   <- c(idx_end,   which(type == "N" & accept_end))
  }

  # Combine start and end positions for tick marks and labels
  tick_start <- vec_slice(graticule, unique0(idx_start))
  tick_end   <- vec_slice(graticule, unique0(idx_end))
  positions  <- c(field(tick_start, axis_start), field(tick_end, axis_end))
  labels     <- c(tick_start$degree_label, tick_end$degree_label)

  # The positions/labels need to be ordered for axis dodging
  ord       <- order(positions)
  positions <- positions[ord]
  labels    <- labels[ord]

  # Find out if the scale has defined guides
  if (scale$position != position) {
    # Try to use secondary axis' guide
    guide <- scale$secondary.axis$guide %||% waiver()
    if (is.derived(guide)) {
      guide <- scale$guide
    }
  } else {
    guide <- scale$guide
  }
  # Instruct default guides: no ticks or labels should default to no guide
  if (length(positions) > 0) {
    guide <- guide %|W|% "axis"
  } else {
    guide <- guide %|W|% "none"
  }

  ggproto(
    NULL, ViewScale,
    scale = scale,
    guide = guide,
    position = position,
    aesthetics = scale$aesthetics,
    name = scale$name,
    scale_is_discrete = scale$is_discrete(),
    limits = limits,
    continuous_range = limits,
    breaks = positions,
    minor_breaks = NULL,

    # This viewscale has fixed labels, not dynamic ones as other viewscales
    # might have.
    labels = labels,
    get_labels = function(self, breaks = self$get_breaks()) {
      self$labels
    }
  )
}
