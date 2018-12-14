#' Visualise sf objects
#'
#' This set of geom, stat, and coord are used to visualise simple feature (sf)
#' objects. For simple plots, you will only need `geom_sf()` as it
#' uses `stat_sf()` and adds `coord_sf()` for you. `geom_sf()` is
#' an unusual geom because it will draw different geometric objects depending
#' on what simple features are present in the data: you can get points, lines,
#' or polygons.
#' For text and labels, you can use `geom_sf_text()` and `geom_sf_label()`.
#'
#' @section Geometry aesthetic:
#' `geom_sf()` uses a unique aesthetic: `geometry`, giving an
#' column of class `sfc` containing simple features data. There
#' are three ways to supply the `geometry` aesthetic:
#'
#'   - Do nothing: by default `geom_sf()` assumes it is stored in
#'     the `geometry` column.
#'   - Explicitly pass an `sf` object to the `data` argument.
#'     This will use the primary geometry column, no matter what it's called.
#'   - Supply your own using `aes(geometry = my_column)`
#'
#' Unlike other aesthetics, `geometry` will never be inherited from
#' the plot.
#'
#' @section CRS:
#' `coord_sf()` ensures that all layers use a common CRS. You can
#' either specify it using the `CRS` param, or `coord_sf()` will
#' take it from the first layer that defines a CRS.
#'
#' @param show.legend logical. Should this layer be included in the legends?
#'   `NA`, the default, includes if any aesthetics are mapped.
#'   `FALSE` never includes, and `TRUE` always includes.
#'
#'   You can also set this to one of "polygon", "line", and "point" to
#'   override the default legend.
#' @seealso [stat_sf_coordinates()]
#' @examples
#' if (requireNamespace("sf", quietly = TRUE)) {
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' ggplot(nc) +
#'   geom_sf(aes(fill = AREA))
#'
#' # If not supplied, coord_sf() will take the CRS from the first layer
#' # and automatically transform all other layers to use that CRS. This
#' # ensures that all data will correctly line up
#' nc_3857 <- sf::st_transform(nc, "+init=epsg:3857")
#' ggplot() +
#'   geom_sf(data = nc) +
#'   geom_sf(data = nc_3857, colour = "red", fill = NA)
#'
#' # Unfortunately if you plot other types of feature you'll need to use
#' # show.legend to tell ggplot2 what type of legend to use
#' nc_3857$mid <- sf::st_centroid(nc_3857$geometry)
#' ggplot(nc_3857) +
#'   geom_sf(colour = "white") +
#'   geom_sf(aes(geometry = mid, size = AREA), show.legend = "point")
#'
#' # You can also use layers with x and y aesthetics: these are
#' # assumed to already be in the common CRS.
#' ggplot(nc) +
#'   geom_sf() +
#'   annotate("point", x = -80, y = 35, colour = "red", size = 4)
#'
#' # Thanks to the power of sf, a geom_sf nicely handles varying projections
#' # setting the aspect ratio correctly.
#' library(maps)
#' world1 <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))
#' ggplot() + geom_sf(data = world1)
#'
#' world2 <- sf::st_transform(
#'   world1,
#'   "+proj=laea +y_0=0 +lon_0=155 +lat_0=-90 +ellps=WGS84 +no_defs"
#' )
#' ggplot() + geom_sf(data = world2)
#'
#' # To add labels, use geom_sf_label().
#' ggplot(nc_3857[1:3, ]) +
#'    geom_sf(aes(fill = AREA)) +
#'    geom_sf_label(aes(label = NAME))
#' }
#' @name ggsf
NULL

geom_column <- function(data) {
  w <- which(vapply(data, inherits, TRUE, what = "sfc"))
  if (length(w) == 0) {
    "geometry" # avoids breaks when objects without geometry list-column are examined
  } else {
    # this may not be best in case more than one geometry list-column is present:
    if (length(w) > 1)
      warning("more than one geometry column present: taking the first")
    w[[1]]
  }
}

is_sf <- function(data) {
  inherits(data, "sf")
}

# stat --------------------------------------------------------------------

#' @export
#' @rdname ggsf
#' @usage NULL
#' @format NULL
StatSf <- ggproto("StatSf", Stat,
  compute_group = function(data, scales) {
    bbox <- sf::st_bbox(data[[ geom_column(data) ]])
    data$xmin <- bbox[["xmin"]]
    data$xmax <- bbox[["xmax"]]
    data$ymin <- bbox[["ymin"]]
    data$ymax <- bbox[["ymax"]]

    data
  },

  required_aes = c("geometry")
)

#' @export
#' @rdname ggsf
#' @inheritParams stat_identity
stat_sf <- function(mapping = NULL, data = NULL, geom = "rect",
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, ...) {
  layer(
    stat = StatSf,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = if (is.character(show.legend)) TRUE else show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      legend = if (is.character(show.legend)) show.legend else "polygon",
      ...
    ),
    layer_class = LayerSf
  )
}


# A special sf layer that auto-maps geometry data to the `geometry` aesthetic

#' @export
#' @rdname ggsf
#' @usage NULL
#' @format NULL
LayerSf <- ggproto("LayerSf", Layer,
  setup_layer = function(self, data, plot) {
    # process generic layer setup first
    data <- ggproto_parent(Layer, self)$setup_layer(data, plot)

    # automatically determine the name of the geometry column
    # and add the mapping if it doesn't exist
    if ((isTRUE(self$inherit.aes) && is.null(self$mapping$geometry) && is.null(plot$mapping$geometry)) ||
        (!isTRUE(self$inherit.aes) && is.null(self$mapping$geometry))) {
      if (is_sf(data)) {
        geometry_col <- attr(data, "sf_column")
        self$mapping$geometry <- as.name(geometry_col)
      }
    }
    data
  }
)


# geom --------------------------------------------------------------------

#' @export
#' @rdname ggsf
#' @usage NULL
#' @format NULL
GeomSf <- ggproto("GeomSf", Geom,
  required_aes = "geometry",
  default_aes = aes(
    shape = NULL,
    colour = NULL,
    fill = NULL,
    size = NULL,
    linetype = 1,
    alpha = NA,
    stroke = 0.5
  ),

  draw_panel = function(data, panel_params, coord, legend = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10) {
    if (!inherits(coord, "CoordSf")) {
      stop("geom_sf() must be used with coord_sf()", call. = FALSE)
    }

    # Need to refactor this to generate one grob per geometry type
    coord <- coord$transform(data, panel_params)
    grobs <- lapply(1:nrow(data), function(i) {
      sf_grob(
        coord[i, , drop = FALSE],
        lineend = lineend,
        linejoin = linejoin,
        linemitre = linemitre
      )
    })
    do.call("gList", grobs)
  },

  draw_key = function(data, params, size) {
    data <- modify_list(default_aesthetics(params$legend), data)
    if (params$legend == "point") {
      draw_key_point(data, params, size)
    } else if (params$legend == "line") {
      draw_key_path(data, params, size)
    } else {
      draw_key_polygon(data, params, size)
    }
  }
)

default_aesthetics <- function(type) {
  if (type == "point") {
    GeomPoint$default_aes
  } else if (type == "line") {
    GeomLine$default_aes
  } else  {
    modify_list(GeomPolygon$default_aes, list(fill = "grey90", colour = "grey35"))
  }
}

sf_grob <- function(row, lineend = "butt", linejoin = "round", linemitre = 10) {
  # Need to extract geometry out of corresponding list column
  geometry <- row$geometry[[1]]

  if (inherits(geometry, c("POINT", "MULTIPOINT"))) {
    row <- modify_list(default_aesthetics("point"), row)
    gp <- gpar(
      col = alpha(row$colour, row$alpha),
      fill = alpha(row$fill, row$alpha),
      # Stroke is added around the outside of the point
      fontsize = row$size * .pt + row$stroke * .stroke / 2,
      lwd = row$stroke * .stroke / 2
    )
    sf::st_as_grob(geometry, gp = gp, pch = row$shape)
  } else {
    row <- modify_list(default_aesthetics("poly"), row)
    gp <- gpar(
      col = row$colour,
      fill = alpha(row$fill, row$alpha),
      lwd = row$size * .pt,
      lty = row$linetype,
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre
    )
    sf::st_as_grob(geometry, gp = gp)
  }
}

#' @export
#' @rdname ggsf
#' @inheritParams geom_point
geom_sf <- function(mapping = aes(), data = NULL, stat = "sf",
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, ...) {
  c(
    layer(
      geom = GeomSf,
      data = data,
      mapping = mapping,
      stat = stat,
      position = position,
      show.legend = if (is.character(show.legend)) TRUE else show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        legend = if (is.character(show.legend)) show.legend else "polygon",
        ...
      ),
      layer_class = LayerSf
    ),
    coord_sf(default = TRUE)
  )
}

#' @export
#' @rdname ggsf
#' @inheritParams geom_label
#' @inheritParams stat_sf_coordinates
geom_sf_label <- function(mapping = aes(), data = NULL,
                          stat = "sf_coordinates", position = "identity",
                          ...,
                          parse = FALSE,
                          nudge_x = 0,
                          nudge_y = 0,
                          label.padding = unit(0.25, "lines"),
                          label.r = unit(0.15, "lines"),
                          label.size = 0.25,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          fun.geometry = NULL) {

  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
    }

    position <- position_nudge(nudge_x, nudge_y)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      label.padding = label.padding,
      label.r = label.r,
      label.size = label.size,
      na.rm = na.rm,
      fun.geometry = fun.geometry,
      ...
    ),
    layer_class = LayerSf
  )
}

#' @export
#' @rdname ggsf
#' @inheritParams geom_text
#' @inheritParams stat_sf_coordinates
geom_sf_text <- function(mapping = aes(), data = NULL,
                         stat = "sf_coordinates", position = "identity",
                         ...,
                         parse = FALSE,
                         nudge_x = 0,
                         nudge_y = 0,
                         check_overlap = FALSE,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE,
                         fun.geometry = NULL) {

  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("You must specify either `position` or `nudge_x`/`nudge_y`.", call. = FALSE)
    }

    position <- position_nudge(nudge_x, nudge_y)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      fun.geometry = fun.geometry,
      ...
    ),
    layer_class = LayerSf
  )
}


#' @export
scale_type.sfc <- function(x) "identity"


# Coord -------------------------------------------------------------------

#' @export
#' @rdname ggsf
#' @usage NULL
#' @format NULL
CoordSf <- ggproto("CoordSf", CoordCartesian,

  # Find the first CRS if not already supplied
  setup_params = function(self, data) {
    if (!is.null(self$crs)) {
      return(list(crs = self$crs))
    }

    for (layer_data in data) {
      if (is_sf(layer_data)) {
        geometry <- sf::st_geometry(layer_data)
      } else
        next

      crs <- sf::st_crs(geometry)
      if (is.na(crs))
        next

      return(list(crs = crs))
    }

    list(crs = NULL)
  },

  # Transform all layers to common CRS (if provided)
  setup_data = function(data, params) {
    if (is.null(params$crs))
      return(data)

    lapply(data, function(layer_data) {
      if (! is_sf(layer_data)) {
        return(layer_data)
      }

      sf::st_transform(layer_data, params$crs)
    })
  },

  transform = function(self, data, panel_params) {
    data[[ geom_column(data) ]] <- sf_rescale01(
      data[[ geom_column(data) ]],
      panel_params$x_range,
      panel_params$y_range
    )

    # Assume x and y supplied directly already in common CRS
    data <- transform_position(
      data,
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
      stop("Breaks and labels along x direction are different lengths", call. = FALSE)
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
      stop("Breaks and labels along y direction are different lengths", call. = FALSE)
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
    # Bounding box of the data
    x_range <- scale_range(scale_x, self$limits$x, self$expand)
    y_range <- scale_range(scale_y, self$limits$y, self$expand)
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
      label_axes = self$label_axes,
      label_graticule = self$label_graticule
    )
  },

  backtransform_range = function(panel_params) {
    # this does not actually return backtransformed ranges in the general case, needs fixing
    warning(
      "range backtransformation not implemented in this coord; results may be wrong.",
      call. = FALSE
    )
    list(x = panel_params$x_range, y = panel_params$y_range)
  },

  range = function(panel_params) {
    list(x = panel_params$x_range, y = panel_params$y_range)
  },


  # CoordSf enforces a fixed aspect ratio -> axes cannot be changed freely under faceting
  is_free = function() FALSE,

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

  render_bg = function(self, panel_params, theme) {
    el <- calc_element("panel.grid.major", theme)
    line_gp <- gpar(col = el$colour, lwd = el$size, lty = el$linetype)
    grobs <- c(
      list(element_render(theme, "panel.background")),
      lapply(sf::st_geometry(panel_params$graticule), sf::st_as_grob, gp = line_gp)
    )
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
      top <- guide_axis(
        tick_positions,
        tick_labels,
        position = "top",
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
      bottom <- guide_axis(
        tick_positions,
        tick_labels,
        position = "bottom",
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
      right <- guide_axis(
        tick_positions,
        tick_labels,
        position = "right",
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
      left <- guide_axis(
        tick_positions,
        tick_labels,
        position = "left",
        theme = theme
      )
    } else {
      left <- zeroGrob()
    }

    list(left = left, right = right)
  }

)

sf_rescale01 <- function(x, x_range, y_range) {
  if (is.null(x)) {
    return(x)
  }

  # Shift + affine transformation to rescale to [0, 1] x [0, 1]
  # Contributed by @edzer
  (x - c(x_range[1], y_range[1])) *
    diag(1 / c(diff(x_range), diff(y_range)))
}
sf_rescale01_x <- function(x, range) {
  (x - range[1]) / diff(range)
}


#' @param crs Use this to select a specific coordinate reference system (CRS).
#'   If not specified, will use the CRS defined in the first layer.
#' @param datum CRS that provides datum to use when generating graticules
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
#'   are not guaranteed to reside on only one particular side of the plot panel.
#'
#'   This parameter can be used alone or in combination with `label_axes`.
#' @param ndiscr number of segments to use for discretising graticule lines;
#'   try increasing this when graticules look unexpected
#' @inheritParams coord_cartesian
#' @export
#' @rdname ggsf
coord_sf <- function(xlim = NULL, ylim = NULL, expand = TRUE,
                     crs = NULL, datum = sf::st_crs(4326),
                     label_graticule = waiver(),
                     label_axes = waiver(),
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
    stop(
      "Panel labeling format not recognized.",
      call. = FALSE
    )
    label_axes <- list(left = "N", bottom = "E")
  }

  if (is.character(label_graticule)) {
    label_graticule <- unlist(strsplit(label_graticule, ""))
  } else {
    stop(
      "Graticule labeling format not recognized.",
      call. = FALSE
    )
    label_graticule <- ""
  }

  ggproto(NULL, CoordSf,
    limits = list(x = xlim, y = ylim),
    datum = datum,
    crs = crs,
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
