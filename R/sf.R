#' Visualise sf objects
#'
#' This set geom, stat, and coord are used to visualise simple feature (sf)
#' objects. For simple plots, you will only need \code{geom_sf} as it
#' uses \code{stat_sf} and adds \code{coord_sf} for you. \code{geom_sf} is
#' an unusual geom because it will draw different geometric objects depending
#' on what simple features are present in the data: you can get points, lines,
#' or polygons.
#'
#' @section Geometry aesthetic:
#' \code{geom_sf} uses a unique aesthetic: \code{geometry}, giving an
#' column of class \code{sfc} containg simple features data. There
#' are three ways to supply the \code{geometry} aesthetic:
#'
#' \itemize{
#'   \item Do nothing: by default \code{geom_sf} assumes it is stored in
#'     the \code{geometry} column.
#'   \item Explicitly pass an \code{sf} object to the \code{data} argument.
#'     This will use the primary geometry column, no matter what it's called.
#'   \item Supply your own using \code{aes(geometry = my_column)}
#' }
#'
#' Unlike other aesthetics, \code{geometry} will never be inherited from
#' the plot.
#'
#' @section CRS:
#' \code{coord_sf()} ensures that all layers use a common CRS. You can
#' either specify it using the \code{CRS} param, or \code{coord_sf} will
#' take it from the first layer that defines a CRS.
#'
#' @examples
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
#' # You can also use layers with x and y aesthetics: these are
#' # assumed to already be in the common CRS.
#' ggplot(nc) +
#'   geom_sf() +
#'   annotate("point", x = -80, y = 35, colour = "red", size = 4)
#'
#' @name ggsf
NULL

# stat --------------------------------------------------------------------

#' @export
#' @rdname ggsf
#' @usage NULL
#' @format NULL
StatSf <- ggproto("StatSf", Stat,
  compute_group = function(data, scales) {
    bbox <- sf::st_bbox(data$geometry)
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
stat_sf <- function(mapping = NULL, data = NULL, geom = "rect",
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, ...) {
  layer(
    stat = StatSf, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# geom --------------------------------------------------------------------

#' @export
#' @rdname ggsf
#' @usage NULL
#' @format NULL
GeomSf <- ggproto("GeomSf", Geom,
  required_aes = "geometry",
  default_aes = aes(
    colour = "grey35",
    fill = "grey90",
    size = 0.5,
    linetype = 1,
    alpha = NA
  ),
  draw_key = draw_key_polygon,

  draw_panel = function(data, panel_params, coord) {
    if (!inherits(coord, "CoordSf")) {
      stop("geom_sf() must be used with coord_sf()", call. = FALSE)
    }

    coord <- coord$transform(data, panel_params)

    gpars <- lapply(1:nrow(data), function(i) sf_gpar(coord[i, , drop = FALSE]))
    grobs <- Map(sf::st_as_grob, coord$geometry, gp = gpars)
    do.call("gList", grobs)
  }
)

sf_gpar <- function(row) {
  gpar(
    col = row$colour,
    fill = alpha(row$fill, row$alpha),
    lwd = row$size * .pt,
    lty = row$linetype,
    lineend = "butt"
  )
}

#' @export
#' @rdname ggsf
#' @inheritParams geom_point
geom_sf <- function(mapping = aes(), data = NULL, stat = "sf",
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, ...) {

  # Automatically determin name of geometry column
  if (!is.null(data) && inherits(data, "sf")) {
    geometry_col <- attr(data, "sf_column")
  } else {
    geometry_col <- "geometry"
  }
  if (is.null(mapping$geometry)) {
    mapping$geometry <- as.name(geometry_col)
  }

  c(
    layer(
      geom = GeomSf, mapping = mapping,  data = data, stat = stat,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    ),
    coord_sf()
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
      geometry <- layer_data$geometry
      if (is.null(geometry))
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
      if (is.null(layer_data$geometry)) {
        return(layer_data)
      }

      layer_data$geometry <- sf::st_transform(layer_data$geometry, params$crs)
      layer_data
    })
  },

  transform = function(self, data, panel_params) {
    data$geometry <- sf_rescale01(
      data$geometry,
      panel_params$x_range,
      panel_params$y_range
    )

    # Assume x and y supplied directly already in common CRS
    data <- transform_position(
      data,
      function(x) sf_rescale01_x(x, panel_params$x_range),
      function(x) sf_rescale01_x(x, panel_params$y_range)
    )

    data
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
      datum = self$datum
    )

    sf::st_geometry(graticule) <- sf_rescale01(sf::st_geometry(graticule), x_range, y_range)
    graticule$x_start <- sf_rescale01_x(graticule$x_start, x_range)
    graticule$x_end <- sf_rescale01_x(graticule$x_end, x_range)
    graticule$y_start <- sf_rescale01_x(graticule$y_start, y_range)
    graticule$y_end <- sf_rescale01_x(graticule$y_end, y_range)
    graticule$degree_label <- lapply(graticule$degree_label, function(x) parse(text = x)[[1]])

    list(
      x_range = x_range,
      y_range = y_range,
      graticule = graticule,
      crs = params$crs
    )
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

  render_bg = function(self, panel_params, theme) {
    line_gp <- gpar(
      col = theme$panel.grid.major$colour,
      lwd = theme$panel.grid.major$size,
      lty = theme$panel.grid.major$linetype
    )
    grobs <- c(
      list(element_render(theme, "panel.background")),
      lapply(sf::st_geometry(panel_params$graticule), sf::st_as_grob, gp = line_gp)
    )
    ggname("grill", do.call("grobTree", grobs))
  },

  render_axis_h = function(self, panel_params, theme) {
    graticule <- panel_params$graticule
    east <- graticule[graticule$type == "E", ]

    list(
      top = nullGrob(),
      bottom = guide_axis(
        east$x_start,
        east$degree_label,
        position = "bottom",
        theme = theme
      )
    )
  },

  render_axis_v = function(self, panel_params, theme) {
    graticule <- panel_params$graticule
    north <- graticule[graticule$type == "N", ]

    list(
      left = guide_axis(
        north$y_start,
        north$degree_label,
        position = "left",
        theme = theme
      ),
      right = nullGrob()
    )
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


#' @param crs Use this to select a specific CRS. If not specified, will
#'   use the CRS defined in the first layer.
#' @param datum CRS that provides datum to use when generating graticules
#' @inheritParams coord_cartesian
#' @export
#' @rdname ggsf
coord_sf <- function(xlim = NULL, ylim = NULL, expand = TRUE,
                     crs = NULL, datum = sf::st_crs(4326)) {
  ggproto(NULL, CoordSf,
    limits = list(x = xlim, y = ylim),
    datum = datum,
    crs = crs,
    expand = expand
  )
}
