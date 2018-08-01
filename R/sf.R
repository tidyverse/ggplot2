#' Visualise sf objects
#'
#' This set of geom, stat, and coord are used to visualise simple feature (sf)
#' objects. For simple plots, you will only need `geom_sf` as it
#' uses `stat_sf` and adds `coord_sf` for you. `geom_sf` is
#' an unusual geom because it will draw different geometric objects depending
#' on what simple features are present in the data: you can get points, lines,
#' or polygons.
#'
#' @section Geometry aesthetic:
#' `geom_sf` uses a unique aesthetic: `geometry`, giving an
#' column of class `sfc` containing simple features data. There
#' are three ways to supply the `geometry` aesthetic:
#'
#'   - Do nothing: by default `geom_sf` assumes it is stored in
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
#' either specify it using the `CRS` param, or `coord_sf` will
#' take it from the first layer that defines a CRS.
#'
#' @param show.legend logical. Should this layer be included in the legends?
#'   `NA`, the default, includes if any aesthetics are mapped.
#'   `FALSE` never includes, and `TRUE` always includes.
#'
#'   You can also set this to one of "polygon", "line", and "point" to
#'   override the default legend.
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
    )
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
    shape = NULL,
    colour = NULL,
    fill = NULL,
    size = NULL,
    linetype = 1,
    alpha = NA,
    stroke = 0.5
  ),

  draw_panel = function(data, panel_params, coord, legend = NULL) {
    if (!inherits(coord, "CoordSf")) {
      stop("geom_sf() must be used with coord_sf()", call. = FALSE)
    }

    # Need to refactor this to generate one grob per geometry type
    coord <- coord$transform(data, panel_params)
    grobs <- lapply(1:nrow(data), function(i) {
      sf_grob(coord[i, , drop = FALSE])
    })
    do.call("gList", grobs)
  },

  draw_key = function(data, params, size) {
    data <- utils::modifyList(default_aesthetics(params$legend), data)
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
    aes(shape = 19, colour = "black", size = 1.5, fill = NA, alpha = NA, stroke = 0.5)
  } else if (type == "line") {
    aes(colour = "black", size = 0.5, linetype = 1, alpha = NA)
  } else {
    aes(size = 0.5, linetype = 1, fill = "grey90", colour = "grey35", alpha = NA)
  }
}

sf_grob <- function(row) {
  # Need to extract geometry out of corresponding list column
  geometry <- row$geometry[[1]]

  if (inherits(geometry, c("POINT", "MULTIPOINT"))) {
    row <- utils::modifyList(default_aesthetics("point"), row)
    gp <- gpar(
      col = alpha(row$colour, row$alpha),
      fill = alpha(row$fill, row$alpha),
      # Stroke is added around the outside of the point
      fontsize = row$size * .pt + row$stroke * .stroke / 2,
      lwd = row$stroke * .stroke / 2
    )
    sf::st_as_grob(geometry, gp = gp, pch = row$shape)
  } else {
    row <- utils::modifyList(default_aesthetics("poly"), row)
    gp <- gpar(
      col = row$colour,
      fill = alpha(row$fill, row$alpha),
      lwd = row$size * .pt,
      lty = row$linetype,
      lineend = "butt"
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

  # Automatically determin name of geometry column
  if (!is.null(data) && is_sf(data)) {
    geometry_col <- attr(data, "sf_column")
  } else {
    geometry_col <- "geometry"
  }
  if (is.null(mapping$geometry)) {
    mapping$geometry <- as.name(geometry_col)
  }

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
      )
    ),
    coord_sf(default = TRUE)
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
      datum = self$datum,
      ndiscr = self$ndiscr
    )

    # remove tick labels not on axes 1 (bottom) and 2 (left)
    if (!is.null(graticule$plot12))
      graticule$degree_label[!graticule$plot12] <- NA

    sf::st_geometry(graticule) <- sf_rescale01(sf::st_geometry(graticule), x_range, y_range)
    graticule$x_start <- sf_rescale01_x(graticule$x_start, x_range)
    graticule$x_end <- sf_rescale01_x(graticule$x_end, x_range)
    graticule$y_start <- sf_rescale01_x(graticule$y_start, y_range)
    graticule$y_end <- sf_rescale01_x(graticule$y_end, y_range)
    if (any(grepl("degree", graticule$degree_label)))
      graticule$degree_label <- lapply(graticule$degree_label, function(x) parse(text = x)[[1]])

    list(
      x_range = x_range,
      y_range = y_range,
      graticule = graticule,
      crs = params$crs
    )
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
    east <- graticule[graticule$type == "E" & !is.na(graticule$degree_label), ]

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
    north <- graticule[graticule$type == "N" & !is.na(graticule$degree_label), ]

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
#' @param ndiscr number of segments to use for discretising graticule lines;
#' try increasing this when graticules look unexpected
#' @inheritParams coord_cartesian
#' @export
#' @rdname ggsf
coord_sf <- function(xlim = NULL, ylim = NULL, expand = TRUE,
                     crs = NULL, datum = sf::st_crs(4326), ndiscr = 100,
                     default = FALSE) {
  ggproto(NULL, CoordSf,
    limits = list(x = xlim, y = ylim),
    datum = datum,
    crs = crs,
    ndiscr = ndiscr,
    expand = expand,
    default = default
  )
}
