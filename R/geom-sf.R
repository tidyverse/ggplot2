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
#' either specify it using the `crs` param, or `coord_sf()` will
#' take it from the first layer that defines a CRS.
#'
#' @section Combining sf layers and regular geoms:
#' Most regular geoms, such as [geom_point()], [geom_path()],
#' [geom_text()], [geom_polygon()] etc. will work fine with `coord_sf()`. However
#' when using these geoms, two problems arise. First, what CRS should be used
#' for the x and y coordinates used by these non-sf geoms? The CRS applied to
#' non-sf geoms is set by the `default_crs` parameter, and it defaults to
#' `NULL`, which means positions for non-sf geoms are interpreted as projected
#' coordinates in the coordinate system set by the `crs` parameter. This setting
#' allows you complete control over where exactly items are placed on the plot
#' canvas, but it may require some understanding of how projections work and how
#' to generate data in projected coordinates. As an alternative, you can set
#' `default_crs = sf::st_crs(4326)`, the World Geodetic System 1984 (WGS84).
#' This means that x and y positions are interpreted as longitude and latitude,
#' respectively. You can also specify any other valid CRS as the default CRS for
#' non-sf geoms.
#'
#' The second problem that arises for non-sf geoms is how straight lines
#' should be interpreted in projected space when `default_crs` is not set to `NULL`.
#' The approach `coord_sf()` takes is to break straight lines into small pieces
#' (i.e., segmentize them) and then transform the pieces into projected coordinates.
#' For the default setting where x and y are interpreted as longitude and latitude,
#' this approach means that horizontal lines follow the parallels and vertical lines
#' follow the meridians. If you need a different approach to handling straight lines,
#' then you should manually segmentize and project coordinates and generate the plot
#' in projected coordinates.
#'
#' @param show.legend logical. Should this layer be included in the legends?
#'   `NA`, the default, includes if any aesthetics are mapped.
#'   `FALSE` never includes, and `TRUE` always includes.
#'
#'   You can also set this to one of "polygon", "line", and "point" to
#'   override the default legend.
#'
#' @seealso
#' The `r link_book("simple feature maps section", "maps#sec-sf")`
#'
#' [stat_sf_coordinates()]
#' @examples
#' if (requireNamespace("sf", quietly = TRUE)) {
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' ggplot(nc) +
#'   geom_sf(aes(fill = AREA))
#'
#' # If not supplied, coord_sf() will take the CRS from the first layer
#' # and automatically transform all other layers to use that CRS. This
#' # ensures that all data will correctly line up
#' nc_3857 <- sf::st_transform(nc, 3857)
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
#' # You can also use layers with x and y aesthetics. To have these interpreted
#' # as longitude/latitude you need to set the default CRS in coord_sf()
#' ggplot(nc_3857) +
#'   geom_sf() +
#'   annotate("point", x = -80, y = 35, colour = "red", size = 4) +
#'   coord_sf(default_crs = sf::st_crs(4326))
#'
#' # To add labels, use geom_sf_label().
#' ggplot(nc_3857[1:3, ]) +
#'    geom_sf(aes(fill = AREA)) +
#'    geom_sf_label(aes(label = NAME))
#' }
#'
#' # Thanks to the power of sf, a geom_sf nicely handles varying projections
#' # setting the aspect ratio correctly.
#' if (requireNamespace('maps', quietly = TRUE)) {
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
    linewidth = NULL,
    linetype = from_theme(linetype),
    alpha = NA,
    stroke = 0.5
  ),

  use_defaults = function(self, data, params = list(), modifiers = aes(),
                          default_aes = NULL, theme = NULL, ...) {
    data <- ggproto_parent(Geom, self)$use_defaults(
      data, params, modifiers, default_aes, theme = theme, ...
    )
    # Early exit for e.g. legend data that don't have geometry columns
    if (!"geometry" %in% names(data)) {
      return(data)
    }

    # geometry column is a character if we're populating legend keys
    type <- if (is.character(data$geometry)) {
      data$geometry
    } else {
      sf_types[sf::st_geometry_type(data$geometry)]
    }

    # Devise splitting index for geometry types
    type <- factor(type, c("point", "line", "other", "collection"))
    index <- split(seq_len(nrow(data)), type)

    # Initialise parts of the data
    points <- lines <- others <- collections <- NULL

    # Go through every part, applying different defaults
    if (length(index$point) > 0) {
      points <- GeomPoint$use_defaults(
        vec_slice(data, index$point),
        params, modifiers, theme = theme
      )
    }
    if (length(index$line) > 0) {
      lines <- GeomLine$use_defaults(
        vec_slice(data, index$line),
        params, modifiers, theme = theme
      )
    }
    other_default <- modify_list(
      GeomPolygon$default_aes,
      aes(
        fill   = from_theme(col_mix(ink, paper, 0.9)),
        colour = from_theme(col_mix(ink, paper, 0.35)),
        linewidth = from_theme(0.4 * borderwidth)
      )
    )
    if (length(index$other) > 0) {
      others <- GeomPolygon$use_defaults(
        vec_slice(data, index$other),
        params, modifiers,
        default_aes = other_default,
        theme = theme
      )
    }
    if (length(index$collection) > 0) {
      modified <- rename(
        GeomPoint$default_aes,
        c(fill = "point_fill")
      )
      modified <- modify_list(other_default, modified)
      collections <- Geom$use_defaults(
        vec_slice(data, index$collection),
        params, modifiers,
        default_aes = modified,
        theme = theme
      )
    }

    # Recombine data in original order
    data <- vec_c(points, lines, others, collections)
    vec_slice(data, order(unlist(index)))
  },

  draw_panel = function(self, data, panel_params, coord, legend = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        arrow = NULL, arrow.fill = NULL, na.rm = TRUE) {
    if (!inherits(coord, "CoordSf")) {
      cli::cli_abort("{.fn {snake_class(self)}} can only be used with {.fn coord_sf}.")
    }
    if (is.character(data$shape)) {
      data$shape <- translate_shape_string(data$shape)
    }

    data <- coord$transform(data, panel_params)

    type <- sf_types[sf::st_geometry_type(data$geometry)]
    is_point <- type == "point"
    is_line  <- type == "line"
    is_collection <- type == "collection"

    fill <- fill_alpha(data$fill %||% rep(NA, nrow(data)), data$alpha)
    fill[is_line] <- arrow.fill %||% fill[is_line]

    colour <- data$colour
    colour[is_point | is_line] <-
      alpha(colour[is_point | is_line], data$alpha[is_point | is_line])

    point_size <- data$size
    point_size[!(is_point | is_collection)] <-
      data$linewidth[!(is_point | is_collection)]

    stroke <- data$stroke * .stroke / 2
    font_size <- point_size * .pt + stroke

    linewidth <- data$linewidth * .pt
    linewidth[is_point] <- stroke[is_point]

    gp <- gpar(
      col = colour, fill = fill, fontsize = font_size, lwd = linewidth,
      lineend = lineend, linejoin = linejoin, linemitre = linemitre
    )

    sf::st_as_grob(data$geometry, pch = data$shape, gp = gp, arrow = arrow)
  },

  draw_key = function(data, params, size) {
    switch(
      params$legend %||% "other",
      point = draw_key_point(data, params, size),
      line  = draw_key_path(data, params, size),
      draw_key_polygon(data, params, size)
    )
  },

  handle_na = function(self, data, params) {
    remove <- rep(FALSE, nrow(data))

    types <- sf_types[sf::st_geometry_type(data$geometry)]
    types <- split(seq_along(remove), types)

    get_missing <- function(geom) {
      detect_missing(data, c(geom$required_aes, geom$non_missing_aes))
    }

    remove[types$point] <- get_missing(GeomPoint)[types$point]
    remove[types$line]  <- get_missing(GeomPath)[types$line]
    remove[types$other] <- get_missing(GeomPolygon)[types$other]

    remove <- remove | get_missing(self)

    if (any(remove)) {
      data <- vec_slice(data, !remove)
      if (!isTRUE(params$na.rm)) {
        cli::cli_warn(
          "Removed {sum(remove)} row{?s} containing missing values or values \\
          outside the scale range ({.fn {snake_class(self)}})."
        )
      }
    }

    data
  }
)

#' @export
#' @rdname ggsf
#' @inheritParams geom_point
geom_sf <- function(mapping = aes(), data = NULL, stat = "sf",
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, ...) {
  c(
    layer_sf(
      geom = GeomSf,
      data = data,
      mapping = mapping,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list2(
        na.rm = na.rm,
        ...
      )
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
      cli::cli_abort(c(
        "Both {.arg position} and {.arg nudge_x}/{.arg nudge_y} are supplied.",
        "i" = "Only use one approach to alter the position."
      ))
    }

    position <- position_nudge(nudge_x, nudge_y)
  }

  layer_sf(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      parse = parse,
      label.padding = label.padding,
      label.r = label.r,
      label.size = label.size,
      na.rm = na.rm,
      fun.geometry = fun.geometry,
      ...
    )
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
      cli::cli_abort(c(
        "Both {.arg position} and {.arg nudge_x}/{.arg nudge_y} are supplied.",
        "i" = "Only use one approach to alter the position."
      ))
    }

    position <- position_nudge(nudge_x, nudge_y)
  }

  layer_sf(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      fun.geometry = fun.geometry,
      ...
    )
  )
}

sf_types <- c(GEOMETRY = "other", POINT = "point", LINESTRING = "line",
              POLYGON = "other", MULTIPOINT = "point", MULTILINESTRING = "line",
              MULTIPOLYGON = "other", GEOMETRYCOLLECTION = "collection",
              CIRCULARSTRING = "line", COMPOUNDCURVE = "line", CURVEPOLYGON = "other",
              MULTICURVE = "line", MULTISURFACE = "other", CURVE = "line",
              SURFACE = "other", POLYHEDRALSURFACE = "other", TIN = "other",
              TRIANGLE = "other")
