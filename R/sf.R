#' Visualise sf objects
#'
#' This set of geom, stat, and coord are used to visualise sf objects.
#' Generally you will only ever need to use \code{geom_sf}: it will
#' automatically use \code{stat_sf} and \code{coord_sf} for you.
#'
#' @examples
#' library(sf)
#'
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"))
#' ggplot(nc) +
#'   geom_sf(aes(geometry = geometry))
#' @name ggsf
NULL


# stat --------------------------------------------------------------------

#' @export
#' @rdname ggsf
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
#' @inheritParams stat_identity
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
GeomSf <- ggproto("GeomSf", Geom,
  required_aes = "geometry",
  default_aes = aes(
    colour = "grey35",
    fill = NA,
    size = 0.5,
    linetype = 1,
    alpha = NA
  ),
  draw_key = draw_key_polygon,

  draw_panel = function(data, panel_scales, coord) {
    if (!inherits(coord, "CoordSf")) {
      stop("geom_sf() must be used with coord_sf()", call. = FALSE)
    }

    coord <- coord$transform(data, panel_scales)

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
geom_sf <- function(mapping = NULL, data = NULL, stat = "sf",
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, ...) {
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
#' @inheritParams coord_cartesian
CoordSf <- ggproto("CoordSf", CoordCartesian,
  transform = function(data, panel_scales) {
    x_range <- panel_scales$x.range
    y_range <- panel_scales$y.range

    # Affine transformation to rescale to c(0, 1)
    # This will need to move into coord_sf
    data$geometry <- (data$geometry - c(x_range[1], y_range[1])) *
      matrix(c(
        1 / (x_range[2] - x_range[1]), 0,
        0, 1 / (y_range[2] - y_range[1])
      ), nrow = 2)

    data
  },

  aspect = function(self, ranges) {
    cos((0.5 * (sum(ranges$y.range)) * pi)/180)
  }
)

coord_sf <- function(xlim = NULL, ylim = NULL, expand = TRUE) {
  ggproto(NULL, CoordSf,
    limits = list(x = xlim, y = ylim),
    expand = expand
  )
}
