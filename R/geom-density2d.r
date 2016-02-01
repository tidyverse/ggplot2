#' Contours from a 2d density estimate.
#'
#' Perform a 2D kernel density estimation using kde2d and display the
#' results with contours. This can be useful for dealing with overplotting.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "density_2d")}
#'
#' @seealso \code{\link{geom_contour}} for contour drawing geom,
#'  \code{\link{stat_sum}} for another way of dealing with overplotting
#' @param geom,stat Use to override the default connection between
#'   \code{geom_density_2d} and \code{stat_density_2d}.
#' @inheritParams layer
#' @inheritParams geom_point
#' @inheritParams geom_path
#' @export
#' @examples
#' m <- ggplot(faithful, aes(x = eruptions, y = waiting)) +
#'  geom_point() +
#'  xlim(0.5, 6) +
#'  ylim(40, 110)
#' m + geom_density_2d()
#' \donttest{
#' m + stat_density_2d(aes(fill = ..level..), geom = "polygon")
#'
#' set.seed(4393)
#' dsmall <- diamonds[sample(nrow(diamonds), 1000), ]
#' d <- ggplot(dsmall, aes(x, y))
#' # If you map an aesthetic to a categorical variable, you will get a
#' # set of contours for each value of that variable
#' d + geom_density_2d(aes(colour = cut))
#'
#' # If we turn contouring off, we can use use geoms like tiles:
#' d + stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE)
#' # Or points:
#' d + stat_density_2d(geom = "point", aes(size = ..density..), n = 20, contour = FALSE)
#' }
geom_density_2d <- function(mapping = NULL, data = NULL,
                            stat = "density2d", position = "identity",
                            ...,
                            lineend = "butt",
                            linejoin = "round",
                            linemitre = 1,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomDensity2d,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      na.rm = na.rm,
      ...
    )
  )
}

#' @export
#' @rdname geom_density_2d
#' @usage NULL
geom_density2d <- geom_density_2d


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomDensity2d <- ggproto("GeomDensity2d", GeomPath,
  default_aes = aes(colour = "#3366FF", size = 0.5, linetype = 1, alpha = NA)
)
