#' 2d contours of a 3d surface
#'
#' ggplot2 can not draw true 3d surfaces, but you can use \code{geom_contour}
#' and \code{\link{geom_tile}} to visualise 3d surfaces in 2d. To be a valid
#' surface, the data must contain only a single row for each unique combination
#' of the variables mapped to the \code{x} and \code{y} aesthetics. Contouring
#' tends to work best when \code{x} and \code{y} form a (roughly) evenly
#' spaced grid. If you data is not evenly spaced, you may want to interpolate
#' to a grid before visualising.
#'
#' @section Aesthetics:
#' \aesthetics{geom}{contour}
#'
#' @inheritParams layer
#' @inheritParams geom_point
#' @inheritParams geom_path
#' @seealso \code{\link{geom_density_2d}}: 2d density contours
#' @export
#' @export
#' @examples
#' #' # Basic plot
#' v <- ggplot(faithfuld, aes(waiting, eruptions, z = density))
#' v + geom_contour()
#'
#' # Or compute from raw data
#' ggplot(faithful, aes(waiting, eruptions)) +
#'   geom_density_2d()
#'
#' \donttest{
#' # Setting bins creates evenly spaced contours in the range of the data
#' v + geom_contour(bins = 2)
#' v + geom_contour(bins = 10)
#'
#' # Setting binwidth does the same thing, parameterised by the distance
#' # between contours
#' v + geom_contour(binwidth = 0.01)
#' v + geom_contour(binwidth = 0.001)
#'
#' # Other parameters
#' v + geom_contour(aes(colour = ..level..))
#' v + geom_contour(colour = "red")
#' v + geom_raster(aes(fill = density)) +
#'   geom_contour(colour = "white")
#' }
geom_contour <- function(mapping = NULL, data = NULL,
                         stat = "contour", position = "identity",
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
    geom = GeomContour,
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

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-path.r
GeomContour <- ggproto("GeomContour", GeomPath,
  default_aes = aes(weight = 1, colour = "#3366FF", size = 0.5, linetype = 1,
    alpha = NA)
)
