#' 2D contours of a 3D surface
#'
#' @description
#' ggplot2 can not draw true 3D surfaces, but you can use `geom_contour()`,
#' `geom_contour_filled()`, and [geom_tile()] to visualise 3D surfaces in 2D.
#'
#' These functions require regular data, where the `x` and `y` coordinates
#' form an equally spaced grid, and each combination of `x` and `y` appears
#' once. Missing values of `z` are allowed, but contouring will only work for
#' grid points where all four corners are non-missing. If you have irregular
#' data, you'll need to first interpolate on to a grid before visualising,
#' using [interp::interp()], [akima::bilinear()], or similar.
#'
#' @eval rd_aesthetics("geom", "contour")
#' @eval rd_aesthetics("geom", "contour_filled")
#' @inheritParams layer
#' @inheritParams geom_point
#' @inheritParams geom_path
#' @param binwidth The width of the contour bins. Overridden by `bins`.
#' @param bins Number of contour bins. Overridden by `breaks`.
#' @param breaks One of:
#'   - Numeric vector to set the contour breaks
#'   - A function that takes the range of the data and binwidth as input
#'   and returns breaks as output. A function can be created from a formula
#'   (e.g. ~ fullseq(.x, .y)).
#'
#'   Overrides `binwidth` and `bins`. By default, this is a vector of length
#'   ten with [pretty()] breaks.
#' @seealso [geom_density_2d()]: 2d density contours
#' @export
#' @examples
#' # Basic plot
#' v <- ggplot(faithfuld, aes(waiting, eruptions, z = density))
#' v + geom_contour()
#'
#' # Or compute from raw data
#' ggplot(faithful, aes(waiting, eruptions)) +
#'   geom_density_2d()
#'
#' \donttest{
#' # use geom_contour_filled() for filled contours
#' v + geom_contour_filled()
#'
#' # Setting bins creates evenly spaced contours in the range of the data
#' v + geom_contour(bins = 3)
#' v + geom_contour(bins = 5)
#'
#' # Setting binwidth does the same thing, parameterised by the distance
#' # between contours
#' v + geom_contour(binwidth = 0.01)
#' v + geom_contour(binwidth = 0.001)
#'
#' # Other parameters
#' v + geom_contour(aes(colour = after_stat(level)))
#' v + geom_contour(colour = "red")
#' v + geom_raster(aes(fill = density)) +
#'   geom_contour(colour = "white")
#' }
geom_contour <- function(mapping = NULL, data = NULL,
                         stat = "contour", position = "identity",
                         ...,
                         bins = NULL,
                         binwidth = NULL,
                         breaks = NULL,
                         lineend = "butt",
                         linejoin = "round",
                         linemitre = 10,
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
    params = list2(
      bins = bins,
      binwidth = binwidth,
      breaks = breaks,
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_contour
#' @export
geom_contour_filled <- function(mapping = NULL, data = NULL,
                                stat = "contour_filled", position = "identity",
                                ...,
                                bins = NULL,
                                binwidth = NULL,
                                breaks = NULL,
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomContourFilled,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      bins = bins,
      binwidth = binwidth,
      breaks = breaks,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-path.R
GeomContour <- ggproto("GeomContour", GeomPath,
  default_aes = aes(
    weight = 1,
    colour = from_theme(accent),
    linewidth = from_theme(linewidth),
    linetype = from_theme(linetype),
    alpha = NA
  )
)

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-polygon.R
GeomContourFilled <- ggproto("GeomContourFilled", GeomPolygon)

