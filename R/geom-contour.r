#' 2D contours of a 3D surface
#'
#' ggplot2 can not draw true 3D surfaces, but you can use `geom_contour()`,
#' `geom_contour_filled()`, and [geom_tile()] to visualise 3D surfaces in 2D. To
#' specify a valid surface, the data must contain `x`, `y`, and `z` coordinates,
#' and each unique combination of `x` and `y` can appear at most once.
#' Contouring requires that the points can be rearranged so that the `z` values
#' form a matrix, with rows corresponding to unique `x` values, and columns
#' corresponding to unique `y` values. Missing entries are allowed, but contouring
#' will only be done on cells of the grid with all four `z` values present. If
#' your data is irregular, you can interpolate to a grid before visualising
#' using the [interp::interp()] function from the `interp` package
#' (or one of the interpolating functions from the `akima` package.)
#'
#' @eval rd_aesthetics("geom", "contour")
#' @eval rd_aesthetics("geom", "contour_filled")
#' @inheritParams layer
#' @inheritParams geom_point
#' @inheritParams geom_path
#' @param bins Number of contour bins. Overridden by `binwidth`.
#' @param binwidth The width of the contour bins. Overridden by `breaks`.
#' @param breaks Numeric vector to set the contour breaks. Overrides `binwidth`
#'   and `bins`. By default, this is a vector of length ten with [pretty()]
#'   breaks.
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
#'
#' # Irregular data
#' if (requireNamespace("interp")) {
#'   # Use a dataset from the interp package
#'   data(franke, package = "interp")
#'   origdata <- as.data.frame(interp::franke.data(1, 1, franke))
#'   grid <- with(origdata, interp::interp(x, y, z))
#'   griddf <- subset(data.frame(x = rep(grid$x, nrow(grid$z)),
#'                               y = rep(grid$y, each = ncol(grid$z)),
#'                               z = as.numeric(grid$z)),
#'                    !is.na(z))
#'   ggplot(griddf, aes(x, y, z = z)) +
#'     geom_contour_filled() +
#'     geom_point(data = origdata)
#' } else
#'   message("Irregular data requires the 'interp' package")
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
    params = list(
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
    params = list(
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
#' @include geom-path.r
GeomContour <- ggproto("GeomContour", GeomPath,
  default_aes = aes(
    weight = 1,
    colour = "#3366FF",
    size = 0.5,
    linetype = 1,
    alpha = NA
  )
)

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-polygon.r
GeomContourFilled <- ggproto("GeomContourFilled", GeomPolygon)

