#' Contours of a 2D density estimate
#'
#' Perform a 2D kernel density estimation using [MASS::kde2d()] and
#' display the results with contours. This can be useful for dealing with
#' overplotting. This is a 2D version of [geom_density()]. `geom_density_2d()`
#' draws contour lines, and `geom_density_2d_filled()` draws filled contour
#' bands.
#'
#' @eval rd_aesthetics("geom", "density_2d")
#' @eval rd_aesthetics("geom", "density_2d_filled")
#' @seealso [geom_contour()], [geom_contour_filled()] for information about
#'  how contours are drawn; [geom_bin2d()] for another way of dealing with
#'  overplotting.
#' @param geom,stat Use to override the default connection between
#'   `geom_density_2d()` and `stat_density_2d()`.
#' @inheritParams layer
#' @inheritParams geom_point
#' @inheritParams geom_path
#' @param contour_var Character string identifying the variable to contour
#'   by. Can be one of `"density"`, `"ndensity"`, or `"count"`. See the section
#'   on computed variables for details.
#' @export
#' @examples
#' m <- ggplot(faithful, aes(x = eruptions, y = waiting)) +
#'  geom_point() +
#'  xlim(0.5, 6) +
#'  ylim(40, 110)
#'
#' # contour lines
#' m + geom_density_2d()
#'
#' \donttest{
#' # contour bands
#' m + geom_density_2d_filled(alpha = 0.5)
#'
#' # contour bands and contour lines
#' m + geom_density_2d_filled(alpha = 0.5) +
#'   geom_density_2d(size = 0.25, colour = "black")
#'
#' set.seed(4393)
#' dsmall <- diamonds[sample(nrow(diamonds), 1000), ]
#' d <- ggplot(dsmall, aes(x, y))
#' # If you map an aesthetic to a categorical variable, you will get a
#' # set of contours for each value of that variable
#' d + geom_density_2d(aes(colour = cut))
#'
#' # If you draw filled contours across multiple facets, the same bins are
#' # used across all facets
#' d + geom_density_2d_filled() + facet_wrap(vars(cut))
#' # If you want to make sure the peak intensity is the same in each facet,
#' # use `contour_var = "ndensity"`.
#' d + geom_density_2d_filled(contour_var = "ndensity") + facet_wrap(vars(cut))
#' # If you want to scale intensity by the number of observations in each group,
#' # use `contour_var = "count"`.
#' d + geom_density_2d_filled(contour_var = "count") + facet_wrap(vars(cut))
#'
#' # If we turn contouring off, we can use other geoms, such as tiles:
#' d + stat_density_2d(
#'   geom = "raster",
#'   aes(fill = after_stat(density)),
#'   contour = FALSE
#' ) + scale_fill_viridis_c()
#' # Or points:
#' d + stat_density_2d(geom = "point", aes(size = after_stat(density)), n = 20, contour = FALSE)
#' }
geom_density_2d <- function(mapping = NULL, data = NULL,
                            stat = "density_2d", position = "identity",
                            ...,
                            contour_var = "density",
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
    geom = GeomDensity2d,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      contour = TRUE,
      contour_var = contour_var,
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
  default_aes = aes(colour = "#3366FF", linewidth = 0.5, linetype = 1, alpha = NA)
)

#' @export
#' @rdname geom_density_2d
geom_density_2d_filled <- function(mapping = NULL, data = NULL,
                                   stat = "density_2d_filled", position = "identity",
                                   ...,
                                   contour_var = "density",
                                   na.rm = FALSE,
                                   show.legend = NA,
                                   inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomDensity2dFilled,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      contour = TRUE,
      contour_var = contour_var,
      ...
    )
  )
}

#' @export
#' @rdname geom_density_2d
#' @usage NULL
geom_density2d_filled <- geom_density_2d_filled

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-polygon.r
GeomDensity2dFilled <- ggproto("GeomDensity2dFilled", GeomPolygon)

