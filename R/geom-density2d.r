#' Contours of a 2D density estimate
#'
#' Perform a 2D kernel density estimation using [MASS::kde2d()] and
#' display the results with contours. This can be useful for dealing with
#' overplotting. This is a 2D version of [geom_density()]. `geom_density_2d()`
#' draws contour lines, and `geom_density_2d_filled()` draws filled contour
#' bands.
#'
#' @eval rd_aesthetics("geom", "density_2d")
#' @seealso [geom_contour()], [geom_contour_filled()] for information about
#'  how contours are drawn; [geom_bin2d()] for another way of dealing with
#'  overplotting.
#' @param geom,stat Use to override the default connection between
#'   `geom_density_2d` and `stat_density_2d`.
#' @inheritParams layer
#' @inheritParams geom_point
#' @inheritParams geom_path
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
#' # Similarly, if you apply faceting to the plot, contours will be
#' # drawn for each facet, but the levels will calculated across all facets
#' d + geom_density_2d_filled(alpha = 0.5) +
#'   facet_grid(. ~ cut)# + scale_fill_viridis_c()
#' # To override this behavior (for instance, to better visualize the density
#' # within each facet), use `after_stat(nlevel)`
#' d + geom_density_2d_filled(aes(fill = after_stat(nlevel)), alpha = 0.5) +
#'   facet_grid(. ~ cut)# + scale_fill_viridis_c()
#'
#' # If we turn contouring off, we can use other geoms, such as tiles:
#' d + stat_density_2d(geom = "raster", aes(fill = after_stat(density)), contour = FALSE)
#' # Or points:
#' d + stat_density_2d(geom = "point", aes(size = after_stat(density)), n = 20, contour = FALSE)
#' }
geom_density_2d <- function(mapping = NULL, data = NULL,
                            stat = "density_2d", position = "identity",
                            ...,
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

#' @export
#' @rdname geom_density_2d
geom_density_2d_filled <- function(mapping = NULL, data = NULL,
                                   stat = "density_2d_filled", position = "identity",
                                   ...,
                                   na.rm = FALSE,
                                   show.legend = NA,
                                   inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPolygon,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      contour_type = "bands",
      ...
    )
  )
}

#' @export
#' @rdname geom_density_2d
#' @usage NULL
geom_density2d_filled <- geom_density_2d_filled
