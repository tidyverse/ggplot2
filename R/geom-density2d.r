#' Contours of a 2d density estimate
#'
#' Perform a 2D kernel density estimation using [MASS::kde2d()] and
#' display the results with contours. This can be useful for dealing with
#' overplotting. This is a 2d version of [geom_density()].
#'
#' @eval rd_aesthetics("geom", "density_2d")
#' @seealso [geom_contour()] for information about how contours
#'  are drawn; [geom_bin2d()] for another way of dealing with
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
#' m + geom_density_2d()
#' \donttest{
#' m + stat_density_2d(aes(fill = stat(level)), geom = "polygon")
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
#' d + stat_density_2d(aes(fill = stat(level)), geom = "polygon") +
#'   facet_grid(. ~ cut) + scale_fill_viridis_c()
#' # To override this behavior (for instace, to better visualize the density
#' # within each facet), use stat(nlevel)
#' d + stat_density_2d(aes(fill = stat(nlevel)), geom = "polygon") +
#'   facet_grid(. ~ cut) + scale_fill_viridis_c()
#'
#' # If we turn contouring off, we can use use geoms like tiles:
#' d + stat_density_2d(geom = "raster", aes(fill = stat(density)), contour = FALSE)
#' # Or points:
#' d + stat_density_2d(geom = "point", aes(size = stat(density)), n = 20, contour = FALSE)
#' }
geom_density_2d <- function(mapping = NULL, data = NULL,
                            stat = "density2d", position = "identity",
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
