#' Smoothed density estimates
#'
#' Computes and draws kernel density estimate, which is a smoothed version of
#' the histogram. This is a useful alternative to the histogram for continuous
#' data that comes from an underlying smooth distribution.
#'
#' @eval rd_orientation()
#'
#' @eval rd_aesthetics("geom", "density")
#' @seealso See [geom_histogram()], [geom_freqpoly()] for
#'   other methods of displaying continuous distribution.
#'   See [geom_violin()] for a compact density display.
#' @inheritParams layer
#' @inheritParams geom_bar
#' @inheritParams geom_ribbon
#' @param geom,stat Use to override the default connection between
#'   `geom_density()` and `stat_density()`. For more information about
#'   overriding these connections, see how the [stat][layer_stats] and
#'   [geom][layer_geoms] arguments work.
#' @export
#' @examples
#' ggplot(diamonds, aes(carat)) +
#'   geom_density()
#' # Map the values to y to flip the orientation
#' ggplot(diamonds, aes(y = carat)) +
#'   geom_density()
#'
#' ggplot(diamonds, aes(carat)) +
#'   geom_density(adjust = 1/5)
#' ggplot(diamonds, aes(carat)) +
#'   geom_density(adjust = 5)
#'
#' ggplot(diamonds, aes(depth, colour = cut)) +
#'   geom_density() +
#'   xlim(55, 70)
#' ggplot(diamonds, aes(depth, fill = cut, colour = cut)) +
#'   geom_density(alpha = 0.1) +
#'   xlim(55, 70)
#'
#' # Use `bounds` to adjust computation for known data limits
#' big_diamonds <- diamonds[diamonds$carat >= 1, ]
#' ggplot(big_diamonds, aes(carat)) +
#'   geom_density(color = 'red') +
#'   geom_density(bounds = c(1, Inf), color = 'blue')
#'
#' \donttest{
#' # Stacked density plots: if you want to create a stacked density plot, you
#' # probably want to 'count' (density * n) variable instead of the default
#' # density
#'
#' # Loses marginal densities
#' ggplot(diamonds, aes(carat, fill = cut)) +
#'   geom_density(position = "stack")
#' # Preserves marginal densities
#' ggplot(diamonds, aes(carat, after_stat(count), fill = cut)) +
#'   geom_density(position = "stack")
#'
#' # You can use position="fill" to produce a conditional density estimate
#' ggplot(diamonds, aes(carat, after_stat(count), fill = cut)) +
#'   geom_density(position = "fill")
#' }
geom_density <- function(mapping = NULL, data = NULL,
                         stat = "density", position = "identity",
                         ...,
                         na.rm = FALSE,
                         orientation = NA,
                         show.legend = NA,
                         inherit.aes = TRUE,
                         outline.type = "upper") {
  outline.type <- arg_match0(outline.type, c("both", "upper", "lower", "full"))

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomDensity,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      orientation = orientation,
      outline.type = outline.type,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-ribbon.R
GeomDensity <- ggproto("GeomDensity", GeomArea,
  default_aes = defaults(
    aes(fill = NA, weight = 1, colour = "black", alpha = NA),
    GeomArea$default_aes
  )
)
