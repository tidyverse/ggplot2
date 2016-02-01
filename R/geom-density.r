#' Display a smooth density estimate.
#'
#' A kernel density estimate, useful for display the distribution of variables
#' with underlying smoothness.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "density")}
#'
#' @seealso See \code{\link{geom_histogram}}, \code{\link{geom_freqpoly}} for
#'   other methods of displaying continuous distribution.
#'   See \code{\link{geom_violin}} for a compact density display.
#' @inheritParams layer
#' @inheritParams geom_point
#' @param geom,stat Use to override the default connection between
#'   \code{geom_density} and \code{stat_density}.
#' @export
#' @examples
#' ggplot(diamonds, aes(carat)) +
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
#' \donttest{
#' # Stacked density plots: if you want to create a stacked density plot, you
#' # probably want to 'count' (density * n) variable instead of the default
#' # density
#'
#' # Loses marginal densities
#' ggplot(diamonds, aes(carat, fill = cut)) +
#'   geom_density(position = "stack")
#' # Preserves marginal densities
#' ggplot(diamonds, aes(carat, ..count.., fill = cut)) +
#'   geom_density(position = "stack")
#'
#' # You can use position="fill" to produce a conditional density estimate
#' ggplot(diamonds, aes(carat, ..count.., fill = cut)) +
#'   geom_density(position = "fill")
#' }
geom_density <- function(mapping = NULL, data = NULL,
                         stat = "density", position = "identity",
                         ...,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomDensity,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-ribbon.r
GeomDensity <- ggproto("GeomDensity", GeomArea,
  default_aes = defaults(
    aes(fill = NA, weight = 1, colour = "black", alpha = NA),
    GeomArea$default_aes
  )
)
