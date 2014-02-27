#' Display a smooth density estimate.
#'
#' A smooth density estimate calculated by \code{\link{stat_density}}.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "density")}
#'
#' @seealso \code{\link{geom_histogram}} for the histogram and
#'   \code{\link{stat_density}} for examples.
#' @inheritParams geom_point
#' @export
#' @examples
#' # See stat_density for examples
geom_density <- function (mapping = NULL, data = NULL, stat = "density", position = "identity",
na.rm = FALSE, ...) {
  GeomDensity$new(mapping = mapping, data = data, stat = stat, position = position,
  na.rm = na.rm, ...)
}

GeomDensity <- proto(GeomArea, {
  objname <- "density"

  default_stat <- function(.) StatDensity
  default_pos <- function(.) PositionIdentity

  default_aes <- function(.) defaults(aes(fill=NA, weight=1, colour="black", alpha = NA), GeomArea$default_aes())
})
